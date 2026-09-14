## cohort_sim.R — multi-year lottery simulation
##
## Starts from the actual 2026 applicant pool (2026HLdata.csv), then for each
## simulated year: computes tickets under a candidate formula, draws the lottery
## (weighted, without replacement), retires winners, re-enters losers with an
## n-dependent re-application probability, and adds a growing cohort of new
## rookies bootstrapped from the 2026 rookies.
##
## Usage:  Rscript cohort_sim.R            (runs the default scenario set)
##         source("cohort_sim.R")          (to use the functions interactively)
##
## Outputs go to sim_results/.

suppressMessages({ library(dplyr); library(tidyr) })   # plots use base graphics

## ---------------------------------------------------------------------------
## Draw mechanics
## ---------------------------------------------------------------------------

# Mean-field odds of selection in a weighted draw without replacement.
# Same algorithm as app.R / the lottery site. Sum of result == n_picks.
calc_odds <- function(tickets, n_picks) {
  surv <- rep(1, length(tickets))
  for (i in seq_len(min(n_picks, length(tickets)))) {
    tot <- sum(tickets * surv); if (tot == 0) break
    surv <- surv * (1 - tickets / tot)
  }
  1 - surv
}

# One realisation of the draw. Sequential weighted sampling without replacement
# is equivalent to ranking by Exp(1)/ticket ("exponential race"), which is much
# faster than a sample() loop. Returns indices of the winners.
draw_lottery <- function(tickets, n_picks) {
  n_picks <- min(n_picks, length(tickets))
  order(rexp(length(tickets)) / tickets)[seq_len(n_picks)]
}

## ---------------------------------------------------------------------------
## Candidate ticket formulas: function(n, k, s) -> tickets
##   n = previous applications since last reset
##   k = weighted finishes as the system stores them (0, .5, 1, 1.5, ...)
##   s = service points (volunteer + trailwork)
## ---------------------------------------------------------------------------
k_flat <- function(k) pmin(k, 0.5)   # any finish counts as +0.5, capped

formulas <- list(
  current      = function(n, k, s) 2^(n + k + 1) + 2 * log(s + 1),
  base3        = function(n, k, s) 3^(n + k),
  base4        = function(n, k, s) 4^(n + k),
  base4_kflat  = function(n, k, s) 4^(n + k_flat(k)),
  caleb        = function(n, k, s) 4^n + n * (k + s),
  base4_expsvc = function(n, k, s) 4^(n + k_flat(k) + pmin(0.5, s / 4))
)

## ---------------------------------------------------------------------------
## Starting pool
## ---------------------------------------------------------------------------
load_pool_2026 <- function(path = "2026HLdata.csv") {
  read.csv(path, check.names = FALSE) %>%
    filter(Status %in% c("drawn", "waitlist", "lost")) %>%   # lottery entrants only
    transmute(pool = `Lottery Pool`,
              n = Previous_Applications,
              k = Weighted_Finishes,
              s = Volunteer_Points + Extra_Trailwork_Points,
              drawn = Status == "drawn")
}

## ---------------------------------------------------------------------------
## Simulation
## ---------------------------------------------------------------------------
# pool0        the 2026 lottery entrants (pool, n, k, s, drawn). The 2026 draw
#              has already happened: its losers re-apply (with prob retention)
#              at n+1 in 2027, and its rookies seed the bootstrap for new rookies.
# formula      function(n, k, s) -> tickets
# years        number of lottery years to simulate (first = 2027)
# picks        lottery picks per pool (after pre-selections)
# growth       annual growth in the number of new rookies
# retention    list(M = c(r0, r1, r2, ...), F = ...): P(a loser with n prior
#              apps re-applies next year); last value is reused beyond its length
# service_mode "latest" = s stays at the applicant's initial value each year
#              "cumulative" = s accumulates (initial value added every year)
# reps         Monte Carlo replications
simulate_lottery <- function(pool0, formula, years = 8,
                             picks = c(M = 96, F = 114),
                             growth = 0.25,
                             retention = list(M = c(0.45, 0.55, 0.55, 0.55),
                                              F = c(0.30, 0.35, 0.35, 0.35)),
                             service_mode = c("latest", "cumulative"),
                             reps = 30, seed = 1, start_year = 2027) {
  service_mode <- match.arg(service_mode)
  set.seed(seed)
  rookies0 <- pool0 %>% filter(n == 0) %>% select(pool, n, k, s)
  ret_p <- function(pl, n) {
    rM <- retention$M; rF <- retention$F
    ifelse(pl == "M", rM[pmin(n + 1, length(rM))], rF[pmin(n + 1, length(rF))])
  }
  advance_losers <- function(pool) {            # after a draw: winners leave, losers re-apply w.p. retention
    losers <- pool[!pool$drawn, ]
    stay <- runif(nrow(losers)) < ret_p(losers$pool, losers$n)
    losers[stay, ] %>% mutate(n = n + 1)
  }

  year_rows <- list(); cohort_rows <- list()
  for (rep in seq_len(reps)) {
    pool <- pool0 %>% mutate(s0 = s, cohort = 2026 - n, won = NA_integer_) %>% advance_losers()
    n_rookies <- table(rookies0$pool)
    for (y in seq_len(years)) {
      yr <- start_year + y - 1
      # --- new rookies: bootstrap from 2026 rookies, growing each year ---
      n_rookies <- round(n_rookies * (1 + growth))
      new <- bind_rows(lapply(names(n_rookies), function(pl) {
        src <- rookies0[rookies0$pool == pl, ]
        src[sample(nrow(src), n_rookies[[pl]], replace = TRUE), ]
      })) %>% mutate(s0 = s, cohort = yr, won = NA_integer_)
      pool <- bind_rows(pool, new)
      # --- tickets & draw, per pool ---
      pool$tickets <- formula(pool$n, pool$k, pool$s)
      pool$odds <- NA_real_; pool$drawn <- FALSE
      for (pl in names(picks)) {
        idx <- which(pool$pool == pl)
        pool$odds[idx] <- calc_odds(pool$tickets[idx], picks[[pl]])
        pool$drawn[idx[draw_lottery(pool$tickets[idx], picks[[pl]])]] <- TRUE
      }
      # --- record year summary ---
      year_rows[[length(year_rows) + 1]] <- pool %>%
        group_by(pool, n) %>%
        summarise(N = n(), odds = mean(odds), slots = sum(drawn), .groups = "drop") %>%
        mutate(rep = rep, year = yr)
      # --- record cohort outcomes for winners ---
      pool$won[pool$drawn] <- yr
      cohort_rows[[length(cohort_rows) + 1]] <- pool %>%
        filter(drawn) %>% count(pool, cohort, name = "won") %>% mutate(rep = rep, year = yr)
      # --- winners leave; losers re-apply with prob retention(n) ---
      pool <- advance_losers(pool)
      if (service_mode == "cumulative") pool$s <- pool$s + pool$s0
    }
  }
  yearly <- bind_rows(year_rows)
  cohorts <- bind_rows(cohort_rows)
  # cohort sizes (rookie entrants) per pool/year, for cumulative entry rates
  cohort_size <- yearly %>% filter(n == 0) %>% group_by(pool, cohort = year) %>%
    summarise(size = mean(N), .groups = "drop") %>%
    bind_rows(pool0 %>% mutate(cohort = 2026 - n) %>% count(pool, cohort, name = "size"))
  list(yearly = yearly, cohorts = cohorts, cohort_size = cohort_size,
       picks = picks, years = years, start_year = start_year, reps = reps)
}

## ---------------------------------------------------------------------------
## Summaries
## ---------------------------------------------------------------------------

# Per-year table: pool size, backlog, odds by n, slots to rookies.
summarise_years <- function(sim, pl = "M") {
  sim$yearly %>% filter(pool == pl) %>%
    group_by(year, n) %>% summarise(N = mean(N), odds = mean(odds), slots = mean(slots), .groups = "drop") %>%
    group_by(year) %>%
    summarise(pool_size = round(sum(N)),
              backlog_n2plus = round(sum(N[n >= 2])),
              odds_n0 = mean(odds[n == 0]), odds_n1 = mean(odds[n == 1]),
              odds_n2 = mean(odds[n == 2]), odds_n3 = mean(odds[n == 3]),
              rookie_slots = round(sum(slots[n == 0]), 1),
              .groups = "drop")
}

# Cumulative P(in by year j) for a rookie who first applies in `cohort`
# and *never gives up* — computed from the odds faced at n = 0, 1, 2, ... in
# successive years. This is the "how long is the queue" number.
persistent_path <- function(sim, cohort, pl = "M", horizon = 5) {
  o <- sim$yearly %>% filter(pool == pl) %>% group_by(year, n) %>%
    summarise(odds = mean(odds), .groups = "drop")
  p <- sapply(seq_len(horizon) - 1, function(j) {
    v <- o$odds[o$year == cohort + j & o$n == j]; if (length(v) == 0) NA else v })
  tibble(year_of_applying = seq_len(horizon), odds_that_year = p, cum_in = 1 - cumprod(1 - p))
}

# Realised cumulative share of a rookie cohort that has gotten in by each year,
# *including* the people who gave up (this is what the RD will see happen).
realised_path <- function(sim, cohort, pl = "M") {
  size <- sim$cohort_size %>% filter(pool == pl, cohort == !!cohort) %>% pull(size)
  yrs <- tibble(year = seq(cohort, sim$start_year + sim$years - 1))
  sim$cohorts %>% filter(pool == pl, cohort == !!cohort) %>%
    group_by(year) %>% summarise(won = sum(won) / sim$reps, .groups = "drop") %>%
    right_join(yrs, by = "year") %>% arrange(year) %>%
    mutate(won = coalesce(won, 0), cum_share_in = cumsum(won) / size)
}

## ---------------------------------------------------------------------------
## Default scenario run
## ---------------------------------------------------------------------------
if (sys.nframe() == 0) {
  dir.create("sim_results", showWarnings = FALSE)
  pool0 <- load_pool_2026()
  cat("Starting pool (2026 lottery entrants):\n"); print(table(pool0$pool, pool0$n))

  scenarios <- expand.grid(formula = names(formulas), growth = c(0.10, 0.25, 0.30),
                           stringsAsFactors = FALSE)
  all_years <- list(); all_paths <- list()
  for (i in seq_len(nrow(scenarios))) {
    f <- scenarios$formula[i]; g <- scenarios$growth[i]
    sim <- simulate_lottery(pool0, formulas[[f]], growth = g, years = 8, reps = 30)
    all_years[[i]] <- summarise_years(sim, "M") %>% mutate(formula = f, growth = g)
    for (co in c(2027, 2030)) {
      all_paths[[length(all_paths) + 1]] <- persistent_path(sim, co, "M") %>%
        mutate(formula = f, growth = g, cohort = co, kind = "persistent") %>%
        select(formula, growth, cohort, kind, yr = year_of_applying, value = cum_in)
      all_paths[[length(all_paths) + 1]] <- realised_path(sim, co, "M") %>%
        mutate(formula = f, growth = g, cohort = co, kind = "realised",
               yr = year - co + 1) %>%
        select(formula, growth, cohort, kind, yr, value = cum_share_in)
    }
    cat(sprintf("done %-13s growth=%.2f\n", f, g))
  }
  years_tbl <- bind_rows(all_years); paths_tbl <- bind_rows(all_paths)
  write.csv(years_tbl, "sim_results/men_by_year.csv", row.names = FALSE)
  write.csv(paths_tbl, "sim_results/men_cohort_paths.csv", row.names = FALSE)

  ## ---- console report (growth = 25%) ----
  fmt <- function(x) sprintf("%3.0f%%", 100 * x)
  cat("\n==== MEN, growth 25%/yr: per-person odds by prior applications & rookie slots ====\n")
  for (f in names(formulas)) {
    cat("\n--", f, "--\n")
    print(years_tbl %>% filter(formula == f, growth == 0.25) %>%
            transmute(year, pool_size, backlog_n2plus, rookie_slots,
                      n0 = fmt(odds_n0), n1 = fmt(odds_n1), n2 = fmt(odds_n2), n3 = fmt(odds_n3)) %>%
            as.data.frame(), row.names = FALSE)
  }
  cat("\n==== MEN: cumulative P(in) for a rookie who never gives up ====\n")
  for (g in c(0.10, 0.25, 0.30)) for (co in c(2027, 2030)) {
    cat(sprintf("\n-- first applies %d, rookie growth %.0f%%/yr --\n", co, 100 * g))
    print(paths_tbl %>% filter(growth == g, cohort == co, kind == "persistent") %>%
            mutate(value = fmt(value)) %>%
            pivot_wider(id_cols = formula, names_from = yr, values_from = value, names_prefix = "yr") %>%
            as.data.frame(), row.names = FALSE)
  }
  cat("\n==== MEN: realised share of the 2027 rookie cohort that has run, incl. dropouts (growth 25%) ====\n")
  print(paths_tbl %>% filter(growth == 0.25, cohort == 2027, kind == "realised") %>%
          mutate(value = fmt(value)) %>%
          pivot_wider(id_cols = formula, names_from = yr, values_from = value, names_prefix = "yr") %>%
          as.data.frame(), row.names = FALSE)

  ## ---- plots (base graphics; no ggplot2 dependency) ----
  cols <- setNames(c("black", "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"), names(formulas))
  png("sim_results/men_persistent_paths.png", width = 1200, height = 720, res = 120)
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(3, 0, 2, 0))
  for (co in c(2027, 2030)) for (g in c(0.10, 0.25, 0.30)) {
    plot(NA, xlim = c(1, 5), ylim = c(0, 1), xlab = "years of applying", ylab = "cumulative P(in)",
         main = sprintf("first applies %d, rookie growth %.0f%%", co, 100 * g), yaxt = "n")
    axis(2, at = seq(0, 1, .25), labels = paste0(seq(0, 100, 25), "%")); grid()
    for (f in names(formulas)) {
      d <- paths_tbl %>% filter(formula == f, growth == g, cohort == co, kind == "persistent")
      lines(d$yr, d$value, col = cols[f], lwd = 2); points(d$yr, d$value, col = cols[f], pch = 16, cex = .7) }
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE); plot.new()
  legend("bottom", legend = names(formulas), col = cols, lwd = 2, horiz = TRUE, bty = "n", cex = .9)
  mtext("Men: cumulative chance of getting in for an applicant who never gives up", outer = TRUE, line = -1.2, font = 2)
  dev.off()

  png("sim_results/men_pool_dynamics.png", width = 1200, height = 450, res = 120)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), oma = c(3, 0, 2, 0))
  for (v in c("pool_size", "backlog_n2plus", "rookie_slots")) {
    d <- years_tbl %>% filter(growth == 0.25)
    plot(NA, xlim = range(d$year), ylim = c(0, max(d[[v]])), xlab = "year", ylab = v, main = v); grid()
    for (f in names(formulas)) { dd <- d %>% filter(formula == f); lines(dd$year, dd[[v]], col = cols[f], lwd = 2) }
    if (v == "backlog_n2plus") abline(h = 96, lty = 2)
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE); plot.new()
  legend("bottom", legend = names(formulas), col = cols, lwd = 2, horiz = TRUE, bty = "n", cex = .9)
  mtext("Men, rookie growth 25%/yr (dashed = annual lottery slots)", outer = TRUE, line = -1.2, font = 2)
  dev.off()
  cat("\nWrote sim_results/*.csv and *.png\n")
}
