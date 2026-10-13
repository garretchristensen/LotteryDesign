## lottery_projection.R
##
## Fast, DETERMINISTIC (expected-value) multi-year projection engine for the
## Shiny dashboard (app.R). This is a lighter-weight cousin of
## cohort_sim.R's simulate_lottery(): instead of resampling individuals and
## running many Monte Carlo replications (accurate, but ~3 minutes per run
## per ANALYSIS_LOG.md -- too slow to sit behind a slider in a live app),
## this tracks *expected* population counts by (n, k, s) group --
##   n = previous applications (prior losses) since the last reset
##   k = weighted finishes, on the system's own 0 / .5 / 1 / 1.5 scale
##   s = service points (volunteer + extra trailwork), combined as the
##       formula already does
## and advances the whole table forward one lottery year at a time using
## expected values only (no randomness, no reps).
##
## This is meant for quick, illustrative "what if" exploration inside the
## app -- NOT a replacement for cohort_sim.R, which remains the
## authoritative source for the numbers already written up in
## ANALYSIS_LOG.md. Known simplifications, so nobody mistakes this for the
## full model:
##
##   1. No dormancy / comeback modeling. In cohort_sim.R (and in reality,
##      per the panel data), a loser who doesn't re-apply the very next
##      year can still come back later with their ticket count (n) intact
##      -- ANALYSIS_LOG.md calls this the "dormant reservoir" and found it
##      is one of the biggest drivers of long-run backlog growth. This lite
##      projection has no such memory: if someone doesn't re-apply next
##      year, they are simply gone. That makes every number out of this
##      file systematically OPTIMISTIC (faster apparent entry, slower
##      apparent oversubscription of any automatic-entry tier) relative to
##      ANALYSIS_LOG.md's dormancy-adjusted and panel-based figures.
##   2. New rookies each year are added as a single homogeneous group with
##      k = 0, s = 0 (no previous finishes, no service points yet). The
##      large majority of real rookies have k = s = 0, so this is a
##      reasonable approximation, but it is not an exact bootstrap of real
##      rookie profiles the way cohort_sim.R's resampling is.
##   3. Service points never accumulate across years (equivalent to
##      cohort_sim.R's service_mode = "latest"). cohort_sim.R's
##      "cumulative" mode is not implemented here.
##   4. Retention (re-application) rates are fixed constants taken from
##      ANALYSIS_LOG.md's panel-derived figures (the "Revision 2" section);
##      they are not exposed as a dashboard knob, to keep the UI focused on
##      the things this conversation specifically asked for (formula,
##      automatic-entry threshold, growth rate).
##
## For a number that must exactly match the ANALYSIS_LOG.md write-up, use
## cohort_sim.R instead of this file.

suppressMessages(library(dplyr))

## ---------------------------------------------------------------------------
## Group-weighted odds (the core piece that makes this fast)
## ---------------------------------------------------------------------------

# calc_group_odds(): the same mean-field "exact" recursion as app.R's
# calc_lottery_odds(), generalised so that each row represents a GROUP of N
# identical individuals rather than one individual. This is mathematically
# IDENTICAL to expanding every group into N individual rows and calling
# calc_lottery_odds() on the full vector -- everyone within a group has the
# same ticket count, so they also have the same survival probability at
# every step -- but it runs in time proportional to the number of groups
# (a few dozen) rather than the number of individuals (thousands), which is
# what makes an 8-year projection fast enough to sit behind a button in a
# live Shiny app.
calc_group_odds <- function(N, tickets, n_picks) {
  if (length(tickets) == 0 || n_picks <= 0) return(numeric(length(tickets)))
  survivors <- rep(1, length(tickets))         # P(group not yet fully drawn out)
  for (draw in seq_len(n_picks)) {
    total_tix <- sum(N * tickets * survivors)  # expected tickets still "in the hat"
    if (total_tix == 0) break
    survivors <- survivors * (1 - tickets / total_tix)
  }
  1 - survivors
}

## ---------------------------------------------------------------------------
## Ticket formula families
## ---------------------------------------------------------------------------

# build_ticket_formula(): returns a function(n, k, s) -> tickets for one of
# the formula "families" discussed in ANALYSIS_LOG.md's static-counterfactual
# table. Shared by app.R's live odds tables AND this projection engine, so a
# single sidebar control drives both consistently instead of risking two
# formula implementations drifting apart.
#
#   "current"          the system's actual 2026 formula:
#                       exp^(n+k+1) + mult*log(s+1)
#   "pure_power"        a plain power formula with NO service credit at all,
#                       e.g. 3^(n+k) or 4^(n+k) from the log's
#                       static-counterfactuals table -- used to show that
#                       service/trailwork would need to move INTO the
#                       exponent to matter once the base gets steep
#   "caleb"             Caleb's literal proposal, base^n + n*(k+s) -- kept
#                       here so the dashboard can reproduce the finding that
#                       it is numerically almost a no-op vs. plain pure_power
#   "garret_decoupled"  Garret's own proposal from the 2026-10-13 "Decoupled
#                       formula design" log entry: exp_val^n * 2^(k+s).
#                       `exp_val` (the SAME base slider the other families
#                       reuse -- "b" in the log's writeup) controls only how
#                       steeply waiting time (n) matters. The `2^(...)`
#                       multiplier is a SEPARATE, deliberately-fixed-at-base-2
#                       factor, decoupled from exp_val/b, whose only job is
#                       to bound the max "being a veteran" bonus at exactly
#                       2x (2^1) -- independent of whatever base is chosen
#                       for the n-term. By the time `s` reaches this
#                       function it is NOT raw volunteer+trailwork points --
#                       app.R's apply_service_toggles() has already turned
#                       it into the family-appropriate exponent contribution
#                       (v/v_scale, uncapped, plus min(t/t_scale, t_cap),
#                       capped), per that same log entry's v/t design. See
#                       ANALYSIS_LOG.md's 2026-10-13 entries for the full
#                       derivation of why 2^(k+v+t) == 2^k * 2^(v+t) means
#                       capping k alone is NOT broken by leaving v uncapped
#                       in the same written exponent.
build_ticket_formula <- function(family, exp_val = 2, mult_val = 2) {
  switch(family,
    current          = function(n, k, s) exp_val ^ (n + k + 1) + mult_val * log(s + 1),
    pure_power       = function(n, k, s) exp_val ^ (n + k),
    caleb            = function(n, k, s) exp_val ^ n + n * (k + s),
    garret_decoupled = function(n, k, s) exp_val ^ n * 2 ^ (k + s),
    stop("build_ticket_formula(): unknown family '", family, "'")
  )
}

## ---------------------------------------------------------------------------
## One year of the lottery, applied to a group table
## ---------------------------------------------------------------------------

# draw_year(): computes tickets and draw odds for one simulated year.
# Honours an optional automatic-entry guarantee at n >= auto_at, with the
# exact same semantics as cohort_sim.R's simulate_lottery(auto_at = ...):
# automatics are placed before the lottery draw and do not compete for
# slots; if they outnumber the slots, they ALL still get in (the policy
# promise is kept literally) and nobody else is drawn that year. That is
# what lets pct_of_slots exceed 100% in the yearly summary -- it is not a
# bug, it is the point: it shows exactly how oversubscribed the guarantee
# has become.
draw_year <- function(groups, formula_fn, n_picks, auto_at = Inf) {
  groups$tickets <- formula_fn(groups$n, groups$k, groups$s)
  groups$odds <- 0
  is_auto <- groups$n >= auto_at
  n_auto <- sum(groups$N[is_auto])
  slots_left <- max(n_picks - n_auto, 0)
  groups$odds[is_auto] <- 1
  rest <- which(!is_auto)
  if (length(rest) > 0 && slots_left > 0) {
    groups$odds[rest] <- calc_group_odds(groups$N[rest], groups$tickets[rest], slots_left)
  }
  groups
}

# retention_rate(): P(a loser with n prior applications re-applies next
# year), from ANALYSIS_LOG.md's panel-derived "Revision 2" figures (the
# exact 2022-2025 rates, the log's most trustworthy version). See that file
# for how they were measured -- these are not knobs here, by design (see
# file header, limitation #4).
retention_rate <- function(n, pool = c("M", "F")) {
  pool <- match.arg(pool)
  rates <- if (pool == "M") c(0.42, 0.54, 0.57, 0.50) else c(0.34, 0.50, 0.50, 0.50)
  rates[pmin(n + 1, length(rates))]
}

# advance_year(): losers who don't win this year either re-apply next year
# at n+1 (same k, s -- "latest" service mode) with probability
# retention_rate(), or are dropped (see file header, limitation #1: no
# dormancy). Rows are re-aggregated by (n, k, s) so the table doesn't grow
# with every distinct starting row -- only with genuinely distinct profiles.
advance_year <- function(groups, pool = "M") {
  losers_N <- groups$N * (1 - groups$odds)
  reapply_N <- losers_N * retention_rate(groups$n, pool)
  groups %>%
    mutate(N = reapply_N, n = n + 1) %>%
    filter(N > 1e-6) %>%
    group_by(n, k, s) %>%
    summarise(N = sum(N), .groups = "drop")
}

## ---------------------------------------------------------------------------
## Multi-year projection
## ---------------------------------------------------------------------------

# project_pool(): starting from an observed (n, k, s, N) group table for one
# season (see seed_groups_from_data() in app.R), projects `years` lottery
# years forward. Each year, `growth`-percent more new rookies are added
# (modelled as one k=0, s=0 group -- see limitation #2) before the draw.
#
# Returns:
#   $yearly  -- one row per simulated year: pool size, how many are already
#               past the automatic-entry threshold (if any), and what share
#               of n_picks that group alone would consume.
#   $history -- the full group table (with that year's tickets/odds) for
#               each simulated year, kept so persistent_rookie_path() can
#               look up the odds a specific hypothetical applicant faced.
project_pool <- function(seed_groups, formula_fn, n_picks, years = 8,
                          growth = 0.25, auto_at = Inf, pool = "M") {
  groups <- seed_groups
  rookie_n <- sum(seed_groups$N[seed_groups$n == 0])
  yearly <- vector("list", years)
  history <- vector("list", years)

  for (y in seq_len(years)) {
    rookie_n <- rookie_n * (1 + growth)
    groups <- bind_rows(groups, tibble(n = 0, k = 0, s = 0, N = rookie_n)) %>%
      group_by(n, k, s) %>% summarise(N = sum(N), .groups = "drop")

    groups <- draw_year(groups, formula_fn, n_picks, auto_at)
    history[[y]] <- groups

    is_auto <- groups$n >= auto_at
    yearly[[y]] <- tibble(
      year = y,
      pool_size = sum(groups$N),
      auto_eligible = sum(groups$N[is_auto]),
      auto_slots = sum(groups$N[is_auto] * groups$odds[is_auto]),
      pct_of_slots = 100 * auto_slots / n_picks
    )

    groups <- advance_year(groups, pool)
  }

  list(yearly = bind_rows(yearly), history = history)
}

# persistent_rookie_path(): "if someone first applies as a rookie in
# projection-year 1 and NEVER gives up, what's their cumulative chance of
# having been drawn by year j?" Mirrors cohort_sim.R's persistent_path(),
# but reads the odds off this deterministic projection's history instead of
# averaging Monte Carlo reps. Because our injected rookies always start at
# k = 0, s = 0 and those never change year to year (limitation #2/#3), the
# hypothetical persistent rookie's own lineage is exactly the
# (n = year - 1, k = 0, s = 0) group in each year's history.
persistent_rookie_path <- function(proj, horizon = 5) {
  horizon <- min(horizon, length(proj$history))
  odds_by_year <- sapply(seq_len(horizon), function(y) {
    grp <- proj$history[[y]]
    row <- grp[grp$n == (y - 1) & grp$k == 0 & grp$s == 0, ]
    if (nrow(row) == 0) return(NA_real_)
    row$odds[1]
  })
  tibble(
    year_of_applying = seq_along(odds_by_year),
    odds_that_year = odds_by_year,
    cum_in = 1 - cumprod(1 - odds_by_year)
  )
}
