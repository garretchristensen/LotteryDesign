## generate_synthetic_years.R
##
## Produces plausible 2027-2030 applicant pools so the Shiny app's "Data
## Year" dropdown can include them alongside the real 2023-2026 years, for
## exploring how any hypothetical formula would behave on a FUTURE pool
## instead of just a historical one.
##
## Methodology: starts from the real, raw 2026 pool (2026HLdata.csv) and the
## panel-derived dormant-applicant reservoir (lottery_panel.csv, via
## cohort_sim.R's load_dormant_2026()), then advances it forward one lottery
## year at a time using the SAME bootstrap/retention/dormancy mechanics
## cohort_sim.R already uses for its authoritative analysis -- EXCEPT it
## keeps Volunteer_Points and Extra_Trailwork_Points as two separate numbers
## throughout (cohort_sim.R only ever needs their SUM, so it collapses them
## on load; app.R's "Boosted" formula needs them separately, so this script
## duplicates the relevant bootstrap/advance logic rather than touching
## cohort_sim.R's existing, already-validated pool representation).
##
## Each simulated year's draw uses TODAY'S PRODUCTION formula (exp=2,
## current/log-service-credit) -- this is the correct NEUTRAL baseline
## regardless of which hypothetical formula someone later explores on the
## resulting pool in the dashboard, exactly mirroring how a REAL year's
## Status column reflects what actually happened, independent of whatever
## formula the dashboard's selector is currently set to. Growth is fixed at
## 25%/yr (this app's "recent actual trend" default). ONE seeded,
## deterministic run (not averaged over reps) -- these are meant to be
## concrete, reproducible snapshots, not a distribution.
##
## Known simplifications (consistent with cohort_sim.R's own documented
## ones): new rookies are bootstrapped (with replacement) from REAL 2026
## rookie profiles, so their (k, v, t) joint distribution is realistic;
## dormant returnees carry v = t = 0 (service points genuinely unknown for
## them, same assumption cohort_sim.R's load_dormant_2026() already makes);
## no preselects or denials are modeled. The output's `Weighted_Finishes`
## column means these synthetic runner_ids won't match anything in
## lottery_panel.csv, so app.R's reconstruct_2026_raw_finishes() harmlessly
## falls back to its "implied" default for the ambiguous 0.5 bucket (can't
## unmask 4+-time veterans it has no panel history for -- fine, since none
## of these people exist yet).
##
## Run: Rscript generate_synthetic_years.R  (a few seconds)

source("cohort_sim.R")

SEED <- 2027
GROWTH <- 0.25
TARGET_YEARS <- 2027:2030
PICKS <- c(M = 118, F = 142)   # today's "effective" combined draw+waitlist throughput

set.seed(SEED)

load_pool_2026_vt <- function(path = "2026HLdata.csv") {
  read.csv(path, check.names = FALSE) %>%
    filter(Status %in% c("drawn", "waitlist", "lost")) %>%
    transmute(pool = `Lottery Pool`, n = Previous_Applications, k = Weighted_Finishes,
              v = Volunteer_Points, t = Extra_Trailwork_Points, drawn = Status == "drawn")
}

pool0 <- load_pool_2026_vt()
dormant0 <- load_dormant_2026() %>% mutate(v = 0, t = 0) %>% select(-s)
rookies0 <- pool0 %>% filter(n == 0) %>% select(pool, n, k, v, t)

ret_p <- function(pl, n) {
  rM <- c(0.42, 0.54, 0.57, 0.50); rF <- c(0.34, 0.50, 0.50, 0.50)
  ifelse(pl == "M", rM[pmin(n + 1, length(rM))], rF[pmin(n + 1, length(rF))])
}
COMEBACK <- c(0.22, 0.10, 0.05)

advance_losers <- function(pool, dormant) {
  losers <- pool[!pool$drawn, ] %>% mutate(n = n + 1)
  stay <- runif(nrow(losers)) < ret_p(losers$pool, losers$n - 1)
  new_dormant <- losers[!stay, ] %>% mutate(yrs = 0L)
  if (nrow(dormant) > 0) {
    dormant$yrs <- dormant$yrs + 1L
    back <- runif(nrow(dormant)) < COMEBACK[pmin(dormant$yrs, length(COMEBACK))]
    returned <- dormant[back, ] %>% select(-yrs); dormant <- dormant[!back, ]
  } else returned <- losers[0, ]
  list(active = bind_rows(losers[stay, ], returned), dormant = bind_rows(dormant, new_dormant))
}

st <- pool0 %>% advance_losers(dormant = dormant0 %>% mutate(drawn = FALSE))
pool <- st$active; dormant <- st$dormant
n_rookies <- round(table(rookies0$pool) * (1 + GROWTH))

snapshot_cols <- c("Lottery Pool", "Previous_Applications", "Weighted_Finishes",
                    "Volunteer_Points", "Extra_Trailwork_Points", "Status", "runner_id")

for (y in seq_along(TARGET_YEARS)) {
  yr <- TARGET_YEARS[y]
  n_rookies <- round(n_rookies * (1 + GROWTH))
  new <- bind_rows(lapply(names(n_rookies), function(pl) {
    src <- rookies0[rookies0$pool == pl, ]
    src[sample(nrow(src), n_rookies[[pl]], replace = TRUE), ]
  }))
  pool <- bind_rows(pool, new)

  pool$tickets <- formulas[["current"]](pool$n, pool$k, pool$v + pool$t)
  pool$Status <- "lost"
  for (pl in names(PICKS)) {
    idx <- which(pool$pool == pl)
    ranked <- idx[order(rexp(length(idx)) / pool$tickets[idx])]
    n_drawn <- min(96, length(ranked)) ; if (pl == "F") n_drawn <- min(114, length(ranked))
    n_wait <- min(PICKS[[pl]] - n_drawn, length(ranked) - n_drawn)
    if (n_drawn > 0) pool$Status[ranked[seq_len(n_drawn)]] <- "drawn"
    if (n_wait > 0) pool$Status[ranked[n_drawn + seq_len(n_wait)]] <- "waitlist"
  }
  pool$drawn <- pool$Status %in% c("drawn", "waitlist")   # both leave the pool (real throughput)

  if (yr %in% TARGET_YEARS) {
    snap <- pool %>% transmute(
      `Lottery Pool` = pool, Previous_Applications = n, Weighted_Finishes = k,
      Volunteer_Points = v, Extra_Trailwork_Points = t, Status = Status,
      runner_id = sprintf("synthetic_%d_%05d", yr, row_number())
    )
    fname <- sprintf("synthetic_%dHLdata.csv", yr)
    write.csv(snap, fname, row.names = FALSE)
    cat(sprintf("Wrote %s (%d men, %d women; %d drawn, %d waitlist)\n",
                fname, sum(snap$`Lottery Pool` == "M"), sum(snap$`Lottery Pool` == "F"),
                sum(snap$Status == "drawn"), sum(snap$Status == "waitlist")))
  }

  st <- advance_losers(pool, dormant); pool <- st$active; dormant <- st$dormant
}
