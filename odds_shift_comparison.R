## odds_shift_comparison.R
##
## Summarizes the shift in draw odds between today's production formula
## ("Original": 2^(n+k+1) + 2*ln(v+t+1)) and the "Boosted" family at its
## dashboard defaults (b=2; k from the bonus-per-finish-count lookup
## 1/0.5/0.1; v = cumulative volunteer shifts / 4, capped at +2; t = this
## year's trailwork shifts / 8, capped at +0.5), applied to the real 2026
## applicant pool.
##
## Volunteer points are summed by NAME MATCH across 2022-2026
## (2026HLdata.csv is the only year with runner_id, so a cross-year join by
## id isn't possible for service points). This is the same name-matching
## approach this project already used for retention before lottery_panel.csv
## existed (ANALYSIS_LOG.md's 2020-2025 "name-matched" retention numbers),
## and carries the same caveat noted there: nicknames, married-name changes,
## and common names mean some matches are missed or (rarely) wrong. Treat
## the resulting cumulative-v total as an approximation, not an audit figure.
##
## Trailwork is NOT summed across years -- per Garret's framing, t is a
## one-year credit, so 2026's own Extra_Trailwork_Points is used directly,
## exactly as "Original" already does.
##
## Known simplification (documented, not hidden): this sums volunteer points
## across ALL years 2022-2026 found for a matched name, regardless of
## whether the applicant actually ran (and reset) in between. We don't have
## a reliable cross-year link for pre-2026 applicants to know when that
## happened (lottery_panel.csv's runner_id can't be joined to the
## name-only 2022-2025 files), so this is a lifetime-since-2022 total, not a
## true "since last reset" total. It's an upper bound on cumulative v for
## anyone who ran in the interim; for most applicants (resets are the
## minority outcome in any given year) it won't matter.
##
## Run: Rscript odds_shift_comparison.R
## Outputs: prints a profile-bucketed summary table to the console, writes
## sim_results/odds_shift_comparison.csv (per-applicant detail) and
## sim_results/odds_shift_by_profile.csv (the bucketed summary).

source("cohort_sim.R")    # dplyr/tidyr + calc_odds()
source("lottery_projection.R")  # build_ticket_formula()

## ---------------------------------------------------------------------------
## 1. Name-matched cumulative volunteer history, 2022-2026
## ---------------------------------------------------------------------------

name_key <- function(first, last) {
  tolower(trimws(iconv(paste(first, last), from = "", to = "ASCII", sub = "")))
}

# Each year's file uses a different name for the volunteer column, and 2025
# needs latin-1 (ANALYSIS_LOG.md: non-UTF-8 byte broke default read.csv()
# elsewhere in this project).
year_specs <- list(
  "2022" = list(file = "2022HLdata.csv", vol_col = "Volunteer_Shifts",   fenc = ""),
  "2023" = list(file = "2023HLdata.csv", vol_col = "Volunteer_Points",   fenc = ""),
  "2024" = list(file = "2024HLdata.csv", vol_col = "Volunteer_Points",   fenc = ""),
  "2025" = list(file = "2025HLdata.csv", vol_col = "Volunteer_Points",   fenc = "latin1"),
  "2026" = list(file = "2026HLdata.csv", vol_col = "Volunteer_Points",   fenc = "")
)

volunteer_history <- bind_rows(lapply(names(year_specs), function(yr) {
  spec <- year_specs[[yr]]
  d <- read.csv(spec$file, check.names = FALSE, fileEncoding = spec$fenc, stringsAsFactors = FALSE)
  tibble(
    year = as.integer(yr),
    key  = name_key(d$First_Name, d$Last_Name),
    v    = suppressWarnings(as.numeric(d[[spec$vol_col]]))
  )
}))
volunteer_history$v[is.na(volunteer_history$v)] <- 0

cumulative_v <- volunteer_history %>%
  group_by(key) %>%
  summarise(cum_v = sum(v), .groups = "drop")

## ---------------------------------------------------------------------------
## 2. The 2026 pool, with raw finishes reconstructed and both formulas' terms
## ---------------------------------------------------------------------------

# Mirrors app.R's reconstruct_2026_raw_finishes() (not sourced from app.R to
# avoid pulling in shiny/ggplot2/bslib for a plain analysis script -- see
# CLAUDE.md's RAM-limited-machine note). Keep in sync if that changes.
reconstruct_raw_finishes <- function(weighted_finishes, runner_id, panel_path = "lottery_panel.csv") {
  implied <- dplyr::case_when(
    weighted_finishes == 0   ~ 0,
    weighted_finishes == 1   ~ 2,
    weighted_finishes == 1.5 ~ 3,
    weighted_finishes == 0.5 ~ 1,
    TRUE ~ NA_real_
  )
  panel <- read.csv(panel_path, stringsAsFactors = FALSE)
  true_finishes <- panel %>%
    dplyr::filter(year < 2026, ran == TRUE, result == "finished") %>%
    dplyr::count(runner_id, name = "n_finishes")
  lookup <- setNames(true_finishes$n_finishes, true_finishes$runner_id)
  panel_true <- unname(lookup[runner_id])
  panel_true[is.na(panel_true)] <- 0
  ifelse(weighted_finishes == 0.5 & panel_true >= 4, panel_true, implied)
}

derive_k_boosted <- function(finishes, bonus_1 = 1, bonus_2 = 0.5, bonus_3plus = 0.1) {
  finishes <- as.numeric(finishes)
  ifelse(finishes <= 0, 0,
         ifelse(finishes == 1, bonus_1, ifelse(finishes == 2, bonus_2, bonus_3plus)))
}

pool <- read.csv("2026HLdata.csv", check.names = FALSE, stringsAsFactors = FALSE) %>%
  filter(Status %in% c("drawn", "waitlist", "lost")) %>%   # exclude preselect/denied, same as generate_synthetic_years.R
  mutate(
    key          = name_key(First_Name, Last_Name),
    raw_finishes = reconstruct_raw_finishes(Weighted_Finishes, runner_id),
    pool         = `Lottery Pool`
  ) %>%
  left_join(cumulative_v, by = "key") %>%
  mutate(cum_v = ifelse(is.na(cum_v), Volunteer_Points, cum_v))  # fall back to this year's own value if no match found at all

## ---------------------------------------------------------------------------
## 3. Tickets and odds under both formulas, at their live dashboard defaults
## ---------------------------------------------------------------------------

n_picks <- c(M = 96, F = 114)   # 2026's actual lottery-only picks (CLAUDE.md)

formula_current <- build_ticket_formula("current", exp_val = 2, mult_val = 2)
formula_boost   <- build_ticket_formula("boost",   exp_val = 2)

pool <- pool %>%
  mutate(
    s_current    = Volunteer_Points + Extra_Trailwork_Points,
    tickets_orig = formula_current(Previous_Applications, Weighted_Finishes, s_current),
    k_boost      = derive_k_boosted(raw_finishes, bonus_1 = 1, bonus_2 = 0.5, bonus_3plus = 0.1),
    v_boost      = pmin(cum_v / 4, 2),
    t_boost      = pmin(Extra_Trailwork_Points / 8, 0.5),
    s_boost      = v_boost + t_boost,
    tickets_boost = formula_boost(Previous_Applications, k_boost, s_boost)
  )

pool$odds_orig <- NA_real_
pool$odds_boost <- NA_real_
for (p in c("M", "F")) {
  idx <- pool$pool == p
  pool$odds_orig[idx]  <- calc_odds(pool$tickets_orig[idx],  n_picks[[p]])
  pool$odds_boost[idx] <- calc_odds(pool$tickets_boost[idx], n_picks[[p]])
}
pool$odds_shift <- pool$odds_boost - pool$odds_orig

dir.create("sim_results", showWarnings = FALSE)
write.csv(pool %>% select(pool, Previous_Applications, raw_finishes, Weighted_Finishes,
                           Volunteer_Points, cum_v, Extra_Trailwork_Points,
                           tickets_orig, tickets_boost, odds_orig, odds_boost, odds_shift),
          "sim_results/odds_shift_comparison.csv", row.names = FALSE)

## ---------------------------------------------------------------------------
## 4. Bucketed summary: pool x previous applications x raw finishes
## ---------------------------------------------------------------------------

finish_bucket <- function(f) ifelse(f >= 3, "3+", as.character(f))

summary_tbl <- pool %>%
  mutate(finish_grp = finish_bucket(raw_finishes),
         n_grp = ifelse(Previous_Applications >= 5, "5+", as.character(Previous_Applications))) %>%
  group_by(pool, n_grp, finish_grp) %>%
  summarise(
    n_people    = n(),
    mean_cum_v  = mean(cum_v),
    mean_odds_orig  = mean(odds_orig),
    mean_odds_boost = mean(odds_boost),
    mean_shift_pp   = mean(odds_boost - odds_orig) * 100,
    .groups = "drop"
  ) %>%
  arrange(pool, as.numeric(gsub("\\+", "", n_grp)), finish_grp)

write.csv(summary_tbl, "sim_results/odds_shift_by_profile.csv", row.names = FALSE)

cat("\n=== Odds shift, Original -> Boosted (both at dashboard defaults), 2026 pool ===\n\n")
print(as.data.frame(summary_tbl), row.names = FALSE)

cat("\n=== Pool-wide totals (sanity check: each should sum to its n_picks) ===\n")
for (p in c("M", "F")) {
  idx <- pool$pool == p
  cat(sprintf("%s: n=%d, sum(odds_orig)=%.1f, sum(odds_boost)=%.1f (n_picks=%d)\n",
              p, sum(idx), sum(pool$odds_orig[idx]), sum(pool$odds_boost[idx]), n_picks[[p]]))
}

cat("\n=== Biggest winners and losers (by odds shift, percentage points) ===\n")
top_bottom <- pool %>%
  select(pool, Previous_Applications, raw_finishes, cum_v, Extra_Trailwork_Points, odds_orig, odds_boost, odds_shift) %>%
  mutate(odds_shift_pp = odds_shift * 100)
cat("\n-- Top 10 gainers --\n")
print(as.data.frame(top_bottom %>% arrange(desc(odds_shift_pp)) %>% head(10)), row.names = FALSE)
cat("\n-- Top 10 losers --\n")
print(as.data.frame(top_bottom %>% arrange(odds_shift_pp) %>% head(10)), row.names = FALSE)

## ---------------------------------------------------------------------------
## 5. Pool-wide picture: every applicant's odds, Original vs Boosted
## ---------------------------------------------------------------------------
## Base graphics on purpose (cohort_sim.R's convention -- keep this script
## runnable on the RAM-limited machine without ggplot2).

png("sim_results/odds_shift_scatter.png", width = 900, height = 450)
par(mfrow = c(1, 2))
for (p in c("M", "F")) {
  idx <- pool$pool == p
  plot(pool$odds_orig[idx], pool$odds_boost[idx],
       xlim = c(0, 1), ylim = c(0, 1), pch = 16,
       col = adjustcolor(if (p == "M") "steelblue" else "firebrick", alpha.f = 0.4),
       xlab = "Odds under Original", ylab = "Odds under Boosted",
       main = sprintf("%s pool (n=%d)", if (p == "M") "Men's" else "Women's", sum(idx)))
  abline(0, 1, lty = 2, col = "gray40")
}
dev.off()

cat("\nWrote sim_results/odds_shift_comparison.csv (per-applicant detail),\n",
    "sim_results/odds_shift_by_profile.csv (bucketed summary), and\n",
    "sim_results/odds_shift_scatter.png (pool-wide picture).\n", sep = "")
