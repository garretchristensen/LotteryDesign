## auto_waitlist_sim.R
##
## Tests Garret's 2026-11-02 "auto-waitlist" proposal: every year, recompute
## whichever n value(s) are currently the pool's highest, and give anyone in
## that tier who isn't drawn automatic FIRST priority for that year's
## waitlist call-ups (random tiebreak), ahead of the ordinary ticket-weighted
## waitlist order. Question: does this ever clear, or does the backlog
## compound without bound ("last essentially forever")?
##
## Uses cohort_sim.R's simulate_lottery(waitlist_tiers = ...) -- see that
## file's header comment for the mechanic. Run: Rscript auto_waitlist_sim.R
## (seconds, not minutes -- this is much cheaper than cohort_sim.R's own
## default scenario sweep).

source("cohort_sim.R")

dir.create("sim_results", showWarnings = FALSE)
pool0 <- load_pool_2026(); dormant0 <- load_dormant_2026()

YEARS <- 18
REPS <- 15
GROWTH <- 0.25

cat("Running tiers=1 (single highest-n tier gets auto-waitlist priority)...\n")
sim1 <- simulate_lottery(pool0, formulas[["current"]], years = YEARS, reps = REPS,
                          dormant0 = dormant0, waitlist_tiers = 1, growth = GROWTH, seed = 1)

cat("Running tiers=2 (top two highest-n tiers)...\n")
sim2 <- simulate_lottery(pool0, formulas[["current"]], years = YEARS, reps = REPS,
                          dormant0 = dormant0, waitlist_tiers = 2, growth = GROWTH, seed = 1)

bl1_M <- summarise_backlog(sim1, "M") %>% mutate(tiers = 1, pool = "M")
bl1_F <- summarise_backlog(sim1, "F") %>% mutate(tiers = 1, pool = "F")
bl2_M <- summarise_backlog(sim2, "M") %>% mutate(tiers = 2, pool = "M")
bl2_F <- summarise_backlog(sim2, "F") %>% mutate(tiers = 2, pool = "F")
backlog_all <- bind_rows(bl1_M, bl1_F, bl2_M, bl2_F)
write.csv(backlog_all, "sim_results/auto_waitlist_backlog.csv", row.names = FALSE)

fmt <- function(x) sprintf("%5.1f", x)
cat("\n==== MEN, waitlist_tiers=1 (single highest-n tier), growth 25%/yr ====\n")
print(bl1_M %>% transmute(year, tier_size = fmt(tier_size), tier_served = fmt(tier_served),
                           tier_backlog = fmt(tier_backlog), cum_unserved = fmt(cum_unserved)) %>%
        as.data.frame(), row.names = FALSE)

cat("\n==== WOMEN, waitlist_tiers=1, growth 25%/yr ====\n")
print(bl1_F %>% transmute(year, tier_size = fmt(tier_size), tier_served = fmt(tier_served),
                           tier_backlog = fmt(tier_backlog), cum_unserved = fmt(cum_unserved)) %>%
        as.data.frame(), row.names = FALSE)

cat("\n==== MEN, waitlist_tiers=2 (top two highest-n tiers), growth 25%/yr ====\n")
print(bl2_M %>% transmute(year, tier_size = fmt(tier_size), tier_served = fmt(tier_served),
                           tier_backlog = fmt(tier_backlog), cum_unserved = fmt(cum_unserved)) %>%
        as.data.frame(), row.names = FALSE)

cat("\n==== WOMEN, waitlist_tiers=2, growth 25%/yr ====\n")
print(bl2_F %>% transmute(year, tier_size = fmt(tier_size), tier_served = fmt(tier_served),
                           tier_backlog = fmt(tier_backlog), cum_unserved = fmt(cum_unserved)) %>%
        as.data.frame(), row.names = FALSE)

## ---- plot: tier size vs. waitlist throughput, and cumulative unserved ----
png("sim_results/auto_waitlist_backlog.png", width = 1100, height = 750, res = 120)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(2, 0, 2, 0))
throughput <- c(M = 22, F = 28)
for (tiers_sel in c(1, 2)) {
  for (pl in c("M", "F")) {
    d <- backlog_all %>% filter(tiers == tiers_sel, pool == pl)
    plot(d$year, d$tier_size, type = "l", lwd = 2, col = "#B7822E",
         ylim = c(0, max(d$tier_size, d$cum_unserved, na.rm = TRUE)),
         xlab = "year", ylab = "people",
         main = sprintf("%s, waitlist_tiers=%d", ifelse(pl == "M", "Men", "Women"), tiers_sel))
    lines(d$year, d$cum_unserved, lwd = 2, col = "#d95f02")
    abline(h = throughput[[pl]], lty = 2, col = "grey40")
    grid()
    legend("topleft", legend = c("tier size (this year)", "cumulative never-served", "waitlist throughput/yr"),
           col = c("#B7822E", "#d95f02", "grey40"), lty = c(1, 1, 2), lwd = 2, bty = "n", cex = 0.75)
  }
}
mtext("Auto-waitlist tier size vs. annual waitlist throughput (growth 25%/yr)", outer = TRUE, line = 0.5, font = 2)
dev.off()

cat("\nWrote sim_results/auto_waitlist_backlog.csv and .png\n")
