# LotteryDesign

Tools for designing the High Lonesome 100 entry lottery: a Shiny app that
shows how a ticket-weighting formula changes who gets drawn, plus analysis
scripts. Owner: Garret Christensen (does the math for the RD, Caleb Efta).
Currently working on the **2027 lottery redesign** — see `ANALYSIS_LOG.md`
for the full narrative, findings, and open items. **Append a dated entry to
`ANALYSIS_LOG.md` (newest first) whenever you do substantive analysis.**

## Files

- `app.R` — the Shiny app (deployed to shinyapps.io as `garretchristensen/LotteryDesign`, config in `rsconnect/`). `intro.md` and `www/` are its static content.
- `cohort_sim.R` — multi-year cohort simulation of the lottery (`Rscript cohort_sim.R`, ~3 min, writes `sim_results/`). Needs only `dplyr` and `tidyr`; plots are base graphics on purpose. `source()` it to use the functions interactively.
- `20XXHLdata.csv` — applicant pools by year (2022–2026 tracked). Column names drift across years; 2026 is the cleanest.
- `2026HLdata.csv` — built from the admin export `applications-2026-high-lonesome-100-5.csv` joined by `runner_id` to the public entrants API (see log). Includes draw outcomes.
- `prediction.ipynb` — Python notebook forecasting applicant growth (random forest etc.). `synthetic_202X_predictions.csv`, `longitudinal_data.csv` come from it.
- `old/` — superseded Rmd version of the app.

## Domain facts

- Two pools, `Lottery Pool` = `M` / `F`. Non-binary applicants choose a pool.
- 2026 formula: `Tickets = 2^(n + k + 1) + 2·ln(v + t + 1)` — n = previous applications since last reset, k = weighted finishes (0/.5/1/1.5, back to .5 at 4+), v = volunteer points, t = extra trailwork points.
- Draw is weighted, without replacement, per pool. 2026 drew 125 per pool including pre-selects (29 M / 11 F) → 96 M / 114 F lottery picks, plus 125-deep waitlists.
- `Status` in 2026 data: `drawn`, `waitlist`, `lost`, `preselect`, `denied`.
- Men's pool ≈ 10% odds and growing ~25–30%/yr; women's ≈ 37%. The formula can only redistribute odds, not shorten the queue — keep that framing.
- Lottery site: `https://lottery.highlonesome100.com`. Public per-entrant data (pool, tickets, odds) via `/lottery/lottery-api/listEntrants?lottery_id=<slug>`. Gender identity is private; only chosen pool is public.

## Odds math

- `calc_lottery_odds()` in `app.R` (and `calc_odds()` in the sim) is the mean-field recursion: each draw `surv <- surv * (1 - tickets/sum(tickets*surv))`, odds = `1 - surv`. Odds must sum to `n_picks` — use that as a sanity check. It matches Monte Carlo to ~0.003 on real pools; it is *not* exact on tiny pools. It was buggy before Sept 2026 (double-counted survival).
- Fast exact Monte Carlo draw: `order(rexp(N) / tickets)[1:n_picks]` (exponential race ≡ sequential weighted sampling without replacement).

## Environment gotchas

- Garret is often on a RAM-limited machine. Prefer `Rscript` scripts over launching Shiny; keep sims under ~300 MB.
- One machine has R 4.6 with the user library still under `~/R/.../4.5` — `shiny`, `ggplot2`, `DT`, `bslib` won't load there until reinstalled. Don't add ggplot2 to scripts that need to run everywhere.
- Name matching across years needs `iconv(x, "", "ASCII", sub="")` first; some names have non-UTF-8 bytes.
- `.~lock.*`, `.ipynb_checkpoints/`, `.Rproj.user/` are editor droppings — ignore.

## Conventions

- Commit messages: short imperative summary, as in the existing history.
- Don't commit the raw admin export (`applications-2026-*.csv`) — it contains emails; `2026HLdata.csv` is the cleaned version.
