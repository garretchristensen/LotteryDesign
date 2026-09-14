# Analysis log

Running notes on the lottery-redesign analysis. Newest entry first.

---

## 2026-09-14 — 2027 lottery redesign: data, odds bug, cohort simulation

### Context

Caleb (RD) emailed a proposal for "lotto 2.0" for the 2027 race (lottery moving
to Dec 2026). His points, paraphrased:

1. Lottery creep — reduce time-to-entry, especially for men. Wants base 4
   instead of base 2 (`4^n`), "average applicant in ~3 years or less".
2. Service (volunteer/trailwork) barely matters for multi-year applicants under
   the `ln` term; wants it indexed to years in the lottery; wants volunteering
   to be cumulative rather than one-year-only; keep trailwork.
3. Non-binary applicants: separate pool with 4 guaranteed spots, or proportional
   floor within a chosen binary pool.
4. Temper the previous-finish boost so it isn't "accidental elitism".
5. Looser refund/reset rules (50% / 25% / 0% windows; tickets reset only if you
   start, miss trailwork deadline, or don't register).
6. Proposed structure: `Tickets = 4^n + n(k + v + t)`.
7. Wants simulations with 6 years of data and +30%/yr growth.

Formula used for 2026 (confirmed from the export): `2^(n + k + 1) + 2·ln(v + t + 1)`.

### Data added: `2026HLdata.csv`

- Source: admin CSV export `applications-2026-high-lonesome-100-5.csv` (no
  gender/pool column) joined by `runner_id` to the public entrants API
  (`https://lottery.highlonesome100.com/lottery/lottery-api/listEntrants?lottery_id=2026-high-lonesome-100`),
  which carries `pool`, `total_tickets`, `odds`, `runner_id`. All 1,256 entrants
  matched; tickets agree 100%. The 40 unmatched CSV rows are `Denied`.
- Columns: `Lottery Pool`, names, `Age`, `Previous_Applications`,
  `Weighted_Finishes` (0/.5/1/1.5 — the export doesn't carry a raw finish count),
  `Extra_Trailwork_Points`, `Volunteer_Points`, `Tickets`, `System_Odds`,
  `Status` (drawn / waitlist / lost / preselect / denied), `Selection_Position`,
  `Pre_Selection_Reason`, `runner_id`.
- Draw structure: 125 per pool (incl. pre-selects: 29 M, 11 F) + 125-deep
  waitlist per pool. Lottery picks were therefore **96 M / 114 F**.
- The site stores `gender`, `birthGender`, `pronouns` privately and a public
  `gender_pool`; non-binary runners choose their pool. NB counts would need an
  admin export.

**2026 pool (lottery entrants, excl. pre-selects):**

| | Men | Women |
|---|---|---|
| Entrants | 906 | 310 |
| Lottery picks | 96 | 114 |
| Overall odds | 10.6% | 36.8% |
| Prior apps 0/1/2/3/4 | 567 / 208 / 99 / 27 / 5 | 265 / 42 / 3 / 0 / 0 |
| Any finish | 80 | 30 |
| Any service points | 97 | 51 |

Observed men's draw rate by prior apps 5.8 / 14.9 / 24.2 / 22 / 40% vs
formula-predicted 7.1 / 13.6 / 24.7 / 42 / 69% — consistent with noise (n=3
cohort was unlucky, 6 of 27 vs ~11 expected). The software does what we think.

**History (men in lottery):** 305 (2022) → 387 → 572 → 715 → 935 (2026).
Rookies: 234 → 286 → 396 → 452 → 567, ≈ +25%/yr. Women rookies similar rate.

**Retention (losers who re-applied next year, name-matched):** men ≈ 40–45%
after a first loss, ≈ 50–55% after later losses; women ≈ 25–30%. Count-based
estimates are a bit higher (~50/65%); name matching misses some people.

### Bug fix: `calc_lottery_odds()` in `app.R`

The "exact" odds function multiplied by survival twice (the pick probability
already contained the survival factor, then survival was updated with it
again). Odds summed to 88 instead of 96 on the 2026 men's pool; a 4-time
applicant showed 53% vs true 68%. This is why Monte Carlo and "exact"
disagreed. Fixed to

```r
survivors <- survivors * (1 - tickets / total_tix)   # each draw
odds <- 1 - survivors
```

Verified: sums to `n_picks`, matches 100k-draw Monte Carlo to ±0.0035, and
reproduces the site's published `System_Odds` to 1e-5 (the site uses the same
recursion, with 108 picks — presumably computed before final pre-selects).
Note this is still a mean-field approximation; fine for pools this size, off
by a few points on tiny toy pools. Monte Carlo remains ground truth.

Fast Monte Carlo trick used in the sim (equivalent to sequential weighted
sampling without replacement): `order(rexp(N) / tickets)[1:n_picks]`.

### Static counterfactuals on the 2026 men's pool (96 picks)

Per-person odds by prior applications:

| Formula | n=0 | n=1 | n=2 | n=3 | n=4 | rookie slots |
|---|---|---|---|---|---|---|
| current | 5.9% | 10.9% | 19.4% | 32% | 52% | 36 |
| `3^(n+k)` | 3.4% | 9.9% | 24.5% | 48% | 77% | 21 |
| `4^(n+k)` | 2.2% | 8.4% | 26.3% | 57% | 87% | 13 |
| Caleb `4^n + n(k+v+t)` | 2.1% | 8.5% | 26.8% | 59% | 86% | 13 |

- Caleb's additive booster is numerically a no-op (identical to `4^(n+k)`),
  and is zero at n=0 — so trailwork does nothing for rookies. Putting service in
  the exponent with a cap (`4^(n + k + min(0.5, (v+t)/4))`) makes a service
  point worth a fixed fraction of a year of waiting at every n.
- Finishes under base 4 with the current k schedule: a 3-finish rookie has
  13.7% vs 1.9% (7×). Flattening to "any finish = +0.5" gives 4% vs 2% (2×).
- Only 131 men at n≥2 already exceed a year's 96 slots.

### Cohort simulation: `cohort_sim.R` → `sim_results/`

Starts from 2026 entrants; 2026 losers re-apply w.p. retention at n+1; new
rookies bootstrapped from 2026 rookies growing 10/25/30%/yr; draw 96 M /
114 F; 6 formulas; 30 reps; 2027–2034. `Rscript cohort_sim.R` ≈ 3 min,
~220 MB. Base graphics only (no ggplot2 dependency).

**Men, cumulative P(in) for a rookie who never gives up, 25% growth:**

| First applies 2027 | yr1 | yr2 | yr3 | yr4 | yr5 |
|---|---|---|---|---|---|
| current | 5% | 12% | 22% | 35% | 50% |
| base 3 | 3% | 10% | 24% | 46% | 75% |
| base 4 | 2% | 8% | 24% | 55% | 89% |

| First applies 2030 | yr1 | yr2 | yr3 | yr4 | yr5 |
|---|---|---|---|---|---|
| current | 2% | 6% | 11% | 17% | 27% |
| base 3 | 1% | 4% | 10% | 21% | 40% |
| base 4 | 1% | 3% | 9% | 23% | 51% |

Key conclusions:

- The formula decides *who* waits, not *how much* waiting there is. 96 men/yr
  get in regardless. A steeper base turns the lottery into an orderly queue;
  it does not shorten the queue, and the queue grows with the pool.
- Base 4 does not achieve "in by 3 years" even for the 2027 cohort (24%); for
  the 2030 cohort it's 9% by year 3.
- Base 3 vs 4 are indistinguishable through year 3; they differ in year 4–5
  and in rookie slots (base 4: ~13 → ~6 by 2033; base 3: ~21 → ~11; current ~35).
- Realised: ~15–16% of the 2027 male rookie cohort ever runs within 8 years,
  under every formula, because most give up. That's throughput.
- Growth is the dominant variable (base 4, 2027 cohort by year 4: 66% at 10%
  growth vs 53% at 30%). The qualifier / pool-cap conversation is the real lever.
- Women's pool: ~37–40% odds every year; formula choice is nearly irrelevant.
- Refund leniency = higher retention = longer queue; and the
  "register-then-refund to keep tickets" loophole is more tempting when a
  year-3 ticket is 64× a rookie's rather than 8×.

Recommendation as of now: base 3 or 4 with finishes flattened (+0.5 for any
finish) and service in the exponent with a cap; lead the reply with the
throughput point and the 2030-cohort table.

### Automatic-entry rule ("in on your 3rd/4th try")

Added `auto_at` to `simulate_lottery()`: applicants with n >= auto_at are placed
before the draw; if they outnumber the slots they all get in and nobody else is
drawn. Results in `sim_results/men_auto_rule.csv` (men, 96 slots, lottery
among the rest with the current formula).

- **3rd try (n>=2):** infeasible immediately — ~150 automatics in 2027 vs 96
  slots (131 men are already at n>=2 in 2026). Once oversubscribed, nobody else
  is ever drawn and the tier snowballs (>500% of slots by 2034 at 25% growth).
- **4th try (n>=3), today's retention:** ~55% of slots in 2027, crosses 75%
  around 2030, exceeds 100% in 2031 (25–30% growth) or 2032 (10% growth).
- **4th try, guarantee-aware retention** (losers one short of the guarantee
  re-apply at 85% instead of 55%): ~80% of slots from 2027, >100% by 2029
  regardless of growth.
- Framing: an automatic rule is the exponent formula with base → infinity. It
  gives clarity but no control once the tier overflows, and it strengthens the
  retention feedback that lengthens the queue. A promise people can plan
  around needs a cap on inflow (qualifier / applicant cap), not a guarantee on
  outflow.

### Revision: dormant applicants (attempts need not be consecutive)

Garret confirmed prior attempts count even after skipping years. Data check:
71 of 348 men at n>=1 in 2026 (20%) skipped 2025. From dormancy ~18% return
the next year, ~11% the year after. Added to the sim: losers who don't
re-apply go dormant with n+1 retained and return w.p. `comeback`
(default c(.18, .11, .08)); `load_dormant_2026()` seeds the real reservoir
(559 men: 355 dormant 1 yr, 204 dormant 2 yrs; 122 already at n>=2).
**The tables above are superseded by these** (all men, 25% growth):

Persistent rookie, base 4: first applies 2027 → 2% / 6% / 16% / 35% / 64%
by years 1–5 (was 24% / 89% at yr 3 / 5). First applies 2030 → 0 / 1 / 4 / 9 / 20%.
Current formula, 2027 cohort → 5 / 11 / 18 / 28 / 39%.
Men's pool ≈ 1,160 (2027) → 1,980 (2029) → 2,550 (2030); n>=2 backlog > 500 by 2030.

Automatic entry, "4th try" (n>=3), share of 96 men's slots:
- today's retention: 63% (2027), 72% (2028), **~110% (2029)**, ~160% (2030) — nearly
  independent of growth rate in the first 3 years.
- guarantee-aware (85% re-apply when one short): 85% (2027), ~100% (2028), ~165% (2029).
- "3rd try": 170%+ from 2027, never recovers.

New lever for Caleb: expire prior attempts after a gap (e.g. skip two years →
reset). The dormant reservoir is the largest source of creep besides rookie
growth and it never decays under current rules.

### Revision 2: exact longitudinal panel from the site (`lottery_panel.csv`)

The site publishes every year's entrants and official drawing:
`listEntrants?lottery_id=<year>-high-lonesome-100` and
`getOfficialDrawing?lottery_id=…&withRunners=true`. Built `lottery_panel.csv`
(4,782 applications, 2,978 runners, 2020–2026) keyed by `runner_id`, with
status (drawn / waitlist / lost / preselect), position, tickets, odds, whether
the person actually ran that year (from HL100 results), and panel-derived
prior apps (matches the system's `Previous_Applications` for 97% of 2026).
Only non-sensitive fields kept. Note for Caleb / the developer: the
`withRunners=true` endpoint returns runner profile fields the UI marks as
private; those were not copied.

Exact facts (2022–2025), now the sim defaults:
- Picks per year (M/F): 2022 62/66, 2023 63/68, 2024 77/85, 2025 95/103, 2026 125/125 (incl. pre-selects).
- Waitlist call-ups who ran: ~20 M / ~28 F per year. ~20–23% of drawn runners don't run.
  Effective throughput ≈ drawn + call-ups → sim `picks = c(M = 118, F = 142)`.
- Retention (loser re-applies next year): M .42/.54/.57 at n=0/1/2; F .34/.50. Stable by year.
- Comeback from dormancy: M .22 (after 1 yr away), .10 (after 2); F .12.
- Dormant reservoir into 2027: **717 men** (324/193/112/88 by years away; n = 1/2/3/4: 559/116/34/8), 164 women.
- Past winners re-apply next year at 28%.
- Panel-derived retention supersedes the name-matched numbers above.

Results (men, supersede earlier tables):

Persistent rookie, cumulative P(in), 25% growth:

| | yr1 | yr2 | yr3 | yr4 | yr5 |
|---|---|---|---|---|---|
| current, first applies 2027 | 6% | 14% | 23% | 35% | 49% |
| base 3, 2027 | 3% | 11% | 23% | 43% | 68% |
| base 4, 2027 | 2% | 9% | 23% | 49% | 80% |
| current, 2030 | 2% | 5% | 9% | 15% | 22% |
| base 4, 2030 | 1% | 2% | 6% | 15% | 32% |

Men's pool ≈ 1,130 (2027) → 1,940 (2029) → 2,490 (2030). Realised: ~23% of
the 2027 rookie cohort runs within 8 years (any formula).

Automatic entry "4th try" (n>=3), share of 118 effective men's slots:
- today's retention: 49% (2027), 55% (2028), ~80% (2029), **~120% (2030)**, ~165% (2031).
- guarantee-aware (85%): 67% (2027), 75% (2028), **~115% (2029)**, ~175% (2030).
- "3rd try": ~138% from 2027 — infeasible.

### Open items

- Confirm with Caleb: is 125/pool the new race size or an over-draw?
- NB applicant counts — needs admin export of `gender` / `gender_pool`.
- Sim extensions not yet in the default run: `service_mode = "cumulative"`,
  women's tables (`summarise_years(sim, "F")`), pool cap / qualifier scenarios,
  returning-finisher modelling (currently implicit via rookie bootstrap).
- Port the exponential-race draw into the app's Monte Carlo.
- Environment: R was upgraded to 4.6 but the user package library is 4.5;
  `shiny`, `ggplot2`, `DT`, `bslib` etc. do not load until reinstalled.
