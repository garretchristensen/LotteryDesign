# Analysis log

Running notes on the lottery-redesign analysis. Newest entry first.

---

## 2026-10-13 (12) — Merging dashboardguy's branch into main

**Rollback point:** before this merge, `main` is tagged
`pre-dashboard-merge-2026-10-13` (pointing at commit 9a2cbf6, the last
commit before any of dashboardguy's `app.R` work lands on `main`). To
revert the dashboard to the pre-redesign version:

    git checkout pre-dashboard-merge-2026-10-13 -- app.R lottery_projection.R

or, to roll `main` all the way back:

    git reset --hard pre-dashboard-merge-2026-10-13

That tag is the single rollback anchor for everything described below.

**What's being merged**, from branch `worktree-agent-a58abef50e243d5a8`
(14 commits, dashboardguy -- a forked background agent, not written by
Garret, though reviewed and steered by him and Claude round by round all
day): the 2026-data/Monte-Carlo crash fixes, the odds-dot-plot
guaranteed-entry contamination fix, the generalized RAMP/FLOOR/MAX-BOOST
knob for the finish-bonus term `k` (applying to every formula family
except `caleb`, where `k` is additive rather than exponential), the new
"Garret's decoupled proposal" formula family (`b^n * 2^(k+v+t)`, with its
own v_scale/t_scale/t_cap sliders), the dynamic ticket-formula display
string, the collapsible Finish-Bonus-Ramp help text, the default-year
correction (2026), and the repositioned Guaranteed-Entry Snapshot box
(now a standalone element between the odds results and the Multi-Year
Projection section, visible only once a guarantee rule is selected). Full
round-by-round detail for all of this is in the entries below this one
(dashboardguy's own log entries, carried over verbatim by the merge).

Still true after the merge: nothing has been pushed to GitHub, nothing
has been deployed to shinyapps.io. The live site at
garretchristensen.shinyapps.io/LotteryDesign/ is unaffected until Garret
deploys explicitly.

---

## 2026-10-13 (2) — Decoupled formula design; fact-checking Garret's draft reply to Caleb

Continuation of same-day work: designed the `b^n * 2^(min(1,k)) * c^(v+t)`-style
decoupled formula (separate base for n-steepness vs. a hard-capped
veteran/finish multiplier vs. an optionally-uncapped service multiplier --
see math below), fact-checked Garret's near-final draft reply to Caleb against
the real data, and corrected a mistake of my own in that fact-check.

**Decoupling insight.** Caleb's two-term proposal (`4^n + n(k+v+t)`) fails two
ways: numerically it's a no-op vs. `4^(n+k)` (nearly identical odds at every
n), and it's zero at n=0 so trailwork does nothing for first-time applicants,
then grows unboundedly for veterans -- the inverse of what "reward service
fairly" should look like. An additive `(n+1)` term fixes the n=0 dead zone
but overcorrects: a single service point then swamps a rookie's tiny ticket
base (2x) while being nearly invisible to a high-n veteran (1.06x), because
the bonus term grows linearly while the `4^n` base grows exponentially under
it. Any additive-on-top-of-exponential term has this problem.

The clean fix is to decouple onto a second, independent base entirely:
`Tickets = b^n * 2^(f(k,v,t))`, where `b` controls only how steeply waiting
matters (year-3 cohort convergence is ~23% regardless of b = 2, 3, or 4 --
confirmed again below -- so there's no backlog-clearing reason to prefer a
higher b; it's a political/visual choice about curve steepness, not a
substantive one), and the second factor is a *flat* multiplier on top,
independent of n. With `f = min(1, k)` (k capped at 1, e.g. via steps
0.5/0.75/1 over 3 finishes then reset to 0.5 forever), the veteran-specific
multiplier is hard-capped at exactly `2^1 = 2x`, by construction, matching
Garret's explicit design goal ("doubling is the ideal max benefit for being a
vet") -- and critically, since `2^(k+v+t) = 2^k * 2^(v+t)` by the laws of
exponents, k's cap bounds *only* the finisher-specific slice. Adding
uncapped v/t terms into the same written exponent does NOT break that cap --
it just rides a second, genuinely separate multiplicative factor. (I got this
backwards once in live fact-checking -- see "Mistake" below.)

**Fact-check of Garret's draft reply to Caleb.** Verified against the data:
the n-based odds table (99/27/5 men at n=2/3/4, matching `load_pool_2026()`
exactly), the 5-year cumulative P(in) table for all three bases (confirmed by
rerunning: current 6/14/23/35/49%, base3 3/11/23/43/68%, base4 2/9/23/49/80% --
exact match), and the auto-entry break years (2029-30 for try-4, 2032-33 for
try-5). Two real corrections surfaced and were sent back to Garret:
- "4^ makes it harder to get in by year 3" doesn't match the rerun or his own
  table -- year 3 is a near-exact three-way tie (23.1/23.1/23.3%). The honest
  framing is worse at years 1-2, roughly even at year 3, better at years 4-5
  for higher bases.
- "Veterans will have 3x/4x odds" under base 3/4 overstates the typical case:
  79% of finishers have exactly 1 finish (k=0.5), whose mechanistic multiplier
  is `base^0.5` (1.73x under base3, 2.00x under base4) -- the full 3x/4x
  figure only applies to the smaller 2-finish (k=1) group.

**Mistake (logged so I don't repeat it -- see `[[feedback-dont-overliteral-caps]]`
in my memory system):** I flagged Garret's draft as internally inconsistent --
"k maxes at 1 so veterans can't exceed 2x" next to "v should go sky-high,
uncapped" -- treating "2x cap" as a claim about the *combined* `2^(k+v+t)`
total. It isn't; it's scoped to the k-sourced factor alone, and
`2^k * 2^(v+t)` cleanly isolates that regardless of how large v+t gets. No
contradiction existed. Garret (correctly) called this out as over-literal
interpretation -- the same mistake, in miniature, as an earlier exchange this
same day where I treated "veterans doing trailwork" as some suspicious
causal-fairness problem instead of just two independent facts about a person.
Lesson: before flagging a design statement as internally inconsistent, do the
algebra / isolate the component the claim is actually scoped to, rather than
pattern-matching "capped" against "uncapped" in the same paragraph.

---

## 2026-10-13 — Caleb's full email, auto-entry refinements, dashboard debugging

### Bottom line

- **Got Caleb's actual email** (not just the 2026-09-14 paraphrase). Saved
  verbatim as `caleb_email_2026-09.md`, gitignored (personal correspondence,
  not for the public repo). Paraphrase holds up; extra, non-obvious detail:
  Garret had already suggested base 3 to Caleb before this email; Caleb's
  rationale for the two-component split (`4^n` + booster) is to keep "prior
  apps since reset" simple and front-and-center; his NB proportional-floor
  idea has a concrete worked example (2% of applicants -> >=2% of draw);
  refund changes need Kelsey's (half-owner) sign-off; he wants outside
  ethical input on the NB question, is only putting numbers on the table.
- **5th-try guarantee (n>=4), run for the first time against the
  panel-based model**: oversubscribes in **2032** (96 literal slots) /
  **2033** (118 effective slots) at 25% growth -- about 3 years more
  runway than the already-logged 4th-try guarantee (n>=3), which breaks
  2029/2030. See "Automatic-entry rule" section below for both thresholds
  side by side.
- **Year-3 convergence, re-verified at higher precision (200 reps):**
  current/base3/base4 land at 23.27% / 23.28% / 23.15% for a 2027 rookie
  -- close, not exactly equal; the "23% across the board" framing from the
  first-pass table is a rounding coincidence, not an identity. Still a
  real, structural finding (driven by fixed slots/year + most ticket mass
  going to the existing high-n backlog regardless of base), just don't
  oversell its precision to Caleb.
- **The backlog benefits immediately, no simulation needed.** Applying
  `3^(n+k)`/`4^(n+k)` to the actual 2026 pool (one year, no growth
  modeling) lifts today's 131 men at n>=2 right away: n=2 odds 19.4% ->
  24.5% -> 26.3%; n=3: 32% -> 48% -> 57%; n=4: 52% -> 77% -> 87%. This is
  a direct transfer from this year's rookie slots (36 -> 21 -> 13), not a
  multi-year effect -- useful to separate from the "does it help *future*
  rookies" question (it doesn't, see year-3 convergence above).
- **Spawned a background agent ("dashboardguy") to debug and extend
  `app.R`.** Its work is on a separate branch, `worktree-agent-a58abef50e243d5a8`,
  committed but not merged to main or deployed -- see that branch's own
  `ANALYSIS_LOG.md` entry (2026-10-12) for the full write-up: fixed a 2026
  data crash, a Monte Carlo `sample()` bug, unlabeled Women/Men columns,
  and added formula/auto-entry/growth-rate/year-3-convergence knobs to the
  dashboard. Left for Garret to review and merge/deploy himself.

### Automatic-entry rule, both thresholds, panel-based model, 25% growth

Share of men's slots claimed by the automatic tier, by year:

| Rule | 2027 | 2028 | 2029 | 2030 | 2031 | 2032 | 2033 |
|---|---|---|---|---|---|---|---|
| 4th-try-and-up (n>=3), 118 slots | 50% | 54% | 81% | **128%** | | | |
| 4th-try-and-up (n>=3), 96 slots | 61% | 69% | **106%** | | | | |
| 5th-try-and-up (n>=4), 118 slots | 11% | 16% | 23% | 37% | 58% | 93% | **135%** |
| 5th-try-and-up (n>=4), 96 slots | 14% | 20% | 31% | 50% | 80% | **121%** | |

Real-data cross-check on the 2026 pool itself: of men already at n=3 (4th
try), 12/28 (43%) weren't even waitlisted this year (6 drawn, 9
waitlisted, 1 preselect); of men at n=4 (5th try), 0/5 were lost outright
(2 drawn, 3 waitlisted) -- today's 4th-try cohort is still getting
meaningfully worse treatment than the odds table alone suggests, which is
part of why the 4th-try guarantee has so little runway before 100%.

### Non-binary applicants (Caleb's point 3) -- mechanism only, not the ethics

Caleb's two options: (1) separate NB pool, fixed floor of 4 guaranteed
spots; (2) NB stays in chosen binary pool, with a proportional floor
(NB's share of that pool's draw >= NB's share of that pool's applicants).
No NB applicant-count data exists anywhere in this repo or the public API
(gender fields are private) -- this is the real blocker, not the choice
of mechanism. Mechanically: option 2 is not the status quo ("just compete
in your chosen pool") -- it's an active, enforced minimum on top of it,
using the same reserved-slot mechanism the software already uses for
`Pro Runner`/`Legacy Runner`/etc. Without an enforced floor, NB
applicants' share of the draw is not guaranteed to match their share of
applicants, and probably runs below it for the same structural reason
rookies are underrepresented in the draw generally (no accumulated
tickets). Fixed quotas (option 1) risk a self-identification incentive if
sized generously relative to actual (unknown) NB demand, and go stale the
same way the current base has. Recommend getting the real NB count from
Caleb before sizing either option; deferred the ethical weighting
question to the outside group Caleb already said he wants to involve.

### Draft reply to Caleb, point 1 (base exponent) -- reviewed

Caught before it went out: draft cited the superseded first-pass cumulative
table (base4/yr5 at 89%, should be 80%; base3/yr5 at 75%, should be 68%)
and an unfilled "X men already applying 3+ times" -- confirmed X = 131
directly from the 2026 data. Suggested leading with the headcount argument
(131 men at n>=2 vs. 96 slots/year -- true regardless of any model) rather
than the "higher base = worse for rookies, that's what we want" tradeoff
framing, since the headcount point is unimpeachable and the tradeoff
framing risks reading as something tunable. Added the 2030-cohort numbers
(yr3: 9%/7%/6% current/base3/base4; yr5: 22%/29%/32%) so the reply doesn't
imply picking a base solves lottery creep once and for good.

---

## 2026-09-14 — 2027 lottery redesign: data, odds bug, cohort simulation

### Bottom line (as of end of day)

- **Data:** `lottery_panel.csv` is the authoritative source — every application
  2020–2026 by `runner_id` with outcome and whether they ran, pulled from the
  lottery site's public APIs. `2026HLdata.csv` is the 2026 pool in app format.
- **Odds bug:** `app.R`'s "exact" odds double-counted survival; fixed. Sum of
  odds must equal picks — use that check.
- **The formula can't shorten the queue.** ~118 men/yr get in (96 drawn + ~22
  off the waitlist). A steeper base (3 or 4) makes the wait orderly, not
  shorter. Base 4 gets a persistent 2027 rookie to 23% by year 3, 80% by
  year 5; for a 2030 rookie, 6% and 32%. Caleb's `4^n + n(k+v+t)` is a no-op
  vs `4^(n+k)` — put service and finishes in the exponent with caps instead.
- **Automatic entry on the 4th try** is ~50% of men's slots in 2027, ~80% in
  2029, and over 100% by 2030 (2029 if people respond to the guarantee).
  3rd-try is infeasible from day one.
- **Overlooked levers:** the dormant reservoir (717 men with tickets intact,
  never expires — consider attempt expiry after a gap); ~20% of drawn runners
  don't start (interacts with refund policy); the real fix for creep is a cap
  on inflow (qualifier / applicant cap).
- **Site security:** `getOfficialDrawing?…&withRunners=true` is public and
  returns runner profile fields the UI marks private (gender identity,
  birthdate, pronouns, emails, photo URLs). Caleb should have the developer
  restrict it. Nothing from those fields is in this repo.

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
