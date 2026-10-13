# Analysis log

Running notes on the lottery-redesign analysis. Newest entry first.

---

## 2026-10-13 (5) — Dashboard (app.R): year selector default corrected to 2026

**Status: local changes only, in a git worktree, NOT committed to main/pushed/deployed.**

Correction from Garret on entry (4) below: the previous round verified
`selected = "2025"` on `selectInput("year_select", ...)` and left it alone
as already-correct, but Garret actually wants the default to be **2026**,
not 2025. Changed `selected = "2025"` to `selected = "2026"` (line ~300 of
`app.R`). Re-rendered the real `ui` object (`as.character(ui)`) and
confirmed the served HTML now shows `<option value="2026"
selected>2026</option>`. One-line fix, verified the same way as last
round.

---

## 2026-10-13 (4) — Dashboard (app.R): fixed 3 UI issues Garret hit running the app in RStudio

**Status: local changes only, in a git worktree, NOT committed/pushed/deployed.**
Garret should review the diff and deploy himself.

Garret ran this worktree's app.R in RStudio (after the (3) decoupled-formula
entry below) and reported three UI problems. Investigated each before
touching anything, per usual practice on this branch (extend, don't
reimplement):

1. **"Default data year should be 2025, not whatever it currently
   defaults to."** Checked `selectInput("year_select", ...)` -- it already
   reads `selected = "2025"`, and no `updateSelectInput()` anywhere
   overrides it. Rendered the real `ui` object (`htmltools::doRenderTags`)
   and also ran the live app and curled its served HTML: both show
   `<option value="2025" selected>2025</option>`. **No code change was
   needed here** -- this was already correct in the (3) commit Garret was
   running. Likely explanation: Garret saw an older cached session, or
   conflated this with issue 2 below (see next item). Left as-is; verified
   working, not just assumed.

2. **"The ticket-formula display is static text... shows 2^(0+0+1) +
   2*log(0+0+1) no matter what."** Read `output$ticketFormula` closely: it
   already builds its string from `input$exp`, `input$mult`,
   `input$formula_family`, `input$odds_toggles`, and the Example Applicant
   Profile sliders (`apps`/`finishes`/`volunteer`/`trailwork`, via
   `derive_k_from_finishes()`), with a `switch()` arm per formula family
   including `garret_decoupled`'s `b^n * 2^(k+v+t)` shape -- fully dynamic,
   generic across all four families already. The catch: **every one of
   those sliders defaults to 0**, so on first load (before touching any
   slider) it renders exactly `Tickets = 2^(0 + 0 + 1) + 2 * log(0 + 0 +
   1)` -- precisely what Garret described. That's the formula correctly
   evaluated at the all-zero example profile, not a static string; it's
   indistinguishable from "hardcoded zeros" unless you actually move a
   slider. **No code change was needed here either** -- confirmed by
   running `shiny::testServer()` against the real server function (not a
   reimplementation) across 5 scenarios: `current` at defaults, `current`
   with apps=4/finishes=2/volunteer=7/trailwork=3 (text changed from the
   defaults case), `pure_power` at exp=4, `caleb` at exp=3/apps=3, and
   `garret_decoupled` at b=3/n=2/k=1/v=2/t=0.5 -- every family's displayed
   string correctly substituted the live values, and swapping
   `formula_family` alone (same example sliders) also changed the text.
   Logged here anyway, with the verification, since Garret's report named
   this as a problem and the fix is "move a slider and look again," not a
   code change.

3. **"Finish-Bonus Ramp explanation text should be collapsible."** This
   one was real: the `a4e6692` commit's helpText (the k ramp/floor
   explanation) rendered unconditionally, permanently taking up sidebar
   space. Fixed by reusing the EXACT toggle idiom already used for every
   other collapsible section in this app (`introPanel`, `avgOddsPanel`,
   `oddsPanel`, `oddsDistPanel`, `histAppsPanel`, `histFinPanel`,
   `projectionPanel`): wrapped the helpText in `hidden(div(id =
   "kRampInfoPanel", ...))` so it starts closed, and added an
   `actionLink("toggleKRampInfo", "ℹ️ More info on the ramp/floor shape")`
   bound to a new `observeEvent(input$toggleKRampInfo, {
   shinyjs::toggle(id = "kRampInfoPanel") })` in the server, right next to
   the near-identical `toggleIntro` observer. No new toggle mechanism
   introduced (no `bslib::accordion`, no `<details>`) -- this app already
   has one pattern for "collapsible section," so this just adds a seventh
   instance of it rather than an eighth kind of thing.

### Verification (all three, against the real app.R)

- Rendered the actual `ui` object to HTML (`htmltools::doRenderTags`) and
  separately launched the real app with `shiny::runApp()` on a local port
  and `curl`'d the served page -- both confirm `year_select` shows 2025
  selected, and show the new `toggleKRampInfo` link plus a
  `kRampInfoPanel` div with class `shinyjs-hide` (i.e. hidden by default,
  containing the "DROPS BACK DOWN to a floor" text, confirming the content
  survived the refactor and is only hidden, not deleted).
- Used `shiny::testServer()` against the real server function for issue 2,
  covering `current`/`pure_power`/`caleb`/`garret_decoupled` (4 of the 4
  families) at distinct slider settings, plus an explicit
  before/after-slider-move comparison within `current` and an explicit
  same-sliders-different-family comparison -- all showed the displayed
  formula text changing as expected, no family left static.
- Killed the temporary local app process after curling it; nothing left
  running.

### Files touched

Only `app.R`: added the `toggleKRampInfo`/`kRampInfoPanel` UI (sidebar,
"Finish-Bonus Ramp (k)" section) and its server `observeEvent`. No changes
to `lottery_projection.R` or any data file. Not merged to main, not
pushed, not deployed to shinyapps.io.

---

## 2026-10-13 (3) — Dashboard (app.R): added Garret's decoupled proposal as a selectable formula family

**Status: local changes only, in a git worktree, NOT committed/pushed/deployed.**
Garret should review the diff and deploy himself.

### What this is

Implements the `Tickets = b^n * 2^(k+v+t)` design from main's 2026-10-13
"Decoupled formula design; fact-checking Garret's draft reply to Caleb" log
entry as a fourth, selectable `formula_family` option in the dashboard
(alongside `current`/`pure_power`/`caleb` -- Caleb's own `4^n + n(k+v+t)`
proposal is untouched). `b` is NOT a new slider -- it reuses the existing
"Exponent / Base" slider every other family already shares, per the task's
explicit instruction, with a small `observeEvent(input$formula_family, ...)`
that nudges it (and `k_ramp`) to this family's own proposed defaults the
moment you select it. The `2^(...)` multiplier is a second, independent
base, ALWAYS fixed at 2 regardless of `b` -- its only job is bounding the
max "being a veteran" bonus at exactly 2x, by construction, per the
decoupling insight already worked out in that log entry
(`2^(k+v+t) == 2^k * 2^(v+t)`, so capping k alone is not broken by leaving
v uncapped in the same written exponent).

### k: reused the existing ramp/floor knob, just a different default

No new k-ladder was invented. `derive_k_from_finishes()` (built in this
worktree's earlier 2026-10-13 "generalized the finish bonus" commit,
`a4e6692`) already generalizes today's production k-rule into a
ramp/floor pair. For this family, the only change is the DEFAULT: the same
`observeEvent` above sets `k_ramp` to 2 (not 3) the instant you pick this
family, so the ladder becomes finishes 0/1/2/3+ -> k = 0/0.5/1/0.5(floor)
-- reaching the k=1 cap at 2 finishes, which is exactly what makes
`2^1 = 2x` the hard ceiling on the veteran bonus at 2 finishes. The
ramp/floor sliders stay visible and adjustable for every family, same as
before; this is a UI nudge on entry, not a second code path, and it does
NOT force the value back if you then move the slider yourself, nor does it
restore b/ramp to 2/3 when you switch away -- consistent with how this app
already treats every other shared slider (e.g. nothing resets "mult" when
you leave "current").

### v and t: new, independent scale/cap controls, only shown for this family

Added three new sidebar sliders (`v_scale`, `t_scale`, `t_cap`), inside a
`conditionalPanel` keyed to `formula_family == 'garret_decoupled'` so they
don't clutter the UI for the other three families, which don't use them.
The exponent contribution from service is `v/v_scale + min(t/t_scale,
t_cap)` -- volunteering uncapped (per Garret's explicit "sky high" request),
trailwork capped (per Garret's explicit "harder to verify and perhaps of
more variable quality in practice" reasoning).

**Defaults chosen, and why**, checked against the real `Volunteer_Points`/
`Extra_Trailwork_Points` distributions across all 4 data years (2023-2026):

- `v_scale = 4` (4 volunteer points = +1 in the exponent = another full 2x,
  matching the task's own suggested example). Checked against the data:
  median NONZERO volunteer total is only ~2.5-3.5 points across years (most
  applicants have 0), so a typical volunteer gets a modest ~1.5-1.8x bump,
  while the real tail is wide -- p99 is 14-25 points, max observed is 17-30
  -- giving multipliers from ~11x at p99 up to ~181x at the observed max
  (30 points / 2026's cap). That's genuinely "sky high" and uncapped, as
  asked, without needing a special-cased huge default scale.
- `t_scale = 8`, `t_cap = 0.5`. The cap value (0.5) is the task's own
  suggested example -- exactly what one race finish is worth, so no amount
  of trailwork can ever outweigh a single finish. The scale (8, saturating
  at 4 raw trailwork points) was chosen deliberately HIGHER than v_scale's 4
  -- i.e. each trailwork point earns only HALF the per-point exponent credit
  volunteering gets, on top of the hard cap -- so trailwork is doubly
  conservative (lower rate AND a ceiling), matching Garret's stated reason
  for treating it differently. Checked against the data: nonzero trailwork
  medians are ~1-1.25 points across years, so most real trailwork
  contributors (p50-p75) get real, graduated credit below the cap; only the
  heaviest contributors (roughly p85-p99+, well within the observed 0-10
  point range) are saturated. A naive `t_scale = 4` (matching v_scale) would
  have put the cap at just 2 raw points, saturating roughly HALF of all
  nonzero trailworkers immediately -- rejected as too aggressive to call
  "graduated," even though the end state (hard cap at 0.5) is intentional.
- Slider ranges: `v_scale`/`t_scale` 1-20 step 1 (min 1 to avoid a
  divide-by-zero), `t_cap` 0-2 step 0.25 (same step as the existing
  `k_floor` slider, range wide enough to push trailwork's cap well above
  one finish's worth if Garret wants to experiment with that).

### Wiring: no parallel calculation path

Followed the same pattern as the two prior rounds of work on this branch
(formula-family generalization, then k-ramp/floor generalization): extended
the ONE existing shared function at each layer, rather than adding a second
code path.

- `apply_service_toggles()` (app.R) gained `family`/`v_scale`/`t_scale`/
  `t_cap` parameters. For every other family it still returns exactly
  `volunteer + trailwork` (with the existing "drop trailwork" toggle),
  byte-for-byte the same as before. Only for `garret_decoupled` does it
  return the rescaled `v/v_scale + min(t/t_scale, t_cap)` instead -- this is
  the ONLY place v/t get rescaled; everywhere downstream just sees "the `s`
  argument whatever `formula_fn(n, k, s)` already expects," unchanged in
  shape.
- `build_ticket_formula()` (lottery_projection.R) gained one more `switch()`
  arm: `garret_decoupled = function(n, k, s) exp_val ^ n * 2 ^ (k + s)`. Same
  `(n, k, s) -> tickets` signature as every other family.
- Both call sites of `apply_service_toggles()` -- `base_data()` (real
  applicant pools) and `output$userTickets` (the Example Applicant Profile
  preview) -- now pass `input$formula_family`/`v_scale`/`t_scale`/`t_cap`
  through. `output$ticketFormula`'s live formula-text display got a new
  `switch()` arm showing the actual rescaled v/t contributions, so what's
  displayed always matches what's computed.
- The multi-year projection engine needed NO changes at all beyond the
  `build_ticket_formula()` arm above: `seed_groups_for_projection()` already
  reads its `s` straight off `base_data()`'s `service` column, which is
  already correctly family-rescaled by the time it gets there.

### Sanity checks (per task requirement 4)

Standalone checks of the formula math directly (not through Shiny):
- Rookie (k=0, s=0) at n = 0/1/2/3/5 under b=3: tickets = 1/3/9/27/243 =
  `b^n` exactly, as expected -- nonzero and sensible at every n.
- 2-finish veteran (k=1, the ramp=2 default's cap) vs. 0-finish (k=0) at the
  same n, s: ratio is exactly 2.0000 at n = 0, 2, and 4 -- confirms the 2x
  veteran cap holds regardless of n, by construction (`2^k` factors out of
  `2^n`... no, out of `2^(k+s)`, independent of the `b^n` term entirely).
- Large v (`v_scale=4`): v=0/4/8/20/30 -> multiplier vs. v=0 of
  1x/2x/4x/32x/181x -- confirms "sky high" is not accidentally capped; grows
  without bound as v grows.
- Large t (`t_scale=8, t_cap=0.5`): t=0/2/4/8/10/100/100000 -> multiplier
  1.00x/1.19x/1.41x/1.41x/1.41x/1.41x/1.41x -- confirms saturation at the
  cap (`2^0.5 = 1.414x`) by t=4 and staying there regardless of how large t
  gets afterward.

Then `shiny::testServer()` against the REAL app.R (not a reimplementation),
with `formula_family = "garret_decoupled"`:
- All 4 data years (2023-2026) at this family's defaults (`exp=3, k_ramp=2,
  k_floor=0.5, v_scale=4, t_scale=8, t_cap=0.5`): `base_data()$tickets` has
  no NAs, no non-positive values, on every year; `assigned_data()` has no
  NA odds for any `SelectionStatus == "Lottery"` row; every existing output
  (`ticketFormula`, `userTickets`, `kRampPlot`, both odds table pairs, all
  4 histograms, both odds-dist scatter plots, `autoSnapshot`) rendered
  without error on every year.
- Extreme slider combination (2026 data, `exp=5, k_ramp=6, k_floor=0,
  v_scale=1, t_scale=20, t_cap=2`, plus every discrete-rule toggle turned on
  at once and Monte Carlo odds): no NAs, no crashes; max ticket count
  reached ~1.3e8 (an extreme-but-finite number, as expected from stacking
  every slider at its extreme simultaneously) and all outputs still
  rendered.
- Degenerate scale values (`v_scale=1, t_scale=1, t_cap=0`, which would be
  `0/0 = NaN` if `trailwork` could be 0 AND `t_scale` could be 0 -- guarded
  against by the slider's own `min = 1`): confirmed `is.finite()` holds for
  every ticket value.
- A real `shiny::runApp()` launch on a local port with this family selected
  also returned HTTP 200.

The only warnings seen anywhere in this testing were the pre-existing,
already-documented-as-out-of-scope cosmetic warning from the 2026-10-12 log
entry ("Monte Carlo layer draws even in Deterministic mode, all-NA, dropped
by ggplot with a harmless warning") -- not something this change introduced
or needed to fix.

### Left for Garret to review before deploying

- The choice of `t_scale = 8` (vs. matching `v_scale`'s 4) is a judgment
  call, not something Caleb specified -- flagged above with the reasoning
  and the data check that motivated it; easy to change via one slider
  default if Garret disagrees.
- As with the k-ramp/floor work, the `observeEvent` that nudges `exp`/
  `k_ramp` to this family's defaults only fires going INTO this family, not
  coming back out -- so switching to `garret_decoupled` and back to
  `current` in the same session can leave `exp`/`k_ramp` at 3/2 instead of
  today's production 2/3, unless you reset them yourself. Not a bug in the
  new family's own math, just a minor shared-slider UX wrinkle worth
  knowing about if demoing both back-to-back for Caleb.

---

## 2026-10-13 — Dashboard (app.R): generalized the finish bonus (k) into a ramp/floor knob

**Status: local changes only, in a git worktree, NOT committed/pushed/deployed.**
Garret should review the diff and deploy himself.

### What this is

Caleb's "lotto 2.0" proposal raised the Hardrock 100 concern: at Hardrock,
10+-time finishers get near-guaranteed entry and newcomers have almost no
chance, which Garret considers ethically undesirable here. The current
formula already guards against that via the `k` (weighted finishes) term,
and Garret confirmed its piecewise shape -- k = 0/.5/1/1.5 for 0/1/2/3
finishes, then DROPS BACK DOWN to 0.5 for 4+ finishes -- is deliberate, not
a bug: it caps the maximum lifetime benefit a mega-veteran can ever get, so
a 10-time finisher earns no more ticket benefit than a 1-time finisher.

That piecewise rule collapses to one formula:

```
k(finishes) = 0.5 * min(finishes, RAMP),  if finishes <= RAMP
k(finishes) = FLOOR,                       if finishes >  RAMP
```

with today's production values RAMP = 3, FLOOR = 0.5. Added two sidebar
sliders (`k_ramp` 1-6, `k_floor` 0-1.5 step 0.25) so Garret/Caleb can
experiment with this shape directly, a small always-visible plot
(`output$kRampPlot`, finishes 0-10) comparing the chosen shape against
today's production rule side by side, and wired `derive_k_from_finishes()`
(now taking `ramp`/`floor_val` params, defaulting to today's values) into
every place that already computed k from a finish count -- `base_data()`
(the real applicant pools), the sidebar's "Example Applicant Profile"
(`ticketFormula` / `userTickets`), and therefore the multi-year projection
engine too, which already reads `k` off of `base_data()`'s output. No
second formula path was added; everything downstream (tickets, odds
tables, plots, projections) just sees a different `k` column when the
sliders move. Defaults reproduce today's exact rule -- verified
`k_ramp=3, k_floor=0.5` on 2026 data gives back `Weighted_Finishes` exactly
(`max(abs(sort(k) - sort(Weighted_Finishes))) == 0` via `testServer()`).

### The two-finish-count discrepancy (investigated as part of this)

The task flagged a real puzzle: 2026's official `Weighted_Finishes` field
has only 4 people (all men) at its max-bonus tier (1.5, i.e. 3 finishes),
but `lottery_panel.csv`'s `panel_prior_wins` shows far more people with
"3 lifetime wins" (10 men / 6 women) and "4+ lifetime wins" (3 men / 1
woman). Both counts checked out exactly against the data. The question was
which one is the right raw input for the new `k()`.

**Verified `panel_prior_wins` is the wrong field** -- despite the name, it
does not track race finishes. Cross-tabbing it against the panel's own
`status` history shows it tracks (almost exactly, modulo a small
pre-2020 offset) how many times someone was ever **selected** into the
race (`status %in% c("drawn","preselect")`), not how many times they
**finished** it:

```
panel_prior_wins - cumulative_times_selected_before_this_year
  0: 4657 rows   1: 117 rows   2: 8 rows   (never negative)
```

whereas cross-tabbing it against actual finishes (`ran == TRUE & result ==
"finished"`) is much messier and has far more slack. The giveaway: every
2021 entrant preselected via a "Covid Rollover" `preselect_reason` (i.e.
their 2020 race was cancelled and they were rolled over -- they never
ran it) already shows `panel_prior_wins = 1`. ~30% of people who start the
2020-2026 field don't finish (613 finishes out of 879 starts in the panel),
so `panel_prior_wins` systematically overstates true finishes and would
credit DNFs/no-shows as if they were veterans -- exactly backwards for a
formula whose entire point is capping *finish*-based benefit. Using it
would make the Hardrock-stacking problem worse, not better.

**Used instead: a reconstructed raw finish count**, built from the panel's
own `ran`/`result` fields (`ran == TRUE & result == "finished"`, counted
per `runner_id` for years strictly before 2026) --
`reconstruct_2026_raw_finishes()` in `app.R`. This disambiguates the one
place `Weighted_Finishes` is genuinely lossy: its 0.5 bucket is worn by
*both* 1-time finishers and 4+-time veterans alike (today's floor hides
exactly the distinction a wider ramp needs). Checked this reconstruction
against `Weighted_Finishes` directly: every unambiguous bucket
(`Weighted_Finishes` 0, 1, 1.5 implying 0, 2, 3 raw finishes) has a
panel-observed finish count <= the implied raw count, with the small gap
fully explained by pre-2020 finishes the panel window (2020-2026) can't
see -- a clean, structurally consistent result, unlike `panel_prior_wins`.
Of the 100 2026 entrants in the ambiguous 0.5 bucket, the panel resolves 4
of them (3 men / 1 woman -- the same `panel_prior_wins`-based count the
task description used as a sanity check, reached here by a different and,
per the analysis above, more defensible route) as true 4+/5-time
finishers; the rest default to "1 finish" (the modal case). For
2023-2025 data, which already carries a true raw count under
`Previous_Finishes`, none of this reconstruction is needed -- it's used
for the parametrized `k()`'s input unchanged.

Because the reconstruction only matters to break the 0.5 bucket apart, and
every person in that bucket still lands back on `FLOOR` under the default
`RAMP = 3` regardless of whether they're reconstructed as exactly 4 or 5,
the default-reproduction guarantee holds regardless of any remaining
uncertainty in the reconstruction itself.

**Open item for Caleb/the developer:** worth asking the lottery site's
admin export to include a raw lifetime finish count directly (or rename
`panel_prior_wins` / clarify what it tracks) -- reconstructing it from
`ran`/`result` history works but is more fragile than having the real
number.

### Testing

`shiny::testServer()`: confirmed `k_ramp=3, k_floor=0.5` (defaults)
exactly reproduce `Weighted_Finishes` on 2026 data; confirmed moving the
sliders changes `k`, `tickets`, and downstream odds on years where
raw finishes exceed the new ramp (2026, 2024, 2023) and correctly has *no*
effect on 2025 (verified separately: no 2025 applicant has more than 3 raw
finishes, so widening the ramp past 3 has nothing to bite on that year --
not a bug); rendered every existing output (`kRampPlot`, `ticketFormula`,
`userTickets`, both odds table pairs, all four histograms, both odds-dist
scatter plots, the snapshot/stats text) with both default and extreme
(`k_ramp=6, k_floor=0`) slider settings -- all rendered without error, and
the resulting `k` column at the extreme setting (0, 0.5, 1, 1.5, 2, 2.5 for
raw finishes 0-5) matched the formula by hand. Also did a real
`shiny::runApp()` launch on a local port and confirmed HTTP 200.

---

## 2026-10-13 — Dashboard (app.R): fixed "Ticket vs Odds" points piling up at 100%

**Status: local changes only, in a git worktree, NOT committed/pushed/deployed.**
Garret should review the diff and deploy himself.

### Bug report

Garret noticed an unexpectedly large number of the "Distribution of Odds
(Tickets vs Odds)" dot plots' Monte Carlo-simulated points sitting at or
near 100%, when only applicants with enormous ticket counts relative to the
pool should ever approach certainty.

### Root cause: category error, not an odds-math bug

`oddsDistW`/`oddsDistM` plot `assigned_data()` for the gender with **no
filter on `SelectionStatus`**. But `assigned_data()` hard-sets
`Odds_Exact <- 1` / `Odds_MC <- 1` for every `"Auto-Selected"` row and
`<- 0` for every `"Removed"` row -- these are categorical overrides, not
outputs of `calc_lottery_odds()`/`simulate_odds()` at all (those two
functions are only ever called on `SelectionStatus == "Lottery"` rows).
`"Auto-Selected"` covers two real things: (a) 2026 preselects (29 M / 11 F,
*always* marked this way whenever 2026 data loads, independent of any
slider -- see the 2026-10-12 entry below) and (b) anyone qualifying under
the sidebar's "Guaranteed-entry rule" (`auto_at`) when one is active. With
no filter, every one of those applicants got plotted as a point at exactly
y = 100% in the *same* "Direct"/"Monte Carlo" series as genuinely-drawn
applicants, at whatever (often perfectly ordinary) ticket count their
profile happened to produce -- nothing to do with having an enormous
ticket count.

Quantified with a standalone reproduction (`base_data()`/`processed_data()`
logic re-run outside Shiny against `2026HLdata.csv`): with the 3rd-try
guarantee (`auto_at = "2"`) active on 2026 men, **17.1% of all points that
would be plotted (160 of 935) were hard-set to 100% this way** -- and their
ticket-count range (median 8, up to 45) overlapped the *same* range as
genuinely-drawn lottery applicants (median 2, up to ~11), confirming these
weren't legitimately-high-ticket people, just guaranteed-in people with
ordinary ticket counts. Even with no `auto_at` rule at all, 2026 preselects
alone contaminate 29 men's / 11 women's points every time 2026 data is
viewed.

Checked the other hypotheses and ruled them out:
- **Not a Monte Carlo denominator bug.** `simulate_odds()` correctly
  computes P(drawn in one simulated draw) -- `counts[picks] <- counts[picks]
  + 1` per rep, divided by `n_sim` at the end -- not P(ever drawn across
  reps). Confirmed `calc_lottery_odds()` and `simulate_odds()` agree closely
  on the real `SelectionStatus == "Lottery"` subset.
- **Not a small-pool mean-field artifact.** Restricting to genuine
  `"Lottery"`-status 2026 men (906 of them, 95 picks, the default formula),
  the *highest* odds anyone legitimately reaches is ~72%
  (`sum(calc_lottery_odds(...)) == 95` as the sanity check, matching
  `simulate_odds()` to within simulation noise) -- nowhere near 100%, and
  nobody is close. So "legitimately high odds from legitimately high
  tickets" was not happening anywhere in the data checked; every point at
  or near 100% was a categorical override leaking into the chart.

### Fix

Filter both plots to `SelectionStatus == "Lottery"` before plotting --
this chart's whole purpose is comparing the two *draw-probability*
calculation methods against each other, which is meaningless for
applicants who bypass the draw entirely. Added a subtitle/caption to each
plot reporting how many applicants were excluded, so the exclusion is
visible rather than silent (Caleb/Garret can still see e.g. "160 of 935 men
excluded" and know the guarantee rule is doing a lot of work, just not on
this particular chart).

### Related bug found via regression testing (same code path)

While testServer-regression-testing the fix across all 4 years x 4
`auto_at` settings x 2 odds methods, found that when the guaranteed-entry
tier already equals or exceeds `Nm`/`Nw` on its own (`n_picks_m`/`n_picks_w`
computed as 0), the remaining genuine `"Lottery"`-status applicants were
left at `Odds_Exact`/`Odds_MC`'s `NA_real_` default instead of their true
value, which is exactly 0 (nobody is left to draw). ggplot silently drops
NA points, so this wasn't the reported "piling up at 100%" symptom, but
it's the same code region (`assigned_data()`'s picks<=0 branch, previously
missing) and is wrong in the same spirit -- fixed by setting
`Odds_Exact[idx] <- 0; Odds_MC[idx] <- 0` when `length(idx) > 0` but
`picks <= 0`. Reproduces with 2026 men + `auto_at = "2"` (3rd try): 775
genuine lottery men, all correctly 0%, zero `NA`s, instead of 775 silently
dropped `NA` points.

### Testing

Standalone `Rscript` reproduction against `2026HLdata.csv` (outside Shiny)
to quantify the contamination and separately confirm the mean-field/Monte
Carlo odds themselves are not the problem (see Root cause above). Then
`shiny::testServer()` regression across all 4 data years x all 4 `auto_at`
settings (`none`/`2`/`3`/`4`) x both `deterministic` and `mc1000` odds
methods, asserting for each: every plotted point's relevant odds column is
non-`NA` and <= 1, and every plotted row's `SelectionStatus` really is
`"Lottery"` (nothing categorical leaked through the new filter). Also
rendered `output$oddsDistM`/`output$oddsDistW` directly in each scenario to
confirm no rendering errors. All scenarios passed after both fixes above.

### Does this change how the dashboard's other plots should be read?

No -- this was specific to `oddsDistW`/`oddsDistM`. The "Expected # Picked"
bar charts (`histW`/`histM`/`finHistW`/`finHistM`) correctly include
Auto-Selected/Removed applicants' hard-set odds in their `sum()`, because
those charts are about total expected picks across *everyone*, guaranteed
or not -- excluding the guaranteed tier there would under-count real
picks. Only the tickets-vs-odds scatter, whose specific job is comparing
the two probabilistic-draw calculations against each other, needed the
exclusion. The pre-existing cosmetic "Monte Carlo layer draws even in
Deterministic mode, all-NA, dropped with a harmless warning" issue noted in
the 2026-10-12 entry is unrelated and still not fixed (out of scope here
too).

---

## 2026-10-12 — Dashboard (app.R): fixed 2026 data crash, added interactive knobs

**Status: local changes only, in a git worktree, NOT committed/pushed/deployed.**
Garret should review the diff and deploy himself.

### Update: two issues Garret hit running the app in RStudio

- **"error: incorrect number of probabilities" in the Monte Carlo odds
  method.** This is a classic R footgun, pre-existing in `simulate_draw()`
  (not introduced by this session's other changes): `sample(x, 1, prob =
  probs)` behaves differently when `x` has length 1 -- R treats a
  length-1 *numeric* `x` as "draw from `1:x`", not "this one candidate
  already identified," so a length-1 `prob` then doesn't match the
  length R expects internally and it throws. This fires whenever a draw
  runs all the way down to exactly one remaining candidate -- i.e.
  whenever "Number of Men/Women to Pick" is close to or exceeds the
  "Lottery"-eligible pool for that year. Fixed by special-casing
  `length(remaining) == 1` to take that candidate directly instead of
  calling `sample()` at all. Verified with direct unit tests of
  `simulate_draw()`/`simulate_odds()` at the exhaustion boundary (pools of
  length 1, 2, 5, 6, with exact/over-asked pick counts, and a zero-ticket
  case), then through the full app pipeline forcing exhaustion on the
  smallest real pool (2023 women, N=151) across all 3 formula families x
  all 4 automatic-entry thresholds, then a happy-path regression check
  (realistic Nm/Nw defaults, no forced exhaustion) across all 4 years --
  all passed. (Noted but out of scope: `simulate_draw()`'s loop-based
  approach is O(picks x pool) per replication and gets slow near full-pool
  exhaustion; CLAUDE.md documents a faster exact method,
  `order(rexp(N)/tickets)[1:n_picks]`, already used in `cohort_sim.R`,
  that doesn't have this bug at all -- worth swapping in if Monte Carlo
  speed becomes a real problem, not done here to keep this fix minimal.)
- **Side-by-side Women/Men columns had no header.** The two
  `fluidRow(column(6, ...), column(6, ...))` blocks under "Lottery Odds
  Summary" and "Detailed Odds" (the `avgOddsW`/`avgOddsM` and
  `oddsW`/`oddsM` DataTables) had nothing distinguishing which side was
  which pool -- unlike the plots further down, which already had ggplot
  titles. Added "Women (F)" / "Men (M)" headers above both table pairs,
  and added matching "Women"/"Men" plot titles to `histW`/`histM` and
  `finHistW`/`finHistM`, which had the same ambiguity (only distinguished
  by bar color).

### Bugs found and fixed in `app.R`

- **Selecting "2026" in the Data Year dropdown crashed the app outright.**
  `base_data()` unconditionally expected a `Previous_Finishes` column, but
  the 2026 export doesn't carry one -- it supplies the already-weighted `k`
  value directly under `Weighted_Finishes` (see this file's 2026-09-14
  entry). Now branches on which column is actually present.
- **40 "denied" 2026 applicants have no pool assignment at all**
  (`Lottery Pool` is `NA` for all of them -- they were rejected before ever
  being assigned a pool). Left in, any logic that counts people by gender
  (e.g. the new automatic-entry rule below) silently produces `NA` instead
  of a slot count. Now excluded in `base_data()`, matching
  `cohort_sim.R`'s `load_pool_2026()`, which already excludes them.
- **2026 preselects (29 M / 11 F) were competing in the ordinary lottery
  pool** instead of being treated as already-guaranteed, unrelated to the
  ticket formula -- inconsistent with this log's own "125 per pool incl.
  pre-selects -> 96/114 lottery picks" framing. Now marked
  `SelectionStatus == "Auto-Selected"` up front, same as a guaranteed-entry
  rule, so `Nm`/`Nw` sliders correctly mean lottery-only slots.
- Removed a duplicated `checkboxInput("no_firsttime_zero", ...)` (same
  input registered twice, copy-paste artifact).
- `textOutput("avgAppsStats")` / `textOutput("avgFinishesStats")` existed
  in the UI with no server-side `renderText` behind them (rendered blank).
  Implemented both.
- Removed `calc_deterministic_odds()`, a dead function (never called --
  `assigned_data()` calls `calc_lottery_odds()` directly) that also did a
  pointless `digest::digest()` into an attribute nothing ever read; trimmed
  the same dead pattern out of `memo_simulate_odds()`.
- Confirmed `calc_lottery_odds()`'s recursion already matches this file's
  documented September 2026 fix (survival is not double-counted) --
  no change needed there.
- `checkboxGroupInput("odds_toggles", ...)` ("Drop trailwork", "Drop log
  and +1") existed in the UI but `input$odds_toggles` was never read
  anywhere in the server -- wired both up (see `build_app_formula()` /
  `apply_service_toggles()` in `app.R`).

### New interactive knobs

Per Garret's request, audited the existing UI first rather than adding
parallel controls:

1. **Formula selector -- extended existing controls.** The `exp`/`mult`
   sliders already let you vary the current formula's base and service
   multiplier; they could not express a formula with no service term at
   all (e.g. `3^(n+k)`) or Caleb's proposal. Added one
   `selectInput("formula_family", ...)` (current / pure power / Caleb)
   that reuses the existing `exp` slider as the base for all three
   families, rather than adding a second base control. Shared with the new
   projection engine via `lottery_projection.R`'s `build_ticket_formula()`.
2. **Automatic-entry guarantee -- generalized an existing control.**
   `radioButtons("threeplus_rule", ...)` already implemented a guarantee,
   hardcoded at n>=3 ("3+ year applicant"). Replaced with
   `selectInput("auto_at", ...)` (3rd/4th/5th try, i.e. n>=2/3/4),
   preserving the old n>=3 behavior as one of the choices, plus a small
   always-on "Guaranteed-Entry Snapshot" text block showing how many
   applicants already qualify vs. slots available for whichever threshold
   is selected -- no projection/button needed for that part, it's cheap.
3. **Growth-rate scenario selector -- new** (10% / 25% / 30% per year,
   matching `cohort_sim.R`'s three scenarios).
4. **Year-3-convergence view -- new.** A chart comparing current vs.
   base-3 vs. base-4's cumulative "persistent rookie" odds by year, to
   make visible in the dashboard itself that the three formulas are nearly
   indistinguishable through year 3 and only diverge from year 4 on.

(3) and (4), plus a companion "when does the guarantee become
oversubscribed" chart for (2), are powered by a new file,
`lottery_projection.R`: a fast, deterministic (expected-value, not Monte
Carlo) multi-year projection engine, since the real `cohort_sim.R`
simulation takes ~3 minutes and can't sit behind a dashboard slider. It
tracks population by `(n, k, s)` group instead of by individual, which
is mathematically identical to the individual-level recursion when
everyone in a group is interchangeable, and runs in milliseconds.
**Deliberately simplified** relative to `cohort_sim.R` (see that file's
header comment for the full list): no dormant-reservoir/comeback
modeling, new rookies modeled as one homogeneous k=0/s=0 group, no
service-point accumulation, fixed (non-adjustable) retention rates taken
from this log's panel-derived figures. This makes it systematically
*optimistic* relative to this log's dormancy-adjusted numbers -- use
`cohort_sim.R` for anything that needs to match this write-up exactly.
Validated against this log's first-pass cohort-sim table (before the
dormancy revision) and landed within a few points on every formula/year
(e.g. base 4 at year 5: 86% here vs. 89% there), with the year-3
convergence point holding even tighter (19-21% here vs. 22-24% there).
Both the new engine's outputs and all 2026 data use the same real
person-level data already used to run the Monte Carlo odds elsewhere in
the app; nothing synthetic was introduced.

Projections are gated behind an explicit "Run multi-year projection"
button (`eventReactive`, not `reactive`) so an 8-year loop doesn't re-run
on every slider tick, per this file's RAM/speed guidance.

### Testing

All packages (`shiny`, `dplyr`, `DT`, `bslib`, `ggplot2`, etc.) loaded
fine in this environment (R 4.3.3) -- the "library mismatch" gotcha noted
elsewhere in this file did not apply here. Tested with `shiny::testServer()`
(simulated inputs, no browser) across all 4 data years, all 3 formula
families, all 4 automatic-entry settings, and the projection button; also
did a real `shiny::runApp()` launch on a local port and confirmed it serves
HTTP 200. The `testServer` run is what surfaced the "denied" applicants'
`NA`-gender bug above -- it wasn't visible just from reading the code.

### Left for Garret to review before deploying

- The projection engine's simplifications (no dormancy, homogeneous
  rookies) are a deliberate speed/accuracy tradeoff for an interactive
  tool; if the exact dormancy-adjusted numbers matter for a given
  conversation with Caleb, point him at this log / `cohort_sim.R` instead.
- Didn't touch the pre-existing cosmetic warning where the "Ticket vs
  Odds" scatter plots always draw a "Monte Carlo" layer even when
  Odds Calculation Method is "Deterministic" (all-NA, silently dropped by
  ggplot with a harmless warning) -- out of scope for this pass.
- Growth rate and automatic-entry threshold are dashboard knobs; the
  underlying retention rates are not (kept as fixed, documented constants)
  to keep the UI focused on what this conversation specifically asked for.

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
