## app.R -- High Lonesome 100 lottery design dashboard
##
## Lets Caleb (the RD) explore how different ticket-weighting formulas,
## automatic-entry guarantee rules, and applicant-pool growth assumptions
## would change who gets drawn into the race. See ANALYSIS_LOG.md for the
## narrative write-up this dashboard is meant to make interactively
## explorable, and CLAUDE.md for the domain facts (what each column means,
## how the formula works today, etc).

library(shiny)
library(dplyr)
library(tibble)
library(DT)
library(bslib)
library(ggplot2)
library(shinyjs)
library(markdown)
library(memoise)  # Only for Monte Carlo memoization
library(digest)   # Only for Monte Carlo memoization

# lottery_projection.R holds the multi-year projection engine (the "Run
# multi-year projection" feature below) and the shared build_ticket_formula()
# used both there and in this file's single-year odds tables, so the
# formula-family selector drives both consistently instead of risking two
# separate implementations drifting apart. See that file's header comment
# for the model's assumptions and known simplifications.
source("lottery_projection.R")

# Function: Odds calculation (weighted draw without replacement).
# Mean-field recursion: at each draw, P(i picked | i still in) ~= t_i / E[tickets remaining].
# Sum of the returned odds equals n_picks. Matches Monte Carlo to ~0.003 on pools this size
# and is the same algorithm the lottery site uses for its published odds.
# (Verified against ANALYSIS_LOG.md's documented fix: this already uses
# `survivors * (1 - tickets/total_tix)`, i.e. survival is NOT double-counted,
# so the September 2026 bug described in the log is not present here.)
calc_lottery_odds <- function(tickets, n_picks) {
  if (length(tickets) == 0 || n_picks <= 0) return(numeric(0))
  survivors <- rep(1, length(tickets))   # P(i not yet drawn)
  for (draw in seq_len(n_picks)) {
    total_tix <- sum(tickets * survivors)
    if (total_tix == 0) break
    survivors <- survivors * (1 - tickets / total_tix)
  }
  1 - survivors
}

# Function: Simulate one draw (Monte Carlo)
simulate_draw <- function(tickets, n_picks) {
  selected <- integer(0)
  remaining <- seq_along(tickets)
  remaining_tickets <- tickets
  for (i in seq_len(n_picks)) {
    if (length(remaining) == 0) break
    # When exactly one candidate is left, `remaining` is a length-1
    # NUMERIC vector -- and R's sample(x, ...) treats a length-1 numeric x
    # as "draw from 1:x", not "this one candidate", regardless of `prob`.
    # That mismatch (prob has length 1; sample() expects length `x`) is
    # exactly what throws "incorrect number of probabilities". Since
    # there's only one candidate anyway, just take it directly.
    if (length(remaining) == 1) {
      pick <- remaining
    } else {
      probs <- remaining_tickets / sum(remaining_tickets)
      pick <- sample(remaining, 1, prob = probs)
    }
    selected <- c(selected, pick)
    remaining_tickets <- remaining_tickets[remaining != pick]
    remaining <- remaining[remaining != pick]
  }
  selected
}

# Function: Monte Carlo odds over many simulations
simulate_odds <- function(tickets, n_picks, n_sim = 100000) {
  counts <- numeric(length(tickets))
  for (i in seq_len(n_sim)) {
    picks <- simulate_draw(tickets, n_picks)
    counts[picks] <- counts[picks] + 1
  }
  counts / n_sim
}

# derive_k_from_finishes(): converts a RAW finish count into the lottery's
# "weighted finishes" (k) ticket bonus, GENERALIZING the piecewise rule
# Garret confirmed is deliberate (not a bug): k ramps up for a while, then
# DROPS BACK DOWN to a floor once you pass that ramp -- so a 10-time
# finisher (think Hardrock 100's near-guaranteed veterans) never earns more
# lifetime benefit than someone who has finished exactly `ramp` times.
# Today's production rule is ramp = 3, floor = 0.5, step 0.5/finish (0/1/2/3
# finishes -> k = 0/.5/1/1.5, then 4+ finishes drops back to k = 0.5):
#
#   k(finishes) = step * min(finishes, ramp),  if finishes <= ramp
#   k(finishes) = floor,                        if finishes >  ramp
#
# `step` is k_cap / ramp -- the per-finish increment needed to reach the cap
# exactly at `ramp` finishes. `k_cap` defaults to NULL, meaning "reproduce
# today's rule" (k_cap <- 0.5 * ramp, giving step = 0.5, exactly like before
# this parameter existed) -- so any caller that doesn't pass k_cap
# explicitly behaves exactly as before.
#
# 2026-10-13 (Max Boost slider): added the `k_cap` parameter so Garret's
# decoupled proposal can make the cap itself adjustable (via a "Max finish-
# bonus boost (multiplier)" slider, k_cap = log2(max_boost)) instead of the
# hardcoded cap of k=1 (-> 2x) from the a4e6692 commit. See
# ANALYSIS_LOG.md's 2026-10-13 entry for the derivation. Every OTHER family
# still calls this with k_cap left at its NULL default, so nothing about
# their behavior changed.
#
# The `ramp`/`floor` defaults below reproduce today's exact rule, so any
# caller that doesn't pass them explicitly (or the sidebar's k_ramp/k_floor
# sliders, when left at their defaults) behaves exactly as before. See the
# sidebar's "Finish-Bonus Ramp (k)" section and output$kRampPlot for the
# interactive version of this.
#
# Needed for the 2023-2025 data files, which carry a raw Previous_Finishes
# count directly. The 2026 export does NOT carry a raw count at all -- it
# supplies an already-k-mapped value under a column literally named
# Weighted_Finishes, lossy above 3 finishes (it can't tell a 1-time
# finisher from a 10-time finisher apart, both show 0.5 under today's
# rule) -- see reconstruct_2026_raw_finishes() below for how base_data()
# recovers an approximate raw count for 2026 so this slider still does
# something on that year's data. (Mixing up which column is raw vs.
# already-mapped, by always expecting a raw Previous_Finishes column, is
# what used to make selecting "2026" in the Data Year dropdown crash the
# app outright.)
derive_k_from_finishes <- function(previous_finishes, ramp = 3, floor_val = 0.5, k_cap = NULL) {
  ramp <- as.numeric(ramp)
  floor_val <- as.numeric(floor_val)
  if (is.null(k_cap)) k_cap <- 0.5 * ramp  # reproduce today's rule exactly
  k_cap <- as.numeric(k_cap)
  step <- k_cap / ramp
  ifelse(previous_finishes <= ramp,
         step * pmin(previous_finishes, ramp),
         floor_val)
}

# garret_k_cap(): the one place that turns the "Max Boost" slider's
# intuitive unit (a multiplier, e.g. 2x/4x) into the exponent unit
# derive_k_from_finishes() actually needs (k_cap, such that 2^k_cap ==
# max_boost). Only "garret_decoupled" uses an adjustable cap at all -- every
# other family gets NULL back, which derive_k_from_finishes() already
# treats as "use today's implicit cap" (see above). Centralizing this here
# means every one of this family's derive_k_from_finishes() call sites
# (ticketFormula, kRampPlot, base_data(), userTickets) computes the cap
# identically -- a second, slightly-different copy of this ternary at each
# call site is exactly the kind of drift this app's existing "one shared
# function per concept" convention is meant to avoid.
garret_k_cap <- function(family, max_boost) {
  if (identical(family, "garret_decoupled")) log2(as.numeric(max_boost)) else NULL
}

# reconstruct_2026_raw_finishes(): recovers an approximate RAW lifetime
# finish count for 2026 applicants, who only carry the already-k-mapped
# Weighted_Finishes column (see derive_k_from_finishes() above and
# ANALYSIS_LOG.md's 2026-10-13 entry for the full writeup of this
# investigation).
#
# Under TODAY'S rule, Weighted_Finishes == 0, 1, and 1.5 are unambiguous --
# they can only come from exactly 0, 2, or 3 raw finishes respectively. But
# Weighted_Finishes == 0.5 is ambiguous: it's the floor value, so it's worn
# by BOTH genuine 1-time finishers AND every 4+-time veteran alike -- today's
# rule hides the very distinction a wider ramp needs to see. To unmask the
# real veterans in that bucket, this joins to lottery_panel.csv (by
# runner_id) and counts each person's lifetime race finishes actually
# observed within the panel (ran == TRUE & result == "finished", strictly
# before 2026).
#
# IMPORTANT, and the reason this is a *reconstruction* and not a second
# source of truth: we checked this against the OTHER candidate panel
# column, panel_prior_wins, and rejected it. panel_prior_wins tracks (to
# within pre-panel-history noise) how many times someone was ever SELECTED
# into the race (status drawn/preselect) -- not how many times they
# actually FINISHED it. Those are materially different: ~30% of people who
# start the race don't finish, and some selected people never start at
# all, so panel_prior_wins systematically overstates true finishes and
# would credit non-finishers as if they were veterans -- exactly backwards
# for a formula whose whole point is capping veteran benefit based on
# actually finishing. The panel's own `ran`/`result` fields (true finishes,
# computed here) line up with Weighted_Finishes almost perfectly (verified
# against 2026HLdata.csv: every unambiguous Weighted_Finishes bucket's
# panel-observed finish count is <= the bucket's implied raw count, with
# the small gap fully explained by pre-2020 finishes the panel can't see),
# while panel_prior_wins does not. So: raw finishes, not panel_prior_wins,
# is what feeds the k() ramp here.
#
# Because this only matters to disambiguate the 0.5 bucket, and anyone
# reconstructed as 4+ there still lands on the floor under today's
# defaults (ramp = 3) exactly like Weighted_Finishes == 0.5 always did,
# this reconstruction is exact-reproducing regardless of any uncertainty
# left in distinguishing e.g. a 4-time from a 5-time finisher.
reconstruct_2026_raw_finishes <- function(weighted_finishes, runner_id,
                                           panel_path = "lottery_panel.csv") {
  # Whatever Weighted_Finishes unambiguously implies under today's rule;
  # the 0.5 bucket defaults to "1" (the modal case -- see ANALYSIS_LOG.md)
  # unless the panel join below finds evidence of a real 4+-time veteran.
  implied <- dplyr::case_when(
    weighted_finishes == 0   ~ 0,
    weighted_finishes == 1   ~ 2,
    weighted_finishes == 1.5 ~ 3,
    weighted_finishes == 0.5 ~ 1,
    TRUE ~ NA_real_
  )

  panel <- tryCatch(read.csv(panel_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(panel) || !all(c("year", "runner_id", "ran", "result") %in% names(panel))) {
    return(implied)  # degrade gracefully if the panel file is missing/malformed
  }

  true_finishes <- panel %>%
    dplyr::filter(year < 2026, ran == TRUE, result == "finished") %>%
    dplyr::count(runner_id, name = "n_finishes")
  lookup <- setNames(true_finishes$n_finishes, true_finishes$runner_id)
  panel_true <- unname(lookup[runner_id])
  panel_true[is.na(panel_true)] <- 0

  ifelse(weighted_finishes == 0.5 & panel_true >= 4, panel_true, implied)
}

# apply_service_toggles(): the sidebar's "Drop trailwork (keep
# volunteering)" checkbox only makes sense as a decision about which raw
# point totals feed INTO the formula, so it's applied here, once, rather
# than separately in every place that needs a service total.
#
# GENERALIZED (2026-10-13, same session as the k ramp/floor knob) to also
# produce the "garret_decoupled" family's exponent contribution, per
# ANALYSIS_LOG.md's "Decoupled formula design" entry. Every OTHER family
# (current / pure_power / caleb) still gets back exactly `volunteer +
# trailwork` as before -- this function's return value is always just "the
# `s` argument whatever formula_fn(n, k, s) expects", and for those three
# families that's the simple raw-point sum, unchanged.
#
# For "garret_decoupled" specifically, the raw points are rescaled BEFORE
# summing, per Garret's explicit design split:
#   - volunteering: v / v_scale, deliberately left UNCAPPED ("sky high" is
#     the design goal -- a big volunteer total should keep paying off, not
#     flatten out).
#   - trailwork: min(t / t_scale, t_cap), deliberately CAPPED -- Garret's
#     own reasoning is that trailwork is "harder to verify and perhaps of
#     more variable quality in practice" than volunteering, so its
#     contribution is bounded no matter how large the raw point total gets.
# Doing the rescale here (rather than inside build_ticket_formula()) means
# build_ticket_formula()'s garret_decoupled case can stay a plain
# `exp_val^n * 2^(k+s)`, identical in shape to how every other family
# already consumes `s` -- no second/parallel calculation path, just a
# family-aware version of the one function that already turns raw
# volunteer/trailwork points into "the thing the formula adds".
#
# v_scale/t_scale/t_cap are only ever read when family == "garret_decoupled"
# (the sidebar only shows those sliders then too), so their defaults here
# are irrelevant to every other family -- kept only so this function has a
# sane signature if ever called without them.
apply_service_toggles <- function(volunteer, trailwork, toggles, family = "current",
                                   v_scale = 4, t_scale = 8, t_cap = 0.5) {
  if ("drop_trail" %in% toggles) trailwork <- 0
  if (identical(family, "garret_decoupled")) {
    volunteer / v_scale + pmin(trailwork / t_scale, t_cap)
  } else {
    volunteer + trailwork
  }
}

# build_app_formula(): wraps lottery_projection.R's build_ticket_formula()
# to also honour the "Drop log and +1 (use integer points)" checkbox, which
# only makes sense for the "current" family (the other two families --
# pure power, and Caleb's proposal -- don't use a log term at all, so the
# toggle has no effect there; that's intentional, not a bug, and is called
# out in the UI help text). Keeping this wrapper in app.R (rather than
# lottery_projection.R) keeps the three canonical formulas in that file
# exactly matching the ones documented in ANALYSIS_LOG.md, uncomplicated by
# this app-only interactive toggle.
build_app_formula <- function(family, exp_val, mult_val, toggles) {
  if (identical(family, "current") && "no_log" %in% toggles) {
    function(n, k, s) exp_val ^ (n + k + 1) + mult_val * s
  } else {
    build_ticket_formula(family, exp_val, mult_val)
  }
}

default_nw <- 103
default_nm <- 95

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "minty"),
  tags$head(
    tags$style(HTML("
      .intro-logo { display: flex; align-items: center; justify-content: flex-start; margin-bottom: 18px; }
      .intro-logo img { height: 80px; margin-right: 24px; border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.15); background: #222A2A; padding: 8px; }
      .intro-title { font-size: 2.1em; color: #B7822E; font-weight: bold; margin-bottom: 8px; }
      .odds-cols { display: flex; gap: 32px; }
      .odds-col { flex: 1 1 0; }
      .dt-compact-custom { font-size: 12px !important; }
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate { font-size: 12px !important; }
      .collapsible-btn { margin-bottom: 10px; background: #e3e3e3; border: 1px solid #bbb; color: #333; font-weight: bold; }
      .collapsible-title { font-size: 1.15em; font-weight: bold; }
      .formula-box { background: #f4f4f4; padding: 10px 18px; border-radius: 8px; font-size: 1.05em; margin-bottom: 10px; }
      .formula-highlight { color: #A97B1C; font-weight: bold; }
      .odds-table-compact { font-size: 11px !important; }
      .dataTables_wrapper .odds-table-compact .dataTables_scrollHeadInner,
      .dataTables_wrapper .odds-table-compact .dataTables_scrollBody { max-width: 730px !important; }
    "))
  ),
  tags$div(
    class = "intro-logo",
    tags$img(src = "image1.png", alt = "Freestone Logo"),
    tags$div(class = "intro-title", "Lottery Design Choices")
  ),
  br(),
  actionButton("toggleIntro", "Show/Hide Introduction", icon = icon("info-circle")),
  hidden(
    div(id = "introPanel", wellPanel(includeMarkdown("intro.md")))
  ),
  br(),

  sidebarLayout(
    sidebarPanel(
      # Garret's UI feedback (2026-10-13, item 2): the formula/ticket-
      # designer controls and the real-data "Observed Applicant Stats"
      # block used to just be stacked vertically with an hr() between them
      # -- not enough visual separation for two things that are
      # conceptually different ("what ticket formula should we use" vs.
      # "what does the real historical applicant data look like"). Fixed
      # by splitting the sidebar into two tabs with shiny::tabsetPanel()
      # (this app already has `library(bslib)` loaded for theming only --
      # it never used bslib::navset_tab()/navbarPage anywhere, and
      # tabsetPanel is the simplest mechanism that (a) needs no new
      # top-level page structure, since sidebarLayout()/sidebarPanel() stay
      # exactly as they are, just with their contents now wrapped in a
      # tabset, and (b) plays correctly with the bs_theme() already applied
      # to the whole fluidPage()). Everything that drives or explores the
      # TICKET FORMULA (Lottery Parameters through the Example Applicant
      # Profile preview, plus the Multi-Year Projection controls, since
      # that's also a formula-driven exploration tool, not raw data) lives
      # in the "Ticket Formula Designer" tab; the "Observed Applicant
      # Stats" reactive text block (year-specific real-data percentages)
      # gets its own "Observed Applicant Stats" tab. Tab content is just
      # CSS-hidden when its tab isn't active (shiny's normal tabsetPanel
      # behavior) -- the underlying inputs/outputs are never
      # destroyed/recreated, so switching tabs does not reset any slider or
      # lose any reactive state; every output below renders into whichever
      # tab it's now nested in exactly as before.
      tabsetPanel(
        type = "tabs",
        tabPanel("Ticket Formula Designer",
      br(),
      h4("Lottery Parameters"),
      selectInput(
        "formula_family", "Ticket formula:",
        choices = c(
          "Current system (power + log service credit)" = "current",
          "Pure power, no service credit (e.g. 3^(n+k) or 4^(n+k))" = "pure_power",
          "Caleb's proposal: base^n + n*(finishes + service)" = "caleb",
          "Garret's decoupled proposal: b^n x 2^(k+v+t)" = "garret_decoupled"
        ),
        selected = "current"
      ),
      uiOutput("ticketFormula"),
      selectInput("year_select", "Data Year:", choices = c("2026", "2025", "2024", "2023"), selected = "2026"),
      sliderInput("exp", "Exponent / Base:", min = 2, max = 5, value = 2, step = 0.5),
      # The multiplier only applies to the "current" family's log(service+1)
      # term -- the other two families don't have a separately-weighted
      # service term, so there's nothing for this slider to do there.
      conditionalPanel(
        condition = "input.formula_family == 'current'",
        sliderInput("mult", "Service-Credit Multiplier:", min = 1, max = 10, value = 2)
      ),
      # Garret's decoupled proposal (ANALYSIS_LOG.md, 2026-10-13 "Decoupled
      # formula design" entry): Tickets = b^n * 2^(k + v/v_scale +
      # min(t/t_scale, t_cap)). `b` reuses the "Exponent / Base" slider
      # above (same control other families already use for their base --
      # no separate slider, per the task's "reuse whatever base selector
      # already exists" instruction); an observer below nudges it to 3 the
      # moment you select this family, since that's Garret's own proposed
      # default, but it stays a normal adjustable slider afterward. The
      # `2^(...)` factor is a SEPARATE base, always fixed at 2 regardless of
      # what `b` is set to -- its only job is bounding the veteran bonus. k
      # itself still comes from the "Finish-Bonus Ramp" sliders below (same
      # ramp/floor knob as every other family) -- this family just wants a
      # different DEFAULT ramp (2, not 3) so the k-cap lands at 2 finishes
      # instead of 3 (nudged by the same observer); v and t get their own
      # independent scale/cap controls here because Garret wants them to
      # behave very differently: volunteering should go "sky high" (no cap
      # at all), while trailwork -- "harder to verify and perhaps of more
      # variable quality in practice" per Garret -- is deliberately capped
      # low.
      #
      # 2026-10-13 (Max Boost slider): the veteran bonus's cap used to be
      # hardcoded at exactly 2x (k capped at 1, via the k_ramp default of 2
      # above). Garret asked for that cap itself to be adjustable, in the
      # multiplier unit he actually thinks in (e.g. "2x", "4x") rather than
      # the underlying exponent (k_cap). `max_boost` is that slider;
      # `garret_k_cap()` (top of file) converts it to k_cap = log2(max_boost)
      # wherever derive_k_from_finishes() is called for this family. Default
      # 2.0 reproduces the old hardcoded behavior exactly (log2(2) == 1).
      conditionalPanel(
        condition = "input.formula_family == 'garret_decoupled'",
        helpText(
          "b (the base above) controls only how steeply waiting (n) matters; ",
          "the veteran bonus is a SEPARATE factor, always base 2, capped by ",
          "the Max Boost slider below (acting through the Finish-Bonus Ramp ",
          "sliders further down -- default ramp 2 for this family). ",
          "Volunteer and trailwork points get their own scale/cap controls ",
          "here, independent of k:"
        ),
        sliderInput("max_boost", "Max finish-bonus boost (multiplier):",
                    min = 1.1, max = 5, value = 2.0, step = 0.1),
        sliderInput("v_scale", "Volunteer scale (points per +1 in the exponent; left UNCAPPED on purpose -- 'sky high'):",
                    min = 1, max = 20, value = 4, step = 1),
        sliderInput("t_scale", "Trailwork scale (points per +1 in the exponent, before the cap below):",
                    min = 1, max = 20, value = 8, step = 1),
        sliderInput("t_cap", "Trailwork cap (max exponent contribution from trailwork, ever):",
                    min = 0, max = 2, value = 0.5, step = 0.25)
      ),
      numericInput("Nm", "Number of Men to Pick:", min = 0, max = 200, value = default_nm),
      numericInput("Nw", "Number of Women to Pick:", min = 0, max = 200, value = default_nw),
      h4("Finish-Bonus Ramp (k)"),
      # Garret's UI feedback (2026-10-13, item 3): this explanatory helpText
      # used to always render inline, permanently eating sidebar space even
      # though most visits don't need it. Made collapsible, hidden by
      # default, using the EXACT same show/hide idiom this app already uses
      # everywhere else (introPanel/avgOddsPanel/oddsPanel/oddsDistPanel/
      # histAppsPanel/histFinPanel/projectionPanel, all a few lines above
      # and below this one): an actionLink/actionButton bound to a
      # server-side observeEvent() that calls shinyjs::toggle(id = ...) on a
      # div wrapped in hidden() so it starts closed. Reusing this pattern
      # (rather than introducing bslib::accordion or a <details> tag, which
      # nothing else in this file uses) keeps every collapsible section in
      # the app working the same way.
      actionLink("toggleKRampInfo",
                 HTML("&#8505;&#65039; More info on the ramp/floor shape")),
      hidden(
        div(id = "kRampInfoPanel",
          helpText(
            "The current formula's finish bonus (k) ramps up with each lifetime ",
            "race finish, then deliberately DROPS BACK DOWN to a floor -- so a ",
            "mega-veteran (think Hardrock 100's 10+-time finishers, who get ",
            "near-guaranteed entry) never earns more benefit than someone who's ",
            "finished exactly 'Ramp length' times. These two sliders let you ",
            "replay that same shape with different knobs. Defaults (ramp = 3, ",
            "floor = 0.5) exactly reproduce today's production rule -- nothing ",
            "changes until you move them. For Garret's decoupled proposal only, ",
            "a 'Max finish-bonus boost (multiplier)' slider (above, in the ",
            "formula controls) sets how high k is allowed to ramp to, in the ",
            "intuitive 'Nx' unit rather than the raw exponent: k's cap = ",
            "log2(max_boost), so the per-finish step becomes cap / Ramp length. ",
            "The default Max Boost of 2.0x reproduces the old hardcoded 'veteran ",
            "bonus capped at exactly 2x' behavior (k capped at 1); moving it to, ",
            "say, 4.0x raises the cap to k = 2, so a veteran at the ramp's top ",
            "gets a 4x bonus instead of 2x. The Floor slider is unaffected by ",
            "Max Boost -- it still just sets what k drops back down to past the ",
            "ramp, exactly as before."
          )
        )
      ),
      sliderInput("k_ramp", "Ramp length (finishes until the bonus maxes out):",
                  min = 1, max = 6, value = 3, step = 1),
      sliderInput("k_floor", "Floor (k value once past the ramp):",
                  min = 0, max = 1.5, value = 0.5, step = 0.25),
      plotOutput("kRampPlot", height = "220px"),
      hr(),
      h4("Discrete Rules"),
      # Generalizes what used to be a hardcoded "3+ year applicant" rule
      # into a pickable threshold, so Caleb can compare guaranteeing entry
      # on the 3rd, 4th, or 5th try instead of only ever seeing the 4th-try
      # version. "Guaranteed by your 4th try" (threshold = 3 prior losses)
      # reproduces the app's previous behavior exactly.
      selectInput(
        "auto_at", "Guaranteed-entry rule:",
        choices = c(
          "None (no automatic guarantee)" = "none",
          "Guaranteed by your 3rd try (2 prior losses)" = "2",
          "Guaranteed by your 4th try (3 prior losses)" = "3",
          "Guaranteed by your 5th try (4 prior losses)" = "4"
        ),
        selected = "none"
      ),
      checkboxInput("auto_zero_finishes_only",
                    "...but only guarantee applicants with zero previous finishes",
                    FALSE),
      br(),
      checkboxInput("no_firsttime_zero", "Disallow first-time applicants with no finishes", FALSE),
      br(),
      checkboxGroupInput(
        inputId = "odds_toggles",
        label   = "Odds adjustments (select any):",
        choices = c(
          "Drop trailwork (keep volunteering)"     = "drop_trail",
          "Drop log and +1 (use integer points)"   = "no_log"
        ),
        selected = character(0)
      ),
      hr(),
      radioButtons("odds_method", "Odds Calculation Method:",
                   choices = c("Deterministic (Fast)" = "deterministic",
                               "Monte Carlo (N = 1,000, Slow)" = "mc1000",
                               "Monte Carlo (N = 10,000, Slower)" = "mc10000"),
                   selected = "deterministic"),
      hr(),
      h4("Example Applicant Profile"),
      sliderInput("apps", label = "Previous Applications", min = 0, max = 6, value = 0),
      sliderInput("finishes", label = "Previous Finishes", min = 0, max = 5, value = 0),
      sliderInput("volunteer", label = "Volunteer Points", min = 0, max = 30, value = 0),
      sliderInput("trailwork", label = "Extra Trailwork Points", min = 0, max = 10, value = 0),
      h5("Your tickets in the lottery:"),
      verbatimTextOutput("userTickets"),
      hr(),
      h4("Multi-Year Projection (Men)"),
      helpText("A fast, simplified ('lite') multi-year projection -- good for exploring tradeoffs ",
               "quickly, but it does not model the dormant-applicant reservoir that ",
               "ANALYSIS_LOG.md's full cohort_sim.R simulation does, so it will look ",
               "somewhat more optimistic than that write-up. Men's pool only (the ",
               "analysis found formula choice barely matters for the women's pool)."),
      selectInput("growth_rate", "Applicant pool growth:",
                  choices = c("10% / yr (conservative)" = "10",
                              "25% / yr (recent actual trend)" = "25",
                              "30% / yr (aggressive)" = "30"),
                  selected = "25"),
      numericInput("years_to_project", "Years to project:", min = 3, max = 10, value = 8),
      actionButton("runProjection", "Run multi-year projection", class = "collapsible-btn")
        ),
        tabPanel("Observed Applicant Stats",
      br(),
      h5(textOutput("statsTitle", inline = TRUE)),
      textOutput("avgAppsStats"),
      textOutput("avgFinishesStats"),
      textOutput("trailworkStats"),
      textOutput("volunteerStats")
        )
      )
    ),
    mainPanel(
      wellPanel(
        h4("Guaranteed-Entry Snapshot (this year's data, no projection needed)"),
        textOutput("autoSnapshot")
      ),
      h3("Lottery Odds Summary"),
      actionButton("toggleAvgOdds", "Show/Hide Average Odds by Previous Applications", class = "collapsible-btn"),
      tags$div(
        id = "avgOddsPanel",
        fluidRow(
          column(6, h5("Women (F)"), DTOutput("avgOddsW")),
          column(6, h5("Men (M)"), DTOutput("avgOddsM"))
        )
      ),
      br(),
      h3("Detailed Odds"),
      actionButton("toggleOdds", "Show/Hide Full Detail Lottery Odds", class = "collapsible-btn"),
      tags$div(
        id = "oddsPanel",
        fluidRow(
          column(6, h5("Women (F)"), DTOutput("oddsW")),
          column(6, h5("Men (M)"), DTOutput("oddsM"))
        )
      ),
      br(),
      h3("Distribution of Odds (Tickets vs Odds)"),
      actionButton("toggleOddsDist", "Show/Hide Distribution of Odds", class = "collapsible-btn"),
      tags$div(
        id = "oddsDistPanel",
        fluidRow(
          column(6, plotOutput("oddsDistW")),
          column(6, plotOutput("oddsDistM"))
        )
      ),


      br(),
      actionButton("toggleHistApps", "Show/Hide Picks by Previous Applications", class="collapsible-btn"),
      tags$div(
        id = "histAppsPanel",
        tags$div(class = "collapsible-title", "Expected Distribution of Picks by Previous Applications"),
        fluidRow(
          column(6, plotOutput("histW")),
          column(6, plotOutput("histM"))
        )
      ),
      br(),
      actionButton("toggleHistFin", "Show/Hide Picks by Weighted Finishes", class="collapsible-btn"),
      tags$div(
        id = "histFinPanel",
        tags$div(class = "collapsible-title", "Expected Distribution of Picks by Weighted Finishes (k)"),
        fluidRow(
          column(6, plotOutput("finHistW")),
          column(6, plotOutput("finHistM"))
        )
      ),
      br(),
      actionButton("toggleProjection", "Show/Hide Multi-Year Projection", class = "collapsible-btn"),
      tags$div(
        id = "projectionPanel",
        tags$div(class = "collapsible-title", "Multi-Year Projection (Men's pool)"),
        h5("Does the current guaranteed-entry rule become oversubscribed?"),
        tableOutput("autoTierTable"),
        plotOutput("autoTierPlot"),
        br(),
        h5("Does a steeper formula actually get people in faster? (current vs base-3 vs base-4)"),
        plotOutput("convergencePlot")
      )
    )
  )
)

# Wrap simulate_odds call with progress bar
withProgressSimulate <- function(tix, picks, n_sim, label = "Running Monte Carlo...") {
  withProgress(message = label, value = 0.3, {
    Sys.sleep(0.1)  # Allow UI to update
    simulate_odds(tix, picks, n_sim)
  })
}


server <- function(input, output, session) {

  # Nudge the shared "Exponent / Base" (b) and "Ramp length" (k_ramp)
  # sliders to Garret's own proposed defaults (b=3, ramp=2) the moment the
  # decoupled formula family is selected -- see ANALYSIS_LOG.md's
  # 2026-10-13 "Decoupled formula design" entry and the task instructions
  # this implements ("default to base 3 ... keep it adjustable" / "change
  # the default ramp to 2 ... just change the DEFAULT when this formula
  # family is active"). Both sliders are shared across all formula
  # families (exactly like "mult" already is, hidden-but-not-reset when
  # not "current") -- this only sets a starting point on entry to this
  # family; it does NOT keep forcing the value back if you then adjust it,
  # and deliberately does NOT restore b/ramp to 2/3 when you switch away,
  # consistent with how this app already treats every other shared slider
  # (nothing resets "mult" when you leave "current" either). ignoreInit =
  # TRUE so this doesn't fire on initial app load (default family is
  # "current", not this one).
  observeEvent(input$formula_family, {
    if (identical(input$formula_family, "garret_decoupled")) {
      updateSliderInput(session, "exp", value = 3)
      updateSliderInput(session, "k_ramp", value = 2)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$toggleIntro, { shinyjs::toggle(id = "introPanel") })
  # Garret's UI feedback item 3 (2026-10-13): collapsible finish-bonus-ramp
  # explanation, same toggle idiom as every other collapsible section here.
  observeEvent(input$toggleKRampInfo, { shinyjs::toggle(id = "kRampInfoPanel") })
  observeEvent(input$toggleAvgOdds, { shinyjs::toggle(id = "avgOddsPanel") })
  observeEvent(input$toggleOdds, { shinyjs::toggle(id = "oddsPanel") })
  observeEvent(input$toggleOddsDist, { shinyjs::toggle(id = "oddsDistPanel") })
  observeEvent(input$toggleHistApps, { toggle("histAppsPanel") })
  observeEvent(input$toggleHistFin, { toggle("histFinPanel") })
  observeEvent(input$toggleProjection, { toggle("projectionPanel") })

  # Memoized simulate_odds including every input that can change which
  # people are even eligible to be drawn (tix and picks already reflect
  # the formula/year/rule choices by the time they get here, since they're
  # computed from them -- year/rule/no_firsttime_zero are included too only
  # as an extra safety net against cache collisions, not because tix/picks
  # alone could ever be ambiguous in practice).
  memo_simulate_odds <- memoise::memoise(function(tix, picks, n_sim, exp_val, mult_val, year, rule, zero_finishes_only, no_firsttime_zero) {
    withProgressSimulate(tix, picks, n_sim)
  })


  # Reactive selector for display odds column
  display_odds_col <- reactive({
    if (input$odds_method %in% c("mc1000", "mc10000")) "Odds_MC" else "Odds_Exact"
  })

  # Show dynamic ticket formula, reflecting the chosen formula family and
  # both optional odds-adjustment toggles, using an example applicant built
  # from the "Example Applicant Profile" sliders below.
  output$ticketFormula <- renderUI({
    exp_val <- input$exp
    mult_val <- input$mult
    family <- input$formula_family
    toggles <- input$odds_toggles
    apps <- input$apps
    # k_cap is only non-NULL for "garret_decoupled" (see garret_k_cap()'s
    # header comment) -- every other family's k_val is computed exactly as
    # before (NULL -> derive_k_from_finishes()'s own "reproduce today's
    # rule" default).
    k_val <- derive_k_from_finishes(input$finishes, input$k_ramp, input$k_floor,
                                     k_cap = garret_k_cap(family, input$max_boost))
    v_sim <- input$volunteer
    t_sim <- if ("drop_trail" %in% toggles) 0 else input$trailwork

    formula_text <- switch(
      family,
      "current" = if ("no_log" %in% toggles) {
        paste0(exp_val, "^(", k_val, " + ", apps, " + 1) + ", mult_val, " * (", v_sim, " + ", t_sim, ")")
      } else {
        paste0(exp_val, "^(", k_val, " + ", apps, " + 1) + ", mult_val, " * log(", v_sim, " + ", t_sim, " + 1)")
      },
      "pure_power" = paste0(exp_val, "^(", apps, " + ", k_val, ")  [service points have no effect in this family]"),
      "caleb" = paste0(exp_val, "^", apps, " + ", apps, " * (", k_val, " + ", (v_sim + t_sim), ")"),
      "garret_decoupled" = {
        # Same rescale apply_service_toggles() applies for this family in
        # base_data()/userTickets -- v uncapped, t capped -- shown here so
        # the live display always matches what the actual draw uses.
        #
        # BUG FIX (2026-10-13, Garret's UI report): this used to append a
        # bracketed explanatory aside ("[b=... controls only n-steepness;
        # ... capped at 2x via k]") directly onto the end of this paste0()
        # call, so it rendered inline as part of the formula text itself --
        # explanatory/reasoning text leaking into what's supposed to be a
        # clean math expression. That explanation already belongs (and now
        # lives only) in the conditionalPanel helpText right above the
        # sliders for this family and in the collapsible "Finish-Bonus
        # Ramp" help text below -- it has no business being re-stated
        # inside the live formula display. Whatever extra stray character(s)
        # Garret saw trailing after that bracket is removed along with it,
        # since nothing follows this paste0() call's closing parenthesis
        # anymore. Fixed by simply not building that aside into formula_text
        # at all -- the formula display is now just the clean math.
        v_contrib <- round(v_sim / input$v_scale, 2)
        t_contrib <- round(pmin(t_sim / input$t_scale, input$t_cap), 2)
        paste0(exp_val, "^", apps, " * 2^(", k_val, " + ", v_contrib, " + ", t_contrib, ")")
      }
    )
    HTML(paste0("<div class='formula-box'><span class='formula-highlight'>Tickets = ", formula_text, "</span></div>"))
  })

  # kRampPlot: small, always-visible visualization of the k(finishes)
  # ramp-then-floor shape, so moving the k_ramp/k_floor sliders is visually
  # obvious without having to dig through the odds tables below. Plots
  # finishes 0-10 (per task spec) against k under the CURRENT slider
  # settings, alongside today's fixed production rule (ramp=3, floor=0.5)
  # as a reference, so it's immediately clear whether/how the shape has
  # changed -- directly illustrating "no one becomes a Hardrock-style lock"
  # regardless of where the sliders are set (the line always flattens, it
  # just flattens to a different floor and after a different ramp).
  output$kRampPlot <- renderPlot({
    finishes <- 0:10
    # "Your settings" needs the Max Boost-derived cap too, when this family
    # is active, so the plot matches what the formula actually uses (same
    # garret_k_cap() helper as every other derive_k_from_finishes() call
    # site for this family).
    dd <- data.frame(
      finishes = rep(finishes, 2),
      k = c(derive_k_from_finishes(finishes, 3, 0.5),
            derive_k_from_finishes(finishes, input$k_ramp, input$k_floor,
                                    k_cap = garret_k_cap(input$formula_family, input$max_boost))),
      which = rep(c("Current production rule (ramp 3, floor 0.5)", "Your settings"),
                  each = length(finishes))
    )
    ggplot(dd, aes(x = finishes, y = k, color = which)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = c(
        "Current production rule (ramp 3, floor 0.5)" = "grey60",
        "Your settings" = "#B7822E"
      )) +
      scale_x_continuous(breaks = finishes) +
      labs(x = "Lifetime race finishes", y = "k (finish bonus)",
           title = "No one becomes a Hardrock-style lock", color = NULL) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.text = element_text(size = 8),
            plot.title = element_text(size = 11))
  })

  # base_data(): loads the selected year's applicant pool and computes each
  # applicant's k (weighted finishes) and tickets.
  #
  # IMPORTANT FIX: the 2026 export (2026HLdata.csv) does not carry a raw
  # Previous_Finishes count at all -- it supplies the already-weighted k
  # value directly, under a column named Weighted_Finishes (see
  # ANALYSIS_LOG.md). The older files (2023-2025) carry a raw finish COUNT
  # instead, under Previous_Finishes, which has to be translated onto the
  # 0/.5/1/1.5(/.5) scale via derive_k_from_finishes(). Branching on which
  # column is actually present is the fix for a real bug: previously, this
  # function unconditionally looked for Previous_Finishes, so selecting
  # "2026" in the Data Year dropdown crashed the whole app.
  base_data <- reactive({
    fname <- switch(input$year_select,
                    "2026" = "2026HLdata.csv",
                    "2025" = "2025HLdata.csv",
                    "2024" = "2024HLdata.csv",
                    "2023" = "2023HLdata.csv")
    df <- read.csv(fname, stringsAsFactors = FALSE)

    # 2026 is the only file with a Status column, and it includes 40
    # "denied" applicants who were rejected before ever being assigned a
    # Lottery Pool -- their Lottery.Pool is NA, not "M"/"F". Left in, they
    # turn Gender NA, which then poisons sum(Gender == ... & ...) with NA
    # as soon as anything (like the automatic-entry rule) tries to count
    # them. cohort_sim.R's load_pool_2026() already excludes them for the
    # same reason -- they were never real lottery participants.
    if ("Status" %in% names(df)) {
      df <- df %>% filter(Status != "denied")
    }

    if ("Weighted_Finishes" %in% names(df)) {
      # 2026: no raw finish count in the export, only the already-k-mapped
      # Weighted_Finishes. Reconstruct an approximate raw count (see
      # reconstruct_2026_raw_finishes()'s header comment for the full
      # investigation/rationale -- short version: use the panel's own
      # observed race finishes, NOT panel_prior_wins, to unmask 4+-time
      # veterans hiding in the 0.5 floor bucket) so the k_ramp/k_floor
      # sliders below have real raw material to work with on this year's
      # data too, not just on 2023-2025's native Previous_Finishes column.
      df$Weighted_Finishes <- as.numeric(df$Weighted_Finishes)
      df$Previous_Finishes <- reconstruct_2026_raw_finishes(df$Weighted_Finishes, df$runner_id)
      df$k <- derive_k_from_finishes(df$Previous_Finishes, input$k_ramp, input$k_floor,
                                      k_cap = garret_k_cap(input$formula_family, input$max_boost))
    } else {
      df$Previous_Finishes <- as.numeric(df$Previous_Finishes)
      df$k <- derive_k_from_finishes(df$Previous_Finishes, input$k_ramp, input$k_floor,
                                      k_cap = garret_k_cap(input$formula_family, input$max_boost))
    }

    df <- df %>% mutate(
      Gender = ifelse(Lottery.Pool == "F", "Female", "Male"),
      Previous_Applications = as.numeric(Previous_Applications),
      Volunteer_Points = pmin(as.numeric(Volunteer_Points), 30),
      Extra_Trailwork_Points = pmin(as.numeric(Extra_Trailwork_Points), 10)
    )

    df$service <- apply_service_toggles(df$Volunteer_Points, df$Extra_Trailwork_Points, input$odds_toggles,
                                         input$formula_family, input$v_scale, input$t_scale, input$t_cap)
    formula_fn <- build_app_formula(input$formula_family, input$exp, input$mult, input$odds_toggles)
    df$tickets <- formula_fn(df$Previous_Applications, df$k, df$service)
    df
  })

  # Show user's ticket count for chosen slider profile (same formula logic
  # as base_data(), applied to the hypothetical example applicant instead
  # of the real data).
  output$userTickets <- renderText({
    k_sim <- derive_k_from_finishes(input$finishes, input$k_ramp, input$k_floor,
                                     k_cap = garret_k_cap(input$formula_family, input$max_boost))
    v_sim <- pmin(input$volunteer, 30)
    t_sim <- pmin(input$trailwork, 10)
    s_sim <- apply_service_toggles(v_sim, t_sim, input$odds_toggles,
                                    input$formula_family, input$v_scale, input$t_scale, input$t_cap)
    formula_fn <- build_app_formula(input$formula_family, input$exp, input$mult, input$odds_toggles)
    round(formula_fn(input$apps, k_sim, s_sim), 2)
  })

  # Marks applicants Auto-Selected (per the guaranteed-entry rule),
  # Removed (per the "disallow first-time zero-finish applicants" rule),
  # or left as ordinary Lottery entrants.
  processed_data <- reactive({
    df <- base_data()
    df$SelectionStatus <- "Lottery"

    # Pre-selects (2026 only) already have a guaranteed spot through a
    # channel unrelated to the ticket formula -- volunteer lottery, sponsor
    # spot, previous-year podium, etc. (see Pre_Selection_Reason) -- and
    # don't compete for the Nm/Nw lottery slots, exactly as
    # ANALYSIS_LOG.md's "125 per pool (incl. pre-selects) -> 96/114 lottery
    # picks" describes. Marking them here, before the discretionary
    # auto_at rule below, means Nm/Nw correctly represent lottery-only
    # slots whenever 2026 data (the only year with this column) is loaded.
    if ("Status" %in% names(df)) {
      df$SelectionStatus[df$Status == "preselect"] <- "Auto-Selected"
    }

    if (input$auto_at != "none") {
      threshold <- as.numeric(input$auto_at)
      eligible <- df$Previous_Applications >= threshold
      if (input$auto_zero_finishes_only) eligible <- eligible & (df$k == 0)
      df$SelectionStatus[eligible & df$SelectionStatus == "Lottery"] <- "Auto-Selected"
    }

    if (input$no_firsttime_zero) {
      # Uses k == 0 rather than the raw Previous_Finishes count. For 2026,
      # Previous_Finishes is now populated too (see base_data()'s
      # reconstruct_2026_raw_finishes() call), but it's only an
      # approximation reconstructed from the panel, whereas k == 0 means
      # exactly the same thing (zero weighted finishes) either way and is
      # always exact regardless of which year is loaded. Guarded to only
      # ever downgrade an ordinary "Lottery" row, so it can never
      # clobber a pre-select's or auto_at's guaranteed status.
      df$SelectionStatus[df$SelectionStatus == "Lottery" &
                            df$Previous_Applications == 0 & df$k == 0] <- "Removed"
    }

    df
  })

  # Simplified assigned_data() reactive function
  assigned_data <- reactive({
    df <- processed_data()
    n_picks_w <- max(0, input$Nw - sum(df$Gender == "Female" & df$SelectionStatus == "Auto-Selected"))
    n_picks_m <- max(0, input$Nm - sum(df$Gender == "Male" & df$SelectionStatus == "Auto-Selected"))

    df$Odds_Exact <- NA_real_
    df$Odds_MC <- NA_real_

    run_mc <- input$odds_method %in% c("mc1000", "mc10000")
    n_sim <- switch(input$odds_method,
                    "mc1000" = 1000,
                    "mc10000" = 100000,
                    0)

    for (gender in c("Female", "Male")) {
      idx <- which(df$Gender == gender & df$SelectionStatus == "Lottery")
      picks <- ifelse(gender == "Female", n_picks_w, n_picks_m)

      if (length(idx) > 0 && picks > 0) {
        tix <- df$tickets[idx]

        # Direct calculation - no memoization needed, super fast
        df$Odds_Exact[idx] <- calc_lottery_odds(tix, picks)

        # Only memoize the slow Monte Carlo simulations
        if (run_mc) {
          df$Odds_MC[idx] <- memo_simulate_odds(
            tix = tix,
            picks = picks,
            n_sim = n_sim,
            exp_val = input$exp,
            mult_val = input$mult,
            year = input$year_select,
            rule = input$auto_at,
            zero_finishes_only = input$auto_zero_finishes_only,
            no_firsttime_zero = input$no_firsttime_zero
          )
        }
      } else if (length(idx) > 0) {
        # picks <= 0: the guaranteed-entry tier (Auto-Selected) already
        # equals or exceeds this year's Nm/Nw slots on its own, so nobody
        # left in "Lottery" status has ANY chance of being drawn -- their
        # true odds are exactly 0, not unknown. Found while chasing the
        # 2026-10-13 "points piling up at 100%" bug report: with this
        # branch missing, these rows were left at the NA_real_ default
        # instead, which ggplot silently drops from oddsDistW/M (not the
        # reported symptom, but discovered via regression testing while
        # fixing it) and which would also silently distort any mean()
        # computed over Display_Odds elsewhere (avgOddsW/M already pass
        # na.rm = TRUE, so those just under-count rather than crash).
        df$Odds_Exact[idx] <- 0
        df$Odds_MC[idx] <- 0
      }

      # Handle auto-selected and removed cases
      df$Odds_Exact[df$Gender == gender & df$SelectionStatus == "Auto-Selected"] <- 1
      df$Odds_MC[df$Gender == gender & df$SelectionStatus == "Auto-Selected"] <- 1
      df$Odds_Exact[df$Gender == gender & df$SelectionStatus == "Removed"] <- 0
      df$Odds_MC[df$Gender == gender & df$SelectionStatus == "Removed"] <- 0
    }

    df
  })

  get_display_odds <- function(df) {
    method <- input$odds_method
    if (method %in% c("mc1000", "mc10000")) df$Odds_MC else df$Odds_Exact
  }

  # --- Guaranteed-entry snapshot (always-on, cheap -- no projection needed) ---
  output$autoSnapshot <- renderText({
    if (input$auto_at == "none") return("No automatic-entry guarantee is active.")
    threshold <- as.numeric(input$auto_at)
    df <- processed_data()
    elig_m <- sum(df$Gender == "Male" & df$SelectionStatus == "Auto-Selected")
    elig_w <- sum(df$Gender == "Female" & df$SelectionStatus == "Auto-Selected")
    sprintf(
      paste0("Guaranteed entry at %d+ prior losses, using %s data: %d men already qualify ",
             "(vs. %d men's slots total), %d women already qualify (vs. %d women's slots total)."),
      threshold, input$year_select, elig_m, input$Nm, elig_w, input$Nw
    )
  })

  output$avgOddsW <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Female")
    df$Display_Odds <- df[[display_odds_col()]]
    avg <- df %>%
      group_by(Previous_Applications) %>%
      summarise(
        N = n(),
        `Avg Odds (%)` = round(mean(Display_Odds, na.rm = TRUE) * 100, 2),
        `Min Odds (%)` = round(min(Display_Odds, na.rm = TRUE) * 100, 2),
        `Max Odds (%)` = round(max(Display_Odds, na.rm = TRUE) * 100, 2),
        .groups = "drop") %>%
      rename(`Previous Applications` = Previous_Applications)
    datatable(avg, options = list(dom = "t"), rownames = FALSE)
  })
  output$avgOddsM <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Male")
    df$Display_Odds <- df[[display_odds_col()]]
    avg <- df %>%
      group_by(Previous_Applications) %>%
      summarise(
        N = n(),
        `Avg Odds (%)` = round(mean(Display_Odds, na.rm = TRUE) * 100, 2),
        `Min Odds (%)` = round(min(Display_Odds, na.rm = TRUE) * 100, 2),
        `Max Odds (%)` = round(max(Display_Odds, na.rm = TRUE) * 100, 2),
        .groups = "drop") %>%
      rename(`Previous Applications` = Previous_Applications)
    datatable(avg, options = list(dom = "t"), rownames = FALSE)
  })

  output$oddsW <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Female")
    df$Display_Odds <- df[[display_odds_col()]]
    grouped <- df %>%
      group_by(tickets, Previous_Applications, k, SelectionStatus, Display_Odds) %>%
      summarise(N = n(), .groups = "drop") %>%
      mutate(
        tickets_char = formatC(round(tickets,2), format="f", digits=2),
        odds_percent = formatC(round(100 * Display_Odds,2), format="f", digits=2)
      ) %>%
      arrange(as.numeric(tickets_char)) %>%
      select("Tickets" = tickets_char, "N" = N,
             "Previous Apps" = Previous_Applications,
             "Finishes (k)" = k,
             "Selection Status" = SelectionStatus,
             "Odds (%)" = odds_percent)

    datatable(grouped,
              options = list(
                dom = "tip",
                pageLength = 20,
                # Add styling to make font smaller
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'font-size': '12px'});",
                  "$(this.api().table().body()).css({'font-size': '11px'});",
                  "}"
                )
              ),
              rownames = FALSE)
  })

  output$oddsM <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Male")
    df$Display_Odds <- df[[display_odds_col()]]
    grouped <- df %>%
      group_by(tickets, Previous_Applications, k, SelectionStatus, Display_Odds) %>%
      summarise(N = n(), .groups = "drop") %>%
      mutate(
        tickets_char = formatC(round(tickets,2), format="f", digits=2),
        odds_percent = formatC(round(100 * Display_Odds,2), format="f", digits=2)
      ) %>%
      arrange(as.numeric(tickets_char)) %>%
      select("Tickets" = tickets_char, "N" = N,
             "Previous Apps" = Previous_Applications,
             "Finishes (k)" = k,
             "Selection Status" = SelectionStatus,
             "Odds (%)" = odds_percent)

    datatable(grouped,
              options = list(
                dom = "tip",
                pageLength = 20,
                # Add styling to make font smaller
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'font-size': '12px'});",
                  "$(this.api().table().body()).css({'font-size': '11px'});",
                  "}"
                )
              ),
              rownames = FALSE)
  })

  # --- Fractions / averages of applicant characteristics (selected year) ---
  output$statsTitle <- renderText({ "Observed Applicant Stats" })

  output$avgAppsStats <- renderText({
    df <- base_data()
    if (is.null(df) || nrow(df) == 0) return("Average previous applications: n/a")
    sprintf("Average previous applications: %.1f", mean(df$Previous_Applications, na.rm = TRUE))
  })

  output$avgFinishesStats <- renderText({
    df <- base_data()
    if (is.null(df) || nrow(df) == 0) return("Average weighted finishes (k): n/a")
    sprintf("Average weighted finishes (k): %.2f", mean(df$k, na.rm = TRUE))
  })

  output$volunteerStats <- renderText({
    df <- base_data()
    if (is.null(df) || nrow(df) == 0) return("Volunteer points used: n/a")
    frac <- mean(ifelse(is.na(df$Volunteer_Points), 0, df$Volunteer_Points) > 0)
    sprintf("Volunteer points used: %.1f%%", frac * 100)
  })

  output$trailworkStats <- renderText({
    df <- base_data()
    if (is.null(df) || nrow(df) == 0) return("Extra trailwork used: n/a")
    frac <- mean(ifelse(is.na(df$Extra_Trailwork_Points), 0, df$Extra_Trailwork_Points) > 0)
    sprintf("Extra trailwork used: %.1f%%", frac * 100)
  })



  # --- Expected picks by previous applications (bar) ---
  output$histW <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Female")
    df$Display_Odds <- df[[display_odds_col()]]
    df <- df %>% group_by(Previous_Applications) %>% summarise(ExpectedPicks = sum(Display_Odds, na.rm = TRUE))
    ggplot(df, aes(x = factor(Previous_Applications), y = ExpectedPicks)) +
      geom_col(fill = "#B7822E") +
      labs(x = "Previous Applications", y = "Expected # Picked", title = "Women") +
      theme_minimal()
  })

  output$histM <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Male")
    df$Display_Odds <- df[[display_odds_col()]]
    df <- df %>% group_by(Previous_Applications) %>% summarise(ExpectedPicks = sum(Display_Odds, na.rm = TRUE))
    ggplot(df, aes(x = factor(Previous_Applications), y = ExpectedPicks)) +
      geom_col(fill = "#2270AE") +
      labs(x = "Previous Applications", y = "Expected # Picked", title = "Men") +
      theme_minimal()
  })


  # --- Expected picks by weighted finishes (k) (bar) ---
  output$finHistW <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Female")
    df$Display_Odds <- df[[display_odds_col()]]
    df <- df %>% group_by(k) %>% summarise(ExpectedPicks = sum(Display_Odds, na.rm = TRUE))
    ggplot(df, aes(x = factor(k), y = ExpectedPicks)) +
      geom_col(fill = "#E1B882") +
      labs(x = "Weighted Finishes (k)", y = "Expected # Picked", title = "Women") +
      theme_minimal()
  })

  output$finHistM <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Male")
    df$Display_Odds <- df[[display_odds_col()]]
    df <- df %>% group_by(k) %>% summarise(ExpectedPicks = sum(Display_Odds, na.rm = TRUE))
    ggplot(df, aes(x = factor(k), y = ExpectedPicks)) +
      geom_col(fill = "#9EC6F5") +
      labs(x = "Weighted Finishes (k)", y = "Expected # Picked", title = "Men") +
      theme_minimal()
  })

  # oddsDistW / oddsDistM: this plot's whole purpose is to sanity-check
  # calc_lottery_odds() (the mean-field "exact" formula) against
  # simulate_odds() (Monte Carlo) -- two different ways of computing the SAME
  # probabilistic quantity, "chance of being drawn given your ticket count."
  # Both methods only ever produce a number for applicants who actually
  # compete in the draw, i.e. SelectionStatus == "Lottery" -- see
  # assigned_data() above, where Odds_Exact/Odds_MC are computed by
  # calc_lottery_odds()/simulate_odds() ONLY over idx <- which(... &
  # SelectionStatus == "Lottery").
  #
  # BUG (found 2026-10-13, see ANALYSIS_LOG.md): this used to plot the WHOLE
  # assigned_data() for the gender, with no SelectionStatus filter at all.
  # assigned_data() also hard-sets Odds_Exact <- 1 / Odds_MC <- 1 for every
  # "Auto-Selected" row (2026 preselects, unconditionally whenever 2026 data
  # is loaded -- not gated on any slider; PLUS anyone qualifying under the
  # "Guaranteed-entry rule" (auto_at) selector when one is active) and
  # Odds_Exact <- 0 / Odds_MC <- 0 for every "Removed" row (the "disallow
  # first-time zero-finish applicants" checkbox). None of those 1s/0s come
  # from the draw at all -- they're categorical overrides that bypass
  # calc_lottery_odds()/simulate_odds() entirely -- but with no filter they
  # got plotted in the exact same "Direct"/"Monte Carlo" series as the real
  # calculated points, at whatever (often perfectly ordinary) ticket count
  # that applicant's profile happened to produce. That's what made it look
  # like "an unexpectedly large number of Monte Carlo points piling up at
  # 100%": e.g. with the 3rd-try guarantee active on 2026 men, 17% of all
  # plotted points were hard-set to 100% this way, overlapping the SAME
  # ticket range as genuinely-drawn applicants (median ticket count of the
  # auto-selected group was actually close to the lottery pool's own
  # median) -- nothing to do with having an enormous ticket count. Verified
  # this is the whole story and not an odds-math problem: restricting to
  # real Lottery-status applicants, calc_lottery_odds() and simulate_odds()
  # agree closely and the highest odds anyone legitimately reaches under the
  # 2026 default formula/picks is ~72%, not near 100%.
  #
  # Fix: filter to SelectionStatus == "Lottery" before plotting, so this
  # chart only ever shows points where Odds_Exact/Odds_MC are actually
  # outputs of the two draw-probability calculations being compared. A
  # caption reports how many applicants were excluded as a reminder that
  # they exist (guaranteed in, or removed) -- they just don't belong on a
  # chart about how the probabilistic draw treats ticket counts.
  output$oddsDistW <- renderPlot({
    df_all <- assigned_data() %>% filter(Gender == "Female")
    df <- df_all %>% filter(SelectionStatus == "Lottery")
    n_excluded <- nrow(df_all) - nrow(df)
    caption <- if (n_excluded > 0) {
      sprintf("%d of %d women excluded (guaranteed in or removed by a discrete rule, not drawn)",
              n_excluded, nrow(df_all))
    } else NULL
    ggplot() +
      geom_jitter(data = df, aes(x = tickets, y = Odds_Exact, color = "Direct"), width = 0.1, height = 0) +
      geom_jitter(data = df, aes(x = tickets, y = Odds_MC, color = "Monte Carlo"), width = 0.1, height = 0) +
      scale_color_manual(values = c("Direct" = "#2270AE", "Monte Carlo" = "#B7822E")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "Tickets", y = "Odds", color = "Method",
           title = "Women: Ticket vs Odds (Both Methods)",
           subtitle = "Lottery-eligible applicants only -- excludes guaranteed/removed applicants",
           caption = caption) +
      theme_minimal()
  })

  output$oddsDistM <- renderPlot({
    df_all <- assigned_data() %>% filter(Gender == "Male")
    df <- df_all %>% filter(SelectionStatus == "Lottery")
    n_excluded <- nrow(df_all) - nrow(df)
    caption <- if (n_excluded > 0) {
      sprintf("%d of %d men excluded (guaranteed in or removed by a discrete rule, not drawn)",
              n_excluded, nrow(df_all))
    } else NULL
    ggplot() +
      geom_jitter(data = df, aes(x = tickets, y = Odds_Exact, color = "Direct"), width = 0.1, height = 0) +
      geom_jitter(data = df, aes(x = tickets, y = Odds_MC, color = "Monte Carlo"), width = 0.1, height = 0) +
      scale_color_manual(values = c("Direct" = "#2270AE", "Monte Carlo" = "#B7822E")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "Tickets", y = "Odds", color = "Method",
           title = "Men: Ticket vs Odds (Both Methods)",
           subtitle = "Lottery-eligible applicants only -- excludes guaranteed/removed applicants",
           caption = caption) +
      theme_minimal()
  })

  ## -------------------------------------------------------------------------
  ## Multi-year projection (Men's pool). See lottery_projection.R for the
  ## engine and its documented assumptions/limitations. Wrapped in
  ## eventReactive()s tied to the "Run multi-year projection" button (rather
  ## than plain reactive()s) so the projection -- an 8-year loop -- doesn't
  ## re-run on every slider tick, per CLAUDE.md's guidance to keep things
  ## fast on a RAM-limited machine.
  ## -------------------------------------------------------------------------

  # seed_groups_for_projection(): collapses this year's real applicant data
  # (men only) down to (n, k, s, N) group counts -- the input format
  # project_pool() expects.
  seed_groups_for_projection <- function() {
    df <- base_data() %>% filter(Gender == "Male")
    # Preselects (2026 only) have a guaranteed spot unrelated to the ticket
    # formula and don't compete in the lottery draw -- see the same
    # exclusion in processed_data(). Keeping them out here too means the
    # projection's pool matches what the main odds tables actually show.
    if ("Status" %in% names(df)) df <- df %>% filter(Status != "preselect")
    df %>%
      transmute(n = Previous_Applications, k = k, s = service) %>%
      group_by(n, k, s) %>%
      summarise(N = n(), .groups = "drop")
  }

  # Projects the CURRENTLY SELECTED formula/guarantee-rule combination
  # forward, to see whether/when it becomes oversubscribed.
  auto_tier_projection <- eventReactive(input$runProjection, {
    seed <- seed_groups_for_projection()
    growth <- as.numeric(input$growth_rate) / 100
    auto_at <- if (input$auto_at == "none") Inf else as.numeric(input$auto_at)
    formula_fn <- build_app_formula(input$formula_family, input$exp, input$mult, input$odds_toggles)
    project_pool(seed, formula_fn, n_picks = input$Nm, years = input$years_to_project,
                 growth = growth, auto_at = auto_at, pool = "M")
  })

  # Projects THREE FIXED, NAMED formulas (current / base-3 / base-4) --
  # independent of whatever the sidebar's formula_family/exp/mult controls
  # happen to be set to -- because this chart's whole point is the specific,
  # already-documented comparison from ANALYSIS_LOG.md's static-
  # counterfactuals table: that those three formulas give a persistent
  # rookie nearly identical odds through year 3, and only diverge from
  # year 4 onward.
  convergence_projection <- eventReactive(input$runProjection, {
    seed <- seed_groups_for_projection()
    growth <- as.numeric(input$growth_rate) / 100
    horizon <- min(5, input$years_to_project)
    fams <- list(
      "current: 2^(n+k+1)+2log(s+1)" = build_ticket_formula("current", exp_val = 2, mult_val = 2),
      "base 3:  3^(n+k)"             = build_ticket_formula("pure_power", exp_val = 3),
      "base 4:  4^(n+k)"             = build_ticket_formula("pure_power", exp_val = 4)
    )
    lapply(fams, function(f) {
      proj <- project_pool(seed, f, n_picks = input$Nm, years = input$years_to_project,
                            growth = growth, auto_at = Inf, pool = "M")
      persistent_rookie_path(proj, horizon = horizon)
    })
  })

  output$autoTierTable <- renderTable({
    if (input$auto_at == "none") {
      return(data.frame(Note = "Pick a guaranteed-entry rule in the sidebar to see this projection."))
    }
    proj <- auto_tier_projection()
    proj$yearly %>%
      transmute(
        `Years from now` = year,
        `Projected men's pool` = round(pool_size),
        `Already past the guarantee threshold` = round(auto_eligible),
        `Their share of men's slots` = sprintf("%.0f%%", pct_of_slots)
      )
  })

  output$autoTierPlot <- renderPlot({
    if (input$auto_at == "none") return(NULL)
    proj <- auto_tier_projection()
    ggplot(proj$yearly, aes(x = year, y = pct_of_slots)) +
      geom_line(color = "#B7822E", linewidth = 1) +
      geom_point(color = "#B7822E", size = 2) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
      labs(x = "Years from now", y = "% of men's slots used by the guaranteed tier alone",
           title = "When does this guarantee become oversubscribed? (red line = 100%)") +
      theme_minimal()
  })

  output$convergencePlot <- renderPlot({
    paths <- convergence_projection()
    dd <- bind_rows(lapply(names(paths), function(nm) paths[[nm]] %>% mutate(formula = nm)))
    ggplot(dd, aes(x = year_of_applying, y = cum_in, color = formula)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(x = "Years of persistently applying", y = "Cumulative chance of getting in",
           title = "A steeper base barely matters before year 4",
           color = "Formula") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
