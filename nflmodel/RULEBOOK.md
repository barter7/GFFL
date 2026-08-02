# nflmodel RULEBOOK

The assumptions, conventions, calibrated constants, and standalone
studies behind this model — the NFL sibling of PropSZN's rulebook.
Every rule states its provenance. When a rule changes, update this
file in the same commit.

> **Status note (2026-07):** the project restarted data-first. §1–§2
> and §4 (data, context, game script) are the LIVE foundation being
> built now. §3 and §5 document the v1 prototype (model_core.R /
> backtest.R) — kept as the record of what was learned there; the
> rebuilt model will re-adopt or revise those rules explicitly when
> we get to the modeling stage.

---

## 1. Data rules

**R1.1 — Single source family.** All data comes from nflverse
(play-by-play, schedules, rosters, snap counts, NGS, weekly player
stats, depth charts, injuries), loaded via `nflreadr` where installed
or the corresponding nflverse-data release asset URL otherwise. No
Wikipedia. PFF exports are an optional local drop-in (`data/pff/`),
never required.

**R1.2 — Cache-first.** The model layer never touches the network.
Every source lands in a local cache (`data/pbp`, `data/context`,
`data/rosters`, `data/players`, `data/cache`) and loaders read only
caches. Refresh functions follow one convention: completed seasons
are immutable and never re-downloaded; the current season's file
re-downloads when older than ~20h (nflverse updates nightly in
season).

**R1.3 — Season window.** Player/team modeling data: last season +
current (current season's files appear in September; before
September, "current" means the just-completed season). The
spread game-script history is the exception: rolling 20 seasons
(R4.3).

**R1.4 — Team codes.** nflverse codes are canonical; `normalize_team()`
maps everything else in (LAR→LA, JAC→JAX, WSH→WAS, OAK→LV, SD→LAC,
STL→LA). Applied at every load boundary.

**R1.5 — Betting columns are CLOSING numbers.** `spread_line`,
`total_line`, moneylines and juice in the schedule data are closing
lines (nfldata sources). Never treat them as openers.

**R1.6 — Spread sign convention.** `spread_line` = points the HOME
team is favored by. Posteam-relative: `team_spread` = points THIS
team is favored by (positive = favored). Verified empirically:
cor(home margin, spread_line) = +0.437 (Study S1).

**R1.7 — Implied team total** = `total_line/2 + team_spread/2`.
Tracks actual team scoring monotonically (cor 0.43 at the team-game
level; bucket means within ~1 point through the middle of the
distribution, slight under at the top).

**R1.8 — Playoff weeks.** Weeks number 1–18 (REG) and 19–22 (POST)
everywhere; per-game tables (coordinators, gamescript) must cover
POST weeks too.

**R1.9 — Pushes are NA, not losses.** `cover`/`over` outcome flags
are NA on exact pushes (the original prep script coded them 0).

**R1.10 — pbp play population: penalties, timeouts, kneels.** The
modeled snap population is `play_type %in% c("pass","run")` (no 2-pt
attempts). Audited on 2025 (Study S11):
- Timeouts are `no_play` rows → excluded everywhere; never counted
  as snaps.
- Penalties on plays that STOOD (~1.5% of kept snaps) are included;
  the `penalty` flag rides along and penalty yardage is never in
  `yards_gained`.
- No-play penalty rows (pre-snap fouls + nullified plays) are
  excluded, matching official stat accounting — nullified plays
  credit no attempt/target/carry, and props settle on official
  stats. CAVEAT: ~4.4% of real dropbacks/runs are nullified; DPI-
  drawn targets in those rows are genuine receiver-usage signal that
  official stats never credit. Logged as a future usage feature
  (penalty-drawn targets / DPI yards), not silently discarded.
- Kneels and spikes have their own play_types → excluded (forced
  clock plays, not play-calling signal). Sacks and scrambles stay
  in the population with flags intact.

**R1.11 — Player profile layer** (player_profiles.R): usage and
efficiency per GAME, per SEASON, and CAREER (career = the cached
17-game era, 2021+; true careers extend earlier). Season/career
rates are recomputed from summed counts, never averaged averages.
Definitions: carries = DESIGNED carries (`rush == 1`, scrambles
excluded by construction) for both team carry share and the RB-only
share; targets = pass attempts with a receiver id (throwaways have
no receiver/air yards and drop from depth-band rates); depth bands
on air yards: behind-LOS <0, 0-4, 5-9, 10-19, 20+; scrambles are
attributed via rusher_player_id (their passer id is NA) and QB
dropbacks = throws + sacks + scrambles. Profiles include playoffs —
published REG-only season stats will differ for playoff teams.
Position-dependent denominators need that season's roster file
(all era rosters cached).

**R1.12 — Defense profile layer** (defense_profiles.R): the exact
mirror of R1.11 — everything a defense ALLOWS, same grains (game /
season / era-2021+), same definitions: designed carries faced
(scrambles tracked separately as scrambles allowed), targets faced
= attempts with a receiver id, identical depth bands with
completion%-allowed per band, sacks/INTs GENERATED, CPOE-allowed,
plus receiver-position splits (targets/receptions/yards allowed to
RB/WR/TE via the offense's season roster). Verified by identity:
league offense totals equal defense-allowed totals to the yard
(Study S14). Rates recomputed from sums at every grain.

---

## 2. Game context rules

**R2.1 — Context joins by `game_id`,** posteam-relative
(`pbp_with_context()`): team_spread, implied_total, moneylines, rest,
coaches, starting QBs, referee, stadium, roof/surface, temp/wind +
humidity/wind-direction parsed from the pbp weather string. Temp is
NA in domes/closed roofs by design — do not impute weather indoors.

**R2.2 — Coordinators are per-GAME, not per-season.**
`data/context/coordinators.csv` is a week-range timeline (season,
team, oc_name, play_caller, from_week, to_week, source). OC-of-record
and play-caller are tracked separately because they diverge
constantly (15 HC-play-caller teams; five 2024–25 in-season
play-calling changes with no OC title change). Every row cites a
source. `validate_coordinators()` must show 0 gaps / 0 overlaps
against pbp before commit (it caught a missing DEN 2025 row).
**Maintenance:** add rows for a new season before week 1; add a
mid-season change the week it happens.

**R2.3 — Prefer `team_play_caller` over `team_oc`** as the scheme
signal; the play-caller is who shapes the offense.

**R2.4 — Depth charts are per-game via snapshots.** Through 2024 the
vendor publishes per-week charts; 2025+ publishes ~daily snapshots.
Rule: a game's chart = the latest snapshot on or before kickoff
date. Coverage verified 1,140/1,140 team-games 2024–25.

**R2.5 — Injury designations encode rest too.** Late-season
Out/Doubtful rows can be "Coaching Decision" (e.g., KC week 18
2024). Availability logic must not read every designation as
health.

**R2.6 — Offseason sheet is data-derived only** (offseason_sheet.R).
The 2026 schedule file ships with head coaches attached, so HC
changes are verifiable; QB1/QB2 come from the latest depth-chart
snapshot, draft classes from the draft_picks release, and veteran
movement from a 2025→2026 roster diff. Three honesty constraints
are baked in: (a) a roster diff CANNOT distinguish a free-agent
signing from a trade — both are labeled "veteran additions";
(b) preseason depth charts are camp snapshots, not Week 1 truth;
(c) coordinator NAMES remain unavailable (R2.2), so the sheet
reports only what is derivable — a 2025 OC who appears as a 2026
head coach (vacancy certain) or a new HC (turnover likely) — and
defers incoming names to coordinators.csv. Coach-name matching is
fuzzy (edit distance ≤2) because the schedule file contains typos
(S17).

**R2.7 — Depth-chart churn view** (depth_sheet.R). The offseason
board is per team x position group: the 2026 depth chart (latest
camp snapshot) with each player marked ROOKIE / NEW / returning,
departures (2025 players with 250+ snaps no longer rostered)
listed under each group, and a value line for 2025 and career
(2021-2025). Rookies are detected by `years_exp == 0` on the 2026
roster and matched to the draft class BY NAME — most 2026 draftees
have no gsis_id yet, so an id-only join silently finds ~1 rookie
in 90 (S18). QUALITY LIMIT: PFF grades are proprietary and in no
nflverse dataset, so the lines carry nflverse production instead;
real grades join automatically from data/pff/ via the roster
pff_id crosswalk. Offensive line is the one group with no public
per-player signal beyond snaps — labeled as such, never faked.

---

## 3. Modeling assumptions (props model core)

**R3.1 — Architecture.** Every market = matchup-adjusted RATE ×
VOLUME-distribution mixture (the PropSZN strikeout recipe).
Receptions: team pass-volume dist × Beta(target share) × Log5 catch
rate → Poisson mixture. Pass yards: volume dist × ratio-adjusted
yards/target × Normal residual. Pass TDs: blended Vegas/volume λ →
empirical λ-bucketed count shape. Anytime TD: Vegas-implied team TDs
× usage-prior TD share → 1 − e^(−λ).

**R3.2 — Volume unit is team targets** (not attempts) so target
shares, catch rates, and yards-per-target stay internally consistent
across sources. Documented proxy; revisit if switching to dropbacks.

**R3.3 — Log5 for probabilities, ratio for continuous rates.**
`calc_matchup_prob()` (player vs defense vs league) for catch rate
and per-opportunity TD rates; `calc_matchup_rate()` for YPT/YPA.

**R3.4 — Defense factors are shrunk empirical-Bayes toward 1.0.**
k grid-searched by one-week-ahead MSE (Study S2): catch-rate k=16,
targets-funnel k=32 (defenses barely control opponent pass volume),
YPT/TD borrow k=16.

**R3.5 — No double-counting Vegas.** Vegas lines already price
opponent quality. Quantities derived FROM the lines
(game-script multiplier, expected team TDs) never get an additional
defense multiplier. Defense factors apply only to efficiency
channels Vegas doesn't decompose (catch rate, YPT, per-target TD
rate, targets funnel). Anytime-TD's residual defense factor is
half-damped (0.5) for this reason.

**R3.6 — Recency weighting.** Exponential half-life 6 games on
player rates (12 for team volume — volume is stickier), extra ×0.6
per season boundary.

**R3.7 — Pass TDs are NOT Poisson.** 1,718 QB-games 2021–24 at
leave-one-out λ: empirical P(0)/P(1)/P(2+) = .225/.338/.438 vs
Poisson .268/.321/.411 — TDs cluster; exactly-1 is rarer, both tails
richer. No mean-preserving Poisson mixture can produce that shape
(P(2+) is concave in λ above ~1), so the model uses an EMPIRICAL
λ-bucketed count distribution, exponentially tilted to the projected
mean (Study S4).

**R3.8 — Rate-variance mixtures only where λ is large.** PropSZN's
K-rate Beta layer helps at λ≈6 (convex region); at pass-TD λ≈1.4 it
actively hurt (dropped P(2+) from .399 to .366 vs empirical .425).
Removed for pass TDs, kept for receptions.

**R3.9 — Shrink star rates toward league.** QB per-target TD rate
shrinks with 10 weighted games of league prior (QB_TDR_PRIOR_GAMES);
without it, projections fanned (2025 Lamar Jackson projected 2.05,
actual 1.00). TD share shrinks toward the positional usage prior
with 24 games (TD_SHARE_SHRINK_GAMES), then keeps 75% of the
residual (ANY_TD_RESID_COMPRESS, tuned on 2024).

**R3.10 — Positional TD priors** (Study S5): td_share ≈ a + b ×
touch_share with QB(−.006, 1.294), RB(−.022, .900), WR(−.010,
1.287), TE(+.021, .911); touch_share = (targets+carries)/(team
targets+carries) — never approximate the denominator.

**R3.11 — Team pass-TD share** (for the Vegas leg of pass TDs)
shrinks toward the league 61.1% with k=16 games.

---

## 4. Game script

**R4.1 — Definition.** A snap's game-script state comes from the
nflfastR win-probability column `wp`, in QUARTILE states
(Study S9; supersedes the original 3-state .25/.75 scheme):
  q1_trail_big  wp < .25
  q2_trail      .25 ≤ wp < .50
  q3_lead       .50 ≤ wp < .75
  q4_lead_big   wp ≥ .75
(quarters 1–4, pass/run snaps, no 2-pt conversions). The legacy
3-state positive/neutral/negative columns are still emitted for
compatibility.

**R4.1a — The cache stores the WP histogram, not a bucketing.**
`gamescript_games.csv` records each team-game's snap shares across
twenty 5%-wide WP bins, so any state scheme with boundaries at
multiples of 0.05 (quartiles, quintiles, legacy 3-state) is
derivable via `gamescript_state_shares()` without ever re-reading
raw pbp. Bucket-scheme changes are now free.

**R4.2 — Use `wp`, NOT `vegas_wp`** (Study S7). `vegas_wp` bakes the
pregame spread into the state from kickoff (a −9 favorite "starts"
at ~78%), so a tied first quarter counts as positive script. Two
findings: (a) behavior given a state is nearly identical under
either definition (neutral pass rate .596 vs .598) — teams play the
score, not the spread; (b) our projection identity is
P(state | spread) × pass-rate(state); defining states with vegas_wp
would put the spread on both sides and double-count it. `wp` keeps
the state purely situational.

**R4.3 — The spread→state-mix table is built from 20 seasons**
(2006–2025, 10,862 team-games) — Study S6. Two-layer cache:
per-team-game rows (`gamescript_games.csv`, committed, immutable for
completed seasons) → aggregated bucket table
(`spread_gamescript.csv`). `refresh_spread_gamescript()` recomputes
only the current season, so the table updates as games are added.
Team-games with <30 recorded offensive snaps are excluded.

**R4.4 — Never trust thin buckets.** With only 2024–25 data the
+17.5 bucket (n=1) showed a 91% positive share; the 20-year value
(n=18) is 59%. Any new bucketed table must report per-bucket n.

**R4.6 — Smoothed spread curve.** Alongside the bucket table, the
per-game cache is kernel-smoothed (Nadaraya-Watson, Gaussian
bandwidth 1.5 spread points, 10-fold-CV-selected — Study S10) into
`spread_gamescript_smooth.csv`: state shares on a half-point spread
grid, with Kish effective sample size per point. Thin numbers borrow
from well-sampled neighbors by distance; the four shares sum to 1 at
every query; the curve is symmetric around 0 by construction. Use
`gamescript_shares_at()` for slate spreads; never use a smoothed
value without checking its eff_n.

**R4.7 — Play-calling is measured by the CALL, not the result.**
called pass = nflfastR `pass` indicator (dropbacks: throws + sacks +
scrambles); called run = `rush` (designed runs). Audited loopholes
(Study S12): all 1,149 in-play 2025 scrambles carry pass==1 (a
scramble is a called pass); sacks live inside called pass; kneels and
spikes carry no intent flags and self-exclude (clock decisions, not
tendencies); nullified snaps with revealed intent (1,527 in 2025)
count toward tendency RATES but never toward play VOLUME (official
snaps only). RPOs are unobservable and land on the called-run side
unless thrown — known limitation. Tendencies are era-scoped: the window is
the 17-game era (2021+, kept in the pbp cache), never 20 years —
league play-calling drifts. Tables: playcall_league.csv / playcall_team.csv
(playcall_tendencies.R), refreshed from the local pbp cache.

**R4.9 — Pass/run split projection (Projection Step 2)**
(pass_split_projection.R): a team-game's expected called-pass rate,
ladder-built walk-forward (train 2022-24, test 2025): league base →
league SCRIPT-EXPECTED rate (smoothed spread→quartile curve × prior
season's per-state league rates) → + team base-rate deviation (EB
k=6) → + script-sensitivity interactions (prior-season dev_q1/q4 ×
expected script shift) → + opponent pass-rate-FACED funnel. The
fitted sens coefficients are NEGATIVE — the regression mean-reverts
prior-season deltas, confirming S12's stability finding in-model.
UNIVERSE BRIDGE: nullified snaps are ~77% pass, so the call-based
rate is multiplied by official_bridge (≈0.989, measured on train)
before it scales OFFICIAL projected plays. Outputs: exp dropbacks =
plays_proj × rate_proj(official); exp designed runs = remainder.
Coefficients in data/context/pass_split_model.csv; score with
project_pass_split().

**R4.8 — Team plays projection (Projection Step 1)**
(plays_projection.R): expected offensive plays for a team-game,
built as an evaluation ladder with all predictors walk-forward
(pre-kickoff only): league prior (previous season's plays/g — the
league slows ~0.5 plays/yr, so the 5-year average enters through a
trend-aware prior) → + team plays/g (season-to-date, EB-shrunk k=6
toward previous season then league) → + opponent plays ALLOWED → +
team & opponent pace (sec/play) → + game script (signed spread,
|spread|, centered total; these carry the smoothed spread→script
curve, which is a deterministic function of spread). Fitted
coefficients live in data/context/plays_model.csv; score slates
with project_team_plays(). HONEST CEILING (S15): plays are ~8.2-sd
noisy per game; the model beats the league baseline by only ~1.2%
MAE but its cross-sectional spread is well-calibrated — treat
projected plays as the MEAN of a wide distribution (per the v1
volume-mixture lesson), never as a point fact.

**R4.5 — Play-volume projection** (ported from the original model):
plays = mean of (team plays for + opp plays allowed)/2 and
team plays × (opp allowed / league avg); pass/rush split two ways —
v1 season rate, v2 expected state mix (from R4.3) × the team's
conditional pass/rush rates per state.

---

## 5. Backtesting & evaluation rules

**R5.1 — No lookahead, ever.** Every feature builder takes
(asof_season, asof_week) and uses only strictly-earlier games.
Walk-forward: fit on data before week w, project week w.

**R5.2 — Era discipline for constants.** Structural constants were
calibrated on 2021–2024 (`calibrate.R`); 2024 walk-forward tuned
three post-calibration knobs (QB_TDR_PRIOR_GAMES,
ANY_TD_RESID_COMPRESS, the empirical TD shape); 2025 is untouched
holdout. Re-run `calibrate.R` after each season and refresh the
constants in `model_core.R` with updated provenance comments.

**R5.3 — Evaluate distributions, not just points.** MAE vs a naive
baseline, P(over) calibration by predicted-probability bucket, PIT
uniformity for continuous markets, Brier for binary. A model that
wins MAE but is miscalibrated in the tails loses money.

**R5.4 — Condition like the market settles.** Played-but-zero-target
games count as unders (dropping them inflated the empirical over
rate ~5pp); props void only for inactive players.

**R5.5 — Holdout results of record (2025 wk 5–17):** receptions MAE
1.32 vs 1.34 naive, calibration within ~2–3pp; pass yards MAE 61.2
vs 65.4, PIT near-uniform; pass TDs mean 1.49 vs 1.40 (~1.5σ),
P(2+) .445 vs .425; anytime TD Brier .154 vs .165 naive. Full
record: BACKTEST.md.

---

## 6. Studies & analyses log

**S1 — Spread sign / implied-total validation** (calibrate.R §1;
2021–24). cor(home margin, spread_line) = +0.437; implied totals
track actual team points monotonically (15.7→15.2 low bucket,
27.9→30.1 top; cor 0.43).

**S2 — Defense-factor shrinkage grid** (calibrate.R §4). One-week-
ahead MSE minimized at k=16 (catch rate allowed), k=32 (targets
funnel). Defenses have little stable control over opponent pass
volume.

**S3 — Game-script pass-volume regression** (calibrate.R §2;
n=4,236 team-games). log(team targets) ~ +0.00212·spread (p=.023;
favorites sustain drives — the "trailing teams throw" effect nets
out positive for favorites) + 0.00995·(total−44.7) (p<1e-12; ~1%
volume per point of total). Clamped [0.85, 1.15].
Expected TDs ~ implied total: −0.709 + 0.137·implied (calibrate §3).

**S4 — Pass-TD count distribution is non-Poisson** (2026-07). See
R3.7/R3.8. Led to the empirical λ-bucketed shape + tilt, and to
removing the rate-variance layer for low-λ markets.

**S5 — Positional touch-share → TD-share mapping** (calibrate.R §6;
player-seasons 2021–24, n≥8 games). Coefficients in R3.10; WR
touches carry ~29% more end-zone equity per touch, RB ~10% less.
Also surfaced a denominator bug (targets×1.45 ≈ 20% inflation) —
hence the "never approximate the denominator" rule.

**S6 — 20-season spread game-script table** (2026-07; 2006–2025,
10,862 team-games). Monotonic across all 14 buckets; quantified
thin-tail distortion (R4.4). Bucket ns: 14–1,976.

**S7 — wp vs vegas_wp for game-script states** (2026-07; 2024–25,
~69k snaps). Play-level state agreement 82.8% overall but 73.4% in
Q1 vs 93.6% in Q4 — the divergence is pregame-prior contamination.
A +7.5–10 favorite spends 62% of snaps "positive" under vegas_wp vs
35% under wp. Pass rate within each state is nearly identical under
both definitions. Decision: R4.2 (use wp).

**S9 — WP bucket granularity** (2026-07; 2024–25, ~69k snaps).
Pass rate falls monotonically across the WP range: 80% at wp<.05 to
36% at wp>.95. The 3-state scheme's "neutral" band (.25–.75, 38k
snaps) hid a real split at its midpoint: trailing-neutral passes at
61.9%, leading-neutral at 57.5% (~67% vs ~57% at wp≈.26 vs .74).
Snap-level R² of pass~state: 3-state .0298, quartiles .0308,
quintiles .0343. Decision: quartiles become canonical (R4.1), and
the cache stores the full 5%-bin histogram (R4.1a) so finer schemes
remain one function call away. 20-season quartile table verified
monotonic in all four states across all 14 spread buckets.

**S10 — Kernel smoothing of the spread curve** (2026-07; 10,862
team-games). 10-fold CV over Gaussian bandwidths {.75, 1, 1.5, 2, 3,
4}: flat minimum at h = 1.5 (MSE ×1e4: 566.06 vs 566.41/569.94 at
the extremes). Motivating case verified: raw spread-16 favorites
(n=10 after the 30-snap filter) showed noisy, non-monotone shares
vs their 15.5/16.5 neighbors; the smoothed estimate at 16 rests on
eff_n = 211 and sits monotonically between them. Curve q4 share is
monotone in spread; symmetry holds to |Δ| ≤ .015 (residual from the
30-snap filter occasionally dropping one side of a game).

**S11 — Penalty / timeout audit of the pbp filters** (2026-07; 2025
season). Of 4,732 `no_play` rows: 2,140 timeouts, 2,536 accepted
penalties; 1,527 of the penalty rows were real dropbacks/runs that
were nullified — 4.4% of the pass/run snap population (e.g. deep DPI
targets). Kept population contains 527 stood-with-penalty snaps
(1.5%), 1,352 sacks, 1,149 scrambles, zero kneels/spikes. Outcome:
rule R1.10 + penalty flag added to FEATURE_PBP_COLS; penalty-drawn
targets flagged as a future usage feature.

**S18 — Depth-chart churn build** (2026-08). 1,184 depth slots +
270 departures across 32 teams. Status mix 851 returning / 243 new
/ 90 rookies. Two joins failed silently before verification: (a)
2026 draft picks mostly have NO gsis_id in the draft release, so
id-based rookie detection found 1 of 90 — fixed with years_exp +
name matching; (b) re-joining snap counts inside the value helper
collided with a pre-existing column. Illustrative output: LV's QB
room reads Cousins (NEW, from ATL) over Mendoza (ROOKIE, R1.1);
MIN shows Murray (NEW, from ARI) ahead of McCarthy with Harrison
Smith (793 snaps) out to free agency.

**S17 — 2026 offseason sheet** (2026-08). Derived entirely from
cached nflverse data: 7 HC changes (BAL Harbaugh→Minter, CLE
Stefanski→Monken, LV Carroll→Kubiak, MIA McDaniel→Hafley, NYG
Daboll→Harbaugh, PIT Tomlin→McCarthy, TEN Callahan→Saleh); 6 new
QB1s (ATL Penix→Tagovailoa, CLE Sanders→Watson, LV Smith→Cousins,
MIA Tagovailoa→Willis, MIN McCarthy→Murray, NYJ Taylor→Smith); 257
draft picks with 22 teams taking a QB/RB/WR/TE in rounds 1-3.
DATA-QUALITY FINDING: the schedule file misspells LV's new HC as
"Klint Kubliak", which silently broke the exact-match test for
"did a 2025 OC leave to become a head coach" — SEA's vacancy went
undetected until fuzzy matching (edit distance ≤2) was added.
Any future coach-name join must be fuzzy or explicitly aliased.

**S16 — Pass-split ladder & end-to-end volume** (2026-07; test 2025,
n=570). Rate MAE: league .0808 → script curve .0793 → +team .0796 →
+sens .0786 → +opp funnel .0779 (rate sd .0998). Monotone quintile
calibration with the top quintile ~3pp hot (.666 proj / .636 act) —
watch in-season. Coefs: team_dev 0.73, opp_dev 0.45, sens_q1 −1.13
(mean-reversion of prior deltas). End-to-end with Step 1 on the
OFFICIAL universe after the 0.9887 bridge: dropbacks 36.8 proj vs
36.2 actual, MAE 6.28 vs 6.44 baseline; designed runs 23.8 vs 23.9,
MAE 5.33 vs 5.40. Eval lesson recorded: call universe ≠ official
universe (~2.6 snaps/team-game) — never mix them in an evaluation.

**S15 — Plays-projection ladder** (2026-07; train 2022-24, test
2025, n=570 test team-games). MAE by rung: league baseline 6.57 →
+team 6.52 → +opponent-allowed 6.51 → +pace 6.49 → +game-script
6.50 (script terms help in-sample, wash on holdout — kept on
theory, flagged). Coefficients: team_dev 0.26 (heavy shrink —
a +4-plays team projects +1), pace_dev −0.52 plays per second of
pace, opp_pace_dev +0.21, team_spread +0.15/pt (favorites sustain
drives, consistent with S3), ctotal +0.08. 2025 projections span
56.3–64.1 with monotone quintile calibration (58.7→57.4 … 62.3→
62.2). Conclusion: play volume is mostly irreducible noise
(sd 8.2); the projection's job is centering a distribution, and
downstream models must consume it as such.

**S14 — Defense profile build & identity check** (2026-07; 2021-25).
Defense-allowed totals reconcile EXACTLY with offense totals (2025:
58,303 rush yds / 128,399 rec yds / 17,490 targets on both sides) —
the mirror drops and double-counts nothing. League comp%-allowed
depth gradient .767/.741/.681/.532/.362. 2025 stingiest pass
defenses by YPT allowed: DEN 6.43 (9.9% sack rate — league leader),
JAX 6.49, SEA 6.55; most generous WAS/TEN/DAL 8.5-8.7. Positional
target funnels span WR-share-faced .47 (CIN) to .62 (LAC).

**S13 — Player profile build & reconciliation** (2026-07; 2021-25,
28,058 player-games, 3,055 player-seasons, 1,211 era careers).
Reconciles exactly with official weekly stats (Chase 2025: 185
targets / 125 receptions both sources). Depth-band completion
gradients are textbook (Allen 2025: 85% short falling to 36% on
20+). Two bugs caught by verification: scrambles carry no
passer_player_id (rate silently zero until attributed via
rusher_player_id), and missing 2021-23 roster files broke the
RB-only carry-share denominator (RB determination needs the
season's roster). Era bell-cow check: Henry .82 / Mixon .80 /
Taylor .80 RB-carry share. Includes red-zone + end-zone target
counts, inside-10/-5 carries, EPA and success rates, stuff and
explosive rates, target depth mix, CPOE, sack/scramble rates.

**S12 — Play-calling tendencies by script quartile** (2026-07;
17-game era 2021-2025, 181,945 called snaps, 160 team-seasons).
League called-pass rate 61.4%, stable by season (60.8-62.0) and
monotone across script: 74.4% trailing big / 62.7% / 58.8% / 50.0%
leading big. Real slow-down trend: 61.9 plays per team-game (2021)
-> 60.1 (2025); pace 29.6 -> 30.4 s/snap. Pooled pace: 29.9 s/snap;
26.0 s after a called pass vs ~36 s after a called run
(incompletions stop the clock); trailing-big offenses accelerate to
26.0 s/snap. 2025 team base rates span SEA 52.5% to ARI 69.7%;
trailing-big shift spans WAS +4.3pp to NE +28.2pp.

**Trait stability (the modeling-relevant result):** year-over-year
correlations across 128 same-team season pairs — sec/snap pace 0.51,
base pass rate 0.42, plays/g 0.24, script-sensitivity dev_q1 0.22 /
dev_q4 0.19. A team's base rate and pace carry real signal; its
script-sensitivity DELTAS are mostly noise and must be shrunk hard
toward the league curve. Follow-up once coordinators cover more
seasons: condition stability on play-caller continuity.

**S8 — Legacy weekly-log model** (2026-07). The v1 pipeline
(data_sources.R hvpkod-derived weekly caches) validated the market
models before pbp data was available; superseded by the official
stats_player weekly files + pbp. Kept for the backtest record.

---

## 7. Data quirks & gotchas

- **Depth chart format break (2025):** per-week → ~daily snapshots
  (`dt` column); loader normalizes (R2.4).
- **SF 2024 has no official OC** — oc_name NA, play_caller Shanahan.
- **stats_player release naming:** weekly player stats live under the
  `stats_player` release tag (`stats_player_week_{yyyy}.csv`), not
  the legacy `player_stats` tag.
- **NGS files are one-per-stat-type** covering all seasons
  (`ngs_receiving.csv.gz` etc.); week 0 rows are season aggregates —
  filter them out.
- **All-NA CSV columns type as logical** on re-read; the season
  binder harmonizes conflicting column types to character.
- **pbp weather:** temp/wind NA indoors; humidity parseable from the
  weather string ~93% of snaps including domes.
- **ifelse over an all-NA vector stays logical** — wrap
  case_when branches in as.character() (results_tracker fix).
- **Snap-count IDs:** pfr_id → gsis_id crosswalk first, then
  unique-name fallback, then team-qualified name (original script's
  three-stage join, preserved).
- **Rosters carry the full ID crosswalk** (gsis/pfr/pff/espn) — all
  cross-source joins go through it, never through raw names.

## 8. Operational runbook

Weekly, in season (`Rscript run_model.R`): refresh caches → pbp →
gamescript (current season) → rates/factors → project slate → fetch
props (Action Network; ledger dedupes to genuine ticks, line is part
of market identity for CLV) → snapshot + card + report → match
results to actuals.

Before 2026 week 1:
1. coordinators.csv: add 2026 rows (validate: 0 gaps / 0 overlaps)
2. verify Action Network NFL market slugs (`discover_prop_markets()`)
3. re-run `calibrate.R` on 2021–2025, refresh model_core.R constants
4. confirm 2026 files appear (pbp, injuries, depth charts, rosters,
   snap counts, weekly stats) on first refresh
