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
