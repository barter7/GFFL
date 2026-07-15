# ============================================================
#  model_core.R  (nflmodel)
#
#  Shared projection model functions — sourced by run_model.R,
#  backtest.R and results_tracker.R. The NFL sibling of
#  PropSZN's model_core.R: every market follows the same
#  architecture that the strikeout model uses —
#
#     matchup-adjusted RATE  ×  VOLUME distribution mixture
#
#  Markets:
#    1. Receptions      — team pass-volume dist × Beta(target share)
#                         × Log5 catch rate      → P(rec = n)  (Poisson mix)
#    2. QB pass yards   — team pass-volume dist × ratio-adjusted
#                         yards/target × Normal residual → P(yds > line)
#    3. QB pass TDs     — blended Vegas/volume λ × Beta(TD rate)
#                         → P(TD = n) (Poisson mix)
#    4. Anytime TD      — Vegas-implied team TDs × player TD share
#                         → P(≥1) = 1 − exp(−λ)
#
#  Requires utils.R and data_sources.R to be sourced first.
# ============================================================

library(dplyr)
library(tidyr)

# ── Calibrated constants ─────────────────────────────────────
#
# All fit by calibrate.R on 2021-2024 regular seasons (2025 held
# out for the walk-forward backtest). Provenance in comments —
# re-run calibrate.R after each season and refresh these.

# Game-script pass-volume multiplier.
#   log(team_targets) ~ team_spread + (total_line − 44.72), n = 4,236
#   team-games:
#     spread slope = +0.00212 (p = .023) — favored teams sustain
#       drives and run MORE plays; the "trailing teams throw more"
#       effect nets out to slightly positive for favorites.
#     total slope  = +0.00995 (p < 1e-12) — each point of Vegas
#       total ≈ +1% pass volume.
#   Clamped like PropSZN's OPP_K_ADJ to avoid silly multipliers
#   at extreme lines (e.g. 2022 DEN games with 36.5 totals).
GS_VOL_SPREAD_SLOPE <- 0.00212
GS_VOL_TOTAL_SLOPE  <- 0.00995
GS_VOL_TOTAL_BASE   <- 44.72
GS_VOL_CLAMP        <- c(0.85, 1.15)

# Expected offensive TDs from Vegas implied team total.
#   team_off_td ~ implied_total, n = 4,236 team-games:
#   intercept = −0.7086, slope = +0.1373 (t = 17.6).
#   At the league-average implied total (22.4) this gives the
#   observed league mean of 2.36 offensive TD/game.
EXP_TD_INTERCEPT <- -0.7086
EXP_TD_SLOPE     <-  0.1373

# League pass share of offensive TDs (2021-2024): 61.1%. Teams'
# own pass shares are shrunk toward this with k games of prior
# weight (backtest-tuned: team identity carries real signal here
# — using the league share flattened P(2+ pass TD) for high-pass
# offenses and underpredicted the 2+ rate by ~6pp).
LG_PASS_TD_SHARE      <- 0.611
PASS_TD_SHARE_SHRINK  <- 16

# Defense-factor empirical-Bayes shrinkage (games of prior weight).
#   Grid-searched by one-week-ahead MSE in calibrate.R:
#   catch-rate-allowed factor  k = 16 (MSE-optimal on 2021-24)
#   targets-funnel factor      k = 32 (defenses barely control
#     opponent pass volume — shrink hard)
#   ypt / TD factors borrow k = 16 (same sample-size regime as
#     catch rate; not separately searched).
DEF_CR_SHRINK_K  <- 16
DEF_VOL_SHRINK_K <- 32
DEF_YPT_SHRINK_K <- 16
DEF_TD_SHRINK_K  <- 16

# Player-rate recency weighting: exponential, half-life 6 games,
# with an extra ×0.6 carry-over multiplier across season
# boundaries (offseason roster/scheme churn).
RECENCY_HALF_LIFE <- 6
SEASON_CARRYOVER  <- 0.6

# Catch-rate shrinkage toward league in targets (Beta prior mass).
CR_PRIOR_TARGETS <- 25

# TD-share shrinkage toward usage-based prior, in games.
# Backtest-tuned upward from 12: at 12 the anytime-TD high-
# probability buckets ran ~10pp hot (pred 0.59 vs emp 0.44) —
# hot streaks were being trusted too much.
TD_SHARE_SHRINK_GAMES <- 24

# League baselines (2021-2024, calibrate.R §5) — used only as
# fallbacks; live values are recomputed from the data window.
LG_CATCH_RATE_DEFAULT <- 0.6758
LG_TEAM_TARGETS_DEFAULT <- 32.06
LG_YPT_DEFAULT <- 7.42
LG_PASS_TD_PER_TGT_DEFAULT <- 0.0447

# Grids for the mixture discretizations (PropSZN uses 50 K-rate
# bins over 0.02-0.60; we match that resolution).
TS_GRID  <- seq(0.005, 0.60, length.out = 50)   # target share
TDR_GRID <- seq(0.002, 0.15, length.out = 50)   # pass TD per target
REC_MAX  <- 20L
PTD_MAX  <- 8L

# ============================================================
#  Data-window builders
#  Every builder takes `logs` already filtered to the modeling
#  window (all games strictly BEFORE the slate being projected)
#  so backtest.R can guarantee no lookahead.
# ============================================================

# ── Team-week aggregates ─────────────────────────────────────
build_team_weeks <- function(logs) {
  logs %>%
    group_by(season, week, team) %>%
    summarise(
      team_targets    = sum(targets, na.rm = TRUE),
      team_carries    = sum(carries, na.rm = TRUE),
      team_receptions = sum(receptions, na.rm = TRUE),
      team_rec_yards  = sum(rec_yards, na.rm = TRUE),
      team_pass_td    = sum(pass_td, na.rm = TRUE),
      team_off_td     = sum(pass_td, na.rm = TRUE) + sum(rush_td, na.rm = TRUE),
      opponent        = first(opponent),
      .groups = "drop"
    ) %>%
    filter(team_targets >= 10)
}

# ── League baselines from the window ─────────────────────────
build_league_baselines <- function(team_weeks) {
  list(
    catch_rate      = sum(team_weeks$team_receptions) / sum(team_weeks$team_targets),
    team_targets    = mean(team_weeks$team_targets),
    ypt             = sum(team_weeks$team_rec_yards) / sum(team_weeks$team_targets),
    pass_td_per_tgt = sum(team_weeks$team_pass_td) / sum(team_weeks$team_targets),
    off_td_pg       = mean(team_weeks$team_off_td)
  )
}

# ── Empirical team pass-volume distribution ──────────────────
#
#  The NFL analog of PropSZN's batters-faced distribution: the
#  team's last `n_recent` games' target totals, recency-weighted.
#  Returned as support + weights; the game-script multiplier is
#  applied to the support at projection time.
build_team_volume_dist <- function(team_weeks, team_, n_recent = 16) {
  tw <- team_weeks %>%
    filter(team == team_) %>%
    arrange(season, week) %>%
    tail(n_recent)
  if (nrow(tw) == 0) return(NULL)
  games_ago <- rev(seq_len(nrow(tw))) - 1
  w <- recency_weights(games_ago, RECENCY_HALF_LIFE * 2)  # volume is sticky;
                                                          # decay slower than skill
  list(support = tw$team_targets, weights = w / sum(w), n_games = nrow(tw))
}

# ── Game-script volume multiplier ────────────────────────────
#  team_spread : points this team is favored by (negative = dog)
#  total_line  : Vegas game total
game_script_mult <- function(team_spread, total_line,
                             clamp = GS_VOL_CLAMP) {
  if (is.na(team_spread) || is.na(total_line)) return(1.0)
  m <- exp(GS_VOL_SPREAD_SLOPE * team_spread +
           GS_VOL_TOTAL_SLOPE  * (total_line - GS_VOL_TOTAL_BASE))
  pmax(clamp[1], pmin(clamp[2], m))
}

# ── Vegas-implied expected offensive TDs ─────────────────────
expected_team_tds <- function(implied_total, lg_off_td_pg) {
  if (is.na(implied_total)) return(lg_off_td_pg)
  pmax(0.5, EXP_TD_INTERCEPT + EXP_TD_SLOPE * implied_total)
}

# ── Defense factors ──────────────────────────────────────────
#
#  One row per defense, all factors shrunk toward 1.0.
#  Built from what each defense ALLOWED, weighted toward the
#  current season (previous seasons decayed by SEASON_CARRYOVER
#  per boundary — defenses change fast).
#
#  NOTE on double counting (same reasoning as PropSZN's
#  opp_k_adjustment wOBA-vs-K% discussion): Vegas lines already
#  price opponent quality, so the Vegas-derived quantities
#  (game_script_mult, expected_team_tds) must NOT also get a
#  defense multiplier. Defense factors are applied only to
#  EFFICIENCY channels Vegas doesn't decompose: catch rate,
#  yards per target, per-target TD rate, and the targets funnel.
build_defense_factors <- function(team_weeks, lg) {
  max_season <- max(team_weeks$season)
  team_weeks %>%
    mutate(w = SEASON_CARRYOVER ^ (max_season - season)) %>%
    group_by(defense = opponent) %>%
    summarise(
      n_g    = sum(w),
      f_vol  = (sum(team_targets * w) / sum(w)) / lg$team_targets,
      f_cr   = (sum(team_receptions * w) / sum(team_targets * w)) / lg$catch_rate,
      f_ypt  = (sum(team_rec_yards * w) / sum(team_targets * w)) / lg$ypt,
      f_ptd  = (sum(team_pass_td * w) / sum(team_targets * w)) / lg$pass_td_per_tgt,
      f_otd  = (sum(team_off_td * w) / sum(w)) / lg$off_td_pg,
      .groups = "drop"
    ) %>%
    mutate(
      f_vol = shrink_factor(f_vol, n_g, DEF_VOL_SHRINK_K),
      f_cr  = shrink_factor(f_cr,  n_g, DEF_CR_SHRINK_K),
      f_ypt = shrink_factor(f_ypt, n_g, DEF_YPT_SHRINK_K),
      f_ptd = shrink_factor(f_ptd, n_g, DEF_TD_SHRINK_K),
      f_otd = shrink_factor(f_otd, n_g, DEF_TD_SHRINK_K)
    )
}

default_def_factors <- function() {
  tibble(defense = NA_character_, n_g = 0,
         f_vol = 1, f_cr = 1, f_ypt = 1, f_ptd = 1, f_otd = 1)
}

# ── Team pass share of offensive TDs ─────────────────────────
#  Recency-weighted (season carryover) share of a team's
#  offensive TDs that came through the air, shrunk toward the
#  league's 61.1%. Feeds the Vegas leg of the pass-TD model so a
#  pass-heavy offense's implied TDs convert to pass TDs at its
#  own rate rather than the league's.
team_pass_td_share <- function(team_weeks, team_) {
  max_season <- max(team_weeks$season)
  tw <- team_weeks %>%
    filter(team == team_) %>%
    mutate(w = SEASON_CARRYOVER ^ (max_season - season))
  if (nrow(tw) == 0) return(LG_PASS_TD_SHARE)
  n_w   <- sum(tw$w)
  share <- sum(tw$team_pass_td * tw$w) / pmax(sum(tw$team_off_td * tw$w), 1)
  (share * n_w + LG_PASS_TD_SHARE * PASS_TD_SHARE_SHRINK) /
    (n_w + PASS_TD_SHARE_SHRINK)
}

# ── Player rates ─────────────────────────────────────────────
#
#  One row per player: recency-weighted usage and efficiency,
#  plus game-to-game variance where a mixture needs it.
#  Only rows where the player actually participated count
#  (an injured week is a missing game, not a zero-usage game).
build_player_rates <- function(logs, team_weeks) {
  max_season <- max(logs$season)

  pl <- logs %>%
    inner_join(team_weeks %>%
                 select(season, week, team, team_targets, team_carries,
                        team_off_td),
               by = c("season", "week", "team")) %>%
    mutate(played = (targets + carries > 0) | (position == "QB" & pass_yards > 0)) %>%
    filter(played) %>%
    group_by(player_id) %>%
    arrange(season, week, .by_group = TRUE) %>%
    mutate(
      games_ago = n() - row_number(),
      w = recency_weights(games_ago, RECENCY_HALF_LIFE) *
          SEASON_CARRYOVER ^ (max_season - season)
    ) %>%
    ungroup()

  pl %>%
    group_by(player_id) %>%
    summarise(
      player_name = last(player_name),
      position    = last(position),
      team        = last(team),
      n_games     = n(),
      w_games     = sum(w),
      # usage
      ts_mean = w_mean(targets / team_targets, w),
      ts_sd   = w_sd(targets / team_targets, w),
      tgt_pg  = w_mean(targets, w),
      # receiving efficiency (shrunk toward league at projection time)
      rec_w   = sum(receptions * w),
      tgt_w   = sum(targets * w),
      # TD involvement
      td_share_raw = w_mean(ifelse(team_off_td > 0,
                                   (rush_td + rec_td) / team_off_td, 0), w),
      touch_share  = w_mean((targets + carries) /
                              pmax(team_targets + team_carries, 1), w),
      rz_pg        = w_mean(rz_targets + rz_touches, w),
      # QB channel
      qb_ypt_mean  = w_mean(ifelse(position == "QB" & pass_yards > 0,
                                   pass_yards / team_targets, NA), w),
      qb_yds_sd    = w_sd(ifelse(position == "QB" & pass_yards > 0,
                                 pass_yards, NA), w),
      qb_tdr_mean  = w_mean(ifelse(position == "QB" & pass_yards > 0,
                                   pass_td / team_targets, NA), w),
      qb_tdr_sd    = w_sd(ifelse(position == "QB" & pass_yards > 0,
                                 pass_td / team_targets, NA), w),
      qb_games     = sum(position == "QB" & pass_yards > 0),
      .groups = "drop"
    )
}

# ============================================================
#  Market 1 — Receptions
#
#  P(rec = r) = Σ_v P(vol = v) Σ_s P(share = s)
#                  Poisson(r | v · f_vol · gs · s · cr_adj)
#
#  The double mixture is the direct analog of PropSZN's
#  compute_double_mixture: volume distribution (BF dist ↔ team
#  targets dist) × rate variance (K-rate Beta ↔ target-share
#  Beta), with the matchup-adjusted catch rate playing the role
#  of the per-batter matchup K%.
# ============================================================
project_receptions <- function(player, vol_dist, def, gs_mult, lg,
                               rec_max = REC_MAX) {
  if (is.null(vol_dist) || is.na(player$ts_mean) || player$ts_mean <= 0)
    return(NULL)

  # Matchup catch rate: player (shrunk toward league) vs defense via Log5
  cr_player <- (player$rec_w + lg$catch_rate * CR_PRIOR_TARGETS) /
               (player$tgt_w + CR_PRIOR_TARGETS)
  cr_def    <- pmin(0.95, pmax(0.35, lg$catch_rate * def$f_cr))
  cr_adj    <- calc_matchup_prob(cr_player, cr_def, lg$catch_rate)

  # Target-share Beta weights (variance floor mirrors PropSZN's
  # 0.05 SD floor for low-variance pitchers)
  ts_sd <- ifelse(is.na(player$ts_sd) | player$ts_sd < 0.02, 0.04, player$ts_sd)
  ts_w  <- beta_grid_weights(player$ts_mean, ts_sd, TS_GRID)
  if (is.null(ts_w)) return(NULL)

  r_range <- 0:rec_max
  probs   <- rep(0, length(r_range))
  v_eff   <- vol_dist$support * def$f_vol * gs_mult
  for (i in seq_along(v_eff)) {
    vw <- vol_dist$weights[i]
    if (vw == 0) next
    lam <- v_eff[i] * TS_GRID * cr_adj          # vector over share grid
    for (j in seq_along(TS_GRID)) {
      if (ts_w[j] == 0) next
      probs <- probs + vw * ts_w[j] * dpois(r_range, lam[j])
    }
  }
  exp_rec <- sum(r_range * probs)
  list(probs = probs, exp_rec = exp_rec, cr_adj = cr_adj,
       exp_targets = sum(v_eff * vol_dist$weights) * player$ts_mean)
}

# ============================================================
#  Market 2 — QB passing yards
#
#  P(Y > line) = Σ_v P(vol = v) ·
#                  P( Normal(v · f_vol · gs · ypt_adj, σ_resid) > line )
#
#  σ_resid is the QB's per-game yardage SD deflated for the
#  volume variance already carried by the mixture (the mixture
#  supplies the volume component; the Normal supplies the
#  efficiency component). The 0.85 deflator approximates
#  sqrt(1 − share of variance explained by volume) — volume
#  explains ~25-30% of per-game passing yards variance
#  (r² of yards ~ team targets on 2021-24 ≈ 0.28).
# ============================================================
QB_YDS_SD_DEFLATOR <- 0.85
QB_YDS_SD_FLOOR    <- 35

project_pass_yards <- function(player, vol_dist, def, gs_mult, lg) {
  if (is.null(vol_dist) || is.na(player$qb_ypt_mean) || player$qb_games < 3)
    return(NULL)

  ypt_adj <- calc_matchup_rate(player$qb_ypt_mean,
                               lg$ypt * def$f_ypt, lg$ypt)
  sd_res  <- max(QB_YDS_SD_FLOOR,
                 QB_YDS_SD_DEFLATOR * ifelse(is.na(player$qb_yds_sd), 60,
                                             player$qb_yds_sd))
  v_eff   <- vol_dist$support * def$f_vol * gs_mult
  exp_yds <- sum(v_eff * vol_dist$weights) * ypt_adj

  p_over <- function(line) {
    sum(vol_dist$weights * (1 - pnorm(line, mean = v_eff * ypt_adj, sd = sd_res)))
  }
  list(exp_yds = exp_yds, ypt_adj = ypt_adj, sd_res = sd_res, p_over = p_over)
}

# ============================================================
#  Market 3 — QB passing TDs
#
#  λ blends two independent estimates (50/50):
#    λ_vol   = E[targets] · f_vol · gs · tdr_adj  (rate × volume,
#              same channel as receptions)
#    λ_vegas = expected_team_tds(implied) · team pass share
#              (Vegas already prices the defense — no def factor)
#
#  The count DISTRIBUTION around λ is NOT Poisson. Walk-forward
#  evidence (and 1,718 QB-games 2021-24 at leave-one-out λ):
#  empirical P(0)=0.225 / P(1)=0.338 / P(2+)=0.438 versus Poisson
#  0.268 / 0.321 / 0.411 — pass TDs cluster (a team that finds the
#  end zone through the air keeps throwing there), so exactly-1 is
#  rarer and 0 / 2+ are both richer than Poisson allows. No
#  mean-preserving Poisson mixture can produce that shape (P(2+)
#  is concave in λ here), so instead of a parametric family we use
#  the same device PropSZN uses for batters faced: an EMPIRICAL
#  distribution — historical QB-games bucketed by λ, interpolated,
#  then exponentially tilted to match the projected λ exactly.
# ============================================================
PTD_VEGAS_BLEND <- 0.5

# QB per-target TD-rate shrinkage toward league, in weighted
# games of prior mass. Without it the volume leg fans out:
# 2025 walk-forward showed actual ~ 0.77·projected + regression
# (stars off career years — 2.05 projected vs 1.00 actual for
# 2025 Lamar Jackson — and low-λ backups both missed toward the
# league mean). Tuned on 2024, validated on 2025.
QB_TDR_PRIOR_GAMES <- 10

# ── Empirical TD-count shape table ───────────────────────────
#  Buckets historical QB-games by leave-one-out season λ and
#  records the empirical TD-count distribution per bucket.
#  Built from the modeling window (no lookahead).
build_qb_td_shape <- function(logs, n_bins = 6, k_max = PTD_MAX,
                              min_games = 8) {
  qb <- logs %>%
    filter(position == "QB", pass_yards > 0) %>%
    group_by(season, player_id) %>%
    filter(n() >= min_games) %>%
    mutate(lam_hat = (sum(pass_td) - pass_td) / (n() - 1)) %>%
    ungroup()
  if (nrow(qb) < 300) return(NULL)

  brks <- unique(quantile(qb$lam_hat, probs = seq(0, 1, length.out = n_bins + 1)))
  qb$bin <- cut(qb$lam_hat, brks, include.lowest = TRUE)

  centers <- tapply(qb$lam_hat, qb$bin, mean)
  mat <- t(sapply(levels(qb$bin), function(b) {
    x <- pmin(qb$pass_td[qb$bin == b], k_max)
    cnt <- tabulate(x + 1, nbins = k_max + 1) + 0.25   # add-quarter smoothing
    cnt / sum(cnt)
  }))
  list(centers = as.numeric(centers), mat = mat, k_max = k_max)
}

# ── Exponential tilt: reshape probs to have mean = target ────
#  P'(k) ∝ P(k)·t^k with t solved so Σ k·P'(k) = target.
#  Preserves the empirical shape (contagion) while hitting the
#  model's λ exactly.
tilt_to_mean <- function(probs, target, tol = 1e-6, iters = 60) {
  k <- seq_along(probs) - 1
  f <- function(lt) sum(k * probs * exp(lt * k)) / sum(probs * exp(lt * k))
  lo <- -3; hi <- 3
  if (f(lo) > target || f(hi) < target) return(probs)  # out of reach — bail
  for (i in 1:iters) {
    mid <- (lo + hi) / 2
    if (f(mid) < target) lo <- mid else hi <- mid
    if (hi - lo < tol) break
  }
  w <- probs * exp((lo + hi) / 2 * k)
  w / sum(w)
}

project_pass_tds <- function(player, vol_dist, def, gs_mult, lg,
                             implied_total, td_max = PTD_MAX,
                             pass_share = LG_PASS_TD_SHARE,
                             td_shape = NULL) {
  if (is.null(vol_dist) || is.na(player$qb_tdr_mean) || player$qb_games < 3)
    return(NULL)

  tdr_shrunk <- (player$qb_tdr_mean * player$w_games +
                   lg$pass_td_per_tgt * QB_TDR_PRIOR_GAMES) /
                (player$w_games + QB_TDR_PRIOR_GAMES)
  tdr_adj <- calc_matchup_prob(
    pmin(0.12, pmax(0.005, tdr_shrunk)),
    pmin(0.12, pmax(0.005, lg$pass_td_per_tgt * def$f_ptd)),
    lg$pass_td_per_tgt
  )

  v_eff    <- vol_dist$support * def$f_vol * gs_mult
  exp_vol  <- sum(v_eff * vol_dist$weights)
  lam_vol  <- exp_vol * tdr_adj
  lam_veg  <- expected_team_tds(implied_total, lg$off_td_pg) * pass_share
  lam_mix  <- (1 - PTD_VEGAS_BLEND) * lam_vol + PTD_VEGAS_BLEND * lam_veg

  t_range <- 0:td_max
  if (!is.null(td_shape)) {
    # Empirical shape: interpolate between the two λ-buckets
    # bracketing lam_mix, then tilt to the projected mean.
    ctr <- td_shape$centers
    if (lam_mix <= ctr[1]) {
      base <- td_shape$mat[1, ]
    } else if (lam_mix >= ctr[length(ctr)]) {
      base <- td_shape$mat[length(ctr), ]
    } else {
      hi <- which(ctr >= lam_mix)[1]; lo <- hi - 1
      wu <- (lam_mix - ctr[lo]) / (ctr[hi] - ctr[lo])
      base <- (1 - wu) * td_shape$mat[lo, ] + wu * td_shape$mat[hi, ]
    }
    probs <- tilt_to_mean(base, lam_mix)
  } else {
    # Fallback: Poisson mixed over the volume distribution
    rate_c <- lam_mix / exp_vol
    probs  <- rep(0, length(t_range))
    for (i in seq_along(v_eff)) {
      probs <- probs + vol_dist$weights[i] * dpois(t_range, v_eff[i] * rate_c)
    }
  }
  list(probs = probs, exp_td = sum(t_range * probs),
       lam_vol = lam_vol, lam_vegas = lam_veg)
}

# ============================================================
#  Market 4 — Anytime TD
#
#  λ_player = expected_team_tds(implied_total) · td_share_adj
#  P(anytime) = 1 − exp(−λ)   (Poisson zero-class)
#
#  td_share is the player's recency-weighted share of his team's
#  offensive TDs, shrunk toward a usage-based prior (touch share)
#  — a player with 3 TDs in 3 games but a 10% touch share is
#  mostly variance, and the prior pulls him back. Defense enters
#  only through the f_otd efficiency residual at half strength:
#  Vegas already prices most of the defense into implied_total,
#  but positional TD-allowed profiles retain some signal.
# ============================================================
ANY_TD_DEF_DAMPING <- 0.5

# Positional touch-share → TD-share priors, fit on 2021-2024
# player-seasons (calibrate.R §6):
#   td_share ≈ a_pos + b_pos × touch_share
#     QB: −0.0058 + 1.294·ts  (n=136, r=0.66 — QB carries are
#                              TD-rich: sneaks, goal-line keepers)
#     RB: −0.0218 + 0.900·ts  (n=340, r=0.82)
#     WR: −0.0099 + 1.287·ts  (n=534, r=0.68 — WR touches carry
#                              more end-zone equity per touch)
#     TE: +0.0213 + 0.911·ts  (n=261, r=0.47)
# touch_share = (targets + carries) / (team targets + carries).
TD_PRIOR_COEF <- list(
  QB = c(-0.0058, 1.2940), RB = c(-0.0218, 0.9002),
  WR = c(-0.0099, 1.2871), TE = c( 0.0213, 0.9109)
)

# Residual compression: how much of the player's own observed
# TD-share deviation from the usage prior to keep. Tuned on the
# 2024 season walk-forward, validated on 2025.
ANY_TD_RESID_COMPRESS <- 0.75

project_anytime_td <- function(player, def, lg, implied_total) {
  if (is.na(player$td_share_raw) && is.na(player$touch_share)) return(NULL)

  cf <- TD_PRIOR_COEF[[player$position]]
  if (is.null(cf)) cf <- c(0, 1)
  ts <- ifelse(is.na(player$touch_share), 0.05, player$touch_share)
  prior_share <- pmin(0.6, pmax(0.01, cf[1] + cf[2] * ts))
  n_w <- player$w_games
  share <- (ifelse(is.na(player$td_share_raw), prior_share, player$td_share_raw) *
              n_w + prior_share * TD_SHARE_SHRINK_GAMES) /
           (n_w + TD_SHARE_SHRINK_GAMES)
  share <- prior_share + ANY_TD_RESID_COMPRESS * (share - prior_share)

  f_def <- 1 + ANY_TD_DEF_DAMPING * (def$f_otd - 1)
  lam   <- expected_team_tds(implied_total, lg$off_td_pg) * share * f_def
  list(lam = lam, p_anytime = 1 - exp(-lam), td_share = share)
}

# ============================================================
#  Distribution helpers
# ============================================================

# P(X > line) for a discrete probs vector indexed 0..N.
# Prop lines are X.5, so no push handling needed; for integer
# lines the push mass is reported separately.
prob_over_discrete <- function(probs, line) {
  n <- length(probs) - 1
  k <- floor(line) + 1
  if (k > n) return(0)
  sum(probs[(k + 1):(n + 1)])
}
