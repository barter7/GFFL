# ============================================================
#  calibrate.R  (nflmodel)
#
#  One-off analysis script — fits the model constants that are
#  hard-coded (with provenance comments) in model_core.R.
#  Analog of PropSZN's analyze_tbf.R / opp_k_adjustment
#  calibration workflow.
#
#  Run:  Rscript calibrate.R      (from the nflmodel/ directory)
#
#  Fits, using 2021-2024 regular seasons (2025 held out for the
#  walk-forward backtest in backtest.R):
#    1. spread_line sign check (home margin vs spread)
#    2. game-script pass-volume model:
#         log(team_targets) ~ team_spread + (total_line - lg_total)
#    3. expected offensive TDs ~ Vegas implied team total
#    4. defense-factor shrinkage k (games) via one-week-ahead MSE
#    5. league baseline rates
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr) })
source("utils.R")
source("data_sources.R")

CAL_SEASONS <- 2021:2024

logs  <- load_gamelogs()  %>% filter(season %in% CAL_SEASONS)
sched <- load_schedules_cache() %>% filter(season %in% CAL_SEASONS)
tv    <- schedule_team_view(sched)

# ── Team-week aggregates ─────────────────────────────────────
# NOTE: team offensive TDs = pass TDs + rush TDs. Receiving TDs
# are the same scores as passing TDs — never add both.
team_week <- logs %>%
  group_by(season, week, team) %>%
  summarise(
    team_targets    = sum(targets, na.rm = TRUE),
    team_receptions = sum(receptions, na.rm = TRUE),
    team_rec_yards  = sum(rec_yards, na.rm = TRUE),
    team_pass_td    = sum(pass_td, na.rm = TRUE),
    team_rush_td    = sum(rush_td, na.rm = TRUE),
    team_off_td     = sum(pass_td, na.rm = TRUE) + sum(rush_td, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(tv, by = c("season", "week", "team")) %>%
  filter(team_targets >= 10)          # drop bad/missing weeks

# ── 1. Spread sign sanity check ──────────────────────────────
sign_chk <- team_week %>%
  filter(home == 1, !is.na(team_spread), !is.na(team_score)) %>%
  summarise(cor_margin_spread = cor(team_score - opp_score, team_spread))
cat(sprintf("\n[1] cor(home margin, spread_line) = %.3f  (expect strongly positive)\n",
            sign_chk$cor_margin_spread))

# ── 2. Game-script pass volume ───────────────────────────────
lg_total   <- mean(team_week$total_line, na.rm = TRUE)
lg_targets <- mean(team_week$team_targets, na.rm = TRUE)

gs <- team_week %>%
  filter(!is.na(team_spread), !is.na(total_line)) %>%
  mutate(ltgt = log(team_targets), ctotal = total_line - lg_total)
fit_gs <- lm(ltgt ~ team_spread + ctotal, data = gs)
cat("\n[2] Game-script pass volume: log(team_targets) ~ spread + centered total\n")
print(summary(fit_gs)$coefficients)
cat(sprintf("    lg_total = %.2f   lg_targets/game = %.2f\n", lg_total, lg_targets))
cat(sprintf("    GS_VOL_SPREAD_SLOPE = %.5f   GS_VOL_TOTAL_SLOPE = %.5f\n",
            coef(fit_gs)["team_spread"], coef(fit_gs)["ctotal"]))

# ── 3. Expected offensive TDs vs implied total ───────────────
td_fit <- lm(team_off_td ~ implied_total,
             data = team_week %>% filter(!is.na(implied_total)))
cat("\n[3] Expected offensive TDs ~ implied team total\n")
print(summary(td_fit)$coefficients)
cat(sprintf("    EXP_TD_INTERCEPT = %.4f   EXP_TD_SLOPE = %.4f\n",
            coef(td_fit)[1], coef(td_fit)[2]))
cat(sprintf("    (league avg off TD/game = %.3f at avg implied total %.2f)\n",
            mean(team_week$team_off_td), mean(team_week$implied_total, na.rm = TRUE)))

# Pass share of offensive TDs (league)
cat(sprintf("    league pass share of off TDs = %.3f\n",
            sum(team_week$team_pass_td) / sum(team_week$team_off_td)))

# ── 4. Defense-factor shrinkage k ────────────────────────────
#  For each defense-week, build a trailing catch-rate-allowed
#  factor from weeks < w (same season), shrink with k, predict the
#  week-w catch rate allowed; grid-search k minimizing MSE.
def_week <- team_week %>%
  transmute(season, week, defense = opponent,
            tgt = team_targets, rec = team_receptions)

lg_cr <- sum(def_week$rec) / sum(def_week$tgt)

grid_mse <- sapply(c(1, 2, 4, 8, 16, 32, 64), function(k) {
  errs <- def_week %>%
    group_by(season, defense) %>%
    arrange(week, .by_group = TRUE) %>%
    mutate(
      prior_rec = cumsum(rec) - rec,
      prior_tgt = cumsum(tgt) - tgt,
      n_prior   = row_number() - 1,
      raw_f     = ifelse(prior_tgt > 0, (prior_rec / prior_tgt) / lg_cr, NA_real_),
      shrunk_f  = shrink_factor(raw_f, n_prior, k),
      pred_cr   = lg_cr * shrunk_f,
      err       = (rec / tgt - pred_cr)^2
    ) %>%
    ungroup() %>%
    filter(n_prior >= 3)
  mean(errs$err, na.rm = TRUE)
})
names(grid_mse) <- c(1, 2, 4, 8, 16, 32, 64)
cat("\n[4] Defense shrinkage k grid (one-week-ahead MSE of catch rate allowed):\n")
print(round(grid_mse * 1e4, 4))
cat(sprintf("    best k = %s\n", names(which.min(grid_mse))))

# Same grid for the targets-funnel factor (targets allowed vs league)
grid_mse_t <- sapply(c(1, 2, 4, 8, 16, 32, 64), function(k) {
  errs <- def_week %>%
    group_by(season, defense) %>%
    arrange(week, .by_group = TRUE) %>%
    mutate(
      prior_tgt = cumsum(tgt) - tgt,
      n_prior   = row_number() - 1,
      raw_f     = ifelse(n_prior > 0, (prior_tgt / n_prior) / lg_targets, NA_real_),
      shrunk_f  = shrink_factor(raw_f, n_prior, k),
      err       = (tgt - lg_targets * shrunk_f)^2
    ) %>%
    ungroup() %>%
    filter(n_prior >= 3)
  mean(errs$err, na.rm = TRUE)
})
names(grid_mse_t) <- c(1, 2, 4, 8, 16, 32, 64)
cat("\n    Targets-funnel factor k grid (MSE of team targets):\n")
print(round(grid_mse_t, 2))
cat(sprintf("    best k = %s\n", names(which.min(grid_mse_t))))

# ── 5. League baselines ──────────────────────────────────────
qb_week <- logs %>% filter(position == "QB") %>%
  group_by(season, week, team) %>%
  summarise(pass_yards = sum(pass_yards), pass_td = sum(pass_td), .groups = "drop") %>%
  inner_join(team_week %>% select(season, week, team, team_targets),
             by = c("season", "week", "team"))

cat("\n[5] League baselines (2021-2024):\n")
cat(sprintf("    catch rate (rec/target)      = %.4f\n", lg_cr))
cat(sprintf("    team targets / game          = %.3f\n", lg_targets))
cat(sprintf("    yards per target (team pass) = %.4f\n",
            sum(qb_week$pass_yards) / sum(qb_week$team_targets)))
cat(sprintf("    pass TD per target           = %.5f\n",
            sum(qb_week$pass_td) / sum(qb_week$team_targets)))
cat(sprintf("    team off TD / game           = %.4f\n", mean(team_week$team_off_td)))

# ── 6. Positional touch-share → TD-share priors ──────────────
#  Feeds TD_PRIOR_COEF in model_core.R (anytime-TD usage prior).
team_week_tc <- logs %>%
  group_by(season, week, team) %>%
  summarise(team_targets = sum(targets), team_carries = sum(carries),
            team_off_td = sum(pass_td) + sum(rush_td), .groups = "drop") %>%
  filter(team_targets >= 10)

ps <- logs %>%
  inner_join(team_week_tc, by = c("season", "week", "team")) %>%
  filter((targets + carries > 0) | (position == "QB" & pass_yards > 0)) %>%
  group_by(season, player_id, position) %>%
  summarise(
    n = n(),
    touch_share = sum(targets + carries) / sum(team_targets + team_carries),
    td_share    = sum(rush_td + rec_td) / pmax(sum(team_off_td), 1),
    .groups = "drop"
  ) %>%
  filter(n >= 8)

cat("\n[6] Positional touch-share -> TD-share priors (player-seasons, n>=8 games):\n")
for (p in c("QB", "RB", "WR", "TE")) {
  f <- lm(td_share ~ touch_share, data = ps %>% filter(position == p))
  cat(sprintf("    %s: td_share = %.4f + %.4f * touch_share  (n=%d, r=%.2f)\n",
              p, coef(f)[1], coef(f)[2], sum(ps$position == p),
              cor(ps$touch_share[ps$position == p],
                  ps$td_share[ps$position == p])))
}
