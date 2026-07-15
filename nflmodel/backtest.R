# ============================================================
#  backtest.R  (nflmodel)
#
#  Walk-forward backtest — the validation harness for
#  model_core.R (analog of PropSZN's matched_results analysis).
#
#  For each week w of the test season, the model is fit on ALL
#  data strictly before that week (previous seasons + weeks
#  1..w-1), then projects every player who played in week w.
#  No lookahead: constants in model_core.R were calibrated on
#  2021-2024 only, and the test season defaults to 2025.
#
#  Because historical prop LINES aren't archived in this repo,
#  the backtest evaluates the model's distributions directly:
#    • point accuracy (MAE) vs a recency-weighted naive baseline
#    • probability calibration: predicted P(over synthetic line)
#      vs empirical hit rate, bucketed
#    • PIT (probability integral transform) uniformity for
#      pass yards
#    • Brier score + calibration for anytime TD
#
#  Run:  Rscript backtest.R            (defaults: 2025, weeks 5-17)
#        Rscript backtest.R 2024 6 15
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr); library(purrr) })
source("utils.R")
source("data_sources.R")
source("model_core.R")

args        <- commandArgs(trailingOnly = TRUE)
TEST_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2025L
WEEK_FROM   <- if (length(args) >= 2) as.integer(args[2]) else 5L
WEEK_TO     <- if (length(args) >= 3) as.integer(args[3]) else 17L

logs_all  <- load_gamelogs()
sched_all <- load_schedules_cache()
tv_all    <- schedule_team_view(sched_all)

message(sprintf("Backtest: season %d, weeks %d-%d", TEST_SEASON, WEEK_FROM, WEEK_TO))

rows_rec <- list(); rows_qby <- list(); rows_qtd <- list(); rows_any <- list()

for (wk in WEEK_FROM:WEEK_TO) {
  window <- logs_all %>%
    filter(season < TEST_SEASON | (season == TEST_SEASON & week < wk))
  tw  <- build_team_weeks(window)
  lg  <- build_league_baselines(tw)
  dfx <- build_defense_factors(tw, lg)
  pr  <- build_player_rates(window, tw)
  tds <- build_qb_td_shape(window)

  slate <- tv_all %>% filter(season == TEST_SEASON, week == wk)
  actual <- logs_all %>%
    filter(season == TEST_SEASON, week == wk) %>%
    inner_join(slate %>% select(team, team_spread, total_line, implied_total),
               by = "team")

  vol_cache <- new.env()
  get_vol <- function(tm) {
    if (is.null(vol_cache[[tm]])) vol_cache[[tm]] <- build_team_volume_dist(tw, tm)
    vol_cache[[tm]]
  }

  for (r in seq_len(nrow(actual))) {
    a  <- actual[r, ]
    p  <- pr %>% filter(player_id == a$player_id)
    if (nrow(p) == 0 || p$n_games < 3 || p$team != a$team) next
    vd <- get_vol(a$team)
    dd <- dfx %>% filter(defense == a$opponent)
    if (nrow(dd) == 0) dd <- default_def_factors()
    gs <- game_script_mult(a$team_spread, a$total_line)

    played_off <- (a$targets + a$carries > 0) ||
                  (a$position == "QB" && a$pass_yards > 0)
    if (!played_off) next

    # Receptions — route-runner positions. Zero-target games are
    # kept: a played prop with 0 targets settles as an under, and
    # dropping them would bias the empirical over rate upward.
    if (a$position %in% c("RB", "WR", "TE")) {
      pj <- project_receptions(p, vd, dd, gs, lg)
      if (!is.null(pj)) {
        line <- max(0.5, floor(pj$exp_rec) + 0.5)      # synthetic nearest line
        rows_rec[[length(rows_rec) + 1]] <- tibble(
          week = wk, player = a$player_name, pos = a$position,
          exp_rec = pj$exp_rec, actual = a$receptions,
          naive = p$tgt_pg * lg$catch_rate,
          line, p_over = prob_over_discrete(pj$probs, line),
          over = as.integer(a$receptions > line)
        )
      }
    }

    # QB markets
    if (a$position == "QB" && a$pass_yards > 0) {
      py <- project_pass_yards(p, vd, dd, gs, lg)
      if (!is.null(py)) {
        rows_qby[[length(rows_qby) + 1]] <- tibble(
          week = wk, player = a$player_name,
          exp_yds = py$exp_yds, actual = a$pass_yards,
          naive = p$qb_ypt_mean * lg$team_targets,
          pit = 1 - py$p_over(a$pass_yards),
          p_over_line = py$p_over(round(py$exp_yds / 5) * 5 + 0.5),
          over_line = as.integer(a$pass_yards > round(py$exp_yds / 5) * 5 + 0.5)
        )
      }
      pt <- project_pass_tds(p, vd, dd, gs, lg, a$implied_total,
                             pass_share = team_pass_td_share(tw, a$team),
                             td_shape = tds)
      if (!is.null(pt)) {
        rows_qtd[[length(rows_qtd) + 1]] <- tibble(
          week = wk, player = a$player_name,
          exp_td = pt$exp_td, actual = a$pass_td,
          p_over_1.5 = prob_over_discrete(pt$probs, 1.5),
          over_1.5 = as.integer(a$pass_td > 1.5)
        )
      }
    }

    # Anytime TD — all offensive players who took the field
    pa <- project_anytime_td(p, dd, lg, a$implied_total)
    if (!is.null(pa)) {
      rows_any[[length(rows_any) + 1]] <- tibble(
        week = wk, player = a$player_name, pos = a$position,
        p_any = pa$p_anytime,
        naive = 1 - exp(-pmax(0.01, p$td_share_raw * lg$off_td_pg)),
        scored = as.integer(a$rush_td + a$rec_td > 0)
      )
    }
  }
  message(sprintf("  week %2d done (rec %d, qb %d)", wk,
                  length(rows_rec), length(rows_qby)))
}

rec <- bind_rows(rows_rec); qby <- bind_rows(rows_qby)
qtd <- bind_rows(rows_qtd); any <- bind_rows(rows_any)

dir.create(file.path("data", "results"), recursive = TRUE, showWarnings = FALSE)
write_csv(rec, file.path("data", "results", "backtest_receptions.csv"))
write_csv(qby, file.path("data", "results", "backtest_pass_yards.csv"))
write_csv(qtd, file.path("data", "results", "backtest_pass_tds.csv"))
write_csv(any, file.path("data", "results", "backtest_anytime_td.csv"))

cat("\n================ BACKTEST SUMMARY ================\n")

cat(sprintf("\n[Receptions]  n = %d\n", nrow(rec)))
cat(sprintf("  MAE  model %.3f | naive %.3f\n",
            mean(abs(rec$exp_rec - rec$actual)),
            mean(abs(rec$naive - rec$actual))))
cat("  Calibration of P(over synthetic line):\n")
rec %>%
  mutate(bucket = cut(p_over, seq(0, 1, 0.1))) %>%
  group_by(bucket) %>%
  summarise(n = n(), pred = mean(p_over), emp = mean(over), .groups = "drop") %>%
  filter(n >= 20) %>%
  as.data.frame() %>% print(digits = 3)

cat(sprintf("\n[QB pass yards]  n = %d\n", nrow(qby)))
cat(sprintf("  MAE  model %.1f | naive %.1f\n",
            mean(abs(qby$exp_yds - qby$actual)),
            mean(abs(qby$naive - qby$actual))))
cat("  PIT deciles (should each be ~0.10):\n")
print(round(table(cut(qby$pit, seq(0, 1, 0.1))) / nrow(qby), 3))

cat(sprintf("\n[QB pass TDs]  n = %d\n", nrow(qtd)))
cat(sprintf("  MAE %.3f | mean exp %.3f vs mean actual %.3f\n",
            mean(abs(qtd$exp_td - qtd$actual)),
            mean(qtd$exp_td), mean(qtd$actual)))
cat(sprintf("  P(2+ TD): predicted %.3f | empirical %.3f\n",
            mean(qtd$p_over_1.5), mean(qtd$over_1.5)))

cat(sprintf("\n[Anytime TD]  n = %d\n", nrow(any)))
cat(sprintf("  Brier  model %.4f | naive %.4f | base rate %.3f\n",
            mean((any$p_any - any$scored)^2),
            mean((any$naive - any$scored)^2),
            mean(any$scored)))
cat("  Calibration:\n")
any %>%
  mutate(bucket = cut(p_any, c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 1))) %>%
  group_by(bucket) %>%
  summarise(n = n(), pred = mean(p_any), emp = mean(scored), .groups = "drop") %>%
  as.data.frame() %>% print(digits = 3)
