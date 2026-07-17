# ============================================================
#  pass_split_projection.R  (nflmodel)
#
#  PROJECTION STEP 2: split projected plays into expected
#  CALLED passes (dropbacks) vs CALLED runs (rule R4.9 /
#  Study S16). Outcome = a team-game's called-pass share under
#  the R4.7 definition (dropback = throw + sack + scramble).
#
#  Structure mirrors Step 1 — an evaluation ladder, walk-forward
#  features, train 2022-24 / test 2025:
#    P0  league base rate (previous season's)
#    P1  league SCRIPT-EXPECTED rate: the smoothed spread→
#        quartile-share curve (R4.6) × previous season's league
#        pass rate per state — what an average team would call
#        given this spread
#    P2  + team base-rate deviation (season-to-date, EB k=6
#        toward previous season, then league)
#    P3  + team script-sensitivity: previous-season dev_q1/dev_q4
#        interacted with expected trailing/leading shares
#        (S12 predicts ~no holdout gain — deltas are noise)
#    P4  + opponent funnel: pass rate FACED deviation
#
#  Output of the step: exp_dropbacks = plays_proj × rate_proj,
#  exp_designed_runs = plays_proj × (1 − rate_proj).
#
#  Coefficients cached to data/context/pass_split_model.csv;
#  slate scoring via project_pass_split().
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr) })

PASS_SPLIT_MODEL_FILE <- file.path("data", "context", "pass_split_model.csv")

build_pass_split_dataset <- function(seasons) {
  snaps <- build_playcall_snaps(seasons)

  tg <- snaps %>%
    group_by(season, week, game_id, team = posteam) %>%
    summarise(calls = n(), pass_calls = sum(called == "pass"),
              rate = pass_calls / calls, .groups = "drop")

  ctx <- load_game_context() %>% filter(season %in% seasons)
  tv  <- schedule_team_view(ctx)
  tg  <- tg %>%
    left_join(tv %>% select(game_id, team, opponent, team_spread, total_line),
              by = c("game_id", "team")) %>%
    filter(!is.na(team_spread))

  # league: previous season's base rate and per-state rates
  lg_states <- read_csv(PLAYCALL_LEAGUE_FILE, show_col_types = FALSE) %>%
    filter(season > 0) %>%
    select(season, state, pass_rate) %>%
    pivot_wider(names_from = state, values_from = pass_rate,
                names_prefix = "lgr_") %>%
    mutate(season = season + 1)
  tg <- tg %>% left_join(lg_states, by = "season")

  # expected quartile shares from the smoothed spread curve
  curve <- read_csv(GAMESCRIPT_SMOOTH_FILE, show_col_types = FALSE)
  sh <- gamescript_shares_at(tg$team_spread, curve)
  tg <- bind_cols(tg, sh %>% select(-team_spread)) %>%
    mutate(
      lg_script_rate = q1_trail_big * lgr_q1_trail_big +
                       q2_trail     * lgr_q2_trail +
                       q3_lead      * lgr_q3_lead +
                       q4_lead_big  * lgr_q4_lead_big
    )

  # walk-forward team base rate: season-to-date + prev-season prior
  s2d_rate <- function(p, c) { n <- seq_along(p) - 1
    ifelse(n > 0, (cumsum(p) - p) / pmax(cumsum(c) - c, 1), NA) }
  prev_team <- tg %>% group_by(season, team) %>%
    summarise(prev_rate = sum(pass_calls) / sum(calls), .groups = "drop") %>%
    mutate(season = season + 1)
  prev_dev <- read_csv(PLAYCALL_TEAM_FILE, show_col_types = FALSE) %>%
    select(season, team, dev_q1, dev_q4) %>%
    mutate(season = season + 1) %>%
    rename(prev_dev_q1 = dev_q1, prev_dev_q4 = dev_q4)

  # opponent: pass rate faced (their defense's funnel), walk-forward
  faced <- tg %>%
    group_by(season, week, game_id, team = opponent) %>%
    summarise(f_pass = sum(pass_calls), f_calls = sum(calls), .groups = "drop") %>%
    arrange(season, week) %>% group_by(season, team) %>%
    mutate(n_prior_d = row_number() - 1,
           faced_s2d = s2d_rate(f_pass, f_calls)) %>%
    ungroup()
  prev_faced <- faced %>% group_by(season, team) %>%
    summarise(prev_faced = sum(f_pass) / sum(f_calls), .groups = "drop") %>%
    mutate(season = season + 1)

  tg %>%
    arrange(season, week) %>%
    group_by(season, team) %>%
    mutate(n_prior = row_number() - 1,
           rate_s2d = s2d_rate(pass_calls, calls)) %>%
    ungroup() %>%
    left_join(prev_team, by = c("season", "team")) %>%
    left_join(prev_dev,  by = c("season", "team")) %>%
    left_join(faced %>% select(season, week, game_id, team,
                               n_prior_d, faced_s2d),
              by = c("season", "week", "game_id", "opponent" = "team")) %>%
    left_join(prev_faced, by = c("season", "opponent" = "team")) %>%
    mutate(
      lg0 = coalesce(lgr_overall, mean(rate)),
      team_rate_hat = (coalesce(rate_s2d, 0) * n_prior +
                       coalesce(prev_rate, lg0) * EB_K_TEAM) /
                      (n_prior + EB_K_TEAM),
      opp_faced_hat = (coalesce(faced_s2d, 0) * coalesce(n_prior_d, 0) +
                       coalesce(prev_faced, lg0) * EB_K_TEAM) /
                      (coalesce(n_prior_d, 0) + EB_K_TEAM),
      team_dev  = team_rate_hat - lg0,
      opp_dev   = opp_faced_hat - lg0,
      # script-sensitivity interaction: prev-season deltas × how
      # much MORE trailing/leading script this spread implies
      sens_q1 = coalesce(prev_dev_q1, 0) * (q1_trail_big - 0.23),
      sens_q4 = coalesce(prev_dev_q4, 0) * (q4_lead_big - 0.23)
    )
}

fit_pass_split_ladder <- function(d, test_season = 2025) {
  tr <- d %>% filter(season < test_season, season > min(season))
  te <- d %>% filter(season == test_season)
  mae <- function(p, a) mean(abs(p - a))

  forms <- list(
    P0_league  = NULL,
    P1_script  = rate ~ offset(lg_script_rate) - 1,
    P2_team    = rate ~ offset(lg_script_rate) + team_dev - 1,
    P3_sens    = rate ~ offset(lg_script_rate) + team_dev +
                        sens_q1 + sens_q4 - 1,
    P4_oppfunnel = rate ~ offset(lg_script_rate) + team_dev +
                          sens_q1 + sens_q4 + opp_dev - 1
  )
  rows <- list(); final <- NULL
  for (nm in names(forms)) {
    if (nm == "P0_league") { pr_tr <- tr$lg0; pr_te <- te$lg0; fit <- NULL }
    else if (nm == "P1_script") { pr_tr <- tr$lg_script_rate
      pr_te <- te$lg_script_rate; fit <- NULL }
    else {
      fit <- lm(forms[[nm]], data = tr, weights = calls)
      pr_tr <- predict(fit, tr); pr_te <- predict(fit, te)
      final <- fit
    }
    rows[[nm]] <- tibble(model = nm,
                         mae_train = round(mae(pr_tr, tr$rate), 4),
                         mae_test  = round(mae(pr_te, te$rate), 4))
  }
  list(table = bind_rows(rows), final = final)
}

refresh_pass_split_model <- function(seasons = 2021:2025, test_season = 2025) {
  d <- build_pass_split_dataset(seasons)
  lad <- fit_pass_split_ladder(d, test_season)
  co <- coef(lad$final)

  # universe bridge (train seasons): nullified snaps are ~77% pass,
  # so the CALL-based rate overstates the OFFICIAL pass share.
  # official_pass_share = call_pass_rate x bridge.
  tr <- build_playcall_snaps(setdiff(seasons, test_season) %>%
                               setdiff(min(seasons)))
  call_rate <- mean(tr$called == "pass")
  off_rate  <- mean(tr$called[tr$official] == "pass")
  bridge <- off_rate / call_rate

  write_csv(bind_rows(tibble(term = names(co), coef = round(co, 4)),
                      tibble(term = "official_bridge",
                             coef = round(bridge, 4))),
            PASS_SPLIT_MODEL_FILE)
  list(data = d, ladder = lad$table, model = lad$final, bridge = bridge)
}

# ── slate-time scoring ───────────────────────────────────────
#  official = TRUE applies the call->official universe bridge so
#  the rate multiplies OFFICIAL projected plays (what props settle
#  on); official = FALSE returns the raw called-pass rate.
project_pass_split <- function(lg_script_rate, team_dev, sens_q1, sens_q4,
                               opp_dev, official = TRUE,
                               coefs = read_csv(PASS_SPLIT_MODEL_FILE,
                                                show_col_types = FALSE)) {
  cf <- setNames(coefs$coef, coefs$term)
  r <- lg_script_rate + cf["team_dev"]*team_dev + cf["sens_q1"]*sens_q1 +
    cf["sens_q4"]*sens_q4 + cf["opp_dev"]*opp_dev
  if (official && "official_bridge" %in% names(cf)) r <- r * cf["official_bridge"]
  r
}
