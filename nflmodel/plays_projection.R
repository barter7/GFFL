# ============================================================
#  plays_projection.R  (nflmodel)
#
#  PROJECTION STEP 1: expected offensive plays for a team-game
#  (rule R4.8 / Study S15).
#
#  Built as a LADDER — each ingredient must earn its keep on a
#  held-out season (train 2021-2024, test 2025), with every
#  predictor computed walk-forward (pre-kickoff information
#  only):
#    M0  league average plays/game (previous season's — the
#        league is slowing ~0.5 plays/yr, so "last 5y average"
#        enters through the trend-aware prior)
#    M1  + team's own plays/game (season-to-date, EB-shrunk
#        toward its previous-season value then the league)
#    M2  + opponent plays ALLOWED per game (same construction)
#    M3  + pace: team & opponent sec/play deviations
#    M4  + game script: signed spread, |spread|, total line
#        (the smoothed spread→script curve is a deterministic
#        function of spread, so spread terms carry it)
#
#  Population: official offensive snaps (R1.10), qtr 1-4, REG+POST.
#
#  Outputs:
#    data/context/plays_model.csv       fitted coefficients (M4)
#    project_team_plays(...)            slate-time scoring fn
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr) })

PLAYS_MODEL_FILE <- file.path("data", "context", "plays_model.csv")
EB_K_TEAM <- 6   # games of prior weight for season-to-date shrink
                 # (small grid {4,6,8,10} — flat, 6 marginally best)

# ── team-game observations with pre-game features ────────────
build_plays_dataset <- function(seasons) {
  pbp <- load_pbp_cache(seasons, cols = c(
    "game_id","season","week","posteam","defteam","qtr","play_type",
    "two_point_attempt","game_seconds_remaining","drive")) %>%
    filter(!is.na(posteam), play_type %in% c("pass","run"),
           two_point_attempt == 0, qtr %in% 1:4)

  tg <- pbp %>%
    arrange(game_id, desc(game_seconds_remaining)) %>%
    group_by(game_id, drive) %>%
    mutate(el = game_seconds_remaining - lead(game_seconds_remaining),
           el = ifelse(!is.na(el) & el > 0 & el <= 60, el, NA)) %>%
    group_by(season, week, game_id, team = posteam, opp = defteam) %>%
    summarise(plays = n(), sec_play = mean(el, na.rm = TRUE),
              .groups = "drop")

  ctx <- load_game_context() %>% filter(season %in% seasons)
  tv  <- schedule_team_view(ctx)
  tg <- tg %>%
    left_join(tv %>% select(game_id, team, team_spread, total_line),
              by = c("game_id", "team"))

  # league plays/g by season (for the previous-season prior)
  lg <- tg %>% group_by(season) %>%
    summarise(lg_plays = mean(plays), lg_pace = mean(sec_play, na.rm = TRUE),
              .groups = "drop")

  # walk-forward season-to-date means with EB shrink:
  # s2d -> shrink toward previous-season team avg -> toward league
  s2d <- function(x) { n <- seq_along(x) - 1
    ifelse(n > 0, (cumsum(x) - x) / n, NA) }
  prior_of <- function(df, val) df %>%
    group_by(season, team) %>% summarise(v = mean({{val}}), .groups="drop") %>%
    mutate(season = season + 1) %>% rename(prior_v = v)

  tg <- tg %>%
    arrange(season, week) %>%
    group_by(season, team) %>%
    mutate(n_prior = row_number() - 1,
           plays_s2d = s2d(plays),
           pace_s2d  = s2d(sec_play)) %>%
    ungroup() %>%
    left_join(prior_of(tg, plays) %>% rename(plays_prev = prior_v),
              by = c("season","team")) %>%
    left_join(prior_of(tg, sec_play) %>% rename(pace_prev = prior_v),
              by = c("season","team")) %>%
    left_join(lg %>% mutate(season = season + 1) %>%
                rename(lg_plays_prev = lg_plays, lg_pace_prev = lg_pace),
              by = "season")

  # opponent plays-allowed / pace-allowed, same construction
  allowed <- tg %>%
    group_by(season, week, game_id, team = opp) %>%
    summarise(plays_faced = sum(plays), pace_faced = mean(sec_play, na.rm=TRUE),
              .groups = "drop") %>%
    arrange(season, week) %>%
    group_by(season, team) %>%
    mutate(n_prior_d = row_number() - 1,
           faced_s2d = s2d(plays_faced),
           pace_faced_s2d = s2d(pace_faced)) %>%
    ungroup() %>%
    left_join(tg %>% group_by(season, team = opp) %>%
                summarise(v = mean(plays), .groups="drop") %>%
                mutate(season = season + 1) %>% rename(faced_prev = v),
              by = c("season","team"))

  tg %>%
    left_join(allowed %>% select(season, week, game_id, team,
                                 n_prior_d, faced_s2d, pace_faced_s2d,
                                 faced_prev),
              by = c("season", "week", "game_id", "opp" = "team")) %>%
    mutate(
      lg0 = coalesce(lg_plays_prev, mean(plays)),
      # EB blend: s2d (n games) + prev-season prior (EB_K_TEAM) + league
      team_plays_hat = (coalesce(plays_s2d,0)*n_prior +
                        coalesce(plays_prev, lg0)*EB_K_TEAM) /
                       (n_prior + EB_K_TEAM),
      opp_faced_hat  = (coalesce(faced_s2d,0)*coalesce(n_prior_d,0) +
                        coalesce(faced_prev, lg0)*EB_K_TEAM) /
                       (coalesce(n_prior_d,0) + EB_K_TEAM),
      team_pace_hat  = (coalesce(pace_s2d,0)*n_prior +
                        coalesce(pace_prev, coalesce(lg_pace_prev, 30))*EB_K_TEAM) /
                       (n_prior + EB_K_TEAM),
      opp_pace_hat   = (coalesce(pace_faced_s2d,0)*coalesce(n_prior_d,0) +
                        coalesce(lg_pace_prev, 30)*EB_K_TEAM) /
                       (coalesce(n_prior_d,0) + EB_K_TEAM),
      team_dev = team_plays_hat - lg0,
      opp_dev  = opp_faced_hat - lg0,
      pace_dev = team_pace_hat - coalesce(lg_pace_prev, 30),
      opp_pace_dev = opp_pace_hat - coalesce(lg_pace_prev, 30),
      abs_spread = abs(team_spread),
      ctotal = total_line - 44.5
    ) %>%
    filter(!is.na(team_spread), !is.na(total_line))
}

# ── fit the ladder, evaluate on held-out season ──────────────
fit_plays_ladder <- function(d, test_season = 2025) {
  tr <- d %>% filter(season < test_season, season > min(season))
  te <- d %>% filter(season == test_season)
  mae <- function(p, a) mean(abs(p - a))

  forms <- list(
    M0_league     = plays ~ 1 + offset(lg0) - 1,
    M1_team       = plays ~ offset(lg0) + team_dev - 1,
    M2_opp        = plays ~ offset(lg0) + team_dev + opp_dev - 1,
    M3_pace       = plays ~ offset(lg0) + team_dev + opp_dev +
                            pace_dev + opp_pace_dev - 1,
    M4_gamescript = plays ~ offset(lg0) + team_dev + opp_dev +
                            pace_dev + opp_pace_dev +
                            team_spread + abs_spread + ctotal - 1
  )
  res <- lapply(names(forms), function(nm) {
    f <- forms[[nm]]
    fit <- if (nm == "M0_league") NULL else lm(f, data = tr)
    pr_tr <- if (is.null(fit)) tr$lg0 else predict(fit, tr)
    pr_te <- if (is.null(fit)) te$lg0 else predict(fit, te)
    list(name = nm, fit = fit,
         row = tibble(model = nm,
                      mae_train = round(mae(pr_tr, tr$plays), 3),
                      mae_test  = round(mae(pr_te, te$plays), 3),
                      sd_test_resid = round(sd(pr_te - te$plays), 3)))
  })
  list(table = bind_rows(lapply(res, `[[`, "row")),
       final = res[[length(res)]]$fit)
}

refresh_plays_model <- function(seasons = 2021:2025, test_season = 2025) {
  d <- build_plays_dataset(seasons)
  lad <- fit_plays_ladder(d, test_season)
  co <- coef(lad$final)
  write_csv(tibble(term = names(co), coef = round(co, 4)), PLAYS_MODEL_FILE)
  list(data = d, ladder = lad$table, model = lad$final)
}

# ── slate-time scoring ───────────────────────────────────────
#  All inputs are pre-game estimates in the same units as the
#  training features (league-relative deviations).
project_team_plays <- function(lg0, team_dev, opp_dev, pace_dev,
                               opp_pace_dev, team_spread, total_line,
                               coefs = read_csv(PLAYS_MODEL_FILE,
                                                show_col_types = FALSE)) {
  cf <- setNames(coefs$coef, coefs$term)
  lg0 + cf["team_dev"]*team_dev + cf["opp_dev"]*opp_dev +
    cf["pace_dev"]*pace_dev + cf["opp_pace_dev"]*opp_pace_dev +
    cf["team_spread"]*team_spread + cf["abs_spread"]*abs(team_spread) +
    cf["ctotal"]*(total_line - 44.5)
}
