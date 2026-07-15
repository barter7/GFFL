# ============================================================
#  data_sources.R  (nflmodel)
#
#  All data loading goes through a local CSV cache in
#  data/cache/ so the model itself can always run offline
#  (mirrors PropSZN's incremental caching approach).
#
#  Two cache files:
#    data/cache/player_gamelogs.csv  — one row per player-week
#        season, week, player_id, player_name, position, team,
#        opponent, home, pass_yards, pass_td, pass_int, carries,
#        rush_yards, rush_td, targets, receptions, rec_yards,
#        rec_td, rz_targets, rz_touches, fantasy_points
#    data/cache/schedules.csv        — one row per game (nflverse
#        games.csv subset): Vegas spread_line / total_line +
#        moneylines + final scores.
#
#  refresh_cache() rebuilds both from nflreadr (preferred, run
#  locally where CRAN/GitHub access exists). The cache can also
#  be produced by any other source that matches the schema —
#  the model layer never talks to the network.
#
#  nflverse conventions used throughout:
#    spread_line — points the HOME team is favored by (+3 = home -3)
#    total_line  — the game total (over/under)
# ============================================================

library(dplyr)
library(readr)

CACHE_DIR      <- file.path("data", "cache")
GAMELOGS_CACHE <- file.path(CACHE_DIR, "player_gamelogs.csv")
SCHEDULE_CACHE <- file.path(CACHE_DIR, "schedules.csv")

MODEL_POSITIONS <- c("QB", "RB", "WR", "TE")

# ── Refresh cache from nflreadr (network required) ───────────
refresh_cache <- function(seasons = 2021:as.integer(format(Sys.Date(), "%Y"))) {
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    stop("nflreadr not installed — install it (install.packages('nflreadr')) ",
         "or provide data/cache/*.csv from another source.")
  }
  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

  message("  Downloading nflreadr weekly player stats...")
  ps <- nflreadr::load_player_stats(seasons = seasons) %>%
    filter(position %in% MODEL_POSITIONS, season_type == "REG") %>%
    transmute(
      season, week,
      player_id      = player_id,
      player_name    = player_display_name,
      position,
      team           = normalize_team(recent_team),
      opponent       = normalize_team(opponent_team),
      home           = NA_integer_,       # filled from schedule below
      pass_yards     = coalesce(passing_yards, 0),
      pass_td        = coalesce(passing_tds, 0),
      pass_int       = coalesce(interceptions, 0),
      carries        = coalesce(carries, 0),
      rush_yards     = coalesce(rushing_yards, 0),
      rush_td        = coalesce(rushing_tds, 0),
      targets        = coalesce(targets, 0),
      receptions     = coalesce(receptions, 0),
      rec_yards      = coalesce(receiving_yards, 0),
      rec_td         = coalesce(receiving_tds, 0),
      rz_targets     = NA_real_,          # not in weekly stats; optional
      rz_touches     = NA_real_,
      fantasy_points = coalesce(fantasy_points_ppr, 0)
    )

  message("  Downloading nflreadr schedules...")
  sched <- nflreadr::load_schedules(seasons = seasons) %>%
    transmute(
      game_id, season, game_type, week, gameday, weekday, gametime,
      away_team = normalize_team(away_team), away_score,
      home_team = normalize_team(home_team), home_score,
      result, total,
      away_moneyline, home_moneyline, spread_line, total_line,
      roof, surface, temp, wind
    )

  home_lookup <- bind_rows(
    sched %>% transmute(season, week, team = home_team, home = 1L),
    sched %>% transmute(season, week, team = away_team, home = 0L)
  )
  ps <- ps %>%
    select(-home) %>%
    left_join(home_lookup, by = c("season", "week", "team"))

  write_csv(ps,    GAMELOGS_CACHE)
  write_csv(sched, SCHEDULE_CACHE)
  message(sprintf("  Cache written: %d player-weeks, %d games.",
                  nrow(ps), nrow(sched)))
  invisible(TRUE)
}

# ── Load from cache ──────────────────────────────────────────
load_gamelogs <- function(path = GAMELOGS_CACHE) {
  if (!file.exists(path)) {
    stop("Gamelog cache missing (", path, ") — run refresh_cache() first.")
  }
  read_csv(path, show_col_types = FALSE) %>%
    mutate(team     = normalize_team(team),
           opponent = normalize_team(opponent),
           position = toupper(position)) %>%
    filter(position %in% MODEL_POSITIONS)
}

load_schedules_cache <- function(path = SCHEDULE_CACHE) {
  if (!file.exists(path)) {
    stop("Schedule cache missing (", path, ") — run refresh_cache() first.")
  }
  read_csv(path, show_col_types = FALSE) %>%
    mutate(home_team = normalize_team(home_team),
           away_team = normalize_team(away_team)) %>%
    filter(game_type == "REG")
}

# ── Long (one row per team-game) schedule with Vegas context ─
#
#  team_spread    — points THIS team is favored by (negative = dog)
#  implied_total  — this team's Vegas-implied points:
#                     total_line / 2 + team_spread / 2
schedule_team_view <- function(sched) {
  bind_rows(
    sched %>% transmute(
      game_id, season, week, gameday,
      team = home_team, opponent = away_team, home = 1L,
      team_score = home_score, opp_score = away_score,
      team_spread = spread_line, total_line
    ),
    sched %>% transmute(
      game_id, season, week, gameday,
      team = away_team, opponent = home_team, home = 0L,
      team_score = away_score, opp_score = home_score,
      team_spread = -spread_line, total_line
    )
  ) %>%
    mutate(implied_total = total_line / 2 + team_spread / 2)
}
