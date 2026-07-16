# ============================================================
#  nfl_features.R  (nflmodel)
#
#  Feature-engineering layer — a faithful port of the owner's
#  original NFL model prep script onto this repo's cached data
#  foundation (pbp_data.R, game_context.R, roster_context.R).
#
#  Differences from the original, on purpose:
#   • every "current season/week" is a parameter (asof_season,
#     asof_week) so the same code builds features for ANY
#     historical week with no lookahead — required for
#     walk-forward backtesting
#   • nflspreaddata.csv (historical game script by spread range)
#     is COMPUTED from cached pbp instead of read from disk —
#     see build_spread_gamescript_table()
#   • PFF exports (paid, local-only) are an optional drop-in:
#     put weekly CSVs in data/pff/ and build_model_table() picks
#     them up; everything else works without them
#   • schedule outcome flags use the posteam-relative spread
#     convention from game_context.R (team_spread > 0 = favored)
#     and handle pushes as NA (the original coded pushes as 0)
#
#  Sources of the new caches (data/players/):
#    roster_{season}.csv            nflverse rosters release
#    snap_counts_{season}.csv       nflverse snap_counts release
#    stats_player_week_{season}.csv nflverse stats_player release
#    ngs_{passing|receiving|rushing}.csv.gz  nextgen_stats release
#  refresh_players() updates them (nflreadr primary).
# ============================================================

library(dplyr)
library(tidyr)
library(readr)

PLAYERS_DIR <- file.path("data", "players")
PFF_DIR     <- file.path("data", "pff")

SKILL_POS <- c("QB", "RB", "WR", "TE", "FB")

# ── Refresh player-level caches ──────────────────────────────
refresh_players <- function(seasons = NULL, max_age_hrs = 20) {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  current_season <- if (as.integer(format(Sys.Date(), "%m")) >= 9)
    this_year else this_year - 1L
  if (is.null(seasons)) seasons <- (current_season - 1L):(current_season + 1L)
  dir.create(PLAYERS_DIR, recursive = TRUE, showWarnings = FALSE)

  per_season <- list(
    c("roster_%d.csv",            "rosters",      "load_rosters"),
    c("snap_counts_%d.csv",       "snap_counts",  "load_snap_counts"),
    c("stats_player_week_%d.csv", "stats_player", "load_player_stats")
  )
  for (spec in per_season) for (s in seasons) {
    path <- file.path(PLAYERS_DIR, sprintf(spec[1], s))
    if (file.exists(path) &&
        (s < current_season ||
           difftime(Sys.time(), file.mtime(path), units = "hours") < max_age_hrs)) next
    url <- sprintf("https://github.com/nflverse/nflverse-data/releases/download/%s/%s",
                   spec[2], sprintf(spec[1], s))
    ok <- FALSE
    if (requireNamespace("nflreadr", quietly = TRUE)) {
      ok <- tryCatch({
        write_csv(getExportedValue("nflreadr", spec[3])(seasons = s), path); TRUE
      }, error = function(e) FALSE)
    }
    if (!ok) ok <- tryCatch({
      download.file(url, path, mode = "wb", quiet = TRUE); TRUE
    }, error = function(e) FALSE)
    message(sprintf("  %s: %s", basename(path),
                    if (ok) "refreshed" else "not available"))
  }
  # NGS: one all-seasons file per stat type
  for (tp in c("passing", "receiving", "rushing")) {
    path <- file.path(PLAYERS_DIR, sprintf("ngs_%s.csv.gz", tp))
    if (file.exists(path) &&
        difftime(Sys.time(), file.mtime(path), units = "hours") < max_age_hrs) next
    url <- sprintf(paste0("https://github.com/nflverse/nflverse-data/releases/",
                          "download/nextgen_stats/ngs_%s.csv.gz"), tp)
    ok <- tryCatch({ download.file(url, path, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE)
    message(sprintf("  ngs_%s: %s", tp, if (ok) "refreshed" else "not available"))
  }
  invisible(TRUE)
}

.per_season_cache <- function(pattern, seasons) {
  paths <- file.path(PLAYERS_DIR, sprintf(pattern, seasons))
  have  <- file.exists(paths)
  if (!any(have)) stop("Missing cache ", pattern, " — run refresh_players().")
  dfs <- lapply(paths[have], read_csv, show_col_types = FALSE,
                guess_max = 10000)
  # harmonize columns whose guessed type differs across seasons
  # (e.g. an all-NA column typed logical in one file, character in
  # another) by coercing the disagreeing columns to character
  types <- lapply(dfs, function(d) vapply(d, function(x) class(x)[1], ""))
  all_cols <- unique(unlist(lapply(types, names)))
  for (col in all_cols) {
    tt <- unique(na.omit(vapply(types, function(t) t[col] %||% NA_character_, "")))
    if (length(tt) > 1) {
      dfs <- lapply(dfs, function(d) {
        if (col %in% names(d)) d[[col]] <- as.character(d[[col]])
        d
      })
    }
  }
  bind_rows(dfs)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── Rosters (ID crosswalk: gsis / pfr / pff / espn) ──────────
load_rosters_cache <- function(seasons, skill_only = TRUE) {
  r <- .per_season_cache("roster_%d.csv", seasons) %>%
    mutate(team = normalize_team(team)) %>%
    select(season, team, position, full_name, gsis_id, pfr_id, pff_id,
           espn_id, headshot_url, jersey_number) %>%
    distinct(season, gsis_id, .keep_all = TRUE)
  if (skill_only) r <- r %>% filter(position %in% SKILL_POS)
  r
}

# ── Snap counts, gsis-resolved via roster crosswalk ──────────
load_snap_counts_cache <- function(seasons) {
  rosters_all <- load_rosters_cache(seasons, skill_only = FALSE)
  .per_season_cache("snap_counts_%d.csv", seasons) %>%
    filter(position %in% SKILL_POS) %>%
    mutate(team = normalize_team(team)) %>%
    select(player, snap_position = position, season, week, team,
           pfr_player_id, offense_snaps, offense_pct) %>%
    left_join(rosters_all %>% select(season, pfr_id, gsis_id) %>%
                filter(!is.na(pfr_id)) %>% distinct(),
              by = c("season", "pfr_player_id" = "pfr_id")) %>%
    left_join(rosters_all %>%
                group_by(season, full_name) %>% filter(n() == 1) %>% ungroup() %>%
                select(season, full_name, gsis_id),
              by = c("season", "player" = "full_name"),
              suffix = c("", "_byname")) %>%
    mutate(gsis_id = coalesce(gsis_id, gsis_id_byname)) %>%
    select(-gsis_id_byname)
}

# ── Official weekly player stats (gamelogs) ──────────────────
load_player_stats_cache <- function(seasons) {
  .per_season_cache("stats_player_week_%d.csv", seasons) %>%
    mutate(team = normalize_team(team),
           opponent_team = normalize_team(opponent_team))
}

# ── Next Gen Stats ───────────────────────────────────────────
load_ngs_cache <- function(type = c("receiving", "rushing", "passing"),
                           seasons) {
  type <- match.arg(type)
  read_csv(file.path(PLAYERS_DIR, sprintf("ngs_%s.csv.gz", type)),
           show_col_types = FALSE) %>%
    filter(season %in% seasons, week > 0)     # week 0 = season aggregate
}

# ── Schedule, long format with betting outcomes ──────────────
#  One row per team-game (the original schedule_away/home rbind):
#  team_spread > 0 = favored (game_context convention).
#  cover/over are NA on pushes.
build_schedule_long <- function(ctx = load_game_context()) {
  tv <- bind_rows(
    ctx %>% mutate(team = home_team, team_score = home_score,
                   opponent = away_team, opponent_score = away_score,
                   home = "vs", team_spread = spread_line,
                   team_ml = home_moneyline, opp_ml = away_moneyline,
                   team_qb_id = home_qb_id, team_qb_name = home_qb_name,
                   opp_qb_id = away_qb_id, opp_qb_name = away_qb_name),
    ctx %>% mutate(team = away_team, team_score = away_score,
                   opponent = home_team, opponent_score = home_score,
                   home = "@", team_spread = -spread_line,
                   team_ml = away_moneyline, opp_ml = home_moneyline,
                   team_qb_id = away_qb_id, team_qb_name = away_qb_name,
                   opp_qb_id = home_qb_id, opp_qb_name = home_qb_name)
  ) %>%
    transmute(
      game_id, season, game_type, week, gameday,
      up_matchup = paste(home_team, "vs.", away_team),
      team, opponent, home,
      team_score, opponent_score,
      team_margin = team_score - opponent_score,
      total_score = total, total_line,
      team_ml, opp_ml, team_spread,
      implied_total = total_line / 2 + team_spread / 2,
      team_qb_id, team_qb_name, opp_qb_id, opp_qb_name,
      favorite = as.integer(team_spread > 0),
      win   = case_when(is.na(team_margin) ~ NA_integer_,
                        team_margin > 0 ~ 1L, team_margin < 0 ~ 0L,
                        TRUE ~ NA_integer_),
      cover = case_when(is.na(team_margin) | is.na(team_spread) ~ NA_integer_,
                        team_margin + team_spread > 0 ~ 1L,
                        team_margin + team_spread < 0 ~ 0L,
                        TRUE ~ NA_integer_),
      over  = case_when(is.na(total_score) | is.na(total_line) ~ NA_integer_,
                        total_score > total_line ~ 1L,
                        total_score < total_line ~ 0L,
                        TRUE ~ NA_integer_)
    )
}

# ── pbp slice for feature building ───────────────────────────
FEATURE_PBP_COLS <- c(
  "game_id", "season", "season_type", "week", "posteam", "defteam",
  "qtr", "down", "ydstogo", "yardline_100", "score_differential", "wp",
  "play_type", "pass", "rush", "pass_attempt", "rush_attempt",
  "qb_dropback", "qb_scramble", "qb_kneel", "qb_spike", "sack",
  "complete_pass", "incomplete_pass", "interception", "two_point_attempt",
  "yards_gained", "passing_yards", "receiving_yards", "rushing_yards",
  "air_yards", "yards_after_catch", "epa", "success", "cp", "cpoe",
  "passer_player_id", "passer_player_name",
  "receiver_player_id", "receiver_player_name",
  "rusher_player_id", "rusher_player_name",
  "spread_line", "total_line", "touchdown", "pass_touchdown",
  "rush_touchdown", "td_player_id"
)

load_feature_pbp <- function(seasons) {
  load_pbp_cache(seasons, cols = FEATURE_PBP_COLS) %>%
    filter(!is.na(posteam), play_type %in% c("pass", "run"),
           two_point_attempt == 0) %>%
    mutate(
      neutral  = as.integer(wp >= .25 & wp <= .75),
      positive = as.integer(wp > .75),
      negative = as.integer(wp < .25),
      unrealized_air_yards = ifelse(pass_attempt == 1 & complete_pass == 0,
                                    coalesce(air_yards, 0), 0),
      unrealized_targets   = as.integer(pass_attempt == 1 & complete_pass == 0)
    )
}

# ── Play-calling by win-prob state, weekly ───────────────────
build_playcall_weekly <- function(pbp) {
  pbp %>%
    filter(qtr %in% 1:4) %>%
    group_by(posteam, season, week) %>%
    summarise(
      total_pass_plays = sum(pass), total_rush_plays = sum(rush),
      total_sacks = sum(sack),
      total_passes_thrown = sum(complete_pass + incomplete_pass + interception),
      total_scramble = sum(qb_scramble),
      total_plays = total_pass_plays + total_rush_plays,
      pass_perc = total_pass_plays / total_plays,
      pass_thrown_perc = total_passes_thrown / total_plays,
      pass_off_perc = total_passes_thrown / total_pass_plays,
      sack_perc = total_sacks / total_pass_plays,
      scramble_perc = total_scramble / total_pass_plays,
      rush_perc = total_rush_plays / total_plays,
      neutral_plays = sum(neutral), positive_plays = sum(positive),
      negative_plays = sum(negative),
      neutral_pass_perc  = sum(neutral  * pass) / pmax(neutral_plays, 1),
      neutral_rush_perc  = sum(neutral  * rush) / pmax(neutral_plays, 1),
      positive_pass_perc = sum(positive * pass) / pmax(positive_plays, 1),
      positive_rush_perc = sum(positive * rush) / pmax(positive_plays, 1),
      negative_pass_perc = sum(negative * pass) / pmax(negative_plays, 1),
      negative_rush_perc = sum(negative * rush) / pmax(negative_plays, 1),
      .groups = "drop"
    )
}

# ── Season aggregates as of a week (no lookahead) ────────────
#  Per-game averages over `asof_season` weeks < asof_week.
build_playcall_season <- function(playcall_weekly, asof_season, asof_week) {
  playcall_weekly %>%
    filter(season == asof_season, week < asof_week) %>%
    group_by(posteam) %>%
    summarise(
      g = n(),
      across(c(total_pass_plays, total_rush_plays, total_sacks,
               total_passes_thrown, total_scramble, total_plays,
               neutral_plays, positive_plays, negative_plays), mean),
      across(c(pass_perc, pass_thrown_perc, pass_off_perc, sack_perc,
               scramble_perc, rush_perc,
               neutral_pass_perc, neutral_rush_perc,
               positive_pass_perc, positive_rush_perc,
               negative_pass_perc, negative_rush_perc),
             \(x) mean(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#  Defense mirror: plays faced per game.
build_plays_defense <- function(pbp, asof_season, asof_week) {
  pbp %>%
    filter(qtr %in% 1:4, season == asof_season, week < asof_week) %>%
    group_by(defteam, season, week) %>%
    summarise(pass_plays = sum(pass), rush_plays = sum(rush),
              plays = pass_plays + rush_plays, .groups = "drop") %>%
    group_by(defteam) %>%
    summarise(total_pass_plays = mean(pass_plays),
              total_rush_plays = mean(rush_plays),
              total_plays = mean(plays),
              pass_perc = total_pass_plays / total_plays,
              rush_perc = total_rush_plays / total_plays,
              .groups = "drop")
}

# ── Player gamelogs with derived efficiency ──────────────────
build_gamelogs <- function(seasons, rosters = load_rosters_cache(seasons),
                           sched_long = build_schedule_long()) {
  load_player_stats_cache(seasons) %>%
    mutate(
      adot = receiving_air_yards / targets,
      yards_per_target = receiving_yards / targets,
      yards_per_catch = receiving_yards / receptions,
      yards_per_carry = rushing_yards / carries,
      qb_completion_perc = completions / attempts,
      qb_yards_per_completion = passing_yards / completions,
      qb_yards_per_attempt = passing_yards / attempts,
      qb_ay_per_attempt = passing_air_yards / attempts,
      qb_yac_perc = passing_yards_after_catch / passing_yards,
      catch_perc = receptions / targets,
      receiver_yac_perc = receiving_yards_after_catch / receiving_yards,
      avg_yac = receiving_yards_after_catch / receptions,
      rush_plus_rec = receiving_yards + rushing_yards,
      total_td = receiving_tds + rushing_tds
    ) %>%
    left_join(rosters %>% select(season, gsis_id, pfr_id, pff_id,
                                 headshot_url, jersey_number),
              by = c("season", "player_id" = "gsis_id")) %>%
    left_join(sched_long %>%
                select(game_id, season, week, team, opponent, home,
                       team_spread, implied_total, total_line,
                       team_ml, opp_ml, favorite, team_qb_id, opp_qb_id),
              by = c("season", "week", "team"))
}

#  Season efficiency as of a week (original gamelogs_season).
build_gamelogs_season <- function(gamelogs, asof_season, asof_week) {
  gamelogs %>%
    filter(season == asof_season, week < asof_week) %>%
    group_by(player_id, player_display_name) %>%
    summarise(across(c(completions, attempts, passing_yards, passing_tds,
                       passing_air_yards, passing_yards_after_catch,
                       carries, rushing_yards, receptions, targets,
                       receiving_yards, receiving_air_yards,
                       receiving_yards_after_catch),
                     \(x) sum(x, na.rm = TRUE), .names = "szn_{.col}"),
              .groups = "drop") %>%
    mutate(
      szn_completion_perc = szn_completions / szn_attempts,
      szn_yards_per_completion = szn_passing_yards / szn_completions,
      szn_passing_adot = szn_passing_air_yards / szn_attempts,
      szn_yards_per_carry = szn_rushing_yards / szn_carries,
      szn_yards_per_catch = szn_receiving_yards / szn_receptions,
      szn_yards_per_target = szn_receiving_yards / szn_targets,
      szn_catch_perc = szn_receptions / szn_targets,
      szn_adot = szn_receiving_air_yards / szn_targets,
      szn_ayac = szn_receiving_yards_after_catch / szn_receptions
    )
}

# ── Defense allowed: weekly, season, positional, ranks ───────
build_defense_allowed <- function(gamelogs, asof_season, asof_week) {
  weekly <- gamelogs %>%
    filter(!is.na(opponent)) %>%
    group_by(opponent, season, week) %>%
    summarise(across(c(completions, attempts, passing_yards, passing_tds,
                       passing_interceptions, passing_air_yards,
                       passing_yards_after_catch, passing_first_downs,
                       carries, rushing_yards, rushing_tds,
                       receptions, targets, receiving_yards, receiving_tds,
                       receiving_air_yards, receiving_yards_after_catch,
                       receiving_first_downs),
                     \(x) sum(x, na.rm = TRUE)),
              across(c(passing_epa, rushing_epa, receiving_epa),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")

  season <- weekly %>%
    filter(season == asof_season, week < asof_week) %>%
    group_by(opponent) %>%
    summarise(across(-c(season, week), mean), .groups = "drop") %>%
    mutate(
      comp_perc = completions / attempts,
      yards_per_completion = passing_yards / completions,
      yards_per_attempt = passing_yards / attempts,
      ay_per_attempt = passing_air_yards / attempts,
      yards_per_carry = rushing_yards / carries,
      catch_rate = receptions / targets,
      yards_per_catch = receiving_yards / receptions,
      avg_yac = receiving_yards_after_catch / receptions
    )

  by_pos <- gamelogs %>%
    filter(season == asof_season, week < asof_week, !is.na(opponent),
           position %in% c("WR", "RB", "TE")) %>%
    group_by(opponent, season, week, position) %>%
    summarise(receptions = sum(receptions), targets = sum(targets),
              receiving_yards = sum(receiving_yards),
              receiving_yac = sum(receiving_yards_after_catch),
              .groups = "drop") %>%
    group_by(opponent, position) %>%
    summarise(def_receptions = mean(receptions), def_targets = mean(targets),
              def_receiving_yards = mean(receiving_yards),
              def_receiving_yac = mean(receiving_yac), .groups = "drop") %>%
    mutate(def_catch_rate = def_receptions / def_targets,
           def_yards_per_catch = def_receiving_yards / def_receptions) %>%
    group_by(position) %>%
    mutate(across(c(def_receptions, def_receiving_yards, def_receiving_yac,
                    def_yards_per_catch, def_catch_rate),
                  rank, .names = "{.col}_rank")) %>%
    ungroup()

  ranks <- season %>%
    pivot_longer(-opponent, names_to = "measure", values_to = "stat") %>%
    group_by(measure) %>%
    mutate(rank = rank(stat),
           league_average = mean(stat, na.rm = TRUE)) %>%
    ungroup()

  list(weekly = weekly, season = season, by_position = by_pos, ranks = ranks)
}

# ── EPA ranks (original epa_ranks) ───────────────────────────
build_epa_ranks <- function(pbp, asof_season, asof_week) {
  base <- pbp %>%
    filter(season == asof_season, week < asof_week,
           season_type == "REG", !is.na(epa))
  off <- base %>% group_by(team = posteam) %>%
    summarise(off_epa = mean(epa),
              off_pass_epa = mean(epa[pass_attempt == 1]),
              off_rush_epa = mean(epa[pass == 0]), .groups = "drop")
  def <- base %>% group_by(team = defteam) %>%
    summarise(def_epa = mean(epa),
              def_pass_epa = mean(epa[pass_attempt == 1]),
              def_rush_epa = mean(epa[pass == 0]), .groups = "drop")
  off %>%
    left_join(def, by = "team") %>%
    mutate(total_epa = round(off_epa - def_epa, 3),
           total_rank = rank(desc(total_epa)),
           off_rank = rank(desc(off_epa)), def_rank = rank(def_epa),
           off_pass_rank = rank(desc(off_pass_epa)),
           off_rush_rank = rank(desc(off_rush_epa)),
           def_pass_rank = rank(def_pass_epa),
           def_rush_rank = rank(def_rush_epa))
}

# ── Unrealized air yards; longest plays ──────────────────────
build_unrealized_receiving <- function(pbp) {
  pbp %>%
    filter(!is.na(receiver_player_id)) %>%
    group_by(season, week, receiver_player_id) %>%
    summarise(unrealized_air_yards = sum(unrealized_air_yards),
              unrealized_targets = sum(unrealized_targets), .groups = "drop")
}

build_longest_plays <- function(pbp) {
  list(
    reception = pbp %>%
      filter(!is.na(receiver_player_id), !is.na(receiving_yards)) %>%
      group_by(season, week, receiver_player_id) %>%
      summarise(longest_rec = max(receiving_yards), .groups = "drop"),
    rush = pbp %>%
      filter(!is.na(rusher_player_id), !is.na(rushing_yards)) %>%
      group_by(season, week, rusher_player_id) %>%
      summarise(longest_rush = max(rushing_yards), .groups = "drop")
  )
}

# ── Historical game script by closing spread range ───────────
#
#  Replaces the original's 20-year nflspreaddata.csv import: for
#  each closing team-spread bucket, the average share of a team's
#  snaps spent in positive (wp > .75), neutral, and negative
#  (wp < .25) win-probability states.
#
#  Two-layer cache so the 20-season history never has to be
#  rebuilt from raw pbp:
#    data/context/gamescript_games.csv   one row per team-game
#        (season, game_id, posteam, team_spread, shares, n plays)
#        — COMMITTED; completed seasons are immutable
#    data/context/spread_gamescript.csv  the aggregated bucket
#        table consumed by build_play_projections()
#
#  refresh_spread_gamescript() computes rows for any season
#  missing from the cache and RE-computes the current season on
#  every run (its pbp file grows weekly), so the table keeps
#  updating as new games are added. run_model.R calls it after
#  refresh_pbp().
SPREAD_RANGES <- tibble(
  lo = c(0, 3.5, 6.5, 7.5, 10.5, 14.5, 17.5,
         -3, -6, -7, -10, -14, -17, -Inf),
  hi = c(3, 6, 7, 10, 14, 17, Inf,
         -0.01, -3.5, -6.5, -7.5, -10.5, -14.5, -17.51),
  range = c("0 to +3", "+3.5 to +6", "+6.5 to +7", "+7.5 to +10",
            "+10.5 to +14", "+14.5 to +17", "+17.5 +",
            "0 to -3", "-3.5 to -6", "-6.5 to -7", "-7.5 to -10",
            "-10.5 to -14", "-14.5 to -17", "-17.5 +")
)

spread_range_label <- function(team_spread) {
  sapply(team_spread, function(s) {
    if (is.na(s)) return(NA_character_)
    hit <- which(s >= pmin(SPREAD_RANGES$lo, SPREAD_RANGES$hi) &
                 s <= pmax(SPREAD_RANGES$lo, SPREAD_RANGES$hi))
    if (length(hit)) SPREAD_RANGES$range[hit[1]] else NA_character_
  })
}

GAMESCRIPT_GAMES_FILE <- file.path("data", "context", "gamescript_games.csv")
GAMESCRIPT_TABLE_FILE <- file.path("data", "context", "spread_gamescript.csv")
GAMESCRIPT_SEASONS    <- 2006:2100   # "last 20 years" rolling start

# One row per team-game: the team's snap distribution over
# twenty 5%-wide win-probability bins (wp_00 = [0,.05), ...,
# wp_95 = [.95,1]) plus the posteam-relative closing spread.
# Storing the raw WP histogram — not a fixed bucketing — means
# ANY state scheme with boundaries at multiples of 0.05
# (quartiles, quintiles, legacy .25/.75) is derivable later
# without re-downloading 20 seasons of raw pbp.
compute_gamescript_rows <- function(season) {
  path <- pbp_path(season)
  if (!file.exists(path)) return(NULL)
  df <- read_csv(path, col_types = cols_only(
    game_id = col_character(), season = col_integer(),
    posteam = col_character(), qtr = col_integer(),
    play_type = col_character(), two_point_attempt = col_double(),
    wp = col_double(), spread_line = col_double()
  ), progress = FALSE) %>%
    filter(!is.na(posteam), play_type %in% c("pass", "run"),
           two_point_attempt == 0, qtr %in% 1:4, !is.na(wp)) %>%
    mutate(home_team_id = sub(".*_", "", game_id),
           team_spread  = ifelse(posteam == home_team_id,
                                 spread_line, -spread_line),
           bin = sprintf("wp_%02d", pmin(floor(wp * 20), 19) * 5))
  df %>%
    count(season, game_id, posteam, team_spread, bin) %>%
    group_by(season, game_id, posteam, team_spread) %>%
    mutate(n_plays = sum(n), share = n / n_plays) %>%
    ungroup() %>%
    select(-n) %>%
    tidyr::pivot_wider(names_from = bin, values_from = share,
                       values_fill = 0) %>%
    select(season, game_id, posteam, team_spread, n_plays,
           any_of(sprintf("wp_%02d", seq(0, 95, 5))))
}

# ── Derive state shares from the WP histogram ────────────────
#  boundaries: interior cut points, each a multiple of 0.05.
#  Canonical scheme: QUARTILES c(.25,.5,.75) — Study S9 (the old
#  3-state "neutral" band hid a 61.9% vs 57.5% pass-rate split at
#  its midpoint). Legacy 3-state = c(.25,.75).
GS_STATE_BOUNDS  <- c(.25, .50, .75)
GS_STATE_LABELS  <- c("q1_trail_big", "q2_trail", "q3_lead", "q4_lead_big")

gamescript_state_shares <- function(games, boundaries = GS_STATE_BOUNDS,
                                    labels = GS_STATE_LABELS) {
  stopifnot(all(abs(boundaries * 20 - round(boundaries * 20)) < 1e-9))
  edges <- c(0, boundaries, 1)
  bin_cols <- sprintf("wp_%02d", seq(0, 95, 5))
  for (i in seq_along(labels)) {
    cols <- bin_cols[(edges[i] * 20 + 1):(edges[i + 1] * 20)]
    games[[labels[i]]] <- rowSums(games[, cols, drop = FALSE], na.rm = TRUE)
  }
  games
}

#  Fills the per-game cache for any season whose pbp is cached
#  locally but absent from gamescript_games.csv; always
#  recomputes `current_season` (new games arrive weekly).
refresh_spread_gamescript <- function(current_season = NULL) {
  existing <- if (file.exists(GAMESCRIPT_GAMES_FILE))
    read_csv(GAMESCRIPT_GAMES_FILE, show_col_types = FALSE) else NULL

  cached_pbp <- as.integer(gsub("\\D", "", basename(
    list.files(PBP_DIR, pattern = "^play_by_play_\\d{4}\\.csv\\.gz$"))))
  have_rows  <- if (is.null(existing)) integer(0) else unique(existing$season)
  todo <- setdiff(cached_pbp, have_rows)
  if (!is.null(current_season) && current_season %in% cached_pbp)
    todo <- union(todo, current_season)
  if (length(todo) == 0) return(invisible(existing))

  new_rows <- bind_rows(lapply(sort(todo), function(s) {
    message(sprintf("  gamescript: computing %d", s))
    compute_gamescript_rows(s)
  }))
  keep <- if (is.null(existing)) NULL else existing %>% filter(!season %in% todo)
  out  <- bind_rows(keep, new_rows) %>%
    arrange(season, game_id, posteam)
  dir.create(dirname(GAMESCRIPT_GAMES_FILE), recursive = TRUE,
             showWarnings = FALSE)
  write_csv(out, GAMESCRIPT_GAMES_FILE)

  # aggregate to the bucket table: canonical quartile states plus
  # the legacy 3-state columns (derived from the same histogram)
  tbl <- out %>%
    filter(season >= min(GAMESCRIPT_SEASONS), n_plays >= 30) %>%
    gamescript_state_shares() %>%
    gamescript_state_shares(boundaries = c(.25, .75),
                            labels = c("negative", "neutral", "positive")) %>%
    mutate(range = spread_range_label(team_spread)) %>%
    filter(!is.na(range)) %>%
    group_by(range) %>%
    summarise(games = n(),
              across(all_of(c(GS_STATE_LABELS,
                              "positive", "neutral", "negative")), mean),
              .groups = "drop")
  write_csv(tbl, GAMESCRIPT_TABLE_FILE)
  message(sprintf("  gamescript: %d team-games (%d-%d) -> %s",
                  nrow(out), min(out$season), max(out$season),
                  GAMESCRIPT_TABLE_FILE))
  invisible(out)
}

#  The bucket table consumed by build_play_projections().
build_spread_gamescript_table <- function() {
  if (!file.exists(GAMESCRIPT_TABLE_FILE)) refresh_spread_gamescript()
  read_csv(GAMESCRIPT_TABLE_FILE, show_col_types = FALSE)
}

# ── Play-volume projections for a slate ──────────────────────
#
#  The original projections block:
#   proj1 = (team plays for + opp plays allowed) / 2
#   proj2 = team plays for x (opp plays allowed / league avg)
#   team_projected_plays = mean(proj1, proj2)
#  then splits into pass/rush two ways:
#   v1: season pass/rush rate
#   v2: expected game-script mix (spread-range positive/neutral/
#       negative shares) x the team's conditional pass/rush rates
build_play_projections <- function(slate, playcall_season, plays_defense,
                                   gamescript_table) {
  avg_plays <- mean(plays_defense$total_plays)
  slate %>%
    left_join(playcall_season, by = c("team" = "posteam")) %>%
    left_join(plays_defense, by = c("opponent" = "defteam"),
              suffix = c("", "_oppdef")) %>%
    mutate(
      team_proj1 = (total_plays + total_plays_oppdef) / 2,
      team_proj2 = total_plays * (total_plays_oppdef / avg_plays),
      team_projected_plays = (team_proj1 + team_proj2) / 2,
      range = spread_range_label(team_spread)
    ) %>%
    left_join(gamescript_table, by = "range") %>%
    mutate(
      pass_proj_1 = team_projected_plays * pass_perc,
      rush_proj_1 = team_projected_plays * rush_perc,
      pass_proj_2 = team_projected_plays *
        (positive * positive_pass_perc + negative * negative_pass_perc +
           neutral * neutral_pass_perc),
      rush_proj_2 = team_projected_plays *
        (positive * positive_rush_perc + negative * negative_rush_perc +
           neutral * neutral_rush_perc),
      passes_thrown_proj_1 = team_projected_plays * pass_thrown_perc,
      passes_thrown_proj_2 = pass_proj_2 * pass_off_perc
    )
}

# ── Optional PFF drop-in ─────────────────────────────────────
#  Put pff_receiving_summary_{season}_week{n}.csv files (PFF
#  exports) in data/pff/ and they are stacked and gsis-resolved.
load_pff_receiving <- function(seasons, rosters) {
  files <- list.files(PFF_DIR, pattern = "^pff_receiving_summary_.*\\.csv$",
                      full.names = TRUE)
  if (length(files) == 0) return(NULL)
  bind_rows(lapply(files, function(f) {
    m <- regmatches(basename(f),
                    regexec("summary_(\\d{4})_week(\\d+)", basename(f)))[[1]]
    read_csv(f, col_types = cols(player_id = col_character())) %>%
      mutate(season = as.integer(m[2]), week = as.integer(m[3]))
  })) %>%
    filter(season %in% seasons) %>%
    left_join(rosters %>% filter(!is.na(pff_id)) %>%
                select(season, pff_id, gsis_id),
              by = c("season", "player_id" = "pff_id"))
}

# ── The combined model table (original NFLmodel) ─────────────
build_model_table <- function(seasons, asof_season, asof_week) {
  rosters    <- load_rosters_cache(seasons)
  sched_long <- build_schedule_long()
  gamelogs   <- build_gamelogs(seasons, rosters, sched_long)
  pbp        <- load_feature_pbp(seasons)

  playcall_w <- build_playcall_weekly(pbp)
  snaps      <- load_snap_counts_cache(seasons)
  defense    <- build_defense_allowed(gamelogs, asof_season, asof_week)
  szn        <- build_gamelogs_season(gamelogs, asof_season, asof_week)
  unreal     <- build_unrealized_receiving(pbp)
  longest    <- build_longest_plays(pbp)
  pff        <- load_pff_receiving(seasons, rosters)

  out <- gamelogs %>%
    left_join(playcall_w, by = c("season", "team" = "posteam", "week")) %>%
    left_join(snaps %>% filter(!is.na(gsis_id)) %>%
                select(gsis_id, season, week, offense_snaps, offense_pct),
              by = c("player_id" = "gsis_id", "season", "week")) %>%
    left_join(load_ngs_cache("receiving", seasons) %>%
                select(player_gsis_id, season, week, avg_cushion,
                       avg_separation, avg_intended_air_yards,
                       percent_share_of_intended_air_yards,
                       catch_percentage, ngs_avg_yac = avg_yac,
                       avg_expected_yac, avg_yac_above_expectation),
              by = c("player_id" = "player_gsis_id", "season", "week")) %>%
    left_join(load_ngs_cache("rushing", seasons) %>%
                select(player_gsis_id, season, week, efficiency,
                       percent_attempts_gte_eight_defenders,
                       avg_time_to_los, expected_rush_yards,
                       rush_yards_over_expected, avg_rush_yards,
                       rush_yards_over_expected_per_att,
                       rush_pct_over_expected),
              by = c("player_id" = "player_gsis_id", "season", "week")) %>%
    left_join(load_ngs_cache("passing", seasons) %>%
                select(player_gsis_id, season, week, avg_time_to_throw,
                       avg_completed_air_yards, ngs_avg_intended_air_yards =
                         avg_intended_air_yards, avg_air_yards_differential,
                       completion_percentage, expected_completion_percentage,
                       completion_percentage_above_expectation),
              by = c("player_id" = "player_gsis_id", "season", "week")) %>%
    left_join(szn %>% select(-player_display_name), by = "player_id") %>%
    left_join(unreal, by = c("season", "week",
                             "player_id" = "receiver_player_id")) %>%
    left_join(longest$reception,
              by = c("season", "week", "player_id" = "receiver_player_id")) %>%
    left_join(longest$rush,
              by = c("season", "week", "player_id" = "rusher_player_id")) %>%
    mutate(rush_share = carries / total_rush_plays)

  if (!is.null(pff)) {
    out <- out %>%
      left_join(pff %>% select(gsis_id, season, week, avg_depth_of_target,
                               grades_offense, grades_pass_route, route_rate,
                               routes, slot_rate, wide_rate, yprr, drops),
                by = c("player_id" = "gsis_id", "season", "week"))
  }
  out
}
