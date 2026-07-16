# ============================================================
#  pbp_data.R  (nflmodel)
#
#  Play-by-play data layer — nflfastR pbp, one file per season,
#  ~48k plays x 372 columns each. This is the model's ground
#  truth going forward; the weekly-log cache in data_sources.R
#  is derivable from it.
#
#  Storage: data/pbp/play_by_play_{season}.csv.gz  (gitignored;
#  ~19 MB per season compressed).
#
#  Sources, in order of preference:
#    1. nflreadr::load_pbp()          (local machines with CRAN)
#    2. direct download from the nflverse-data GitHub release:
#       https://github.com/nflverse/nflverse-data/releases/
#         download/pbp/play_by_play_{season}.csv.gz
#       (nightly-updated during the season)
#
#  The current season's file appears when the season starts
#  (2026's does not exist yet as of July 2026) — refresh_pbp()
#  treats a 404 on the current season as "not started", not an
#  error, so the same call works year-round.
# ============================================================

library(dplyr)
library(readr)

PBP_DIR <- file.path("data", "pbp")

pbp_path <- function(season) {
  file.path(PBP_DIR, sprintf("play_by_play_%d.csv.gz", season))
}

pbp_release_url <- function(season) {
  sprintf(paste0("https://github.com/nflverse/nflverse-data/releases/",
                 "download/pbp/play_by_play_%d.csv.gz"), season)
}

# ── Refresh the pbp cache ────────────────────────────────────
#  seasons     : integer vector (default: last season + current)
#  max_age_hrs : re-download the CURRENT season if its cache is
#                older than this (nflverse updates nightly);
#                completed seasons are never re-downloaded.
refresh_pbp <- function(seasons = NULL, max_age_hrs = 20) {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  # NFL season YYYY runs Sep YYYY - Feb YYYY+1; before September,
  # "current" is still last year's completed season.
  current_season <- if (as.integer(format(Sys.Date(), "%m")) >= 9)
    this_year else this_year - 1L
  if (is.null(seasons)) seasons <- (current_season - 1L):(current_season + 1L)

  dir.create(PBP_DIR, recursive = TRUE, showWarnings = FALSE)

  for (s in seasons) {
    path  <- pbp_path(s)
    fresh <- file.exists(path) &&
      (s < current_season ||
         difftime(Sys.time(), file.mtime(path), units = "hours") < max_age_hrs)
    if (fresh) { message(sprintf("  pbp %d: cache fresh", s)); next }

    ok <- FALSE
    if (requireNamespace("nflreadr", quietly = TRUE)) {
      ok <- tryCatch({
        df <- nflreadr::load_pbp(seasons = s)
        write_csv(df, path)
        message(sprintf("  pbp %d: %d plays via nflreadr", s, nrow(df)))
        TRUE
      }, error = function(e) FALSE)
    }
    if (!ok) {
      ok <- tryCatch({
        download.file(pbp_release_url(s), path, mode = "wb", quiet = TRUE)
        message(sprintf("  pbp %d: downloaded from nflverse release", s))
        TRUE
      }, error = function(e) FALSE)
    }
    if (!ok) {
      if (s > current_season || (s == current_season && !file.exists(path))) {
        message(sprintf("  pbp %d: not available (season not started?)", s))
      } else {
        warning(sprintf("pbp %d: refresh failed, using stale cache", s))
      }
    }
  }
  invisible(TRUE)
}

# ── Load pbp from cache ──────────────────────────────────────
#  seasons : integer vector
#  cols    : optional character vector of columns (the files are
#            372 columns wide — subsetting keeps memory sane;
#            readr skips unselected columns at parse time)
load_pbp_cache <- function(seasons, cols = NULL) {
  paths <- pbp_path(seasons)
  have  <- file.exists(paths)
  if (!any(have)) stop("No pbp cache for seasons ",
                       paste(seasons, collapse = ", "),
                       " — run refresh_pbp() first.")
  if (!all(have)) message("  pbp missing (skipped): ",
                          paste(seasons[!have], collapse = ", "))

  sel <- if (is.null(cols)) NULL else do.call(cols_only, setNames(
    rep(list(col_guess()), length(cols)), cols))

  bind_rows(lapply(paths[have], function(p) {
    if (is.null(sel)) read_csv(p, show_col_types = FALSE, guess_max = 10000)
    else read_csv(p, col_types = sel, show_col_types = FALSE)
  }))
}

# ── Columns the props models care about ──────────────────────
#  A curated subset (~1/8 of the file) covering usage, efficiency
#  and situation for all four markets. Passed to load_pbp_cache()
#  by downstream builders.
PBP_MODEL_COLS <- c(
  # identity / context
  "game_id", "season", "week", "game_date", "posteam", "defteam",
  "home_team", "away_team", "season_type",
  # situation
  "qtr", "down", "ydstogo", "yardline_100", "goal_to_go", "drive",
  "game_seconds_remaining", "half_seconds_remaining",
  "score_differential", "wp", "vegas_wp",
  # play
  "play_type", "desc", "yards_gained", "epa", "success",
  "shotgun", "no_huddle", "qb_dropback", "qb_scramble", "qb_kneel",
  "qb_spike", "sack", "penalty",
  # passing
  "passer_player_id", "passer_player_name", "pass_attempt",
  "complete_pass", "incomplete_pass", "interception",
  "passing_yards", "pass_touchdown", "air_yards", "yards_after_catch",
  "pass_location", "pass_length", "cpoe", "xpass", "pass_oe",
  # receiving
  "receiver_player_id", "receiver_player_name", "receiving_yards",
  # rushing
  "rusher_player_id", "rusher_player_name", "rush_attempt",
  "rushing_yards", "rush_touchdown", "run_location", "run_gap",
  # scoring
  "touchdown", "td_player_id", "td_player_name", "two_point_attempt",
  # vegas
  "spread_line", "total_line",
  # environment (game-constant; game_context.R parses weather further)
  "weather", "temp", "wind", "roof", "surface", "stadium"
)
