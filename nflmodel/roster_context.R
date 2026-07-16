# ============================================================
#  roster_context.R  (nflmodel)
#
#  Per-game depth charts and injury reports, joined to the pbp
#  world by (season, week, team) — the availability/usage layer
#  the props models condition on.
#
#  Sources (nflverse releases; nflreadr::load_depth_charts /
#  load_injuries serve the same files):
#
#  INJURIES  data/rosters/injuries_{season}.csv
#    Official weekly injury reports, one row per player-week:
#    report_status  = game designation (Out / Doubtful /
#                     Questionable / blank = no designation)
#    practice_status = Wed-Fri practice participation (DNP /
#                     Limited / Full)
#    Already per-game grain: (season, week, team, gsis_id).
#
#  DEPTH CHARTS  data/rosters/depth_charts_{season}.csv
#    TWO vendor formats, normalized here into one schema:
#    - through 2024: one chart per team-WEEK
#      (club_code, week, formation, depth_team = rank,
#       depth_position)
#    - 2025+: timestamped SNAPSHOTS (~daily), (dt, team,
#       pos_grp, pos_abb, pos_slot, pos_rank).
#      Made per-game by keeping the LATEST snapshot on or
#      before each game's kickoff date (game_context dates).
#
#  Normalized depth chart schema (one row per player-slot):
#    season, week, team, side (Offense/Defense/ST),
#    position, depth_rank, gsis_id, player_name
#
#  Public API:
#    refresh_rosters(seasons)         download/update both caches
#    load_injuries_cache(seasons)     per player-week report
#    load_depth_charts_game(seasons)  normalized per-game charts
#    starters(dc, side = "Offense")   depth_rank == 1 slice
# ============================================================

library(dplyr)
library(readr)

ROSTER_DIR <- file.path("data", "rosters")

.roster_path <- function(kind, season)
  file.path(ROSTER_DIR, sprintf("%s_%d.csv", kind, season))

.roster_url <- function(kind, season)
  sprintf(paste0("https://github.com/nflverse/nflverse-data/releases/",
                 "download/%s/%s_%d.csv"), kind, kind, season)

# ── Refresh both caches ──────────────────────────────────────
refresh_rosters <- function(seasons = NULL, max_age_hrs = 20) {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  current_season <- if (as.integer(format(Sys.Date(), "%m")) >= 9)
    this_year else this_year - 1L
  if (is.null(seasons)) seasons <- (current_season - 1L):(current_season + 1L)
  dir.create(ROSTER_DIR, recursive = TRUE, showWarnings = FALSE)

  for (kind in c("injuries", "depth_charts")) {
    loader <- switch(kind, injuries = "load_injuries",
                     depth_charts = "load_depth_charts")
    for (s in seasons) {
      path  <- .roster_path(kind, s)
      fresh <- file.exists(path) &&
        (s < current_season ||
           difftime(Sys.time(), file.mtime(path), units = "hours") < max_age_hrs)
      if (fresh) { message(sprintf("  %s %d: cache fresh", kind, s)); next }
      ok <- FALSE
      if (requireNamespace("nflreadr", quietly = TRUE)) {
        ok <- tryCatch({
          df <- getExportedValue("nflreadr", loader)(seasons = s)
          write_csv(df, path); TRUE
        }, error = function(e) FALSE)
      }
      if (!ok) ok <- tryCatch({
        download.file(.roster_url(kind, s), path, mode = "wb", quiet = TRUE)
        TRUE
      }, error = function(e) FALSE)
      message(sprintf("  %s %d: %s", kind, s,
                      if (ok) "refreshed" else "not available (season not started?)"))
    }
  }
  invisible(TRUE)
}

# ── Injuries (already per-game grain) ────────────────────────
load_injuries_cache <- function(seasons) {
  paths <- .roster_path("injuries", seasons)
  have  <- file.exists(paths)
  if (!any(have)) stop("No injuries cache — run refresh_rosters() first.")
  bind_rows(lapply(paths[have], function(p)
    read_csv(p, show_col_types = FALSE) %>%
      transmute(
        season, week, game_type,
        team = normalize_team(team),
        gsis_id, full_name, position,
        report_status,          # Out / Doubtful / Questionable / NA
        practice_status,
        primary_injury = coalesce(report_primary_injury,
                                  practice_primary_injury)
      )))
}

# ── Depth charts, normalized per-game ────────────────────────
#  `sched` is load_game_context() output — required for the
#  2025+ snapshot format (kickoff dates); harmless otherwise.
load_depth_charts_game <- function(seasons, sched = load_game_context()) {
  out <- list()
  for (s in seasons) {
    path <- .roster_path("depth_charts", s)
    if (!file.exists(path)) { message("  depth charts missing: ", s); next }
    hdr <- names(read_csv(path, n_max = 0, show_col_types = FALSE))

    if ("dt" %in% hdr) {
      # 2025+ snapshot format -> latest snapshot per game date
      dc <- read_csv(path, show_col_types = FALSE) %>%
        mutate(team = normalize_team(team), snap_date = as.Date(dt))
      games <- sched %>%
        filter(season == s) %>%
        tidyr::pivot_longer(c(home_team, away_team), values_to = "team") %>%
        transmute(season, week, team, gameday = as.Date(gameday))
      snap_dates <- sort(unique(dc$snap_date))
      pick <- games %>%
        rowwise() %>%
        mutate(snap_date = {
          d <- snap_dates[snap_dates <= gameday]
          if (length(d)) max(d) else as.Date(NA)
        }) %>%
        ungroup() %>%
        filter(!is.na(snap_date))
      dc_latest <- dc %>%
        group_by(team, snap_date) %>%
        filter(dt == max(dt)) %>%          # last snapshot of that day
        ungroup()
      out[[as.character(s)]] <- pick %>%
        inner_join(dc_latest, by = c("team", "snap_date"),
                   relationship = "many-to-many") %>%
        transmute(
          season, week, team,
          side = case_when(
            grepl(" D$| D |Defense", pos_grp) ~ "Defense",
            pos_grp == "Special Teams"        ~ "ST",
            TRUE                              ~ "Offense"
          ),
          position   = pos_abb,
          depth_rank = pos_rank,
          gsis_id, player_name
        )
    } else {
      # through-2024 weekly format
      out[[as.character(s)]] <- read_csv(path, show_col_types = FALSE) %>%
        transmute(
          season, week,
          team       = normalize_team(club_code),
          side       = ifelse(formation == "Special Teams", "ST", formation),
          position   = coalesce(depth_position, position),
          depth_rank = as.integer(depth_team),
          gsis_id, player_name = full_name
        )
    }
  }
  bind_rows(out)
}

# ── Starters: depth_rank 1 on a side ─────────────────────────
starters <- function(dc, side = "Offense") {
  dc %>% filter(side == !!side, depth_rank == 1L)
}
