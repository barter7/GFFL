# ============================================================
#  playcall_tendencies.R  (nflmodel)
#
#  Play-calling tendencies from pbp, by CALLED play, not result
#  (rule R4.7 / Study S12):
#
#    called pass = pass == 1   (nflfastR dropback indicator:
#                               throws + sacks + scrambles)
#    called run  = rush == 1   (designed runs only)
#
#  Loopholes handled (audited on 2025, Study S12):
#   • scrambles: play_type "run" but pass==1 → called pass
#   • sacks: inside called pass
#   • kneels/spikes carry no intent flags → auto-excluded
#     (clock decisions, not tendencies)
#   • nullified snaps (no_play with pass/rush==1) reveal the
#     call → INCLUDED in tendency rates, EXCLUDED from play
#     volume (official plays only)
#   • RPOs are unobservable in pbp: they land on the called-run
#     side unless thrown. Known limitation, not fixable here.
#
#  Outputs (data/context/):
#    playcall_league.csv  per season (+ ALL row) × script state:
#                         called-pass%, snaps, plays/g,
#                         sec/snap overall + after pass / run
#    playcall_team.csv    per team-season: overall + per-quartile
#                         called-pass%, deviation from own base,
#                         plays/g, sec/snap splits
#
#  Game-script state = pre-snap wp quartile (R4.1). Seconds per
#  play = game clock elapsed from this snap to the next snap of
#  the SAME drive (capped at 60s; last snap of a drive has no
#  elapsed value). Timeout/penalty gaps inside a drive inflate
#  the raw value slightly — capped, and identical across teams.
#
#  Tendencies are era-scoped by design: build from recent
#  seasons (default = the local pbp cache window), not 20 years —
#  league play-calling drifts.
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr) })

PLAYCALL_LEAGUE_FILE <- file.path("data", "context", "playcall_league.csv")
PLAYCALL_TEAM_FILE   <- file.path("data", "context", "playcall_team.csv")

# ── Snap-level table with intent, state, and elapsed clock ───
build_playcall_snaps <- function(seasons) {
  pbp <- load_pbp_cache(seasons, cols = c(
    "game_id", "season", "week", "posteam", "qtr", "drive",
    "play_type", "pass", "rush", "qb_scramble", "sack",
    "two_point_attempt", "wp", "game_seconds_remaining",
    "complete_pass", "incomplete_pass"
  )) %>%
    filter(!is.na(posteam), two_point_attempt == 0, qtr %in% 1:4,
           pass == 1 | rush == 1) %>%          # a call was revealed
    mutate(
      called    = ifelse(pass == 1, "pass", "run"),
      official  = play_type %in% c("pass", "run"),   # counts as a play run
      state     = cut(wp, c(-Inf, .25, .5, .75, Inf),
                      labels = c("q1_trail_big", "q2_trail",
                                 "q3_lead", "q4_lead_big"))
    ) %>%
    arrange(game_id, desc(game_seconds_remaining)) %>%
    group_by(game_id, drive) %>%
    mutate(elapsed = game_seconds_remaining - lead(game_seconds_remaining),
           elapsed = ifelse(official & !is.na(elapsed) &
                              elapsed > 0 & elapsed <= 60, elapsed, NA)) %>%
    ungroup()
  pbp
}

.summarise_block <- function(d) {
  d %>% summarise(
    calls        = n(),
    called_pass  = sum(called == "pass"),
    pass_rate    = called_pass / calls,
    snaps        = sum(official),                    # official plays run
    sec_per_play = mean(elapsed, na.rm = TRUE),
    sec_after_pass = mean(elapsed[called == "pass"], na.rm = TRUE),
    sec_after_run  = mean(elapsed[called == "run"],  na.rm = TRUE),
    .groups = "drop")
}

# ── League table: per season and pooled, overall + by state ──
build_playcall_league <- function(snaps) {
  games_per <- snaps %>% filter(official) %>%
    group_by(season) %>% summarise(team_games = n_distinct(paste(game_id, posteam)))
  by_state <- bind_rows(
    snaps %>% group_by(season, state) %>% .summarise_block(),
    snaps %>% mutate(state = "overall") %>% group_by(season, state) %>% .summarise_block(),
    snaps %>% mutate(season = 0L) %>% group_by(season, state) %>% .summarise_block(),
    snaps %>% mutate(season = 0L, state = "overall") %>%
      group_by(season, state) %>% .summarise_block()
  )
  by_state %>%
    left_join(bind_rows(games_per,
                        tibble(season = 0L,
                               team_games = sum(games_per$team_games))),
              by = "season") %>%
    mutate(plays_pg = ifelse(state == "overall", snaps / team_games, NA)) %>%
    mutate(across(where(is.numeric), \(x) round(x, 4)))
}

# ── Team-season table: base rate, per-state rates, deviations ─
build_playcall_team <- function(snaps) {
  base <- snaps %>% group_by(season, team = posteam) %>% .summarise_block() %>%
    rename_with(\(x) paste0(x, "_all"), -c(season, team))
  st <- snaps %>%
    group_by(season, team = posteam, state) %>%
    summarise(calls = n(), pass_rate = sum(called == "pass") / calls,
              sec_per_play = mean(elapsed, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = state,
                values_from = c(calls, pass_rate, sec_per_play))
  gpg <- snaps %>% filter(official) %>%
    group_by(season, team = posteam) %>%
    summarise(games = n_distinct(game_id), plays_pg = n() / games,
              .groups = "drop")
  base %>%
    left_join(st,  by = c("season", "team")) %>%
    left_join(gpg, by = c("season", "team")) %>%
    mutate(
      dev_q1 = pass_rate_q1_trail_big - pass_rate_all,
      dev_q2 = pass_rate_q2_trail     - pass_rate_all,
      dev_q3 = pass_rate_q3_lead      - pass_rate_all,
      dev_q4 = pass_rate_q4_lead_big  - pass_rate_all
    ) %>%
    mutate(across(where(is.numeric), \(x) round(x, 4)))
}

refresh_playcall <- function(seasons = NULL) {
  if (is.null(seasons)) {
    seasons <- as.integer(gsub("\\D", "", basename(
      list.files(PBP_DIR, pattern = "^play_by_play_\\d{4}\\.csv\\.gz$"))))
  }
  snaps <- build_playcall_snaps(seasons)
  lg <- build_playcall_league(snaps)
  tm <- build_playcall_team(snaps)
  write_csv(lg, PLAYCALL_LEAGUE_FILE)
  write_csv(tm, PLAYCALL_TEAM_FILE)
  message(sprintf("  playcall: %d snaps, %d team-seasons -> %s / %s",
                  nrow(snaps), nrow(tm),
                  basename(PLAYCALL_LEAGUE_FILE), basename(PLAYCALL_TEAM_FILE)))
  invisible(list(league = lg, team = tm))
}
