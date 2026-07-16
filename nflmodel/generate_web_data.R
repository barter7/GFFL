# ============================================================
#  generate_web_data.R  (nflmodel)
#
#  Pre-computes the JSON data contract for the props dashboard
#  (the NFL sibling of PropSZN's generate_web_data.R). The site
#  reads only these files — no computation in the browser.
#
#  Output (data/web/):
#    meta.json                 generated_at, seasons, league avgs
#    players.json              selector list: id, name, pos, team,
#                              headshot, games, current-season usage
#    teams/{TEAM}_{season}.json  target-share matrix (player x week)
#    defense.json              EPA ranks + plays faced (latest asof)
#    gamelogs/{gsis_id}.json   per-player weekly rows: schedule
#                              context (opp, H/A, spread, total),
#                              snap%, team playcall + WP-state
#                              shares, rushing, receiving, usage
#                              shares, efficiency, longest plays
#    props/  projections/      placeholder dirs — filled by
#                              scrape_props.R and the model layer
#                              once those stages come online
#
#  Usage: Rscript generate_web_data.R [seasons...]  (default 2024 2025)
# ============================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(jsonlite)
})
source("utils.R"); source("pbp_data.R"); source("game_context.R")
source("nfl_features.R")

args    <- commandArgs(trailingOnly = TRUE)
SEASONS <- if (length(args)) as.integer(args) else c(2024L, 2025L)
ASOF_S  <- max(SEASONS); ASOF_W <- 30L      # browse mode: all completed weeks

WEB_DIR <- file.path("data", "web")
for (d in c("gamelogs", "teams", "props", "projections"))
  dir.create(file.path(WEB_DIR, d), recursive = TRUE, showWarnings = FALSE)

cat("[web-data] building model table...\n")
mt <- build_model_table(SEASONS, ASOF_S, ASOF_W)

# per-game WP-state shares from the game-script cache
gs <- read_csv(GAMESCRIPT_GAMES_FILE, show_col_types = FALSE) %>%
  filter(season %in% SEASONS) %>%
  gamescript_state_shares() %>%
  select(game_id, team = posteam, all_of(GS_STATE_LABELS))

mt <- mt %>% left_join(gs, by = c("game_id", "team"))

# ── players.json ─────────────────────────────────────────────
players <- mt %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "FB")) %>%
  group_by(player_id) %>%
  summarise(
    name = last(player_display_name), pos = last(position),
    team = last(team), headshot = last(headshot_url),
    games = n(),
    latest_season = max(season),
    tgt_pg   = round(mean(targets[season == max(season)], na.rm = TRUE), 1),
    carry_pg = round(mean(carries[season == max(season)], na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  filter(tgt_pg + carry_pg > 0.5 | pos == "QB") %>%
  arrange(desc(tgt_pg + carry_pg))
write_json(players, file.path(WEB_DIR, "players.json"), auto_unbox = TRUE,
           na = "null")
cat(sprintf("[web-data] players.json: %d players\n", nrow(players)))

# ── defense.json ─────────────────────────────────────────────
pbp <- load_feature_pbp(SEASONS)
def <- build_epa_ranks(pbp, ASOF_S, ASOF_W) %>%
  left_join(build_plays_defense(pbp, ASOF_S, ASOF_W),
            by = c("team" = "defteam")) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3)))
write_json(def, file.path(WEB_DIR, "defense.json"), auto_unbox = TRUE,
           na = "null")

# ── teams/{TEAM}_{season}.json — target share matrix ─────────
for (s in SEASONS) for (tm in sort(unique(mt$team))) {
  tsm <- mt %>%
    filter(season == s, team == tm, targets > 0 | receptions > 0) %>%
    select(player = player_display_name, week, target_share) %>%
    mutate(target_share = round(target_share, 3)) %>%
    pivot_wider(names_from = week, values_from = target_share,
                names_sort = TRUE) %>%
    rowwise() %>%
    mutate(total = round(mean(c_across(-player), na.rm = TRUE), 3)) %>%
    ungroup() %>%
    arrange(desc(total))
  write_json(tsm, file.path(WEB_DIR, "teams",
                            sprintf("%s_%d.json", tm, s)),
             auto_unbox = TRUE, na = "null", digits = 4)
}
cat("[web-data] team target-share matrices written\n")

# ── gamelogs/{gsis_id}.json ──────────────────────────────────
r2 <- function(x) round(x, 2); r3 <- function(x) round(x, 3)
logs <- mt %>%
  filter(player_id %in% players$player_id) %>%
  transmute(
    player_id, season, week,
    opp = opponent, home = home,
    team_spread = r2(team_spread), total_line, implied = r2(implied_total),
    snap_pct = r3(offense_pct),
    team_plays = total_plays, pass_pct = r3(pass_perc), rush_pct = r3(rush_perc),
    q1 = r3(q1_trail_big), q2 = r3(q2_trail),
    q3 = r3(q3_lead),      q4 = r3(q4_lead_big),
    rush_att = carries, rush_yds = rushing_yards, ypc = r2(yards_per_carry),
    rush_share = r3(rush_share), long_rush = longest_rush,
    rush_td = rushing_tds,
    tgt = targets, rec = receptions, rec_yds = receiving_yards,
    rec_td = receiving_tds,
    air_yds = receiving_air_yards, unrl_ay = unrealized_air_yards,
    unrl_tgt = unrealized_targets, long_rec = longest_rec,
    rush_rec = r2(rush_plus_rec), yac = receiving_yards_after_catch,
    tgt_share = r3(target_share), ay_share = r3(air_yards_share),
    catch_pct = r3(catch_perc), ypt = r2(yards_per_target),
    adot = r2(adot), avg_yac = r2(avg_yac),
    ngs_sep = r2(avg_separation), ngs_cushion = r2(avg_cushion),
    # QB block
    pass_att = attempts, comp = completions, pass_yds = passing_yards,
    pass_td = passing_tds, ints = passing_interceptions,
    ypa = r2(qb_yards_per_attempt), cpoe = r2(passing_cpoe)
  ) %>%
  arrange(player_id, season, week)

n_written <- 0
for (pid in unique(logs$player_id)) {
  write_json(logs %>% filter(player_id == pid) %>% select(-player_id),
             file.path(WEB_DIR, "gamelogs", paste0(pid, ".json")),
             auto_unbox = TRUE, na = "null")
  n_written <- n_written + 1
}
cat(sprintf("[web-data] gamelogs: %d player files\n", n_written))

# ── meta.json ────────────────────────────────────────────────
write_json(list(
  seasons = SEASONS, generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  players = nrow(players),
  props = "placeholder — populated by scrape_props.R in season",
  projections = "placeholder — populated by the model layer"
), file.path(WEB_DIR, "meta.json"), auto_unbox = TRUE)
cat("[web-data] done ->", WEB_DIR, "\n")
