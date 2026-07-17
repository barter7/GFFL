# ============================================================
#  player_profiles.R  (nflmodel)
#
#  Player usage/efficiency profiles from pbp at three grains:
#  per GAME, per SEASON, and CAREER (= the cached 17-game era,
#  2021+; true careers extend earlier — documented limitation).
#  Season and career rates are recomputed from summed counts,
#  never averages-of-averages.
#
#  Definitions (rule R5.x / Study S13):
#   • carries = DESIGNED carries only: rush == 1 excludes QB
#     scrambles by construction (audited, S12). Team carry share
#     and the RB-only carry share both use designed carries.
#   • targets = pass attempts with a receiver id; air yards from
#     the throw. Throwaways (no receiver) are dropbacks but not
#     targets and are excluded from depth-band completion rates
#     (air_yards unknown).
#   • depth bands (air yards): BLOS < 0, short 0-4, mid 5-9,
#     deep 10-19, bomb 20+.
#   • population: official snaps (R1.10) — nullified plays are
#     excluded so counts reconcile with official weekly stats.
#   • QB rushing rows exist too (designed QB runs); scramble
#     production is tracked separately on the passing profile.
#
#  Outputs (data/players/, regenerable):
#    profile_game.csv.gz   one row per player-game
#    profile_season.csv    one row per player-season
#    profile_career.csv    one row per player (era career)
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr) })

PROFILE_GAME_FILE   <- file.path("data", "players", "profile_game.csv.gz")
PROFILE_SEASON_FILE <- file.path("data", "players", "profile_season.csv")
PROFILE_CAREER_FILE <- file.path("data", "players", "profile_career.csv")

DEPTH_BANDS <- c(-Inf, 0, 5, 10, 20, Inf)          # [ , ) cuts
DEPTH_LABS  <- c("blos", "d0_4", "d5_9", "d10_19", "d20p")

.profile_pbp <- function(seasons) {
  load_pbp_cache(seasons, cols = c(
    "game_id", "season", "week", "posteam", "qtr", "two_point_attempt",
    "play_type", "pass", "rush", "qb_scramble", "sack", "qb_dropback",
    "pass_attempt", "complete_pass", "interception",
    "air_yards", "yards_after_catch", "yards_gained",
    "passing_yards", "receiving_yards", "rushing_yards",
    "pass_touchdown", "rush_touchdown", "epa", "success", "cpoe",
    "yardline_100", "first_down",
    "passer_player_id", "receiver_player_id", "rusher_player_id"
  )) %>%
    filter(!is.na(posteam), play_type %in% c("pass", "run"),
           two_point_attempt == 0) %>%
    mutate(depth_band = cut(air_yards, DEPTH_BANDS, DEPTH_LABS,
                            right = FALSE))
}

# ── per player-GAME counting table ───────────────────────────
build_profile_game <- function(seasons) {
  pbp <- .profile_pbp(seasons)

  # positions per season (for the RB-only carry denominator)
  pos <- load_rosters_cache(seasons, skill_only = FALSE) %>%
    select(season, gsis_id, position)

  # -- rushing: designed carries only ------------------------
  ru <- pbp %>%
    filter(rush == 1, !is.na(rusher_player_id)) %>%
    left_join(pos, by = c("season", "rusher_player_id" = "gsis_id")) %>%
    group_by(season, week, game_id, team = posteam,
             player_id = rusher_player_id) %>%
    summarise(
      carries = n(), rush_yds = sum(rushing_yards, na.rm = TRUE),
      rush_td = sum(rush_touchdown), rush_fd = sum(first_down, na.rm = TRUE),
      rush_epa = sum(epa, na.rm = TRUE), rush_succ = sum(success, na.rm = TRUE),
      stuffs = sum(yards_gained <= 0), explosive_rush = sum(yards_gained >= 10),
      carries_i10 = sum(yardline_100 <= 10), carries_i5 = sum(yardline_100 <= 5),
      is_rb = first(position) == "RB",
      .groups = "drop")

  # -- receiving: targets ------------------------------------
  rc <- pbp %>%
    filter(pass_attempt == 1, sack == 0, !is.na(receiver_player_id)) %>%
    group_by(season, week, game_id, team = posteam,
             player_id = receiver_player_id) %>%
    summarise(
      targets = n(), receptions = sum(complete_pass),
      rec_yds = sum(receiving_yards, na.rm = TRUE),
      rec_td = sum(pass_touchdown),
      air_yds = sum(air_yards, na.rm = TRUE),
      yac = sum(yards_after_catch, na.rm = TRUE),
      rec_fd = sum(first_down * complete_pass, na.rm = TRUE),
      rec_epa = sum(epa, na.rm = TRUE),
      tgt_rz = sum(yardline_100 <= 20),
      tgt_ez = sum(!is.na(air_yards) & air_yards >= yardline_100),
      # depth-band counters
      !!!setNames(lapply(DEPTH_LABS, function(b)
        quo(sum(depth_band == !!b, na.rm = TRUE))), paste0("tgt_", DEPTH_LABS)),
      !!!setNames(lapply(DEPTH_LABS, function(b)
        quo(sum(complete_pass[depth_band == !!b], na.rm = TRUE))),
        paste0("rec_", DEPTH_LABS)),
      .groups = "drop")

  # -- passing: dropbacks ------------------------------------
  # scrambles carry the QB in rusher_player_id (passer id is NA),
  # so they are attributed separately and joined back in
  scr <- pbp %>%
    filter(qb_scramble == 1, !is.na(rusher_player_id)) %>%
    group_by(season, week, game_id, team = posteam,
             player_id = rusher_player_id) %>%
    summarise(scrambles = n(),
              scramble_yds = sum(yards_gained, na.rm = TRUE),
              .groups = "drop")
  qb <- pbp %>%
    filter(qb_dropback == 1, !is.na(passer_player_id)) %>%
    group_by(season, week, game_id, team = posteam,
             player_id = passer_player_id) %>%
    summarise(
      dropbacks_thrown = n(),
      attempts = sum(pass_attempt == 1 & sack == 0),
      completions = sum(complete_pass),
      pass_yds = sum(passing_yards, na.rm = TRUE),
      pass_td = sum(pass_touchdown), ints = sum(interception),
      pass_air = sum(air_yards, na.rm = TRUE),
      pass_yac = sum(yards_after_catch, na.rm = TRUE),
      sacks = sum(sack),
      cpoe_mean = mean(cpoe, na.rm = TRUE),
      pass_epa = sum(epa, na.rm = TRUE),
      !!!setNames(lapply(DEPTH_LABS, function(b)
        quo(sum(pass_attempt == 1 & sack == 0 & depth_band == !!b,
                na.rm = TRUE))), paste0("att_", DEPTH_LABS)),
      !!!setNames(lapply(DEPTH_LABS, function(b)
        quo(sum(complete_pass[depth_band == !!b], na.rm = TRUE))),
        paste0("cmp_", DEPTH_LABS)),
      .groups = "drop")

  # -- team denominators per team-game -----------------------
  teams <- pbp %>%
    group_by(season, week, game_id, team = posteam) %>%
    summarise(
      team_carries = sum(rush == 1),
      team_targets = sum(pass_attempt == 1 & sack == 0 &
                           !is.na(receiver_player_id)),
      team_air     = sum(air_yards[pass_attempt == 1 & sack == 0],
                         na.rm = TRUE),
      team_plays   = sum(rush == 1 | pass == 1),
      .groups = "drop")
  rb_car <- ru %>% filter(is_rb) %>%
    group_by(season, week, game_id, team) %>%
    summarise(team_rb_carries = sum(carries), .groups = "drop")

  full_join(ru %>% select(-is_rb), rc,
            by = c("season","week","game_id","team","player_id")) %>%
    full_join(qb,  by = c("season","week","game_id","team","player_id")) %>%
    full_join(scr, by = c("season","week","game_id","team","player_id")) %>%
    mutate(dropbacks = coalesce(dropbacks_thrown, 0) + coalesce(scrambles, 0)) %>%
    left_join(teams,  by = c("season","week","game_id","team")) %>%
    left_join(rb_car, by = c("season","week","game_id","team")) %>%
    mutate(across(where(is.numeric), \(x) replace_na(x, 0)),
           cpoe_mean = na_if(cpoe_mean, 0)) %>%
    left_join(pos, by = c("season", "player_id" = "gsis_id"))
}

# ── rates from a summed block (shared by all grains) ─────────
.add_rates <- function(d) {
  sdiv <- function(a, b) ifelse(b > 0, a / b, NA_real_)
  d %>% mutate(
    ypc            = sdiv(rush_yds, carries),
    carry_share    = sdiv(carries, team_carries),
    rb_carry_share = ifelse(position == "RB",
                            sdiv(carries, team_rb_carries), NA_real_),
    rush_epa_att   = sdiv(rush_epa, carries),
    rush_succ_rate = sdiv(rush_succ, carries),
    stuff_rate     = sdiv(stuffs, carries),
    explosive_rate = sdiv(explosive_rush, carries),
    tgt_share      = sdiv(targets, team_targets),
    ay_share       = sdiv(air_yds, team_air),
    catch_pct      = sdiv(receptions, targets),
    ypr            = sdiv(rec_yds, receptions),
    ypt            = sdiv(rec_yds, targets),
    adot           = sdiv(air_yds, targets),        # air yards / target
    ay_per_rec     = sdiv(air_yds, receptions),
    yac_per_rec    = sdiv(yac, receptions),
    rec_epa_tgt    = sdiv(rec_epa, targets),
    rz_tgt_share   = sdiv(tgt_rz, targets),
    comp_pct       = sdiv(completions, attempts),
    ypa            = sdiv(pass_yds, attempts),
    pass_adot      = sdiv(pass_air, attempts),
    sack_rate      = sdiv(sacks, dropbacks),
    scramble_rate  = sdiv(scrambles, dropbacks),
    int_rate       = sdiv(ints, attempts),
    pass_td_rate   = sdiv(pass_td, attempts),
    opportunity_share = sdiv(carries + targets, team_plays)
  ) %>%
    { d2 <- .
      for (b in DEPTH_LABS) {
        d2[[paste0("catch_pct_", b)]] <- sdiv(d2[[paste0("rec_", b)]],
                                              d2[[paste0("tgt_", b)]])
        d2[[paste0("comp_pct_", b)]]  <- sdiv(d2[[paste0("cmp_", b)]],
                                              d2[[paste0("att_", b)]])
        d2[[paste0("tgt_mix_", b)]]   <- sdiv(d2[[paste0("tgt_", b)]],
                                              d2[["targets"]])
      }
      d2 } %>%
    mutate(across(where(is.numeric), \(x) round(x, 4)))
}

# ── grain aggregations ───────────────────────────────────────
.count_cols <- function(d) setdiff(
  names(d)[sapply(d, is.numeric)],
  c("season", "week"))

aggregate_profile <- function(game_tbl, by) {
  cc <- .count_cols(game_tbl)
  game_tbl %>%
    group_by(across(all_of(by))) %>%
    summarise(games = n(),
              position = last(position),
              teams = paste(unique(team), collapse = "/"),
              across(all_of(setdiff(cc, "cpoe_mean")), sum),
              cpoe_mean = mean(cpoe_mean, na.rm = TRUE),
              .groups = "drop") %>%
    .add_rates()
}

refresh_player_profiles <- function(seasons = NULL) {
  if (is.null(seasons)) seasons <- as.integer(gsub("\\D", "", basename(
    list.files(PBP_DIR, pattern = "^play_by_play_\\d{4}\\.csv\\.gz$"))))
  g <- build_profile_game(seasons)
  names_lu <- load_rosters_cache(seasons, skill_only = FALSE) %>%
    group_by(gsis_id) %>% summarise(player = last(full_name))
  gg <- g %>% .add_rates() %>% left_join(names_lu, by = c("player_id" = "gsis_id"))
  write_csv(gg, PROFILE_GAME_FILE)
  ss <- aggregate_profile(g, c("player_id", "season")) %>%
    left_join(names_lu, by = c("player_id" = "gsis_id"))
  write_csv(ss, PROFILE_SEASON_FILE)
  kk <- aggregate_profile(g %>% mutate(era = "2021+"), c("player_id", "era")) %>%
    left_join(names_lu, by = c("player_id" = "gsis_id"))
  write_csv(kk, PROFILE_CAREER_FILE)
  message(sprintf("  profiles: %d player-games, %d player-seasons, %d careers",
                  nrow(gg), nrow(ss), nrow(kk)))
  invisible(list(game = gg, season = ss, career = kk))
}
