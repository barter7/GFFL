# ============================================================
#  depth_sheet.R  (nflmodel)
#
#  2026 depth charts by position group, with the offseason
#  churn shown IN PLACE: who is new, who left, and how good
#  each of them actually was (rule R2.7).
#
#  For every team x position group:
#    - the 2026 depth chart (latest snapshot, ranked)
#    - a status per player: ROOKIE (2026 draftee) / NEW (was on
#      another team in 2025) / returning
#    - a value line for 2025 and for career (2021-2025)
#    - DEPARTURES listed underneath: 2025 contributors at that
#      group who are not on the 2026 roster
#
#  QUALITY METRICS — what we can and cannot show:
#    PFF grades are proprietary and appear in NO nflverse
#    dataset (R1.1). This script therefore reports nflverse
#    production metrics by position, and joins REAL PFF grades
#    when the owner drops exports into data/pff/ (see
#    load_pff_grades() for the expected schema). Offensive line
#    is the position group where nflverse has essentially no
#    per-player signal (snaps only) — that gap is labeled in the
#    output rather than papered over.
#
#  Output: data/context/depth_sheet_2026.json (+ .csv)
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr); library(jsonlite) })
source("utils.R")
source("injury_profile.R")

SEASONS_CAREER <- 2021:2025
DEPTH_JSON <- file.path("data", "context", "depth_sheet_2026.json")

POS_GROUP <- c(
  QB = "QB", RB = "RB", FB = "RB", WR = "WR", TE = "TE",
  LT = "OL", LG = "OL", C = "OL", RG = "OL", RT = "OL",
  LDE = "DL", RDE = "DL", LDT = "DL", RDT = "DL", NT = "DL",
  LILB = "LB", RILB = "LB", MLB = "LB", SLB = "LB", WLB = "LB",
  LCB = "DB", RCB = "DB", NB = "DB", FS = "DB", SS = "DB",
  PK = "ST", P = "ST", LS = "ST", H = "ST", KR = "ST", PR = "ST")
GROUP_ORDER <- c("QB","RB","WR","TE","OL","DL","LB","DB")
GROUP_KEEP  <- c(QB=3, RB=4, WR=6, TE=3, OL=6, DL=5, LB=4, DB=6)

# roster position -> group (departures use roster position)
ROSTER_GROUP <- function(p) {
  p <- toupper(p)
  case_when(
    p %in% c("QB") ~ "QB", p %in% c("RB","FB","HB") ~ "RB",
    p %in% c("WR") ~ "WR", p %in% c("TE") ~ "TE",
    p %in% c("T","G","C","OL","OT","OG") ~ "OL",
    p %in% c("DE","DT","NT","DL") ~ "DL",
    p %in% c("LB","ILB","OLB","MLB") ~ "LB",
    p %in% c("CB","S","FS","SS","DB") ~ "DB",
    TRUE ~ "ST")
}

# ── optional PFF grade drop-in ───────────────────────────────
#  Expected: any CSV in data/pff/ with a player id column
#  (`pff_id` or `player_id`) plus grade columns. Season grades
#  join on (pff_id, season); a file with no season column is
#  treated as career grades. Typical PFF season-summary exports
#  already carry: grades_offense, grades_pass, grades_run,
#  grades_pass_block, grades_defense, grades_run_defense,
#  grades_pass_rush_defense, grades_coverage_defense.
load_pff_grades <- function() {
  files <- list.files(file.path("data", "pff"), pattern = "\\.csv$",
                      full.names = TRUE)
  if (!length(files)) return(NULL)
  raw <- bind_rows(lapply(files, function(f) {
    d <- suppressWarnings(read_csv(f, show_col_types = FALSE,
                                   col_types = cols(.default = col_guess())))
    if (!"pff_id" %in% names(d) && "player_id" %in% names(d))
      d <- d %>% rename(pff_id = player_id)
    d
  }))
  if (!"pff_id" %in% names(raw)) return(NULL)
  gcols <- grep("^grade", names(raw), value = TRUE)
  if (!length(gcols)) return(NULL)
  raw %>% mutate(pff_id = as.character(pff_id)) %>%
    select(pff_id, any_of("season"), all_of(gcols))
}

# ── player value lines from nflverse ─────────────────────────
build_player_values <- function() {
  # read ONLY the needed columns: unrelated all-NA columns type
  # differently across seasons and break bind_rows (see S13)
  keep <- c("player_id","season","attempts","passing_yards","passing_tds",
            "passing_interceptions","passing_epa","carries","rushing_yards",
            "rushing_tds","rushing_epa","targets","receptions","receiving_yards",
            "receiving_tds","receiving_epa","def_tackles_solo",
            "def_tackle_assists","def_tackles_for_loss","def_sacks",
            "def_qb_hits","def_pass_defended","def_interceptions")
  st <- bind_rows(lapply(SEASONS_CAREER, function(s) {
    p <- file.path("data", "players", sprintf("stats_player_week_%d.csv", s))
    if (!file.exists(p)) return(NULL)
    read_csv(p, show_col_types = FALSE, guess_max = 20000,
             col_select = any_of(keep)) %>%
      mutate(across(-c(player_id), as.numeric))
  }))
  agg <- function(d) d %>% summarise(
    g = n(),
    att = sum(attempts, na.rm=TRUE), pass_yds = sum(passing_yards, na.rm=TRUE),
    pass_td = sum(passing_tds, na.rm=TRUE), ints = sum(passing_interceptions, na.rm=TRUE),
    pass_epa = sum(passing_epa, na.rm=TRUE),
    car = sum(carries, na.rm=TRUE), rush_yds = sum(rushing_yards, na.rm=TRUE),
    rush_td = sum(rushing_tds, na.rm=TRUE), rush_epa = sum(rushing_epa, na.rm=TRUE),
    tgt = sum(targets, na.rm=TRUE), rec = sum(receptions, na.rm=TRUE),
    rec_yds = sum(receiving_yards, na.rm=TRUE), rec_td = sum(receiving_tds, na.rm=TRUE),
    rec_epa = sum(receiving_epa, na.rm=TRUE),
    tkl = sum(def_tackles_solo, na.rm=TRUE) + sum(def_tackle_assists, na.rm=TRUE),
    tfl = sum(def_tackles_for_loss, na.rm=TRUE), sk = sum(def_sacks, na.rm=TRUE),
    qbh = sum(def_qb_hits, na.rm=TRUE), pd = sum(def_pass_defended, na.rm=TRUE),
    dint = sum(def_interceptions, na.rm=TRUE),
    .groups = "drop")
  last <- st %>% filter(season == 2025) %>% group_by(player_id) %>% agg()
  car  <- st %>% group_by(player_id) %>% agg()
  snaps <- read_csv(file.path("data","players","snap_counts_2025.csv"),
                    show_col_types = FALSE) %>%
    group_by(pfr_player_id) %>%
    summarise(snaps25 = sum(offense_snaps + defense_snaps, na.rm=TRUE), .groups="drop")
  list(last = last, career = car, snaps = snaps)
}

fmt_line <- function(grp, v) {
  r1 <- function(x) round(x, 1); r2 <- function(x) round(x, 2)
  ifelse(is.na(v$g) | v$g == 0, NA_character_, dplyr::case_when(
    grp == "QB" ~ sprintf("%d att | %s yd | %d/%d TD-INT | %s EPA",
                          v$att, format(v$pass_yds, big.mark=","), v$pass_td,
                          v$ints, r1(v$pass_epa)),
    grp == "RB" ~ sprintf("%d car | %s yd | %s ypc | %d TD | %s tgt",
                          v$car, format(v$rush_yds, big.mark=","),
                          r2(ifelse(v$car>0, v$rush_yds/v$car, NA)), v$rush_td, v$tgt),
    grp %in% c("WR","TE") ~ sprintf("%d tgt | %d rec | %s yd | %s ypt | %d TD",
                          v$tgt, v$rec, format(v$rec_yds, big.mark=","),
                          r2(ifelse(v$tgt>0, v$rec_yds/v$tgt, NA)), v$rec_td),
    grp %in% c("DL","LB","DB") ~ sprintf("%d tkl | %s TFL | %s sk | %d QBH | %d PD | %d INT",
                          v$tkl, r1(v$tfl), r1(v$sk), v$qbh, v$pd, v$dint),
    TRUE ~ NA_character_))
}

build_depth_sheet <- function() {
  dc <- read_csv(file.path("data","rosters","depth_charts_2026.csv"),
                 show_col_types = FALSE) %>%
    group_by(team) %>% filter(dt == max(dt)) %>% ungroup() %>%
    mutate(team = normalize_team(team), grp = POS_GROUP[pos_abb]) %>%
    filter(!is.na(grp), grp %in% GROUP_ORDER, !is.na(gsis_id)) %>%
    group_by(team, grp, gsis_id, player_name) %>%
    summarise(rank = min(pos_rank), spot = first(pos_abb), .groups = "drop") %>%
    group_by(team, grp) %>% arrange(rank, .by_group = TRUE) %>%
    mutate(slot = row_number()) %>%
    filter(slot <= GROUP_KEEP[grp]) %>% ungroup()

  r25 <- read_csv(file.path("data","players","roster_2025.csv"), show_col_types=FALSE) %>%
    filter(!is.na(gsis_id)) %>%
    transmute(gsis_id, name25 = full_name, pos25 = position,
              team25 = normalize_team(team), pff_id = as.character(pff_id),
              pfr_id) %>% distinct(gsis_id, .keep_all = TRUE)
  r26 <- read_csv(file.path("data","players","roster_2026.csv"), show_col_types=FALSE) %>%
    filter(!is.na(gsis_id), status %in% c("ACT","RES")) %>%
    transmute(gsis_id, team26 = normalize_team(team), pos26 = position,
              yrs26 = suppressWarnings(as.numeric(years_exp)),
              nm26 = full_name,
              pff26 = as.character(pff_id)) %>% distinct(gsis_id, .keep_all=TRUE)
  draft <- read_csv(file.path("data","context","draft_picks_2026.csv"), show_col_types=FALSE)
  # ID-FIRST (R1.13). The draft release's `gsis_id` column is NOT
  # nflverse gsis format ("MEN516487" vs "00-00xxxxx") and matches
  # nothing — `pfr_player_id` is the valid bridge to roster pfr_id.
  # Name matching is the documented fallback for the tail of the
  # class that has no PFR page yet (S19).
  nkey <- function(x) gsub("[^a-z]", "", tolower(x))
  rookie_id <- draft %>%
    filter(!is.na(pfr_player_id)) %>%
    transmute(pfr_id = pfr_player_id, dr_round = round, dr_pick = pick) %>%
    distinct(pfr_id, .keep_all = TRUE)
  rookie_nm <- draft %>%
    transmute(nk = nkey(pfr_player_name), dr_round_nm = round,
              dr_pick_nm = pick) %>%
    distinct(nk, .keep_all = TRUE)

  V <- build_player_values(); pff <- load_pff_grades()
  INJ <- build_injury_profile() %>%
    select(gsis_id, cur_status, cur_code, avail_note, cur_available,
           y25_weeks_out_injury, y25_weeks_q, y25_top_injury,
           car_weeks_out_injury, car_seasons_seen)

  attach_val <- function(d) {
    d <- d %>%
      left_join(V$last %>% rename_with(~paste0("l_", .x), -player_id),
                by = c("gsis_id" = "player_id")) %>%
      left_join(V$career %>% rename_with(~paste0("c_", .x), -player_id),
                by = c("gsis_id" = "player_id")) %>%
      { if ("pfr_id" %in% names(.)) . else
          left_join(., r25 %>% select(gsis_id, pfr_id), by = "gsis_id") } %>%
      left_join(V$snaps, by = c("pfr_id" = "pfr_player_id")) %>%
      left_join(INJ, by = "gsis_id")   # availability layer (R2.8)
    lv <- d %>% select(starts_with("l_")) %>% rename_with(~sub("^l_","",.x))
    cv <- d %>% select(starts_with("c_")) %>% rename_with(~sub("^c_","",.x))
    d$line_2025   <- fmt_line(d$grp, lv)
    d$line_career <- fmt_line(d$grp, cv)
    d %>% select(-starts_with("l_"), -starts_with("c_"))
  }

  depth <- dc %>%
    left_join(r25 %>% select(gsis_id, team25, pff_id), by = "gsis_id") %>%
    left_join(r26 %>% select(gsis_id, yrs26), by = "gsis_id") %>%
    left_join(r25 %>% select(gsis_id, pfr_id), by = "gsis_id") %>%
    left_join(rookie_id, by = "pfr_id") %>%          # ID path first
    mutate(nk = nkey(player_name)) %>%
    left_join(rookie_nm, by = "nk") %>%              # name fallback only
    mutate(dr_round = coalesce(dr_round, dr_round_nm),
           dr_pick  = coalesce(dr_pick,  dr_pick_nm)) %>%
    mutate(status = case_when(coalesce(yrs26, 99) == 0 ~ "ROOKIE",
                              is.na(team25) ~ "NEW",
                              team25 != team ~ "NEW",
                              TRUE ~ "RET"),
           from = case_when(
             status == "NEW" & !is.na(team25) ~ team25,
             status == "ROOKIE" & !is.na(dr_round) ~ paste0("R", dr_round,
                                                            ".", dr_pick),
             status == "ROOKIE" ~ "UDFA",
             TRUE ~ NA_character_)) %>%
    attach_val()

  # departures: 2025 contributors gone from the 2026 roster
  out <- r25 %>%
    left_join(r26, by = "gsis_id") %>%
    left_join(V$snaps, by = c("pfr_id" = "pfr_player_id")) %>%
    filter(is.na(team26) | team26 != team25) %>%
    filter(coalesce(snaps25, 0) >= 250) %>%
    # snaps25 intentionally dropped here — attach_val() re-joins it
    transmute(team = team25, grp = ROSTER_GROUP(pos25), gsis_id,
              player_name = name25, spot = pos25, pff_id,
              to = coalesce(team26, "FA/none")) %>%
    filter(grp %in% GROUP_ORDER) %>%
    attach_val()

  if (!is.null(pff)) {
    # ID-first (R1.13): join on pff_id, then fill ONLY the rows it
    # missed by normalized name. nflverse carries pff_id for ~70%
    # of veterans and 0 of 682 rookies, so the name pass is
    # required for the class — it is a labeled fallback, not the
    # primary path.
    pg <- pff %>% filter(is.na(season) | season == 2025)
    gcols <- grep("^grade", names(pg), value = TRUE)
    by_nm <- if ("player" %in% names(pg) || "name" %in% names(pg)) {
      nmcol <- if ("player" %in% names(pg)) "player" else "name"
      pg %>% transmute(nk = nkey(.data[[nmcol]]),
                       across(all_of(gcols))) %>%
        rename_with(~paste0(.x, "_nm"), all_of(gcols)) %>%
        distinct(nk, .keep_all = TRUE)
    } else NULL
    j <- function(d) {
      d <- d %>% left_join(pg %>% select(pff_id, all_of(gcols)), by = "pff_id")
      if (!is.null(by_nm)) {
        d <- d %>% mutate(.nk = nkey(player_name)) %>%
          left_join(by_nm, by = c(".nk" = "nk"))
        for (g in gcols) d[[g]] <- dplyr::coalesce(d[[g]], d[[paste0(g,"_nm")]])
        d <- d %>% select(-.nk, -ends_with("_nm"))
      }
      d
    }
    depth <- j(depth); out <- j(out)
  }
  list(depth = depth, out = out, has_pff = !is.null(pff))
}

if (sys.nframe() == 0) {
  s <- build_depth_sheet()
  write_json(list(
    depth = s$depth %>% select(team, grp, slot, spot, player_name, status, from,
                               snaps25, line_2025, line_career,
                               cur_status, avail_note, y25_weeks_out_injury,
                               y25_weeks_q, y25_top_injury, car_weeks_out_injury,
                               any_of(grep("^grade", names(s$depth), value=TRUE))),
    out = s$out %>% select(team, grp, player_name, spot, to, snaps25,
                           line_2025, line_career, y25_weeks_out_injury,
                           y25_top_injury,
                           any_of(grep("^grade", names(s$out), value=TRUE))),
    has_pff = s$has_pff,
    generated = format(Sys.Date())),
    DEPTH_JSON, auto_unbox = TRUE, na = "null")
  message(sprintf("  depth sheet: %d depth rows, %d departures, PFF=%s -> %s",
                  nrow(s$depth), nrow(s$out), s$has_pff, DEPTH_JSON))
}
