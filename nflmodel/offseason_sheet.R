# ============================================================
#  offseason_sheet.R  (nflmodel)
#
#  2026 offseason cheat sheet, one block per team, built ONLY
#  from nflverse data already in the cache (rule R2.6):
#
#    HC change      games.csv 2025 vs 2026 coach fields
#                   (2026 schedule ships with coaches attached)
#    QB change      2025 primary starter (most starts in
#                   games.csv qb_name) vs 2026 QB1 (latest
#                   depth-chart snapshot)
#    Veteran adds   players on the 2026 roster whose 2025 roster
#                   team differs, excluding 2026 draftees,
#                   ranked by 2025 snaps. NOTE: rosters cannot
#                   distinguish a free-agent signing from a
#                   trade — labeled "veteran additions".
#    Departures     mirror: 2025 contributors no longer on the
#                   2026 roster of that team
#    Draft          2026 draft_picks release, all rounds, with
#                   QB/RB/WR/TE in rounds 1-3 flagged
#
#  OC/DC: no nflverse dataset publishes coordinators (R2.2).
#  This script reports what is DERIVABLE — whether the 2025
#  OC/play-caller vacated (took an HC job elsewhere) and whether
#  the HC changed (which nearly always forces staff turnover) —
#  and leaves the incoming names to data/context/coordinators.csv,
#  the same hand-maintained drop-in the model already reads.
#
#  Usage: Rscript offseason_sheet.R   ->  data/context/offseason_2026.csv
#                                          docs/OFFSEASON_2026.md
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr); library(purrr) })
source("utils.R")
source("game_context.R")   # GAMES_CSV_URL / schedule conventions

SEASON_NEW <- 2026L; SEASON_OLD <- 2025L
OUT_CSV <- file.path("data", "context", "offseason_2026.csv")
OUT_MD  <- file.path("docs", "OFFSEASON_2026.md")

# PFR-style draft codes -> nflverse
PFR_MAP <- c(GNB="GB", KAN="KC", LAR="LA", LVR="LV", NOR="NO",
             NWE="NE", SFO="SF", TAM="TB")
fix_team <- function(x) { x <- toupper(x); ifelse(x %in% names(PFR_MAP),
                                                 PFR_MAP[x], x) }

# coach-name matching tolerant of source typos (see R2.6)
name_key <- function(x) tolower(gsub("[^a-z]", "", tolower(as.character(x))))
name_in_fuzzy <- function(x, pool, max_dist = 2) {
  vapply(x, function(v) {
    if (is.na(v)) return(FALSE)
    any(as.numeric(adist(name_key(v), name_key(pool))) <= max_dist)
  }, logical(1))
}

build_offseason_sheet <- function() {
  games <- read_csv(GAMES_CSV_URL, show_col_types = FALSE, progress = FALSE)

  # ── head coaches ──────────────────────────────────────────
  hc <- bind_rows(
    games %>% transmute(season, team = home_team, coach = home_coach),
    games %>% transmute(season, team = away_team, coach = away_coach)) %>%
    filter(season %in% c(SEASON_OLD, SEASON_NEW), !is.na(coach)) %>%
    mutate(team = normalize_team(team)) %>%
    count(season, team, coach) %>%
    group_by(season, team) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>% select(-n) %>%
    pivot_wider(names_from = season, values_from = coach,
                names_prefix = "hc_")

  # ── starting QBs ──────────────────────────────────────────
  qb25 <- bind_rows(
    games %>% transmute(season, team = home_team, qb = home_qb_name),
    games %>% transmute(season, team = away_team, qb = away_qb_name)) %>%
    filter(season == SEASON_OLD, !is.na(qb)) %>%
    mutate(team = normalize_team(team)) %>%
    count(team, qb, name = "starts") %>%
    group_by(team) %>%
    summarise(qb_2025 = qb[which.max(starts)],
              qb_2025_starts = max(starts),
              qb_2025_all = paste0(qb, " (", starts, ")", collapse = ", "),
              .groups = "drop")

  dc26 <- read_csv(file.path("data", "rosters", "depth_charts_2026.csv"),
                   show_col_types = FALSE)
  qb26 <- dc26 %>%
    filter(pos_abb == "QB") %>%
    group_by(team) %>% filter(dt == max(dt)) %>%
    filter(pos_rank <= 2) %>% ungroup() %>%
    mutate(team = normalize_team(team)) %>%
    distinct(team, pos_rank, player_name) %>%
    pivot_wider(names_from = pos_rank, values_from = player_name,
                names_prefix = "qb26_r")

  # ── rosters: veteran additions & departures ───────────────
  r25 <- read_csv(file.path("data", "players", "roster_2025.csv"),
                  show_col_types = FALSE) %>%
    filter(!is.na(gsis_id)) %>%
    transmute(gsis_id, name = full_name, pos = position,
              team25 = normalize_team(team)) %>% distinct(gsis_id, .keep_all = TRUE)
  r26 <- read_csv(file.path("data", "players", "roster_2026.csv"),
                  show_col_types = FALSE) %>%
    filter(!is.na(gsis_id), status %in% c("ACT", "RES")) %>%
    transmute(gsis_id, name = full_name, pos = position,
              team26 = normalize_team(team)) %>% distinct(gsis_id, .keep_all = TRUE)

  snaps25 <- read_csv(file.path("data", "players", "snap_counts_2025.csv"),
                      show_col_types = FALSE) %>%
    group_by(pfr_player_id) %>%
    summarise(snaps = sum(offense_snaps + defense_snaps, na.rm = TRUE),
              gp = n(), .groups = "drop")
  xwalk <- read_csv(file.path("data", "players", "roster_2025.csv"),
                    show_col_types = FALSE) %>%
    select(gsis_id, pfr_id) %>% filter(!is.na(gsis_id), !is.na(pfr_id)) %>%
    distinct(gsis_id, .keep_all = TRUE)
  usage <- snaps25 %>% inner_join(xwalk, by = c("pfr_player_id" = "pfr_id")) %>%
    select(gsis_id, snaps, gp)

  draft <- read_csv(file.path("data", "context", "draft_picks_2026.csv"),
                    show_col_types = FALSE)
  rookie_ids <- draft$gsis_id[!is.na(draft$gsis_id)]

  moves <- r26 %>%
    inner_join(r25 %>% select(gsis_id, team25), by = "gsis_id") %>%
    filter(team26 != team25, !gsis_id %in% rookie_ids) %>%
    left_join(usage, by = "gsis_id") %>%
    mutate(snaps = replace_na(snaps, 0))

  adds <- moves %>% filter(snaps >= 150) %>%
    arrange(team26, desc(snaps)) %>%
    group_by(team = team26) %>%
    summarise(adds = paste0(name, " (", pos, ", from ", team25, ")",
                            collapse = "; "), .groups = "drop")
  losses <- moves %>% filter(snaps >= 300) %>%
    arrange(team25, desc(snaps)) %>%
    group_by(team = team25) %>%
    summarise(losses = paste0(name, " (", pos, " -> ", team26, ")",
                              collapse = "; "), .groups = "drop")

  # ── draft ─────────────────────────────────────────────────
  dr <- draft %>%
    mutate(team = fix_team(team),
           skill_early = round <= 3 & position %in% c("QB","RB","WR","TE")) %>%
    arrange(team, pick)
  draft_tbl <- dr %>%
    group_by(team) %>%
    summarise(n_picks = n(),
              picks = paste0("R", round, ".", pick, " ", pfr_player_name,
                             " (", position, ")", collapse = "; "),
              skill_picks = paste0(na.omit(ifelse(skill_early,
                             paste0("R", round, " ", pfr_player_name,
                                    " (", position, ")"), NA)),
                             collapse = "; "),
              .groups = "drop")

  # ── coordinator context (derivable only) ──────────────────
  coord25 <- read_csv(file.path("data", "context", "coordinators.csv"),
                      show_col_types = FALSE) %>%
    filter(season == SEASON_OLD) %>%
    group_by(team = normalize_team(team)) %>%
    slice_max(from_week, n = 1, with_ties = FALSE) %>%
    ungroup() %>% select(team, oc_2025 = oc_name, pc_2025 = play_caller)

  hc %>%
    left_join(qb25, by = "team") %>% left_join(qb26, by = "team") %>%
    left_join(coord25, by = "team") %>%
    left_join(adds, by = "team") %>% left_join(losses, by = "team") %>%
    left_join(draft_tbl, by = "team") %>%
    mutate(
      hc_change = hc_2025 != hc_2026,
      qb_change = !is.na(qb26_r1) & qb26_r1 != qb_2025,
      # OC vacancy is only DERIVABLE when the 2025 OC-of-record
      # shows up as a 2026 head coach somewhere (and wasn't
      # already a head coach in 2025 — HCs who call their own
      # plays must not trip this)
      # fuzzy (edit distance <= 2) because the schedule file
      # carries typos — e.g. "Klint Kubliak" for Kubiak, which
      # silently broke the exact match (Study S17)
      oc_vacated = name_in_fuzzy(oc_2025, na.omit(unique(hc_2026))) &
                   !name_in_fuzzy(oc_2025, na.omit(unique(hc_2025))),
      oc_status = case_when(
        oc_vacated ~ "CHANGED (2025 OC/PC took an HC job)",
        hc_change  ~ "Change likely (new HC)",
        TRUE       ~ "Unknown - needs coordinators.csv"),
      dc_status = ifelse(hc_change, "Change likely (new HC)",
                         "Unknown - needs coordinators.csv")
    ) %>%
    arrange(team)
}

write_offseason_md <- function(sheet, path = OUT_MD) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  yn <- function(x) ifelse(isTRUE(x), "**CHANGED**", "same")
  blocks <- pmap_chr(sheet, function(...) {
    r <- list(...)
    paste0(
      "### ", r$team, "\n\n",
      "| | 2025 | 2026 |\n|---|---|---|\n",
      "| Head coach | ", r$hc_2025, " | ", r$hc_2026, " — ", yn(r$hc_change), " |\n",
      "| QB1 | ", r$qb_2025, " (", r$qb_2025_starts, " st) | ",
        coalesce(r$qb26_r1, "?"), " — ", yn(r$qb_change),
        ifelse(is.na(r$qb26_r2), "", paste0(" *(QB2 ", r$qb26_r2, ")*")), " |\n",
      "| OC | ", coalesce(r$oc_2025, "?"),
        ifelse(is.na(r$pc_2025) | identical(r$pc_2025, r$oc_2025), "",
               paste0(" *(PC: ", r$pc_2025, ")*")),
        " | ", r$oc_status, " |\n",
      "| DC | *not in data* | ", r$dc_status, " |\n\n",
      "**Draft (", coalesce(as.character(r$n_picks), "0"), " picks)**",
      ifelse(is.na(r$skill_picks) | r$skill_picks == "", "",
             paste0(" — early skill: **", r$skill_picks, "**")), "  \n",
      coalesce(r$picks, "—"), "\n\n",
      "**Veteran additions** (150+ 2025 snaps): ", coalesce(r$adds, "none"), "  \n",
      "**Notable departures** (300+ 2025 snaps): ", coalesce(r$losses, "none"), "\n")
  })
  writeLines(c(
    "# 2026 Offseason Cheat Sheet",
    "",
    paste0("Generated ", format(Sys.Date()), " by `offseason_sheet.R` from cached",
           " nflverse data (2026 schedule coach fields, Aug-2 depth-chart",
           " snapshot, 2026 rosters, 2026 draft class, our coordinators table)."),
    "",
    "**Read this first — what is and isn't verified:**",
    "",
    "- **Head coach, QB, draft, roster moves: from data.** HCs come from the",
    "  2026 schedule file; QB1/QB2 from the latest 2026 depth-chart snapshot;",
    "  picks from the 2026 draft release; roster moves by diffing 2025 and 2026",
    "  rosters.",
    "- **Roster diffs cannot tell a signing from a trade** — both appear as a",
    "  team change, so they are labeled \"veteran additions\".",
    "- **Depth charts in early August are camp depth charts.** A listed QB1 is",
    "  a snapshot, not a Week 1 guarantee.",
    "- **OC/DC names are NOT in any nflverse dataset (R2.2).** This sheet says",
    "  only what is derivable — whether a 2025 OC left to become a head coach,",
    "  and whether a new HC makes staff turnover near-certain. Incoming",
    "  coordinator names must come from `data/context/coordinators.csv`.",
    "",
    "---",
    "",
    blocks), path)
  message("  wrote ", path)
}

if (sys.nframe() == 0) {
  sheet <- build_offseason_sheet()
  dir.create("docs", showWarnings = FALSE)
  write_csv(sheet, OUT_CSV)
  write_offseason_md(sheet)
  message(sprintf("  offseason sheet: %d teams -> %s", nrow(sheet), OUT_CSV))
}
