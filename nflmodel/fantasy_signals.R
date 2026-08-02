# ============================================================
#  fantasy_signals.R  (nflmodel)
#
#  Per-player fantasy-stock signals for skill positions
#  (rule R2.10). Each signal carries a DIRECTION (up / down /
#  watch), a short label, and the evidence behind it — usually
#  the other player's actual 2025 production, because "they
#  signed a WR" means nothing without knowing whether that WR
#  had 30 targets or 130.
#
#  Signals produced per QB/RB/WR/TE on a 2026 depth chart:
#
#   UP   vacated_targets   share of the team's 2025 targets held
#                          by players no longer on the roster
#        vacated_carries   same for designed carries
#        departed_rival    a same-position player left (with his
#                          2025 line)
#   DOWN drafted_rival     team spent an early pick (R1-3) at his
#                          position
#        signed_rival      veteran arrival at his position (with
#                          his 2025 line)
#        availability      currently non-active, or missed 5+
#                          weeks to injury in 2025
#   WATCH new_hc / new_oc  scheme turnover
#        new_qb            new starting QB (applies to pass
#                          catchers; QB changes reprice a whole
#                          receiving room)
#        pace              team's 2025 seconds/snap vs league —
#                          faster offense = more snaps to share
#
#  Everything is computed from tables already built and
#  ID-joined by gsis_id (R1.13). Draft capital uses the pick
#  itself, not a name lookup.
#
#  Output: data/context/fantasy_signals.json
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr); library(jsonlite) })

SKILL_GRPS <- c("QB", "RB", "WR", "TE")
FANTASY_JSON <- file.path("data", "context", "fantasy_signals.json")

# short production line used inside signal evidence. A QB is
# judged on his arm, a back on carries, a pass catcher on targets
# — one stat line per position rather than a single generic one.
.mini_line <- function(grp, tgt, rec, rec_yds, car, rush_yds, td,
                       att = 0, pass_yds = 0, pass_td = 0) {
  ifelse(grp == "QB",
    ifelse(att == 0, "no 2025 pass attempts",
           sprintf("%d att / %d pass yds / %d pass TD in '25",
                   att, pass_yds, pass_td)),
  ifelse(tgt + car == 0, "no 2025 touches",
  ifelse(grp %in% c("WR", "TE") | (grp == "RB" & tgt > car),
         sprintf("%d tgt / %d rec / %d yds / %d TD in '25", tgt, rec, rec_yds, td),
         sprintf("%d car / %d yds / %d TD in '25", car, rush_yds, td))))
}

build_fantasy_signals <- function() {
  # ---- inputs (all previously built) -----------------------
  depth <- fromJSON(file.path("data", "context", "depth_sheet_2026.json"))
  din   <- depth$depth %>% filter(grp %in% SKILL_GRPS)
  dout  <- depth$out   %>% filter(grp %in% SKILL_GRPS)
  off   <- read_csv(file.path("data", "context", "offseason_2026.csv"),
                    show_col_types = FALSE)
  draft <- read_csv(file.path("data", "context", "draft_picks_2026.csv"),
                    show_col_types = FALSE) %>%
    mutate(team = fix_team(team),
           grp = case_when(position == "QB" ~ "QB",
                           position %in% c("RB","FB") ~ "RB",
                           position == "WR" ~ "WR",
                           position == "TE" ~ "TE", TRUE ~ NA_character_)) %>%
    filter(!is.na(grp))

  # 2025 team usage, and how much of it walked out the door
  pg <- read_csv(file.path("data", "players", "profile_game.csv.gz"),
                 show_col_types = FALSE) %>% filter(season == 2025)
  team25 <- pg %>% group_by(team) %>%
    summarise(t_tgt = sum(targets), t_car = sum(carries), .groups = "drop")
  ply25 <- pg %>% group_by(player_id) %>%
    summarise(team = last(team), tgt = sum(targets), rec = sum(receptions),
              rec_yds = sum(rec_yds), car = sum(carries),
              rush_yds = sum(rush_yds), td = sum(rush_td) + sum(rec_td),
              att = sum(attempts), pass_yds = sum(pass_yds),
              pass_td = sum(pass_td), .groups = "drop")

  # departures carry gsis_id in the export -> join by id (R1.13)
  gone <- dout %>%
    left_join(ply25 %>% select(-team), by = c("gsis_id" = "player_id")) %>%
    mutate(across(c(tgt, rec, rec_yds, car, rush_yds, td,
                    att, pass_yds, pass_td), \(x) replace_na(x, 0)))

  vac <- gone %>% group_by(team) %>%
    summarise(vac_tgt = sum(tgt), vac_car = sum(car), .groups = "drop") %>%
    left_join(team25, by = "team") %>%
    mutate(vac_tgt_share = ifelse(t_tgt > 0, vac_tgt / t_tgt, NA),
           vac_car_share = ifelse(t_car > 0, vac_car / t_car, NA))

  # team-level context
  ctx <- off %>%
    transmute(team, hc_change, qb_change, oc_vacated, oc_status,
              hc_2026, qb26_r1)
  pace <- read_csv(file.path("data", "context", "playcall_team.csv"),
                   show_col_types = FALSE) %>%
    filter(season == 2025) %>%
    transmute(team, sec_per_play = sec_per_play_all, plays_pg)
  lg_pace <- mean(pace$sec_per_play, na.rm = TRUE)

  # ---- assemble per player ---------------------------------
  sig <- din %>%
    left_join(vac,  by = "team") %>%
    left_join(ctx,  by = "team") %>%
    left_join(pace, by = "team")

  mk <- function(dir, code, label, detail)
    list(dir = dir, code = code, label = label, detail = detail)

  out <- lapply(seq_len(nrow(sig)), function(i) {
    r <- sig[i, ]; S <- list()

    # --- competition drafted at his position ---------------
    dr <- draft %>% filter(team == r$team, grp == r$grp, round <= 3)
    if (nrow(dr) && r$status != "ROOKIE") {
      d <- dr %>% slice_min(pick, n = 1)
      S <- c(S, list(mk("down", "DRAFT", sprintf("R%d %s drafted", d$round, d$grp),
        sprintf("%s (%s, pick %d) \u2014 early draft capital at his position",
                d$pfr_player_name, d$position, d$pick))))
    }

    # --- veteran arrival at his position -------------------
    rivals_in <- sig %>% filter(team == r$team, grp == r$grp,
                                status == "NEW", player_name != r$player_name)
    if (nrow(rivals_in)) {
      v <- rivals_in %>% slice_max(coalesce(snaps25, 0), n = 1)
      ln <- ply25 %>% filter(player_id %in% v$gsis_id)
      S <- c(S, list(mk("down", "SIGNED", paste0(v$player_name[1], " added"),
        sprintf("arrived from %s%s", coalesce(v$from[1], "another team"),
          if (nrow(ln)) paste0(" \u2014 ", .mini_line(r$grp, ln$tgt, ln$rec,
                ln$rec_yds, ln$car, ln$rush_yds, ln$td,
                ln$att, ln$pass_yds, ln$pass_td)) else ""))))
    }

    # --- same-position departure ---------------------------
    left <- gone %>% filter(team == r$team, grp == r$grp)
    if (nrow(left)) {
      g <- left %>% slice_max(tgt + car, n = 1)
      S <- c(S, list(mk("up", "VACATED", paste0(g$player_name[1], " gone"),
        sprintf("to %s \u2014 %s", g$to[1],
                .mini_line(r$grp, g$tgt[1], g$rec[1], g$rec_yds[1],
                           g$car[1], g$rush_yds[1], g$td[1],
                           g$att[1], g$pass_yds[1], g$pass_td[1])))))
    }

    # --- quantified vacated opportunity --------------------
    if (r$grp %in% c("WR","TE","RB") && !is.na(r$vac_tgt_share) &&
        r$vac_tgt_share >= 0.08)
      S <- c(S, list(mk("up", "TGTS", sprintf("%.0f%% targets vacated",
              100 * r$vac_tgt_share),
              sprintf("%d of %d team targets from 2025 are off the roster",
                      r$vac_tgt, r$t_tgt))))
    if (r$grp == "RB" && !is.na(r$vac_car_share) && r$vac_car_share >= 0.10)
      S <- c(S, list(mk("up", "CARR", sprintf("%.0f%% carries vacated",
              100 * r$vac_car_share),
              sprintf("%d of %d team carries from 2025 are off the roster",
                      r$vac_car, r$t_car))))

    # --- scheme / QB turnover ------------------------------
    if (isTRUE(r$hc_change))
      S <- c(S, list(mk("watch", "HC", "new head coach",
                        paste0(r$hc_2026, " takes over"))))
    if (isTRUE(r$oc_vacated))
      S <- c(S, list(mk("watch", "OC", "OC vacated",
                        "2025 coordinator left for a head-coaching job")))
    else if (isTRUE(r$hc_change))
      S <- c(S, list(mk("watch", "OC", "OC likely new", r$oc_status)))
    if (isTRUE(r$qb_change) && r$grp %in% c("WR","TE","RB"))
      S <- c(S, list(mk("watch", "QB", paste0("new QB: ", r$qb26_r1),
                        "new starter reprices the whole receiving room")))

    # --- his own availability ------------------------------
    if (!is.na(r$avail_note))
      S <- c(S, list(mk("down", "AVAIL", r$avail_note, "not on the active roster")))
    else if (coalesce(r$y25_weeks_out_injury, 0) >= 5)
      S <- c(S, list(mk("down", "INJ", sprintf("%dw out in '25",
              r$y25_weeks_out_injury),
              paste0("mostly ", coalesce(r$y25_top_injury, "undisclosed")))))

    # --- pace / volume environment -------------------------
    if (!is.na(r$sec_per_play) && r$sec_per_play <= lg_pace - 1.5)
      S <- c(S, list(mk("up", "PACE", "fast offense",
              sprintf("%.1f sec/snap in 2025 vs %.1f league \u2014 more snaps to share",
                      r$sec_per_play, lg_pace))))
    if (!is.na(r$sec_per_play) && r$sec_per_play >= lg_pace + 1.5)
      S <- c(S, list(mk("down", "PACE", "slow offense",
              sprintf("%.1f sec/snap in 2025 vs %.1f league", r$sec_per_play, lg_pace))))

    list(gsis_id = r$gsis_id, team = r$team, grp = r$grp,
         player = r$player_name, slot = r$slot, signals = S)
  })

  Filter(function(x) length(x$signals) > 0, out)
}

if (sys.nframe() == 0) {
  source("utils.R"); source("offseason_sheet.R")
  s <- build_fantasy_signals()
  write_json(s, FANTASY_JSON, auto_unbox = TRUE, na = "null")
  message(sprintf("  fantasy signals: %d players flagged -> %s",
                  length(s), FANTASY_JSON))
}
