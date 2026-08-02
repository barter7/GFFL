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

# Relevance gates. A rival only moves the needle if he actually
# played: an arriving camp body with 13 pass attempts is not
# competition, and a departure with four targets vacates nothing.
MIN_TOUCH <- 25    # targets + carries, for RB/WR/TE
MIN_ATT   <- 50    # pass attempts, the QB equivalent

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

  # 2025 usage, accrued PER TEAM. Grouping by player alone credits
  # a midseason trade's full season to whichever team he finished
  # with, which both overstates that team's vacated share and
  # understates the team he actually produced for.
  pg <- read_csv(file.path("data", "players", "profile_game.csv.gz"),
                 show_col_types = FALSE) %>% filter(season == 2025)
  acc <- pg %>% group_by(player_id, team) %>%
    summarise(tgt = sum(targets), rec = sum(receptions),
              rec_yds = sum(rec_yds), car = sum(carries),
              rush_yds = sum(rush_yds), td = sum(rush_td) + sum(rec_td),
              att = sum(attempts), pass_yds = sum(pass_yds),
              pass_td = sum(pass_td), .groups = "drop")
  team25 <- acc %>% group_by(team) %>%
    summarise(t_tgt = sum(tgt), t_car = sum(car), .groups = "drop")
  # league-wide totals, for describing an arriving player
  ply25 <- acc %>% group_by(player_id) %>%
    summarise(across(c(tgt, rec, rec_yds, car, rush_yds, td,
                       att, pass_yds, pass_td), sum), .groups = "drop")

  # Departures = the FULL 2025 -> 2026 roster diff. The depth
  # sheet's `out` list is capped at 250+ snaps because it is a
  # DISPLAY list; reusing it here silently dropped every slot
  # receiver and change-of-pace back below that cut, which is
  # exactly the usage a vacated-share number is supposed to catch
  # (NO read as 2% vacated when the true figure is 25% — S22).
  r26 <- read_csv(file.path("data", "players", "roster_2026.csv"),
                  show_col_types = FALSE) %>%
    filter(!is.na(gsis_id), status %in% c("ACT", "RES")) %>%
    transmute(gsis_id, team26 = normalize_team(team)) %>%
    distinct(gsis_id, .keep_all = TRUE)
  nm25 <- read_csv(file.path("data", "players", "roster_2025.csv"),
                   show_col_types = FALSE) %>%
    filter(!is.na(gsis_id)) %>%
    transmute(gsis_id, player_name = full_name,
              grp = ROSTER_GROUP(position)) %>%
    distinct(gsis_id, .keep_all = TRUE)

  gone <- acc %>%
    left_join(r26, by = c("player_id" = "gsis_id")) %>%
    filter(is.na(team26) | team26 != team) %>%          # left this team
    left_join(nm25, by = c("player_id" = "gsis_id")) %>%
    mutate(to = coalesce(team26, "FA/none")) %>%
    filter(!is.na(player_name))

  # Vacated carries exclude departing QBs: their designed runs are
  # not carries a back inherits (NO alone had 52 such carries, 6%
  # of the league's vacated total but heavily concentrated).
  vac <- gone %>% group_by(team) %>%
    summarise(vac_tgt = sum(tgt),
              vac_car = sum(car[grp != "QB"]), .groups = "drop") %>%
    left_join(team25, by = "team") %>%
    mutate(vac_tgt_share = ifelse(t_tgt > 0, vac_tgt / t_tgt, NA),
           vac_car_share = ifelse(t_car > 0, vac_car / t_car, NA))

  # Thresholds are calibrated against THIS offseason, not frozen.
  # Roster churn is large every year (median team vacates ~28% of
  # its targets), so a fixed 8% cut fired for 28 of 32 teams —
  # a signal that lights up almost everywhere carries no
  # information. Fire on the top third instead, with a floor so a
  # genuinely quiet offseason doesn't manufacture signals.
  q70 <- function(x, floor) max(quantile(x, 0.70, na.rm = TRUE), floor)
  TGT_CUT <- q70(vac$vac_tgt_share, 0.15)
  CAR_CUT <- q70(vac$vac_car_share, 0.20)
  TGT_MED <- median(vac$vac_tgt_share, na.rm = TRUE)
  CAR_MED <- median(vac$vac_car_share, na.rm = TRUE)

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
  #  own_pick: a rookie's own draft slot, parsed back out of the
  #  depth sheet's "R{round}.{overall}" label, so the DRAFT signal
  #  can compare picks instead of blanket-skipping every rookie.
  sig <- din %>%
    mutate(own_pick = ifelse(status == "ROOKIE",
             suppressWarnings(as.numeric(sub("^R\\d+\\.", "", from))), NA_real_),
           own_pick = ifelse(is.na(own_pick) & status == "ROOKIE",
                             Inf, own_pick)) %>%   # UDFA = last in line
    left_join(vac,  by = "team") %>%
    left_join(ctx,  by = "team") %>%
    left_join(pace, by = "team")

  mk <- function(dir, code, label, detail)
    list(dir = dir, code = code, label = label, detail = detail)

  out <- lapply(seq_len(nrow(sig)), function(i) {
    r <- sig[i, ]; S <- list()

    # --- competition drafted at his position ---------------
    #  A rookie is not immune: a UDFA or a round-6 pick still has
    #  a problem if the team took someone at his position AHEAD of
    #  him. Only picks earlier than his own count, so the drafted
    #  player never flags himself.
    dr <- draft %>% filter(team == r$team, grp == r$grp, round <= 3,
                           pick < coalesce(r$own_pick, Inf))
    if (nrow(dr)) {
      d <- dr %>% slice_min(pick, n = 1, with_ties = FALSE)
      S <- c(S, list(mk("down", "DRAFT", sprintf("R%d %s drafted", d$round, d$grp),
        sprintf("%s (%s, pick %d) \u2014 early draft capital at his position",
                d$pfr_player_name, d$position, d$pick))))
    }

    # --- veteran arrival at his position -------------------
    #  Relevance gate: an arrival only threatens him if he produced
    #  last year or is slotted ahead of him. Without it a camp body
    #  with 13 pass attempts "downgraded" a starting QB.
    rivals_in <- sig %>%
      filter(team == r$team, grp == r$grp, status == "NEW",
             gsis_id != r$gsis_id) %>%       # self-exclusion by ID (R1.13)
      left_join(ply25, by = c("gsis_id" = "player_id")) %>%
      mutate(across(c(tgt, rec, rec_yds, car, rush_yds, td,
                      att, pass_yds, pass_td), \(x) replace_na(x, 0)),
             produced = if (r$grp == "QB") att >= MIN_ATT
                        else tgt + car >= MIN_TOUCH) %>%
      filter(produced | slot < r$slot)
    if (nrow(rivals_in)) {
      v <- rivals_in %>% slice_max(coalesce(snaps25, 0), n = 1, with_ties = FALSE)
      S <- c(S, list(mk("down", "SIGNED", paste0(v$player_name, " added"),
        sprintf("arrived from %s \u2014 %s", coalesce(v$from, "another team"),
                .mini_line(r$grp, v$tgt, v$rec, v$rec_yds, v$car,
                           v$rush_yds, v$td, v$att, v$pass_yds, v$pass_td)))))
    }

    # --- same-position departure ---------------------------
    #  Same relevance gate as an arrival: naming a player who left
    #  with four targets is noise dressed up as a signal.
    left <- gone %>% filter(team == r$team, grp == r$grp) %>%
      filter(if (r$grp == "QB") att >= MIN_ATT else tgt + car >= MIN_TOUCH)
    if (nrow(left)) {
      g <- left %>% slice_max(tgt + car + att, n = 1, with_ties = FALSE)
      S <- c(S, list(mk("up", "VACATED", paste0(g$player_name, " gone"),
        sprintf("to %s \u2014 %s", g$to,
                .mini_line(r$grp, g$tgt, g$rec, g$rec_yds, g$car,
                           g$rush_yds, g$td, g$att, g$pass_yds, g$pass_td)))))
    }

    # --- quantified vacated opportunity --------------------
    if (r$grp %in% c("WR","TE","RB") && !is.na(r$vac_tgt_share) &&
        r$vac_tgt_share >= TGT_CUT)
      S <- c(S, list(mk("up", "TGTS", sprintf("%.0f%% targets vacated",
              100 * r$vac_tgt_share),
              sprintf(paste("%d of %d team targets from 2025 are off the",
                            "roster \u2014 top third of the league, which",
                            "vacates %.0f%% at the median"),
                      r$vac_tgt, r$t_tgt, 100 * TGT_MED))))
    if (r$grp == "RB" && !is.na(r$vac_car_share) && r$vac_car_share >= CAR_CUT)
      S <- c(S, list(mk("up", "CARR", sprintf("%.0f%% carries vacated",
              100 * r$vac_car_share),
              sprintf(paste("%d of %d team carries from 2025 are off the",
                            "roster \u2014 top third of the league, which",
                            "vacates %.0f%% at the median"),
                      r$vac_car, r$t_car, 100 * CAR_MED))))

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
        if (r$grp == "RB") "new starter changes the run/pass mix and the checkdown rate"
        else "new starter reprices the whole receiving room")))

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
  source("utils.R"); source("offseason_sheet.R"); source("depth_sheet.R")
  s <- build_fantasy_signals()
  write_json(s, FANTASY_JSON, auto_unbox = TRUE, na = "null")
  message(sprintf("  fantasy signals: %d players flagged -> %s",
                  length(s), FANTASY_JSON))
}
