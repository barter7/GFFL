# ============================================================
#  injury_profile.R  (nflmodel)
#
#  Availability layer (rule R2.8), two distinct things that must
#  never be conflated:
#
#  1. CURRENT status — where a player stands right now. In the
#     preseason this comes from the ROSTER status field, because
#     official weekly injury reports do not exist until Week 1
#     (injuries_2026.csv 404s in August, by design). Codes:
#       ACT active · RES reserve (IR/PUP/NFI) · E14 exempt
#       RET retired · CUT waived/released
#     Once the season starts, the weekly report supersedes it and
#     carries the real designation (Out/Doubtful/Questionable)
#     plus practice participation.
#
#  2. DURABILITY history — what the player's availability has
#     looked like, per season and across the era (2021-2025):
#       weeks_out    weeks listed Out
#       weeks_dnp    weeks that did not participate in practice
#       weeks_q      weeks listed Questionable/Doubtful
#       weeks_listed weeks appearing on a report at all
#       top_injury   most frequent reported body part
#     A "designation" is NOT the same as missing a game — R2.5
#     applies: late-season Out rows are often rest ("Coaching
#     Decision"), so `weeks_out_injury` excludes non-injury
#     reasons.
#
#  All joins are by gsis_id (R1.13).
#
#  Output: data/context/injury_profile.csv
# ============================================================

suppressMessages({ library(dplyr); library(tidyr); library(readr) })

INJURY_SEASONS <- 2021:2025
INJURY_PROFILE_FILE <- file.path("data", "context", "injury_profile.csv")

# reasons that mean "not hurt" — rest, personal, coach's call
NON_INJURY_RX <- "coach|rest|not injury related|personal|birth|illness"

load_injury_reports <- function(seasons = INJURY_SEASONS) {
  paths <- file.path("data", "rosters", sprintf("injuries_%d.csv", seasons))
  have <- file.exists(paths)
  if (!any(have)) stop("no injury files cached — see roster_context.R")
  bind_rows(lapply(paths[have], function(p)
    read_csv(p, show_col_types = FALSE,
             col_select = any_of(c("season","week","team","gsis_id","full_name",
               "position","report_primary_injury","report_status",
               "practice_primary_injury","practice_status"))))) %>%
    mutate(team = normalize_team(team),
           injury = coalesce(report_primary_injury, practice_primary_injury),
           non_injury = grepl(NON_INJURY_RX, tolower(coalesce(injury, "")), ) |
                        grepl(NON_INJURY_RX, tolower(coalesce(report_status, ""))))
}

build_injury_profile <- function(seasons = INJURY_SEASONS) {
  inj <- load_injury_reports(seasons) %>% filter(!is.na(gsis_id))

  per_season <- inj %>%
    group_by(gsis_id, season) %>%
    summarise(
      weeks_listed = n_distinct(week),
      weeks_out    = n_distinct(week[report_status == "Out"]),
      weeks_out_injury = n_distinct(week[report_status == "Out" & !non_injury]),
      weeks_q      = n_distinct(week[report_status %in% c("Questionable","Doubtful")]),
      weeks_dnp    = n_distinct(week[practice_status ==
                                       "Did Not Participate In Practice"]),
      top_injury   = { t <- table(na.omit(injury[!non_injury]))
                       if (length(t)) names(t)[which.max(t)] else NA_character_ },
      .groups = "drop")

  career <- per_season %>%
    group_by(gsis_id) %>%
    summarise(seasons_seen = n(),
              across(c(weeks_listed, weeks_out, weeks_out_injury,
                       weeks_q, weeks_dnp), sum),
              .groups = "drop") %>%
    rename_with(~paste0("car_", .x), -gsis_id)

  last <- per_season %>% filter(season == max(seasons)) %>%
    select(-season) %>% rename_with(~paste0("y25_", .x), -gsis_id)

  # current status from the 2026 roster (preseason truth)
  cur <- read_csv(file.path("data", "players", "roster_2026.csv"),
                  show_col_types = FALSE) %>%
    filter(!is.na(gsis_id)) %>%
    transmute(gsis_id, cur_status = status,
              cur_code = status_description_abbr,
              rookie = coalesce(as.numeric(years_exp), 99) == 0,
              # code meanings are NOT published; the one we can
              # verify empirically is R09 = 17 rookies / 0 veterans,
              # i.e. an unsigned-or-unreported draft pick, NOT an
              # injury. Everything else stays deliberately generic.
              avail_note = case_when(
                status == "ACT" ~ NA_character_,
                status == "RET" ~ "retired",
                status == "CUT" ~ "waived/released",
                status == "E14" ~ "exempt list",
                cur_code == "R09" & rookie ~ "unsigned/unreported pick",
                status == "RES" ~ "reserve list (IR/PUP/NFI)",
                TRUE ~ paste("not active:", status)),
              cur_available = status == "ACT") %>%
    select(-rookie) %>%
    distinct(gsis_id, .keep_all = TRUE)

  cur %>% full_join(last, by = "gsis_id") %>%
    full_join(career, by = "gsis_id") %>%
    mutate(across(where(is.numeric), \(x) replace_na(x, 0)))
}

if (sys.nframe() == 0) {
  source("utils.R")
  p <- build_injury_profile()
  write_csv(p, INJURY_PROFILE_FILE)
  message(sprintf("  injury profile: %d players -> %s", nrow(p),
                  INJURY_PROFILE_FILE))
}
