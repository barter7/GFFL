# ============================================================
#  scrape_coordinators.R  (nflmodel)
#
#  Builds data/context/coordinators.csv — offensive coordinators
#  by team-season — from Pro Football Reference team pages
#  (the "Offensive Coordinator:" line in each team-season
#  header, PFR's authoritative staff record).
#
#  RUN THIS LOCALLY (open network). PFR is not reachable from
#  the sandboxed dev environment, and no nflverse dataset
#  carries coordinators — this file is the one context input
#  that must be generated on your machine:
#
#      cd nflmodel
#      Rscript scrape_coordinators.R            # 2024-2026
#      Rscript scrape_coordinators.R 2021 2026  # custom range
#
#  Output schema (consumed by game_context.R::load_coordinators):
#    season, team, oc_name, from_week, to_week, source
#  from_week/to_week are left NA by the scraper (= whole
#  season). For mid-season OC changes, edit the CSV by hand:
#  duplicate the row, set the week ranges, keep both names.
#  PFR lists all OCs who held the job in a season separated by
#  commas — the scraper flags those rows with needs_review = 1
#  so you know where hand-editing is required.
#
#  Notes:
#  - Some teams have seasons with no official OC title (the HC
#    or another coach calls plays, e.g. several Shanahan 49ers
#    seasons list none) — those rows get oc_name = NA.
#  - Be polite to PFR: one request per team-season, 4s sleep,
#    ~2.5 min per season of 32 teams.
# ============================================================

suppressMessages({ library(dplyr); library(readr); library(httr) })
source("utils.R")

CONTEXT_DIR       <- file.path("data", "context")
COORDINATORS_FILE <- file.path(CONTEXT_DIR, "coordinators.csv")

# PFR franchise codes -> nflverse team codes
PFR_TEAMS <- c(
  crd = "ARI", atl = "ATL", rav = "BAL", buf = "BUF", car = "CAR",
  chi = "CHI", cin = "CIN", cle = "CLE", dal = "DAL", den = "DEN",
  det = "DET", gnb = "GB",  htx = "HOU", clt = "IND", jax = "JAX",
  kan = "KC",  sdg = "LAC", ram = "LA",  rai = "LV",  mia = "MIA",
  min = "MIN", nwe = "NE",  nor = "NO",  nyg = "NYG", nyj = "NYJ",
  phi = "PHI", pit = "PIT", sea = "SEA", sfo = "SF",  tam = "TB",
  oti = "TEN", was = "WAS"
)

fetch_team_season_oc <- function(pfr_code, season) {
  url <- sprintf("https://www.pro-football-reference.com/teams/%s/%d.htm",
                 pfr_code, season)
  resp <- tryCatch(httr::GET(url, httr::user_agent("nflmodel-research"),
                             httr::timeout(30)),
                   error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) return(NA_character_)
  html <- httr::content(resp, "text", encoding = "UTF-8")
  # header line looks like:
  #   <strong>Offensive Coordinator:</strong> <a href="...">Name</a>
  m <- regmatches(html, regexpr(
    "Offensive Coordinator:</strong>.{0,400}?</p>", html))
  if (length(m) == 0) return(NA_character_)
  names_found <- regmatches(m, gregexpr("(?<=>)[^<>]+(?=</a>)", m, perl = TRUE))[[1]]
  if (length(names_found) == 0) return(NA_character_)
  paste(trimws(names_found), collapse = ", ")
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

args    <- commandArgs(trailingOnly = TRUE)
FROM    <- if (length(args) >= 1) as.integer(args[1]) else 2024L
TO      <- if (length(args) >= 2) as.integer(args[2]) else 2026L

dir.create(CONTEXT_DIR, recursive = TRUE, showWarnings = FALSE)
rows <- list()
for (season in FROM:TO) {
  for (i in seq_along(PFR_TEAMS)) {
    pfr  <- names(PFR_TEAMS)[i]
    team <- PFR_TEAMS[[i]]
    oc   <- fetch_team_season_oc(pfr, season)
    rows[[length(rows) + 1]] <- tibble(
      season, team, oc_name = oc,
      from_week = NA_integer_, to_week = NA_integer_,
      needs_review = as.integer(grepl(",", oc %||% "")),
      source = sprintf("pro-football-reference.com/teams/%s/%d.htm",
                       pfr, season)
    )
    message(sprintf("  %d %s: %s", season, team,
                    ifelse(is.na(oc), "(no OC listed)", oc)))
    Sys.sleep(4)
  }
}

out <- bind_rows(rows)
write_csv(out, COORDINATORS_FILE)
message(sprintf("\n%d rows -> %s", nrow(out), COORDINATORS_FILE))
message("Review rows with needs_review = 1 (multiple OCs in one season) ",
        "and set from_week/to_week for mid-season changes.")
