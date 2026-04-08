# fetch_data.R - Pre-fetch league data and save as cached RDS files
#
# Run this script locally whenever you want to update the cached data:
#   Rscript fetch_data.R
#
# This fetches all seasons from ESPN and saves the results so the Shiny app
# can load instantly without needing ESPN API access.

library(ffscrapr)
library(dplyr)

# --- Configuration ---
LEAGUE_ID <- 570237
SEASONS <- 2017:2025
ESPN_S2 <- Sys.getenv("ESPN_S2")
ESPN_SWID <- Sys.getenv("ESPN_SWID")

# Allow passing credentials as arguments too
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
  ESPN_S2 <- args[1]
  ESPN_SWID <- args[2]
}

if (nchar(ESPN_S2) == 0 || nchar(ESPN_SWID) == 0) {
  stop("ESPN_S2 and ESPN_SWID must be set in .Renviron or passed as arguments.\n",
       "Usage: Rscript fetch_data.R <ESPN_S2> <ESPN_SWID>")
}

cat("Fetching GFFL league data for seasons", min(SEASONS), "-", max(SEASONS), "\n")

# --- Fetch Data ---
all_standings <- list()
all_schedules <- list()
all_drafts <- list()
all_league <- list()

for (s in SEASONS) {
  cat("  Season", s, "... ")

  tryCatch({
    conn <- espn_connect(
      season = s,
      league_id = LEAGUE_ID,
      espn_s2 = ESPN_S2,
      swid = ESPN_SWID
    )

    league_info <- ff_league(conn)
    standings <- ff_standings(conn)
    schedule <- ff_schedule(conn)
    draft <- tryCatch(ff_draft(conn), error = function(e) NULL)

    if (!"season" %in% names(standings)) standings$season <- s
    if (!"season" %in% names(schedule)) schedule$season <- s
    if (!is.null(draft) && !"season" %in% names(draft)) draft$season <- s

    all_league[[as.character(s)]] <- league_info
    all_standings[[as.character(s)]] <- standings
    all_schedules[[as.character(s)]] <- schedule
    if (!is.null(draft) && nrow(draft) > 0) {
      all_drafts[[as.character(s)]] <- draft
    }

    cat("OK\n")
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
  })
}

# --- Combine and Save ---
dir.create("data", showWarnings = FALSE)

standings_data <- bind_rows(all_standings)
schedule_data <- bind_rows(all_schedules)
draft_data <- bind_rows(all_drafts)

saveRDS(all_league, "data/league_info.rds")
saveRDS(standings_data, "data/standings.rds")
saveRDS(schedule_data, "data/schedule.rds")
saveRDS(draft_data, "data/drafts.rds")

cat("\nSaved cached data to data/ folder:\n")
cat("  data/league_info.rds\n")
cat("  data/standings.rds  (", nrow(standings_data), "rows)\n")
cat("  data/schedule.rds   (", nrow(schedule_data), "rows)\n")
cat("  data/drafts.rds     (", nrow(draft_data), "rows)\n")
cat("\nDone! The Shiny app will now load from these cached files.\n")
