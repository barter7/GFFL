# fetch_data.R - Pre-fetch league data and save as cached RDS files
#
# Run this script locally whenever you want to update the cached data:
#   Rscript fetch_data.R
#
# This fetches all seasons from ESPN and saves the results so the Shiny app
# can load instantly without needing ESPN API access.

library(ffscrapr)
library(dplyr)

# --- ESPN moved: fantasy.espn.com/apis/v3 answers 302 "Redirecting"
# to every request (probed from a runner, 2026-09-08, with and
# without cookies), while lm-api-reads.fantasy.espn.com serves the
# same paths — 401 JSON anonymous, 200 JSON with our cookies. The
# installed ffscrapr still hard-codes the old host inside its
# functions, so rewrite it across the package namespace. Scheme-
# anchored, so an already-correct URL can never be double-patched.
local({
  OLD <- "https://fantasy.espn.com"
  NEW <- "https://lm-api-reads.fantasy.espn.com"
  ns <- asNamespace("ffscrapr")

  # Rewrite one function's body AND default arguments (body() does
  # not cover formals). Returns the patched function, or NULL if it
  # never mentions the old host.
  patch_fn <- function(fn) {
    hit <- FALSE
    src <- deparse(body(fn))
    if (any(grepl(OLD, src, fixed = TRUE))) {
      src <- gsub(OLD, NEW, src, fixed = TRUE)
      body(fn) <- parse(text = paste(src, collapse = "\n"))[[1]]
      hit <- TRUE
    }
    fm <- formals(fn)
    fm_hit <- FALSE
    for (a in names(fm)) {
      # an argument with no default is the "missing" sentinel: never
      # bind it to a variable (evaluating that variable errors)
      ds <- tryCatch(paste(deparse(fm[[a]]), collapse = "\n"),
                     error = function(e) "")
      if (!nzchar(ds) || !grepl(OLD, ds, fixed = TRUE)) next
      fm[[a]] <- parse(text = gsub(OLD, NEW, ds, fixed = TRUE))[[1]]
      fm_hit <- TRUE
    }
    if (fm_hit) { formals(fn) <- fm; hit <- TRUE }
    if (hit) fn else NULL
  }

  patched <- character(0)
  for (nm in ls(ns, all.names = TRUE)) {
    obj <- get(nm, envir = ns)
    if (is.function(obj)) {
      p <- patch_fn(obj)
      if (!is.null(p)) {
        assignInNamespace(nm, p, ns = "ffscrapr")
        patched <- c(patched, nm)
      }
      # MEMOISED functions (espn_players — the players_wl call that
      # kept ff_draft empty) are cache wrappers: the real function
      # with the URL lives in the wrapper's closure environment, so
      # patch functions found there in place.
      e <- environment(obj)
      if (!is.null(e) && !identical(e, ns) &&
          !identical(e, globalenv()) && !identical(e, baseenv())) {
        for (inm in ls(e, all.names = TRUE)) {
          io <- tryCatch(get(inm, envir = e), error = function(err) NULL)
          if (!is.function(io)) next
          ip <- patch_fn(io)
          if (!is.null(ip)) {
            assign(inm, ip, envir = e)
            patched <- c(patched, paste0(nm, "<closure>", inm))
          }
        }
      }
    } else if (is.character(obj) && any(grepl(OLD, obj, fixed = TRUE))) {
      assignInNamespace(nm, gsub(OLD, NEW, obj, fixed = TRUE), ns = "ffscrapr")
      patched <- c(patched, nm)
    }
  }
  cat("ffscrapr: pointed", length(patched), "objects at lm-api-reads:",
      paste(patched, collapse = ", "), "\n")
  if (!length(patched))
    cat("ffscrapr: nothing carried the old host — package likely",
        "fixed upstream; patch is a no-op\n")
})

# .espn_week_checkmax trims the requested weeks to what the API's
# status block claims — and via lm-api-reads that claim comes up one
# short on every COMPLETED season, which silently dropped each
# season's final week of starters (weeks 1-16 fetched where the
# committed data has 1-17, all other weeks byte-identical). Let the
# per-week fetch decide instead: a week with no lineups already
# warns and contributes nothing (season 2026 proves that path), so
# the shim answers every request with the weeks it was asked about.
local({
  ns <- asNamespace("ffscrapr")
  if (exists(".espn_week_checkmax", envir = ns, inherits = FALSE)) {
    assignInNamespace(".espn_week_checkmax", function(...) {
      a <- list(...)
      num <- Filter(is.numeric, a)
      if (!length(num)) return(invisible(NULL))
      # the weeks vector is the longest numeric argument
      num[[which.max(vapply(num, length, 0L))]]
    }, ns = "ffscrapr")
    cat("ffscrapr: .espn_week_checkmax no longer trims requested weeks\n")
  }
})

# Warnings surface at the moment they happen, next to the season and
# call that raised them — "There were 12 warnings" at the end of a CI
# log identifies nothing.
options(warn = 1)

# --- Configuration ---
LEAGUE_ID <- 570237
# Latest season = year as of 180 days ago, so the new season is only picked up
# once it actually starts (a January run still fetches last fall's season, and
# the next season is added automatically at the end of the following summer).
SEASONS <- 2017:as.integer(format(Sys.Date() - 180, "%Y"))
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
all_starters <- list()
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
    starters <- tryCatch(ff_starters(conn), error = function(e) NULL)

    if (!"season" %in% names(standings)) standings$season <- s
    if (!"season" %in% names(schedule)) schedule$season <- s
    if (!is.null(draft) && !"season" %in% names(draft)) draft$season <- s
    if (!is.null(starters) && !"season" %in% names(starters)) starters$season <- s

    all_league[[as.character(s)]] <- league_info
    all_standings[[as.character(s)]] <- standings
    all_schedules[[as.character(s)]] <- schedule
    if (!is.null(draft) && nrow(draft) > 0) {
      all_drafts[[as.character(s)]] <- draft
    }
    if (!is.null(starters) && nrow(starters) > 0) {
      all_starters[[as.character(s)]] <- starters
    }

    cat("OK\n")
    # TEMPORARY DIAGNOSTIC: the lm-api-reads move shrank starters by
    # ~one week per season vs the committed JSON — name the weeks so
    # the missing one is identifiable, then remove this.
    if (!is.null(starters) && nrow(starters) > 0 && "week" %in% names(starters)) {
      wk <- table(starters$week)
      cat("    starters weeks: ",
          paste(sprintf("%s=%d", names(wk), as.integer(wk)), collapse = " "),
          "\n", sep = "")
    }
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
  })
}

# --- Validate (fail closed: never overwrite good cached data with a bad fetch) ---
# A season counts as fetched only if it produced at least one standings row.
fetched_seasons <- sort(as.integer(names(Filter(function(x) nrow(x) > 0, all_standings))))
missing_seasons <- setdiff(SEASONS, fetched_seasons)

if (length(fetched_seasons) == 0) {
  stop("FATAL: no seasons were fetched from ESPN. ",
       "Check that ESPN_S2/ESPN_SWID are current (they expire periodically) ",
       "and that league ", LEAGUE_ID, " is reachable. ",
       "Nothing was written to data/.")
}

if (length(missing_seasons) > 0) {
  if (identical(as.integer(missing_seasons), as.integer(max(SEASONS)))) {
    # Only the newest season is missing: the league likely hasn't been renewed
    # on ESPN yet. Historical data is intact, so continue with a loud warning.
    cat("\n")
    cat("****************************************************************\n")
    cat("* WARNING: season", max(SEASONS), "returned no standings data.      *\n")
    cat("* The league has probably not been renewed on ESPN yet.       *\n")
    cat("* Continuing with seasons", min(fetched_seasons), "-", max(fetched_seasons), "only.                  *\n")
    cat("****************************************************************\n\n")
  } else {
    stop("FATAL: standings data is missing for historical season(s) ",
         paste(missing_seasons, collapse = ", "),
         " (expected ", min(SEASONS), "-", max(SEASONS), "). ",
         "This usually means the ESPN_S2/ESPN_SWID cookies have expired ",
         "or ESPN returned partial data. Refusing to save: nothing was ",
         "written to data/, existing cached files are untouched.")
  }
}

# --- Combine and Save ---
dir.create("data", showWarnings = FALSE)

standings_data <- bind_rows(all_standings)
schedule_data <- bind_rows(all_schedules)
draft_data <- bind_rows(all_drafts)
starters_data <- bind_rows(all_starters)

saveRDS(all_league, "data/league_info.rds")
saveRDS(standings_data, "data/standings.rds")
saveRDS(schedule_data, "data/schedule.rds")
saveRDS(draft_data, "data/drafts.rds")
saveRDS(starters_data, "data/starters.rds")

# Also fetch and save NFL player headshots for display
cat("\nFetching NFL player headshots from nflreadr...\n")
tryCatch({
  player_ids <- nflreadr::load_ff_playerids()
  saveRDS(player_ids, "data/player_ids.rds")
  cat("  Saved player IDs (", nrow(player_ids), "players)\n")
}, error = function(e) cat("  Could not fetch player IDs:", e$message, "\n"))

# Fetch full NFL player season stats from nflreadr (not ESPN-dependent)
cat("\nFetching full NFL player season stats from nflreadr...\n")
tryCatch({
  player_stats <- nflreadr::load_player_stats(seasons = SEASONS, stat_type = "offense")
  saveRDS(player_stats, "data/player_stats.rds")
  cat("  Saved player stats (", nrow(player_stats), "rows)\n")
}, error = function(e) cat("  Could not fetch player stats:", e$message, "\n"))

cat("\nSaved cached data to data/ folder:\n")
cat("  data/league_info.rds\n")
cat("  data/standings.rds  (", nrow(standings_data), "rows)\n")
cat("  data/schedule.rds   (", nrow(schedule_data), "rows)\n")
cat("  data/drafts.rds     (", nrow(draft_data), "rows)\n")
cat("  data/starters.rds   (", nrow(starters_data), "rows)\n")
cat("  data/player_ids.rds\n")
cat("  data/player_stats.rds\n")
cat("\nDone! The Shiny app will now load from these cached files.\n")
