# ============================================================
#  rotowire_links.R  (nflmodel)
#
#  RotoWire player news (rule R2.9). Two capabilities:
#
#  1. DEEP LINKS — built from the `rotowire_id` carried in the
#     nflverse roster crosswalk (R1.13), never from a guessed
#     name. Verified against a known page:
#         Kyler Murray  rotowire_id 13613
#         -> /football/player/kyler-murray-13613
#     Coverage: ~66% of the 2026 roster. Players without an id
#     get NO link rather than a guessed URL.
#
#  2. BLURB DROP-IN — this repo does NOT scrape RotoWire. Their
#     news blurbs are the product they sell, and bulk-pulling
#     them would breach their terms; they publish an official
#     API/feed for subscribers who want the text in an app.
#     Put whatever you are entitled to pull into data/rotowire/
#     and load_rotowire_blurbs() joins it by rotowire_id.
#
#     Expected file(s): any .csv/.json in data/rotowire/ with
#       - `rotowire_id` (or `id`), and
#       - a text column named one of: blurb / news / notes /
#         analysis / description
#       - optional `date`/`updated` (newest row per player wins)
#
#  Output: data/context/rotowire_links.csv
# ============================================================

suppressMessages({ library(dplyr); library(readr); library(jsonlite) })

ROTOWIRE_DIR   <- file.path("data", "rotowire")
ROTOWIRE_LINKS <- file.path("data", "context", "rotowire_links.csv")
ROTOWIRE_BASE  <- "https://www.rotowire.com/football/player/"

# name -> URL slug, matching RotoWire's own convention
rw_slug <- function(name) {
  s <- tolower(iconv(name, to = "ASCII//TRANSLIT"))
  s <- gsub("[.'’]", "", s)
  s <- gsub("[^a-z0-9]+", "-", s)
  gsub("^-|-$", "", s)
}

build_rotowire_links <- function(season = 2026) {
  r <- read_csv(file.path("data", "players", sprintf("roster_%d.csv", season)),
                show_col_types = FALSE) %>%
    filter(!is.na(gsis_id)) %>%
    distinct(gsis_id, .keep_all = TRUE)
  r %>%
    transmute(gsis_id, player = full_name, team = normalize_team(team),
              position, rotowire_id,
              rotowire_url = ifelse(is.na(rotowire_id), NA_character_,
                paste0(ROTOWIRE_BASE, rw_slug(full_name), "-", rotowire_id)))
}

# ── optional local blurb cache ───────────────────────────────
load_rotowire_blurbs <- function() {
  if (!dir.exists(ROTOWIRE_DIR)) return(NULL)
  files <- list.files(ROTOWIRE_DIR, pattern = "\\.(csv|json)$", full.names = TRUE)
  if (!length(files)) return(NULL)
  raw <- bind_rows(lapply(files, function(f) {
    d <- if (grepl("\\.json$", f)) as_tibble(fromJSON(f))
         else read_csv(f, show_col_types = FALSE)
    names(d) <- tolower(names(d))
    if (!"rotowire_id" %in% names(d) && "id" %in% names(d))
      d <- rename(d, rotowire_id = id)
    d
  }))
  if (!"rotowire_id" %in% names(raw)) {
    warning("data/rotowire/: no rotowire_id column — cannot join by id (R1.13)")
    return(NULL)
  }
  txt <- intersect(c("blurb","news","notes","analysis","description"), names(raw))
  if (!length(txt)) {
    warning("data/rotowire/: no blurb text column found")
    return(NULL)
  }
  dt <- intersect(c("date","updated","timestamp"), names(raw))
  out <- raw %>% mutate(rotowire_id = as.character(rotowire_id),
                        blurb = .data[[txt[1]]])
  if (length(dt)) out <- out %>% arrange(desc(.data[[dt[1]]]))
  out %>% distinct(rotowire_id, .keep_all = TRUE) %>%
    select(rotowire_id, blurb, any_of(dt[1]))
}

if (sys.nframe() == 0) {
  source("utils.R")
  dir.create(ROTOWIRE_DIR, showWarnings = FALSE, recursive = TRUE)
  L <- build_rotowire_links()
  b <- load_rotowire_blurbs()
  if (!is.null(b)) L <- L %>% mutate(rotowire_id = as.character(rotowire_id)) %>%
    left_join(b, by = "rotowire_id")
  write_csv(L, ROTOWIRE_LINKS)
  message(sprintf("  rotowire: %d players, %d with links (%.0f%%), blurbs=%s",
                  nrow(L), sum(!is.na(L$rotowire_url)),
                  100 * mean(!is.na(L$rotowire_url)), !is.null(b)))
}
