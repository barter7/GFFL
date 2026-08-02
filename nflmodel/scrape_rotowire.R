# ============================================================
#  scrape_rotowire.R  (nflmodel)
#
#  RotoWire player-news scraper — the NFL sibling of PropSZN's
#  RotoWire scraper in default_lineups.R, and written in the same
#  style: rvest + CSS selectors, player IDs pulled out of hrefs,
#  and the whole fetch wrapped in tryCatch so the pipeline
#  degrades gracefully when RotoWire is unavailable (403,
#  CAPTCHA) — the exact failure PropSZN already documents.
#
#  DESIGN NOTES
#
#  1. ONE feed request, not 1,200 player pages. The news feed
#     carries every player's latest blurb on a handful of pages,
#     so we page through it instead of hammering per-player URLs.
#     fetch_rotowire_player() exists for one-off top-ups only.
#
#  2. IDs come from the href (R1.13), never from the displayed
#     name — same trick PropSZN uses:
#         /football/player/kyler-murray-13613  ->  13613
#     which joins straight to the nflverse `rotowire_id` in the
#     roster crosswalk (verified: Murray = 13613).
#
#  3. Selectors are BEST-EFFORT and unverified: this sandbox
#     cannot reach rotowire.com (403 on every request, like every
#     non-GitHub host), so nothing here has been exercised against
#     a live page. Run inspect_rotowire_page() FIRST on your
#     machine — it dumps the candidate containers and their
#     classes — then correct RW_SEL below if the site's markup
#     differs. Same procedure as verifying the Action Network
#     prop slugs.
#
#  4. Polite by default: 2s between requests, a real UA, and a
#     hard page cap. Output caches to data/rotowire/blurbs.csv,
#     which load_rotowire_blurbs() already reads.
# ============================================================

suppressMessages({
  library(dplyr); library(readr); library(stringr); library(purrr)
})

RW_BASE      <- "https://www.rotowire.com"
RW_NEWS_URL  <- paste0(RW_BASE, "/football/news.php")
RW_BLURB_CSV <- file.path("data", "rotowire", "blurbs.csv")
RW_SLEEP     <- 2
RW_UA        <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                      "AppleWebKit/537.36 (KHTML, like Gecko)",
                      "Chrome/124.0 Safari/537.36")

# Candidate selectors — VERIFY with inspect_rotowire_page()
RW_SEL <- list(
  item     = ".news-update",              # one blurb block
  player   = "a[href*='/football/player/']",
  headline = ".news-update__headline",
  body     = ".news-update__news",
  analysis = ".news-update__analysis",
  time     = ".news-update__timestamp"
)

.rw_read <- function(url) {
  if (!requireNamespace("rvest", quietly = TRUE))
    stop("rvest required: install.packages('rvest')")
  Sys.sleep(RW_SLEEP)
  rvest::read_html(httr::GET(url, httr::user_agent(RW_UA),
                             httr::timeout(30)))
}

safe_text <- function(node, css) {
  el <- rvest::html_element(node, css)
  if (length(el) == 0 || is.na(el)) return(NA_character_)
  trimws(rvest::html_text(el))
}

# ── run this FIRST on an open network ────────────────────────
#  Prints the structure of the news page so RW_SEL can be
#  corrected against the real markup before a full run.
inspect_rotowire_page <- function(url = RW_NEWS_URL) {
  pg <- .rw_read(url)
  cat("== containers holding a player link ==\n")
  nodes <- rvest::html_elements(pg, "div,li,article")
  hits <- Filter(function(n)
    length(rvest::html_elements(n, RW_SEL$player)) > 0, nodes)
  cls <- table(unlist(lapply(head(hits, 60), function(n)
    rvest::html_attr(n, "class"))))
  print(head(sort(cls, decreasing = TRUE), 12))
  cat("\n== first candidate block ==\n")
  if (length(hits)) cat(substr(as.character(hits[[1]]), 1, 1200), "\n")
  invisible(pg)
}

# ── parse one news page into tidy blurbs ─────────────────────
parse_rotowire_news <- function(page) {
  items <- rvest::html_elements(page, RW_SEL$item)
  if (length(items) == 0) return(tibble())
  map_dfr(items, function(it) {
    link <- rvest::html_element(it, RW_SEL$player)
    if (length(link) == 0 || is.na(link)) return(tibble())
    href <- rvest::html_attr(link, "href")
    tibble(
      rotowire_id = str_extract(href, "\\d+$"),        # ID from href
      player      = trimws(rvest::html_text(link)),
      player_url  = ifelse(str_starts(href, "http"), href,
                           paste0(RW_BASE, href)),
      headline    = safe_text(it, RW_SEL$headline),
      blurb       = safe_text(it, RW_SEL$body),
      analysis    = safe_text(it, RW_SEL$analysis),
      date        = safe_text(it, RW_SEL$time)
    )
  }) %>% filter(!is.na(rotowire_id))
}

# ── single player page (top-ups only) ────────────────────────
fetch_rotowire_player <- function(rotowire_id, slug = "player") {
  url <- sprintf("%s/football/player/%s-%s", RW_BASE, slug, rotowire_id)
  tryCatch(parse_rotowire_news(.rw_read(url)),
           error = function(e) {
             message("  player ", rotowire_id, ": ", e$message); tibble() })
}

# ── main entry point ─────────────────────────────────────────
#  Pages through the news feed, merges with whatever is already
#  cached (newest row per player wins), and writes the CSV that
#  load_rotowire_blurbs() consumes.
refresh_rotowire_blurbs <- function(max_pages = 5, merge_existing = TRUE) {
  dir.create(dirname(RW_BLURB_CSV), recursive = TRUE, showWarnings = FALSE)

  fresh <- tryCatch({
    map_dfr(seq_len(max_pages), function(p) {
      url <- if (p == 1) RW_NEWS_URL else paste0(RW_NEWS_URL, "?page=", p)
      message("  fetching ", url)
      out <- parse_rotowire_news(.rw_read(url))
      message("    parsed ", nrow(out), " blurbs")
      out
    })
  }, error = function(e) {
    # PropSZN's posture: warn, keep the cache, never fail the run
    message("  WARNING: RotoWire scrape failed — ", e$message)
    message("  Keeping existing cache; links still work without blurbs.")
    tibble()
  })

  if (nrow(fresh) == 0) return(invisible(NULL))

  old <- if (merge_existing && file.exists(RW_BLURB_CSV))
    read_csv(RW_BLURB_CSV, show_col_types = FALSE) else tibble()
  out <- bind_rows(fresh, old) %>%
    mutate(rotowire_id = as.character(rotowire_id)) %>%
    distinct(rotowire_id, headline, .keep_all = TRUE) %>%
    arrange(rotowire_id)
  write_csv(out, RW_BLURB_CSV)
  message(sprintf("  rotowire blurbs: %d rows, %d players -> %s",
                  nrow(out), n_distinct(out$rotowire_id), RW_BLURB_CSV))
  invisible(out)
}

if (sys.nframe() == 0) refresh_rotowire_blurbs()
