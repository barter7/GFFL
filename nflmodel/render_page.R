# ============================================================
#  render_page.R  (nflmodel)
#
#  Splices a data payload into one of the web/*.html templates
#  and writes a single self-contained file ready to publish.
#
#  The templates carry a `/*__DATA__*/` marker inside their
#  <script> block; everything else about them is static. Nothing
#  is fetched at view time — published pages run under a CSP that
#  blocks every external host, so the data has to travel inside
#  the file.
#
#  Usage:
#    source("render_page.R")
#    render_page("web/depth.html", depth_payload(), "/tmp/depth.html")
# ============================================================

suppressMessages({ library(jsonlite) })

render_page <- function(template, data, out) {
  html <- readLines(template, warn = FALSE, encoding = "UTF-8")
  i <- grep("/\\*__DATA__\\*/", html, fixed = FALSE)
  if (length(i) != 1L)
    stop("template needs exactly one /*__DATA__*/ marker: ", template)
  # `null = "null"` is NOT optional. jsonlite's default writes an R
  # NULL as `{}`, and `{}` is TRUTHY in JavaScript — so every
  # `if (r.avail_note)` guard in the template fires for players who
  # have no note at all. Round-tripping a JSON file through
  # fromJSON/toJSON silently converted 1,177 active players into
  # "RESERVE" on the published page (S24).
  js <- paste0("const D=", toJSON(data, auto_unbox = TRUE, na = "null",
                                 null = "null", digits = 4), ";")
  html[i] <- js
  writeLines(html, out, useBytes = TRUE)
  message(sprintf("  %s -> %s (%.0f KB)", basename(template), out,
                  file.size(out) / 1024))
  invisible(out)
}

# ---- payload for web/depth.html ----------------------------
#  depth chart + departures + fantasy-stock signals, all keyed
#  by gsis_id (R1.13).
depth_payload <- function(dir = file.path("data", "context")) {
  d <- fromJSON(file.path(dir, "depth_sheet_2026.json"),
                simplifyVector = FALSE)
  sig_f <- file.path(dir, "fantasy_signals.json")
  d$sig <- if (file.exists(sig_f))
    fromJSON(sig_f, simplifyVector = FALSE) else list()

  # Ranking layer for the board view. ECR ships today; ADP joins in
  # the moment an export lands in data/fantasypros/ (R2.11). The
  # page is told which of the two it actually has via has_adp so
  # the ADP sort can disable itself rather than silently ordering
  # by the wrong column.
  d$rank <- list(); d$has_adp <- FALSE; d$ecr_date <- NULL
  ecr_f <- file.path(dir, "fp_ecr.csv")
  if (file.exists(ecr_f)) {
    suppressMessages({ library(dplyr); library(readr) })
    e <- read_csv(ecr_f, show_col_types = FALSE, progress = FALSE) %>%
      filter(!is.na(gsis_id))
    adp <- tryCatch({
      if (!exists("load_fp_adp")) source("fantasypros.R")
      load_fp_adp(quiet = TRUE)
    }, error = function(e) NULL)
    if (!is.null(adp) && "gsis_id" %in% names(adp)) {
      a <- adp %>% filter(!is.na(gsis_id)) %>%
        select(gsis_id, adp) %>% distinct(gsis_id, .keep_all = TRUE)
      e <- e %>% left_join(a, by = "gsis_id")
      d$has_adp <- any(!is.na(e$adp))
    } else e$adp <- NA_real_
    d$ecr_date <- as.character(max(e$scrape_date, na.rm = TRUE))
    d$rank <- e %>%
      transmute(gsis_id, player, pos, team,
                ecr1 = round(ecr_1qb, 1), ecr2 = round(ecr_2qb, 1),
                r1 = rank_1qb, r2 = rank_2qb,
                adp = if (is.numeric(adp)) round(adp, 1) else NA_real_,
                id_source) %>%
      purrr::transpose()
  }
  d
}
