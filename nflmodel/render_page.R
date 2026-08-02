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
  js <- paste0("const D=", toJSON(data, auto_unbox = TRUE, na = "null",
                                 digits = 4), ";")
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
  d
}
