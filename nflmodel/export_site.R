# ============================================================
#  export_site.R  (nflmodel)
#
#  Writes the depth-chart / draft-board data into the Next.js app
#  at the repo root, which deploys to Vercel.
#
#  The site is a STATIC export (next.config.mjs sets
#  output: "export"), so there is no server and no API route to
#  call at view time — the data has to be committed as JSON and
#  imported at build time, exactly like the league tables in
#  src/data/*.json that scripts/convert_data.py produces. This
#  script is the nflmodel-side equivalent of that converter.
#
#  FORMAT: columnar ({columns, rows}) for the three flat tables,
#  matching the house shape that src/lib/data.ts already decodes.
#  Repeating ~18 key names across 1,184 depth rows costs about
#  three times what the column list does, and this file ships to
#  every visitor.
#
#  NULL HANDLING: jsonlite writes an R NULL as `{}` by default and
#  `{}` is truthy in JavaScript, which is how 1,177 active players
#  were once published as "RESERVE" (S24). Every write here passes
#  na = "null", null = "null". Do not remove those.
#
#  Usage:  Rscript export_site.R
#  Output: ../src/data/nfl-depth.json
# ============================================================

suppressMessages({
  library(dplyr); library(readr); library(jsonlite); library(purrr)
})

SITE_DATA <- file.path("..", "src", "data")
SITE_JSON <- file.path(SITE_DATA, "nfl-depth.json")

# data.frame -> {columns, rows}
.columnar <- function(d) {
  d <- as.data.frame(d)
  # as.list() on a one-row slice gives a flat named list; passing
  # the slice itself keeps it a data.frame and toJSON nests each
  # row one level deeper than the format expects.
  list(columns = names(d),
       rows = unname(lapply(seq_len(nrow(d)),
                            function(i) unname(as.list(d[i, , drop = FALSE])))))
}

build_site_payload <- function(dir = file.path("data", "context")) {
  j <- fromJSON(file.path(dir, "depth_sheet_2026.json"))   # simplified

  depth <- j$depth %>%
    select(gsis_id, team, grp, slot, spot, player_name, status, from,
           snaps25, line_2025, line_career, avail_note, rotowire_url,
           y25_weeks_out_injury, y25_top_injury,
           any_of("blurb"))
  out <- j$out %>%
    select(gsis_id, team, grp, player_name, spot, to, snaps25,
           line_2025, line_career, y25_weeks_out_injury,
           any_of("y25_top_injury"))

  # signals stay nested — they are a variable-length list per
  # player, which columnar cannot express
  sig_f <- file.path(dir, "fantasy_signals.json")
  sig <- if (file.exists(sig_f))
    fromJSON(sig_f, simplifyVector = FALSE) else list()

  # ranking layer (R2.11): ECR now, ADP when an export lands
  rank <- NULL; has_adp <- FALSE; ecr_date <- NA_character_
  ecr_f <- file.path(dir, "fp_ecr.csv")
  if (file.exists(ecr_f)) {
    e <- read_csv(ecr_f, show_col_types = FALSE, progress = FALSE) %>%
      filter(!is.na(gsis_id))
    adp <- tryCatch({
      if (!exists("load_fp_adp")) source("fantasypros.R")
      load_fp_adp(quiet = TRUE)
    }, error = function(e) NULL)
    if (!is.null(adp) && "gsis_id" %in% names(adp)) {
      e <- e %>% left_join(adp %>% filter(!is.na(gsis_id)) %>%
                             select(gsis_id, adp) %>%
                             distinct(gsis_id, .keep_all = TRUE),
                           by = "gsis_id")
      has_adp <- any(!is.na(e$adp))
    } else e$adp <- NA_real_
    ecr_date <- as.character(max(e$scrape_date, na.rm = TRUE))
    rank <- e %>% transmute(gsis_id, player, pos, team,
                            ecr1 = round(ecr_1qb, 1),
                            ecr2 = round(ecr_2qb, 1),
                            r1 = rank_1qb, r2 = rank_2qb,
                            adp = round(as.numeric(adp), 1), id_source)
  }

  list(depth = .columnar(depth),
       out   = .columnar(out),
       rank  = if (is.null(rank)) NULL else .columnar(rank),
       sig   = sig,
       meta  = list(generated = j$generated,
                    has_pff   = isTRUE(j$has_pff),
                    has_adp   = has_adp,
                    ecr_date  = if (is.na(ecr_date)) NULL else ecr_date))
}

export_site <- function() {
  if (!dir.exists(SITE_DATA))
    stop("site data dir not found: ", normalizePath(SITE_DATA, mustWork = FALSE))
  p <- build_site_payload()
  write_json(p, SITE_JSON, auto_unbox = TRUE, na = "null", null = "null",
             digits = 4)
  message(sprintf("  site data: %d depth, %d out, %d ranked, %d flagged -> %s (%.0f KB)",
                  length(p$depth$rows), length(p$out$rows),
                  if (is.null(p$rank)) 0L else length(p$rank$rows),
                  length(p$sig), SITE_JSON, file.size(SITE_JSON) / 1024))
  invisible(p)
}

if (sys.nframe() == 0) export_site()
