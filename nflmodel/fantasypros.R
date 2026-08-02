# ============================================================
#  fantasypros.R  (nflmodel)
#
#  FantasyPros consensus rankings (ECR) + a drop-in slot for real
#  ADP. Rule R2.11.
#
#  TWO THINGS, DELIBERATELY KEPT APART
#
#  1. ECR — expert consensus rank. Fetched automatically from the
#     DynastyProcess mirror on raw.githubusercontent.com, which is
#     the same source nflreadr's load_ff_rankings() uses. Public,
#     reachable from this sandbox, refreshed daily upstream.
#
#  2. ADP — average draft position. NOT fetched. fantasypros.com
#     is unreachable here (curl returns 000, like every non-GitHub
#     host), and ADP is not mirrored anywhere on GitHub that I
#     could find. It arrives as a user export dropped into
#     data/fantasypros/ and is read by load_fp_adp().
#
#  These are NOT interchangeable and the code never substitutes
#  one for the other. ECR is what analysts SAY; ADP is what
#  drafters DO. They diverge exactly where it matters — recency
#  bias, name-brand inertia, post-hype discounts — so the GAP
#  between them is signal. Collapsing them destroys it. Anything
#  downstream that wants ADP must fail loudly when only ECR is
#  present rather than quietly ranking off the wrong column.
#
#  ID DISCIPLINE (R1.13): FantasyPros publishes its own player id
#  (`fp_id`), and DynastyProcess publishes a crosswalk carrying
#  both `fantasypros_id` and `gsis_id`. Every join here goes
#  through that bridge. Name matching is a labeled fallback for
#  the tail only.
#
#  Output: data/context/fp_ecr.csv   (ECR joined to gsis_id)
#          data/context/fp_xwalk.csv (fantasypros_id <-> gsis_id)
# ============================================================

suppressMessages({
  library(dplyr); library(readr); library(stringr)
})

FP_DIR    <- file.path("data", "context")
FP_ECR    <- file.path(FP_DIR, "fp_ecr.csv")
FP_XWALK  <- file.path(FP_DIR, "fp_xwalk.csv")
FP_DROPIN <- file.path("data", "fantasypros")

# DynastyProcess mirrors. github.com/... release paths 403 from
# this sandbox; raw.githubusercontent.com does not (S23).
DP_BASE  <- "https://raw.githubusercontent.com/dynastyprocess/data/master/files"
DP_ECR   <- paste0(DP_BASE, "/values-players.csv")
DP_IDS   <- paste0(DP_BASE, "/db_playerids.csv")

.fp_fetch <- function(url, dest, max_age_hrs = 20) {
  if (file.exists(dest) &&
      difftime(Sys.time(), file.mtime(dest), units = "hours") < max_age_hrs) {
    message("  ", basename(dest), ": cache fresh"); return(TRUE)
  }
  ok <- tryCatch({
    download.file(url, dest, mode = "wb", quiet = TRUE); TRUE
  }, error = function(e) { message("  fetch failed: ", e$message); FALSE })
  message("  ", basename(dest), ": ", if (ok) "refreshed" else "kept cache")
  ok
}

# ── crosswalk: fantasypros_id <-> gsis_id ────────────────────
refresh_fp_xwalk <- function() {
  dir.create(FP_DIR, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(fileext = ".csv")
  if (!.fp_fetch(DP_IDS, tmp, max_age_hrs = 0) && !file.exists(FP_XWALK))
    stop("no crosswalk and no cache")
  if (file.exists(tmp) && file.size(tmp) > 0) {
    x <- read_csv(tmp, show_col_types = FALSE, progress = FALSE) %>%
      filter(!is.na(fantasypros_id), !is.na(gsis_id)) %>%
      transmute(fp_id = as.character(fantasypros_id), gsis_id,
                fp_name = name, fp_pos = position, fp_team = team) %>%
      distinct(fp_id, .keep_all = TRUE)
    write_csv(x, FP_XWALK)
  }
  .read_xwalk()
}

# fp_id is an opaque identifier, not a number — read it as text or
# a CSV round-trip silently retypes it to double and breaks joins.
.read_xwalk <- function()
  read_csv(FP_XWALK, col_types = cols(fp_id = col_character(),
                                      .default = col_guess()),
           progress = FALSE)

# ── ECR, joined to gsis_id ───────────────────────────────────
refresh_fp_ecr <- function() {
  dir.create(FP_DIR, recursive = TRUE, showWarnings = FALSE)
  xw  <- refresh_fp_xwalk()
  tmp <- tempfile(fileext = ".csv")
  got <- .fp_fetch(DP_ECR, tmp, max_age_hrs = 0)
  if (!got) {
    if (file.exists(FP_ECR)) return(invisible(read_csv(FP_ECR,
                                    show_col_types = FALSE)))
    stop("ECR unavailable and no cache")
  }
  raw <- read_csv(tmp, show_col_types = FALSE, progress = FALSE) %>%
    mutate(fp_id = as.character(fp_id))

  e <- raw %>%
    left_join(xw %>% select(fp_id, gsis_id), by = "fp_id") %>%
    transmute(gsis_id, fp_id, player, pos, team,
              age, draft_year,
              ecr_1qb, ecr_2qb, ecr_pos,       # 2qb = superflex
              value_1qb, value_2qb,
              scrape_date,
              id_source = ifelse(is.na(gsis_id), "unmatched", "fp_id"))

  # The 2026 rookie class is absent from the DynastyProcess
  # crosswalk entirely — not merely missing a gsis_id, so there is
  # no pfr/espn/sleeper hop to chain through either. Our own
  # roster_2026 does carry their gsis_id, so this is the documented
  # R1.13 case: no ID exists on one side. Match on name AND
  # position, accept only unambiguous hits, and label the row so a
  # name match is never mistaken downstream for an ID match.
  e <- .fp_rookie_fallback(e)

  # overall ranks derived from ECR so downstream code never has to
  # re-sort; ties broken by positional rank
  e <- e %>% arrange(ecr_1qb, ecr_pos) %>%
    mutate(rank_1qb = row_number()) %>%
    arrange(ecr_2qb, ecr_pos) %>%
    mutate(rank_2qb = row_number()) %>%
    arrange(rank_1qb)

  write_csv(e, FP_ECR)
  m <- sum(!is.na(e$gsis_id))
  message(sprintf("  fp ecr: %d players, %d id-matched (%.0f%%), scraped %s -> %s",
                  nrow(e), m, 100 * m / nrow(e),
                  as.character(max(e$scrape_date, na.rm = TRUE)), FP_ECR))
  invisible(e)
}

.fp_rookie_fallback <- function(e) {
  rp <- file.path("data", "players", "roster_2026.csv")
  if (!file.exists(rp)) return(e)
  # Strip generational suffixes before collapsing: FantasyPros
  # writes "Omar Cooper Jr." where the roster has "Omar Cooper",
  # and the raw collapse turns that into a miss. Ambiguity is still
  # rejected below, so a genuine father/son pair drops out rather
  # than mismatching.
  nk <- function(x) gsub("[^a-z]", "", tolower(
    sub("\\s+(jr|sr|ii|iii|iv|v)\\.?\\s*$", "", trimws(x), ignore.case = TRUE)))
  pgrp <- function(p) case_when(p == "QB" ~ "QB", p %in% c("RB","FB") ~ "RB",
                                p == "WR" ~ "WR", p == "TE" ~ "TE",
                                TRUE ~ NA_character_)
  r <- read_csv(rp, show_col_types = FALSE, progress = FALSE) %>%
    filter(!is.na(gsis_id)) %>%
    transmute(g2 = gsis_id, k = paste0(nk(full_name), "|", pgrp(position))) %>%
    filter(!is.na(k)) %>%
    add_count(k) %>% filter(n == 1) %>% select(-n)   # unambiguous only

  e %>% mutate(k = paste0(nk(player), "|", pgrp(pos))) %>%
    left_join(r, by = "k") %>%
    mutate(id_source = ifelse(is.na(gsis_id) & !is.na(g2),
                              "name-fallback (rookie)", id_source),
           gsis_id = coalesce(gsis_id, g2)) %>%
    select(-k, -g2)
}

load_fp_ecr <- function() {
  if (!file.exists(FP_ECR))
    stop("No ECR cache — run refresh_fp_ecr() first.")
  read_csv(FP_ECR, show_col_types = FALSE, progress = FALSE)
}

# ── ADP drop-in ──────────────────────────────────────────────
#  Reads any CSV in data/fantasypros/ whose name starts with
#  "adp". Required column: `adp`. A FantasyPros export already
#  carries Rank/Player/Team/POS/AVG — see the README for the
#  mapping. Joins on fp_id when the export has one, otherwise on
#  a normalized name, and LABELS which path each row took so a
#  name-matched row is never mistaken for an id-matched one.
load_fp_adp <- function(quiet = FALSE) {
  f <- list.files(FP_DROPIN, pattern = "^adp.*\\.csv$",
                  full.names = TRUE, ignore.case = TRUE)
  if (!length(f)) {
    if (!quiet) message("  no ADP export in ", FP_DROPIN,
                        " — see its README (ECR is NOT a substitute)")
    return(NULL)
  }
  nk <- function(x) gsub("[^a-z]", "", tolower(x))
  xw <- if (file.exists(FP_XWALK)) .read_xwalk() else NULL

  d <- bind_rows(lapply(f, function(p) {
    x <- read_csv(p, show_col_types = FALSE, progress = FALSE)
    names(x) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(x)))
    # tolerate the common export headers
    ren <- c(avg = "adp", average = "adp", adp_ = "adp",
             player_name = "player", name = "player",
             pos = "position", team_ = "team")
    for (a in names(ren)) if (a %in% names(x) && !ren[[a]] %in% names(x))
      names(x)[names(x) == a] <- ren[[a]]
    if (!"adp" %in% names(x))
      stop("`", basename(p), "` has no adp/AVG column")
    x$source_file <- basename(p)
    x
  }))

  if (!"fp_id" %in% names(d)) d$fp_id <- NA_character_
  d <- d %>% mutate(fp_id = as.character(fp_id))

  if (!is.null(xw)) {
    d <- d %>%
      left_join(xw %>% select(fp_id, gsis_id), by = "fp_id") %>%
      mutate(id_source = ifelse(is.na(gsis_id), NA_character_, "fp_id"))
    miss <- is.na(d$gsis_id)
    if (any(miss) && "player" %in% names(d)) {          # labeled fallback
      byname <- xw %>% mutate(nk = nk(fp_name)) %>%
        distinct(nk, .keep_all = TRUE) %>% select(nk, g2 = gsis_id)
      d$nk <- nk(d$player)
      d <- d %>% left_join(byname, by = "nk") %>%
        mutate(gsis_id = coalesce(gsis_id, g2),
               id_source = coalesce(id_source,
                            ifelse(is.na(g2), NA_character_, "name-fallback"))) %>%
        select(-g2, -nk)
    }
  }
  if (!quiet) {
    n <- nrow(d); m <- sum(!is.na(d$gsis_id))
    message(sprintf("  fp adp: %d rows from %d file(s), %d id-matched (%.0f%%)",
                    n, length(f), m, 100 * m / max(n, 1)))
  }
  d
}

# ── the ECR/ADP gap, once BOTH exist ─────────────────────────
#  The whole point of carrying both. Positive `gap` = the market
#  drafts him later than the experts rank him.
fp_market_gap <- function() {
  adp <- load_fp_adp(quiet = TRUE)
  if (is.null(adp)) {
    message("  no ADP present — gap needs both series, refusing to ",
            "approximate it from ECR alone")
    return(NULL)
  }
  load_fp_ecr() %>%
    select(gsis_id, player, pos, team, rank_1qb) %>%
    inner_join(adp %>% filter(!is.na(gsis_id)) %>%
                 select(gsis_id, adp) %>% distinct(gsis_id, .keep_all = TRUE),
               by = "gsis_id") %>%
    mutate(gap = adp - rank_1qb) %>%
    arrange(desc(abs(gap)))
}

if (sys.nframe() == 0) {
  refresh_fp_ecr()
  invisible(load_fp_adp())
}
