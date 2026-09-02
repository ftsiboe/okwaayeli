# article_helpers.R
# Article layer: paths, formatting helpers, presence assertions.
#
# A LIBRARY, NOT A STEP: sourced by 301, 302 and narrative/land-tenure.Rmd, so it
# has no position in a sequence and carries no number. See scripts/README.md.
#
# PATHS come from scripts/_paths.R and are ABSOLUTE. They used to be written
# relative to the okwaayeli repo root, which broke twice over: it assumed the
# monorepo layout, and it assumed a working directory, when knitr moves that to
# the document's own directory mid-render. Absolute paths resolved once, from
# the location of the scripts themselves, hold in every caller.
if (!exists("PROJECT_ROOT")) {
  local({
    me <- NULL
    cl <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(cl)) me <- sub("^--file=", "", cl[1])
    if (is.null(me))
      # INNERMOST frame first. sys.frame(1) is the OUTERMOST call, so a forward
      # loop returns whichever file started the chain -- sections/_setup.R when a
      # section is knitted alone -- and then looks for _paths.R beside THAT,
      # in sections/, where it is not. Walking back from the innermost frame
      # returns this file's own path, which is what "beside this file" needs.
      for (i in rev(seq_len(sys.nframe()))) {
        of <- sys.frame(i)$ofile
        if (!is.null(of)) { me <- of; break }
      }
    # _paths.R sits beside this file; fall back to a search if we cannot see
    # our own path (an eval(parse(readLines(...))) caller, for instance).
    # "../../scripts" covers a knit whose working directory is narrative/sections.
    cand <- c(if (!is.null(me)) file.path(dirname(normalizePath(me, winslash = "/",
                                                                mustWork = FALSE)), "_paths.R"),
              "scripts/_paths.R", "../scripts/_paths.R", "../../scripts/_paths.R",
              "studies/land_tenure/scripts/_paths.R")
    hit <- cand[file.exists(cand)]
    if (!length(hit))
      stop("article_helpers.R: cannot find scripts/_paths.R", call. = FALSE)
    source(hit[1])
  })
}

# sprintf-based rounding so inline text matches the tables' sprintf("%.3f", ...)
# cells exactly (formatC and sprintf can disagree on halfway values, e.g.
# 0.0875 -> formatC "0.087" vs sprintf "0.088").
fmt_num <- function(x, digits = 2)
  formatC(as.numeric(sprintf(paste0("%.", digits, "f"), x)),
          format = "f", digits = digits, big.mark = ",")
fmt_pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
fmt_abs_pct <- function(x, digits = 1) paste0(formatC(abs(100 * x), format = "f", digits = digits), "%")

# Stop if any requested object is missing / NA (guards the knit against silent gaps).
assert_present <- function(x, name = deparse(substitute(x))) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) {
    stop(sprintf("assert_present(): '%s' is missing or NA.", name), call. = FALSE)
  }
  invisible(x)
}

# EXHIBIT NUMBERING ----------------------------------------------------------
# Exhibit numbers in the prose used to be typed literally ("Table S5"), so
# inserting or reordering one exhibit silently invalidated every reference to
# the ones after it, in every section, with nothing to catch the drift. EXHIBITS
# below is the single source of truth: an exhibit's number is its POSITION in
# its series, so the prose (xref) and the headings in 98_/99_ (exhibit_head)
# renumber together the moment an entry moves.
#
# Series carry a start index because the numbering is not uniform: the
# supplementary tables open at S0 (the tenure-construction table), while every
# other series opens at 1. Order within a vector IS the document order.
.exhibit_series <- list(
  table    = list(kind = "Table",  mark = "",  start = 1),
  figure   = list(kind = "Figure", mark = "",  start = 1),
  table_s  = list(kind = "Table",  mark = "S", start = 0),
  figure_s = list(kind = "Figure", mark = "S", start = 1),
  note_s   = list(kind = "Note",   mark = "S", start = 1)
)

EXHIBITS <- list(
  table = c(
    summary_stats      = "Summary statistics of crop producers in Ghana (1991-2017)",
    ownership_details  = "Farmland ownership details amongst Ghanaian crop farmers (2012-2017)",
    input_elasticities = "Land ownership impact on input elasticities and variability in crop production in Ghana (1991-2017)",
    parity_matched     = "Parity in technology level and technical efficiency by farmland ownership details in Ghana (1991-2017)"
  ),
  figure = c(
    input_te         = "Crop production input and output gaps associated with farmland ownership in Ghana (1991-2017)",
    score_trend      = "The association between farmland ownership and farm productivity from 1991-2017",
    het_gender_age   = "Farmland ownership parity in crop production technology adoption and technical efficiency by farmer gender, age, and education in Ghana (1991-2017)",
    het_crop_region  = "Farmland ownership parity in crop production technology adoption and technical efficiency by major crops and administrative regions in Ghana (1991-2017)"
  ),
  table_s = c(
    tenure_construction  = "Construction of the farmland tenure indicators from the GLSS Section 8b plot rosters",
    ownership_status     = "Farmland ownership status amongst Ghanaian crop farmers (2012-2017)",
    acquisition_mode     = "Farmland mode of acquisition amongst Ghanaian crop farmers (2012-2017)",
    ownership_rights     = "Farmland ownership rights amongst Ghanaian crop farmers (2012-2017)",
    sharecropping        = "Farmland sharecropping agreements amongst Ghanaian crop farmers (2012-2017)",
    msf_results          = "Mean meta stochastic frontier analysis results for Ghanaian crop producers from 1991-2017",
    parity_unmatched     = "Parity in technology level and technical efficiency by farmland ownership details in Ghana (1991-2017) - unmatched estimates",
    inefficiency_drivers = "Determinants of crop production technical inefficiency and land-ownership-driven technology gaps in Ghana (1991-2017)"
  ),
  figure_s = c(
    covariate_balance = "Covariate balancing summary",
    robustness        = "Alternative model specifications generally show a decreasing impact of land ownership on crop production output for Ghanaian farmers (1991-2017)"
  ),
  note_s = c(
    inefficiency_note = "Drivers of technical inefficiency"
  )
)

# Locate an id and stop loudly if it is unknown: a typo'd id must fail the knit,
# not render as an empty string or a stale number nobody notices in proof.
.exhibit_find <- function(id) {
  for (s in names(EXHIBITS)) {
    i <- match(id, names(EXHIBITS[[s]]))
    if (!is.na(i)) {
      spec <- .exhibit_series[[s]]
      return(list(series = s, label = paste0(spec$mark, spec$start + i - 1L),
                  kind = spec$kind, title = unname(EXHIBITS[[s]][i])))
    }
  }
  stop(sprintf("xref(): unknown exhibit id '%s'. Known ids: %s", id,
               paste(unlist(lapply(EXHIBITS, names)), collapse = ", ")),
       call. = FALSE)
}

# xref("msf_results")                             -> "Table S5"
# xref(c("input_te", "score_trend"))              -> "Figures 1 and 2"
# xref(c("ownership_status", "sharecropping"), range = TRUE) -> "Tables S1-S4"
# xref("msf_results", num = TRUE)                 -> "S5" (bare, for odd phrasings)
#
# range = TRUE names the endpoints and spans everything between, so a reference
# to a block of exhibits keeps meaning what it said when one is inserted into
# the middle of that block.
xref <- function(id, num = FALSE, range = FALSE) {
  hits <- lapply(id, .exhibit_find)
  labs <- vapply(hits, `[[`, character(1), "label")
  join <- if (range) "–" else " and "
  if (range && length(labs) != 2L)
    stop("xref(range = TRUE): needs exactly two ids, the first and the last.",
         call. = FALSE)
  if (num) return(paste(labs, collapse = join))
  kinds <- unique(vapply(hits, `[[`, character(1), "kind"))
  if (length(kinds) > 1L)
    stop("xref(): cannot combine tables and figures in one reference.", call. = FALSE)
  kind <- if (length(labs) > 1L) paste0(kinds, "s") else kinds
  paste(kind, paste(labs, collapse = join))
}

# The exhibit's own heading: "Table S5. Mean meta stochastic frontier ..."
# Used in 98_/99_ so a heading and every reference to it cannot disagree.
exhibit_head <- function(id) {
  h <- .exhibit_find(id)
  paste0(h$kind, " ", h$label, ". ", h$title)
}

# A lint, not a knit-time guard. Every heading and reference in sections/ now
# comes from EXHIBITS, so the only way the numbering can go stale again is
# someone typing "Table 5" into the prose by hand. This finds exactly that:
# literal exhibit references left anywhere in the sections. Fenced chunks and
# HTML comments are skipped, since a number in a code comment is not a claim
# the reader ever sees. Returns the offending "file:line: text" lines.
exhibit_check <- function(dir = file.path(NARRATIVE, "sections")) {
  files <- list.files(dir, pattern = "[.]Rmd$", full.names = TRUE)
  hits <- unlist(lapply(files, function(f) {
    tx <- readLines(f, warn = FALSE)
    keep <- rep(TRUE, length(tx))
    in_chunk <- FALSE; in_comment <- FALSE
    for (i in seq_along(tx)) {
      if (grepl("^```", tx[i])) { in_chunk <- !in_chunk; keep[i] <- FALSE; next }
      if (grepl("<!--", tx[i])) in_comment <- TRUE
      keep[i] <- !in_chunk && !in_comment
      if (grepl("-->", tx[i])) in_comment <- FALSE
    }
    i <- which(keep & grepl("(Table|Figure|Note)s? S?[0-9]", tx))
    if (!length(i)) return(NULL)
    sprintf("%s:%d: %s", basename(f), i, trimws(substr(tx[i], 1, 90)))
  }))
  if (length(hits))
    warning("exhibit_check(): hand-typed exhibit references (use xref()):\n  ",
            paste(hits, collapse = "\n  "), call. = FALSE)
  invisible(hits)
}
