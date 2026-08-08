# article_helpers.R
# Article layer: repo-root-relative paths, formatting helpers, presence assertions.
#
# A LIBRARY, NOT A STEP: sourced by 301, 302 and narrative/financial-inclusion.Rmd,
# so it has no position in a sequence and carries no number. See scripts/README.md.
#
# NB the paths below are REPO-ROOT-relative. knitr sets the working directory to
# narrative/ during a render, so anything sourced by the Rmd must resolve its own
# paths -- see .STUDY_ROOT in exhibit_helpers_tables.R.

STUDY        <- "studies/financial_inclusion"
DATA         <- file.path(STUDY, "data")
OUTPUT       <- file.path(STUDY, "output")
FIGURE       <- file.path(OUTPUT, "figures")   # v2 layout; see ?study_dirs
TABLES       <- file.path(OUTPUT, "tables")
NARRATIVE    <- file.path(STUDY, "narrative")
OBJECTS_JSON <- file.path(NARRATIVE, "article_objects.json")

# ---- The hole this closes ----------------------------------------------------
# The doctrine is NO FALLBACKS: a lookup that cannot resolve stops the knit
# rather than printing something wrong. tbl_num()/tbl_pct() honour that -- they
# error on a missing row.
#
# The formatters did not, and that left a gap wide enough to drive a paper
# through. `objs$credit$share_hh` on a list with no `credit` element is NULL, not
# an error. Then:
#
#   fmt_pct(NULL)  ->  paste0(formatC(100 * NULL, ...), "%")
#                  ->  paste0(character(0), "%")
#                  ->  "%"
#
# A bare percent sign in the sentence. No error, no warning, no missing-value
# marker -- and probe_wiring reports it "ok", because evaluating it succeeded.
# That is exactly the silent degradation the architecture exists to prevent,
# arrived at from the formatting side instead of the lookup side.
#
# Observed on 2026-08-08: five credit-component shares in the Data section
# rendered as "%" because article_objects.json predated the 301 that emits them.
# The probe passed 101/101.
#
# .scalar() closes it. Every formatter validates before it formats, and the
# error names the expression that produced the bad value, so the fix is one line
# away from the message.
.scalar <- function(x, what) {
  if (is.null(x))
    stop(sprintf("%s is NULL -- the object it indexes does not carry that key.\n%s",
                 what, .objs_hint(what)), call. = FALSE)
  if (length(x) == 0L)
    stop(sprintf("%s is empty (length 0).\n%s", what, .objs_hint(what)), call. = FALSE)
  if (length(x) > 1L)
    stop(sprintf("%s has length %d; inline text needs exactly one value.",
                 what, length(x)), call. = FALSE)
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x))
    stop(sprintf("%s is NA -- present but not computed.\n%s", what, .objs_hint(what)),
         call. = FALSE)
  x
}

.objs_hint <- function(what) {
  if (!grepl("objs\\$", what)) return("")
  paste0("  This reads article_objects.json. If 301_article_objects.R has been\n",
         "  extended since that file was written, re-run the OBJECTS stage:\n",
         "  run_article.R with OBJECTS = TRUE.")
}

# sprintf-based rounding so inline text matches the tables' sprintf("%.3f", ...)
# cells exactly (formatC and sprintf can disagree on halfway values, e.g.
# 0.0875 -> formatC "0.087" vs sprintf "0.088").
fmt_num <- function(x, digits = 2) {
  x <- .scalar(x, deparse(substitute(x)))
  formatC(as.numeric(sprintf(paste0("%.", digits, "f"), x)),
          format = "f", digits = digits, big.mark = ",")
}

fmt_pct <- function(x, digits = 1) {
  x <- .scalar(x, deparse(substitute(x)))
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

fmt_abs_pct <- function(x, digits = 1) {
  x <- .scalar(x, deparse(substitute(x)))
  paste0(formatC(abs(100 * x), format = "f", digits = digits), "%")
}

# Stop if any requested object is missing / NA (guards the knit against silent gaps).
assert_present <- function(x, name = deparse(substitute(x))) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1 && is.na(x))) {
    stop(sprintf("assert_present(): '%s' is missing, empty or NA.", name), call. = FALSE)
  }
  invisible(x)
}

# ---- N(): retained, currently unused ------------------------------------------
# N() rendered a measured quantity that was still a v005 literal, wrapped in
# visible brackets so the debt was legible in the rendered document rather than
# indistinguishable from a live value.
#
# As of 2026-08-08 NOTHING USES IT. Every measured quantity in every section is
# an inline call against the pipeline; `grep -c 'N("' narrative/sections/*.Rmd`
# returns zero across the board, and probe_wiring.R reports zero frozen.
#
# It stays because the next port needs it, and because a marker invented in a
# hurry is a marker nobody greps for. If it reappears in a section, that section
# has un-ported prose in it.
#
# Set ARTICLE_MARK_FROZEN=0 to render bare literals instead -- useful for a clean
# read-through, never for a version anyone signs.
N <- function(x) {
  if (identical(Sys.getenv("ARTICLE_MARK_FROZEN", "1"), "0")) return(as.character(x))
  sprintf("\u27e6%s\u27e7", x)
}
