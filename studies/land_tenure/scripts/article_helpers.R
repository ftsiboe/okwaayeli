# article_helpers.R
# Article layer: repo-root-relative paths, formatting helpers, presence assertions.
#
# A LIBRARY, NOT A STEP: sourced by 301, 302 and narrative/land-tenure.Rmd, so it
# has no position in a sequence and carries no number. See scripts/README.md.
#
# NB the paths below are REPO-ROOT-relative. knitr sets the working directory to
# narrative/ during a render, so anything sourced by the Rmd must resolve its own
# paths -- see .STUDY_ROOT in exhibit_helpers_tables.R. Do not reach for OUTPUT
# from a file the Rmd sources.
#
# RESULTS_XLSX was removed on 2026-07-15: no exhibit reads the workbook any more,
# and a defined-but-unused path is an invitation to reintroduce the dependency.

STUDY        <- "studies/land_tenure"
DATA         <- file.path(STUDY, "data")
OUTPUT       <- file.path(STUDY, "output")
FIGURE       <- file.path(OUTPUT, "figures")   # v2 layout; see ?study_dirs
TABLES       <- file.path(OUTPUT, "tables")
NARRATIVE    <- file.path(STUDY, "narrative")
OBJECTS_JSON <- file.path(NARRATIVE, "article_objects.json")

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
