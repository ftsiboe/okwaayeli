# 300_article_helpers.R
# Article layer: repo-root-relative paths, formatting helpers, presence assertions.
# Working directory is always the okwaayeli repo root.

STUDY        <- "studies/resource_extraction"
DATA         <- file.path(STUDY, "data")
OUTPUT       <- file.path(STUDY, "output")
EXHIBITS     <- file.path(OUTPUT, "exhibits")
SUMMARY      <- file.path(OUTPUT, "summary")
NARRATIVE    <- file.path(STUDY, "narrative")
OBJECTS_JSON <- file.path(NARRATIVE, "article_objects.json")

# Scenario / key placeholders (fill in as the article matures).
SCENARIOS <- list()

fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
fmt_abs_pct <- function(x, digits = 1) paste0(formatC(abs(100 * x), format = "f", digits = digits), "%")

# Stop if any requested object is missing / NA (guards the knit against silent gaps).
assert_present <- function(x, name = deparse(substitute(x))) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) {
    stop(sprintf("assert_present(): '%s' is missing or NA.", name), call. = FALSE)
  }
  invisible(x)
}
