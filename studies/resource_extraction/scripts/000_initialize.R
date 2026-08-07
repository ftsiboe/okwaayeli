# 000_initialize.R
# Study bootstrap: load the okwaayeli package (helpers live in R/), create the
# output/ tree, set global options. Working directory is always the repo root.

options(scipen = 999)
set.seed(20250101)

STUDY <- "studies/resource_extraction"

# Helper functions are part of the okwaayeli package (R/). Load them.
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(okwaayeli)
}

# Create the directory tree. Delegated to study_dirs() -- do not list folders
# here. A second copy of the names drifts from the first, and the failure is
# silent: the tree looks right while every write lands elsewhere.
#
# layout = "v2" must match what 001 passes to study_setup(); on "legacy" this
# would create output/figure/ + output/figure_data/ while the stages write to
# output/figures/ + output/tables/.
invisible(study_dirs(project_name = "resource_extraction", layout = "v2"))

# Study-specific extras, not part of study_dirs()' contract. `summary/` holds
# extraction_definition_sensitivity.csv; `exhibits/` and `releases/` are legacy
# and currently empty. article_helpers.R still defines EXHIBITS and SUMMARY
# constants for them, though nothing reads those constants today -- delete both
# the constants and these two lines once that is confirmed for good.
for (d in c("output/exhibits", "output/summary", "output/releases"))
  dir.create(file.path(STUDY, d), showWarnings = FALSE, recursive = TRUE)

invisible(TRUE)
