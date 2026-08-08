# 000_initialize.R
# Study bootstrap: load the okwaayeli package (helpers live in R/), create the
# output/ tree, set global options. Working directory is always the repo root.

options(scipen = 999)
set.seed(20250101)

STUDY <- "studies/financial_inclusion"

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
invisible(study_dirs(project_name = "financial_inclusion", layout = "v2"))

invisible(TRUE)
