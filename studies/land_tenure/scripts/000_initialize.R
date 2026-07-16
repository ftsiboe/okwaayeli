tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){NULL})
# 000_initialize.R
# Study bootstrap: load the okwaayeli package (helpers live in R/), create the
# output/ tree, set global options. Working directory is always the repo root.

options(scipen = 999)
set.seed(20250101)

STUDY <- "studies/land_tenure"

# Helper functions are part of the okwaayeli package (R/). Load them.
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(okwaayeli)
}

# Create the directory tree.
#
# Delegated to study_dirs() rather than listed here. Until 2026-07-16 this file
# held its own hardcoded vector, study_setup() held a second one, and ~40 call
# sites pasted the folder names as literals a third time. The three drifted:
# this file was migrated to output/figures + output/tables while study_setup()
# went on creating output/figure + output/figure_data, so the tree looked right
# and every write still went to the old names.
#
# One list, in ?study_dirs. layout = "v2" is land_tenure's; the six sibling
# studies are on "legacy".
invisible(study_dirs(project_name = "land_tenure", layout = "v2"))

invisible(TRUE)
