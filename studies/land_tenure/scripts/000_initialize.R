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

# Create the directory tree. Delegated to study_dirs() -- do not list folders
# here. This file, study_setup() and ~40 call sites each held their own copy of
# the names once, and they drifted: the tree said output/figures while every
# write went to output/figure. One list now, in ?study_dirs.
invisible(study_dirs(project_name = "land_tenure", layout = "v2"))

invisible(TRUE)
