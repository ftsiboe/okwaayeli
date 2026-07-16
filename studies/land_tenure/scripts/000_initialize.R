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
# here. A second copy of the names drifts from the first, and the failure is
# silent: the tree looks right while every write lands elsewhere.
invisible(study_dirs(project_name = "land_tenure", layout = "v2"))

invisible(TRUE)
