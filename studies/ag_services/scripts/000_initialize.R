tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){NULL})
# 000_initialize.R
# Study bootstrap: load the okwaayeli package (helpers live in R/), create the
# output/ tree, set global options. Working directory is always the repo root.

options(scipen = 999)
set.seed(20250101)

STUDY <- "studies/ag_services"

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
# layout = "v2" matches land_tenure: output/figures/ holds plots AND the data
# behind them, output/tables/ holds table data and workbook deliverables. The
# study was migrated from "legacy" (figure/ + figure_data/) on 2026-08-07; the
# empty legacy folders are left behind because this mount forbids rmdir.
invisible(study_dirs(project_name = "ag_services", layout = "v2"))

invisible(TRUE)
