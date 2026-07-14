# 000_initialize.R
# Study bootstrap: load the okwaayeli package (helpers live in R/), create the
# output/ tree, set global options. Working directory is always the repo root.

options(scipen = 999)
set.seed(20250101)

STUDY <- "replications/resource_extraction"

# Helper functions are part of the okwaayeli package (R/). Load them.
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(okwaayeli)
}

# Create the directory tree.
for (d in c("data", "output", "output/exhibits", "output/summary", "output/releases",
            "output/estimations", "output/matching")) {
  dir.create(file.path(STUDY, d), showWarnings = FALSE, recursive = TRUE)
}

invisible(TRUE)
