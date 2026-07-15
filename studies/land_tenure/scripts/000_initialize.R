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
for (d in c("data", "output", "output/estimations", "output/matching",
            "output/treatment_effects", "output/figure", "output/figure_data")) {
  dir.create(file.path(STUDY, d), showWarnings = FALSE, recursive = TRUE)
}

invisible(TRUE)
