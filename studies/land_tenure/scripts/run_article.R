# run_article.R
# Build the article end to end: helpers -> objects -> render.
# Run from the okwaayeli repo root.
#
# Pipeline (see scripts/README.md for the naming convention):
#   001 -> 002 -> 003 -> 004           estimation
#   100_exhibit_descriptive_stats.R    -> data/descriptive_exhibits.rds  (~5-10 min)
#   101_exhibit_figures.R              -> output/figure/, output/figure_data/
#   run_article.R (this file)          300 -> 301 -> 302
#
# The 10x exhibit scripts are NOT sourced here: they write caches and take
# minutes, so they run when the estimates change, not on every render. That is
# the contract the retired 100_exhibits.do had.
#
# 110_exhibit_tables.R is 11x -- it only reads caches, so the Rmd sources it at
# knit time. It needs BOTH 100 (Tables 1/2/S1-S4) and 101 (figure_data, for the
# fig1_range() / trend_gap() inline lookups). If either cache is missing it
# errors with the name of the script to run.

# ---- Citation style toggle -------------------------------------------------
# Choose "elsevier" (Elsevier Harvard, author-date) or "ieee" (numbered).
CITATION_STYLE <- "elsevier"
Sys.setenv(ARTICLE_CSL = if (identical(CITATION_STYLE, "ieee"))
  "csl/ieee.csl" else "csl/elsevier-harvard.csl")
# ----------------------------------------------------------------------------

source("studies/land_tenure/scripts/300_article_helpers.R")
source("studies/land_tenure/scripts/301_article_objects.R")
source("studies/land_tenure/scripts/302_render_article.R")
