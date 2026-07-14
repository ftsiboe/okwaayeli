# run_article.R
# Build the article end to end: helpers -> objects -> render.
# Run from the okwaayeli repo root.

# ---- Citation style toggle -------------------------------------------------
# Choose "food_policy" (Elsevier Harvard, author-date) or "ieee" (numbered).
CITATION_STYLE <- "ieee"
Sys.setenv(ARTICLE_CSL = if (identical(CITATION_STYLE, "ieee"))
  "csl/ieee.csl" else "csl/elsevier-harvard.csl")
# ----------------------------------------------------------------------------

source("replications/resource_extraction/scripts/300_article_helpers.R")
source("replications/resource_extraction/scripts/301_article_objects.R")
source("replications/resource_extraction/scripts/302_render_article.R")
