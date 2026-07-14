# 301_article_objects.R
# Assemble the numbers/objects the narrative pulls from, write article_objects.json.
# Working directory is always the okwaayeli repo root.
if (!exists("OBJECTS_JSON")) source("replications/resource_extraction/scripts/300_article_helpers.R")

# TODO: populate object groups from output/estimations, output/matching, and
# output/exhibits/figure_data as the article is parametrized. For now write a
# minimal object so the master Rmd knits before any real numbers are wired in.
#
# TODO object groups (to be filled):
#   - meta_efficiency (TGR/TE/MTE: extraction vs non-extraction)
#   - elasticities (land, labour, capital, variable inputs)
#   - heterogeneity (by extraction activity, crop, region, gender, age, education)
#   - robustness (alternate distributions, specifications)
#   - inline tables for 98_tables_and_figures

objs <- list(
  meta = list(generated = as.character(Sys.time()))
)

jsonlite::write_json(objs, OBJECTS_JSON, auto_unbox = TRUE, pretty = TRUE)
invisible(TRUE)
