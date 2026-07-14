# 302_render_article.R
# Knit the master narrative Rmd to .docx (+ .html).
# Working directory is always the okwaayeli repo root.
if (!exists("NARRATIVE")) source("replications/resource_extraction/scripts/300_article_helpers.R")

rmarkdown::render(
  input         = file.path(NARRATIVE, "resource-extraction.Rmd"),
  output_format = c("word_document", "html_document"),
  knit_root_dir = normalizePath(NARRATIVE)
)
