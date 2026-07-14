# 302_render_article.R
# Knit the master narrative Rmd to .docx (+ .html).
# Working directory is always the okwaayeli repo root.
if (!exists("NARRATIVE")) source("replications/resource_extraction/scripts/300_article_helpers.R")

# ---- Citation style switch --------------------------------------------------
# The master Rmd reads Sys.getenv("ARTICLE_CSL"). Set it here (or in the calling
# session) to switch styles; both files live in narrative/csl/.
#   Food Policy (Elsevier Harvard, author-date):  "csl/elsevier-harvard.csl"  [default]
#   IEEE (numbered):                               "csl/ieee.csl"
Sys.setenv(ARTICLE_CSL = Sys.getenv("ARTICLE_CSL", unset = "csl/elsevier-harvard.csl"))

rmarkdown::render(
  input         = file.path(NARRATIVE, "resource-extraction.Rmd"),
  output_format = c("word_document", "html_document"),
  knit_root_dir = normalizePath(NARRATIVE)
)
