# 302_render_article.R
# Knit the master narrative Rmd to .docx (+ .html).
# Working directory is always the okwaayeli repo root.
if (!exists("NARRATIVE")) source("studies/land_tenure/scripts/article_helpers.R")

# ---- Citation style switch --------------------------------------------------
# The master Rmd reads Sys.getenv("ARTICLE_CSL"). Set it here (or in the calling
# session) to switch styles; both files live in narrative/csl/.
#   Elsevier Harvard (author-date): "csl/elsevier-harvard.csl"  [default]
#   IEEE (numbered):                "csl/ieee.csl"
Sys.setenv(ARTICLE_CSL = Sys.getenv("ARTICLE_CSL", unset = "csl/elsevier-harvard.csl"))

# officedown::rdocx_document replaces word_document so that wide exhibits can
# be wrapped in landscape sections (see BLOCK_LANDSCAPE markers in 98/99).
if (!requireNamespace("officedown", quietly = TRUE))
  stop("302_render_article.R needs 'officedown': install.packages(\"officedown\")",
       call. = FALSE)

rmarkdown::render(
  input         = file.path(NARRATIVE, "land-tenure.Rmd"),
  output_format = c("officedown::rdocx_document", "html_document"),
  knit_root_dir = normalizePath(NARRATIVE)
)
