# 302_render_article.R
# Knit the master narrative Rmd to .docx (+ .html).
# Working directory is always the okwaayeli repo root.
if (!exists("NARRATIVE")) source("studies/financial_inclusion/scripts/article_helpers.R")

# ---- Citation style switch --------------------------------------------------
# The master Rmd reads Sys.getenv("ARTICLE_CSL"). Set it here (or in the calling
# session) to switch styles; both files live in narrative/csl/.
#   IEEE (numbered):                "csl/ieee.csl"               [default]
#   Elsevier Harvard (author-date): "csl/elsevier-harvard.csl"
# run_article.R sets ARTICLE_CSL from CITATION_STYLE before sourcing this, so
# the unset-fallback only bites when 302 is run on its own. Keep it in step with
# CITATION_STYLE's default, or the two entry points disagree silently.
Sys.setenv(ARTICLE_CSL = Sys.getenv("ARTICLE_CSL", unset = "csl/ieee.csl"))

# officedown::rdocx_document replaces word_document so that wide exhibits can
# be wrapped in landscape sections (see BLOCK_LANDSCAPE markers in 98/99).
if (!requireNamespace("officedown", quietly = TRUE))
  stop("302_render_article.R needs 'officedown': install.packages(\"officedown\")",
       call. = FALSE)

rmarkdown::render(
  input         = file.path(NARRATIVE, "financial-inclusion.Rmd"),
  output_format = c("officedown::rdocx_document", "html_document"),
  knit_root_dir = normalizePath(NARRATIVE)
)
