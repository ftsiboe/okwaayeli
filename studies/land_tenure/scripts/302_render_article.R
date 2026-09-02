# 302_render_article.R
# Knit the master narrative Rmd to .docx (+ .html).
#
# LAYOUT. This study is checked out two ways: standalone, with narrative/ and
# scripts/ directly under the project root, and inside the okwaayeli monorepo at
# studies/land_tenure/. article_helpers.R's NARRATIVE global is hardcoded to the
# second ("studies/land_tenure/narrative"), so sourcing that file for it fails
# outright in a standalone checkout -- "cannot open file
# 'studies/land_tenure/scripts/article_helpers.R'". Resolve the path here
# instead, testing candidates for the master Rmd itself, which is the same
# reason 303_render_tex.R keeps its own root finder rather than reusing those
# globals. Everything else the render needs is sourced by the Rmd, relative to
# knit_root_dir below.
NARRATIVE <- local({
  cand <- c(if (exists("NARRATIVE")) NARRATIVE,
            "narrative", "studies/land_tenure/narrative", "../narrative")
  for (p in cand)
    if (!is.null(p) && file.exists(file.path(p, "land-tenure.Rmd"))) return(p)
  stop("302_render_article.R: could not locate narrative/land-tenure.Rmd. Run ",
       "from the study root (the folder holding narrative/ and scripts/) or ",
       "from the okwaayeli repo root.", call. = FALSE)
})

# ---- Citation style switch --------------------------------------------------
# The master Rmd reads Sys.getenv("ARTICLE_CSL"). Set it here (or in the calling
# session) to switch styles; the files live in narrative/references/csl/, and the
# value is resolved relative to narrative/ (the knit root).
#   IEEE (numbered):                "references/csl/ieee.csl"  [default]
#   Elsevier Harvard (author-date): "references/csl/elsevier-harvard.csl"
#
# IEEE is the default here, in 304, and in the Rmd's own YAML fallback, and it is
# what run_article.R's CITATION_STYLE is set to -- so every route renders the
# same. Change one and change the others, or the same manuscript ships with
# different reference formatting depending on how it was built.
#
# The narrative cites in-text as "Fenske [-@Fenske2011] finds", which renders as
# "Fenske [12] finds" under IEEE and "Fenske (2011) finds" under an author-date
# style. Both read correctly; a bare "@Fenske2011" would not, so do not
# "simplify" those back.
Sys.setenv(ARTICLE_CSL = Sys.getenv("ARTICLE_CSL", unset = "references/csl/ieee.csl"))

# officedown::rdocx_document replaces word_document so that wide exhibits can
# be wrapped in landscape sections (see BLOCK_LANDSCAPE markers in 98/99).
if (!requireNamespace("officedown", quietly = TRUE))
  stop("302_render_article.R needs 'officedown': install.packages(\"officedown\")",
       call. = FALSE)

# output_dir keeps the deliverables in narrative/output/ alongside the LaTeX
# build, so narrative/ itself holds only sources. knit_root_dir stays at
# narrative/ -- the Rmd's own paths (assets/, references/, article_objects.json,
# sections/) are written relative to THAT, not to wherever the output lands.
rmarkdown::render(
  input         = file.path(NARRATIVE, "land-tenure.Rmd"),
  output_format = c("officedown::rdocx_document", "html_document"),
  output_dir    = file.path(NARRATIVE, "output"),
  knit_root_dir = normalizePath(NARRATIVE)
)
