# 302_render_article.R  (3## = article; see scripts/README.md)
# Knit the master narrative Rmd to .docx (+ .html).
# Working directory is always the okwaayeli repo root.
#
# The render itself is three lines. Everything above it is pre-flight, because
# every input this document has can go missing WITHOUT the render failing:
#
#   * a missing figure  -> pandoc emits the document with a broken image link
#   * a missing article_objects.json -> the Rmd's setup chunk skips it silently
#     (`if (file.exists(...))`), and `objs` simply does not exist
#   * a missing article_helpers.R -> same silent skip
#   * AG_PREVIEW left set from an earlier session -> a document full of yellow
#     PLACEHOLDER boxes that renders perfectly and says nothing
#
# Each of those produces a .docx that looks finished. So each is checked here,
# before the render, and named.

if (!exists("NARRATIVE"))
  source("studies/ag_services/scripts/article_helpers.R")

MASTER <- file.path(NARRATIVE, "ag-services.Rmd")
if (!file.exists(MASTER))
  stop("302: no master Rmd at ", MASTER, call. = FALSE)

# ---- Citation style switch --------------------------------------------------
# The master Rmd reads Sys.getenv("ARTICLE_CSL"). Set it here (or in the calling
# session) to switch styles; both files live in narrative/csl/.
#   Elsevier Harvard (author-date): "csl/elsevier-harvard.csl"  [default]
#   IEEE (numbered):                "csl/ieee.csl"
Sys.setenv(ARTICLE_CSL = Sys.getenv("ARTICLE_CSL",
                                    unset = "csl/elsevier-harvard.csl"))

# ---- Pre-flight: the document's own dependencies -----------------------------
.need_file <- function(p, what) {
  if (!file.exists(p)) stop("302: missing ", what, "\n  ", p, call. = FALSE)
  invisible(TRUE)
}
.need_file(file.path(NARRATIVE, "reference.docx"), "the Word reference document")
.need_file(file.path(NARRATIVE, "references.bib"), "the bibliography")
.need_file(file.path(NARRATIVE, Sys.getenv("ARTICLE_CSL")),
           paste0("the citation style (ARTICLE_CSL=",
                  Sys.getenv("ARTICLE_CSL"), ")"))

# ---- Pre-flight: AG_PREVIEW --------------------------------------------------
# Not an error. Previewing the layout before the builders exist is exactly what
# the flag is for, and right now twelve of fifteen ft_*() builders are stubs, so
# a preview render is the ONLY render that completes. But the resulting document
# must never be mistaken for a draft anyone can read, so it says so, loudly,
# every time.
PREVIEW <- identical(Sys.getenv("AG_PREVIEW"), "1")
if (PREVIEW)
  message("\n",
          "  ############################################################\n",
          "  #  AG_PREVIEW=1 -- THIS IS A LAYOUT PREVIEW, NOT A DRAFT.   #\n",
          "  #  Unwritten exhibits render as PLACEHOLDER boxes carrying  #\n",
          "  #  no data. Do not circulate the output.                    #\n",
          "  ############################################################\n")

# ---- Pre-flight: article_objects.json ----------------------------------------
# The Rmd loads it only `if (file.exists(...))`, so its absence is invisible
# until a prose chunk reaches for `objs` -- which, with the prose still stubbed,
# is nowhere. Report it now instead of discovering it when the first sentence
# citing a number is written.
if (!file.exists(OBJECTS_JSON)) {
  message("302: no ", OBJECTS_JSON, " -- `objs` will not exist in the knit.\n",
          "     Harmless while the prose is stubbed; run 301 before writing a ",
          "sentence that cites a number.")
} else {
  .est <- list.files(file.path(OUTPUT, "estimations"), pattern = "[.]rds$",
                     full.names = TRUE)
  if (length(.est) && file.mtime(OBJECTS_JSON) < max(file.mtime(.est)))
    message("302: STALE -- ", basename(OBJECTS_JSON), " (",
            format(file.mtime(OBJECTS_JSON), "%Y-%m-%d %H:%M"),
            ") predates the estimation objects (",
            format(max(file.mtime(.est)), "%Y-%m-%d %H:%M"), ").\n",
            "     Re-run 301, or the text quotes one build while the tables ",
            "print another.")
}

# ---- Pre-flight: every figure the document embeds ----------------------------
# Parsed out of the section files rather than hand-listed, so a figure added to
# 98/99 is checked without anyone remembering to update this script.
#
# A missing .png does NOT fail the render: pandoc writes the .docx with a broken
# link and Word shows an empty frame. That is the single most survivable way for
# this document to be wrong, hence a hard stop.
.secs <- list.files(file.path(NARRATIVE, "sections"), pattern = "[.]Rmd$",
                    full.names = TRUE)
.refs <- unique(unlist(lapply(.secs, function(f) {
  txt <- readLines(f, warn = FALSE)
  txt <- txt[!grepl("^\\s*<!--", txt)]          # ignore commented-out exhibits
  m <- regmatches(txt, gregexpr("\\]\\([^)]*\\.png[^)]*\\)", txt))
  m <- gsub("^\\]\\(|\\)$", "", unlist(m))
  sub("\\{.*$", "", m)                           # drop the {width=...} suffix
})))
if (length(.refs)) {
  .abs <- file.path(NARRATIVE, .refs)            # paths are narrative-relative
  .gone <- .refs[!file.exists(.abs)]
  if (length(.gone))
    stop("302: ", length(.gone), " figure(s) referenced by the document do not ",
         "exist:\n  ", paste(.gone, collapse = "\n  "),
         "\n  Run scripts/101_exhibit_figures.R (the FIGURES stage). Rendering ",
         "without them\n  produces a .docx with empty image frames and no ",
         "error.", call. = FALSE)
  message("302: ", length(.refs), " figure(s) present.")
}

# officedown::rdocx_document replaces word_document so that wide exhibits can
# be wrapped in landscape sections (see the sec_landscape() markers in 98/99).
if (!requireNamespace("officedown", quietly = TRUE))
  stop("302_render_article.R needs 'officedown': install.packages(\"officedown\")",
       call. = FALSE)

message("302: rendering ", basename(MASTER), " (csl = ",
        Sys.getenv("ARTICLE_CSL"), ")")

rmarkdown::render(
  input         = file.path(NARRATIVE, "ag-services.Rmd"),
  output_format = c("officedown::rdocx_document", "html_document"),
  knit_root_dir = normalizePath(NARRATIVE)
)

# ---- Report what landed -------------------------------------------------------
for (f in file.path(NARRATIVE, c("ag-services.docx", "ag-services.html"))) {
  if (file.exists(f))
    message("302: wrote ", f, "  (",
            format(round(file.size(f) / 1e6, 2), nsmall = 2), " MB, ",
            format(file.mtime(f), "%Y-%m-%d %H:%M"), ")")
  else
    message("302: expected ", basename(f), " but it is not on disk.")
}
if (PREVIEW)
  message("302: reminder -- AG_PREVIEW=1 was set. The output above is a layout ",
          "preview containing placeholder boxes, not a readable draft.")

invisible(TRUE)
