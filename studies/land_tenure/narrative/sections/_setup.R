# _setup.R
# Everything a section .Rmd needs when it is knitted ON ITS OWN.
#
# The sections are normally children of ../land-tenure.Rmd, and that parent's
# setup chunk is what reads article_objects.json and sources the two helper
# libraries. Knitting a section directly -- RStudio's Knit button on
# sections/05_results.Rmd, say -- skips the parent entirely, so fmt_num(),
# N_ALL, tbl_num() and the rest are undefined and the knit dies on the first
# inline `r ...` call with "could not find function fmt_num".
#
# This file supplies the same environment the parent would have. It is sourced
# by the guard chunk at the top of each section, which does nothing when the
# parent (or 303 / 304) has already set things up -- so it costs nothing in a
# full build and cannot make the two paths disagree.
#
# NB a section knitted alone still cannot resolve citations against the
# bibliography the way the full build does: pandoc's citeproc only fills a
# reference list where a `#refs` div lives in THAT document. Use
# scripts/304_preview_section.R when you want a preview with references.

# scripts/ holds both helper libraries. Search relative to the working
# directory, which knitr sets to the .Rmd's own folder -- sections/ when a
# section is knitted alone, narrative/ when the parent drives it.
.sec_scripts <- local({
  cand <- c("../../scripts", "../scripts", "scripts",
            "studies/land_tenure/scripts")
  hit  <- cand[file.exists(file.path(cand, "article_helpers.R"))]
  if (!length(hit))
    stop("sections/_setup.R: cannot find scripts/article_helpers.R from ",
         getwd(), call. = FALSE)
  normalizePath(hit[1], winslash = "/")
})

# article_helpers.R pulls in scripts/_paths.R, so PROJECT_ROOT / DATA / OUTPUT /
# NARRATIVE land as absolute paths and stay correct even though knitr has moved
# the working directory.
source(file.path(.sec_scripts, "article_helpers.R"))
source(file.path(.sec_scripts, "exhibit_helpers_tables.R"))

# The pre-computed numbers the prose interpolates (scripts/301 writes this).
if (!exists("objs")) {
  .aoj <- file.path(NARRATIVE, "article_objects.json")
  if (!file.exists(.aoj))
    stop("sections/_setup.R: missing ", .aoj,
         "\n  Run: Rscript scripts/301_article_objects.R", call. = FALSE)
  objs <- jsonlite::fromJSON(.aoj)
}

invisible(TRUE)
