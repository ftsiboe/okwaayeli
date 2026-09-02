# 304_preview_section.R
# Standalone, compilable preview of ONE section:
# narrative/output/_preview_<id>.tex (+ .pdf). A reading copy of a single section
# while it is being drafted, not a deliverable -- 303_render_tex.R remains the
# export the manuscript ships from.
#
# WHY THIS KNITS THE SECTION ITSELF RATHER THAN REUSING 303's OUTPUT
# Reusing narrative/output/sections_tex/<id>.tex would be cheaper, but that is
# only as fresh as the last whole-paper run, so previewing an edit would show
# the text as it stood before the edit -- the one failure mode a preview must
# not have. This knits the section directly, so what you read is what the .Rmd
# currently says.
#
# The cost is that citations resolve in their own citeproc pass here rather than
# the single pass 303 runs across the whole manuscript. That is invisible for
# author-date styles; the reference list is appended so citations render against
# a real bibliography instead of as bare keys.
#
# USAGE
#   Rscript scripts/304_preview_section.R 05_results
#   Rscript scripts/304_preview_section.R            # defaults to 05_results
#   source("scripts/304_preview_section.R")          # also fine, see .this_file
#
# OUTPUT
#   narrative/output/_preview_<id>.tex   standalone, compilable on its own
#   narrative/output/_preview_<id>.pdf   compiled via tinytex::xelatex, if available

args <- commandArgs(trailingOnly = TRUE)
section_id <- if (length(args)) args[1] else "05_results"

# Root is resolved from THIS FILE's location, not from the session working
# directory. 303_render_tex.R can search "." / ".." because it is run with
# Rscript from the project root; this one is also read with source() from an
# interactive console, where the working directory is wherever the session
# happens to be. A wd-relative search there either fails to find the project or,
# worse, finds a different copy of it -- so anchor on the script instead, which
# is correct under Rscript, source(), and RStudio's Source button alike.
.this_file <- function() {
  cl <- grep("^--file=", commandArgs(FALSE), value = TRUE)   # Rscript
  if (length(cl)) return(normalizePath(sub("^--file=", "", cl[1]), winslash = "/", mustWork = FALSE))
  for (i in seq_len(sys.nframe())) {                          # source()
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(normalizePath(of, winslash = "/", mustWork = FALSE))
  }
  NULL
}
.here <- .this_file()
if (is.null(.here))
  stop("304_preview_section.R: could not determine this script's own path. ",
       "Run it with Rscript, or source() it with a real file path.", call. = FALSE)

PROJECT_ROOT  <- dirname(dirname(.here))          # <root>/scripts/304_*.R
NARRATIVE_DIR <- file.path(PROJECT_ROOT, "narrative")
SCRIPTS_DIR   <- file.path(PROJECT_ROOT, "scripts")
if (!dir.exists(NARRATIVE_DIR))
  stop("304_preview_section.R: expected ", NARRATIVE_DIR, " to exist. ",
       "This script must live in <project root>/scripts/.", call. = FALSE)

src_path <- file.path(NARRATIVE_DIR, "sections", paste0(section_id, ".Rmd"))
if (!file.exists(src_path)) {
  avail <- sub("\\.Rmd$", "", sort(list.files(file.path(NARRATIVE_DIR, "sections"),
                                              pattern = "\\.Rmd$")))
  stop("304_preview_section.R: no such section '", section_id, "'. Available: ",
       paste(avail, collapse = ", "), call. = FALSE)
}

for (pkg in c("rmarkdown", "knitr", "jsonlite", "flextable", "officer"))
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("304_preview_section.R needs '", pkg, "'.", call. = FALSE)

local({
  old <- setwd(NARRATIVE_DIR)
  on.exit(setwd(old), add = TRUE)

  if (!rmarkdown::pandoc_available()) {
    hit <- c("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
             "/Applications/RStudio.app/Contents/Resources/app/bin/quarto/bin/tools",
             "/usr/lib/rstudio/bin/quarto/bin/tools")
    hit <- hit[dir.exists(hit)]
    if (length(hit)) Sys.setenv(RSTUDIO_PANDOC = hit[1])
    if (!rmarkdown::pandoc_available())
      stop("304_preview_section.R: pandoc not found.", call. = FALSE)
  }

  # Same object/helper setup the master Rmd performs, so the section's inline
  # `r ...` calls resolve identically to a full build.
  objs <<- if (file.exists("article_objects.json")) jsonlite::fromJSON("article_objects.json") else list()
  source(file.path(SCRIPTS_DIR, "article_helpers.R"), local = FALSE)
  source(file.path(SCRIPTS_DIR, "exhibit_helpers_tables.R"), local = FALSE)

  # Resolve every path pandoc needs BEFORE knitting. knit() moves the working
  # directory (to root.dir while chunks run, and it does not reliably land back
  # here afterwards), so a relative "references.bib" resolved after the knit
  # points into the tempdir instead. Absolute paths captured now are immune.
  csl_rel  <- Sys.getenv("ARTICLE_CSL", unset = "references/csl/ieee.csl")
  csl_abs  <- normalizePath(csl_rel, winslash = "/")
  bib_abs  <- normalizePath(file.path("references", "references.bib"), winslash = "/")
  out_tex  <- paste0("_preview_", section_id, ".tex")
  out_dir  <- file.path(NARRATIVE_DIR, "output")   # generated files live here
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_abs  <- file.path(out_dir, out_tex)

  build_dir <- tempfile("preview_"); dir.create(build_dir)
  on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)

  wrapper <- file.path(build_dir, "wrapper.Rmd")
  writeLines(c(
    "```{r setup, include=FALSE}",
    "knitr::opts_knit$set(rmarkdown.pandoc.to = 'latex')",
    # knit() evaluates chunks with the working directory set to the INPUT file's
    # directory, and this wrapper lives in a tempdir. Without pinning root.dir,
    # the "../output/..." paths inside the sections resolve against the tempdir
    # and fail with "cannot open the connection" -- which reads like a missing
    # file rather than a wrong working directory. Same fix as in 303.
    sprintf("knitr::opts_knit$set(root.dir = '%s')", NARRATIVE_DIR),
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
    "```",
    "",
    # Absolute: knitr resolves a relative child= against the CALLING file's
    # directory, which is the tempdir here, not narrative/.
    sprintf('```{r child = "%s"}', src_path),
    "```"
  ), wrapper)

  message("Knitting ", section_id, " ...")
  md <- file.path(build_dir, "preview.knit.md")
  knitr::knit(wrapper, output = md, envir = globalenv(), quiet = TRUE)
  setwd(NARRATIVE_DIR)   # knit() may leave us in the wrapper's tempdir

  # Append the reference list so citations render against a real bibliography
  # rather than as bare keys; citeproc fills whatever `#refs` div it finds.
  body <- readLines(md, warn = FALSE, encoding = "UTF-8")
  pretty <- gsub("_", " ", sub("^[0-9]+_", "", section_id))
  writeLines(c(
    "---",
    sprintf('title: "Preview: %s"', pretty),
    'date: ""',
    "---",
    "",
    body,
    "", "# References", "", "::: {#refs}", ":::"
  ), md, useBytes = TRUE)

  rmarkdown::pandoc_convert(
    input = normalizePath(md, winslash = "/"), to = "latex", output = out_abs,
    options = c("--standalone", "--variable=geometry:margin=1in",
                paste0("--bibliography=", bib_abs),
                paste0("--csl=", csl_abs),
                "--citeproc"))
  message("304_preview_section.R: wrote narrative/output/", out_tex)

  if (requireNamespace("tinytex", quietly = TRUE)) {
    ok <- tryCatch({
      # Compile from output/, where the .tex now lives.
      .wd <- setwd(out_dir)
      on.exit(setwd(.wd), add = TRUE)
      tinytex::xelatex(out_tex)
      setwd(.wd)
      TRUE
    }, error = function(e) {
      message("304_preview_section.R: xelatex failed (the .tex is still written): ",
              conditionMessage(e)); FALSE })
    if (ok)
      message("304_preview_section.R: compiled narrative/output/_preview_",
              section_id, ".pdf")
  } else {
    message("304_preview_section.R: 'tinytex' not installed; run `xelatex ",
            out_tex, "` from narrative/output/.")
  }
})
