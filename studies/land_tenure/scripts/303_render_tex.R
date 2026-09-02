# 303_render_tex.R
# Export the master narrative Rmd to a standalone LaTeX build:
# narrative/output/main.tex (+ main.pdf), with one
# narrative/output/sections_tex/<id>.tex per narrative/sections/<id>.Rmd.
# A LaTeX analogue of 302_render_article.R.
#
# WHY THIS ISN'T JUST A THIRD rmarkdown::render() OUTPUT FORMAT
# officedown::rdocx_document has no LaTeX equivalent, and the flextable
# exhibits render differently per target: rmarkdown's dependency system
# normally injects the LaTeX packages a flextable needs (booktabs-style
# rules, \Oldarrayrulewidth, etc.) automatically when you render straight to
# pdf_document, but that same mechanism makes a section-by-section split
# awkward -- pandoc's citeproc only fills in a bibliography where a `#refs`
# div lives in *that* document, so knitting each section Rmd in isolation
# leaves 97_references.Rmd empty and duplicates the reference list into every
# citing section instead. This script knits each section once, concatenates
# the results, runs citeproc ONE time across the whole thing, then splits the
# output back into per-section .tex files -- giving both a single
# compilable main.tex and section-by-section deliverables that mirror
# narrative/sections/*.Rmd file-for-file.
#
# "LIVE" IN THE SAME SENSE THE .Rmd IS LIVE
# Nothing here is hand-typed: every number is still `` `r ...` `` in the
# source Rmd and gets resolved fresh from output/estimations/ and
# data/descriptive_exhibits.rds each time this script runs, exactly as
# 302_render_article.R refreshes the .docx. Re-run this after re-running the
# pipeline (or after 302) to refresh the LaTeX export; nothing updates it
# automatically the way a spreadsheet formula would.
#
# USAGE
#   Rscript scripts/303_render_tex.R          # from the land_tenure root, or
#   Rscript studies/land_tenure/scripts/303_render_tex.R   # from an okwaayeli
#                                                             monorepo root
#
# OUTPUT
#   narrative/output/main.tex            entry point, \input{}s the rest
#   narrative/output/sections_tex/*.tex  one per narrative/sections/*.Rmd
#   narrative/output/main.pdf            compiled via tinytex::xelatex, if available
#
# Generated files all land in narrative/output/, so narrative/ itself holds only
# sources. That puts main.tex one level deeper than the sections used to assume,
# which is why the \includegraphics rewrite below adds a "../" -- LaTeX resolves
# graphics paths against the COMPILATION directory, not the file they appear in.
#
# A LIBRARY, NOT A STEP in the numeric sense of 301/302 -- it has a number
# because it sits in the same pipeline position (after OBJECTS, alongside
# RENDER) but is invoked on its own; run_article.R does not call it.

.find_root <- function() {
  for (cand in c(".", "..", "studies/land_tenure")) {
    if (dir.exists(file.path(cand, "narrative")) &&
        dir.exists(file.path(cand, "scripts")))
      # winslash = "/": this path gets embedded in generated chunk-option text
      # (child = "...") below, which knitr parses as an R string literal --
      # Windows' default "\" separator would need escaping there and doesn't
      # get it, so e.g. "...\Documents\..." breaks on the invalid escape "\D".
      return(normalizePath(cand, winslash = "/"))
  }
  stop("303_render_tex.R: could not locate the land_tenure project root ",
       "(looked in '.', '..', 'studies/land_tenure'). Run from the project ",
       "root or the okwaayeli monorepo root.", call. = FALSE)
}
# Deliberately NOT named ROOT/NARRATIVE/SCRIPTS: article_helpers.R (sourced
# below) defines its own globals of exactly those names -- STUDY, DATA,
# OUTPUT, FIGURE, TABLES, NARRATIVE, OBJECTS_JSON -- repo-root-relative and
# for a different purpose. Sourcing it would silently clobber ours.
PROJECT_ROOT  <- .find_root()
NARRATIVE_DIR <- file.path(PROJECT_ROOT, "narrative")
SCRIPTS_DIR   <- file.path(PROJECT_ROOT, "scripts")

for (pkg in c("rmarkdown", "knitr", "jsonlite", "flextable", "officer"))
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("303_render_tex.R needs '", pkg, "': install.packages(\"", pkg, "\")",
         call. = FALSE)

old_wd <- setwd(NARRATIVE_DIR)
on.exit(setwd(old_wd), add = TRUE)

# ---- Pandoc -------------------------------------------------------------------
# rmarkdown finds pandoc automatically inside an RStudio session (it sets
# RSTUDIO_PANDOC); an Rscript invocation outside RStudio often does not. Fall
# back to the copy RStudio itself ships (via its bundled Quarto) if needed.
if (!rmarkdown::pandoc_available()) {
  candidates <- c(
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/Resources/app/bin/quarto/bin/tools",
    "/usr/lib/rstudio/bin/quarto/bin/tools")
  hit <- candidates[dir.exists(candidates)]
  if (length(hit)) Sys.setenv(RSTUDIO_PANDOC = hit[1])
  if (!rmarkdown::pandoc_available())
    stop("303_render_tex.R: pandoc not found. Install it, or open this ",
         "project once in RStudio (which sets RSTUDIO_PANDOC) before running ",
         "this script via Rscript.", call. = FALSE)
}

# ---- Shared objects + helpers --------------------------------------------------
objs <- if (file.exists("article_objects.json")) jsonlite::fromJSON("article_objects.json") else list()
source(file.path(SCRIPTS_DIR, "article_helpers.R"))

# exhibit_helpers_tables.R reassigns `.stars` to okwaayeli::exhibit_stars a few
# lines after defining an identical local `.stars` -- a no-op where okwaayeli
# is installed, but a hard failure (halting every definition after it in the
# file) where it isn't. Strip that one reassignment before sourcing; harmless
# either way since the earlier local definition already does the same thing.
.eh_lines <- readLines(file.path(SCRIPTS_DIR, "exhibit_helpers_tables.R"), warn = FALSE)
.eh_lines <- .eh_lines[!grepl("^\\s*\\.stars\\s*<-\\s*okwaayeli::exhibit_stars", .eh_lines)]
eval(parse(text = .eh_lines), envir = globalenv())

# Large estimation/figure files under output/ can sit on cloud-synced storage
# (Nextcloud, OneDrive, etc.) as on-demand placeholders. R's own connection-
# based readers (readRDS()'s gzfile(), read.csv()'s file()) occasionally race
# the client's on-demand download and fail with a spurious "No such file or
# directory" -- sometimes for many retries in a row -- whereas a plain OS-
# level file.copy() reliably forces full materialization first. So: on a
# failed read, copy the file to a local temp path (once per path, cached) and
# retry from there, instead of retrying the same connection against the
# cloud-backed path. Shadowing readRDS/read.csv here in globalenv() is enough
# to cover every helper below, since they were all sourced into globalenv()
# and so resolve unqualified `readRDS`/`read.csv` calls here first.
# A plain OS-level copy (cmd.exe /c copy on Windows) has proven far more
# reliable against this kind of placeholder than R's own file.copy(), which
# was observed to hang or fail repeatedly against the exact same file that a
# fresh `copy` command reads instantly -- plausibly because file.copy()'s
# underlying Windows API call interacts differently with the sync client's
# filter driver than a shell copy does. Fall back to file.copy() on
# non-Windows, where this class of client is rare.
.os_copy <- function(src, dest) {
  if (.Platform$OS.type == "windows") {
    rc <- suppressWarnings(system2("cmd.exe", c("/c", "copy", "/Y",
      shQuote(normalizePath(src, winslash = "\\", mustWork = FALSE)),
      shQuote(normalizePath(dest, winslash = "\\", mustWork = FALSE))),
      stdout = FALSE, stderr = FALSE))
    isTRUE(rc == 0)
  } else {
    isTRUE(tryCatch(file.copy(src, dest, overwrite = TRUE), error = function(e) FALSE))
  }
}

.local_copy_cache <- new.env(parent = emptyenv())
.robust_read <- function(base_fn) {
  force(base_fn)
  function(path, ...) {
    r <- tryCatch(base_fn(path, ...), error = function(e) e)
    if (!inherits(r, "error")) return(r)
    key <- normalizePath(path, mustWork = FALSE)
    local_path <- .local_copy_cache[[key]]
    if (is.null(local_path) || !file.exists(local_path)) {
      message("  '", path, "' would not open directly (likely a cloud-sync ",
              "placeholder); copying locally and retrying ...")
      local_path <- tempfile(fileext = paste0(".", tools::file_ext(path)))
      # Some cloud-sync clients (older reparse-point implementations, seen
      # with Nextcloud) answer a placeholder's first access with an
      # immediate "not found" while quietly starting the download in the
      # background, rather than blocking the caller until it's ready --
      # so the fix is real wall-clock patience, not more attempts per se.
      # Up to ~5 minutes total for one very large (~50MB) file.
      ok <- FALSE
      for (i in 1:30) {
        ok <- .os_copy(path, local_path) &&
              file.exists(local_path) &&
              identical(file.info(local_path)$size, file.info(path)$size)
        if (ok) break
        Sys.sleep(10)
      }
      if (!ok)
        stop("303_render_tex.R: could not read or locally copy '", path,
             "' after several attempts. If this project's output/ folder is ",
             "cloud-synced, try opening the file once locally (or marking ",
             "the folder 'always keep on this device') and re-run.\n",
             "Original error: ", conditionMessage(r), call. = FALSE)
      .local_copy_cache[[key]] <- local_path
    }
    base_fn(local_path, ...)
  }
}
readRDS <- .robust_read(base::readRDS)

# NOT read.csv: exhibit_helpers_tables.R always calls utils::read.csv() with
# the namespace qualified, which resolves straight into the utils package
# and would never see a same-named shadow sitting in globalenv() here. Mirror
# the (small) directories those calls read from instead, and repoint the
# vars that hold them -- .FIGDAT for fig1_est/fig1_range/trend_gap/
# trend_range, .TBL_DIR for .read_tbl()/Table S0. .EST is left alone (the
# estimation objects run 40-120MB each; mirroring the whole directory would
# be wasteful) since .read_est()'s internal `readRDS()` call is unqualified
# and so is already covered by the shadow above.
.mirror_dir <- function(var_name) {
  if (!exists(var_name, envir = globalenv())) return(invisible(NULL))
  src <- get(var_name, envir = globalenv())
  if (!dir.exists(src)) return(invisible(NULL))
  dest_dir <- tempfile(paste0(gsub("[^A-Za-z0-9]", "", var_name), "_"))
  dir.create(dest_dir)
  for (f in list.files(src, full.names = TRUE)) {
    dest <- file.path(dest_dir, basename(f))
    ok <- FALSE
    for (i in 1:6) {
      ok <- .os_copy(f, dest) &&
            file.exists(dest) && identical(file.info(dest)$size, file.info(f)$size)
      if (ok) break
      Sys.sleep(5)
    }
    if (!ok)
      stop("303_render_tex.R: could not locally mirror '", f, "'. If this ",
           "project's output/ folder is cloud-synced, try opening the file ",
           "once locally (or marking the folder 'always keep on this ",
           "device') and re-run.", call. = FALSE)
  }
  assign(var_name, dest_dir, envir = globalenv())
  invisible(dest_dir)
}
invisible(lapply(c(".FIGDAT", ".TBL_DIR"), .mirror_dir))

# IEEE (numbered) is the target style, and the same default 302, 304 and the
# Rmd's YAML fallback carry. Keep the four in step: they are independent
# defaults, so changing one silently gives the same manuscript two different
# reference formats depending on which route built it.
Sys.setenv(ARTICLE_CSL = Sys.getenv("ARTICLE_CSL", unset = "references/csl/ieee.csl"))
csl_abs <- normalizePath(Sys.getenv("ARTICLE_CSL"))
bib_abs <- normalizePath(file.path("references", "references.bib"))

# Generated files live under narrative/output/, keeping narrative/ to sources.
# main.tex and sections_tex/ sit TOGETHER in there, so main.tex's
# \input{sections_tex/...} stays relative and unchanged.
OUT_DIR <- file.path(NARRATIVE_DIR, "output")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Section list ---------------------------------------------------------
# Derived from narrative/sections/, not hardcoded, so adding/removing a
# section there needs no edit here.
section_files <- sort(list.files("sections", pattern = "\\.Rmd$"))
section_ids   <- sub("\\.Rmd$", "", section_files)
message("303_render_tex.R: ", length(section_ids), " sections found: ",
        paste(section_ids, collapse = ", "))

marker <- function(id, tag)
  sprintf("ZZZSECTIONMARKERZZZ%sZZZ%sZZZ", toupper(gsub("[^A-Za-z0-9]", "", id)), tag)

build_dir <- tempfile("land_tenure_tex_")
dir.create(build_dir)
on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)

# ---- Knit each section once ----------------------------------------------------
pieces <- character(0)
for (id in section_ids) {
  message("Knitting ", id, " ...")
  tmp <- file.path(build_dir, sprintf("_tmp_%s.Rmd", id))
  # Absolute path: the wrapper lives in a tempdir, and knitr resolves a
  # relative child= path against the CALLING file's own directory, not the
  # working directory -- a relative "sections/<id>.Rmd" here would look for
  # a sections/ folder inside the tempdir instead of narrative/sections/.
  child_path <- file.path(NARRATIVE_DIR, "sections", paste0(id, ".Rmd"))
  writeLines(c(
    "```{r setup, include=FALSE}",
    "knitr::opts_knit$set(rmarkdown.pandoc.to = 'latex')",
    # knit() evaluates chunks with the working directory set to the INPUT
    # file's directory -- and this wrapper lives in a tempdir. Without this,
    # every "../output/..." path inside the sections resolves against the
    # tempdir and fails with "cannot open the connection", which reads exactly
    # like a missing or dehydrated file rather than a wrong wd. Pin the chunk
    # working directory to narrative/, where those paths are written to resolve.
    sprintf("knitr::opts_knit$set(root.dir = '%s')", NARRATIVE_DIR),
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
    "```",
    "",
    sprintf('```{r child = "%s"}', child_path),
    "```"
  ), tmp)
  md <- file.path(build_dir, paste0(id, ".knit.md"))
  knitr::knit(tmp, output = md, envir = globalenv(), quiet = TRUE)
  content <- paste(readLines(md, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  pieces <- c(pieces, "", marker(id, "BEGIN"), "", content, "", marker(id, "END"), "")
}

# ---- One citeproc pass across the concatenation --------------------------------
combined_md  <- file.path(build_dir, "combined.knit.md")
writeLines(paste(pieces, collapse = "\n"), combined_md, useBytes = TRUE)
combined_tex <- file.path(build_dir, "combined.tex")
rmarkdown::pandoc_convert(
  input = normalizePath(combined_md), to = "latex", output = combined_tex,
  options = c("--standalone",
              paste0("--bibliography=", bib_abs),
              paste0("--csl=", csl_abs),
              "--citeproc"))

full <- paste(readLines(combined_tex, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

m1 <- regexpr("\\\\begin\\{document\\}", full)
preamble <- trimws(substr(full, 1, m1 - 1), which = "right")
preamble <- sub("\\\\author\\{\\}\\s*\\n?", "", preamble)
preamble <- sub("\\\\date\\{\\}\\s*\\n?", "", preamble)

.sec_tex <- file.path(OUT_DIR, "sections_tex")
dir.create(.sec_tex, showWarnings = FALSE, recursive = TRUE)
for (id in section_ids) {
  b <- marker(id, "BEGIN"); e <- marker(id, "END")
  mb <- regexpr(b, full, fixed = TRUE); me <- regexpr(e, full, fixed = TRUE)
  if (mb < 0 || me < 0) stop("303_render_tex.R: markers not found for ", id, call. = FALSE)
  start <- mb + attr(mb, "match.length")
  body <- trimws(substr(full, start, me - 1), which = "both")
  # Figure paths come out of the knit relative to narrative/ ("../output/
  # figures/x.png" -> the PROJECT's output/figures, not this one). The .tex now
  # compiles from narrative/output/, one level deeper, and LaTeX resolves
  # \includegraphics against the compilation directory rather than the file
  # holding it -- so each needs one more "../". Scoped to \includegraphics so
  # nothing else containing ".." is touched.
  body <- gsub("(\\\\includegraphics(?:\\[[^]]*\\])?\\{)\\.\\./", "\\1../../",
               body, perl = TRUE)
  writeLines(body, file.path(.sec_tex, paste0(id, ".tex")), useBytes = TRUE)
}
message("303_render_tex.R: wrote ", length(section_ids),
        " files to narrative/output/sections_tex/")

# ---- Title / author -----------------------------------------------------------
# Pulled from land-tenure.Rmd rather than duplicated here, so a title change
# there does not silently drift from main.tex.
front <- rmarkdown::yaml_front_matter(file.path(NARRATIVE_DIR, "land-tenure.Rmd"))
title <- if (!is.null(front$title)) front$title else "Untitled"
lt_lines <- readLines(file.path(NARRATIVE_DIR, "land-tenure.Rmd"), warn = FALSE)
author_line <- grep("^[A-Z][a-zA-Z. ]+;.*[A-Z][a-zA-Z. ]+$", lt_lines, value = TRUE)
authors <- if (length(author_line)) trimws(strsplit(author_line[1], ";")[[1]]) else "Author"

# ---- Assemble main.tex ---------------------------------------------------------
# Extra packages: flextable's LaTeX tables need these (see
# flextable:::list_latex_dep()) but rmarkdown's knit_meta dependency system --
# which would normally inject them -- never runs in this two-stage
# knit-then-pandoc-convert path. \Oldarrayrulewidth / \Oldtabcolsep likewise
# come from that same dependency (bundled with `hhline` there, not a package
# of their own). Times New Roman matches the manuscript body font
# (set_flextable_defaults() in exhibit_helpers_tables.R) and happens to also
# cover the handful of literal Unicode Greek/IPA characters in the prose that
# Latin Modern's text font lacks.
main <- c(
  preamble,
  "\\usepackage{array}",
  "\\usepackage{longtable}",
  "\\usepackage{colortbl}",
  "\\usepackage{multirow}",
  "\\usepackage{multicol}",
  "\\usepackage[normalem]{ulem}",
  "\\usepackage{hhline}",
  "\\newlength\\Oldarrayrulewidth",
  "\\newlength\\Oldtabcolsep",
  "\\setmainfont{Times New Roman}[Ligatures=TeX]",
  "",
  sprintf("\\title{%s}", title),
  sprintf("\\author{%s}", paste(authors, collapse = " \\and ")),
  "\\date{}",
  "",
  "\\begin{document}",
  "\\maketitle",
  "",
  sprintf("\\input{sections_tex/%s}", section_ids),
  "",
  "\\end{document}"
)
writeLines(main, file.path(OUT_DIR, "main.tex"), useBytes = TRUE)
message("303_render_tex.R: wrote narrative/output/main.tex")

# ---- Compile --------------------------------------------------------------
# tinytex::xelatex() locates the engine itself, auto-installs any missing
# LaTeX package (as it did for pdflscape/colortbl/wrapfig/ulem the first time
# this export ran), and reruns as many times as longtable/hyperref need to
# settle column widths and cross-references.
if (requireNamespace("tinytex", quietly = TRUE)) {
  ok <- tryCatch({
    # Compile FROM output/: main.tex's \input{sections_tex/...} is relative, and
    # xelatex resolves it against the working directory, not the .tex's folder.
    .wd <- setwd(OUT_DIR)
    on.exit(setwd(.wd), add = TRUE)
    tinytex::xelatex("main.tex")
    setwd(.wd)
    TRUE
  }, error = function(e) {
    message("303_render_tex.R: xelatex compile failed (main.tex is still ",
            "written and reviewable): ", conditionMessage(e))
    FALSE
  })
  if (ok) message("303_render_tex.R: compiled narrative/output/main.pdf")
} else {
  message("303_render_tex.R: package 'tinytex' not installed, skipping PDF ",
          "compile. main.tex and sections_tex/*.tex are still written; run ",
          "`xelatex main.tex` (twice) from narrative/output/ to compile manually.")
}
