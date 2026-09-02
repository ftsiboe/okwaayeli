# _paths.R
# Where this study lives, and every path derived from it.
#
# A LIBRARY, NOT A STEP: sourced by the numbered scripts and by
# article_helpers.R, so it has no position in a sequence and carries no number.
# See scripts/README.md.
#
# WHY THIS EXISTS
# The pipeline was written inside the okwaayeli monorepo, where this study sits
# at <repo>/studies/land_tenure/ and every path is expressed relative to the
# repo root. Two things bake that assumption in:
#
#   1. okwaayeli's own study_dirs() builds the tree as
#      file.path("studies", project_name), relative to the working directory.
#   2. data/land_tenure_study_environment.rds carries a SNAPSHOT of those
#      paths in $wd -- "studies/land_tenure/output/estimations" and so on.
#
# In a standalone checkout, where narrative/, scripts/, data/ and output/ sit
# directly under one root, both resolve to a "studies/land_tenure" that is not
# there. Rather than fork the package or rewrite the .rds (which the cluster and
# co-authors share), resolve the real root here and rebase onto it at load time.
# Both layouts then work from the same sources.
#
# USAGE
#   source("scripts/_paths.R")
#
#   PROJECT_ROOT  absolute path to the study root
#   STUDY DATA OUTPUT FIGURE TABLES NARRATIVE SCRIPTS OBJECTS_JSON SE_RDS
#   study_env()      load the study environment with $wd rebased onto this root
#   rebase_wd(se)    rebase an environment you loaded or recomputed yourself
#   okwaayeli_load() attach the package, however it is available here
#
# Paths are ABSOLUTE, so they hold whatever the working directory is -- knitr
# changes it to the document's directory mid-render, which is exactly where
# relative paths used to break.
#
# Override the search with the LAND_TENURE_ROOT environment variable.

# ---- Locating this file ------------------------------------------------------
# Rscript exposes --file=; source() exposes ofile on a frame. Either gives the
# script's own location, which beats searching from the working directory: it is
# right even when sourced from a console sitting somewhere else entirely.
.lt_this_file <- function() {
  cl <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(cl))
    return(normalizePath(sub("^--file=", "", cl[1]), winslash = "/", mustWork = FALSE))
  # INNERMOST frame first. sys.frame(1) is the OUTERMOST call, so a forward loop
  # returns whichever file began the chain -- narrative/sections/_setup.R when a
  # section is knitted on its own -- and this file's location would be read off
  # that instead, putting the root two levels above sections/. Walking back from
  # the innermost frame returns _paths.R's own path, whatever sourced it.
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(normalizePath(of, winslash = "/", mustWork = FALSE))
  }
  NULL
}

# A study root is the folder holding both narrative/ and scripts/. Testing for
# two directories rather than one avoids matching a stray "scripts" folder.
.lt_is_root <- function(p)
  is.character(p) && length(p) == 1L && nzchar(p) &&
  dir.exists(file.path(p, "narrative")) && dir.exists(file.path(p, "scripts"))

PROJECT_ROOT <- local({
  env <- Sys.getenv("LAND_TENURE_ROOT", "")
  if (nzchar(env)) {
    if (!.lt_is_root(env))
      stop("_paths.R: LAND_TENURE_ROOT is set to '", env, "', which does not ",
           "hold narrative/ and scripts/.", call. = FALSE)
    return(normalizePath(env, winslash = "/"))
  }
  me <- .lt_this_file()                      # <root>/scripts/_paths.R
  if (!is.null(me) && .lt_is_root(dirname(dirname(me))))
    return(normalizePath(dirname(dirname(me)), winslash = "/"))
  # Fallback for eval(parse(...)) callers, which expose no file path: search
  # from the working directory. ".." covers a narrative/ cwd and "../.." a
  # narrative/sections/ one, which is where knitr puts a section knitted alone.
  for (p in c(".", "..", "../..", "studies/land_tenure", "../studies/land_tenure"))
    if (.lt_is_root(p)) return(normalizePath(p, winslash = "/"))
  stop("_paths.R: could not locate the land_tenure study root. Set ",
       "LAND_TENURE_ROOT, or run from the folder holding narrative/ and ",
       "scripts/.", call. = FALSE)
})

# ---- Derived paths -----------------------------------------------------------
# Names match what article_helpers.R defined before, so callers are unchanged.
STUDY        <- PROJECT_ROOT
DATA         <- file.path(PROJECT_ROOT, "data")
OUTPUT       <- file.path(PROJECT_ROOT, "output")
FIGURE       <- file.path(OUTPUT, "figures")     # v2 layout; see ?study_dirs
TABLES       <- file.path(OUTPUT, "tables")
NARRATIVE    <- file.path(PROJECT_ROOT, "narrative")
SCRIPTS      <- file.path(PROJECT_ROOT, "scripts")
OBJECTS_JSON <- file.path(NARRATIVE, "article_objects.json")
SE_RDS       <- file.path(DATA, "land_tenure_study_environment.rds")

# ---- Rebasing the stored / recomputed $wd ------------------------------------
# study_dirs() returns paths under file.path("studies", project_name) whatever
# the layout on disk, and the saved .rds carries the same. Move anything under
# that home onto PROJECT_ROOT and leave everything else untouched, so a study
# that has added its own absolute entries keeps them.
rebase_wd <- function(se) {
  if (is.null(se) || is.null(se$wd)) return(se)
  home <- if (!is.null(se$wd$home)) se$wd$home else "studies/land_tenure"
  se$wd <- lapply(se$wd, function(p) {
    if (is.null(p) || !length(p) || !nzchar(p)) return(p)
    p <- gsub("\\\\", "/", p)
    if (identical(p, home)) return(PROJECT_ROOT)
    if (startsWith(p, paste0(home, "/")))
      return(file.path(PROJECT_ROOT, substring(p, nchar(home) + 2L)))
    p
  })
  se
}

# Load the study environment with $wd already pointing at this checkout. Every
# stage that readRDS()es it should come through here instead, or it will write
# its output into a "studies/land_tenure" tree beside the working directory.
study_env <- function(path = SE_RDS) {
  if (!file.exists(path))
    stop("study_env(): no study environment at\n  ", path,
         "\nRun scripts/001_DATA_land_tenure_study.R first.", call. = FALSE)
  rebase_wd(readRDS(path))
}

# ---- The package -------------------------------------------------------------
# Inside the monorepo the package sources ARE the working tree, and load_all()
# picks up edits without reinstalling -- which is why the scripts called
# devtools::document() at the top. Standalone there is no package here to
# document, and that call fails before anything else runs, so fall back to the
# installed copy.
okwaayeli_load <- function(quiet = TRUE) {
  if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = quiet)
  } else if (requireNamespace("okwaayeli", quietly = TRUE)) {
    suppressPackageStartupMessages(library(okwaayeli))
  } else {
    stop("okwaayeli is not available, and this is not the package's own repo.\n",
         "Install it with:\n",
         "  install.packages(\"remotes\")\n",
         "  remotes::install_github(\"ftsiboe/okwaayeli\")", call. = FALSE)
  }
  invisible(TRUE)
}

invisible(TRUE)
