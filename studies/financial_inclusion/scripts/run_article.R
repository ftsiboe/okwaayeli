# run_article.R
# One entry point for the financial_inclusion pipeline. Set a stage TRUE to run it.
# Run from the okwaayeli repo root.
#
# Defaults are the cheap path: rebuild the article from the caches already on
# disk. Turn stages on as their inputs change.
#
# See scripts/README.md for the naming convention. In short: a NUMBER means a
# position in a sequence; unnumbered *_helpers files are libraries, sourced by
# whatever needs them, and never "run".
rm(list = ls(all = TRUE)); gc()

devtools::document()

unlink(list.files(tools::R_user_dir("okwaayeli", which = "cache"),full.names = TRUE))

# ============================================================================
# STAGES
# ============================================================================
INITIALIZE  <- TRUE # 000  study scaffolding                                  fast
INDEX       <- FALSE # 000  -> data/financial_inclusion_index.rds             ~1 min
DATA        <- FALSE # 001  harmonized releases -> study_raw_data              fast
MATCHING    <- FALSE # 002  -> estimation_data, matched samples                EXPENSIVE
TREATMENT   <- FALSE # 003  -> output/treatment_effects/, te_summary.rds       EXPENSIVE
MSF         <- FALSE # 004  -> output/estimations/                             HPC, hours
DESCRIPTIVE <- FALSE # 100  -> data/descriptive_exhibits.rds                   ~5-10 min
INDEXSUM    <- FALSE # 103  -> data/financial_inclusion_index_summary.rds  seconds
FIGURES     <- FALSE # 101  -> output/figures/ (png + data)                    moderate
WORKBOOK    <- FALSE # 102  -> output/tables/financial_inclusion_tables.xlsx  minutes
OBJECTS     <- TRUE # 301  -> narrative/article_objects.json                  fast
RENDER      <- TRUE # 302  -> narrative/financial-inclusion.docx / .html      fast

# ---- Citation style ---------------------------------------------------------
CITATION_STYLE <- "ieee"       # "ieee" (numbered) or "elsevier" (Harvard, author-date)
# This is the switch that matters -- it sets ARTICLE_CSL below, which both 301
# and the Rmd read. The unset-fallbacks in 302_render_article.R and the Rmd's
# YAML are kept in agreement with it so that knitting outside this pipeline
# (RStudio's Knit button, or sourcing 302 directly) produces the same style.

# ============================================================================
# What depends on what
# ============================================================================
# 000_INDEX -> 001                     the index is a 001 INPUT, not a stage
#                                      output; see the guard below
# 001 -> 002 -> 003 -> 004 -> 101      estimation, then the figures off it
#                     001 -> 100       descriptives read study_raw_data
#                            100 -|
#                            101 -+--> the Rmd's tables      -> 301 -> 302
#                                 +--> 102 (same builders -> xlsx)
#
# Typical runs:
#   article only .................. OBJECTS + RENDER                (default)
#   descriptives changed .......... DESCRIPTIVE + OBJECTS + RENDER
#   re-estimated (004 on HPC) ..... FIGURES + WORKBOOK + OBJECTS + RENDER
#   exhibit builders edited ....... FIGURES + WORKBOOK + OBJECTS + RENDER
#   prose only .................... OBJECTS + RENDER
#   index definition changed ...... INDEX + DATA + MATCHING + TREATMENT + (004)
#   harmonized data changed ....... INDEX + DATA + MATCHING + TREATMENT + (004 on HPC)
#                                   then DESCRIPTIVE + FIGURES + OBJECTS + RENDER
#
# The financial-inclusion index USED to be upstream of all this and not a stage:
# a Stata do-file with a hardcoded Windows path, run by hand, writing into
# data-raw/. That made it the one study input every machine had to receive by
# hand -- and on 2026-09-03 a stale copy on the cluster silently dropped 3,214
# farm operators, because 001 inner-joins on it and a stale index is
# indistinguishable from a fresh one until the sample size is inspected.
#
# It is now INDEX above: an R port (000_INDEX_financial_inclusion_study.R,
# validated against the Stata build to 1e-13 on the loadings and exactly on the
# released values) writing data/financial_inclusion_index.rds. Any machine with
# R can rebuild it, so nothing is hand-carried. See
# narrative/diagnostics/financial_inclusion_index_documentation.md.

.SCRIPTS <- "studies/financial_inclusion/scripts"

# ---- Guards: the couplings that are not obvious ------------------------------

# 001 saves a FRESH study_environment holding study_raw_data but NOT
# estimation_data -- 002 is what attaches that. So running DATA without MATCHING
# silently strips estimation_data from the .rds, and everything downstream (003,
# 004, 100, 301) then fails or quietly reads nothing.
if (DATA && !MATCHING)
  stop("run_article.R: DATA = TRUE requires MATCHING = TRUE.\n",
       "  001 re-saves the study environment WITHOUT estimation_data; only 002 ",
       "attaches it.\n  Running 001 alone leaves the environment unusable ",
       "downstream.", call. = FALSE)

# The index is DERIVED from harmonized_crop_farmer_data and
# harmonized_financial_inclusion_data, and 001 inner-joins on it. So an index
# older than either release drops every member whose keys the two no longer
# share -- silently, because an inner join has no way to complain. Warn when
# 001 is about to run against an index that predates its own inputs.
if (DATA && !INDEX) {
  .ix <- file.path("studies", "financial_inclusion", "data",
                   "financial_inclusion_index.rds")
  if (!file.exists(.ix)) {
    stop("run_article.R: DATA = TRUE but the financial-inclusion index is ",
         "missing:\n  ", .ix, "\n  Set INDEX = TRUE; 001 inner-joins on it and ",
         "cannot run without it.", call. = FALSE)
  }
  .cache <- tools::R_user_dir("okwaayeli", which = "cache")
  .src <- file.path(.cache, c("harmonized_crop_farmer_data.dta",
                              "harmonized_financial_inclusion_data.dta"))
  .src <- .src[file.exists(.src)]
  if (length(.src) && any(file.mtime(.src) > file.mtime(.ix))) {
    warning("run_article.R: the financial-inclusion index is OLDER than a ",
            "release it is built from.\n  index : ", .ix, "\n  Set ",
            "INDEX = TRUE, or 001 will join on a stale index and drop members ",
            "without reporting it.", call. = FALSE, immediate. = TRUE)
  }
  rm(.ix, .cache, .src)
}

# 004 is a SLURM array (job_msf.sbatch, --array=1-60). Sourcing it here fits
# every specification sequentially on this machine, which is not a thing you
# want to discover by waiting.
if (MSF)
  warning("run_article.R: MSF = TRUE runs 004 sequentially in THIS session.\n",
          "  It is normally a SLURM array (scripts/job_msf.sbatch, --array=1-60).\n",
          "  If you edited technology_variables, update the array size to match ",
          "nrow(model_specifications) or the new specs never run.",
          call. = FALSE, immediate. = TRUE)

# Each stage runs in its OWN environment.
#
# Several stage scripts open with `rm(list = ls(all = TRUE)); gc()` -- a pattern
# that assumes the script is the only thing in the session. Sourced into the
# global environment they delete the runner itself, and the next stage fails with
# "could not find function '.run'". Anything stashed to survive it (Keep.List and
# friends) is deleted too: ls(all = TRUE) means all.
#
# local = new.env(parent = parent.frame()) contains that. Each script's rm() now
# clears only its own environment, while lexical scoping still lets it see
# whatever the runner has loaded (e.g. NARRATIVE from article_helpers.R). Stages
# communicate through disk -- 001 writes the study environment, 002 reads it --
# so nothing needs to survive in memory anyway.
.run <- function(flag, file, what) {
  if (!isTRUE(flag)) return(invisible(FALSE))
  p <- file.path(.SCRIPTS, file)
  if (!file.exists(p)) stop("run_article.R: missing ", p, call. = FALSE)
  message("\n=== ", what, "  [", file, "] ===")
  t0 <- Sys.time()
  source(p, local = new.env(parent = parent.frame()))
  message("=== done in ", format(round(difftime(Sys.time(), t0), 1)), " ===")
  invisible(TRUE)
}

Keep.List <- c("Keep.List", ls())

# ============================================================================
# Pipeline
# ============================================================================
.run(INITIALIZE,  "000_initialize.R",                        "Initialize")
.run(INDEX,       "000_INDEX_financial_inclusion_study.R",   "Financial-inclusion index (PCA)")
.run(DATA,        "001_DATA_financial_inclusion_study.R",     "Data")
.run(MATCHING,    "002_MATCHING_financial_inclusion_study.R", "Matching")
.run(TREATMENT,   "003_TREATMENT_financial_inclusion_study.R","Treatment effects")
.run(MSF,         "004_MSF_financial_inclusion_study.R",      "Meta-stochastic frontier")
.run(DESCRIPTIVE, "100_exhibit_descriptive_stats.R",          "Descriptive exhibits")
.run(INDEXSUM,    "103_exhibit_index_summary.R",            "Index summary (Table S7)")
.run(FIGURES,     "101_exhibit_figures.R",                    "Figures")
.run(WORKBOOK,    "102_exhibit_table_workbook.R",             "Table workbook (xlsx)")

if (OBJECTS || RENDER) {
  Sys.setenv(ARTICLE_CSL = if (identical(CITATION_STYLE, "ieee"))
    "csl/ieee.csl" else "csl/elsevier-harvard.csl")
  # A library, not a stage: 301 and 302 both need it, and so does the Rmd.
  source(file.path(.SCRIPTS, "article_helpers.R"))
}

.run(OBJECTS, "301_article_objects.R",  "Article objects")
.run(RENDER,  "302_render_article.R",   "Render")

# exhibit_helpers_tables.R is NOT sourced here. It is a library, and the Rmd
# sources it during the render -- at which point knitr's working directory is
# narrative/, which is why it must resolve its own paths via .STUDY_ROOT rather
# than trusting article_helpers.R's repo-root-relative constants.
#
# 102 sources it directly, because it runs at the repo root and needs the same
# builders the Rmd calls -- same functions, same numbers, one definition.
# See scripts/README.md.
message("\nrun_article.R: complete.")
