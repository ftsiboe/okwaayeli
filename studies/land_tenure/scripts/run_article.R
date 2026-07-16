# run_article.R
# One entry point for the land_tenure pipeline. Set a stage TRUE to run it.
# Run from the okwaayeli repo root.
#
# Defaults are the cheap path: rebuild the article from the caches already on
# disk. Turn stages on as their inputs change.
#
# See scripts/README.md for the naming convention. In short: a NUMBER means a
# position in a sequence; the two unnumbered *_helpers files are libraries,
# sourced by whatever needs them, and never "run".
rm(list = ls(all = TRUE)); gc() 

devtools::document()
# ============================================================================
# STAGES
# ============================================================================
INITIALIZE  <- TRUE  # 000  study scaffolding                                  fast
DATA        <- FALSE  # 001  harmonized releases -> study_raw_data              fast
MATCHING    <- FALSE  # 002  -> estimation_data, matched samples                EXPENSIVE
TREATMENT   <- FALSE  # 003  -> output/treatment_effects/, te_summary.rds       EXPENSIVE
MSF         <- FALSE  # 004  -> output/estimations/                             HPC, hours
DESCRIPTIVE <- TRUE  # 100  -> data/descriptive_exhibits.rds                   ~5-10 min
FIGURES     <- TRUE  # 101  -> output/figures/ (png + data), output/tables/    moderate
WORKBOOK    <- TRUE  # 102  -> output/tables/land_tenure_tables.xlsx           fast
OBJECTS     <- TRUE  # 301  -> narrative/article_objects.json                  fast
RENDER      <- TRUE  # 302  -> narrative/land-tenure.docx / .html              fast

# ---- Citation style ---------------------------------------------------------
CITATION_STYLE <- "elsevier"   # "elsevier" (Harvard, author-date) or "ieee"

# ============================================================================
# What depends on what
# ============================================================================
# 001 -> 002 -> 003 -> 004 -> 101      estimation, then the figures off it
#                     001 -> 100       descriptives read study_raw_data
#                            100 ─┐
#                            101 ─┴──> the Rmd's tables      -> 301 -> 302
#                                  └──> 102 (same builders -> xlsx)
#
# 102 needs 100 and 101 for the same reason the Rmd does: it calls the same
# ft_*() builders, and those read the descriptive cache and the figure data.
#
# Typical runs:
#   article only .................. OBJECTS + RENDER                (default)
#   descriptives changed .......... DESCRIPTIVE + OBJECTS + RENDER
#   re-estimated (004 on HPC) ..... FIGURES + OBJECTS + RENDER
#   harmonized data changed ....... DATA + MATCHING + TREATMENT + (004 on HPC)
#                                   then DESCRIPTIVE + FIGURES + OBJECTS + RENDER
#   sending tables to a co-author . WORKBOOK (after 100/101 are current)

.SCRIPTS <- "studies/land_tenure/scripts"

# ---- Guards: the failure modes that cost time on 2026-07-15 -----------------

# 001 saves a FRESH study_environment holding study_raw_data but NOT
# estimation_data -- 002 is what attaches that. So running DATA without MATCHING
# silently strips estimation_data from the .rds, and everything downstream (003,
# 004, 100, 301) then fails or quietly reads nothing.
if (DATA && !MATCHING)
  stop("run_article.R: DATA = TRUE requires MATCHING = TRUE.\n",
       "  001 re-saves the study environment WITHOUT estimation_data; only 002 ",
       "attaches it.\n  Running 001 alone leaves the environment unusable ",
       "downstream.", call. = FALSE)

# 004 is a SLURM array (job_msf.sbatch, --array=1-57). Sourcing it here fits
# every specification sequentially on this machine, which is not a thing you
# want to discover by waiting.
if (MSF)
  warning("run_article.R: MSF = TRUE runs 004 sequentially in THIS session.\n",
          "  It is normally a SLURM array (scripts/job_msf.sbatch, --array=1-57).\n",
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
.run(INITIALIZE,  "000_initialize.R",                 "Initialize")
.run(DATA,        "001_DATA_land_tenure_study.R",     "Data")
.run(MATCHING,    "002_MATCHING_land_tenure_study.R", "Matching")
.run(TREATMENT,   "003_TREATMENT_land_tenure_study.R","Treatment effects")
.run(MSF,         "004_MSF_land_tenure_study.R",      "Meta-stochastic frontier")
.run(DESCRIPTIVE, "100_exhibit_descriptive_stats.R",  "Descriptive exhibits")
.run(FIGURES,     "101_exhibit_figures.R",            "Figures")
.run(WORKBOOK,    "102_exhibit_table_workbook.R",     "Table workbook (xlsx)")

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
# narrative/, which is why it resolves its own paths via .STUDY_ROOT rather than
# trusting article_helpers.R's repo-root-relative constants.
message("\nrun_article.R: complete.")
