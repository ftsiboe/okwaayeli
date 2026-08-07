# run_article.R
# One entry point for the time_poverty pipeline. Set a stage TRUE to run it.
# Run from the okwaayeli repo root.
#
# READ THIS FIRST -- this study is EARLY, and the environment on disk is
# INCOMPLETE. Verified 2026-08-07 by reading data/time_poverty_study_environment.rds:
# it holds ONLY wd, myseed and study_raw_data (24,035 x 83).
#
#   estimation_data              ABSENT
#   match_specifications         ABSENT
#   sample_draw_list             ABSENT
#   match_specification_optimal  ABSENT
#   match_specification_ranking  ABSENT
#   balance_table                ABSENT
#
# 002 clearly ran at some point -- output/matching/ holds 808 matched samples --
# but the environment it wrote did not survive. Either 001 was re-run afterwards
# (which re-saves the environment WITHOUT the matching objects; see the DATA
# guard below) or that run never saved. Either way 003, 004, 100 and 301 have no
# match_specifications to read, and the 808 files on disk are orphaned from the
# specification table that describes them.
#
# So, stage by stage:
#
#   000            works
#   001            works, but the guard below requires MATCHING with it
#   002            works TODAY off study_raw_data -- and is what repopulates the
#                  environment. EXPENSIVE. This is the unblocking step.
#   003 004        BLOCKED on 002: they read match_specifications
#   100 102        stubs (100's bootstrap runs; the exhibit logic is unwritten)
#   101            repaired but UNVERIFIED, and blocked on 004
#   301            partial, and blocked on 004
#   302            works, and renders a narrative that is all section stubs
#
# ALSO VERIFIED: the frozen wd in that .rds points at
#   replications/time_poverty/output/...
# a directory root this repo has not used in a long time -- not merely the
# retired "legacy" layout. Every stage that readRDS()s the environment therefore
# calls study_dirs() on it before touching a path. Without that, 002 writes its
# 808 samples into a tree that does not exist.
#
# The defaults are therefore all FALSE. There is no "cheap path" to rebuild the
# article from caches, because there are no caches. Turn a stage on only when
# you have just supplied its input.
#
# See scripts/README.md for the naming convention. In short: a NUMBER means a
# position in a sequence; the two unnumbered *_helpers files are libraries,
# sourced by whatever needs them, and never "run".
rm(list = ls(all = TRUE)); gc()

devtools::document()
# ============================================================================
# STAGES
# ============================================================================
INITIALIZE  <- FALSE  # 000  study scaffolding                                  fast
DATA        <- FALSE  # 001  harmonized releases -> study_raw_data              fast
MATCHING    <- FALSE  # 002  -> estimation_data, matched samples                EXPENSIVE
TREATMENT   <- FALSE  # 003  -> output/treatment_effects/, te_summary.rds       EXPENSIVE
MSF         <- FALSE  # 004  -> output/estimations/                             HPC, hours
DESCRIPTIVE <- FALSE  # 100  -> data/descriptive_exhibits.rds                   STUB
FIGURES     <- FALSE  # 101  -> output/figures/ (png + data), output/tables/    needs 004
WORKBOOK    <- FALSE  # 102  -> output/tables/time_poverty_tables.xlsx          STUB
OBJECTS     <- FALSE  # 301  -> narrative/article_objects.json                  STUB
RENDER      <- FALSE  # 302  -> narrative/time-poverty.docx / .html             stubs only

# ---- Citation style ---------------------------------------------------------
CITATION_STYLE <- "elsevier"   # "elsevier" (Harvard, author-date) or "ieee"

# ============================================================================
# What depends on what
# ============================================================================
# 001 -> 002 -> 003 -> 004 -> 101      estimation, then the figures off it
#                     001 -> 100       descriptives read study_raw_data
#                            100 -+
#                            101 -+--> the Rmd's tables      -> 301 -> 302
#                                 +--> 102 (same builders -> xlsx)
#
# 102 needs 100 and 101 for the same reason the Rmd does: it calls the same
# ft_*() builders, and those read the descriptive cache and the figure data.
#
# The unblocking order for THIS study, given where it stands:
#   1. MATCHING                               repopulates the environment.
#                                             EXPENSIVE, and the prerequisite for
#                                             everything below. The seed is fixed
#                                             (myseed = 1980632), so the redrawn
#                                             samples should reproduce the 808
#                                             already in output/matching/.
#   2. sbatch scripts/job_msf.sbatch          fills output/estimations/
#   3. TREATMENT                              fills output/treatment_effects/
#   4. FIGURES                                first real test of 101
#   5. write 100 and 102 against what 101 produced
#   6. write the manuscript, then 301 + RENDER

.SCRIPTS <- "studies/time_poverty/scripts"

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

# 004 is a SLURM array (job_msf.sbatch, --array=1-18). Sourcing it here fits
# every specification sequentially on this machine, which is not a thing you
# want to discover by waiting.
if (MSF)
  warning("run_article.R: MSF = TRUE runs 004 sequentially in THIS session.\n",
          "  It is normally a SLURM array (scripts/job_msf.sbatch, --array=1-18).\n",
          "  VERIFY that 18 still equals nrow(model_specifications): 004 filters",
          " the grid to level == 'Pooled' over two technology_variables, and a",
          " short array silently skips the tail.",
          call. = FALSE, immediate. = TRUE)

# 2026-08-07 MIGRATION NOTE -- layout and the frozen environment.
# 001 now calls study_setup(project_name, layout = "v2") and the study
# environment moved from output/ to data/. The .rds currently on disk predates
# both and resolves to the "legacy" layout (figure/ + figure_data/), a folder
# that no longer exists. Until DATA + MATCHING have been re-run once, every
# stage that readRDS()s the environment calls study_dirs() on it to recompute
# paths -- wd is a frozen snapshot, not a live view. See ?study_dirs.

# 003, 004, 100 and 301 all read objects that 002 attaches. Checked here, once,
# rather than four scripts deep where the symptom is a NULL subscript.
if (TREATMENT || MSF) {
  .se_path <- "studies/time_poverty/data/time_poverty_study_environment.rds"
  if (!file.exists(.se_path))
    stop("run_article.R: no study environment at ", .se_path,
         "\n  Run DATA + MATCHING first.", call. = FALSE)
  .se <- readRDS(.se_path)
  .need <- c("estimation_data", "match_specifications", "sample_draw_list")
  .miss <- .need[!.need %in% names(.se)]
  if (length(.miss))
    stop("run_article.R: the study environment is missing ",
         paste(.miss, collapse = ", "), ".\n",
         "  Only 002 attaches these. Set MATCHING = TRUE and re-run.\n",
         "  (output/matching/ may already hold matched samples -- they are ",
         "orphaned without\n   the specification table that indexes them.)",
         call. = FALSE)
  rm(.se, .se_path, .need, .miss)
}

# 101 cannot run before 004. Said here rather than discovered as an empty
# figure: 101's own .est() guard stops with the sbatch command, but the runner
# should not get that far.
if (FIGURES && !length(list.files(file.path("studies/time_poverty/output/estimations"),
                                  pattern = "\\.rds$")))
  stop("run_article.R: FIGURES = TRUE but output/estimations/ is empty.\n",
       "  Run 004 first: sbatch studies/time_poverty/scripts/job_msf.sbatch",
       call. = FALSE)

# ---- Stata ------------------------------------------------------------------
# 100_exhibits.do is NOT driven from here: R cannot run it. It is also still
# UNCONVERTED disability code and carries a hard stop at the top -- see its
# header and scripts/README.md before touching it.

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
.run(INITIALIZE,  "000_initialize.R",                  "Initialize")
.run(DATA,        "001_DATA_time_poverty_study.R",     "Data")
.run(MATCHING,    "002_MATCHING_time_poverty_study.R", "Matching")
.run(TREATMENT,   "003_TREATMENT_time_poverty_study.R","Treatment effects")
.run(MSF,         "004_MSF_time_poverty_study.R",      "Meta-stochastic frontier")
.run(DESCRIPTIVE, "100_exhibit_descriptive_stats.R",   "Descriptive exhibits")
.run(FIGURES,     "101_exhibit_figures.R",             "Figures")
.run(WORKBOOK,    "102_exhibit_table_workbook.R",      "Table workbook (xlsx)")

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
