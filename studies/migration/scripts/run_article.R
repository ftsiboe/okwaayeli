# run_article.R
# One entry point for the migration pipeline. Set a stage TRUE to run it.
# Only the analysis stages exist so far (000-004); exhibit (1xx) and manuscript
# (3xx) stages will follow the land_tenure pattern.
#
# Run from the okwaayeli repo root or the study root; scripts/_paths.R resolves
# either. Set MIGRATION_ROOT to override.
rm(list = ls(all = TRUE)); gc()

if (!exists("PROJECT_ROOT")) {
  .p <- c("scripts/_paths.R", "../scripts/_paths.R",
          "studies/migration/scripts/_paths.R")
  .p <- .p[file.exists(.p)]
  if (!length(.p))
    stop("run_article.R: cannot find scripts/_paths.R -- run from the study root.",
         call. = FALSE)
  source(.p[1])
}
okwaayeli_load()

# NB the 001 flag is DATA_PREP, not DATA: _paths.R defines DATA as a path and a
# flag of that name would overwrite it. Same for OUTPUT, FIGURE, TABLES,
# NARRATIVE, SCRIPTS and STUDY.
INITIALIZE <- FALSE  # 000  study scaffolding                          fast
DATA_PREP  <- FALSE  # 001  releases -> study_raw_data                 fast
MATCHING   <- FALSE  # 002  -> estimation_data, matched samples        EXPENSIVE
TREATMENT  <- FALSE  # 003  -> output/treatment_effects/, te_summary   EXPENSIVE
MSF        <- FALSE  # 004  -> output/estimations/                     HPC, hours

.SCRIPTS <- SCRIPTS

if (DATA_PREP && !MATCHING)
  stop("run_article.R: DATA_PREP = TRUE requires MATCHING = TRUE.\n",
       "  001 re-saves the study environment WITHOUT estimation_data; only 002 ",
       "attaches it.", call. = FALSE)

if (MSF)
  warning("run_article.R: MSF = TRUE runs 004 sequentially in THIS session.\n",
          "  It is normally a SLURM array (scripts/job_msf.sbatch).",
          call. = FALSE, immediate. = TRUE)

# Each stage in its own environment, so a script's rm(list = ls()) cannot
# delete the runner.
.run <- function(flag, file, what) {
  if (!isTRUE(flag)) return(invisible(FALSE))
  p <- file.path(.SCRIPTS, file)
  if (!file.exists(p)) stop("run_article.R: missing ", p, call. = FALSE)
  message("\n=== ", what, "  [", file, "] ===")
  t0 <- Sys.time()
  source(p, local = new.env(parent = parent.frame()))
  message("    done in ", format(round(difftime(Sys.time(), t0), 1)))
  invisible(TRUE)
}

.run(INITIALIZE, "000_initialize.R",                  "000 initialize")
.run(DATA_PREP,  "001_DATA_migration_study.R",        "001 data")
.run(MATCHING,   "002_MATCHING_migration_study.R",    "002 matching")
.run(TREATMENT,  "003_TREATMENT_migration_study.R",   "003 treatment effects")
.run(MSF,        "004_MSF_migration_study.R",         "004 frontier")
