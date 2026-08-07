# exhibit_helpers_tables.R
# Exhibit layer: path resolution, memoized readers, cell formatters.
#
# A LIBRARY, NOT A STEP: sourced by narrative/time-poverty.Rmd and by 102, so it
# has no position in a sequence and carries no number. See scripts/README.md.
#
# WHAT IS AND IS NOT HERE. land_tenure's and resource_extraction's copies of this
# file are ~28 KB because they carry a full set of ft_*() flextable builders --
# one per numbered exhibit in their manuscripts. This study HAS NO MANUSCRIPT and
# therefore has no exhibit list, so there is nothing to build builders for.
# Inventing ft_table1() here would be inventing the paper.
#
# What IS here is the plumbing every one of those builders needs and which is
# identical across studies: root resolution, a memo cache, the estimation reader,
# and the two cell formatters. When the exhibit list exists, builders get added
# below this line and nothing above it changes.

# ---- Root resolution ---------------------------------------------------------
# knitr sets the working directory to narrative/ during a render, but 102 and an
# interactive session run from the repo root. Probe rather than assume: this is
# why the Rmd must NOT reach for article_helpers.R's OUTPUT, which is
# repo-root-relative and silently wrong one directory down.
.STUDY_ROOT <- if (dir.exists("output/estimations")) {
  "."
} else if (dir.exists("../output/estimations")) {
  ".."
} else {
  "studies/time_poverty"
}

# ---- Memoization -------------------------------------------------------------
# The estimation objects are large and several builders read the same one. Cache
# per session; the cache is keyed by tag, so a changed file within one session is
# NOT picked up -- restart R after re-running 004.
.CACHE <- new.env(parent = emptyenv())
.memo <- function(key, fn) {
  if (!exists(key, envir = .CACHE, inherits = FALSE))
    assign(key, fn(), envir = .CACHE)
  get(key, envir = .CACHE, inherits = FALSE)
}

.EST <- file.path(.STUDY_ROOT, "output", "estimations")

# Keep only the summary components. The objects also carry ef_samp/ef_dist (one
# row per farmer per draw), which dwarf everything else and which no table needs
# -- fig_distribution() reads those from the *_fullset* file in 101.
.EST_PARTS <- c("ef_mean", "el_mean", "sf_estm", "disagscors")

.read_est <- function(tag)
  .memo(paste0("est:", tag), function() {
    p <- file.path(.EST, paste0(tag, ".rds"))
    if (!file.exists(p))
      stop("exhibit_helpers_tables.R: missing estimation object\n  ", p,
           "\n  Run 004 first: sbatch studies/time_poverty/scripts/job_msf.sbatch",
           call. = FALSE)
    obj <- readRDS(p)
    obj[intersect(names(obj), .EST_PARTS)]
  })

# ---- The optimal matching specification --------------------------------------
# Read from the study environment rather than re-derived, so every exhibit quotes
# the same spec the estimates were produced under.
.se_path <- file.path(.STUDY_ROOT, "data", "time_poverty_study_environment.rds")
.mspecs  <- if (file.exists(.se_path))
  readRDS(.se_path)$match_specification_optimal else NULL
.opt <- if (!is.null(.mspecs))
  ifelse(is.na(.mspecs$link), .mspecs$distance, .mspecs$link) else NA_character_

# ---- Cell formatters ---------------------------------------------------------
# Deliberately sprintf-based, matching fmt_num() in article_helpers.R, so an
# inline number in the prose and the same number in a table cell round
# identically. formatC and sprintf disagree on halfway values.
tbl_num <- function(x, digits = 3) {
  out <- sprintf(paste0("%.", digits, "f"), as.numeric(x))
  out[is.na(x)] <- ""
  out
}
tbl_pct <- function(x, digits = 1) {
  out <- paste0(sprintf(paste0("%.", digits, "f"), 100 * as.numeric(x)), "%")
  out[is.na(x)] <- ""
  out
}

# ---- Significance stars ------------------------------------------------------
tbl_stars <- function(p)
  ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", ""))))

# ============================================================================
# ft_*() BUILDERS GO BELOW THIS LINE
# ============================================================================
# None yet, by design -- see the header. The pattern to copy when the exhibit
# list exists is studies/land_tenure/scripts/exhibit_helpers_tables.R: one
# exported ft_<name>() per exhibit, each returning a flextable, each reading
# through .read_est() rather than readRDS() so the cache is shared.
