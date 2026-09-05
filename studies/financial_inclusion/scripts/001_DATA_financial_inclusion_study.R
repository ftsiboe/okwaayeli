# =============================================================================
#  DATA and SETUP - FINANCIAL INCLUSION STUDY 
# =============================================================================
#  General Description:
#  ---------------------------------------------------------------------------
#  This script prepares analysis-ready data for the financial study within the
#  GHAgricProductivityLab project. It:
#    - Initializes a study-specific environment (folders, paths, metadata),
#    - Loads harmonized farm/household and financial modules,
#    - Merges them at the household-farmer level,
#    - Restricts the sample to relevant GLSS waves,
#    - Saves both the processed study dataset and the study environment object
#      to disk for downstream analysis.
# =============================================================================

# ---- Housekeeping: clear workspace and run garbage collection
rm(list = ls(all = TRUE)); gc()              

# ---- Rebuild package documentation (if this is part of a package) 
# This calls roxygen2 via devtools to regenerate .Rd docs and NAMESPACE.
devtools::document()                         

# id must match this study's case number in studies/run_data_and_match_for_all.sbatch
# (financial_inclusion is task 4). run_only_for() quits when they disagree, so a
# wrong id makes the task exit 0 having done nothing.
if(! as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", NA)) %in% NA){
  run_only_for(id = 4, allowed_jobnames = "run_all")
}

# ---- Define study name and initialize study environment
project_name <- "financial_inclusion"

# study_setup():
#   - creates / verifies the directory tree (delegated to study_dirs()),
#   - defines paths (wd$home, wd$data, wd$output, wd$figures, wd$tables, ...),
#   - and returns the "study_environment" list every later stage reads back
#     from data/<project>_study_environment.rds.
#
# layout = "v2": plots and the data behind them share output/figures/, and table
# data goes to output/tables/. Must match what 000_initialize.R passes -- on
# "legacy" this would create output/figure/ + output/figure_data/ while the
# stages write to output/figures/. See ?study_dirs.
study_environment <- study_setup(project_name = project_name, layout = "v2")

# ---- Load harmonized household / farmer-level data
# Wrapper that downloads (via piggyback) and caches Stata .dta files from
# the GHAgricProductivityLab GitHub repo, then reads them with haven.
farmer_data <- get_household_data("harmonized_crop_farmer_data")

financial_data <- get_household_data("harmonized_financial_inclusion_data")
# financial_data  <- as.data.frame(haven::read_dta("data-raw/releases/harmonized_data/harmonized_financial_inclusion_data.dta"))

# financial_index <- get_household_data("financial_inclusion_index")
financial_index  <- as.data.frame(haven::read_dta("data-raw/releases/harmonized_data/financial_inclusion_index.dta"))

# ---- Merge farmer and land tenure data at the household-member level
# Merge keys:
#   - Surveyx : survey round 
#   - EaId    : enumeration area
#   - HhId    : household ID
#   - Mid     : member ID
# NAME THE COLLISION, do not let dplyr rename it.
#
# Locality is the ONLY non-key column carried by both harmonized_crop_farmer_data
# and harmonized_financial_inclusion_data. Joined as-is, dplyr suffixes the pair
# to Locality.x / Locality.y and BARE `Locality` ceases to exist -- which breaks
# 002 at its complete-case step, because Locality is one of the exact-match
# strata (match_variables_exact). The failure surfaces as the unattributable
# "undefined columns selected", far from this line.
#
# Verified 2026-09-03: the two columns agree on all 60,530 merged rows, so which
# one survives is immaterial; the farmer file's is kept because that is the base
# table and carries the rest of the farm-level covariates. If a future release
# makes them disagree, this is the line that decides it, and the decision should
# be made deliberately rather than by join order.
#
# The Stata index do-file guards the same collision with an explicit keepusing();
# this is the R-side equivalent.
study_data <- dplyr::inner_join(
  farmer_data,
  dplyr::select(financial_data, -dplyr::any_of("Locality")),
  by = c("Surveyx", "EaId", "HhId", "Mid")
)

study_data <- dplyr::inner_join(
  study_data,
  financial_index,
  by = c("Surveyx", "EaId", "HhId", "Mid")
)

# ---- Restrict to relevant survey rounds and drop certain variables
study_data <- study_data[study_data$Surveyx %in% c("GLSS6","GLSS7"),]

# ---- Attach raw data to study environment (potential issue)
study_environment$study_raw_data <- study_data

# ---- Save study environment object
# Save the entire study environment configuration (paths, metadata, etc.)
# for reproducibility and to simplify subsequent scripts.
#
# wd$data, NOT wd$output: the environment is an INPUT to every later stage, and
# data/ holds inputs while output/ holds what the pipeline produces. Matches
# resource_extraction and land_tenure.
saveRDS(
  study_environment,
  file.path(study_environment$wd$data, paste0(project_name,"_study_environment.rds"))
)
