# =============================================================================
#  DATA and SETUP - TIME POVERTY STUDY 
# =============================================================================
#  General Description:
#  ---------------------------------------------------------------------------
#  This script prepares analysis-ready data for the time poverty study within the
#  GHAgricProductivityLab project. It:
#    - Initializes a study-specific environment (folders, paths, metadata),
#    - Loads harmonized farm/household and time poverty modules,
#    - Merges them at the household-member level,
#    - Restricts the sample to recent GLSS waves (GLSS6 and GLSS7),
#    - Constructs a composite "disabled" indicator (any time poverty in household),
#    - Cleans sub-indicator variables to avoid false zeros when overall
#      time_poverty = 1, and
#    - Saves both the processed study dataset and the study environment object
#      to disk for downstream analysis (e.g., regression, matching, etc.).
# =============================================================================

# ---- Housekeeping: clear workspace and run garbage collection ----------------
rm(list = ls(all = TRUE)); gc()              

# ---- Rebuild package documentation (if this is part of a package) -----------
# This calls roxygen2 via devtools to regenerate .Rd docs and NAMESPACE.
devtools::document()                         

run_only_for(id = 7, allowed_jobnames = "run_all")

# ---- Define study name and initialize study environment ---------------------
project_name <- "time_poverty"

# study_setup() is assumed to:
#   - create / verify directories,
#   - define paths (e.g., wd$home, wd$data, wd$output),
#   - and return a list-like "study_environment" object
#     containing configuration for this specific project.
study_environment <- study_setup(project_name = project_name)

# ---- Load harmonized household / farmer-level data --------------------------
# Wrapper that downloads (via piggyback) and caches Stata .dta files from
# the GHAgricProductivityLab GitHub repo, then reads them with haven.
farmer_data     <- get_household_data("harmonized_crop_farmer_data", force = TRUE)
time_poverty_data <- get_household_data("harmonized_time_poverty_data", force = TRUE)

# ---- Merge farmer and time poverty data at the household-member level ---------
# Merge keys:
#   - Surveyx : survey round (e.g., GLSS6, GLSS7)
#   - EaId    : enumeration area
#   - HhId    : household ID
#   - Mid     : member ID
study_data <- dplyr::inner_join(
  farmer_data,
  time_poverty_data,
  by = c("Surveyx", "EaId", "HhId", "Mid")
)

# ---- Restrict to GLSS6 and GLSS7 and drop certain variables -----------------
# Keep only GLSS6 and GLSS7, i.e., more recent rounds with richer time poverty data.
# Then drop any variables whose names match the patterns "LndFrgMid", "EduWhyNo", or "RentHa".
study_data <- study_data[
  study_data$Surveyx %in% c("GLSS7"),
  names(study_data)[!grepl("LndFrgMid|EduWhyNo|RentHa", names(study_data))]
]

# ---- Attach raw data to study environment (potential issue) ------------------
study_environment$study_raw_data <- study_data

# ---- Save study environment object ------------------------------------------
# Save the entire study environment configuration (paths, metadata, etc.)
# for reproducibility and to simplify subsequent scripts.
saveRDS(
  study_environment,
  file.path(study_environment$wd$output, paste0(project_name,"_study_environment.rds"))
)
