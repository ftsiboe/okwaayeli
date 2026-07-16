# =============================================================================
#  DATA and SETUP - LAND TENURE STUDY 
# =============================================================================
#  General Description:
#  ---------------------------------------------------------------------------
#  This script prepares analysis-ready data for the land tenure study within the
#  GHAgricProductivityLab project. It:
#    - Initializes a study-specific environment (folders, paths, metadata),
#    - Loads harmonized farm/household and land tenure modules,
#    - Merges them at the household-farmer level,
#    - Restricts the sample to relevant GLSS waves,
#    - Saves both the processed study dataset and the study environment object
#      to disk for downstream analysis.
# =============================================================================

# ---- Housekeeping: clear workspace and run garbage collection
tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})         

# ---- Rebuild package documentation (if this is part of a package) 
# This calls roxygen2 via devtools to regenerate .Rd docs and NAMESPACE.
devtools::document()                         

run_only_for(id = 5, allowed_jobnames = "run_all")

# ---- Define study name and initialize study environment
project_name <- "land_tenure"

# study_setup() creates the directory tree and returns the study_environment
# (paths in $wd, seed, layout).
#
# layout = "v2": plots and their data share output/figures/, table data goes to
# output/tables/. Siblings are on "legacy" (figure/ + figure_data/), hence a
# parameter rather than a rename. Downstream, reach for study_dir_figures() /
# study_dir_tables(), never a directory literal next to wd$output.
#
# NB wd is written into the .rds below and is a SNAPSHOT -- stages that readRDS()
# it should call study_dirs() to recompute paths. See ?study_dirs.
study_environment <- study_setup(project_name = project_name, layout = "v2")

# ---- Load harmonized household / farmer-level data
# Wrapper that downloads (via piggyback) and caches Stata .dta files from
# the GHAgricProductivityLab GitHub repo, then reads them with haven.
farmer_data <- get_household_data("harmonized_crop_farmer_data")
farmer_data <- farmer_data[names(farmer_data)[!grepl("OwnLnd", names(farmer_data))]]

# land_tenure_data <- get_household_data("harmonized_land_tenure_data")
land_tenure_data  <- as.data.frame(haven::read_dta("data-raw/releases/harmonized_data/harmonized_land_tenure_data.dta"))

# ---- Merge farmer and land tenure data at the household-member level
# Merge keys:
#   - Surveyx : survey round 
#   - EaId    : enumeration area
#   - HhId    : household ID
#   - Mid     : member ID
study_data <- dplyr::inner_join(
  farmer_data,
  land_tenure_data,
  by = c("Surveyx", "EaId", "HhId", "Mid")
)

# ---- Restrict to relevant survey rounds and drop certain variables
study_data <- study_data[
  study_data$Surveyx %in% c("GLSS3","GLSS4","GLSS5","GLSS6","GLSS7"),
  names(study_data)[!grepl("EduWhyNo", names(study_data))]
]

# ---- Census sampling frame -------------------------------------------------
# GSS changed the sampling frame between GLSS4 and GLSS5. This is documented by
# GSS itself, not inferred:
#
#   GLSS4 report, "Sample Frame":
#     "the list of the 1984 population census Enumeration Areas (EAs) ... was
#      used as the sampling frame. This frame, though quite old, was considered
#      inadequate, it being the best available at the time. Indeed, this frame
#      was used for the earlier rounds of the GLSS."
#
#   GLSS5 report, "Sampling Frame and Units": the 2000 Population and Housing
#     Census EA list.
#   GLSS6 (Community Facilities Report): "Sampling Frame created from the 2010
#     Census."  GLSS7 (Main Report): sampling "based on the 2010 Population and
#     Housing Census."
#
# There are therefore THREE frames, not two:
#   1984 census -> GLSS3, GLSS4      (7 and 14 years stale; GSS: "inadequate")
#   2000 PHC    -> GLSS5 only        (5 years stale)
#   2010 PHC    -> GLSS6, GLSS7      (2 and 6 years stale)
#
# WHY IT MATTERS HERE. Measured land ownership jumps 31.7% -> 79.1% between
# GLSS4 and GLSS5, and the jump appears in TWO independently administered
# questions (Section 8A "does any member own land" +24pp; Section 8B plot roster
# +45pp). The Section 8B instrument is unchanged across the break - GLSS4 Q5 and
# GLSS5 Q5 are materially identical - so this is a change in WHO WAS SAMPLED,
# not in what they were asked.
#
# NOT ALL FRAME CHANGES ARE EQUAL, and the study does not treat them as such.
# Re-basing 2000 -> 2010 (GLSS5 -> GLSS6) is routine maintenance between two
# reasonably current frames and moves ownership ~12pp. Fielding GLSS4 in 1998/99
# on a 1984 frame is 14 years stale, GSS itself calls that frame inadequate, and
# it moves ownership ~47pp. Treating every re-basing as disqualifying would rule
# out trend analysis in ANY repeated cross-section; the 1984-frame waves are the
# defensible exception, not the general case.
#
# EACH WAVE REMAINS INTERNALLY REPRESENTATIVE of its own frame, so all five
# waves are retained in the pooled/matched estimation. Temporal claims are
# restricted to GLSS5-GLSS7, which span the routine 2000->2010 re-basing but not
# the 1984-frame break. See narrative/sections/05_results.Rmd and
# narrative/diagnostics/tenure_variable_documentation.md.
study_data$CensusFrame <- ifelse(
  study_data$Surveyx %in% c("GLSS3", "GLSS4"), "1984",
  ifelse(study_data$Surveyx == "GLSS5", "2000", "2010")
)
# Waves used for temporal claims: excludes the 1984-frame rounds. NB this is not
# a single-frame subset -- it spans the 2000->2010 re-basing, which the text says
# explicitly rather than eliding.
study_data$TrendSample <- study_data$Surveyx %in% c("GLSS5", "GLSS6", "GLSS7")

# ---- Attach raw data to study environment (potential issue)
study_environment$study_raw_data <- study_data
# foreign::write.dta(
#   study_data, file.path(study_environment$wd$output,paste0(project_name,"_study_study_data.dta")),
#   convert.factors = c("labels"),convert.dates = T)

# ---- Save study environment object
# Save the entire study environment configuration (paths, metadata, etc.)
# for reproducibility and to simplify subsequent scripts.
saveRDS(
  study_environment,
  file.path(study_environment$wd$data, paste0(project_name,"_study_environment.rds"))
)
