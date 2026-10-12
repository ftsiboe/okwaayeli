# =============================================================================
#  DATA and SETUP - MIGRATION STUDY
# =============================================================================
#  Rural-urban migration and farm productivity in Ghana.
#
#  This script prepares the analysis-ready data. It:
#    - initializes the study environment (folders, paths, seed, layout),
#    - loads the harmonized crop-farmer release and the migration release
#      (data-raw/data-prep/glss/13_migration.do),
#    - merges them at the farmer level (Surveyx/EaId/HhId/Mid),
#    - restricts to GLSS3-GLSS7 (GLSS1/2 have no migration module) and to farms
#      in RURAL localities -- the destination side of urban-to-rural moves,
#    - builds the treatment and technology variables used by 002-004,
#    - saves data/migration_study_environment.rds.
#
#  TREATMENT DEFINITIONS (all built from harmonized_migration_data):
#    MigUrban      1 = previously lived in a Ghanaian town/city (urban origin)
#                  0 = never moved (no previous place of residence)
#                  NA = rural-origin or international movers (not in the
#                       primary contrast; see MigStatus)
#    MigStatus     Non-migrant / Rural origin / Urban origin
#                  -- separates "having moved" from "urban exposure"
#    MigUrbanType  Non-migrant / Urban returnee / Urban in-migrant
#    RemitUrban    household receives remittances from a sender in urban Ghana
#                  (the household / NELM channel; defined for all farmers)
#
#  COMPARABILITY (see the header of 13_migration.do for the full list):
#    - GLSS7 asks city/town/village, respondent-assessed; GLSS3-6 use the
#      statistical urban/rural place lists.
#    - Previous-occupation (MigPrevAgric) exists in GLSS5-7 only.
#    - Sampling frames change across rounds (1984 census -> GLSS3/4, 2000 PHC ->
#      GLSS5, 2010 PHC -> GLSS6/7); CensusFrame/TrendSample mirror land_tenure.
# =============================================================================

# ---- Housekeeping: clear workspace and run garbage collection
tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

# Paths: resolve the study root once, so this runs from a standalone checkout as
# well as from the okwaayeli monorepo. See scripts/_paths.R.
if (!exists("PROJECT_ROOT")) {
  .p <- c("scripts/_paths.R", "../scripts/_paths.R",
          "studies/migration/scripts/_paths.R")
  .p <- .p[file.exists(.p)]
  if (!length(.p))
    stop("cannot find scripts/_paths.R -- run from the study root.", call. = FALSE)
  source(.p[1])
}

okwaayeli_load()

run_only_for(id = 10, allowed_jobnames = "run_all")

# ---- Define study name and initialize study environment
project_name <- "migration"

# layout = "v2": plots and their data share output/figures/, table data goes to
# output/tables/. Downstream, use study_dir_figures() / study_dir_tables().
study_environment <- study_setup(project_name = project_name, layout = "v2")

# ---- Load harmonized farmer-level data
farmer_data <- get_household_data("harmonized_crop_farmer_data")

# The migration release is not on the hh_data GitHub release yet: read the local
# build written by data-raw/data-prep/glss/13_migration.do. Switch to
# get_household_data("harmonized_migration_data") once it is published.
migration_path <- "data-raw/releases/harmonized_data/harmonized_migration_data.dta"
if (!file.exists(migration_path))
  stop("001_DATA_migration_study.R: ", migration_path, " not found.\n",
       "  Build it in Stata: do data-raw/data-prep/glss/00_run_all.do 13",
       call. = FALSE)
migration_data <- as.data.frame(haven::read_dta(migration_path))

# ---- Merge at the farmer level
study_data <- dplyr::inner_join(
  farmer_data,
  migration_data,
  by = c("Surveyx", "EaId", "HhId", "Mid")
)

# ---- Restrict: survey rounds with a migration module, rural localities
study_data <- study_data[
  study_data$Surveyx %in% c("GLSS3","GLSS4","GLSS5","GLSS6","GLSS7"),
  names(study_data)[!grepl("EduWhyNo", names(study_data))]
]
# Locality is a factor ("Rural"/"Urban") after get_household_data().
study_data <- study_data[as.character(study_data$Locality) %in% "Rural", ]

# ---- Treatment and technology variables --------------------------------------
num <- function(x) as.numeric(haven::zap_labels(x))
lab <- function(x) as.character(haven::as_factor(x))

mover <- num(study_data$mig_mover)
purb  <- num(study_data$mig_prev_urban)
ret   <- num(study_data$mig_return)

study_data$MigUrban <- ifelse(mover %in% 0, 0,
                       ifelse(mover %in% 1 & purb %in% 1, 1, NA))

study_data$MigStatus <- ifelse(mover %in% 0, "Non-migrant",
                        ifelse(mover %in% 1 & purb %in% 0, "Rural origin",
                        ifelse(mover %in% 1 & purb %in% 1, "Urban origin", NA)))

study_data$MigUrbanType <- ifelse(mover %in% 0, "Non-migrant",
                           ifelse(purb %in% 1 & ret %in% 1, "Urban returnee",
                           ifelse(purb %in% 1 & ret %in% 0, "Urban in-migrant", NA)))

study_data$RemitUrban <- num(study_data$hh_remit_urban)

# Heterogeneity dimensions among movers (NA for non-migrants)
study_data$MigDurCat     <- lab(study_data$mig_dur_cat)
study_data$MigReason     <- lab(study_data$mig_reason)
study_data$MigPrevSettle <- lab(study_data$mig_prev_settle)
study_data$MigPrevAgric  <- num(study_data$mig_prev_agric)

# ---- Census sampling frame (same convention as land_tenure; see its 001) -----
study_data$CensusFrame <- ifelse(
  study_data$Surveyx %in% c("GLSS3", "GLSS4"), "1984",
  ifelse(study_data$Surveyx == "GLSS5", "2000", "2010")
)
study_data$TrendSample <- study_data$Surveyx %in% c("GLSS5", "GLSS6", "GLSS7")

# ---- Sanity check: group sizes by round (pooled crop rows) -------------------
.pooled <- study_data[as.character(study_data$CropID) %in% "Pooled", ]
print(table(.pooled$Surveyx, .pooled$MigStatus, useNA = "ifany"))
print(table(.pooled$Surveyx, .pooled$RemitUrban, useNA = "ifany"))
rm(.pooled)

# ---- Attach and save
study_environment$study_raw_data <- study_data

saveRDS(
  study_environment,
  file.path(study_environment$wd$data, paste0(project_name,"_study_environment.rds"))
)
