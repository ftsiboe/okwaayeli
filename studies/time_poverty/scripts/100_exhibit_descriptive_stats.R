# 100_exhibit_descriptive_stats.R  (10x = compute; see scripts/README.md)
# STUB -- structure only. Reads study_raw_data, writes data/descriptive_exhibits.rds.
#
# This is the one exhibit stage that is NOT blocked on 004: study_raw_data
# already exists in the study environment. It is a stub because what it should
# compute is a manuscript decision -- which subgroups, which variables, which
# cuts -- and this study has no manuscript. The bootstrap below is real and
# verified; everything after the marker is the part that needs writing.
#
# The pattern to copy is studies/land_tenure/scripts/100_exhibit_descriptive_stats.R.
tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

project_name = "time_poverty"
study_environment <- readRDS(
  file.path(paste0("studies/", project_name, "/data"),
            paste0(project_name,"_study_environment.rds")))

# wd inside the .rds is a SNAPSHOT. Recompute or this stage writes into the
# retired "legacy" layout. See ?study_dirs.
study_environment <- study_dirs(study_environment, layout = "v2")

raw <- study_environment$study_raw_data
if (is.null(raw) || !nrow(raw))
  stop("100_exhibit_descriptive_stats.R: study_raw_data is absent from the ",
       "environment.\n  Run 001 + 002 (DATA alone strips it -- see run_article.R).",
       call. = FALSE)

message("100_exhibit_descriptive_stats.R: study_raw_data has ",
        format(nrow(raw), big.mark = ","), " rows, ", ncol(raw), " columns.")

# ============================================================================
# NOT YET WRITTEN
# ============================================================================
stop("100_exhibit_descriptive_stats.R is a STUB.\n",
     "  The bootstrap above works; the descriptive tables below it do not exist.\n",
     "  Write them against the manuscript's exhibit list, then delete this stop().\n",
     "  Target output: ",
     file.path(study_environment$wd$data, "descriptive_exhibits.rds"),
     call. = FALSE)

# saveRDS(descriptive_exhibits,
#         file.path(study_environment$wd$data, "descriptive_exhibits.rds"))
