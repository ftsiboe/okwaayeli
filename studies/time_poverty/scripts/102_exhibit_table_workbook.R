# 102_exhibit_table_workbook.R  (10x = compute; see scripts/README.md)
# STUB -- structure only. Writes output/tables/time_poverty_tables.xlsx.
#
# 102 exists to hand a co-author the same tables the article prints, as one
# workbook. It calls the SAME ft_*() builders the Rmd calls -- that is the whole
# point, and why it needs 100 and 101 current, exactly as the Rmd does.
#
# It is a stub because exhibit_helpers_tables.R has no ft_*() builders yet, and
# it has none because this study has no manuscript and therefore no exhibit
# list. See that file's header.
#
# The pattern to copy is studies/land_tenure/scripts/102_exhibit_table_workbook.R.
tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

project_name = "time_poverty"
study_environment <- readRDS(
  file.path(paste0("studies/", project_name, "/data"),
            paste0(project_name,"_study_environment.rds")))
study_environment <- study_dirs(study_environment, layout = "v2")

source("studies/time_poverty/scripts/exhibit_helpers_tables.R")

.builders <- ls(pattern = "^ft_")
if (!length(.builders))
  stop("102_exhibit_table_workbook.R is a STUB.\n",
       "  exhibit_helpers_tables.R defines no ft_*() builders yet, so there is\n",
       "  nothing to write into a workbook. Add the builders there first --\n",
       "  102 is deliberately a thin wrapper and should stay one.\n",
       "  Target output: ",
       file.path(study_environment$wd$tables, "time_poverty_tables.xlsx"),
       call. = FALSE)

# wb <- openxlsx::createWorkbook()
# for (b in .builders) { ... }
# openxlsx::saveWorkbook(wb, file.path(study_environment$wd$tables,
#                                      "time_poverty_tables.xlsx"), overwrite = TRUE)
