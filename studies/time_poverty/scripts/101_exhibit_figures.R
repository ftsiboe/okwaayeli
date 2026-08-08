# 100_exhibits.R  (10x = compute; see scripts/README.md)
# Builds output/figures/*.png and the .csv behind each one, from the estimation
# objects in output/estimations/.
#
# PROVENANCE. This file was, until the 2026-08 consolidation, the DISABILITY
# study's 100_FIGTAB script sitting under a time_poverty filename: it set
# project_name <- "disability", read CropID_Pooled_disabled_*.rds, and labelled
# every axis "Disabled less non-Disabled". It has been repaired to this study's
# treatment (tpoor0150) and object names.
#
# IT HAS NEVER RUN. output/estimations/ is EMPTY -- 004 has never executed for
# this study (its sbatch pointed at studies/disability/004_MSF_disability_study.R
# until the same consolidation). Every object name below is derived from 004's
# est_name construction, not observed on disk:
#
#   est_name <- paste0(disaggregate_variable, "_", disaggregate_level, "_",
#                      technology_variable, "_", names(fxnforms)[f], "_",
#                      names(distforms)[d], "_", matching_type)
#
# so CropID_Pooled_tpoor0150_TL_hnormal_optimal.rds. Treat the first run as
# verification, not as a pass.
#
# NO FALLBACKS. Every read asserts its file exists and every keying assumption
# asserts the levels it expects. A stop() naming the script to run is the
# designed failure; a figure built from the wrong key is not, because it looks
# right.
#
# Paths come from study_dir_figures() / study_dir_figure_data() /
# study_dir_tables() -- never a literal next to wd$output. See ?study_dirs.
#
# The builders (tab_main_specification, fig_heterogeneity00, fig_robustness,
# fig_input_te, fig_covariate_balance, fig_distribution) live in
# R/exhibits-figures.R and are reached through the namespace. The old
# source("data-raw/scripts/figures_and_tables.R") shim and its library() calls
# are gone: the package declares its dependencies in DESCRIPTION.
tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

library(ggplot2)

devtools::document()

project_name = "time_poverty"
study_environment <- readRDS(
  file.path(paste0("studies/", project_name, "/data"),
            paste0(project_name,"_study_environment.rds")))

# Repair wd in memory and create the folders. wd is a snapshot frozen into the
# .rds by whichever run last called study_setup(); without this the stage uses
# the layout as of the last MATCHING run -- which for this study was "legacy",
# i.e. output/figure/, a folder that no longer exists. See ?study_dirs.
study_environment <- study_dirs(study_environment, layout = "v2")

mspecs_optimal <- study_environment$match_specification_optimal

# ---- Estimation objects this script depends on ------------------------------
# TREATMENT is tpoor0150, set in 002 as DATA$Treat and passed to 004 as a
# technology_variable; tpoor0125 is the 1.25x companion. Both are built by
# data-raw/scripts/data-prep/glss/12_time_poverty.do -- WHICH DOES NOT BUILD WHAT ITS LABELS SAY. That
# script computes a committed-time cutoff, saves it, then restricts to s1q3==1
# and recomputes the same variable names off PAID time, overwriting the first.
# tpoor0150 in the release is the paid-time version. See the FLAG at the head of
# 12_time_poverty.do before this study reports what the treatment measures.
.est <- function(name) {
  p <- file.path(study_environment$wd$estimations, name)
  if (!file.exists(p))
    stop("100_exhibits.R: missing estimation object\n  ", p,
         "\n  Run 004 first: sbatch studies/time_poverty/scripts/job_msf.sbatch",
         call. = FALSE)
  p
}

EST_MAIN     <- "CropID_Pooled_tpoor0150_TL_hnormal_optimal.rds"
EST_CD       <- "CropID_Pooled_tpoor0150_CD_hnormal_optimal.rds"
EST_FULLSET  <- "CropID_Pooled_tpoor0150_TL_hnormal_fullset.rds"

Y_LEVEL <- "Level difference (Time-poor less non-time-poor)\n"
Y_ROBUST <- "\nLevel difference [Time-poor less non-time-poor]"

Keep.List <- c("Keep.List", ls())

# ---- Main specification -----------------------------------------------------
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- tab_main_specification(
  res_list          = c(.est(EST_MAIN)),
  study_environment = study_environment)

# The stacked MSF results, emitted for inspection. Nothing reads this back --
# it replaces the openxlsx::loadWorkbook round trip the disability copy did
# against <study>_results.xlsx, a workbook that has never existed here and whose
# absence made that call error rather than create it.
write.csv(res, file.path(study_dir_tables(study_environment), "msf_main_specification.csv"),
          row.names = FALSE)

# ---- Fig - Heterogeneity ----------------------------------------------------
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS(.est(EST_MAIN))$disagscors
if (is.null(res) || !nrow(res))
  stop("100_exhibits.R: ", EST_MAIN, " carries no disagscors.\n",
       "  004 only computes them for tpoor0150 + optimal + Pooled + CropID + f=2 + d=1.",
       call. = FALSE)
res$disasg <- as.character(res$disagscors_var)
res$level  <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_lvl",]
res <- res[c("disasg","level","fxnforms","distforms","Survey","input",
             "technology_variable","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]

fig <- fig_heterogeneity00(res = res, y_title = Y_LEVEL, study_environment = study_environment)
fig[["genderAge"]] <- fig[["genderAge"]] + theme(axis.text.x = element_text(size = 5.5))
ggsave(file.path(study_dir_figures(study_environment),"heterogeneity_crop_region.png"),
       fig[["crop_region"]], dpi = 600, width = 8, height = 5)
ggsave(file.path(study_dir_figures(study_environment),"heterogeneity_genderAge.png"),
       fig[["genderAge"]], dpi = 600, width = 8, height = 5)

# The data behind the figure, so every number the prose might quote off it is
# machine-checkable.
write.csv(res, file.path(study_dir_figure_data(study_environment),"heterogeneity.csv"),
          row.names = FALSE)

# ---- Fig - Robustness -------------------------------------------------------
rm(list= ls()[!(ls() %in% c(Keep.List))])
res_list <- c(.est(EST_CD),
              list.files(study_environment$wd$estimations,
                         pattern = "^CropID_Pooled_tpoor0150_TL_", full.names = TRUE))
if (length(res_list) < 2)
  stop("100_exhibits.R: robustness needs the TL variants alongside ", EST_CD,
       "; found ", length(res_list), ".", call. = FALSE)
fig_robustness(y_title = Y_ROBUST, res_list = res_list, study_environment = study_environment)

# ---- Fig - Matching treatment effects ---------------------------------------
rm(list= ls()[!(ls() %in% c(Keep.List))])
if (!file.exists(file.path(study_environment$wd$output, "te_summary.rds")))
  stop("100_exhibits.R: missing output/te_summary.rds. Run 003 first.", call. = FALSE)
fig_input_te(
  y_title    = "\nTime-poverty gap (%)",
  tech_lable = c("Full sample", "Time-poor sample", "Non-time-poor sample"),
  study_environment = study_environment)

# ---- Fig - Covariate balance ------------------------------------------------
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_covariate_balance(study_environment = study_environment)

# The balance table and the specification ranking, emitted for inspection.
# These were the "CovBalDATA" and "ranking" sheets of the disability workbook.
bal_tab <- study_environment$balance_table
ranking <- study_environment$match_specification_ranking
CovBalDATA <- rbind(bal_tab[(bal_tab$sample %in% "Un" & bal_tab$ARRAY %in% min(ranking$ARRAY)), ],
                    bal_tab[bal_tab$sample %in% "Adj", ])
CovBalDATA$sample <- ifelse(CovBalDATA$sample %in% "Un", CovBalDATA$sample,
                            ifelse(CovBalDATA$link %in% NA, CovBalDATA$distance, CovBalDATA$link))
CovBalDATA <- CovBalDATA[!CovBalDATA$value %in% NA, ]
CovBalDATA <- CovBalDATA[!CovBalDATA$Coef %in% NA, ]
CovBalDATA <- CovBalDATA[c("sample","stat","Coef","value")]
write.csv(CovBalDATA, file.path(study_dir_figure_data(study_environment),"covariate_balance.csv"),
          row.names = FALSE)
write.csv(ranking[c("ID","name","Diff.mean","V_Ratio.mean","KS.mean","rate.mean")],
          file.path(study_dir_tables(study_environment),"match_specification_ranking.csv"),
          row.names = FALSE)

# ---- Fig - Distribution -----------------------------------------------------
rm(list= ls()[!(ls() %in% c(Keep.List))])
dataFrq <- readRDS(.est(EST_FULLSET))$ef_dist
dataFrq <- dataFrq[dataFrq$estType %in% "teBC",]
dataFrq <- dataFrq[dataFrq$Survey %in% "GLSS0",]
dataFrq <- dataFrq[dataFrq$stat %in% "estimate_weight",]
dataFrq <- dataFrq[dataFrq$restrict %in% "Restricted",]

# KEY ON THE LABELLED COLUMN, NEVER THE NUMERIC ONE. 004 builds Tech as
# as.integer(as.factor(...)), so a 0/1 treatment becomes 1/2 -- it does NOT
# agree with TCHLvel, whose labels are the ORIGINAL tpoor0150 values plus the
# "National" and "Meta" sentinels. Keying on Tech transposes the two series with
# every other element of the figure intact, which is why this asserts rather
# than assumes.
.lvl <- setdiff(sort(unique(as.character(dataFrq$TCHLvel))), c("National","Meta"))
if (!identical(.lvl, c("0","1")))
  stop("100_exhibits.R: TCHLvel carries levels {", paste(.lvl, collapse = ", "),
       "}, not {0, 1}.\n  tpoor0150 was expected to reach 004 as an unlabelled 0/1",
       " indicator (see data-raw/scripts/data-prep/glss/12_time_poverty.do: gen TimPov15 = CommTime > Cutoff15).",
       "\n  Fix the labels below to match before trusting this figure.", call. = FALSE)
dataFrq$Tech <- factor(as.character(dataFrq$TCHLvel),
                       levels = c("0","1"),
                       labels = c("Non-time-poor","Time-poor"))

fig_distribution(dataFrq, study_environment = study_environment)
write.csv(dataFrq, file.path(study_dir_figure_data(study_environment),"score_distributions.csv"),
          row.names = FALSE)

# ---- Region and crop rankings quoted in the text ----------------------------
# The disability copy only paste0()'d these to the console. Anything the
# manuscript quotes has to be on disk or it cannot be checked.
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS(.est(EST_MAIN))$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level  <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
res <- res[res$input %in% "MTE",]

rankings <- do.call(rbind, lapply(c("Region","CROP"), function(v) {
  d <- res[res$disasg %in% v, c("disasg","level","Estimate","Estimate.sd","jack_pv")]
  d[order(d$Estimate), ]
}))
write.csv(rankings, file.path(study_dir_figure_data(study_environment),"gap_rankings.csv"),
          row.names = FALSE)

message("100_exhibits.R: complete. Figures in ", study_dir_figures(study_environment))
