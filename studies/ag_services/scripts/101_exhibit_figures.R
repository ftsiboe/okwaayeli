# 101_exhibit_figures.R  (1## = exhibits; see scripts/README.md)
# Builds output/figures/*.png and the .csv/.rds behind each one, from the
# estimation objects in output/estimations/.
#
# ON THE modern001 SYSTEM (2026-08-13). Every published figure is now built by
# the fig_modern001_*() helpers in R/exhibits-figures-modern001.R. Those helpers
# are ADDITIVE: the older fig_*() builders are untouched and every other study
# still calls them. ag_services opted in; nothing else changed underneath it.
#
#   THE ONE RULE. Colour encodes WHICH MEASURE a value belongs to and nothing
#   else. Sign is carried by position against the zero rule, and significance by
#   whether a marker is filled or hollow. A reader learns three colours once.
#   The palette was validated rather than chosen; fig_modern001_check() prints
#   the report.
#
#   ONE DEFAULT CHANGED, DELIBERATELY. These figures draw 95 percent intervals.
#   The retired versions drew plus or minus one jackknife standard error, which
#   is not what a reader assumes an error bar means. No estimate changed; some
#   intervals now visibly cross zero that previously appeared not to. That is the
#   point of the change.
#
# TABLE 7 IS NOW FIGURE 1. The by-source estimates were a twelve-cell table whose
# entire content was that two of its columns carry opposite signs, which a table
# asks a reader to notice and a chart simply shows. ft_table7() IS DELIBERATELY
# LEFT IN PLACE in exhibit_helpers_tables.R: it is the numeric reference this
# figure is checked against, and service_source_gaps.csv below is written so the
# two can be diffed cell for cell. Do not delete the builder to tidy up.
#
# Which exhibit is which in the manuscript (narrative/sections/98 and 99):
#   Figure 1  service_source_gaps.png        NEW, was Table 7
#   Figure 2  score_trend.png                was Figure 1
#   Figure 3  score_by_services.png          was Figure 2
#   Figure 4  heterogeneity_genderAge.png    was Figure 3
#   Figure 5  heterogeneity_crop_region.png  was Figure 4
#   Figure S1 covariate_balance_love.png     was Covariate_balance_variance.png
#   Figure S2 robustness.png                  UNCHANGED, legacy builder
#
# FIGURE S1 IS NO LONGER GUARDED. The first draft of this script wrapped it in
# tryCatch and fell back to copying the legacy PNG, because the column names in
# covariate_balance.rds had been guessed rather than read. The guess was wrong and
# the fallback fired. The columns are now pinned against what
# fig_covariate_balance() actually saveRDS(), the block says so, and a mismatch
# stops the build with the columns it found. A supplementary figure that silently
# reverts is worse than one that fails.
#
# FIGURE S2 STAYS ON THE LEGACY BUILDER by choice. See its block below.
#   (unused)  input_TE.png, score_distributions.png -- built, referenced nowhere
#             in the v000 draft. land_tenure uses input_TE.png as its Figure 1.
#
# Three things inherited from the previous version and still true:
#   1. NO source("data-raw/scripts/figures_and_tables.R"). That file is a
#      deprecation shim that only loads the package.
#   2. NO directory literals. Every write goes through study_dir_figures() /
#      study_dir_figure_data() / study_dir_tables(). ag_services is on "v2".
#   3. NO openxlsx round trip. An exhibit that reads its numbers out of a
#      workbook is not an exhibit; those sheets are CSVs now.
#
# Run from the repo root, AFTER 004 (MSF).

tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

library(ggplot2)

devtools::document()

project_name <- "ag_services"
SE_RDS <- file.path(paste0("studies/", project_name, "/data"),
                    paste0(project_name, "_study_environment.rds"))
if (!file.exists(SE_RDS))
  stop("101: no study environment at ", SE_RDS, call. = FALSE)
study_environment <- readRDS(SE_RDS)

# Repair wd in memory and create the folders. wd is a snapshot frozen into the
# .rds by whichever run last called study_setup(), so without this a stage uses
# the layout as of the last MATCHING run. layout is passed explicitly so this
# works before 001 next re-runs and bakes it in. See ?study_dirs.
study_environment <- study_dirs(study_environment, layout = "v2")

mspecs_optimal <- study_environment$match_specification_optimal

# The matched-sample id, resolved once. Every modern001 figure below keys on it,
# and so does .t7_cell() in exhibit_helpers_tables.R -- which is why Figure 1 and
# the retired Table 7 agree cell for cell.
SAMPLE_ID <- ifelse(mspecs_optimal$link %in% NA, mspecs_optimal$distance,
                    mspecs_optimal$link)

# The four service sources. services0-3 are four SEPARATE binary treatments --
# four estimation objects, each with its own two-group frontier -- not one
# multi-level treatment.
SERVICE_TAGS <- c(services0 = "Any source",
                  services1 = "Agricultural/fishing association",
                  services2 = "Agricultural cooperative",
                  services3 = "Agricultural extension")

# Measure labels, and therefore palette slots, fixed in one place. Slot order is
# assignment order: technology first, efficiency second, their product third.
# Changing this line re-colours every figure in the study at once, which is the
# reason it is a constant rather than eight repeated literals.
MEASURES <- c("Technology gap ratio", "Technical efficiency",
              "Meta-technical efficiency")

MAIN <- file.path(study_environment$wd$estimations,
                  "CropID_Pooled_services0_TL_hnormal_optimal.rds")
if (!file.exists(MAIN))
  stop("101: missing the main estimation object\n  ", MAIN,
       "\n  Run the MSF stage (004) before the figures.", call. = FALSE)

FIG <- function(n) file.path(study_dir_figures(study_environment), n)
DAT <- function(n) file.path(study_dir_figure_data(study_environment), n)

# ef_mean carries the frontier group in a level column whose name has drifted
# across builds. Resolve it rather than hardcoding, and stop loudly rather than
# silently matching nothing: an unmatched level filter yields an empty figure,
# and an empty figure is easy to mistake for a data problem.
.level_col <- function(d) {
  hit <- intersect(c("TCHLvel", "TCHlvel", "TCHLevel", "TCH_level"), names(d))
  if (!length(hit))
    stop("101: no frontier-level column in ef_mean. Columns present: ",
         paste(names(d), collapse = ", "),
         "\n  Key on the LABELLED level column, never the numeric Tech. See the ",
         "KEYING block in exhibit_helpers_tables.R.", call. = FALSE)
  hit[[1L]]
}

# Every figure writes the data it plotted, so a claim in the prose can be
# machine-checked against the picture without re-reading a 37 MB object.
.emit <- function(d, stem) {
  saveRDS(d, file = DAT(paste0(stem, ".rds")))
  write.csv(d, file = DAT(paste0(stem, ".csv")), row.names = FALSE)
  invisible(d)
}

Keep.List <- c("Keep.List", ls())

# =============================================================================
#  Main specification table  (was the workbook's `msf` sheet)
# =============================================================================
rm(list = ls()[!(ls() %in% c(Keep.List))])

res_list <- file.path(study_environment$wd$estimations,
                      paste0("CropID_Pooled_", names(SERVICE_TAGS),
                             "_TL_hnormal_optimal.rds"))
.absent <- res_list[!file.exists(res_list)]
if (length(.absent))
  stop("101: missing estimation objects for the main specification:\n  ",
       paste(basename(.absent), collapse = "\n  "),
       "\n  Figure 1 reads all four service sources; a partial set would give a ",
       "figure\n  that looks complete and is not.", call. = FALSE)

res <- tab_main_specification(res_list = res_list,
                              study_environment = study_environment)

write.csv(res, file.path(study_dir_tables(study_environment),
                         "msf_main_specification.csv"), row.names = FALSE)

# =============================================================================
#  Figure 1 - service_source_gaps            NEW. Was Table 7.
# =============================================================================
# The aggregate gap in each of the three measures, for each of the four sources
# of provision, on one common scale.
#
# The extraction below mirrors .t7_cell() in exhibit_helpers_tables.R key for
# key: Survey GLSS0, estType teBC, stat wmean, restrict Restricted, the matched
# sample, CoefName efficiencyGap_lvl, and the level column at "1". That is not a
# coincidence to be preserved by luck -- it is why the figure and the retired
# table carry the same numbers, and it is checked below.
rm(list = ls()[!(ls() %in% c(Keep.List))])

by_source <- do.call(rbind, lapply(names(SERVICE_TAGS), function(tg) {
  f <- file.path(study_environment$wd$estimations,
                 paste0("CropID_Pooled_", tg, "_TL_hnormal_optimal.rds"))
  d  <- readRDS(f)$ef_mean
  lc <- .level_col(d)
  d  <- d[d$Survey   %in% "GLSS0" &
          d$estType  %in% "teBC" &
          d$stat     %in% "wmean" &
          d$restrict %in% "Restricted" &
          d$sample   %in% SAMPLE_ID &
          d$CoefName %in% "efficiencyGap_lvl" &
          as.character(d[[lc]]) %in% "1" &
          d$type     %in% c("TGR", "TE", "MTE"), ]
  if (!nrow(d))
    stop("101: no rows for ", tg, " in the Figure 1 extraction.\n",
         "  matched sample id sought: '", SAMPLE_ID, "'\n",
         "  ids present: ", paste(unique(readRDS(f)$ef_mean$sample),
                                  collapse = ", "), call. = FALSE)
  data.frame(source  = unname(SERVICE_TAGS[[tg]]),
             tag     = tg,
             measure = factor(d$type, levels = c("TGR", "TE", "MTE"),
                              labels = MEASURES),
             Estimate = d$Estimate, Estimate.sd = d$Estimate.sd,
             jack_pv  = d$jack_pv, stringsAsFactors = FALSE)
}))

# One row per source and measure, or the figure is drawing duplicates on top of
# each other and reporting whichever landed last.
if (anyDuplicated(by_source[c("source", "measure")]))
  stop("101: the Figure 1 extraction returned more than one row per source and ",
       "measure. The key is under-specified; do not de-duplicate to hide it.",
       call. = FALSE)

.emit(by_source, "service_source_gaps")

fig1 <- fig_modern001_decomposition(
  by_source, group = "source", measure = "measure", measures = MEASURES,
  groups = unname(SERVICE_TAGS),
  x_title = "Difference in level, service provided less not provided",
  base_size = 10)

fig_modern001_save(fig1, FIG("service_source_gaps.png"),
                   width = 7.0, height = 4.6)

message("101: Figure 1 covers ", length(unique(by_source$source)),
        " sources x ", length(unique(by_source$measure)), " measures. ",
        sum(!fig_modern001_significant(by_source$Estimate, by_source$Estimate.sd,
                                       by_source$jack_pv)),
        " estimate(s) drawn hollow, not distinguishable from zero.")

# =============================================================================
#  Figure 2 - score_trend                    (was Figure 1)
# =============================================================================
# Filters unchanged from the retired version, so the plotted values are the same
# ones. Only the form changed: lines rather than grouped bars, because a bar
# chart makes each round a separate comparison and hides the trajectory, which
# here is the finding.
rm(list = ls()[!(ls() %in% c(Keep.List))])

ef_mean <- readRDS(MAIN)$ef_mean
ef_mean <- ef_mean[ef_mean$stat %in% "wmean", ]
ef_mean <- ef_mean[ef_mean$estType %in% "teBC", ]
ef_mean$estm_type  <- "ef_mean"
ef_mean$level_type <- gsub("efficiency", "", ef_mean$CoefName)
ef_mean$level_type <- ifelse(ef_mean$level_type %in% "", "level", ef_mean$level_type)
ef_mean$CoefName   <- ef_mean$type
ef_mean <- ef_mean[c("technology_variable", "fxnforms", "distforms", "estm_type",
                     "level_type", "sample", "Survey", "restrict", "Tech",
                     "CoefName", "Estimate", "Estimate.sd", "jack_pv")]
ef_mean <- ef_mean[ef_mean$restrict %in% "Restricted", ]
ef_mean <- ef_mean[ef_mean$sample %in% SAMPLE_ID, ]
ef_mean <- ef_mean[ef_mean$level_type %in% "Gap_lvl", ]
ef_mean <- ef_mean[!ef_mean$CoefName %in% "TE0", ]
ef_mean <- ef_mean[!ef_mean$Survey %in% "GLSS0", ]

if (!nrow(ef_mean))
  stop("101: the trend figure has no rows after filtering. The matched sample ",
       "id is\n  '", SAMPLE_ID,
       "' -- check it against unique(ef_mean$sample).", call. = FALSE)

ef_mean$measure <- factor(ef_mean$CoefName, levels = c("TGR", "TE", "MTE"),
                          labels = MEASURES)

# Round labels are literal by repository convention, along with section numbers
# and the significance convention. Two lines rather than one: on a slope figure
# the round is an axis tick, not a legend entry, so it has room.
WAVES <- c(GLSS5 = "2005/06\n(GLSS5)", GLSS6 = "2012/13\n(GLSS6)",
           GLSS7 = "2016/17\n(GLSS7)")
.unmapped <- setdiff(unique(as.character(ef_mean$Survey)), names(WAVES))
if (length(.unmapped))
  stop("101: survey round(s) with no label: ", paste(.unmapped, collapse = ", "),
       "\n  Add them to WAVES. An unlabelled round is drawn as a bare code.",
       call. = FALSE)
ef_mean$wave <- unname(WAVES[as.character(ef_mean$Survey)])

.emit(ef_mean, "score_trend")

fig2 <- fig_modern001_slope(
  ef_mean, time = "wave", measure = "measure", measures = MEASURES,
  times = unname(WAVES[intersect(names(WAVES), unique(as.character(ef_mean$Survey)))]),
  y_title = "Difference in level, service provided less not provided",
  base_size = 10)

fig_modern001_save(fig2, FIG("score_trend.png"), width = 6.6, height = 4.4)

# =============================================================================
#  Figure 3 - score_by_services              (was Figure 2)
# =============================================================================
# The gap in each score, by TYPE of service the community provides. Reads the
# services_* slice of disagscors.
rm(list = ls()[!(ls() %in% c(Keep.List))])

res <- readRDS(MAIN)$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level  <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC", ]
res <- res[res$Survey  %in% "GLSS0", ]
res <- res[res$restrict %in% "Restricted", ]
res <- res[res$stat %in% "mean", ]
res <- res[!res$sample %in% "unmatched", ]
res <- res[res$CoefName %in% "disag_efficiencyGap_lvl", ]
res <- res[c("disasg", "level", "fxnforms", "distforms", "Survey", "input",
             "technology_variable", "Tech", "CoefName", "Estimate",
             "Estimate.sd", "jack_pv")]

res_all <- res[grepl("^services_", res$disasg), ]
res <- res_all[res_all$level %in% "1", ]
res <- res[order(res$disasg), ]

# WHY THIS FIGURE HAS FOUR ROWS AND disagscors HAS EIGHT SERVICES.
#
# disagscors is TWO-WAY. `disagscors_level` is the level of the disaggregating
# variable; TCHLvel is still the services0 frontier group. So a row keyed
# (disasg = services_credit, level = 1, TCHLvel = 1, disag_efficiencyGap_lvl) is
# "the served-minus-unserved gap AMONG communities that provide credit services".
#
# Four of the eight -- credit, husbandry, labour, records -- have no such row.
# Within those levels there is no untreated group to compare against: a
# community providing that service is served by construction, so the level-1
# cell is degenerate and 004 emits nothing. They DO carry a level-0 gap.
#
# The map covers all ten items the harmonizer can produce, so if a future build
# makes another level-1 gap estimable it arrives with a label instead of a bare
# "1". An unmapped variable STOPS: an axis category called "1" is how two
# services end up drawn on top of each other.
#
# Labels are single-line here. The retired version wrapped them because a
# rotated bar axis had no room; a dumbbell puts them flush left with the width
# of the page behind them.
SERVICE_LABELS <- c(
  services_planting      = "Planting and seed use",
  services_agchemicals   = "Fertilizer and agrochemicals",
  services_mechanization = "Mechanization",
  services_post_harvest  = "Post-harvest, marketing, storage",
  services_credit        = "Credit",
  services_irrigation    = "Irrigation",
  services_husbandry     = "Animal husbandry",
  services_employment    = "Employment",
  services_records       = "Farm records",
  services_labour        = "Communal labour")

.unmapped <- setdiff(unique(res$disasg), names(SERVICE_LABELS))
if (length(.unmapped))
  stop("101: services_* categories with no label: ",
       paste(.unmapped, collapse = ", "),
       "\n  Add them to SERVICE_LABELS. Leaving them unmapped collapses every ",
       "unnamed\n  category into one row -- which is the defect this check ",
       "exists to prevent.", call. = FALSE)
if (!nrow(res))
  stop("101: no services_* rows in disagscors. 004 only builds them for the ",
       "services0 / optimal / TL / hnormal cell -- check that MAIN is that ",
       "object.", call. = FALSE)

res$level <- unname(SERVICE_LABELS[res$disasg])

# No silent caps: say what is in the figure and what is not.
.dropped <- setdiff(sort(unique(res_all$disasg)), unique(res$disasg))
message("101: Figure 3 covers ", length(unique(res$disasg)), " of ",
        length(unique(res_all$disasg)), " services (",
        paste(sort(unique(res$disasg)), collapse = ", "), ").")
if (length(.dropped))
  message("     Not plotted, no level-1 gap estimable (degenerate cell -- ",
          "everyone in that\n     level is served): ",
          paste(.dropped, collapse = ", "),
          ".\n     Their level-0 gaps are in score_by_services_all_levels.csv.")

write.csv(res_all[order(res_all$disasg, res_all$level, res_all$input), ],
          file = DAT("score_by_services_all_levels.csv"), row.names = FALSE)

res$measure <- factor(res$input, levels = c("TGR", "TE", "MTE"),
                      labels = MEASURES)
res <- res[!is.na(res$measure), ]

.emit(res, "score_by_services")

# The dumbbell carries the two channels; the product goes in a text column
# rather than a third dot, because three dots on a row of four turns the
# comparison the figure exists to make back into arithmetic.
pair <- res[res$measure %in% MEASURES[1:2], ]
pair$measure <- droplevels(pair$measure)
net  <- res[res$measure %in% MEASURES[3], c("level", "Estimate")]
names(net)[2] <- "net"
pair <- merge(pair, net, by = "level", all.x = TRUE)

fig3 <- fig_modern001_dumbbell(
  pair, group = "level", measure = "measure", measures = MEASURES[1:2],
  order_by = MEASURES[2], band = MEASURES[1],
  third = "net", third_title = "Right-hand column: meta-technical efficiency",
  x_title = "Difference in level, service provided less not provided",
  base_size = 10)

fig_modern001_save(fig3, FIG("score_by_services.png"), width = 7.0,
                   height = max(3.4, 0.72 * length(unique(res$disasg)) + 1.6))

# =============================================================================
#  Figures 4 and 5 - heterogeneity           (were Figures 3 and 4)
# =============================================================================
# Small multiples: one column per measure, categories down the rows. Past about
# seven categories colour cannot carry identity, so identity moves to the row
# label and colour is freed to encode the measure. Adding hues instead is how a
# crop figure becomes unreadable.
rm(list = ls()[!(ls() %in% c(Keep.List))])

res <- readRDS(MAIN)$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level  <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC", ]
res <- res[res$Survey  %in% "GLSS0", ]
res <- res[res$restrict %in% "Restricted", ]
res <- res[res$stat %in% "mean", ]
res <- res[!res$sample %in% "unmatched", ]
res <- res[res$CoefName %in% "disag_efficiencyGap_lvl", ]
res <- res[c("disasg", "level", "input", "Estimate", "Estimate.sd", "jack_pv")]
res <- res[res$input %in% c("TGR", "TE", "MTE"), ]
res$measure <- factor(res$input, levels = c("TGR", "TE", "MTE"),
                      labels = MEASURES)

# The disaggregation keys are the ones fig_heterogeneity00() uses. They are
# named here rather than discovered so that a renamed key stops the build
# instead of quietly dropping a panel.
BLOCKS <- list(
  farmer = list(
    Female   = list(title = "Gender of operator",
                    levels = c("0" = "Men", "1" = "Women")),
    AgeCat   = list(title = "Age of operator",
                    levels = c("1" = "35 or under", "2" = "36 to 59",
                               "3" = "60 or over")),
    EduLevel = list(title = "Schooling of operator",
                    levels = c("0" = "None", "1" = "Primary",
                               "2" = "Junior secondary",
                               "3" = "Senior secondary",
                               "4" = "Post-secondary"))),
  place = list(
    CROP   = list(title = "Major crops", levels = NULL),
    Region = list(title = "Administrative regions", levels = NULL)))

.expand <- function(spec) {
  do.call(rbind, lapply(names(spec), function(k) {
    s <- spec[[k]]
    d <- res[res$disasg %in% k, ]
    if (!nrow(d))
      stop("101: no disagscors rows for '", k, "'. Keys present: ",
           paste(sort(unique(res$disasg)), collapse = ", "),
           "\n  Do not drop the panel to make the figure build.", call. = FALSE)
    if (!is.null(s$levels)) {
      miss <- setdiff(unique(d$level), names(s$levels))
      if (length(miss))
        stop("101: '", k, "' has unlabelled level(s): ",
             paste(miss, collapse = ", "), call. = FALSE)
      d$label <- unname(s$levels[d$level])
      d$label <- factor(d$label, levels = rev(unname(s$levels)))
    } else {
      ord <- d[d$measure %in% MEASURES[3], ]
      ord <- ord[order(ord$Estimate), "level"]
      d$label <- factor(d$level, levels = ord)
    }
    d$block <- s$title
    d
  }))
}

farmer <- .expand(BLOCKS$farmer)
place  <- .expand(BLOCKS$place)

.emit(farmer, "heterogeneity_genderAge")
.emit(place,  "heterogeneity_crop_region")

# strip_position = "left" puts each block heading outside the axis, horizontal,
# left-aligned and bold, alongside the rows it heads. The first build left it on
# ggplot's switched-strip default, which renders rotated and tucked between the
# axis labels and the panel, so it read as a stray caption rather than a heading.
# "top" stacks the blocks and puts the heading above each one instead; it is one
# argument away if the category labels ever grow long enough to push a left
# heading off the plot.
fig4 <- fig_modern001_grid(
  farmer, group = "label", measure = "measure", block = "block",
  measures = MEASURES, strip_position = "left",
  x_title = "Difference in level, any service less no service", base_size = 9)

fig5 <- fig_modern001_grid(
  place, group = "label", measure = "measure", block = "block",
  measures = MEASURES, strip_position = "left",
  x_title = "Difference in level, any service less no service", base_size = 9)

fig_modern001_save(fig4, FIG("heterogeneity_genderAge.png"),
                   width = 9.2, height = 5.2)
fig_modern001_save(fig5, FIG("heterogeneity_crop_region.png"),
                   width = 9.2, height = 6.6)

# =============================================================================
#  Figure S2 - robustness                    (legacy builder, deliberately)
# =============================================================================
# THIS ONE STAYS ON fig_robustness(). A modern001 specification curve was built
# here and then withdrawn at the author's request. The curve answers "does the
# result survive the whole space of choices"; fig_robustness() answers "what does
# each alternative give, measure by measure, against the preferred estimate",
# which is the question this appendix is actually asked. Keeping the old figure
# is a judgement about the exhibit, not a limitation of the new system.
#
# fig_modern001_speccurve() is still in R/exhibits-figures-modern001.R and is
# still exported. It is unused by ag_services and available to any study that
# wants it; do not delete it as dead code.
#
# fig_robustness() writes robustness.png / .rds / .csv itself, through the
# study_dir_* accessors.
rm(list = ls()[!(ls() %in% c(Keep.List))])

fig_robustness(
  y_title = "\nLevel difference [Any services less No services]",
  res_list = unique(c(
    MAIN,
    list.files(study_environment$wd$estimations,
               pattern = "^CropID_Pooled_services0_TL_", full.names = TRUE))),
  study_environment = study_environment)

# =============================================================================
#  Input / output treatment effects (input_TE.png) - built, currently unused
# =============================================================================
# Kept because it also writes output/figures/input_TE_data.csv, which is what
# land_tenure's fig1_range() inline lookups read. If ag_services adopts an
# equivalent exhibit (see the UNUSED FIGURES note in 98_tables_and_figures.Rmd),
# the data is already there.
rm(list = ls()[!(ls() %in% c(Keep.List))])

fig_input_te(
  y_title = "\nGap associated with agricultural services (%)",
  tech_lable = c("Full\nsample", "Any services\nsample", "No services\nsample"),
  study_environment = study_environment)

# =============================================================================
#  Figure S1 - covariate balance             (Love plot)
# =============================================================================
# fig_covariate_balance() still runs: it builds CovBalDATA internally and writes
# covariate_balance.rds / .csv. The Love plot is drawn from that, so there is one
# source of truth.
#
# It also fixed a hardcode the retired script kept: the unadjusted rows are taken
# from min(ranking$ARRAY), not the literal ARRAY 5, which broke whenever spec 5
# failed.
rm(list = ls()[!(ls() %in% c(Keep.List))])

fig_covariate_balance(study_environment = study_environment)

# COLUMNS PINNED 2026-08-13 against fig_covariate_balance()'s own saveRDS():
# CovBalDATA carries Coef (the covariate label), sample (a factor whose first
# level is "Unmatched" and whose remaining levels are the ranked matching
# specifications), stat (a three-level factor naming WHICH statistic), and value.
#
# Two traps an earlier draft of this block fell into. The covariate column is
# `Coef`, not `covariate` or `name` -- `name` exists but holds the specification
# name. And `stat` is a factor of statistic NAMES, not numbers, so it must be
# filtered on rather than plotted; without that filter the figure mixes
# standardized differences, variance ratios and KS statistics on one axis.
cb <- readRDS(DAT("covariate_balance.rds"))
if (!is.data.frame(cb)) cb <- as.data.frame(cb)

.need <- c("Coef", "sample", "stat", "value")
.miss <- setdiff(.need, names(cb))
if (length(.miss))
  stop("101: covariate_balance.rds lacks ", paste(.miss, collapse = ", "),
       ".\n  Columns present: ", paste(names(cb), collapse = ", "),
       "\n  Re-pin against fig_covariate_balance()'s saveRDS().", call. = FALSE)

SMD <- "Absolute Standardized Mean Differences"
if (!SMD %in% levels(factor(cb$stat)))
  stop("101: no '", SMD, "' rows in covariate_balance.rds. Levels present: ",
       paste(levels(factor(cb$stat)), collapse = " / "), call. = FALSE)
cb <- cb[as.character(cb$stat) %in% SMD, ]

# Which matched specification is the paper's. fig_covariate_balance() relabels
# `sample` from the raw id to the ranking's human name, so SAMPLE_ID cannot be
# matched directly; resolve it through the ranking table and say which was used.
.rk <- study_environment$match_specification_ranking
.after <- NULL
for (.k in intersect(c("distance", "link", "sample", "spec"), names(.rk))) {
  .hit <- unique(.rk$name[as.character(.rk[[.k]]) %in% SAMPLE_ID])
  if (length(.hit) == 1L) { .after <- .hit; break }
}
if (is.null(.after)) {
  .after <- .rk$name[[which.min(.rk$ID)]]
  message("101: could not map the matched-sample id '", SAMPLE_ID, "' onto the ",
          "ranking table,\n     so Figure S1 uses the top-ranked specification, '",
          .after, "'. Confirm that is the\n     one the paper reports before ",
          "submission.")
}
.lv <- levels(factor(cb$sample))
if (!"Unmatched" %in% .lv || !.after %in% .lv)
  stop("101: Figure S1 needs both 'Unmatched' and '", .after, "' among the ",
       "sample levels.\n  Levels present: ", paste(.lv, collapse = " / "),
       call. = FALSE)

.key  <- paste(cb$Coef, cb$sample)
wide  <- data.frame(covariate = unique(as.character(cb$Coef)),
                    stringsAsFactors = FALSE)
wide$before <- cb$value[match(paste(wide$covariate, "Unmatched"), .key)]
wide$after  <- cb$value[match(paste(wide$covariate, .after), .key)]
wide <- wide[stats::complete.cases(wide), ]
if (!nrow(wide))
  stop("101: no covariate appears in both the unmatched and the '", .after,
       "' sample.", call. = FALSE)

# Sixty-odd covariates do not fit one legible column on a landscape page, so the
# rows are split across panels. Panel 1 carries the largest imbalances.
.cols <- if (nrow(wide) > 34) 2L else 1L
figS1 <- fig_modern001_balance(
  wide, order_by = "before", ncol = .cols,
  labels = c("Before matching", paste0("After matching (", .after, ")")),
  base_size = 8)

fig_modern001_save(figS1, FIG("covariate_balance_love.png"), width = 9.2,
                   height = min(6.4, max(3.8,
                                         0.155 * ceiling(nrow(wide) / .cols) + 1.5)))

message("101: Figure S1 is a Love plot over ", nrow(wide), " covariates in ",
        .cols, " panel(s); ", sum(wide$after > 0.1),
        " remain above the 0.10 threshold after matching.")

# The specification ranking behind Table S6  (was the workbook's `ranking` sheet)
ranking <- study_environment$match_specification_ranking

# DEFECT NOT INHERITED. The retired script asked for Diff.mean / V_Ratio.mean /
# KS.mean / rate.mean. The ranking object carries Diff / V_Ratio / KS / rate --
# no `.mean` suffix -- so those four columns were NA or the write failed. Names
# are checked rather than assumed.
.want <- c("ID", "name", "Diff", "V_Ratio", "KS", "rate")
.miss <- setdiff(.want, names(ranking))
if (length(.miss))
  stop("101: match_specification_ranking lacks ", paste(.miss, collapse = ", "),
       ".\n  Columns present: ", paste(names(ranking), collapse = ", "),
       "\n  Re-pin against the object before editing -- do not rename to fit.",
       call. = FALSE)

write.csv(ranking[.want],
          file.path(study_dir_tables(study_environment),
                    "match_specification_ranking.csv"), row.names = FALSE)

# =============================================================================
#  Score distributions (score_distributions.png) - built, currently unused
# =============================================================================
# Reads the *_fullset* object: ef_dist is one row per farmer per draw and is the
# reason that file is ~93 MB, so nothing else touches it.
rm(list = ls()[!(ls() %in% c(Keep.List))])

FULLSET <- file.path(study_environment$wd$estimations,
                     "CropID_Pooled_services0_TL_hnormal_fullset.rds")
if (!file.exists(FULLSET)) {
  message("101: no ", basename(FULLSET),
          " -- skipping score_distributions.png. Nothing in the v000 draft ",
          "references it; re-run 004's fullset cell if an exhibit starts to.")
} else {
  dataFrq <- readRDS(FULLSET)$ef_dist
  dataFrq <- dataFrq[dataFrq$estType  %in% "teBC", ]
  dataFrq <- dataFrq[dataFrq$Survey   %in% "GLSS0", ]
  dataFrq <- dataFrq[dataFrq$stat     %in% "estimate_weight", ]
  dataFrq <- dataFrq[dataFrq$restrict %in% "Restricted", ]
  # Keyed on the LABELLED level column, never the numeric Tech. See the KEYING
  # block in exhibit_helpers_tables.R.
  dataFrq$Tech <- factor(as.numeric(as.character(dataFrq$TCHLvel)),
                         levels = 0:1,
                         labels = c("No services", "Some services"))

  .emit(dataFrq, "score_distributions")

  # fig_distribution(), not fig_dsistribution(): the misspelling is a deprecated
  # alias that warns and forwards. See ?fig_distribution.
  fig_distribution(dataFrq, study_environment = study_environment)
}

# =============================================================================
#  Crop and region ranking, for the prose
# =============================================================================
rm(list = ls()[!(ls() %in% c(Keep.List))])

res <- readRDS(MAIN)$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level  <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC", ]
res <- res[res$Survey  %in% "GLSS0", ]
res <- res[res$restrict %in% "Restricted", ]
res <- res[res$stat %in% "mean", ]
res <- res[!res$sample %in% "unmatched", ]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct", ]
res <- res[res$input %in% "MTE", ]

# Emitted so the crop/region claims in the prose can be machine-checked. 301
# reads this rather than the paste0() strings the retired script printed to the
# console and nobody captured.
write.csv(res[c("disasg", "level", "Survey", "input", "CoefName", "Estimate",
                "Estimate.sd", "jack_pv")],
          file = DAT("mte_gap_pct_crop_region.csv"), row.names = FALSE)

reg <- res[res$disasg %in% "Region", ]
reg <- reg[order(reg$Estimate), ]
message("Regions, worst to best: ",
        paste0(paste0(reg$level, " (", round(reg$Estimate, 2), "%)"),
               collapse = ", "))

CROP <- res[res$disasg %in% "CROP", ]
CROP <- CROP[order(CROP$Estimate), ]
message("Crops, worst to best: ",
        paste0(paste0(CROP$level, " (", round(CROP$Estimate, 2), "%)"),
               collapse = ", "))

message("101: complete. Figures in ", study_dir_figures(study_environment))
message("101: palette in use --")
fig_modern001_check()
invisible(TRUE)
