# 301_article_objects.R
# Assemble the numbers the narrative pulls from and write article_objects.json.
# Working directory is always the okwaayeli repo root.
#
# The aggregate technology-gap / efficiency figures are extracted from the same
# pooled meta-stochastic-frontier object that scripts/100_exhibits.R uses, so the
# manuscript numbers stay in sync with the exhibits.
if (!exists("OBJECTS_JSON")) source("studies/resource_extraction/scripts/300_article_helpers.R")
suppressPackageStartupMessages(library(jsonlite))

EST <- file.path(OUTPUT, "estimations")

# Optimal matched-sample id (same selection 100_exhibits.R uses).
se_path <- file.path(DATA, "resource_extraction_study_environment.rds")
mspecs  <- if (file.exists(se_path)) readRDS(se_path)$match_specification_optimal else NULL
opt_sample <- if (!is.null(mspecs)) ifelse(is.na(mspecs$link), mspecs$distance, mspecs$link) else NA

pooled <- readRDS(file.path(EST, "CropID_Pooled_extraction_any_TL_hnormal_optimal.rds"))

# --- Aggregate group-level TGR / TE / MTE by extraction status -----------------
# ef_mean rows: estType (teBC), stat (wmean), Survey (GLSS0 = pooled),
# restrict (Restricted), sample (matched), type (TGR/TE/MTE),
# CoefName ("efficiency" = level; "efficiencyGap_lvl" = difference).
# The extraction GROUP is coded in TCHLvel (0 = No extraction, 1 = Some extraction),
# matching 100_exhibits.R: factor(TCHLvel, 0:1, c("No extraction","Any extraction")).
# NB: `Tech` in the extraction_any object is the analysis label ("Any extraction")
# on every row, so we must split on TCHLvel, not Tech.
ef <- pooled$ef_mean
ef <- ef[ef$estType %in% "teBC" & ef$stat %in% "wmean" &
         ef$Survey  %in% "GLSS0" & ef$restrict %in% "Restricted" &
         ef$CoefName %in% "efficiency", ]
if (!is.na(opt_sample) && "sample" %in% names(ef)) ef <- ef[ef$sample %in% opt_sample, ]

grp_col <- intersect(c("TCHLvel", "Tech"), names(ef))[1]
grp <- as.character(ef[[grp_col]])

grab <- function(metric, g) {
  v <- ef$Estimate[ef$type %in% metric & grp %in% g]
  if (length(v) == 0) NA_real_ else as.numeric(v[1])
}
mk <- function(metric) {
  none <- grab(metric, c("0", "No extraction", "No"))
  any  <- grab(metric, c("1", "Any extraction", "Some extraction", "Some"))
  list(none = none, any = any,
       gap = if (is.na(any) || is.na(none)) NA_real_ else any - none)
}
eff <- list(tgr = mk("TGR"), te = mk("TE"), mte = mk("MTE"))

# Diagnostics: if anything did not resolve, print the available group / type codes
# so the filter can be adjusted; also flag an implausible TGR ordering (extraction
# communities should have the LOWER technology gap ratio).
if (anyNA(unlist(eff)))
  message("301: unresolved values. Group column '", grp_col, "' = {",
          paste(unique(grp), collapse = ", "), "}; type = {",
          paste(unique(as.character(ef$type)), collapse = ", "), "}.")
if (!is.na(eff$tgr$none) && !is.na(eff$tgr$any) && eff$tgr$any > eff$tgr$none)
  warning("301: TGR(any) > TGR(none) - extraction group may be swapped; check TCHLvel coding.")

objs <- list(
  meta = list(
    generated      = as.character(Sys.time()),
    source         = "output/estimations/CropID_Pooled_extraction_any_TL_hnormal_optimal.rds",
    matched_sample = opt_sample
  ),
  eff = eff
  # TODO next object groups:
  #   elasticities  -> Table 3 (input elasticities / returns to scale)
  #   activities    -> Table 4 (mining / quarrying / sand winning MTE channels)
  #   heterogeneity -> Figures 2-3 (gender, age, education, crop, region)
)

jsonlite::write_json(objs, OBJECTS_JSON, auto_unbox = TRUE, pretty = TRUE, na = "null")
message("Wrote ", OBJECTS_JSON)
invisible(TRUE)
