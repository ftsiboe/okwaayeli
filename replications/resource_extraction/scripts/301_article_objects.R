# 301_article_objects.R
# Assemble the numbers the narrative pulls from and write article_objects.json.
# Working directory is always the okwaayeli repo root.
#
# The aggregate technology-gap / efficiency figures are extracted from the same
# pooled meta-stochastic-frontier object that scripts/100_exhibits.R uses, so the
# manuscript numbers stay in sync with the exhibits.
if (!exists("OBJECTS_JSON")) source("replications/resource_extraction/scripts/300_article_helpers.R")
suppressPackageStartupMessages(library(jsonlite))

EST <- file.path(OUTPUT, "estimations")

# Optimal matched-sample id (same selection 100_exhibits.R uses).
se_path <- file.path(DATA, "resource_extraction_study_environment.rds")
mspecs  <- if (file.exists(se_path)) readRDS(se_path)$match_specification_optimal else NULL
opt_sample <- if (!is.null(mspecs)) ifelse(is.na(mspecs$link), mspecs$distance, mspecs$link) else NA

pooled <- readRDS(file.path(EST, "CropID_Pooled_extraction_any_TL_hnormal_optimal.rds"))

# --- Aggregate group-level TGR / TE / MTE by extraction status -----------------
# ef_mean rows: estType (teBC), stat (wmean), Survey (GLSS0 = pooled),
# restrict (Restricted), sample (matched), type (TGR/TE/MTE), Tech (0/1),
# CoefName ("efficiency" = level; "efficiencyGap_lvl" = difference).
ef <- pooled$ef_mean
ef <- ef[ef$estType %in% "teBC" & ef$stat %in% "wmean" &
         ef$Survey  %in% "GLSS0" & ef$restrict %in% "Restricted" &
         ef$CoefName %in% "efficiency", ]
if (!is.na(opt_sample)) ef <- ef[ef$sample %in% opt_sample, ]

# Tech may be coded 0/1 or labelled; match both.
none_lab <- c("0", "No extraction", "No")
any_lab  <- c("1", "Any extraction", "Any")
grab <- function(metric, labs) {
  v <- ef$Estimate[ef$type %in% metric & as.character(ef$Tech) %in% labs]
  if (length(v) == 0) NA_real_ else as.numeric(v[1])
}
mk <- function(metric) {
  none <- grab(metric, none_lab); any <- grab(metric, any_lab)
  list(none = none, any = any, gap = any - none)
}
eff <- list(tgr = mk("TGR"), te = mk("TE"), mte = mk("MTE"))

# Guard: warn (do not stop) if the aggregate figures did not resolve, so the
# knit still runs and the NAs are visible in the draft.
for (m in names(eff)) for (k in c("none", "any"))
  if (is.na(eff[[m]][[k]]))
    warning(sprintf("301: %s$%s did not resolve - check ef_mean filters.", m, k))

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
