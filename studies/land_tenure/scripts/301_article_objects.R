# 301_article_objects.R
# Assemble the numbers the narrative pulls from and write article_objects.json.
# Working directory is always the okwaayeli repo root.
#
# Everything is extracted from the same pooled meta-stochastic-frontier objects
# that scripts/100_exhibits.R uses, so the manuscript text and
# the exhibits cannot drift apart.
#
# Keying (mirrors the resource_extraction study; VERIFY against the results
# workbook before first use -- the TCHLvel codes below are the expected ones):
#   TCHLvel identifies the frontier: "National" (naive), "0" (non-owner),
#   "1" (owner), "Meta" (meta-frontier). `Tech` is only an analysis label and
#   must NOT be used to split groups.
#   Efficiency comparisons use the MATCHED sample (opt_sample); frontier
#   parameters (elasticities, gamma) use the UNMATCHED sample for group
#   frontiers, the matched sample for the meta-frontier.
if (!exists("OBJECTS_JSON")) source("studies/land_tenure/scripts/300_article_helpers.R")
suppressPackageStartupMessages(library(jsonlite))

EST <- file.path(OUTPUT, "estimations")

# Optimal matched-sample id (same selection 100_exhibits uses).
se_path <- file.path(DATA, "land_tenure_study_environment.rds")
mspecs  <- if (file.exists(se_path)) readRDS(se_path)$match_specification_optimal else NULL
opt_sample <- if (!is.null(mspecs)) ifelse(is.na(mspecs$link), mspecs$distance, mspecs$link) else NA

read_est <- function(tag)
  readRDS(file.path(EST, sprintf("CropID_Pooled_%s_TL_hnormal_optimal.rds", tag)))

NONE <- c("0", "No ownership", "Non-owner", "No")
ANY  <- c("1", "Some ownership", "Owner", "Yes")

grp_of <- function(df) {
  col <- intersect(c("TCHLvel", "Tech"), names(df))[1]
  as.character(df[[col]])
}
pick <- function(df, keep, value = "Estimate") {
  v <- df[[value]][keep]
  if (length(v) == 0) NA_real_ else as.numeric(v[1])
}
gap_of <- function(a, n) if (is.na(a) || is.na(n)) NA_real_ else a - n

# --- 1) Efficiency: aggregate + per tenure dimension (Table 4) ----------------
# OwnLnd = landownership status (main specification)
# LndOwn = ownership documentation forms
# LndRgt = ownership rights forms
TENURES <- c(ownership = "OwnLnd",
             documents = "LndOwn",
             rights    = "LndRgt")

eff_for <- function(tag) {
  ef <- read_est(tag)$ef_mean
  ef <- ef[ef$estType %in% "teBC" & ef$stat %in% "wmean" &
           ef$Survey  %in% "GLSS0" & ef$restrict %in% "Restricted" &
           ef$CoefName %in% "efficiency", ]
  if (!is.na(opt_sample) && "sample" %in% names(ef)) ef <- ef[ef$sample %in% opt_sample, ]
  g <- grp_of(ef)
  mk <- function(metric) {
    n <- pick(ef, ef$type %in% metric & g %in% NONE)
    a <- pick(ef, ef$type %in% metric & g %in% ANY)
    list(none = n, any = a, gap = gap_of(a, n))
  }
  list(tgr = mk("TGR"), te = mk("TE"), mte = mk("MTE"))
}
tenures <- lapply(TENURES, function(tg)
  tryCatch(eff_for(tg), error = function(e) { warning("301: ", tg, ": ", conditionMessage(e)); NULL }))

eff <- tenures$ownership   # objs$eff is the aggregate ownership comparison

# --- 2) Elasticities and returns to scale (Table 3) ---------------------------
# input_variables = c("Area","SeedKg","HHLaborAE","HirdHr","FertKg","PestLt")
# => el1..el6 are those inputs; el7 is the summed elasticity = returns to scale.
EL <- c(el1 = "land", el2 = "planting_materials", el3 = "family_labour",
        el4 = "hired_labour", el5 = "fertilizer", el6 = "pesticide", el7 = "rts")

pooled <- read_est("OwnLnd")
el <- pooled$el_mean
el <- el[el$stat %in% "wmean" & el$Survey %in% "GLSS0" & el$restrict %in% "Restricted", ]
if ("CoefName" %in% names(el) && any(el$CoefName %in% "elasticity"))
  el <- el[el$CoefName %in% "elasticity", ]          # levels (not Gap_lvl)
el_at <- function(inp, lv, samp)
  pick(el, el$input %in% inp & as.character(el$TCHLvel) %in% lv & el$sample %in% samp)
elasticities <- stats::setNames(lapply(names(EL), function(i) {
  n <- el_at(i, "0", "unmatched")
  a <- el_at(i, "1", "unmatched")
  list(naive = el_at(i, "National", "unmatched"),
       none  = n,
       any   = a,
       meta  = el_at(i, "Meta", opt_sample),
       gap   = gap_of(a, n))
}), unname(EL))

# --- 3) Diagnostics: gamma variance ratio (Table 3) ---------------------------
sf  <- pooled$sf_estm
sfg <- sf[sf$CoefName %in% "Gamma" & sf$restrict %in% "Restricted", ]
g_at <- function(lv, samp)
  pick(sfg, as.character(sfg$TCHLvel) %in% lv & sfg$sample %in% samp)
gamma <- list(naive = g_at("National", "unmatched"),
              none  = g_at("0",        "unmatched"),
              any   = g_at("1",        "unmatched"),
              meta  = g_at("Meta",     opt_sample))

# --- 4) Sample size -----------------------------------------------------------
# NOT extracted here: sf_estm's Nobs is the estimating model's N, not the
# 35,185 farm-household analysis sample quoted in the text. That figure
# belongs to the analysis-dataset extraction (Table 1) -- deliberately
# omitted rather than emitted incorrectly.

# --- Diagnostics --------------------------------------------------------------
if (anyNA(unlist(eff)) || anyNA(unlist(elasticities)) || anyNA(unlist(gamma))) {
  message("301 diagnostics (unresolved values above; codes actually present):")
  message("  ef_mean TCHLvel : {", paste(unique(grp_of(pooled$ef_mean)), collapse = ", "), "}")
  message("  el_mean input   : {", paste(unique(el$input), collapse = ", "), "}")
  message("  el_mean CoefName: {", paste(unique(pooled$el_mean$CoefName), collapse = ", "), "}")
  message("  sf_estm sample  : {", paste(unique(sfg$sample), collapse = ", "), "}")
  message("  el_mean sample  : {", paste(unique(el$sample), collapse = ", "), "}")
}

objs <- list(
  meta = list(
    generated      = as.character(Sys.time()),
    source         = "output/estimations/CropID_Pooled_<tenure>_TL_hnormal_optimal.rds",
    matched_sample = opt_sample
  ),
  eff          = eff,           # aggregate ownership comparison (Table 4)
  tenures      = tenures,       # Table 4 blocks: ownership/documents/rights
  elasticities = elasticities,  # Table 3: el1..el6 inputs + el7 = returns to scale
  diagnostics  = list(gamma = gamma)
)

jsonlite::write_json(objs, OBJECTS_JSON, auto_unbox = TRUE, pretty = TRUE, na = "null")
message("Wrote ", OBJECTS_JSON)
invisible(TRUE)
