# 301_article_objects.R
# Assemble the numbers the narrative pulls from and write article_objects.json.
# Working directory is always the okwaayeli repo root.
#
# Everything is extracted from the same pooled meta-stochastic-frontier objects
# that scripts/101_exhibit_figures.R uses, so the manuscript text and
# the exhibits cannot drift apart.
#
# Keying:
#   TCHLvel identifies the frontier: "National" (naive), "0" (non-owner),
#   "1" (owner), "Meta" (meta-frontier). `Tech` is only an analysis label and
#   must NOT be used to split groups -- it carries a different coding for the
#   same concept, so it silently transposes the groups.
#   Efficiency comparisons use the MATCHED sample (opt_sample); frontier
#   parameters (elasticities, gamma) use the UNMATCHED sample for group
#   frontiers, the matched sample for the meta-frontier.
if (!exists("OBJECTS_JSON")) source("studies/land_tenure/scripts/article_helpers.R")
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

# --- 1) Efficiency: the aggregate ownership comparison ------------------------
# OwnLnd ONLY, and deliberately. Its TCHLvel is binary (0 = no ownership,
# 1 = some), which is what NONE/ANY and gap_of() assume.
#
# Do NOT extend this to LndOwn/LndRgt. Those frontiers key 1 = reference,
# 2/3/4 = categories, so the binary coding silently yields none = NA, gap = NA,
# and `any` = the REFERENCE category under the wrong label -- a real number
# meaning its opposite. Table 4 covers documentation and rights live, keyed
# correctly, and feeds the prose the same build via .LIVE_IDS.

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

# objs$eff is the aggregate ownership comparison.
eff <- tryCatch(eff_for("OwnLnd"),
                error = function(e) {
                  warning("301: OwnLnd: ", conditionMessage(e)); NULL
                })

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

# --- 2b) Matched stored gaps (published Table 3 gap column) -------------------
# The stored Gap_lvl carries its own SE/p-value on the MATCHED sample; this is
# the figure the text quotes (e.g. RTS delta = 0.088), not any(un) - none(un).
elg <- pooled$el_mean
elg <- elg[elg$stat %in% "wmean" & elg$Survey %in% "GLSS0" &
           elg$restrict %in% "Restricted", ]
.gcoef <- unique(as.character(elg$CoefName))
.gcoef <- .gcoef[grepl("Gap_lvl$", .gcoef)][1]
for (i in names(EL)) {
  b <- elg[elg$input %in% i & elg$CoefName %in% .gcoef &
           as.character(elg$TCHLvel) %in% "1" & elg$sample %in% opt_sample, ]
  elasticities[[EL[[i]]]]$gapm <- if (nrow(b)) as.numeric(b$Estimate[1]) else NA_real_
}

# --- 3) Diagnostics: gamma variance ratio (Table 3) ---------------------------
sf  <- pooled$sf_estm
sfg <- sf[sf$CoefName %in% "Gamma" & sf$restrict %in% "Restricted", ]
g_at <- function(lv, samp)
  pick(sfg, as.character(sfg$TCHLvel) %in% lv & sfg$sample %in% samp)
gamma <- list(naive  = g_at("National", "unmatched"),
              none   = g_at("0",        "unmatched"),
              any    = g_at("1",        "unmatched"),
              meta   = g_at("Meta",     opt_sample),
              meta_u = g_at("Meta",     "unmatched"))

# --- 3b) Theoretical-property satisfaction rates (Table 3 diagnostics) --------
sfr <- sf[sf$restrict %in% "Restricted" & sf$Survey %in% "GLSS0", ]
r_at <- function(coef, lv, samp)
  pick(sfr, sfr$CoefName %in% coef & as.character(sfr$TCHLvel) %in% lv &
            sfr$sample %in% samp)
rates <- function(coef) list(
  naive  = r_at(coef, "National", "unmatched"),
  none   = r_at(coef, "0",        "unmatched"),
  any    = r_at(coef, "1",        "unmatched"),
  meta_m = r_at(coef, "Meta",     opt_sample),
  meta_u = r_at(coef, "Meta",     "unmatched"))
mono <- rates("mono")
curv <- rates("curv")

# --- 3c) Ownership gaps within acquisition / sharecropping categories --------
# disag_efficiencyGap_lvl, matched sample: (no ownership minus some ownership)
# within each category. Labels per data-raw/okwaayeli_DATA.do.
dg <- pooled$disagscors
dg$disasg <- as.character(dg$disagscors_var)
dg$level  <- as.character(dg$disagscors_level)
dg <- dg[dg$estType %in% "teBC" & dg$Survey %in% "GLSS0" &
         dg$restrict %in% "Restricted" & dg$stat %in% "mean" &
         !dg$sample %in% "unmatched" &
         dg$CoefName %in% "disag_efficiencyGap_lvl", ]
.dgv <- function(var, lv, metric)
  pick(dg, dg$disasg %in% var & dg$level %in% lv & dg$input %in% metric)
.dgset <- function(var, lv) list(tgr = .dgv(var, lv, "TGR"),
                                 te  = .dgv(var, lv, "TE"),
                                 mte = .dgv(var, lv, "MTE"))
acq <- list(free      = .dgset("LndAq", "1"),
            sharecrop = .dgset("LndAq", "2"),
            rented    = .dgset("LndAq", "3"),
            purchased = .dgset("LndAq", "4"),
            kinship   = .dgset("LndAq", "5"),
            other     = .dgset("LndAq", "6"))
shrcrp <- list(none = .dgset("ShrCrpCat", "1"),
               low  = .dgset("ShrCrpCat", "2"),
               high = .dgset("ShrCrpCat", "3"))

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

# --- Ownership prevalence by wave (sampling-frame break) ----------------------
# Feeds the frame caveat in 02_data.Rmd and 05_results.Rmd. Emitted here rather
# than hardcoded in the prose so the text cannot drift from the analysis data.
#
# WHY THIS IS IN THE PAPER AT ALL: measured ownership jumps from ~32% (GLSS4) to
# ~79% (GLSS5). That is not our construction -- the Section 8b ownership question
# is unchanged across the break, and the same jump appears independently in
# Section 8A ("does any member own land", +24pp). GSS changed the sampling frame
# at exactly this boundary: GLSS3/GLSS4 were drawn from the 1984 census EAs (the
# GLSS4 report calls that frame "quite old" and "inadequate"), GLSS5 onward from
# the 2000 PHC. Each wave stays internally representative, so all five are kept
# in the pooled estimation, but comparisons spanning GLSS4/GLSS5 are not drawn
# from a common population -- hence the trend analysis is restricted to GLSS5-7.
# Full verification trail: narrative/diagnostics/tenure_variable_documentation.md
frame <- local({
  se <- if (file.exists(se_path)) readRDS(se_path) else NULL
  # estimation_data is attached by 002_MATCHING; study_raw_data by 001_DATA.
  # Accept either: running 001 alone re-saves the environment WITHOUT
  # estimation_data (002 has not re-attached it yet), and the ownership
  # prevalence is identical in both -- 002's harmonized_data_prep() transforms
  # and adds columns but drops no rows and no tenure variables.
  d <- if (is.null(se)) NULL
       else if (!is.null(se$estimation_data)) se$estimation_data
       else se$study_raw_data
  if (is.null(d)) {
    message("301: no estimation_data or study_raw_data in the study environment; ",
            "objs$frame not emitted. Run 001 (and ideally 002) first.")
    return(NULL)
  }
  wv <- intersect(c("Surveyx", "Survey"), names(d))[1]
  if (is.na(wv) || !"OwnLnd" %in% names(d)) {
    message("301: need OwnLnd + Surveyx/Survey for objs$frame; found: ",
            paste(intersect(c("Surveyx", "Survey", "OwnLnd"), names(d)), collapse = ", "))
    return(NULL)
  }
  # THE ANALYSIS SAMPLE IS CropID == "Pooled" -- 35,185 rows, matching Table 1's
  # header and N_ALL. Do NOT de-duplicate on (wave, EaId, HhId, Mid): that key is
  # not unique within Pooled (28,411 distinct keys vs 35,185 rows), so deduping
  # silently drops 6,774 observations and biases the wave prevalences by 1-2pp.
  # The group sizes sum to 13,099 / 22,086 / 35,185; Table 1's header is the
  # check.
  dd <- d[as.character(d$CropID) %in% "Pooled", ]
  if (!nrow(dd)) {
    message("301: no CropID == 'Pooled' rows; objs$frame not emitted.")
    return(NULL)
  }
  p <- tapply(as.numeric(dd$OwnLnd), as.character(dd[[wv]]), mean, na.rm = TRUE)
  # Expected shares owning, by wave:
  #   GLSS3 .4319 (1509/3494) | GLSS4 .3206 (1584/4941) | GLSS5 .8114 (5766/7106)
  #   GLSS6 .7099 (8708/12266) | GLSS7 .6125 (4519/7378)
  # Frame gaps the narrative quotes: GLSS4->GLSS5 = 49pp; GLSS5->GLSS6 = 10pp.
  list(
    own_prev = as.list(round(p, 4)),                       # share owning, by wave
    n        = as.list(table(as.character(dd[[wv]]))),
    census   = list(GLSS3 = "1984", GLSS4 = "1984",
                    GLSS5 = "2000", GLSS6 = "2010", GLSS7 = "2010"),
    trend_waves = c("GLSS5", "GLSS6", "GLSS7")             # common-frame subset
  )
})

objs <- list(
  meta = list(
    generated      = as.character(Sys.time()),
    source         = "output/estimations/CropID_Pooled_<tenure>_TL_hnormal_optimal.rds",
    matched_sample = opt_sample
  ),
  frame        = frame,         # ownership prevalence by wave + census frame
  eff          = eff,           # aggregate ownership comparison (Table 4)
  # No `tenures` field: documentation/rights come from Table 4, not from here.
  # See the note at eff_for().
  elasticities = elasticities,  # Table 3: el1..el6 inputs + el7 = returns to scale
  acq          = acq,           # ownership gap within acquisition modes
  shrcrp       = shrcrp,        # ownership gap by sharecropping intensity
  diagnostics  = list(gamma = gamma, mono = mono, curv = curv)
)

jsonlite::write_json(objs, OBJECTS_JSON, auto_unbox = TRUE, pretty = TRUE, na = "null")
message("Wrote ", OBJECTS_JSON)
invisible(TRUE)
