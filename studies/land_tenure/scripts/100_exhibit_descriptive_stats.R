# 100_exhibit_descriptive_stats.R
# Compute Tables 1, 2 and S1-S4 from study_raw_data and cache them to
# data/descriptive_exhibits.rds, which exhibit_helpers_tables.R reads at knit
# time.
#
# WHY A SEPARATE STEP: the engine fits a model per (treatment x crop x outcome)
# -- ~250s for the pooled sample alone, longer with crops. Too slow to run
# inside every knit, hence compute-once-and-cache.
#
# The engine (R/descriptive-exhibits-core.R) is validated against the Stata
# workbooks it replaced: resource_extraction (14,405 assertions) and land_tenure
# (678). See tests/testthat/test-descriptive-exhibits-*.R.
#
# Run from the repo root.

tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})  

devtools::document()

STUDY   <- "studies/land_tenure"
SE_RDS  <- file.path(STUDY, "data", "land_tenure_study_environment.rds")
OUT_RDS <- file.path(STUDY, "data", "descriptive_exhibits.rds")

stopifnot(file.exists(SE_RDS))
d <- readRDS(SE_RDS)$study_raw_data
message("study_raw_data: ", nrow(d), " rows")

# ---- Table 1 -----------------------------------------------------------------
# Two models, one table: OLS over the continuous outcomes, logit over the binary
# ones. The Stata workbook this replaced recorded no distinction between them;
# the spec grid does, via `families`.
CONT <- c("Yield", "Area", "SeedKg", "HHLaborAE", "HirdHr", "FertKg",
          "PestLt", "AgeYr", "YerEdu", "HHSizeAE", "Depend", "CrpMix")
BIN  <- c("Female", "EqipMech", "Credit", "Extension", "EqipIrig")

# Crops carrying a Table 1 row (the crop production block), plus Pooled.
T1_CROPS <- c("Pooled", "Maize", "Rice", "Millet", "Sorghum", "Beans", "Peanut",
              "Cassava", "Yam", "Cocoyam", "Plantain", "Pepper", "Okra",
              "Tomatoe", "Cocoa", "Palm")

spec <- descriptive_specifications(
  d,
  outcomes   = c(CONT, BIN),
  treatments = "OwnLnd",
  crops      = intersect(T1_CROPS, unique(as.character(d$CropID))),
  families   = c(stats::setNames(rep("gaussian", length(CONT)), CONT),
                 stats::setNames(rep("binomial",  length(BIN)),  BIN)))

message("Table 1: ", nrow(spec), " specifications ...")
t1 <- draw_descriptive_summary(spec, d, study = "land_tenure")

# ---- Tables 2 and S1-S4 ------------------------------------------------------
# GLSS6/GLSS7 only. This is a COMPARABILITY restriction, not an availability
# one: all five rounds administer all four items (see data/tables/tableS0.csv,
# transcribed from the questionnaires). Per S0:
#
#   LndAq      no purchase option before GLSS5; "Inherited" exists only in GLSS7
#              and is folded into kinship, where it is the larger component
#   ShrCrpCat  GLSS5's coded fractions are a narrower set than GLSS6/GLSS7's
#   GLSS3/4    1984 frame; excluded from every temporal claim (see 001)
#
# LndOwn and LndRgt are identically coded in all five rounds and could span
# GLSS5-GLSS7 alone. They are held here anyway: Table 2 puts all four variables
# under one pooled column, so a per-variable window would give it a header true
# of only half its rows, and would let Table 2 and S1/S3 report different shares
# for the same variable.
#
# NB 001's study_data$TrendSample (GLSS5-GLSS7) governs the EFFICIENCY trend,
# not this window. The descriptives are deliberately narrower.
#
# Trend flavor is wave_diff: logit on i.Survey, then nlcom (GLSS6 - GLSS7) * 100
# -- percentage POINTS, earlier minus later.
DESC_WAVES <- c("GLSS6", "GLSS7")
dt <- d[as.character(d$Surveyx) %in% DESC_WAVES, , drop = FALSE]
for (v in c("LndOwn", "LndRgt", "LndAq", "ShrCrpCat"))
  if (v %in% names(dt)) dt <- descriptive_expand_category(dt, v)

IND <- intersect(c("OwnLnd", paste0("LndOwn_", 1:3), paste0("LndRgt_", 1:4),
                   paste0("LndAq_", 1:6), paste0("ShrCrpCat_", 1:3)), names(dt))

crops_b <- unique(as.character(dt$CropID))
message("Tables 2/S1-S4: ", length(IND), " indicators x ", length(crops_b), " crops ...")

t2 <- do.call(rbind, lapply(crops_b, function(cr) {
  dc <- dt[as.character(dt$CropID) == cr, , drop = FALSE]
  if (!nrow(dc)) return(NULL)
  r <- try(descriptive_indicator_shares(
    descriptive_prepare(dc), IND,
    trend = "wave_diff", waves = DESC_WAVES, per_wave = TRUE),
    silent = TRUE)
  if (inherits(r, "try-error") || is.null(r)) {
    message("  no shares for crop: ", cr)
    return(NULL)
  }
  r$crop <- cr
  r
}))

# ---- Cache -------------------------------------------------------------------
res <- list(table1 = t1, shares = t2,
            meta = list(generated = as.character(Sys.time()),
                        weights = attr(t1, "weights"),
                        n_rows = nrow(d)))
saveRDS(res, OUT_RDS)
message("Wrote ", OUT_RDS,
        "  (table1: ", nrow(t1), " rows; shares: ", nrow(t2), " rows)")
invisible(TRUE)
