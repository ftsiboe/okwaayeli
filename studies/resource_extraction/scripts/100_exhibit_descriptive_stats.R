# 100_exhibit_descriptive_stats.R
# Compute the RESOURCE EXTRACTION descriptive exhibits from study_raw_data and
# cache them to data/descriptive_exhibits.rds, which the table builders read at
# knit time.  This is the engine-backed replacement for the frozen
# data/tables/*.csv files (table1, table2, tableA2, tableA3).
#
# WHY A SEPARATE STEP: the engine fits a model per (treatment x crop x outcome).
# With 7 treatments x ~17 crops x 17 outcomes that is thousands of fits and far
# too slow to run inside every knit -- hence compute-once-and-cache.
#
# The engine is R/descriptive-exhibits-core.R, covered by ~15,000 assertions
# across two studies. See tests/testthat/test-descriptive-exhibits-*.R.
#
# Mirrors studies/land_tenure/scripts/100_exhibit_descriptive_stats.R. Two
# resource-extraction-specific differences in the Table 1 outcome set:
#   * OwnLnd  ("Land owned (dummy)") is INCLUDED as a binomial outcome
#   * Extension is EXCLUDED here (it appears only in the analytical Table A9)
#
# Run from the okwaayeli repo root.

tryCatch({ rm(list = ls()[!(ls() %in% c(Keep.List))]); gc() }, error = function(e) {
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

STUDY   <- "studies/resource_extraction"
SE_RDS  <- file.path(STUDY, "data", "resource_extraction_study_environment.rds")
OUT_RDS <- file.path(STUDY, "data", "descriptive_exhibits.rds")

stopifnot(file.exists(SE_RDS))
d <- readRDS(SE_RDS)$study_raw_data
message("study_raw_data: ", nrow(d), " rows, ", ncol(d), " cols")

# ---- Fail loud if the data does not carry the columns the exhibits key on ----
# A silently-missing column would drop rows/columns from a table and read as a
# parity difference downstream; surface it here instead.
CONT <- c("Yield", "Area", "SeedKg", "HHLaborAE", "HirdHr", "FertKg", "PestLt",
          "AgeYr", "YerEdu", "HHSizeAE", "Depend", "CrpMix")
BIN  <- c("Female", "EqipMech", "EqipIrig", "Credit", "OwnLnd")   # NB OwnLnd in, Extension out

# The engine's own worked example (R/descriptive-exhibits-core.R, ~line 705) and
# tableA2/tableA3 headers fix these seven, in this order.
TREATMENTS <- c("extraction_any", "mining_any", "mining_comm", "mining_gala",
                "quarrying", "sand", "salt")

need <- c(CONT, BIN, TREATMENTS, "CropID", "Surveyx")
miss <- setdiff(need, names(d))
if (length(miss))
  stop("study_raw_data is missing expected columns: ", paste(miss, collapse = ", "),
       call. = FALSE)

# ---- Engine A: Table 1 (extraction_any) + Tables A2 / A3 (six mining subtypes)-
# One draw over all seven treatments produces every cell of all three tables:
#   Table 1  = treatment "extraction_any"  -> pooled / group 0 / group 1
#   Table A2 = pooled (= extraction_any pooled) + group 1 of the other six, means
#   Table A3 = same layout, trend_pct
#
# Crops carrying a Table 1 crop-production row, plus the "Pooled" pseudo-crop.
T1_CROPS <- c("Pooled", "Maize", "Rice", "Millet", "Sorghum", "Beans", "Peanut",
              "Cassava", "Yam", "Cocoyam", "Plantain", "Pepper", "Okra",
              "Tomatoe", "Cocoa", "Palm")

have_crops <- intersect(T1_CROPS, unique(as.character(d$CropID)))
dropped <- setdiff(T1_CROPS, have_crops)
if (length(dropped))
  message("NOTE: Table 1 crops not present as CropID levels (dropped): ",
          paste(dropped, collapse = ", "))

spec <- descriptive_specifications(
  d,
  outcomes   = c(CONT, BIN),
  treatments = TREATMENTS,
  crops      = have_crops,
  families   = c(stats::setNames(rep("gaussian", length(CONT)), CONT),
                 stats::setNames(rep("binomial",  length(BIN)),  BIN)))

message("Engine A: ", nrow(spec), " specifications ...")
t1 <- draw_descriptive_summary(spec, d, study = "resource_extraction")

# ---- Engine B: Table 2 (seven activity indicators, all crops, % change/yr) ---
# resource_extraction's do-file collects only StatTotal (the pooled GLSS0 share)
# and a CONTINUOUS trend (semi-elasticity, percent change per year), so
# per_wave = FALSE and trend = "continuous". This is the substantive difference
# from land_tenure, which uses trend = "wave_diff" with per-wave shares.
IND <- intersect(TREATMENTS, names(d))
crops_b <- unique(as.character(d$CropID))
message("Engine B: ", length(IND), " indicators x ", length(crops_b), " crops ...")

t2 <- do.call(rbind, lapply(crops_b, function(cr) {
  dc <- d[as.character(d$CropID) == cr, , drop = FALSE]
  if (!nrow(dc)) return(NULL)
  r <- try(descriptive_indicator_shares(
    descriptive_prepare(dc), IND,
    trend = "continuous", per_wave = FALSE),
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
                        treatments = TREATMENTS,
                        n_rows = nrow(d)))
saveRDS(res, OUT_RDS)
message("Wrote ", OUT_RDS,
        "  (table1: ", nrow(t1), " rows; shares: ",
        if (is.null(t2)) 0 else nrow(t2), " rows)")
invisible(TRUE)
