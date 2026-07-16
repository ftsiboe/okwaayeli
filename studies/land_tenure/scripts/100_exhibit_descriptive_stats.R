# 100_exhibit_descriptive_stats.R
# Compute the descriptive exhibits (Tables 1, 2 and S1-S4) from the analysis data
# and cache them. Replaces the Stata half of 100_exhibits.do and the
# land_tenure_results.xlsx round trip.
#
# WHY A SEPARATE STEP. The engine fits a model per (treatment x crop x outcome):
# ~250s for the pooled sample alone, longer with crops. That cannot run inside
# every knit, so this mirrors what 100_exhibits.do did -- compute when the data
# changes, cache, and let 110_exhibit_tables.R read the cache.
#
#   001 -> 002            build study_raw_data / estimation_data
#   304 (this file)       -> data/descriptive_exhibits.rds
#   run_article.R         300 -> 301 -> 302, reads the cache
#
# The engine is validated against BOTH studies' Stata workbooks:
# resource_extraction (14,405 assertions; 7 treatments x 28 crops, both engines)
# and land_tenure (678; two families in one table, the wave_diff trend flavor).
# See tests/testthat/test-descriptive-exhibits-*.R.
#
# Run from the repo root:  Rscript studies/land_tenure/scripts/100_exhibit_descriptive_stats.R

if (!requireNamespace("okwaayeli", quietly = TRUE)) devtools::load_all(".")
suppressPackageStartupMessages(library(okwaayeli))

STUDY   <- "studies/land_tenure"
SE_RDS  <- file.path(STUDY, "data", "land_tenure_study_environment.rds")
OUT_RDS <- file.path(STUDY, "data", "descriptive_exhibits.rds")

stopifnot(file.exists(SE_RDS))
d <- readRDS(SE_RDS)$study_raw_data
message("study_raw_data: ", nrow(d), " rows")

# ---- Table 1 -----------------------------------------------------------------
# Two models, one table. 100_exhibits.do line 51 is `reg` over the continuous
# outcomes; line 103 is `logit` over the binary ones. The sheet recorded no
# distinction between them; the spec grid does.
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
# The tenure detail modules are only administered in GLSS6/GLSS7, and
# 100_exhibits.do restricts to them before expanding the categoricals.
# Land's trend is the wave_diff flavor: logit on i.Survey, then
# nlcom (b[GLSS6] - b[GLSS7]) * 100 -- percentage POINTS, earlier minus later.
dt <- d[as.character(d$Surveyx) %in% c("GLSS6", "GLSS7"), , drop = FALSE]
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
    trend = "wave_diff", waves = c("GLSS6", "GLSS7"), per_wave = TRUE),
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
