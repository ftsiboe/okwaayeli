# Parity: R descriptive engine vs land_tenure's Stata workbook.
#
# Land exercises two things resource_extraction does not:
#   1. TWO FAMILIES IN ONE TABLE. 100_exhibits.do line 51 is a `reg` over 12
#      continuous outcomes; line 103 is a `logit` over Female, EqipMech, Credit,
#      Extension, EqipIrig. Both land in the same sheet with no record of which
#      model produced them.
#   2. THE wave_diff TREND FLAVOR. Land's Engine B is `logit <ind> i.Survey` with
#      nlcom (b[GLSS6] - b[GLSS7]) * 100 -- a percentage-POINT change. RE's is
#      `logit <ind> Trend` with margins ... eydx -- a percent-per-year
#      semi-elasticity. Validated here for the first time.
#
# GOLDEN CAVEAT -- read before re-freezing.
# Land's `means` sheet carries the `mat roweq A = Female` collision for
# Equ == "Female" ONLY: the OwnLnd0/OwnLnd1 prevalence rows were written under
# that tag, so Equ=="Female" has two Mean_OwnLnd0 rows (N=35,185 = the ownership
# share; N=13,099 = the real Female mean). Every other outcome is clean, and the
# `land_tenure` sheet is clean throughout -- the bug is in a block that only
# writes to `means`.
#
# The fix is in 100_exhibits.do (lines 181/199, `mat roweq A = \`Var'`) but the
# workbook of 2026-07-15 15:21 predates it: a Stata do-editor buffer runs the
# BUFFER, not the file on disk. Once it has been re-run from a fresh session
# (`do "studies/land_tenure/scripts/100_exhibits.do"`), Equ will gain OwnLnd0 /
# OwnLnd1, "Female" can come out of EXCLUDE_EQU below, and the golden should be
# re-frozen.
#
# Freeze:
#   XL <- "studies/land_tenure/output/land_tenure_results.xlsx"
#   data.table::fwrite(okwaayeli::read_exhibit_sheet(XL, "means"),
#                      "tests/testthat/golden/land_means.csv")
#   data.table::fwrite(okwaayeli::read_exhibit_sheet(XL, "land_tenure"),
#                      "tests/testthat/golden/land_tenure.csv")

GOLDEN_DIR <- testthat::test_path("golden")
GOLD_A <- file.path(GOLDEN_DIR, "land_means.csv")
GOLD_B <- file.path(GOLDEN_DIR, "land_tenure.csv")

SE_RDS <- local({
  p <- testthat::test_path("..", "..", "studies", "land_tenure",
                           "data", "land_tenure_study_environment.rds")
  if (file.exists(p)) normalizePath(p) else p
})

CONT <- c("Yield", "Area", "SeedKg", "HHLaborAE", "HirdHr", "FertKg",
          "PestLt", "AgeYr", "YerEdu", "HHSizeAE", "Depend", "CrpMix")
BIN  <- c("Female", "EqipMech", "Credit", "Extension", "EqipIrig")

# Contaminated by the roweq collision; remove once the workbook is re-run clean.
EXCLUDE_EQU <- "Female"

MIN_GROUP_N <- 30L

skip_unless <- function(g) {
  testthat::skip_if_not(file.exists(g), paste0("golden absent: ", basename(g)))
  testthat::skip_if_not(file.exists(SE_RDS), "land_tenure environment absent")
  for (p in c("fixest", "marginaleffects")) testthat::skip_if_not_installed(p)
}

.d <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- readRDS(SE_RDS)$study_raw_data
    cache
  }
})

parse_coef <- function(coef, tag = "OwnLnd") {
  grp <- function(s) if (s == "Pooled") "pooled"
                     else if (s == paste0(tag, "0")) "0"
                     else if (s == paste0(tag, "1")) "1" else NA_character_
  if (coef == "CATDif")   return(list(wave = "all", group = NA, statistic = "cat_diff"))
  if (coef == "TrendDif") return(list(wave = "all", group = NA, statistic = "trend_diff"))
  if (grepl("^Mean_", coef))
    return(list(wave = "all", group = grp(sub("^Mean_", "", coef)), statistic = "mean"))
  if (grepl("^Trend_", coef))
    return(list(wave = "all", group = grp(sub("^Trend_", "", coef)), statistic = "trend_pct"))
  if (grepl("^GLSS[0-9]_", coef))
    return(list(wave = sub("_.*$", "", coef),
                group = grp(sub("^GLSS[0-9]_", "", coef)), statistic = "mean"))
  NULL
}

test_that("Engine A reproduces land's means sheet, both families", {
  skip_unless(GOLD_A)
  gold <- data.table::fread(GOLD_A)
  d    <- .d()

  outcomes <- c(CONT, BIN)
  sp <- okwaayeli::descriptive_specifications(
    d, outcomes = outcomes, treatments = "OwnLnd", crops = "Pooled",
    families = c(stats::setNames(rep("gaussian", length(CONT)), CONT),
                 stats::setNames(rep("binomial",  length(BIN)),  BIN)))
  testthat::expect_true(all(sp$family[sp$outcome %in% BIN] == "binomial"))

  res <- okwaayeli::draw_descriptive_summary(sp, d, study = "land_tenure",
                                             quiet = TRUE)
  testthat::expect_true(!is.null(res))

  sizes <- local({
    m <- gold[grepl("^Mean_", gold$Coef) & gold$CropIDx == "Pooled", ]
    stats::setNames(as.numeric(m$N), paste(m$Equ, sub("^Mean_", "", m$Coef)))
  })

  n <- 0L
  for (i in seq_len(nrow(gold))) {
    if (gold$CropIDx[i] != "Pooled") next
    if (!gold$Equ[i] %in% outcomes) next
    if (gold$Equ[i] %in% EXCLUDE_EQU) next
    k <- parse_coef(gold$Coef[i])
    if (is.null(k) || is.na(k$group) || k$statistic != "mean") next

    r <- res[res$outcome == gold$Equ[i] & res$crop == "Pooled" &
             res$wave == k$wave & res$group == k$group & res$statistic == "mean", ]
    if (!nrow(r) || is.na(gold$Beta[i])) next
    lab <- paste("land", gold$Equ[i], gold$Coef[i])
    testthat::expect_equal(r$estimate[1], gold$Beta[i], tolerance = 1e-6, label = lab)
    if (!is.na(gold$N[i]))
      testthat::expect_equal(r$n[1], gold$N[i], tolerance = 1e-8,
                             label = paste(lab, "n"))
    n <- n + 1L
  }
  testthat::expect_gt(n, 0L)
})

test_that("Engine A reproduces land's trends", {
  skip_unless(GOLD_A)
  gold <- data.table::fread(GOLD_A)
  d    <- .d()
  outcomes <- c(CONT, BIN)
  sp <- okwaayeli::descriptive_specifications(
    d, outcomes = outcomes, treatments = "OwnLnd", crops = "Pooled",
    families = c(stats::setNames(rep("gaussian", length(CONT)), CONT),
                 stats::setNames(rep("binomial",  length(BIN)),  BIN)))
  res <- okwaayeli::draw_descriptive_summary(sp, d, study = "land_tenure",
                                             quiet = TRUE)
  n <- 0L
  for (i in seq_len(nrow(gold))) {
    if (gold$CropIDx[i] != "Pooled") next
    if (!gold$Equ[i] %in% outcomes || gold$Equ[i] %in% EXCLUDE_EQU) next
    k <- parse_coef(gold$Coef[i])
    if (is.null(k) || k$statistic != "trend_pct" || is.na(k$group)) next
    r <- res[res$outcome == gold$Equ[i] & res$crop == "Pooled" &
             res$group == k$group & res$statistic == "trend_pct", ]
    if (!nrow(r) || is.na(gold$Beta[i]) || gold$Beta[i] == 0) next
    testthat::expect_equal(r$estimate[1], gold$Beta[i], tolerance = 1e-3,
                           label = paste("land", gold$Equ[i], gold$Coef[i]))
    n <- n + 1L
  }
  testthat::expect_gt(n, 0L)
})

# ---- Engine B: the wave_diff flavor -----------------------------------------
# Land's land_tenure sheet is NOT touched by the roweq collision.
INDICATORS <- c("OwnLnd",
                paste0("LndOwn_", 1:3), paste0("LndRgt_", 1:4),
                paste0("LndAq_", 1:5), paste0("ShrCrpCat_", 1:3))

test_that("Engine B reproduces land's shares and wave_diff trend", {
  skip_unless(GOLD_B)
  gold <- data.table::fread(GOLD_B)
  d    <- .d()
  dc <- d[as.character(d$CropID) == "Pooled", , drop = FALSE]

  # The do-file expands the categoricals first: `for var LndOwn LndRgt LndAq
  # ShrCrpCat: tab X, gen(X_)`, and restricts to the waves that carry the module.
  dc <- dc[as.character(dc$Surveyx) %in% c("GLSS6", "GLSS7"), , drop = FALSE]
  for (v in c("LndOwn", "LndRgt", "LndAq", "ShrCrpCat"))
    if (v %in% names(dc)) dc <- okwaayeli::descriptive_expand_category(dc, v)
  d2 <- okwaayeli::descriptive_prepare(dc)

  res <- okwaayeli::descriptive_indicator_shares(
    d2, intersect(INDICATORS, names(d2)),
    trend = "wave_diff", waves = c("GLSS6", "GLSS7"), per_wave = TRUE)
  testthat::expect_true(!is.null(res))

  n <- 0L
  for (i in seq_len(nrow(gold))) {
    if (gold$crop[i] != "Pooled" || !gold$Variable[i] %in% INDICATORS) next
    lab <- paste("land B", gold$Variable[i], gold$mesure[i])
    if (gold$mesure[i] %in% c("GLSS6", "GLSS7")) {
      r <- res[res$outcome == gold$Variable[i] & res$wave == gold$mesure[i], ]
      if (!nrow(r) || is.na(gold$Beta[i])) next
      testthat::expect_equal(r$estimate[1], gold$Beta[i], tolerance = 1e-6, label = lab)
      n <- n + 1L
    } else if (gold$mesure[i] == "Trend") {
      # nlcom (b[GLSS6] - b[GLSS7]) * 100: percentage POINTS, earlier minus
      # later, so a category that grew reports negative. Land's Table 2 shows
      # "Not owned = -9.743" while the share rose 0.290 -> 0.388.
      r <- res[res$outcome == gold$Variable[i] & res$wave == "trend", ]
      if (!nrow(r) || is.na(gold$Beta[i]) || gold$Beta[i] == 0) next
      testthat::expect_equal(r$estimate[1], gold$Beta[i], tolerance = 1e-2, label = lab)
      n <- n + 1L
    }
  }
  testthat::expect_gt(n, 0L)
})
