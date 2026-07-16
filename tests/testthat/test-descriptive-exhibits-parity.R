# Parity: R descriptive engine vs the Stata/Excel path it replaces.
#
# Reference: studies/resource_extraction/output/resource_extraction_results.xlsx
#
# resource_extraction is the golden because it is the ONLY study whose Engine A
# sheets were never affected by the `mat roweq A = Female` bug (fixed repo-wide
# 2026-07-15). Land's workbook predates that fix and must NOT be used as a
# reference until its 100_exhibits.do has been re-run.
#
# Sheet layout (NOT the same across studies -- each do-file names sheets from its
# own treatment loop):
#   Engine A   Means_<treatment>, one sheet per treatment (7 for RE).
#              Land has a single "means" only because it has one treatment.
#   Engine B   "extraction"  (land calls its equivalent "land_tenure")
#
# Freeze the goldens (Phase 0) with tests/testthat/golden/_freeze.R
#
# Coverage:
#   default          7 treatments x crop "Pooled"          (~10 min)
#   OKWAAYELI_SWEEP  + the crop dimension                  (~1 h)
# The crop sweep is opt-in because the counterfactual grid doubles the data for
# every avg_slopes() call; 7 x 28 x 12 models is over an hour and does not belong
# in a default run.

GOLDEN_DIR <- testthat::test_path("golden")

# testthat runs from tests/testthat/, so a repo-relative path is not found and
# every test silently SKIPs (seen 2026-07-15: "5 skipped, 0 passed"). Resolve
# against the package root instead.
SE_RDS <- local({
  p <- testthat::test_path("..", "..", "studies", "resource_extraction",
                           "data", "resource_extraction_study_environment.rds")
  if (file.exists(p)) normalizePath(p) else p
})

TREATMENTS <- c("extraction_any", "mining_any", "mining_comm", "mining_gala",
                "quarrying", "sand", "salt")

OUTCOMES <- c("Yield", "Area", "SeedKg", "HHLaborAE", "HirdHr", "FertKg",
              "PestLt", "AgeYr", "YerEdu", "HHSizeAE", "Depend", "CrpMix")

# The sheet's group suffix is the LITERAL "disagCat", not the treatment name --
# every do-file does `gen disagCat = `disag'` and names its matrix rows from that.
GROUP_TAG <- "disagCat"

FULL_SWEEP <- nzchar(Sys.getenv("OKWAAYELI_SWEEP"))

# Minimum observations per treatment group before a TREND comparison is scored.
#
# Below this, `reg y c.Trend##i.disagCat` has almost no within-crop variation to
# work with and `margins ... eydx` divides by fitted values near zero. Both
# implementations then return noise, and they do not return the SAME noise. The
# 2026-07-15 crop sweep produced, in the Stata golden:
#
#   Pineapple Depend Trend_disagCat1   -669,487,338,753,097,984  (%/yr)
#   Pineapple FertKg Trend_disagCat1   -153,054,440,947,974,016
#   Onion     PestLt Trend_disagCat1      8,109,369,589,760
#
# Those are collapsed fits written to the sheet as estimates. R's values for the
# same cells (206, -851, -810bn) are equally meaningless.
#
# The rule is deliberately on GROUP SIZE -- a property of the data -- and not on
# the magnitude of either result. Skipping wherever the two disagree would make
# the harness self-fulfilling. Means, SDs and Ns are still compared at every n:
# only the trend is gated, because only the trend is unestimable here.
#
# MUST match descriptive_trend_model()'s `min_group_n` default. The engine now
# refuses these fits at source and returns NULL, so this gate is belt-and-braces:
# it keeps the harness honest if the engine's default is ever loosened, and it
# stops the golden's own junk (which remains in RE's workbook) from being scored
# as truth either way.
MIN_GROUP_N <- 30L
testthat::expect_identical(
  formals(okwaayeli::descriptive_trend_model)$min_group_n, MIN_GROUP_N)

# Group sizes per (crop, group) read from the golden's Mean_* rows, so the gate
# uses the same sample the reference used.
group_sizes <- function(gold, tag = GROUP_TAG) {
  m <- gold[grepl("^Mean_", gold$Coef), ]
  key <- paste(m$CropIDx, sub("^Mean_", "", m$Coef))
  stats::setNames(as.numeric(m$N), key)
}

trend_estimable <- function(sizes, crop, tag = GROUP_TAG) {
  n0 <- sizes[[paste(crop, paste0(tag, "0"))]]
  n1 <- sizes[[paste(crop, paste0(tag, "1"))]]
  !is.null(n0) && !is.null(n1) && !is.na(n0) && !is.na(n1) &&
    n0 >= MIN_GROUP_N && n1 >= MIN_GROUP_N
}

.se <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- readRDS(SE_RDS)$study_raw_data
    cache
  }
})

skip_unless_data <- function(golden) {
  testthat::skip_if_not(file.exists(golden), paste0("golden absent: ", basename(golden)))
  testthat::skip_if_not(file.exists(SE_RDS), "resource_extraction environment absent")
  for (p in c("fixest", "marginaleffects")) testthat::skip_if_not_installed(p)
}

# Map the sheet's Coef vocabulary onto the engine's (wave, group, statistic).
parse_coef <- function(coef, tag = GROUP_TAG) {
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

# Walk one treatment's golden sheet and check every cell the engine emits.
# Returns the number of comparisons made, so a silently-empty sweep fails.
check_engine_a <- function(treatment, crops) {
  g <- file.path(GOLDEN_DIR, paste0("re_means_", treatment, ".csv"))
  skip_unless_data(g)
  gold <- data.table::fread(g)
  d    <- .se()

  sp  <- okwaayeli::descriptive_specifications(
           d, outcomes = OUTCOMES, treatments = treatment, crops = crops)
  res <- okwaayeli::draw_descriptive_summary(sp, d, study = "resource_extraction",
                                             quiet = TRUE)
  testthat::expect_true(!is.null(res),
    info = paste0(treatment, ": engine returned nothing"))

  sizes <- group_sizes(gold)
  n <- 0L
  skipped <- character(0)
  for (i in seq_len(nrow(gold))) {
    k <- parse_coef(gold$Coef[i])
    if (is.null(k)) next
    if (!gold$CropIDx[i] %in% crops || !gold$Equ[i] %in% OUTCOMES) next

    r <- res[res$outcome == gold$Equ[i] & res$crop == gold$CropIDx[i] &
             res$statistic == k$statistic &
             (is.na(k$group) | (!is.na(res$group) & res$group == k$group)) &
             (k$statistic %in% c("cat_diff","trend_diff") | res$wave == k$wave), ]
    if (!nrow(r)) next
    lab <- paste(treatment, gold$CropIDx[i], gold$Equ[i], gold$Coef[i])

    if (k$statistic == "mean" && !is.na(k$group)) {
      # Pure aggregation: should be near-exact.
      if (!is.na(gold$Beta[i])) {
        testthat::expect_equal(r$estimate[1], gold$Beta[i], tolerance = 1e-6, label = lab)
        n <- n + 1L
      }
      if (!is.na(gold$SD[i]))
        testthat::expect_equal(r$sd[1], gold$SD[i], tolerance = 1e-6,
                               label = paste(lab, "sd"))
      if (!is.na(gold$N[i]))
        testthat::expect_equal(r$n[1], gold$N[i], tolerance = 1e-8,
                               label = paste(lab, "n"))
    } else if (k$statistic == "trend_pct" && !is.na(k$group)) {
      # Gate on group size, not on the result -- see MIN_GROUP_N.
      if (!trend_estimable(sizes, gold$CropIDx[i])) {
        skipped <- union(skipped, gold$CropIDx[i])
        next
      }
      # margins ... eydx: looser. A failure here means the estimand is wrong
      # (see the counterfactual note in descriptive_trend_model), NOT that the
      # tolerance should be widened.
      if (!is.na(gold$Beta[i])) {
        testthat::expect_equal(r$estimate[1], gold$Beta[i], tolerance = 1e-3, label = lab)
        n <- n + 1L
      }
    } else if (k$statistic %in% c("cat_diff", "trend_diff")) {
      # Stata reports testparm's F in Tv; fixest::wald() computes its own
      # statistic. Compare p, which is the quantity the exhibits use.
      if (!is.na(gold$Pv[i])) {
        testthat::expect_equal(r$p[1], gold$Pv[i], tolerance = 1e-3, label = lab)
        n <- n + 1L
      }
    }
  }
  # Say which crops went unscored, so the gate stays visible rather than becoming
  # a silent hole in the coverage.
  if (length(skipped))
    message(sprintf("[%s] trend not scored (group n < %d): %s",
                    treatment, MIN_GROUP_N, paste(sort(skipped), collapse = ", ")))
  n
}

# ---- Engine A: every treatment, pooled --------------------------------------
for (tr in TREATMENTS) {
  local({
    treatment <- tr
    test_that(paste0("Engine A reproduces Means_", treatment, " (Pooled)"), {
      n <- check_engine_a(treatment, "Pooled")
      testthat::expect_gt(n, 0L)
    })
  })
}

# ---- Engine A: the crop dimension (opt-in) ----------------------------------
# Set OKWAAYELI_SWEEP=1 to run. This is where descriptive_prepare()'s per-crop
# trend origin matters: Stata recomputes `sum Season; gen Trend = Season - r(min)`
# AFTER `keep if CropIDx == "<crop>"`, so a crop absent from the first wave has a
# different origin than the pooled sample. Faithful, and untested until this runs.
test_that("Engine A reproduces the crop dimension", {
  testthat::skip_if_not(FULL_SWEEP, "set OKWAAYELI_SWEEP=1 for the crop sweep")
  g <- file.path(GOLDEN_DIR, "re_means_extraction_any.csv")
  skip_unless_data(g)
  crops <- setdiff(unique(data.table::fread(g)$CropIDx), c("Pooled", "_", NA))
  n <- check_engine_a("extraction_any", crops)
  testthat::expect_gt(n, 0L)
})

# ---- Engine B: the "extraction" sheet ---------------------------------------
# RE's Engine B is NOT land's. It fits `logit <ind> Trend` and reports
# margins ... eydx(Trend) * 100 -- a percent-per-year semi-elasticity -- with the
# pooled share only (mesure GLSS0 / Trend). Land fits `logit <ind> i.Survey` and
# reports a percentage-point wave difference with per-wave shares.
# See descriptive_indicator_shares(), "Two trend flavors".
GOLDEN_B <- file.path(GOLDEN_DIR, "resource_extraction_extraction.csv")
INDICATORS <- TREATMENTS

check_engine_b <- function(crops) {
  skip_unless_data(GOLDEN_B)
  gold <- data.table::fread(GOLDEN_B)
  d    <- .se()
  n <- 0L
  for (cr in crops) {
    dc <- d[as.character(d$CropID) == cr, , drop = FALSE]
    if (!nrow(dc)) next
    d2  <- okwaayeli::descriptive_prepare(dc)
    res <- okwaayeli::descriptive_indicator_shares(
             d2, INDICATORS, trend = "continuous", per_wave = FALSE)
    if (is.null(res)) next
    gc <- gold[gold$crop == cr, ]
    for (i in seq_len(nrow(gc))) {
      if (!gc$Variable[i] %in% INDICATORS) next
      lab <- paste("B", cr, gc$Variable[i], gc$mesure[i])
      if (gc$mesure[i] == "GLSS0") {
        r <- res[res$outcome == gc$Variable[i] & res$wave == "pooled", ]
        if (!nrow(r) || is.na(gc$Beta[i])) next
        testthat::expect_equal(r$estimate[1], gc$Beta[i], tolerance = 1e-6, label = lab)
        if (!is.na(gc$N[i]))
          testthat::expect_equal(r$n[1], gc$N[i], tolerance = 1e-8,
                                 label = paste(lab, "n"))
        n <- n + 1L
      } else if (gc$mesure[i] == "Trend") {
        r <- res[res$outcome == gc$Variable[i] & res$wave == "trend", ]
        # Stata blanks a non-converging logit (`if r(r)==1 mat A = J(1,8,.)`,
        # inside cap{}). Where the golden is NA there is nothing to match, and R
        # returning a value there is not a parity failure.
        if (!nrow(r) || is.na(gc$Beta[i])) next
        # An EXACT zero is not an estimate. Stata's cap{} swallowed the failed
        # fit and left the cell at 0 -- seen for salt in Tomatoe, mining_comm in
        # Sorghum, and ~25 other rare-indicator/small-crop cells on 2026-07-15.
        # R now refuses those fits (descriptive_trend_model min_events), so it
        # emits nothing. Scoring 0 as truth would enshrine the silent failure.
        if (gc$Beta[i] == 0) next
        testthat::expect_equal(r$estimate[1], gc$Beta[i], tolerance = 1e-3, label = lab)
        n <- n + 1L
      }
    }
  }
  n
}

test_that("Engine B reproduces the extraction sheet (Pooled)", {
  n <- check_engine_b("Pooled")
  testthat::expect_gt(n, 0L)
})

test_that("Engine B reproduces the extraction sheet (crops)", {
  testthat::skip_if_not(FULL_SWEEP, "set OKWAAYELI_SWEEP=1 for the crop sweep")
  skip_unless_data(GOLDEN_B)
  crops <- setdiff(unique(data.table::fread(GOLDEN_B)$crop), c("Pooled", "_", NA))
  n <- check_engine_b(crops)
  testthat::expect_gt(n, 0L)
})

# ---- Schema ------------------------------------------------------------------
test_that("the emitted frame is keyed, not positional", {
  skip_unless_data(file.path(GOLDEN_DIR, "re_means_extraction_any.csv"))
  d   <- .se()
  sp  <- okwaayeli::descriptive_specifications(
           d, outcomes = OUTCOMES[1:2], treatments = "extraction_any", crops = "Pooled")
  res <- okwaayeli::draw_descriptive_summary(sp, d, study = "resource_extraction",
                                             quiet = TRUE)
  key <- res[, c("treatment", "crop", "outcome", "wave", "group", "statistic")]
  testthat::expect_false(any(duplicated(key)),
    info = "duplicate keys would reintroduce the roweq-collision failure mode")
  testthat::expect_identical(attr(res, "weights"), "none")
})
