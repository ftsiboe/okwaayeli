# exhibit_sheet_to_schema(): does the workbook adapter produce the same frame the
# engine does?
#
# This is the whole point of the adapter. The two existing parity suites each
# carry their own parse_coef() to bridge the workbook's vocabulary
# (CropIDx / Equ / Coef) to the engine's (crop / outcome / group / statistic /
# wave). Once the adapter is trusted, that duplication goes and a parity test is
# just a join.
#
# Those suites are deliberately left ALONE while this lands. They still pass or
# fail on their own terms, so if this file is wrong it shows up here rather than
# quietly redefining what ~15,000 assertions compare against.

GOLDEN_A <- testthat::test_path("golden", "re_means_extraction_any.csv")
GOLDEN_B <- testthat::test_path("golden", "resource_extraction_extraction.csv")

skip_unless <- function(g) {
  testthat::skip_if_not(file.exists(g), paste0("golden absent: ", basename(g)))
}

# The goldens are frozen post-read_exhibit_sheet(), so they already carry the
# canonical column names but not the layout attribute.
as_sheet <- function(path, layout) {
  d <- as.data.frame(data.table::fread(path))
  attr(d, "layout") <- layout
  d
}

test_that("means layout translates to the engine schema", {
  skip_unless(GOLDEN_A)
  s <- exhibit_sheet_to_schema(as_sheet(GOLDEN_A, "means"),
                               group_tag = "disagCat",
                               treatment = "extraction_any")

  testthat::expect_true(all(c("treatment", "crop", "outcome", "wave", "group",
                              "statistic", "estimate", "se", "t", "p",
                              "min", "max", "sd", "n") %in% names(s)))
  testthat::expect_setequal(unique(s$statistic),
                            c("mean", "trend_pct", "cat_diff", "trend_diff"))
  testthat::expect_setequal(stats::na.omit(unique(s$group)), c("pooled", "0", "1"))

  # The rows that carry no result must be gone.
  testthat::expect_false(any(grepl("miss|^r1$|^_$", s$outcome)))
})

test_that("model rows do not carry df and crit as sd and n", {
  skip_unless(GOLDEN_A)
  raw <- as_sheet(GOLDEN_A, "means")
  s   <- exhibit_sheet_to_schema(raw, group_tag = "disagCat")

  # In the sheet, Trend_* rows show N = 1.959964 -- that is r(table)'s critical
  # value, not a sample size. Mapping it to `n` would pass off a z-value as an n.
  trend_raw <- raw[grepl("^Trend_", raw$Coef) & !is.na(raw$N), ]
  testthat::expect_true(nrow(trend_raw) > 0)
  testthat::expect_true(all(abs(trend_raw$N - 1.959964) < 1e-4))

  model <- s[s$statistic %in% c("trend_pct", "cat_diff", "trend_diff"), ]
  testthat::expect_true(nrow(model) > 0)
  testthat::expect_true(all(is.na(model$n)))
  testthat::expect_true(all(is.na(model$sd)))

  # ... while the tabstat-derived mean rows keep theirs.
  means <- s[s$statistic == "mean" & s$crop == "Pooled" & s$outcome == "Yield", ]
  testthat::expect_true(any(!is.na(means$n)))
  testthat::expect_true(all(stats::na.omit(means$n) > 100))
})

test_that("study layout translates, and the trend flavor is explicit", {
  skip_unless(GOLDEN_B)
  raw <- as_sheet(GOLDEN_B, "study")

  s1 <- exhibit_sheet_to_schema(raw, trend_statistic = "trend_pct")
  testthat::expect_true("trend_pct" %in% s1$statistic)
  testthat::expect_setequal(stats::na.omit(unique(s1$wave)), c("pooled", "trend"))

  # The sheet cannot say which estimator produced its Trend row; the caller must.
  # RE fits logit on a continuous trend (percent/year); land fits logit on wave
  # dummies (percentage points). Same column, different quantity.
  s2 <- exhibit_sheet_to_schema(raw, trend_statistic = "change_pp")
  testthat::expect_true("change_pp" %in% s2$statistic)
  testthat::expect_false("trend_pct" %in% s2$statistic)
  testthat::expect_equal(s1$estimate, s2$estimate)
})

test_that("a wrong group_tag errors instead of degrading to pooled-only", {
  skip_unless(GOLDEN_A)
  # RE's sheets tag groups "disagCat", not the treatment name.
  #
  # This must ERROR, not return a smaller frame. "Pooled" resolves whatever the
  # tag is, so a mismatched tag drops every group row while keeping every pooled
  # one: 1,895 rows that look perfectly well-formed and are missing half the
  # content. Caught 2026-07-15 -- the first version of this returned quietly.
  testthat::expect_error(
    exhibit_sheet_to_schema(as_sheet(GOLDEN_A, "means"),
                            group_tag = "extraction_any"),
    "matches no group")

  # The message must name what the sheet actually uses, so the fix is obvious.
  testthat::expect_error(
    exhibit_sheet_to_schema(as_sheet(GOLDEN_A, "means"),
                            group_tag = "extraction_any"),
    "disagCat")

  # And the right tag still works.
  s <- exhibit_sheet_to_schema(as_sheet(GOLDEN_A, "means"), group_tag = "disagCat")
  testthat::expect_setequal(stats::na.omit(unique(s$group)), c("pooled", "0", "1"))
})

test_that("the adapter reproduces the engine, key for key", {
  skip_unless(GOLDEN_A)
  se_rds <- testthat::test_path("..", "..", "studies", "resource_extraction",
                                "data", "resource_extraction_study_environment.rds")
  testthat::skip_if_not(file.exists(se_rds), "resource_extraction environment absent")
  for (p in c("fixest", "marginaleffects")) testthat::skip_if_not_installed(p)

  d  <- readRDS(normalizePath(se_rds))$study_raw_data
  sp <- descriptive_specifications(d, outcomes = c("Yield", "Area"),
                                   treatments = "extraction_any", crops = "Pooled")
  eng <- draw_descriptive_summary(sp, d, study = "resource_extraction", quiet = TRUE)
  wb  <- exhibit_sheet_to_schema(as_sheet(GOLDEN_A, "means"),
                                 group_tag = "disagCat", treatment = "extraction_any")

  key <- c("crop", "outcome", "wave", "group", "statistic")
  m <- merge(eng[eng$outcome %in% c("Yield", "Area"), c(key, "estimate")],
             wb[, c(key, "estimate")], by = key, suffixes = c(".eng", ".wb"))
  testthat::expect_gt(nrow(m), 20L)

  # Means are pure aggregation; trends come from margins ... eydx.
  mm <- m[m$statistic == "mean", ]
  testthat::expect_equal(mm$estimate.eng, mm$estimate.wb, tolerance = 1e-6)
  tt <- m[m$statistic == "trend_pct", ]
  testthat::expect_equal(tt$estimate.eng, tt$estimate.wb, tolerance = 1e-3)
})
