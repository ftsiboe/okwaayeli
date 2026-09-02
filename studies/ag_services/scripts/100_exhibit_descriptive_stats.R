# 100_exhibit_descriptive_stats.R  (1## = exhibits; see scripts/README.md)
# Compute the study's descriptive exhibits from study_raw_data and cache them to
# data/descriptive_exhibits.rds, which exhibit_helpers_tables.R reads at knit
# time.
#
# Replaces the retired _to_delete/100_exhibits.do. Same two engines, same
# estimators, no Stata. The .do's two output sheets map onto the two objects
# cached here:
#
#   .do sheet `Means_<disag>`  ->  res$table1   (Engine A: means + trends by group)
#   .do sheet `services`       ->  res$shares   (Engine B: indicator shares + trend)
#
# and those two feed four published exhibits:
#
#   draft Table 1  <- Engine A, treatment services0            (ref sumstat::Table1)
#   draft Table 4  <- Engine B                                 (ref sumstat::Table2-services)
#   draft Table S3 <- Engine A, treatments services3/coop/assoc (ref sumstat::TableS2)
#   draft Table S4 <- Engine A, the same, trend rows            (ref sumstat::TableS3)
#
# WHY A SEPARATE STEP: Engine A fits a model per (treatment x crop x outcome).
# The grid below is ~1,600 fits; far too slow to run inside every knit, hence
# compute-once-and-cache. Engine B adds one logit per (indicator x crop).
#
# The engine is R/descriptive-exhibits-core.R, covered by ~15,000 assertions
# across two studies. See tests/testthat/test-descriptive-exhibits-*.R.
#
# Run from the repo root.

tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

project_name <- "ag_services"
STUDY   <- file.path("studies", project_name)
SE_RDS  <- file.path(STUDY, "data", paste0(project_name, "_study_environment.rds"))
OUT_RDS <- file.path(STUDY, "data", "descriptive_exhibits.rds")

if (!file.exists(SE_RDS))
  stop("100: no study environment at ", SE_RDS,
       ".\n  Run the DATA + MATCHING stages of run_article.R first.", call. = FALSE)

study_environment <- readRDS(SE_RDS)

# wd is a frozen snapshot -- recompute it. See ?study_dirs and the 2026-08-07
# migration note in run_article.R.
study_environment <- study_dirs(study_environment, layout = "v2")

d <- study_environment$study_raw_data
if (is.null(d) || !nrow(d))
  stop("100: the study environment carries no study_raw_data. 001 writes it; ",
       "check that DATA ran and that MATCHING re-attached estimation_data.",
       call. = FALSE)
message("study_raw_data: ", nrow(d), " rows x ", ncol(d), " cols; waves ",
        paste(sort(unique(as.character(d$Surveyx))), collapse = "/"))

# ---- Weights ------------------------------------------------------------------
# UNWEIGHTED, deliberately. The .do's `tabstat ... , stat(mean sem min max sd n)`
# and its `reg`/`logit` calls carry no weight, so the frozen reference is an
# unweighted sample. Passing WeightHH here would produce defensible numbers that
# do not reconcile with a single cell of
# narrative/diagnostics/verification_reference_2026-08-07.json.
# Change this only together with a re-verification, and say so in the paper.
WEIGHTS <- NULL

# =============================================================================
#  DERIVED COLUMNS -- the .do's `gen` and `tab ..., gen()` lines
# =============================================================================
# .do lines 8-10:
#   gen extension0 = extension > 2
#   tab ag_services,          gen(ag_services)     -> ag_services1..8
#   tab extension_compliance, gen(compliance)      -> compliance1..4
#   tab extension,            gen(extensionCat)    -> extensionCat1..7
#
# descriptive_expand_category() is the R spelling of `tab, gen()`. It names the
# dummies <var>_1, <var>_2, ... in LEVEL order, one-based -- so Stata's
# `ag_services1` is `ag_services_1` here, and the two indices agree as long as
# the level set is the contiguous 0..k the data carries. NA stays NA in every
# dummy, as in Stata; coding it 0 would deflate the shares under item
# nonresponse.
#
# Expansion happens ONCE, on the whole frame, before any crop split. Expanding
# inside the crop loop would let a crop that never observes a level produce a
# different dummy numbering from its neighbours, and Table 4's columns would
# then mean different things in different rows.

# `extension` is 0..7 in the current release but its value labels cover 0..6
# only, so `tab extension, gen()` now yields EIGHT dummies where the .do got
# seven. extensionCat2..7 (the ones draft Table 4 prints) are levels 1..6 either
# way, so the published columns are unaffected -- but the unlabelled level 7 is
# a finding, not a rounding detail. Reported below rather than silently folded.
.ext_lvls <- sort(unique(as.numeric(as.character(d$extension))))
.ext_lbl  <- attr(d$extension, "labels")
if (length(.ext_lvls) > length(.ext_lbl))
  message("100: FINDING -- `extension` carries ", length(.ext_lvls),
          " levels (", paste(.ext_lvls, collapse = ","), ") but only ",
          length(.ext_lbl), " value labels (", paste(.ext_lbl, collapse = ","),
          ").\n     The .do's extensionCat1..7 becomes extension_1..",
          length(.ext_lvls), " here. Levels 1-6 (draft Table 4's ",
          "extensionCat2..7) are unaffected.")

d$extension0 <- as.numeric(as.numeric(as.character(d$extension)) > 2)

# Capture each expansion's dummy names as it happens. attr(d, "indicators") is
# overwritten by every call, so reading it once at the end would silently give
# the last variable's dummies to all three.
.DUMMIES <- list()
for (v in c("ag_services", "extension_compliance", "extension")) {
  d <- descriptive_expand_category(d, v)
  .DUMMIES[[v]] <- attr(d, "indicators")
  message("100: ", v, " -> ", length(.DUMMIES[[v]]), " dummies (",
          paste(range(.DUMMIES[[v]]), collapse = " ... "), ")")
}

# =============================================================================
#  ENGINE A -- means, group differences and trends  (.do sheet `Means_<disag>`)
# =============================================================================
# Two models, one table: OLS over the continuous outcomes, logit over the binary
# ones. `families` is what carries that distinction into the spec grid.
#
# Outcome list is the .do's, verbatim: line 48 (continuous, `reg`) and line 100
# (binary, `logit`).
CONT <- c("Yield", "Area", "SeedKg", "HHLaborAE", "HirdHr", "FertKg", "PestLt",
          "AgeYr", "YerEdu", "HHSizeAE", "Depend", "CrpMix",
          "extension_distance", "community_tractors")
BIN  <- c("Female", "EqipMech", "Credit", "OwnLnd", "EqipIrig", "Extension")

# ---- Treatments ---------------------------------------------------------------
# The .do runs four: services0, farm_association, community_cooperative,
# extension0.
#
# FINDING, 2026-08-13. Draft Table S3's four columns are Pooled (n=22,519),
# Extension (n=11,752), Community cooperative (n=3,690), Farm association
# (n=5,713). Three of those reconcile exactly against study_raw_data. The
# Extension one does NOT come from the .do's `extension0`:
#
#     extension0 = extension > 2   ->  n = 12,004   (not published anywhere)
#     services3                    ->  n = 11,752   <- this is the column
#
# services3 is identical to extension_officer_visit on the pooled sample.
# So the .do computes a treatment the paper does not print, and the paper prints
# a treatment the .do does not compute. Both are carried here: services3 because
# an exhibit needs it, extension0 because the .do specified it and dropping it
# would make this script unverifiable against the retired sheet. Neither is
# silently substituted for the other.
#
# NOTE FOR WHOEVER WRITES ft_tableS3(). services1/2/3 are coded 0 = no services
# at all, 1 = this source, NA = some other source -- the two-group frontier
# coding, not a plain dummy. descriptive_group_summary() drops rows with a
# missing treatment BEFORE it computes anything, so the pooled row of the
# services3 grid sits on n = 20,056 (8,304 + 11,752), not on 22,519.
# Draft Table S3's "Pooled (n=22,519)" column must therefore be read from the
# services0 grid. Taking it from services3's would print a different sample
# under the same header, and every cell would still look reasonable.
TREATMENTS <- c("services0",              # draft Table 1        (n 8,304 / 14,215)
                "services3",              # draft Table S3/S4 "Extension"   (11,752)
                "community_cooperative",  # draft Table S3/S4               ( 3,690)
                "farm_association",       # draft Table S3/S4               ( 5,713)
                "extension0")             # .do only; no published exhibit  (12,004)

# ---- Crops --------------------------------------------------------------------
# The .do loops every crop level (28). The published exhibits print sixteen: the
# pooled row plus the crop production block. Restricting the grid cuts Engine A
# from ~2,800 fits to ~1,600.
#
# This is a real cap, so it is logged rather than assumed. Set ALL_CROPS = TRUE
# to reproduce the .do's full sheet.
ALL_CROPS <- FALSE

T1_CROPS <- c("Pooled", "Maize", "Rice", "Millet", "Sorghum", "Beans", "Peanut",
              "Cassava", "Yam", "Cocoyam", "Plantain", "Pepper", "Okra",
              "Tomatoe", "Cocoa", "Palm")

crops_all <- sort(unique(as.character(d$CropID)))
crops_A   <- if (isTRUE(ALL_CROPS)) crops_all else intersect(T1_CROPS, crops_all)

.missing <- setdiff(T1_CROPS, crops_all)
if (length(.missing))
  stop("100: crops named by the published tables are absent from study_raw_data: ",
       paste(.missing, collapse = ", "),
       "\n  That is a data problem, not a table problem -- do not proceed by ",
       "dropping them.", call. = FALSE)
if (!isTRUE(ALL_CROPS))
  message("100: Engine A covers ", length(crops_A), " crops; ",
          length(setdiff(crops_all, crops_A)), " excluded (",
          paste(setdiff(crops_all, crops_A), collapse = ", "),
          "). Set ALL_CROPS = TRUE to reproduce the .do's full sheet.")

spec <- descriptive_specifications(
  d,
  outcomes   = c(CONT, BIN),
  treatments = TREATMENTS,
  crops      = crops_A,
  families   = c(stats::setNames(rep("gaussian", length(CONT)), CONT),
                 stats::setNames(rep("binomial",  length(BIN)),  BIN)))

message("Engine A: ", nrow(spec), " specifications (",
        length(TREATMENTS), " treatments x ", length(crops_A), " crops x ",
        length(c(CONT, BIN)), " outcomes) ... this is the slow part.")
t1 <- draw_descriptive_summary(spec, d, study = project_name, weights = WEIGHTS,
                               quiet = TRUE)
if (is.null(t1) || !nrow(t1))
  stop("100: Engine A returned nothing. Check that the outcome columns exist ",
       "and that CropID carries the crops named above.", call. = FALSE)

# =============================================================================
#  ENGINE B -- indicator shares and trends  (.do sheet `services`)
# =============================================================================
# TREND FLAVOR: "continuous". The .do (lines 262-265) is
#     logit <ind> Trend ; margins, eydx(Trend) grand post predict(pr) ;
#     nlcom (_b[Trend]*100)
# i.e. a semi-elasticity -- PERCENT change in the probability per year -- and it
# collects only `StatTotal`, so per_wave = FALSE. land_tenure's "wave_diff"
# flavor emits percentage POINTS between two waves; the two are not
# interchangeable and `trend` therefore has no default. See
# ?descriptive_indicator_shares.
#
# WAVE WINDOW: all three waves, GLSS5-GLSS7, exactly as the .do. Verified
# 2026-08-13 against sumstat::Table2-services:
#     Millet services_planting     all waves 0.515 (0.500)  <- reference
#                                  GLSS6+7   0.478 (0.500)
#     Millet services_agchemicals  all waves 0.439 (0.496)  <- reference
#                                  GLSS6+7   0.457 (0.498)
#
# FINDING: draft Table 4 is titled "... in Ghana (2012-2017)", but its numbers
# span 2005-2017. Either the title or the window is wrong. The pipeline
# reproduces the printed numbers, so the window stays; the title is the thing to
# fix. Same question applies to draft Tables 2, 3 and 5, which carry the same
# date range and read off the same sheet.
IND <- c(
  # the four treatments, as shares
  "services0", "farm_association", "community_cooperative", "extension0",
  # ag_services 0..7  (Stata: ag_services1..8)
  .DUMMIES[["ag_services"]],
  # extension agency
  "extension_agency_mofa", "extension_agency_ngo", "extension_agency_coop",
  # the service menu -- draft Table 4's first block
  "services_planting", "services_mechanization", "services_credit",
  "services_irrigation", "services_husbandry", "services_agchemicals",
  "services_post_harvest", "services_employment", "services_records",
  "services_labour",
  # extension_compliance 0..3  (Stata: compliance1..4)
  .DUMMIES[["extension_compliance"]],
  # extension 0..k  (Stata: extensionCat1..7)
  .DUMMIES[["extension"]])
IND <- intersect(IND, names(d))

.absent <- setdiff(c("services0", "farm_association", "community_cooperative",
                     "extension0", "services_planting", "services_agchemicals",
                     "extension_agency_mofa"), IND)
if (length(.absent))
  stop("100: indicators required by draft Table 4 are missing after expansion: ",
       paste(.absent, collapse = ", "), call. = FALSE)

message("Engine B: ", length(IND), " indicators x ", length(crops_all),
        " crops ...")

t2 <- do.call(rbind, lapply(crops_all, function(cr) {
  dc <- d[as.character(d$CropID) == cr, , drop = FALSE]
  if (!nrow(dc)) return(NULL)
  r <- try(descriptive_indicator_shares(
    descriptive_prepare(dc), IND,
    trend = "continuous", per_wave = FALSE, weights = WEIGHTS),
    silent = TRUE)
  if (inherits(r, "try-error") || is.null(r)) {
    message("  no shares for crop: ", cr)
    return(NULL)
  }
  r$crop <- cr
  r
}))
if (is.null(t2) || !nrow(t2))
  stop("100: Engine B returned nothing.", call. = FALSE)

# =============================================================================
#  PARITY REPORT -- against the frozen reference, not against a fallback
# =============================================================================
# Anchors read off narrative/diagnostics/verification_reference_2026-08-07.json
# on 2026-08-13 and confirmed against study_raw_data before this script existed.
# They are checked here so a later change to the outcome list, the wave window
# or the weighting announces itself instead of quietly moving the paper.
#
# This REPORTS; it does not stop. A compute step that refuses to cache its own
# output leaves nothing to diff. The builders in exhibit_helpers_tables.R are
# where a mismatch must be fatal.
.anchor <- function(label, got, want, tol = 0.005) {
  ok <- length(got) == 1L && !is.na(got) && abs(got - want) <= tol
  message(sprintf("  [%s] %-42s got %-12s want %s",
                  if (ok) "ok" else "DIFF", label,
                  format(round(got, 4)), format(want)))
  ok
}
.pick1 <- function(x, ...) {
  k <- list(...); ok <- rep(TRUE, nrow(x))
  for (n in names(k)) ok <- ok & as.character(x[[n]]) == as.character(k[[n]])
  v <- x$estimate[ok]; if (length(v) == 1L) v else NA_real_
}

message("Parity against verification_reference_2026-08-07.json:")
.ok <- c(
  .anchor("Table1 Pooled Female mean",
          .pick1(t1, treatment = "services0", crop = "Pooled", outcome = "Female",
                 wave = "all", group = "pooled", statistic = "mean"), 0.2477),
  .anchor("Table1 Pooled Yield mean",
          .pick1(t1, treatment = "services0", crop = "Pooled", outcome = "Yield",
                 wave = "all", group = "pooled", statistic = "mean"), 1112.91, tol = 0.05),
  .anchor("TableS2 Rice Yield mean",
          .pick1(t1, treatment = "services0", crop = "Rice", outcome = "Yield",
                 wave = "all", group = "pooled", statistic = "mean"), 1251.80, tol = 0.05),
  .anchor("Table2-services Millet services_planting",
          .pick1(t2[t2$crop == "Millet", ], outcome = "services_planting",
                 wave = "pooled", statistic = "mean"), 0.515, tol = 0.001))
if (!all(.ok))
  message("100: at least one anchor moved. That is a FINDING -- investigate it ",
          "before the next render; do not adjust the anchor to match.")

# ---- Cache -------------------------------------------------------------------
res <- list(
  table1 = t1,
  shares = t2,
  meta = list(
    generated   = as.character(Sys.time()),
    study       = project_name,
    weights     = attr(t1, "weights"),
    waves       = sort(unique(as.character(d$Surveyx))),
    treatments  = TREATMENTS,
    crops_A     = crops_A,
    all_crops   = ALL_CROPS,
    indicators  = IND,
    trend_flavor = "continuous",
    n_rows      = nrow(d)))

saveRDS(res, OUT_RDS)
message("Wrote ", OUT_RDS,
        "  (table1: ", nrow(t1), " rows; shares: ", nrow(t2), " rows)")
invisible(TRUE)
