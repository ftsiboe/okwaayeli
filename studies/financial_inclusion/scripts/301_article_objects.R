# 301_article_objects.R
# Emit narrative/article_objects.json -- every number the prose cites that comes
# from an estimation object, so text and exhibits cannot drift apart.
#
# Run from the repo root, AFTER 004 (estimations) and 003 (te_summary.rds).
#
# ============================================================================
# READ THIS BEFORE TRUSTING THE OUTPUT
# ============================================================================
# This file is DELIBERATELY MINIMAL. It emits only what can be keyed correctly
# against this study's objects, and nothing else.
#
# The temptation is to mirror land_tenure's 301, which emits a block per
# treatment dimension. That would be wrong here, and wrong in a way that does not
# error:
#
#   land_tenure's treatment is MULTI-LEVEL (tenure form: 1 = reference,
#   2/3 = categories). This study's treatment is BINARY -- 002 sets
#   Treat <- as.integer(credit_hh > 0). Applying a multi-level keying to a binary
#   frontier yields NA for the missing level and puts the REFERENCE category
#   under the COMPARISON group's label: a real number meaning its opposite,
#   serialized into JSON looking authoritative.
#
# So: emit what you can key, and let the exhibit builders cover the rest. Extend
# this file one key at a time, and verify each against a known-correct figure
# before wiring it into the prose.
#
# The diagnostic block below prints the codes ACTUALLY PRESENT in the objects.
# Read it on the first local run. Do not delete it.
# ============================================================================

tryCatch({rm(list = ls()[!(ls() %in% c(Keep.List))]); gc()}, error = function(e) {
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

if (!exists("OBJECTS_JSON"))
  source("studies/financial_inclusion/scripts/article_helpers.R")

if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("301: package 'jsonlite' is required.", call. = FALSE)

project_name      <- "financial_inclusion"
study_environment <- readRDS(file.path(DATA, paste0(project_name, "_study_environment.rds")))
study_environment <- study_dirs(study_environment, layout = "v2")

objs <- list()

# ---- Sample sizes ------------------------------------------------------------
# From the same environment the tables' headers count, so a header and the prose
# cannot disagree.
raw <- study_environment$study_raw_data
est <- study_environment$estimation_data

if (is.null(est))
  stop("301: estimation_data is absent from the study environment.\n",
       "  002_MATCHING attaches it; 001_DATA re-saves without it.\n",
       "  Run MATCHING = TRUE in run_article.R.", call. = FALSE)

objs$sample <- list(
  n_raw            = nrow(raw),
  n_estimation     = nrow(est),
  n_pooled         = sum(as.character(est$CropID) %in% "Pooled"),
  waves            = sort(unique(as.character(raw$Surveyx)))
)

# ---- Treatment prevalence ----------------------------------------------------
# credit_hh is the treatment (002:63) AND the technology variable (004:86).
# The composite index is no longer reported or plotted; heterogeneity is shown
# across formal account ownership (Banked). See
# narrative/diagnostics/financial_inclusion_index_documentation.md.
pooled <- est[as.character(est$CropID) %in% "Pooled", , drop = FALSE]
if (nrow(pooled) && "Treat" %in% names(pooled)) {
  objs$treatment <- list(
    variable  = "credit_hh",
    n_treated = sum(pooled$Treat %in% 1L),
    n_control = sum(pooled$Treat %in% 0L),
    share_treated = mean(pooled$Treat %in% 1L)
  )
}

# ---- Credit component shares -------------------------------------------------
# The Data section reports the household credit rate and its components by who
# in the household holds the credit. These are not in any table -- the tables
# split BY credit, they do not report its incidence -- so they belong here.
#
# 004:86's technology_variables are the component definitions. They are NOT
# mutually exclusive (a household can have both farmer and spouse credit), so
# the components sum to more than the household rate. The prose says so; do not
# "fix" it.
# ============================================================================
# READ THIS BEFORE CHANGING .share(). Flagged 2026-08-09.
#
# `na.rm = TRUE` here is NOT harmless, and the reason is not obvious from this
# file. In the harmonized release the sub-indicators are never 0 inside a credit
# household -- they are 1 or MISSING. That is the disability study's convention,
# carried over: blank the sub-category where the composite is 1 but the specific
# category is not identified, so a zero is never read as "explicitly not this
# category". credit_hh itself has no missing.
#
# Measured on the pooled sample (n = 15,860; 1,405 treated):
#
#   variable        ones   missing among credit_hh == 1
#   credit_self      972   433  (30.8%)
#   credit_spouse    474   931  (66.3%)
#   credit_child      35  1370  (97.5%)
#   credit_close     508   897  (63.8%)
#   credit_member   1026   379  (27.0%)
#
# na.rm = TRUE drops exactly those rows, so each component is divided by a
# denominator shrunk by its own missingness:
#
#   component       published   correct (missing = 0)   error
#   share_self       6.3006%     6.1286%                +0.172 pp
#   share_spouse     3.1750%     2.9887%                +0.186 pp
#   share_child      0.2415%     0.2207%                +0.021 pp
#   share_close      3.3950%     3.2030%                +0.192 pp
#   share_member     6.6275%     6.4691%                +0.158 pp
#
# share_hh is unaffected (no missing), so the headline treatment rate is right;
# the three component figures quoted in 02_data.Rmd are each ~0.17 pp too high.
#
# THE FIX IS ONE ARGUMENT, and it is deliberately NOT APPLIED here because it
# changes three published numbers and that is a decision, not a cleanup:
#
#   .share <- function(df, v) {
#     if (!v %in% names(df)) return(NA_real_)
#     x <- suppressWarnings(as.numeric(df[[v]]))
#     mean(!is.na(x) & x > 0)        # blanked sub-indicator == not that category
#   }
#
# See narrative/diagnostics/credit_variable_documentation.md section 4.2.
# ============================================================================
.share <- function(df, v) if (v %in% names(df)) mean(df[[v]] > 0, na.rm = TRUE) else NA_real_

# Blank-as-zero reader, used by the derived quantities below. Kept separate from
# .share() so that fixing one does not silently change the other.
.num0 <- function(df, v) {
  if (!v %in% names(df)) return(NULL)
  x <- suppressWarnings(as.numeric(df[[v]]))
  x[is.na(x)] <- 0
  x
}

objs$credit <- list(
  share_hh     = .share(pooled, "credit_hh"),
  share_self   = .share(pooled, "credit_self"),
  share_spouse = .share(pooled, "credit_spouse"),
  share_child  = .share(pooled, "credit_child"),
  share_close  = .share(pooled, "credit_close"),
  share_member = .share(pooled, "credit_member")
)

# ---- Derived treatment-definition quantities ---------------------------------
# Added 2026-08-09 for the Data and Context sections. Each is computed with
# blanks read as zeros (see .num0 above), which is the intended convention.

# Reclassification rate, farmer-only vs household definition. 02_data.Rmd cites
# this to make the robustness promise specific rather than gestural. The
# household measure is a strict superset -- no operator borrows in a household
# recorded as having no credit -- so this is a one-directional share.
.hh   <- .num0(pooled, "credit_hh")
.self <- .num0(pooled, "credit_self")
objs$credit$reclass_farmer_only <-
  if (!is.null(.hh) && !is.null(.self)) mean(.hh != .self) else NA_real_

# Approval conditional on applying, by round. NOT the same as the Table 3
# "Accepted" row, which is a share of ALL farmers, not of applicants. 03's
# claim that approval "improved" was checked against this and did not survive:
# it is flat-to-falling, while REJECTIONS fall. The prose now says so.
.approval <- function(df, wave) {
  if (!all(c("Applied", "Accept", "Surveyx") %in% names(df))) return(NA_real_)
  a <- df[as.character(df$Surveyx) %in% wave & .num0(df, "Applied") %in% 1, , drop = FALSE]
  if (!nrow(a)) return(NA_real_)
  mean(suppressWarnings(as.numeric(a$Accept)) %in% 1)
}
objs$credit$approval_glss6 <- .approval(pooled, "GLSS6")
objs$credit$approval_glss7 <- .approval(pooled, "GLSS7")

# Share applying for a loan regardless of outcome. The Data section contrasts
# this with share_self to show that relaxing the gate to "any application"
# changes the construct rather than loosening it: the three tighter rules
# (applied & granted, amount > 0, and the study rule) select identical rows.
objs$credit$applied_share <- local({
  a <- .num0(pooled, "Applied")
  if (is.null(a)) NA_real_ else mean(a %in% 1)
})

# Share of GLSS7 applicants with an application still in "processing" -- a
# status code GLSS6's instrument does not have (s12aq8 is binary there;
# s12aq8a-c is three-way in GLSS7). The indicator treats it as non-use. Cited in
# 02_data.Rmd as an instrument asymmetry. `Proces` is missing for ALL of GLSS6,
# which is correct and is why this is a GLSS7-only key.
objs$credit$processing_share_glss7 <- local({
  if (!all(c("Applied", "Surveyx") %in% names(pooled)) || !"Proces" %in% names(pooled))
    return(NA_real_)
  a <- pooled[as.character(pooled$Surveyx) %in% "GLSS7" &
                .num0(pooled, "Applied") %in% 1, , drop = FALSE]
  if (!nrow(a)) return(NA_real_)
  p <- suppressWarnings(as.numeric(a$Proces)); p[is.na(p)] <- 0
  mean(p > 0)
})

missing_comp <- names(objs$credit)[vapply(objs$credit, function(z) is.null(z) || is.na(z), logical(1))]
if (length(missing_comp))
  warning("301: credit key(s) absent or NA: ",
          paste(missing_comp, collapse = ", "),
          " -- the Data and Context sections cite them and the knit will stop.",
          call. = FALSE, immediate. = TRUE)

# ---- Treatment-effect summary ------------------------------------------------
TE <- file.path(OUTPUT, "te_summary.rds")
if (file.exists(TE)) {
  te <- readRDS(TE)
  objs$te_summary_available <- TRUE
  # DIAGNOSTIC -- read this, then add keys for the rows the prose actually cites.
  message("\n--- te_summary.rds columns ---")
  print(names(te))
  if ("estimand" %in% names(te)) {
    message("--- estimands present ---"); print(unique(te$estimand))
  }
} else {
  objs$te_summary_available <- FALSE
  message("301: ", TE, " not found -- run TREATMENT = TRUE.")
}

# ---- DIAGNOSTIC: what codes do the estimation objects actually carry? --------
# The pooled specification is the one 004 attaches disaggregated scores to
# (004:180-187: credit_hh x optimal x Pooled x CropID).
main <- file.path(study_environment$wd$estimations,
                  "CropID_Pooled_credit_hh_TL_hnormal_optimal.rds")
if (file.exists(main)) {
  m <- readRDS(main)
  message("\n--- main estimation object: top-level names ---")
  print(names(m))

  if (!is.null(m$disagscors)) {
    ds <- m$disagscors
    message("--- disagscors: disaggregation variables present ---")
    print(sort(unique(as.character(ds$disagscors_var))))

    # THE TRAP, restated. sf_estm-style objects can carry two codings of the same
    # concept in different columns that need not agree -- a numeric `Tech` where
    # 1 means the group a labelled `TCHLvel` calls "0". Key on the LABELLED
    # column. Guess wrong and every cell populates, every star is right, and the
    # columns are transposed.
    for (nm in intersect(c("Tech", "TCHLvel"), names(ds))) {
      message("--- ", nm, " (", class(ds[[nm]])[1], ") levels ---")
      print(sort(unique(as.character(ds[[nm]]))))
    }
    objs$disag_variables <- sort(unique(as.character(ds$disagscors_var)))
  }
} else {
  message("301: ", main, " not found -- run MSF (004) on the HPC first.")
}

# ---- Write -------------------------------------------------------------------
# NOTHING BEYOND THIS POINT IS KEYED TO A FRONTIER GROUP. Add keys deliberately,
# one at a time, each verified against a known-correct figure. A key that is
# wrong here is wrong everywhere the prose cites it, silently.
jsonlite::write_json(objs, OBJECTS_JSON, auto_unbox = TRUE, pretty = TRUE,
                     digits = NA, null = "null")
message("\nWrote ", OBJECTS_JSON, "  (", length(objs), " top-level keys)")
invisible(TRUE)
