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
# The index this study is named for enters as a covariate (FinIdxSi) and a
# heterogeneity dimension (FinIdxCat) -- see
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
.share <- function(df, v) if (v %in% names(df)) mean(df[[v]] > 0, na.rm = TRUE) else NA_real_
objs$credit <- list(
  share_hh     = .share(pooled, "credit_hh"),
  share_self   = .share(pooled, "credit_self"),
  share_spouse = .share(pooled, "credit_spouse"),
  share_child  = .share(pooled, "credit_child"),
  share_close  = .share(pooled, "credit_close"),
  share_member = .share(pooled, "credit_member")
)
missing_comp <- names(objs$credit)[vapply(objs$credit, is.na, logical(1))]
if (length(missing_comp))
  warning("301: credit component(s) absent from estimation_data: ",
          paste(missing_comp, collapse = ", "),
          " -- the Data section cites them and the knit will stop.",
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
