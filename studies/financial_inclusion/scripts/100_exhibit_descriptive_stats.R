# 100_exhibit_descriptive_stats.R
# Compute the descriptive tables from study_raw_data and cache them to
# data/descriptive_exhibits.rds, which exhibit_helpers_tables.R reads at knit
# time.
#
# WHY A SEPARATE STEP: the engine fits a model per (treatment x crop x outcome)
# -- minutes for the pooled sample alone, longer with crops. Too slow to run
# inside every knit, hence compute-once-and-cache.
#
# The engine is R/descriptive-exhibits-core.R, covered by ~15,000 assertions
# across two studies. See tests/testthat/test-descriptive-exhibits-*.R.
#
# Run from the repo root.
#
# ============================================================================
# ADAPTED FROM land_tenure, NOT YET RUN AGAINST THIS STUDY'S DATA.
#
# The engine API below is faithful to land_tenure's usage. The VARIABLE NAMES
# are inferred from what 002_MATCHING and 004_MSF reference, and are therefore
# guesses until this runs once. Specifically unverified:
#
#   - `credit_hh` as the treatment argument. land_tenure passes its raw binary
#     ("OwnLnd"); 002 here derives Treat from credit_hh > 0. Confirm the engine
#     wants the raw column, and confirm credit_hh is 0/1 rather than a count.
#     Note land_tenure's own outcome list contains a DIFFERENT column called
#     "Credit" -- do not assume the two are the same variable.
#   - the indicator block. Taken from 004:187's disagscors_list, which is the
#     only place in this study that enumerates the financial-access categoricals.
#   - which crops carry a row.
#
# Every name is guarded by intersect(..., names(d)), so a wrong guess drops the
# variable quietly rather than erroring. THAT IS A HAZARD, not a convenience:
# check the messages below report the counts you expect on the first run.
# ============================================================================

tryCatch({rm(list = ls()[!(ls() %in% c(Keep.List))]); gc()}, error = function(e) {
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

STUDY   <- "studies/financial_inclusion"
SE_RDS  <- file.path(STUDY, "data", "financial_inclusion_study_environment.rds")
OUT_RDS <- file.path(STUDY, "data", "descriptive_exhibits.rds")

stopifnot(file.exists(SE_RDS))
d <- readRDS(SE_RDS)$study_raw_data
if (is.null(d) || !nrow(d))
  stop("100: study_raw_data is absent or empty. Run DATA + MATCHING.", call. = FALSE)
message("study_raw_data: ", nrow(d), " rows")

# ---- Table 1 -----------------------------------------------------------------
# Two models, one table: OLS over the continuous outcomes, logit over the binary
# ones. `families` is what carries that distinction into the spec grid.
CONT <- c("Yield", "Area", "SeedKg", "HHLaborAE", "HirdHr", "FertKg",
          "PestLt", "AgeYr", "YerEdu", "HHSizeAE", "Depend", "CrpMix",
          "FinIdxSi")                      # the index enters as a covariate
BIN  <- c("Female", "EqipMech", "Extension", "EqipIrig", "OwnLnd",
          "Banked", "Insured")

CONT <- intersect(CONT, names(d))
BIN  <- intersect(BIN,  names(d))

# Six treatment splits, not one -- verified against the v005 draft (see
# narrative/diagnostics/exhibit_inventory_v005.md):
#
#   Table 1      Pooled | No credit | Some credit          -> credit_hh
#   Table S1/S2  Farmer | Spouse | Child | Spouse-or-child |
#                Household member other                    -> the other five
#
# These are 004:86's technology_variables. Passing only credit_hh does not
# error; it silently caches one split where three tables need six.
TREAT <- c("credit_hh", "credit_self", "credit_spouse",
           "credit_child", "credit_close", "credit_member")
missing_treat <- setdiff(TREAT, names(d))
if (length(missing_treat) == length(TREAT))
  stop("100: none of the treatment columns are in study_raw_data (",
       paste(TREAT, collapse = ", "), ").\n  Credit-like columns present: ",
       paste(grep("redit", names(d), value = TRUE), collapse = ", "), call. = FALSE)
if (length(missing_treat))
  warning("100: treatment columns absent, Tables S1/S2 will be incomplete: ",
          paste(missing_treat, collapse = ", "), call. = FALSE, immediate. = TRUE)
TREAT <- intersect(TREAT, names(d))
message("Treatments: ", paste(TREAT, collapse = ", "))

# Crops carrying a Table 1 row, plus Pooled. Restricted to what is present.
T1_CROPS <- c("Pooled", "Maize", "Rice", "Millet", "Sorghum", "Beans", "Peanut",
              "Cassava", "Yam", "Cocoyam", "Plantain", "Pepper", "Okra",
              "Tomatoe", "Cocoa", "Palm")

spec <- descriptive_specifications(
  d,
  outcomes   = c(CONT, BIN),
  treatments = TREAT,
  crops      = intersect(T1_CROPS, unique(as.character(d$CropID))),
  families   = c(stats::setNames(rep("gaussian", length(CONT)), CONT),
                 stats::setNames(rep("binomial", length(BIN)),  BIN)))

message("Table 1: ", nrow(spec), " specifications ...")
t1 <- draw_descriptive_summary(spec, d, study = "financial_inclusion")

# ---- Financial-access indicator shares ---------------------------------------
# 001 restricts the study to GLSS6 and GLSS7, so there is no wider window to
# choose from here -- unlike land_tenure, where the descriptive window is a
# deliberate COMPARABILITY restriction narrower than the estimation sample.
#
# If that ever changes, document the per-round comparability of each financial
# module item first. This study has no Table S0 equivalent, which is a gap:
# see narrative/diagnostics/financial_inclusion_index_documentation.md.
#
# Trend flavor is wave_diff: logit on i.Survey, then nlcom (GLSS6 - GLSS7) * 100
# -- percentage POINTS, earlier minus later.
DESC_WAVES <- c("GLSS6", "GLSS7")
dt <- d[as.character(d$Surveyx) %in% DESC_WAVES, , drop = FALSE]

# Categorical blocks, expanded to dummies.
#
# CORRECTED 2026-08-08 against the REAL column names, read out of
# $disagscors in CropID_Pooled_credit_hh_TL_hnormal_optimal.rds via 301's
# diagnostic block. Four names inferred from the v005 draft's row labels were
# all wrong; the draft describes the CONTENT of each block, not its column
# prefix:
#
#   reasons for no bank account   NoAccRsn  -> NonBanked_Why
#   purpose of loan               LoanPurp  -> Use
#   reason for rejection          RejRsn    -> Refusal
#   source of institution knowledge  (missed) -> Bank_Info
#
# Note Source_* also exists and is a DIFFERENT block -- source of loans
# (Table 3), not source of knowledge (Table 2). Do not merge them.
#
# "Reason for not applying for loan" appears in the draft's Table 3 but has no
# counterpart in disagscors. Either it is named differently in study_raw_data or
# it was never carried into the estimation data. Check names(d) before
# concluding the rows have to be dropped.
CATS <- c("FinIdxCat", "InstTyp", "AccTyp", "PrdTyp", "Source", "Collateral",
          "Insured", "NonBanked_Why", "Use", "Refusal", "Bank_Info", "WhyNoLoan")

# "Reason for not applying for loan" DOES exist -- WhyNoLoan_1..5, confirmed
# from the variable labels in data/financial_inclusion_study_data.dta. The
# earlier note that it had no counterpart was reading disagscols, which omits it.
#
# Continuous rows the draft prints INSIDE Tables 2 and 3 -- distances and loan
# amounts. Pushed through the same shares call so the builders can look them up
# on one key; the engine reports mean and sd for these as it does for the
# binaries. Note the .dta calls the amounts `Loan` and `RePaid` while
# study_raw_data uses `LoanAmt` -- the two files disagree, so both spellings are
# offered and whichever exists wins.
CONT_ROWS_WANTED <- c("BankKm", "RoadKm", "TrnprtKm",
                      "LoanAmt", "Loan", "LoanRepaid", "RePaid")

IND <- c(
  intersect(c("Banked", "Insured", "Applied", "Refused", "Accept", "Proces",
              "FinWorker", "HHFinWorker"), names(dt)),
  grep("^FinIdxCat_",     names(dt), value = TRUE),
  grep("^InstTyp_",       names(dt), value = TRUE),   # institution type
  grep("^AccTyp_",        names(dt), value = TRUE),   # account type
  grep("^PrdTyp_",        names(dt), value = TRUE),   # transaction products
  grep("^Bank_Info_",     names(dt), value = TRUE),   # source of knowledge (T2)
  grep("^NonBanked_Why_", names(dt), value = TRUE),   # reasons for no account (T2)
  grep("^Insured_",       names(dt), value = TRUE),   # insurance enrolment (T2)
  grep("^Source_",        names(dt), value = TRUE),   # source of LOANS (T3)
  grep("^Use_",           names(dt), value = TRUE),   # purpose of loan (T3)
  grep("^Collateral_",    names(dt), value = TRUE),   # guarantee (T3)
  grep("^Refusal_",       names(dt), value = TRUE),   # rejection reason (T3)
  grep("^WhyNoLoan_",     names(dt), value = TRUE),   # reason for NOT applying (T3)
  intersect(CONT_ROWS_WANTED, names(dt)))             # continuous rows (T2/T3)
IND <- unique(IND)

# Continuous rows the draft prints inside the same tables -- distances (Table 2)
# and loan amounts (Table 3). These need a mean/SD treatment, NOT a share, so
# they are recorded in `meta` for the builders rather than pushed through
# descriptive_indicator_shares(). Wire them when ft_table2()/ft_table3() are
# written; leaving them out is why those tables will come up a few rows short.
#
# (Restored 2026-08-08: an earlier edit rewrote the IND block and dropped this
# definition while `meta` below still referenced it -- hence
# "object 'CONT_ROWS' not found" AFTER the expensive Table 1 stage had already
# run. The lesson is in the ordering: validate the whole script's symbols before
# the slow part, not after.)
CONT_ROWS <- intersect(CONT_ROWS_WANTED, names(dt))
message("Continuous rows held for the builders: ",
        if (length(CONT_ROWS)) paste(CONT_ROWS, collapse = ", ") else "(none matched)")

if (!length(IND))
  stop("100: no financial-access indicators matched. The names in CATS/IND are ",
       "inferred from 004_MSF and have not been verified against the data. ",
       "Inspect names(dt) and correct them.", call. = FALSE)

crops_b <- unique(as.character(dt$CropID))
message("Shares: ", length(IND), " indicators x ", length(crops_b), " crops ...")

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
            meta = list(generated  = as.character(Sys.time()),
                        weights    = attr(t1, "weights"),
                        n_rows     = nrow(d),
                        treatment  = TREAT,
                        indicators = IND,
                        cont_rows  = CONT_ROWS,
                        waves      = DESC_WAVES))
saveRDS(res, OUT_RDS)
message("Wrote ", OUT_RDS,
        "  (table1: ", nrow(t1), " rows; shares: ",
        if (is.null(t2)) 0 else nrow(t2), " rows)")
invisible(TRUE)
