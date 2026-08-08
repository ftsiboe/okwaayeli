# exhibit_helpers_tables.R
# Build the manuscript tables as flextable objects.
#
# A LIBRARY, NOT A STEP: no position in a sequence, hence no number. Sourced by
# narrative/financial-inclusion.Rmd at knit time and by 102. See scripts/README.md.
#
# SOURCES
#   Tables 1, 2, 3, S1, S2   data/descriptive_exhibits.rds        (100)
#   Tables 4, S3, S4         sf_estm / el_mean / ef_mean          (004)
#   Tables 5, 6              disagscors                           (004)
#
# STATUS: all ten tables are LIVE.
#
# VINTAGE WARNING. The estimation objects were fitted 2026-04-26; 002_MATCHING
# was re-run 2026-08-08 and 004_MSF has not run since. Tables 4, 5, 6, S3 and S4
# therefore build correctly but print PROVISIONAL numbers, and reproduce none of
# v005's published values. Tables 1, 2, 3, S1 and S2 come from study_raw_data,
# which matching does not touch -- Tables 2 and 3 reproduce v005 to the third
# decimal. Re-run the MSF array, then re-render.
#
# NO FALLBACKS. Every builder errors rather than degrading to a stored value. A
# builder that falls back to a frozen CSV lets the knit "succeed" while printing
# stale numbers beside prose citing live ones. A failed render is cheaper.
# A cell with no matching row renders "" -- that is an absent estimate, not a
# substituted one, and matches land_tenure's behaviour.
#
# KEYING. Credit group is TCHLvel, NOT Tech. The objects carry both for the same
# concept and they disagree: land_tenure's header records Tech 1 == TCHLvel "0",
# and this study's diagnostic shows the same shape (Tech -999/1/2 against
# TCHLvel "0"/"1"/"National"). Treatment here is credit_hh > 0, so "1" reads
# intuitively as *has credit* -- which is exactly how the columns get
# transposed. See narrative/diagnostics/estimation_object_keying.md.
.ft_ok <- tryCatch({ loadNamespace("flextable"); TRUE },
                   error = function(e) conditionMessage(e))
if (!isTRUE(.ft_ok))
  stop("exhibit_helpers_tables.R could not load 'flextable'.\n",
       "  Reason: ", .ft_ok, "\n",
       "  If the package is missing:  install.packages(\"flextable\")\n",
       "  If a dependency is broken (systemfonts / gdtools / textshaping), restart R\n",
       "  in a clean session and run:\n",
       "    install.packages(c(\"systemfonts\",\"textshaping\",\"gdtools\",\"flextable\"))",
       call. = FALSE)
suppressPackageStartupMessages(library(flextable))

# Manuscript font: Times New Roman everywhere (docx + html), matching the
# document body (reference.docx theme + css/tables.css).
set_flextable_defaults(font.family = "Times New Roman")

# Self-contained path resolution: sourced BOTH from the repo root (run_article.R)
# and from narrative/ (the Rmd's knit_root_dir).
.STUDY_ROOT <- if (dir.exists("output/estimations")) {
  "."
} else if (dir.exists("../output/estimations")) {
  ".."
} else {
  "studies/financial_inclusion"
}

# ---- Memoization --------------------------------------------------------------
# Once tbl_num() lookups route through the builders, a manuscript with ~100 calls
# rebuilds a handful of tables that many times, each re-reading estimation
# objects that are tens of MB compressed. Cache per table id and per estimation
# object. Session-lived; call exhibit_cache_clear() after re-running 004 or 100.
.CACHE <- new.env(parent = emptyenv())

.memo <- function(key, f) {
  if (!exists(key, envir = .CACHE, inherits = FALSE))
    assign(key, f(), envir = .CACHE)
  get(key, envir = .CACHE)
}

#' Drop the exhibit cache. Call after re-running 004 or 100 in a live session.
exhibit_cache_clear <- function() {
  rm(list = ls(.CACHE, all.names = TRUE), envir = .CACHE)
  invisible(TRUE)
}

.EST <- file.path(.STUDY_ROOT, "output", "estimations")
.EST_PARTS <- c("ef_mean", "el_mean", "sf_estm", "disagscors")

.read_est <- function(tag)
  .memo(paste0("est:", tag), function() {
    p <- readRDS(file.path(.EST,
      sprintf("CropID_Pooled_%s_TL_hnormal_optimal.rds", tag)))
    keep <- intersect(names(p), .EST_PARTS)
    if (!length(keep))
      stop("exhibit_helpers_tables.R: ", tag, " has none of ",
           paste(.EST_PARTS, collapse = "/"), ". Components present: ",
           paste(names(p), collapse = ", "), call. = FALSE)
    p[keep]
  })

.stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}
# Published cell format: 0.683*** (0.004)
.cell <- function(est, se, p) {
  if (length(est) == 0 || is.na(est)) return("-")
  sprintf("%.3f%s (%.3f)", est, .stars(p), se)
}

# ---- Shared styling -----------------------------------------------------------
.style_desc <- function(ft, hdr, nrows, size = 8) {
  if (length(hdr)) ft <- bold(ft, i = hdr, j = 1, part = "body")
  body <- setdiff(seq_len(nrows), hdr)
  ft <- padding(ft, i = body, j = 1, padding.left = 8, part = "body")
  ft <- bold(ft, part = "header")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- padding(ft, padding.top = 0, padding.bottom = 0, part = "all")
  ft <- line_spacing(ft, space = 1, part = "all")
  ft <- fontsize(ft, size = size, part = "all")
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft
}

# Generic builder: label + value columns from a data.frame (label, header, c1..cN).
.ft_build <- function(d, cols, first_lab = "Variable", size = 8,
                      spanner = NULL, spanwidths = NULL, notes = NULL) {
  hdr <- which(d$header == "1")
  vcols <- paste0("c", seq_along(cols))
  m <- d[, c("label", vcols)]
  ft <- flextable(m)
  ft <- set_header_labels(ft, values = stats::setNames(
    as.list(c(first_lab, cols)), c("label", vcols)))
  if (!is.null(spanner))
    ft <- add_header_row(ft, top = TRUE, values = spanner, colwidths = spanwidths)
  ft <- align(ft, j = seq(2, ncol(m)), align = "right", part = "all")
  ft <- align(ft, i = 1, align = "center", part = "header")
  ft <- valign(ft, valign = "bottom", part = "header")
  ft <- .style_desc(ft, hdr, nrow(m), size = size)
  if (!is.null(notes)) {
    ft <- add_footer_lines(ft, values = notes)
    ft <- fontsize(ft, size = 6, part = "footer")
  }
  ft
}

# Catch a build that came out structurally empty. A guard on emptiness does NOT
# catch a transposed table -- that needs a diff against a known reference -- but
# it does catch a keying failure that renders every cell blank.
.guard_filled <- function(d, id, min_frac = 0.25) {
  vals <- unlist(d[d$header == "0", grep("^c[0-9]+$", names(d)), drop = FALSE])
  filled <- mean(nzchar(vals))
  if (is.nan(filled) || filled < min_frac)
    stop("exhibit_helpers_tables.R: ", id, " is ",
         sprintf("%.0f%%", 100 * (1 - ifelse(is.nan(filled), 0, filled))),
         " blank -- the row map's indicator names almost certainly do not match ",
         "the cache.\n  Inspect: unique(readRDS(\"",
         file.path(.STUDY_ROOT, "data", "descriptive_exhibits.rds"),
         "\")$shares$outcome)", call. = FALSE)
  invisible(TRUE)
}

# ---- Descriptive layer --------------------------------------------------------
# NB use .STUDY_ROOT, NOT article_helpers.R's constants: those are
# repo-root-relative, and knitr's working directory is narrative/ during a render.
.DESC <- file.path(.STUDY_ROOT, "data", "descriptive_exhibits.rds")
.desc <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      if (!file.exists(.DESC))
        stop("exhibit_helpers_tables.R: missing ", .DESC,
             "\n  Run: DESCRIPTIVE = TRUE in scripts/run_article.R",
             call. = FALSE)
      cache <<- readRDS(.DESC)
    }
    cache
  }
})

# Single value from the long frame. Errors on duplicates: a keyed schema should
# never produce two rows for one cell, and taking the first silently turns a
# schema bug into a plausible wrong number.
.pick <- function(d, keys, col) {
  ok <- rep(TRUE, nrow(d))
  for (k in names(keys)) ok <- ok & !is.na(d[[k]]) & d[[k]] == keys[[k]]
  v <- d[[col]][ok]
  if (length(v) == 0) return(NA_real_)
  if (length(v) > 1)
    stop("exhibit_helpers_tables.R: ", length(v), " rows matched ",
         paste(sprintf("%s=%s", names(keys), unlist(keys)), collapse = ", "),
         "; expected 1.", call. = FALSE)
  as.numeric(v[1])
}

.SRC_NOTE <- "Data source: Ghana Living Standards Survey [waves 6-7]."
.SIG_NOTE <- "Significance levels: * p<0.10, ** p<0.05, *** p<0.01."

# ==============================================================================
# Table 2 -- Financial services dynamics
# ==============================================================================
# Row order follows v005 (narrative/diagnostics/exhibit_inventory_v005.md).
# Column names and display labels are the VARIABLE LABELS carried in
# data/financial_inclusion_study_data.dta, read out rather than guessed -- an
# earlier pass inferred four prefixes from the draft's row text and got all four
# wrong. Suffix->label pairings below are the .dta's, verbatim.
.T2_MAP <- data.frame(
  label = c(
    "Has bank account/contributing to a scheme (binary)",
    "Reasons for no bank account and contributing to a loan/savings scheme (single choice)",
    "Don't have enough money or income", "Don't have regular income",
    "Not necessary/interested", "Financial institutions are too far away",
    "Process cumbersome", "Unaware of any", "Low or no income", "Mistrust",
    "Spouse", "Too young",
    "Types of financial institution with accounts or contribution (multichoice)",
    "Commercial/community/rural bank", "Mobile money", "Susu scheme",
    "Savings and loans scheme", "Cooperative/credit union", "Investment/mortgage",
    "Type of account held in the financial institution (multichoice)",
    "Savings", "Current or cheque", "Investment",
    "Transaction products utilized (multichoice)",
    "Cheque book", "ATM card", "e-banking",
    "Source of financial institution knowledge (single choice)",
    "Colleagues/relatives", "Radio",
    "Representative from the financial institution",
    "Community/association leaders", "Employer/union", "Television",
    "Newspaper/magazine", "Non-governmental organization (NGO)", "Self",
    "Financial institution worker (multichoice)",
    "Farmer", "Household member",
    "Distance to nearest amenities (km)",
    "Bank", "Road", "Transportation",
    "Insurance enrolment (multichoice)",
    "Health", "Life", "Vehicle", "Pension", "Investment", "Death", "Education",
    "Asset", "Business", "Travel",
    "Financial inclusion score"),
  header = c(0, 1, rep(0, 10), 1, rep(0, 6), 1, 0,0,0, 1, 0,0,0,
             1, rep(0, 9), 1, 0,0, 1, 0,0,0, 1, rep(0, 10), 0),
  Variable = c(
    "Banked",
    NA, "NonBanked_Why_1", "NonBanked_Why_2", "NonBanked_Why_7",
    "NonBanked_Why_3", "NonBanked_Why_8", "NonBanked_Why_6", "NonBanked_Why_4",
    "NonBanked_Why_5", "NonBanked_Why_9", "NonBanked_Why_10",
    NA, "InstTyp_Bank", "InstTyp_Momo", "InstTyp_Susu", "InstTyp_Save",
    "InstTyp_Coop", "InstTyp_Invt",
    NA, "AccTyp_Save", "AccTyp_Curnt", "AccTyp_Invst",
    NA, "PrdTyp_Cheq", "PrdTyp_ATM", "PrdTyp_Ebnk",
    NA, "Bank_Info_1", "Bank_Info_7", "Bank_Info_8", "Bank_Info_2",
    "Bank_Info_3", "Bank_Info_10", "Bank_Info_5", "Bank_Info_6", "Bank_Info_9",
    NA, "FinWorker", "HHFinWorker",
    NA, "BankKm", "RoadKm", "TrnprtKm",
    NA, "Insured_Health", "Insured_Life", "Insured_Car", "Insured_Pension",
    "Insured_Invest", "Insured_Death", "Insured_Edu", "Insured_Asset",
    "Insured_Buss", "Insured_Travel",
    "FinIdxSi"),
  stringsAsFactors = FALSE)
# Bank_Info_4 ("Family/Friend") has no row in v005 -- distinct from Bank_Info_1
# ("Colleagues/Relatives"). Either the draft merged them or the level is empty.
# Confirm before adding a row; do not fold them together silently.

# ==============================================================================
# Table 3 -- Loan application outcomes
# ==============================================================================
.T3_MAP <- data.frame(
  label = c(
    "Loan applied",
    "Loan application outcomes (multichoice)",
    "Accepted", "Rejected", "Processing",
    "Accepted loan applications",
    "Total amount of the loan (GH₵)", "Total amount of the loan repaid (GH₵)",
    "Source of loans (multichoice)",
    "Friends/relatives", "Group based micro-finance or lending",
    "Formal lender (bank/financial institution)", "Informal credit/savings groups",
    "Non-governmental organization (NGO)", "Governmental organization",
    "Purpose for loans (multichoice)",
    "Agricultural", "Business", "Other", "Education/Training", "Ceremonies",
    "To pay off debts",
    "Guarantee/collateral (multichoice)",
    "None", "Cash", "Third party security", "Other", "Land", "House/building",
    "Farm produce", "Assets",
    "Reason for rejected loan applications (single choice)",
    "Collateral/trust", "Other", "Inappropriate purpose",
    "Loan amount requested was too big", "Previous debt problems",
    "Reason for not applying for loan (single choice)",
    "No need", "Institutional constraint", "Demographic/emotional constraint",
    "Financial constraint", "No source/access"),
  header = c(0, 1, 0,0,0, 1, 0,0, 1, rep(0, 6), 1, rep(0, 6), 1, rep(0, 8),
             1, rep(0, 5), 1, rep(0, 5)),
  Variable = c(
    "Applied",
    NA, "Accept", "Refused", "Proces",
    NA, "LoanAmt", "LoanRepaid",
    NA, "Source_1", "Source_5", "Source_6", "Source_2", "Source_4", "Source_3",
    NA, "Use_1", "Use_2", "Use_6", "Use_3", "Use_5", "Use_4",
    NA, "Collateral_1", "Collateral_5", "Collateral_3", "Collateral_2",
    "Collateral_7", "Collateral_8", "Collateral_6", "Collateral_4",
    NA, "Refusal_3", "Refusal_6", "Refusal_4", "Refusal_2", "Refusal_5",
    NA, "WhyNoLoan_5", "WhyNoLoan_2", "WhyNoLoan_4", "WhyNoLoan_3", "WhyNoLoan_1"),
  stringsAsFactors = FALSE)
# WhyNoLoan_* is the "reason for not applying" block that an earlier pass
# recorded as having no counterpart. It does exist -- it is simply absent from
# disagscors, which is what that pass was reading. Read from the .dta's labels.
# Refusal_1 ("Awaiting Reply") has no row in v005.
#
# LoanAmt / LoanRepaid: the .dta calls these `Loan` and `RePaid`; 100's
# meta$cont_rows confirms `LoanAmt` exists in study_raw_data but NOT
# `LoanRepaid`. The two datasets do not share names here. Expect the repaid row
# to come up blank until that is reconciled.

# Shared cell logic for the wave-difference tables (2 and 3).
.shares_live <- function(map, id) {
  s <- .desc()$shares
  out <- map
  for (cc in paste0("c", 1:3)) out[[cc]] <- ""
  for (i in seq_len(nrow(out))) {
    if (out$header[i] == 1) next
    v <- out$Variable[i]
    if (is.na(v)) next
    sh <- function(w) {
      k <- list(outcome = v, crop = "Pooled", wave = w)
      b <- .pick(s, k, "estimate"); sd <- .pick(s, k, "sd")
      if (is.na(b)) "" else sprintf("%.3f (%.3f)", b, sd)
    }
    # wave_diff: percentage POINTS, GLSS6 - GLSS7.
    # v005 prints the standard error in BRACKETS here ("-10.899 [0.916]"),
    # unlike land_tenure, which reports the point estimate alone. Match the
    # paper: the bracket convention is what distinguishes a trend cell from a
    # level cell throughout this manuscript.
    k <- list(outcome = v, crop = "Pooled", wave = "trend")
    b <- .pick(s, k, "estimate"); bse <- .pick(s, k, "se")
    out$c1[i] <- sh("GLSS6")
    out$c2[i] <- sh("GLSS7")
    out$c3[i] <- if (is.na(b)) "" else if (is.na(bse)) sprintf("%.3f", b) else
      sprintf("%.3f [%.3f]", b, bse)
  }
  out$header <- as.character(out$header)
  out <- out[, c("label", "header", paste0("c", 1:3))]
  .guard_filled(out, id)
  out
}

# Header n's from the same build that prints the table -- never typed. v005
# hardcodes these, and writes the same number two ways (15860 / 15,860).
.shares_hdr <- function(probe) {
  s <- .desc()$shares
  n <- function(w) .pick(s, list(outcome = probe, crop = "Pooled", wave = w), "n")
  f <- function(x) if (is.na(x)) "?" else format(round(x), big.mark = ",")
  c(sprintf("GLSS6 (2012/13) (n=%s)", f(n("GLSS6"))),
    sprintf("GLSS7 (2016/17) (n=%s)", f(n("GLSS7"))),
    sprintf("Change (2012/13 to 2016/17, pp) (n=%s)", f(n("pooled"))))
}

.tbl2_live <- function() .memo("tbl2", function() .shares_live(.T2_MAP, "Table 2"))
.tbl3_live <- function() .memo("tbl3", function() .shares_live(.T3_MAP, "Table 3"))

ft_table2 <- function()
  .ft_build(.tbl2_live(), .shares_hdr("Banked"), first_lab = "Outcome", size = 8,
    spanner = c("", "Mean (standard deviation)", ""),
    spanwidths = c(1, 2, 1),
    notes = c("Standard deviations in parentheses.",
      "The change column is the difference in margins between waves, in percentage points (GLSS6 less GLSS7).",
      "Restricted to GLSS6-GLSS7, the rounds administering the financial inclusion module in comparable form.",
      .SRC_NOTE))

ft_table3 <- function()
  .ft_build(.tbl3_live(), .shares_hdr("Applied"), first_lab = "Outcome", size = 8,
    spanner = c("", "Mean (standard deviation)", ""),
    spanwidths = c(1, 2, 1),
    notes = c("Standard deviations in parentheses.",
      "The change column is the difference in margins between waves, in percentage points (GLSS6 less GLSS7).",
      "Loan amounts are in nominal Ghana cedis.",
      .SRC_NOTE))

# ==============================================================================
# Tables 5 and 6 -- disagscors / ef_mean
# ==============================================================================
# The matched sample is named for the optimal specification's link (or its
# distance where link is NA) -- "probit" here. Read it from the environment
# rather than hardcoding, so a re-run of 002 that changes the optimal spec does
# not silently leave these tables on the old one.
.se_path <- file.path(.STUDY_ROOT, "data", "financial_inclusion_study_environment.rds")
.mspecs  <- if (file.exists(.se_path)) readRDS(.se_path)$match_specification_optimal else NULL
.opt     <- if (!is.null(.mspecs))
  ifelse(is.na(.mspecs$link), .mspecs$distance, .mspecs$link) else NA_character_
if (is.na(.opt))
  warning("exhibit_helpers_tables.R: could not resolve the optimal matching ",
          "sample from match_specification_optimal; Tables 5 and 6 will be blank.",
          call. = FALSE)

# Cell from a long frame carrying jackknife inference.
.jcell <- function(d, keys) {
  ok <- rep(TRUE, nrow(d))
  for (k in names(keys)) ok <- ok & !is.na(d[[k]]) & d[[k]] == keys[[k]]
  r <- d[ok, , drop = FALSE]
  if (!nrow(r)) return("")
  if (nrow(r) > 1) {
    # Name the columns that actually distinguish the matches. A bare "2 rows
    # matched" sends you back to the data to work out which key is missing;
    # this says so directly.
    varying <- names(r)[vapply(names(r), function(cn)
      length(unique(as.character(r[[cn]]))) > 1, logical(1))]
    varying <- setdiff(varying, c("Estimate", "Estimate.mean", "Estimate.sd",
                                  "Estimate.length", "jack_zv", "jack_pv"))
    stop("exhibit_helpers_tables.R: ", nrow(r), " rows matched ",
         paste(sprintf("%s=%s", names(keys), unlist(keys)), collapse = ", "),
         "; expected 1.\n  Add to the key -- these differ across the matches: ",
         if (length(varying)) paste(sprintf("%s (%s)", varying,
           vapply(varying, function(cn)
             paste(sort(unique(as.character(r[[cn]]))), collapse = "/"),
             character(1))), collapse = ", ") else "(none -- true duplicate rows)",
         call. = FALSE)
  }
  .cell(r$Estimate[1], r$Estimate.sd[1], r$jack_pv[1])
}

# ---- Table 5: parity by person with credit -----------------------------------
# The four row groups are FOUR SEPARATE ESTIMATION FILES, not four levels inside
# one. Each technology_variable was fitted on its own frontier, so the object
# tag changes per row -- see 004:86's technology_variables.
.T5_TAGS <- c("credit_hh", "credit_self", "credit_spouse", "credit_member")
.T5_LABS <- c("Anyone including farmer", "Farmer", "Spouse of farmer",
              "Household members other than spouses or child")
# type in ef_mean; the draft's block headings.
.T5_BLOCKS <- c(TGR = "Technology gap ratio (TGR)",
                TE  = "Pure farmer technical efficiency (TE)",
                MTE = "Meta-frontier technical efficiency (MTE)")

.tbl5_live <- function() .memo("tbl5", function() {
  rows <- list()
  for (ty in names(.T5_BLOCKS)) {
    rows[[length(rows) + 1]] <- data.frame(
      label = unname(.T5_BLOCKS[[ty]]), header = 1,
      c1 = "", c2 = "", c3 = "", stringsAsFactors = FALSE)
    for (j in seq_along(.T5_TAGS)) {
      ef <- tryCatch(.read_est(.T5_TAGS[j])$ef_mean, error = function(e) NULL)
      cel <- function(lv, coef) {
        if (is.null(ef)) return("")
        # KEY ON TCHLvel. "0" = no credit, "1" = some credit -- confirmed by
        # cross-tabulation against Tech (1 == "0"). See
        # narrative/diagnostics/estimation_object_keying.md.
        #
        # restrict is pinned to "Unrestricted" but does not matter for these
        # measures: the probe shows Restricted and Unrestricted carry identical
        # Estimates for TGR/TE/MTE. Pinned anyway so the key stays unique --
        # .jcell() stops on duplicates.
        .jcell(ef, list(TCHLvel = lv, type = ty, sample = .opt,
                        Survey = "GLSS0", stat = "mean", CoefName = coef,
                        estType = "teBC", restrict = "Unrestricted"))
      }
      a <- cel("0", "efficiency"); b <- cel("1", "efficiency")
      # The difference is CARRIED, not computed. ef_mean emits a triple for
      # TCHLvel == "1": efficiency (the level), efficiencyGap_lvl (B less A) and
      # efficiencyGap_pct. An earlier version of this builder subtracted the two
      # point estimates by hand and printed no standard error, on the reasoning
      # that the group frontiers are fitted separately so no covariance exists.
      # That was wrong -- the jackknife carries it, which is why v005 can print
      # "0.015 (0.027)".
      dif <- cel("1", "efficiencyGap_lvl")
      rows[[length(rows) + 1]] <- data.frame(
        label = .T5_LABS[j], header = 0, c1 = a, c2 = b, c3 = dif,
        stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  out$header <- as.character(out$header)
  .guard_filled(out, "Table 5")
  out
})

ft_table5 <- function()
  .ft_build(.tbl5_live(),
    c("No credit [A]", "Some credit [B]", "Difference [B-A]"),
    first_lab = "", size = 8,
    notes = c(.SIG_NOTE,
      "Jackknife standard errors in parentheses.",
      "The difference column is B less A, reported by the estimator with its own jackknife standard error.",
      .SRC_NOTE))

# ---- Table 6: parity by financial-service indicator --------------------------
# Same query 101_exhibit_figures.R runs for the heterogeneity figures:
# CoefName == "disag_efficiencyGap_lvl" is the no-credit-less-credit difference,
# and `input` carries TGR / TE / MTE.
.T6_MAP <- data.frame(
  label = c(
    "Has bank account/contributing to a scheme", "Insured",
    "Types of financial institution with accounts or contribution",
    "Commercial/community/rural bank", "Cooperative/credit union",
    "Investment/mortgage", "Mobile money", "Savings and loans scheme",
    "Susu scheme",
    "Reasons for no bank account and contributing to a loan/savings scheme",
    "Don't have enough money or income", "Don't have regular income",
    "Financial institutions are too far away", "Not necessary/interested",
    "Type of account held in the financial institution",
    "Current or cheque", "Savings",
    "Source of financial institution knowledge",
    "Colleagues/relatives", "Community/assoc. leaders", "Employer/union",
    "Radio", "Representative from the financial institution",
    "Loan application outcomes",
    "Loan applied", "Loan application processing", "Loan application rejected",
    "Collateral/trust"),
  header = c(0,0, 1, 0,0,0,0,0,0, 1, 0,0,0,0, 1, 0,0, 1, 0,0,0,0,0, 1, 0,0,0, 0),
  Variable = c(
    "Banked", "Insured",
    NA, "InstTyp_Bank", "InstTyp_Coop", "InstTyp_Invt", "InstTyp_Momo",
    "InstTyp_Save", "InstTyp_Susu",
    NA, "NonBanked_Why_1", "NonBanked_Why_2", "NonBanked_Why_3",
    "NonBanked_Why_7",
    NA, "AccTyp_Curnt", "AccTyp_Save",
    NA, "Bank_Info_1", "Bank_Info_2", "Bank_Info_3", "Bank_Info_7",
    "Bank_Info_8",
    NA, "Applied", "Proces", "Refused", "Collateral_3"),
  stringsAsFactors = FALSE)
# v005 prints "#N/A" in the "Loan application Processing" row -- an Excel
# artifact that reached print. It will render blank or live here; either is an
# improvement. Flagged in exhibit_inventory_v005.md.
# Rows 4 and 11 of the draft repeat the section heading text across the value
# cells -- also a formatting artifact, corrected by marking them as headers.

# Which level of each 0/1 indicator Table 6 reports. See the note in the loop.
.T6_LEVEL <- "1"

.tbl6_live <- function() .memo("tbl6", function() {
  ds <- .read_est("credit_hh")$disagscors
  out <- .T6_MAP
  for (cc in paste0("c", 1:3)) out[[cc]] <- ""
  inputs <- c("TGR", "TE", "MTE")
  for (i in seq_len(nrow(out))) {
    if (out$header[i] == 1) next
    v <- out$Variable[i]
    if (is.na(v)) next
    for (j in seq_along(inputs)) {
      # disag_efficiencyGap_lvl is the carried B-less-A difference, the same
      # triple structure ef_mean uses (efficiency / _Gap_lvl / _Gap_pct). The
      # gap rows exist only under TCHLvel == "1", so no TCHLvel key is needed.
      #
      # disagscors_level IS needed. The gap is computed WITHIN each level of the
      # disaggregation variable, so a binary indicator like Banked carries a gap
      # row for level "0" and another for level "1" -- keying without it matched
      # two rows and .jcell() stopped, correctly.
      #
      # "1" is the level the draft reports: Table 6's rows read as "the credit
      # gap among farmers who HAVE a bank account / ARE insured / used a susu
      # scheme", which is level 1 of each dummy. Every row in .T6_MAP is a
      # 0/1 indicator, so this is uniform across the table.
      out[[paste0("c", j)]][i] <- .jcell(ds, list(
        disagscors_var = v, disagscors_level = .T6_LEVEL, input = inputs[j],
        CoefName = "disag_efficiencyGap_lvl", stat = "mean",
        sample = .opt, Survey = "GLSS0", restrict = "Unrestricted",
        estType = "teBC"))
    }
  }
  out$header <- as.character(out$header)
  out <- out[, c("label", "header", paste0("c", 1:3))]
  .guard_filled(out, "Table 6")
  out
})

ft_table6 <- function()
  .ft_build(.tbl6_live(),
    c("Technology gap ratio (TGR)", "Pure farmer technical efficiency (TE)",
      "Meta-frontier technical efficiency (MTE)"),
    first_lab = "", size = 8,
    spanner = c("", "Difference [no credit less credit]"),
    spanwidths = c(1, 3),
    notes = c(.SIG_NOTE,
      "Jackknife standard errors in parentheses.",
      "Each cell is the no-credit less credit difference within the stated group, from the matched sample.",
      .SRC_NOTE))

# ==============================================================================
# Tables 4, S3, S4 -- the frontier tables
# ==============================================================================
# COLUMN MAPPING, pinned by the Nobs grid against v005's published sample sizes
# (probes/logs/probe_frontier.log). There is a FOURTH TCHLvel level, "Meta",
# absent from disagscors but present in sf_estm/el_mean/ef_mean:
#
#   v005 column            TCHLvel     sample      GLSS6+GLSS7   v005 prints
#   Naive national         National    unmatched   9918 + 5942   15,860  ok
#   Group: No credit [A]   "0"         unmatched   9026 + 5429   14,455  ok
#   Group: Some credit [B] "1"         unmatched    892 +  513    1,405  ok
#   Meta-frontier Matched  Meta        <optimal>   1784 + 1026    2,810  ok
#   Meta-frontier Unmatch. Meta        unmatched   9918 + 5942   15,860  ok
#
# Survey == "GLSS0" is the MEAN across waves, not the pooled total: National
# GLSS0 Nobs is 7,930, exactly half of 15,860. Sample-size cells therefore SUM
# GLSS6 and GLSS7. Everything else reads GLSS0, which is the pooled estimate.
.FRONT_COLS <- list(
  list(key = "National", samp = "unmatched"),
  list(key = "0",        samp = "unmatched"),
  list(key = "1",        samp = "unmatched"),
  list(key = "1",        samp = "unmatched", gap = TRUE),   # Difference [B-A]
  list(key = "Meta",     samp = "OPT"),
  list(key = "Meta",     samp = "unmatched"))

# UNRESOLVED, and deliberately a single switch rather than a scatter of literals.
#
# Every frame carries restrict = "Restricted" / "Unrestricted" and the two
# disagree for everything except TGR. Which one v005 reported cannot be settled
# from the current objects, because they are a different vintage (fitted
# 2026-04-26, against a matching that was re-run 2026-08-08) and reproduce none
# of the draft's numbers exactly.
#
# "Restricted" is the closer of the two on the most diagnostic comparison --
# the Land elasticity, where Restricted gives 0.756 against v005's 0.753 while
# Unrestricted gives 0.547. Taken as the working choice. Flip this one constant
# after MSF re-runs and the parity check is done.
.RESTRICT <- "Restricted"

.samp_of <- function(x) if (identical(x, "OPT")) .opt else x

# One frontier cell. `df` is el_mean / ef_mean / sf_estm; `keys` the frame's own
# discriminators (input, type, CoefName, ...).
.fcell <- function(df, col, keys, coef_col = "CoefName",
                   level_coef = NULL, gap_coef = NULL, survey = "GLSS0") {
  if (is.null(df)) return("")
  k <- c(keys, list(TCHLvel = col$key, sample = .samp_of(col$samp),
                    Survey = survey, restrict = .RESTRICT))
  if (!is.null(level_coef))
    k[[coef_col]] <- if (isTRUE(col$gap)) gap_coef else level_coef
  else if (isTRUE(col$gap)) return("")   # frame carries no gap row
  .jcell(df, k)
}

# Sum a diagnostic over the two waves. Only the sample-size row needs this; see
# the GLSS0 note above.
.sum_waves <- function(df, col, coefname) {
  v <- vapply(c("GLSS6", "GLSS7"), function(w) {
    ok <- df$CoefName == coefname & as.character(df$TCHLvel) == col$key &
          df$sample == .samp_of(col$samp) & df$restrict == .RESTRICT &
          df$Survey == w
    if (!any(ok)) NA_real_ else df$Estimate[ok][1]
  }, numeric(1))
  if (all(is.na(v))) "" else format(round(sum(v, na.rm = TRUE)), big.mark = ",")
}

# Plain value, no stars -- the diagnostics block prints rates and criteria.
.fplain <- function(df, col, coefname, digits = 2) {
  if (is.null(df)) return("")
  ok <- df$CoefName == coefname & as.character(df$TCHLvel) == col$key &
        df$sample == .samp_of(col$samp) & df$restrict == .RESTRICT &
        df$Survey == "GLSS0"
  if (!any(ok)) return("")
  sprintf(paste0("%.", digits, "f"), df$Estimate[ok][1])
}

# ---- Table 4 -----------------------------------------------------------------
.T4_EL <- data.frame(
  label = c("Land", "Planting material", "Family labour", "Hired labour",
            "Fertilizer", "Pesticide", "Returns to scale"),
  input = paste0("el", 1:7), stringsAsFactors = FALSE)

.T4_DIAG <- data.frame(
  label = c("Sample size", "Monotonicity satisfaction rate",
            "Curvature satisfaction rate", "Schmidt & Lin (1984)",
            "Coelli (1995)", "Gutierrez (2001)", "Log likelihood",
            "No. of parameters", "Meta frontier LR test",
            "Ratio variance due to inefficiency"),
  coef  = c("Nobs", "mono", "curv", "olsSkew", "CoelliM3Test", "LRInef",
            "mlLoglik", "nXvar", "LRT", "Gamma"),
  digits = c(0, 2, 2, 3, 3, 3, 0, 0, 3, 3),
  stringsAsFactors = FALSE)
# "Gutierrez (2001)" -> LRInef is INFERRED from magnitude and starring: v005
# prints 192.705**, the shape of a likelihood-ratio statistic, and LRInef is the
# only LR-of-inefficiency row available. Confirm against the paper's methods
# section before this goes out.

.tbl4_live <- function() .memo("tbl4", function() {
  E  <- .read_est("credit_hh")
  el <- E$el_mean; ef <- E$ef_mean; sf <- E$sf_estm
  rows <- list()
  add <- function(label, header, cells)
    rows[[length(rows) + 1]] <<- as.data.frame(c(
      list(label = label, header = header),
      stats::setNames(as.list(cells), paste0("c", seq_along(cells)))),
      stringsAsFactors = FALSE)
  blank <- rep("", 6)

  add("Elasticity", 1, blank)
  for (i in seq_len(nrow(.T4_EL)))
    add(.T4_EL$label[i], 0, vapply(.FRONT_COLS, function(cc)
      .fcell(el, cc, list(input = .T4_EL$input[i], stat = "mean"),
             level_coef = "elasticity", gap_coef = "elasticityGap_lvl"),
      character(1)))

  add("Technology/efficiency", 1, blank)
  # The Matched/Unmatched distinction here is a ROW, not a column: v005 puts it
  # in the stub and leaves the two Meta columns as "-" for this block.
  for (ty in names(.T5_BLOCKS)) {
    add(unname(.T5_BLOCKS[[ty]]), 1, blank)
    for (sm in c(Matched = "OPT", Unmatched = "unmatched"))
      add(names(which(c(Matched = "OPT", Unmatched = "unmatched") == sm)), 0,
          c(vapply(.FRONT_COLS[1:4], function(cc)
              .fcell(ef, utils::modifyList(cc, list(samp = sm)),
                     list(type = ty, stat = "mean", estType = "teBC"),
                     level_coef = "efficiency", gap_coef = "efficiencyGap_lvl"),
              character(1)),
            "-", "-"))
  }

  add("Model diagnostics", 1, blank)
  for (i in seq_len(nrow(.T4_DIAG))) {
    cf <- .T4_DIAG$coef[i]
    add(.T4_DIAG$label[i], 0, vapply(.FRONT_COLS, function(cc) {
      if (isTRUE(cc$gap)) return("-")
      if (identical(cf, "Nobs")) .sum_waves(sf, cc, cf)
      else .fplain(sf, cc, cf, .T4_DIAG$digits[i])
    }, character(1)))
  }

  out <- do.call(rbind, rows)
  out$header <- as.character(out$header)
  .guard_filled(out, "Table 4")
  out
})

.FRONT_HDR <- c("Naive national frontier", "No credit [A]", "Some credit [B]",
                "Difference [B-A]", "Matched", "Unmatched")

ft_table4 <- function()
  .ft_build(.tbl4_live(), .FRONT_HDR, first_lab = "", size = 7,
    spanner = c("", "National", "Group frontier", "Meta-frontier"),
    spanwidths = c(1, 1, 3, 2),
    notes = c(.SIG_NOTE,
      "Jackknife standard errors in parentheses.",
      sprintf("Estimates are from the %s specification; see exhibit_helpers_tables.R on that choice.", tolower(.RESTRICT)),
      "Sample sizes are summed over GLSS6 and GLSS7.",
      .SRC_NOTE))

# ---- Tables S3 / S4: sf_estm coefficients ------------------------------------
# Five columns, no Difference: v005's S3 and S4 omit it.
.SF_COLS <- .FRONT_COLS[c(1, 2, 3, 5, 6)]
.SF_HDR  <- c("Naive national frontier", "No credit", "Some credit",
              "Matched", "Unmatched")

.sf_rows <- function(map, id) {
  sf <- .read_est("credit_hh")$sf_estm
  out <- map
  for (cc in paste0("c", 1:5)) out[[cc]] <- ""
  for (i in seq_len(nrow(out))) {
    if (out$header[i] == 1 || is.na(out$coef[i])) next
    for (j in seq_along(.SF_COLS))
      out[[paste0("c", j)]][i] <-
        .fcell(sf, .SF_COLS[[j]], list(CoefName = out$coef[i]))
  }
  out$header <- as.character(out$header)
  out <- out[, c("label", "header", paste0("c", 1:5))]
  .guard_filled(out, id)
  out
}

.S3_MAP <- data.frame(
  label = c("Production function",
            "Land [lnI1]", "Planting material [lnI2]", "Family labour [lnI3]",
            "Hired labour [lnI4]", "Fertilizer [lnI5]", "Pesticide [lnI6]",
            "1/2 * lnI1 * lnI1", "lnI1*lnI2", "lnI1*lnI3", "lnI1*lnI4",
            "lnI1*lnI5", "lnI1*lnI6",
            "1/2 * lnI2 * lnI2", "lnI2*lnI3", "lnI2*lnI4", "lnI2*lnI5", "lnI2*lnI6",
            "1/2 * lnI3 * lnI3", "lnI3*lnI4", "lnI3*lnI5", "lnI3*lnI6",
            "1/2 * lnI4 * lnI4", "lnI4*lnI5", "lnI4*lnI6",
            "1/2 * lnI5 * lnI5", "lnI5*lnI6",
            "1/2 * lnI6 * lnI6",
            "Proportion of area under listed crop (base = maize)",
            "Cassava", "Peanut", "Plantain", "Rice", "Millet", "Sorghum",
            "Beans", "Yam", "Cocoa", "Other",
            "Ecological zone (base = Coastal Savannah)",
            "Forest", "Guinea Savanah", "Sudan Savanah", "Transitional",
            "Intercept",
            "Production risk function", "Intercept "),
  header = c(1, rep(0, 27), 1, rep(0, 10), 1, 0,0,0,0, 0, 1, 0),
  coef = c(NA,
           paste0("lnI", 1:6),
           "I(1/2 * lnI1 * lnI1)", "lnI1:lnI2", "lnI1:lnI3", "lnI1:lnI4",
           "lnI1:lnI5", "lnI1:lnI6",
           "I(1/2 * lnI2 * lnI2)", "lnI2:lnI3", "lnI2:lnI4", "lnI2:lnI5", "lnI2:lnI6",
           "I(1/2 * lnI3 * lnI3)", "lnI3:lnI4", "lnI3:lnI5", "lnI3:lnI6",
           "I(1/2 * lnI4 * lnI4)", "lnI4:lnI5", "lnI4:lnI6",
           "I(1/2 * lnI5 * lnI5)", "lnI5:lnI6",
           "I(1/2 * lnI6 * lnI6)",
           NA,
           "Area_Cassava", "Area_Peanut", "Area_Plantain", "Area_Rice",
           "Area_Millet", "Area_Sorghum", "Area_Beans", "Area_Yam",
           "Area_Cocoa", "Area_Other",
           NA,
           "factor(Ecozon)Forest Zone", "factor(Ecozon)Guinea Savanah",
           "factor(Ecozon)Sudan Savanah", "factor(Ecozon)Transitional Zone",
           "(Intercept)",
           NA, "Zv_(Intercept)"),
  stringsAsFactors = FALSE)
# v005's S3 carries a "Period (base = 2012/13) / 2016/17" block. sf_estm has NO
# year coefficient -- the wave enters through the Survey column, which indexes
# separate fits rather than a dummy inside one. Those rows are omitted rather
# than mapped to something that merely looks plausible. Establish how the draft
# produced them before adding them back.

.S4_MAP <- data.frame(
  label = c("Female farmer (dummy)", "Age (years)", "Education (years)",
            "Land owned (dummy)", "Crop diversification (index)",
            "Mechanization (dummy)", "Extension (dummy)",
            "Ecological zone (base = Coastal Savannah)",
            "Forest", "Guinea Savanah", "Sudan Savanah", "Transitional",
            "Intercept"),
  header = c(0,0,0,0,0,0,0, 1, 0,0,0,0, 0),
  coef = c("Zu_factor(Female)1", "Zu_lnAgeYr", "Zu_lnYerEdu",
           "Zu_factor(OwnLnd)1", "Zu_CrpMix", "Zu_factor(EqipMech)1",
           "Zu_factor(Extension)1",
           NA,
           "Zu_factor(Ecozon)Forest Zone", "Zu_factor(Ecozon)Guinea Savanah",
           "Zu_factor(Ecozon)Sudan Savanah", "Zu_factor(Ecozon)Transitional Zone",
           "Zu_(Intercept)"),
  stringsAsFactors = FALSE)
# Same omission as S3: v005's "Period / 2016/17" row has no counterpart in Zu_*.

.tblS3_live <- function() .memo("tblS3", function() .sf_rows(.S3_MAP, "Table S3"))
.tblS4_live <- function() .memo("tblS4", function() .sf_rows(.S4_MAP, "Table S4"))

ft_tableS3 <- function()
  .ft_build(.tblS3_live(), .SF_HDR, first_lab = "", size = 7,
    spanner = c("", "National", "Group frontier", "Meta-frontier"),
    spanwidths = c(1, 1, 2, 2),
    notes = c(.SIG_NOTE, "Jackknife standard errors in parentheses.",
      sprintf("%s specification.", .RESTRICT), .SRC_NOTE))

ft_tableS4 <- function()
  .ft_build(.tblS4_live(), .SF_HDR, first_lab = "", size = 7,
    spanner = c("", "National", "Group frontier", "Meta-frontier"),
    spanwidths = c(1, 1, 2, 2),
    notes = c(.SIG_NOTE, "Jackknife standard errors in parentheses.",
      sprintf("%s specification.", .RESTRICT), .SRC_NOTE))

# ==============================================================================
# Not yet written
# ==============================================================================
# Each stops with the reason and the next step, rather than returning something
# printable. Build order and the data each needs are in
# narrative/diagnostics/exhibit_inventory_v005.md sec.6.
.todo <- function(id, what)
  stop("exhibit_helpers_tables.R: ", id, " is not written yet.\n  Needs: ", what,
       "\n  See narrative/diagnostics/exhibit_inventory_v005.md sec.6 for build order.",
       call. = FALSE)

# ==============================================================================
# Tables 1, S1, S2 -- draw_descriptive_summary()
# ==============================================================================
# All three share ONE row map: v005's Table 1, S1 and S2 print identical rows and
# differ only in their columns (credit_hh split vs person-with-credit split, and
# levels vs trends). The only label discrepancy in the draft is "Seed (real
# GH\u20b5/ha)" in Table 1 against "Seed (GHC/ha)" in S1/S2 -- same variable,
# two spellings. Unified here.
.T1_MAP <- data.frame(
  label = c("Farmer",
            "Female farmer (dummy)", "Age (years)", "Education (years)",
            "Selected crop production (real GH\u20b5/ha)",
            "All crops", "Maize", "Rice", "Millet", "Sorghum", "Beans", "Peanut",
            "Cassava", "Yam", "Cocoyam", "Plantain", "Pepper", "Okra", "Tomato",
            "Cocoa", "Palm",
            "Land (ha)", "Land owned (dummy)", "Crop diversification (index)",
            "Seed (real GH\u20b5/ha)", "Household labour (AE)",
            "Hired labour (man-days/ha)", "Fertilizer (Kg/ha)",
            "Pesticide (Liter/ha)", "Mechanization (dummy)", "Irrigation (dummy)",
            "Household",
            "Size (AE)", "Dependency (ratio)"),
  header = c(1, 0,0,0, 1, rep(0, 16), rep(0, 10), 1, 0,0),
  Equ = c(NA,
          "Female", "AgeYr", "YerEdu",
          NA,
          rep("Yield", 16),
          "Area", "OwnLnd", "CrpMix", "SeedKg", "HHLaborAE", "HirdHr", "FertKg",
          "PestLt", "EqipMech", "EqipIrig",
          NA,
          "HHSizeAE", "Depend"),
  crop = c(NA,
           "Pooled", "Pooled", "Pooled",
           NA,
           "Pooled", "Maize", "Rice", "Millet", "Sorghum", "Beans", "Peanut",
           "Cassava", "Yam", "Cocoyam", "Plantain", "Pepper", "Okra",
           "Tomatoe",          # data spelling; the draft prints "Tomato"
           "Cocoa", "Palm",
           rep("Pooled", 10),
           NA,
           "Pooled", "Pooled"),
  stringsAsFactors = FALSE)

# Dagger: "a statistically significant difference from the pooled sample". The
# level difference (cat_diff) flags the mean columns; the interaction
# (trend_diff) flags the trend columns. Both group columns are flagged together
# -- with two groups the contrast is symmetric.
.T1_DAG <- 0.05

# One cell. NOTE the treatment key: land_tenure passed a single treatment so
# outcome/crop/group/wave/statistic was unique. Six treatments here means
# omitting `treatment` matches six rows, and .pick() stops on duplicates.
.d_mean <- function(m, tr, eq, cr, g, dag) {
  k <- list(treatment = tr, outcome = eq, crop = cr, group = g,
            wave = "all", statistic = "mean")
  b <- .pick(m, k, "estimate"); s <- .pick(m, k, "sd")
  if (is.na(b)) return("")
  sprintf("%.2f (%.2f)%s", b, s, if (isTRUE(dag)) " \u2020" else "")
}
.d_trend <- function(m, tr, eq, cr, g, dag) {
  k <- list(treatment = tr, outcome = eq, crop = cr, group = g,
            statistic = "trend_pct")
  b <- .pick(m, k, "estimate"); s <- .pick(m, k, "se"); p <- .pick(m, k, "p")
  if (is.na(b)) return("")
  sprintf("%.2f%s [%.2f]%s", b, .stars(p), s, if (isTRUE(dag)) " \u2020" else "")
}
.d_dag <- function(m, tr, eq, cr, stat) {
  p <- .pick(m, list(treatment = tr, outcome = eq, crop = cr, statistic = stat), "p")
  !is.na(p) && p < .T1_DAG
}

.tbl1_live <- function() .memo("tbl1", function() {
  m <- .desc()$table1
  out <- .T1_MAP
  for (cc in paste0("c", 1:6)) out[[cc]] <- ""
  for (i in seq_len(nrow(out))) {
    if (out$header[i] == 1) next
    eq <- out$Equ[i]; cr <- out$crop[i]
    dcat <- .d_dag(m, "credit_hh", eq, cr, "cat_diff")
    dtrd <- .d_dag(m, "credit_hh", eq, cr, "trend_diff")
    out$c1[i] <- .d_mean(m, "credit_hh", eq, cr, "pooled", FALSE)
    out$c2[i] <- .d_mean(m, "credit_hh", eq, cr, "0", dcat)
    out$c3[i] <- .d_mean(m, "credit_hh", eq, cr, "1", dcat)
    out$c4[i] <- .d_trend(m, "credit_hh", eq, cr, "pooled", FALSE)
    out$c5[i] <- .d_trend(m, "credit_hh", eq, cr, "0", dtrd)
    out$c6[i] <- .d_trend(m, "credit_hh", eq, cr, "1", dtrd)
  }
  out$header <- as.character(out$header)
  out <- out[, c("label", "header", paste0("c", 1:6))]
  .guard_filled(out, "Table 1")
  out
})

# Group sizes, live. v005 types these into the header and writes the same number
# two ways (15860 in Table 1, 15,860 in S1).
.tbl1_n <- function() {
  m <- .desc()$table1
  g <- function(gr) .pick(m, list(treatment = "credit_hh", outcome = "Yield",
                                  crop = "Pooled", group = gr, wave = "all",
                                  statistic = "mean"), "n")
  c(all = g("pooled"), non = g("0"), some = g("1"))
}
.fmt_n <- function(x) if (is.na(x)) "?" else format(round(x), big.mark = ",")

.tbl1_hdr <- function() {
  n <- .tbl1_n()
  lab <- c(sprintf("Pooled (n=%s)", .fmt_n(n[["all"]])),
           sprintf("No credit (n=%s)", .fmt_n(n[["non"]])),
           sprintf("Some credit (n=%s)", .fmt_n(n[["some"]])))
  c(lab, lab)
}

ft_table1 <- function()
  .ft_build(.tbl1_live(), .tbl1_hdr(), size = 8,
    spanner = c("", "Mean (standard deviation)", "Trend (%)"),
    spanwidths = c(1, 3, 3),
    notes = c(.SIG_NOTE,
      "Standard deviations in parentheses; standard errors in brackets. A dagger denotes a statistically significant difference from the pooled sample.",
      "The trend was estimated as the annual percentage change via a generalised linear model.",
      .SRC_NOTE))

# ---- Tables S1 / S2: the person-with-credit split ----------------------------
# Columns are five separate TREATMENTS, not five levels of one. Each contributes
# its group == "1" column: "has credit via this person".
.S_TREAT <- c("credit_self", "credit_spouse", "credit_child",
              "credit_close", "credit_member")
.S_LABS  <- c("Farmer", "Spouse of farmer", "Child (adopted or biological)",
              "Spouse or child of farmer", "Household member other than spouse or child")

.tblS_live <- function(stat) {
  m <- .desc()$table1
  f <- if (identical(stat, "mean")) .d_mean else .d_trend
  dstat <- if (identical(stat, "mean")) "cat_diff" else "trend_diff"
  out <- .T1_MAP
  for (cc in paste0("c", 1:6)) out[[cc]] <- ""
  for (i in seq_len(nrow(out))) {
    if (out$header[i] == 1) next
    eq <- out$Equ[i]; cr <- out$crop[i]
    out$c1[i] <- f(m, "credit_hh", eq, cr, "pooled", FALSE)
    for (j in seq_along(.S_TREAT)) {
      tr <- .S_TREAT[j]
      out[[paste0("c", j + 1)]][i] <- f(m, tr, eq, cr, "1",
                                        .d_dag(m, tr, eq, cr, dstat))
    }
  }
  out$header <- as.character(out$header)
  out[, c("label", "header", paste0("c", 1:6))]
}

.tblS_hdr <- function() {
  m <- .desc()$table1
  n <- function(tr, gr) .pick(m, list(treatment = tr, outcome = "Yield",
                                      crop = "Pooled", group = gr, wave = "all",
                                      statistic = "mean"), "n")
  c(sprintf("Pooled (n=%s)", .fmt_n(n("credit_hh", "pooled"))),
    vapply(seq_along(.S_TREAT), function(j)
      sprintf("%s (n=%s)", .S_LABS[j], .fmt_n(n(.S_TREAT[j], "1"))), character(1)))
}

.tblS1_live <- function() .memo("tblS1", function() {
  d <- .tblS_live("mean");      .guard_filled(d, "Table S1"); d })
.tblS2_live <- function() .memo("tblS2", function() {
  d <- .tblS_live("trend_pct"); .guard_filled(d, "Table S2"); d })

ft_tableS1 <- function()
  .ft_build(.tblS1_live(), .tblS_hdr(), size = 7,
    spanner = c("", "Pooled", "Person with credit"),
    spanwidths = c(1, 1, 5),
    notes = c(.SIG_NOTE,
      "Standard deviations in parentheses. A dagger denotes a statistically significant difference from the pooled sample.",
      .SRC_NOTE))

ft_tableS2 <- function()
  .ft_build(.tblS2_live(), .tblS_hdr(), size = 7,
    spanner = c("", "Pooled", "Person with credit"),
    spanwidths = c(1, 1, 5),
    notes = c(.SIG_NOTE,
      "Annual percentage change; standard errors in brackets. A dagger denotes a statistically significant difference from the pooled sample.",
      .SRC_NOTE))


# ==============================================================================
# Inline text lookups
# ==============================================================================
# A cell lookup must return the SAME BUILD the exhibit prints, not a file. The id
# is spelled like a filename so call sites need not change when a table moves.
#
# CHECK EVERY TABLE IS IN THIS SWITCH. One left out is one section of the paper
# quietly citing a frozen value.
.live_table <- function(id) {
  switch(id,
    "table1"  = .tbl1_live(),
    "table2"  = .tbl2_live(),
    "table3"  = .tbl3_live(),
    "tableS1" = .tblS1_live(),
    "tableS2" = .tblS2_live(),
    "table5"  = .tbl5_live(),
    "table4"  = .tbl4_live(),
    "table6"  = .tbl6_live(),
    "tableS3" = .tblS3_live(),
    "tableS4" = .tblS4_live(),
    stop("exhibit_helpers_tables.R: no live build registered for '", id,
         "'. Registered: table1, table2, table3, tableS1, tableS2. ",
         "Tables 4/5/6/S3/S4 are still stubs.",
         call. = FALSE))
}

#' Pull one cell out of a live table build, by row label and column.
#'
#' @param id     table id registered in .live_table()
#' @param label  the row's display label, exactly as printed
#' @param col    column index among the value columns (1-based)
#' @param part   "first" takes the estimate before any bracket; "paren" the
#'               value in (), "bracket" the value in [].
tbl_num <- function(id, label, col, part = c("first", "paren", "bracket"),
                    block = NULL) {
  part <- match.arg(part)
  d <- .live_table(id)
  i <- which(d$label == label)
  # Table 5 repeats its row labels across the TGR / TE / MTE blocks, and Table 4
  # repeats "Matched"/"Unmatched" the same way. `block` names the bold section
  # header the row sits under; the row is then the first match AFTER it.
  if (!is.null(block)) {
    b <- which(d$label == block & d$header == "1")
    if (length(b) != 1)
      stop("tbl_num(): block '", block, "' not found (or not unique) in ", id,
           ". Blocks present: ",
           paste(d$label[d$header == "1"], collapse = " | "), call. = FALSE)
    nxt <- which(d$header == "1" & seq_len(nrow(d)) > b)
    end <- if (length(nxt)) min(nxt) else nrow(d) + 1L
    i <- i[i > b & i < end]
  }
  if (length(i) != 1)
    stop("tbl_num(): ", length(i), " rows labelled '", label, "'",
         if (!is.null(block)) paste0(" in block '", block, "'") else "",
         " in ", id, "; expected 1.",
         if (length(i) > 1) " Pass block= to disambiguate." else
           paste0(" Labels present: ", paste(utils::head(d$label[d$header == "0"], 40),
                                             collapse = " | ")),
         call. = FALSE)
  x <- d[[paste0("c", col)]][i]
  if (!nzchar(x))
    stop("tbl_num(): '", label, "' column ", col, " of ", id, " is empty. ",
         "The prose cannot cite a cell the table does not print.", call. = FALSE)
  v <- switch(part,
    first   = sub("^\\s*(-?[0-9.]+).*$", "\\1", x),
    paren   = sub("^.*\\(([-0-9.]+)\\).*$", "\\1", x),
    bracket = sub("^.*\\[([-0-9.]+)\\].*$", "\\1", x))
  as.numeric(v)
}

#' Difference between two cells of the same table, in points.
#' The prose repeatedly says things like "a gain of 16.8 percentage points",
#' which is a derived quantity: two cells and a subtraction. Deriving it here
#' keeps it tied to the same build as the table beside it.
tbl_diff <- function(id, label, col_a, col_b, block = NULL, scale = 100, digits = 1)
  formatC(scale * (tbl_num(id, label, col_b, block = block) -
                   tbl_num(id, label, col_a, block = block)),
          format = "f", digits = digits)

tbl_pct <- function(id, label, col, digits = 1, block = NULL)
  paste0(formatC(100 * tbl_num(id, label, col, block = block),
                 format = "f", digits = digits), "%")
