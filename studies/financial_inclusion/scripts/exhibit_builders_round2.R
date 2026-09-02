# ==============================================================================
# ROUND 2 ADDITIONS -- Table S5 (variable definitions) and Table S6 (index
# loadings). Written 2026-09-02 for the Round-2 revision; not yet run.
# ==============================================================================
# WHERE THIS GOES
#   Append this file VERBATIM to the END of scripts/exhibit_helpers_tables.R,
#   after tbl_pct(). Nothing above it is modified. It defines
#
#       .tblS5_live() / ft_tableS5()      Table S5
#       .tblS6_live() / ft_tableS6()      Table S6
#
#   and wraps .live_table() so that tbl_num(), tbl_pct() and tbl_diff() reach
#   the ids "tableS5" and "tableS6".
#
#   It must be appended rather than sourced separately: it uses .memo(),
#   .ft_build(), .style_desc(), .guard_filled(), .fmt_n(), .STUDY_ROOT,
#   .SRC_NOTE and .live_table(), all of which are defined earlier in that file
#   and none of which are exported.
#
#   The appendix calls the two builders from 99_appendix.Rmd as
#   ```{r ft-tableS5} ft_tableS5() ``` and ```{r ft-tableS6} ft_tableS6() ```.
#
# WHEN YOU NEXT EDIT .live_table() BY HAND
#   Fold "tableS5" = .tblS5_live() and "tableS6" = .tblS6_live() into its
#   switch(), delete the wrapper at the foot of this block, and refresh that
#   function's "Registered: ..." error text, which is already stale (it still
#   says Tables 4/5/6/S3/S4 are stubs). The wrapper exists so that this block
#   works the moment it is appended; the switch is where the registration
#   belongs permanently, for the reason its own comment gives.
#
# TABLE S6 NEEDS A RELEASE THAT DOES NOT LIVE IN THE STUDY FOLDER
#   data-raw/releases/harmonized_data/financial_inclusion_index_diagnostics.dta
#   is written by scripts/000_INDEX_financial_inclusion_study.do (the postfile
#   block under DIAGNOSTICS), at the REPOSITORY root -- not under
#   studies/financial_inclusion, which is what .STUDY_ROOT points at. Run that
#   do-file once, from Stata, before knitting. If the .dta is absent
#   ft_tableS6() stops and prints every path it looked in; it never degrades to
#   a stored copy. Table S6 also needs the haven package.
#
#   Table S5 needs nothing at all: it states the specification, not an
#   estimate, so it is a literal data.frame here. Every variable name in it was
#   read off 002_MATCHING_financial_inclusion_study.R (match_variables_scaler,
#   match_variables_factor, match_variables_exact) and
#   004_MSF_financial_inclusion_study.R (output_variable, input_variables,
#   intercept_shifters, inefficiency_covariates, adoption_covariates), not off
#   a draft. If either script's variable lists change, this table is wrong and
#   nothing will error -- that is the price of a static table, and the reason
#   each row names the object it came from.
#
# THREE THINGS TO CHECK BEFORE CIRCULATING (see builders_report.md)
#   1. HrvstKg. 004_MSF passes output_variable = "HrvstKg" while Section 4.2
#      describes the dependent variable as the real value of crop output and
#      Table 1 prints "All crops" in real GH cedi/ha. The S5 row deliberately
#      does not assert a unit. Reconcile the two descriptions.
#   2. SeedKg, likewise: the name says kilograms, Table 1's label says
#      real GH cedi/ha. The row says "planting material" and cites Table 1.
#   3. Insurance labels. Table S6 uses the display labels Table 2 carries for
#      the same Insured_* variables (the .dta variable labels). Note S1's prose
#      uses the questionnaire wording instead -- "commercial" for Insured_Buss,
#      "medical" for Insured_Health, "funeral" for Insured_Death, "property"
#      for Insured_Asset, "retirement" for Insured_Pension. Align one to the
#      other before this goes out; a reader should not have to guess that the
#      note's "commercial insurance" is the table's "Business insurance".
# ==============================================================================

# Sign glyphs, named so the S5 map below reads as a table of decisions rather
# than a wall of escapes. An EM DASH is "the paper posits no expectation" --
# never a blank, which reads as an oversight, and never an invented sign.
# Written as \u escapes, as the cedi and dagger are elsewhere in this file: an
# encoding slip in a source file is silent until it reaches print.
.S5_NONE <- "\u2014"   # em dash: no expectation posited
.S5_NA   <- "Not applicable"
.S5_POS  <- "+"
.S5_NEG  <- "\u2212"   # minus sign

# ==============================================================================
# Table S5 -- definition, measurement, role and expected sign
# ==============================================================================
# STATIC BY DESIGN. Every other builder in this file reads an estimation object
# and refuses to print anything it cannot look up. This one cannot: there is no
# estimated quantity in it. What it can do -- and what the row constructors
# below enforce by shape -- is keep one row per variable the pipeline actually
# passes, with the script that passes it named in the cell.
#
# The blocks follow the order in which a reader meets the variables: the
# frontier of Equation (1), its shifters, the inefficiency term that Equation
# (3) reuses, then the matching of Section 4.3.
#
# ROLE VOCABULARY is closed, six values: frontier output, frontier input,
# frontier intercept shifter, inefficiency determinant, exact-match stratum,
# matching covariate. A variable holding two roles says both, in one cell,
# rather than appearing twice inside one block.
.S5_ROWS <- local({
  H <- function(label)
    data.frame(label = label, header = "1", c1 = "", c2 = "", c3 = "",
               stringsAsFactors = FALSE)
  R <- function(label, definition, role, sign)
    data.frame(label = label, header = "0", c1 = definition, c2 = role,
               c3 = sign, stringsAsFactors = FALSE)

  rbind(
    # ---- dependent variable ---------------------------------------------
    H("Dependent variable"),
    R("HrvstKg",
      paste0("Crop output of the farm operator's pooled crop record for the ",
             "season. Section 4.2 describes it as the real value of crop ",
             "output; Table 1 summarizes crop output as real GH\u20b5 per ",
             "hectare. Enters Equations (1) and (3) in logs."),
      "Frontier output", .S5_NONE),

    # ---- frontier inputs -------------------------------------------------
    H("Frontier inputs (logged, with the translog cross-terms of Equation (1))"),
    R("Area",
      paste0("Cultivated land in hectares (Table 1, \"Land (ha)\"). Enters as ",
             "lnI1 and is the denominator of the per-hectare quantities ",
             "reported in Table 1."),
      "Frontier input", .S5_POS),
    R("SeedKg",
      paste0("Planting material used on the farm (Table 1, ",
             "\"Seed (real GH\u20b5/ha)\"). Enters as lnI2."),
      "Frontier input", .S5_POS),
    R("HHLaborAE",
      paste0("Family labour supplied to the farm, in adult equivalents ",
             "(Table 1, \"Household labour (AE)\"). Enters as lnI3."),
      "Frontier input", .S5_POS),
    R("HirdHr",
      paste0("Hired labour (Table 1, \"Hired labour (man-days/ha)\"). Enters ",
             "as lnI4."),
      "Frontier input", .S5_POS),
    R("FertKg",
      paste0("Fertilizer applied (Table 1, \"Fertilizer (Kg/ha)\"). Enters as ",
             "lnI5."),
      "Frontier input", .S5_POS),
    R("PestLt",
      paste0("Pesticide applied (Table 1, \"Pesticide (Liter/ha)\"). Enters as ",
             "lnI6."),
      "Frontier input", .S5_POS),

    # ---- frontier shifters ----------------------------------------------
    H("Frontier shifters (h in Equations (1) and (3))"),
    R("Area_<crop>",
      paste0("Share of the operator's cultivated area under each crop that ",
             "averages at least 3% of area in the estimation sample, maize the ",
             "base category and Area_Other the residual share. The crop list ",
             "is discovered from the data rather than fixed in advance."),
      "Frontier intercept shifter", .S5_NONE),
    R("Survey",
      paste0("Survey round: GLSS6 (2012/13) or GLSS7 (2016/17). Entered as a ",
             "factor."),
      "Frontier intercept shifter; also an inefficiency determinant and an exact-match stratum",
      .S5_NONE),
    R("Ecozon",
      paste0("Agro-ecological zone, with Coastal Savannah as the base category ",
             "(Tables S3 and S4). Entered as a factor."),
      "Frontier intercept shifter; also an inefficiency determinant and an exact-match stratum",
      .S5_NONE),

    # ---- inefficiency / technology-gap determinants ----------------------
    H("Inefficiency and technology-gap determinants (w in Equations (1) and (3))"),
    R("lnAgeYr",
      paste0("Operator's age in years, logged (Table 1, \"Age (years)\"; ",
             "Table S4, \"Age (years)\"). Experience and accumulated ",
             "managerial skill pull one way and slower adoption of new ",
             "practice the other, so no expectation is posited."),
      "Inefficiency determinant", .S5_NONE),
    R("lnYerEdu",
      paste0("Operator's completed years of schooling, logged (Table 1, ",
             "\"Education (years)\"). Section 4.1 treats schooling as the ",
             "human capital complementary to the technology in use."),
      "Inefficiency determinant", .S5_NEG),
    R("CrpMix",
      paste0("Crop diversification index (Table 1, ",
             "\"Crop diversification (index)\"). Diversification spreads ",
             "managerial attention and buffers risk at the same time, so no ",
             "expectation is posited."),
      "Inefficiency determinant", .S5_NONE),
    R("Female",
      paste0("Operator is a woman (Table 1, \"Female farmer (dummy)\"). ",
             "Sections 4.1 and 5.6 anticipate a disadvantage in access to ",
             "technology rather than in management, so the expectation attaches ",
             "to the metafrontier technology gap and not to the group ",
             "frontiers; no sign is posited for inefficiency."),
      "Inefficiency determinant", .S5_NONE),
    R("OwnLnd",
      paste0("Household owns the land it farms (Table 1, ",
             "\"Land owned (dummy)\"). Secure tenure supports the investment ",
             "and husbandry that keep a farm near its own frontier."),
      "Inefficiency determinant; also a matching covariate", .S5_NEG),
    R("EqipMech",
      paste0("Mechanized equipment used on the farm (Table 1, ",
             "\"Mechanization (dummy)\")."),
      "Inefficiency determinant", .S5_NEG),
    R("Extension",
      paste0("Operator received extension advice (Table S4, ",
             "\"Extension (dummy)\"). Advisory contact is the efficiency-",
             "raising service Section 4.1 names in the bundling argument."),
      "Inefficiency determinant", .S5_NEG),
    R("Survey ",
      "Survey round, entered as a factor (see the frontier-shifter row above).",
      "Inefficiency determinant", .S5_NONE),
    R("Ecozon ",
      paste0("Agro-ecological zone, base Coastal Savannah, entered as a factor ",
             "(Table S4)."),
      "Inefficiency determinant", .S5_NONE),

    # ---- exact-match strata ---------------------------------------------
    H("Exact-match strata (pairs are formed only within a cell)"),
    R("Survey  ",
      "Survey round, so that no user is paired with a non-user from the other round.",
      "Exact-match stratum", .S5_NA),
    R("Region",
      "Administrative region, as coded over the study period.",
      "Exact-match stratum", .S5_NA),
    R("Ecozon  ",
      "Agro-ecological zone.",
      "Exact-match stratum", .S5_NA),
    R("Locality",
      "Rural or urban locality.",
      "Exact-match stratum", .S5_NA),
    R("Female ",
      "Operator gender.",
      "Exact-match stratum", .S5_NA),

    # ---- matching covariates, continuous --------------------------------
    H("Matching covariates entering the distance, continuous"),
    R("AgeYr",
      "Operator's age in years, unlogged in the matching distance.",
      "Matching covariate", .S5_NA),
    R("YerEdu",
      "Operator's completed years of schooling.",
      "Matching covariate", .S5_NA),
    R("HHSizeAE",
      "Household size in adult equivalents (Table 1, \"Size (AE)\").",
      "Matching covariate", .S5_NA),
    R("FmleAERt",
      "Female share of the household's adult equivalents.",
      "Matching covariate", .S5_NA),
    R("Depend",
      "Dependency ratio (Table 1, \"Dependency (ratio)\").",
      "Matching covariate", .S5_NA),
    R("CrpMix ",
      "Crop diversification index (see the inefficiency block above).",
      "Matching covariate", .S5_NA),
    R("Area_<crop> ",
      paste0("Share of cultivated area under each crop, the list discovered ",
             "from the data by get_crop_area_list()."),
      "Matching covariate", .S5_NA),
    R("FinIdxSi",
      paste0("Financial-inclusion index: the first principal component of the ",
             "financial-service indicators listed in Table S6, fitted within ",
             "survey-round-by-locality strata, rescaled onto the pooled ",
             "component and min-max scaled to the unit interval. The indicator ",
             "count is not repeated here because it differs by round; Table S6 ",
             "prints it (Note S1)."),
      "Matching covariate", .S5_NA),

    # ---- matching covariates, categorical --------------------------------
    H("Matching covariates entering the distance, categorical"),
    R("OwnLnd ",
      "Household owns the land it farms (see the inefficiency block above).",
      "Matching covariate", .S5_NA),
    R("Ethnic",
      "Ethnicity of the operator.",
      "Matching covariate", .S5_NA),
    R("Marital",
      "Marital status of the operator.",
      "Matching covariate", .S5_NA),
    R("Religion",
      "Religion of the operator.",
      "Matching covariate", .S5_NA),
    R("Head",
      "Operator's relationship to the household head.",
      "Matching covariate", .S5_NA))
})
# TRAILING-SPACE LABELS. Survey, Ecozon, OwnLnd, CrpMix, Female and the crop
# shares each hold two roles and so appear in two blocks. flextable prints the
# trailing space invisibly, and tbl_num()'s block= argument disambiguates the
# duplicates it would otherwise stop on -- but nothing in this table is a
# number, and per the Round-2 brief no prose calls tbl_num() on tableS5. If a
# future call site needs one of these rows, pass block= and use the label
# exactly as written here, trailing space included.

.tblS5_live <- function() .memo("tblS5", function() {
  d <- .S5_ROWS
  # Cheap structural checks: this table is hand-maintained, so the failure mode
  # is an editing slip (a dropped cell, a block header left with a sign) rather
  # than a keying failure.
  if (any(d$header == "0" & !nzchar(d$c3)))
    stop("exhibit_helpers_tables.R: Table S5 has a row with no expected-sign ",
         "cell. Every row takes a plus, a minus, an em dash (no expectation ",
         "posited) or 'Not applicable'; a blank reads as an oversight.",
         call. = FALSE)
  bad <- setdiff(d$c3[d$header == "0"],
                 c(.S5_POS, .S5_NEG, .S5_NONE, .S5_NA))
  if (length(bad))
    stop("exhibit_helpers_tables.R: Table S5 carries expected-sign values ",
         "outside the permitted set: ", paste(bad, collapse = " | "),
         call. = FALSE)
  .guard_filled(d, "Table S5")
  d
})

ft_tableS5 <- function() {
  ft <- .ft_build(.tblS5_live(),
    c("Definition and measurement", "Role in the analysis", "Expected sign"),
    first_lab = "Variable", size = 7,
    notes = c(
      paste0("The table states the specification rather than any estimate. ",
             "Frontier, shifter and inefficiency rows are the arguments passed ",
             "in 004_MSF_financial_inclusion_study.R; matching rows are the ",
             "covariate lists of 002_MATCHING_financial_inclusion_study.R. The ",
             "corresponding estimates are in Tables 4, S3 and S4."),
      paste0("Expected signs are the a priori expectations of Section 4.1, not ",
             "findings. An em dash marks a variable for which the paper posits ",
             "no expectation; \"Not applicable\" marks a variable that enters ",
             "an exact-match stratum or a matching distance rather than a ",
             "regression, so that no coefficient sign exists to expect. The ",
             "criterion for the matching covariates is balance between users ",
             "and non-users, reported for each of them in Fig. S1."),
      paste0("The inefficiency covariates scale the variance of the one-sided ",
             "error term, so a positive coefficient means greater inefficiency ",
             "and, in Equation (3), a larger technology gap; a negative ",
             "expected sign therefore means the variable is expected to place ",
             "the farm closer to its frontier. 004_MSF passes the same vector ",
             "as inefficiency_covariates and as adoption_covariates, so each ",
             "row in that block governs both margins."),
      paste0("Positive signs on the six inputs follow from monotonicity of the ",
             "production function and are not a prediction about credit. H3 of ",
             "Section 4.1 is a prediction about a difference of elasticities ",
             "between groups -- credit users' frontier more responsive to land ",
             "and labour, and not more responsive to fertilizer -- which no ",
             "single row can carry."),
      paste0("004_MSF passes the survey round as a factor in both the frontier ",
             "shifters and the inefficiency determinants, but Tables S3 and S4 ",
             "print no round coefficient; see the note to those tables."),
      paste0("The treatment, credit_hh, is not listed because it is a ",
             "regressor in none of the three specifications: it partitions the ",
             "sample into the groups whose frontiers are compared (Section 4.2) ",
             "and supplies the flag on which pairs are drawn (Section 4.3). It ",
             "is documented in Table S0. Pairing further requires complete ",
             "information on the identifiers, the survey weight and the ",
             "treatment flag, and an observation missing any matching field is ",
             "dropped before pairs are formed."),
      .SRC_NOTE))
  # .ft_build right-aligns every value column, which is right for estimates and
  # wrong for three columns of prose. Fixed widths (inches, portrait) so the
  # definition column gets the room; adjust here rather than in .ft_build,
  # which every other table depends on.
  ft <- align(ft, j = 2:4, align = "left", part = "all")
  ft <- valign(ft, valign = "top", part = "body")
  ft <- width(ft, j = 1, width = 0.85)
  ft <- width(ft, j = 2, width = 3.05)
  ft <- width(ft, j = 3, width = 1.85)
  ft <- width(ft, j = 4, width = 0.75)
  ft <- set_table_properties(ft, layout = "fixed")
  ft
}

# ==============================================================================
# Table S6 -- first-component loadings for the financial-inclusion index
# ==============================================================================
# SOURCE. financial_inclusion_index_diagnostics.dta, written by the postfile
# block of 000_INDEX_financial_inclusion_study.do: one row per stratum x
# indicator, carrying the first-component loading, the stratum N and the share
# of variance the component explains. Columns, as posted:
#
#   round str8    "POOLED" | "GLSS6" | "GLSS7"
#   locality str8 "POOLED" | "Rural" | "Urban"
#   indicator str32
#   loading double   first-component loading, AS ESTIMATED IN THAT STRATUM
#   N long           stratum N
#   rho double       share of variance explained by the component
#
# PATH. Repo-root-relative, unlike every other input this file reads: the
# do-file writes into data-raw/releases/harmonized_data at the repository root,
# while .STUDY_ROOT points at studies/financial_inclusion and the knit runs from
# narrative/. Search the candidates rather than pinning one, and name all of
# them if the file is absent.
.S6_REL <- file.path("data-raw", "releases", "harmonized_data")
.S6_DTA <- "financial_inclusion_index_diagnostics.dta"
# cwd = repo root -> "."; cwd = study root -> ".." or "../.."; cwd = narrative/
# (the knit) -> "../../..". .STUDY_ROOT is carried too, for a checkout that
# vendors the release under the study.
.S6_CANDIDATES <- unique(file.path(
  c(".", "..", file.path("..", ".."), file.path("..", "..", ".."), .STUDY_ROOT),
  .S6_REL, .S6_DTA))

.s6_path <- function() {
  hit <- .S6_CANDIDATES[file.exists(.S6_CANDIDATES)]
  if (!length(hit))
    stop("exhibit_helpers_tables.R: Table S6 cannot find ", .S6_DTA, ".\n",
         "  Looked in:\n    ", paste(.S6_CANDIDATES, collapse = "\n    "),
         "\n  That release is written by the DIAGNOSTICS block of ",
         "scripts/000_INDEX_financial_inclusion_study.do. Run it once from ",
         "Stata, then re-render. No stored copy is substituted.", call. = FALSE)
  hit[1]
}

# Display labels. YerEdu / Banked / FinWorker / HHFinWorker are spelled out;
# the ten Insured_* take the type word Table 2 prints for the same variable --
# those are the variable labels carried in the .dta, read out rather than
# guessed. An indicator with no entry here stops the build: a mislabelled row
# in a table about what the index measures is worse than a failed render.
.S6_LABS <- c(
  YerEdu          = "Years of schooling",
  Banked          = "Account ownership or scheme contribution",
  FinWorker       = "Works in the financial sector",
  HHFinWorker     = "Household member works in the financial sector",
  Insured_Health  = "Health insurance",
  Insured_Life    = "Life insurance",
  Insured_Car     = "Vehicle insurance",
  Insured_Pension = "Pension insurance",
  Insured_Invest  = "Investment insurance",
  Insured_Death   = "Death insurance",
  Insured_Edu     = "Education insurance",
  Insured_Asset   = "Asset insurance",
  Insured_Buss    = "Business insurance",
  Insured_Travel  = "Travel insurance")

# Column order: the pooled fit that supplies the reference metric, then the
# four strata in the order 000_INDEX loops over them.
.S6_STRATA <- list(
  list(round = "POOLED", locality = "POOLED", head = "Pooled"),
  list(round = "GLSS6",  locality = "Rural",  head = "GLSS6 Rural"),
  list(round = "GLSS6",  locality = "Urban",  head = "GLSS6 Urban"),
  list(round = "GLSS7",  locality = "Rural",  head = "GLSS7 Rural"),
  list(round = "GLSS7",  locality = "Urban",  head = "GLSS7 Urban"))

.s6_read <- function() .memo("index_diag", function() {
  if (!requireNamespace("haven", quietly = TRUE))
    stop("exhibit_helpers_tables.R: Table S6 needs 'haven' to read ", .S6_DTA,
         ".\n  install.packages(\"haven\")", call. = FALSE)
  p <- .s6_path()
  d <- as.data.frame(haven::read_dta(p))
  need <- c("round", "locality", "indicator", "loading", "N", "rho")
  miss <- setdiff(need, names(d))
  if (length(miss))
    stop("exhibit_helpers_tables.R: ", p, " lacks the column(s) ",
         paste(miss, collapse = ", "), ". Columns present: ",
         paste(names(d), collapse = ", "),
         ".\n  These names are set by the postfile statement in ",
         "000_INDEX_financial_inclusion_study.do; if that changed, change ",
         "them here too.", call. = FALSE)
  d <- d[, need]
  for (cc in c("round", "locality", "indicator"))
    d[[cc]] <- trimws(as.character(d[[cc]]))
  for (cc in c("loading", "N", "rho")) d[[cc]] <- as.numeric(d[[cc]])
  # One loading per stratum x indicator. A duplicate means the do-file posted a
  # stratum twice; taking the first would print a plausible wrong number.
  # A missing loading is not the same thing as a screened-out indicator: the
  # screen leaves NO ROW at all, which is what the em dash reports. An NA in a
  # row that exists means the pca returned something unusable, and sprintf would
  # print it as "NA" beside real loadings.
  if (anyNA(d$loading) || anyNA(d$N) || anyNA(d$rho))
    stop("exhibit_helpers_tables.R: ", p, " carries missing loading/N/rho ",
         "values. A screened-out indicator has no row at all; an NA in a row ",
         "that exists means the component did not estimate. Re-run ",
         "000_INDEX_financial_inclusion_study.do and read its pca output.",
         call. = FALSE)
  key <- paste(d$round, d$locality, d$indicator, sep = "|")
  if (anyDuplicated(key))
    stop("exhibit_helpers_tables.R: ", p, " has more than one row for ",
         paste(unique(key[duplicated(key)]), collapse = ", "),
         "; expected one loading per stratum and indicator.", call. = FALSE)
  d
})

.tblS6_live <- function() .memo("tblS6", function() {
  d <- .s6_read()
  slices <- lapply(.S6_STRATA, function(s)
    d[!is.na(d$round) & !is.na(d$locality) &
      d$round == s$round & d$locality == s$locality, , drop = FALSE])
  gone <- vapply(slices, nrow, integer(1)) == 0L
  if (any(gone))
    stop("exhibit_helpers_tables.R: Table S6 found no rows for ",
         paste(vapply(.S6_STRATA[gone], `[[`, character(1), "head"),
               collapse = ", "),
         ". Strata present in the release: ",
         paste(unique(paste(d$round, d$locality)), collapse = " | "),
         call. = FALSE)

  # Row order is the POOLED loading, descending -- the ordering Note S1 reads
  # off, and the reason the pooled column comes first.
  pooled <- slices[[1]]
  ind <- pooled$indicator[order(-pooled$loading)]

  extra <- setdiff(unique(d$indicator), ind)
  if (length(extra))
    stop("exhibit_helpers_tables.R: Table S6 found indicator(s) in a stratum ",
         "but not in the pooled fit: ", paste(extra, collapse = ", "),
         ". The row order is the pooled loading, so such a row has no place ",
         "to sit. Check the pooled pca in 000_INDEX.", call. = FALSE)
  unlabelled <- setdiff(ind, names(.S6_LABS))
  if (length(unlabelled))
    stop("exhibit_helpers_tables.R: Table S6 has no display label for ",
         paste(unlabelled, collapse = ", "),
         ". Add it to .S6_LABS -- and say so in Note S1, which enumerates the ",
         "indicator set.", call. = FALSE)

  rows <- list()
  blank <- rep("", length(.S6_STRATA))
  add <- function(label, header, cells) {
    if (length(cells) != length(.S6_STRATA))
      stop("exhibit_helpers_tables.R: Table S6 row '", label, "' has ",
           length(cells), " cells; expected ", length(.S6_STRATA), ".",
           call. = FALSE)
    rows[[length(rows) + 1L]] <<- data.frame(
      c(list(label = label, header = header),
        stats::setNames(as.list(cells), paste0("c", seq_along(cells)))),
      stringsAsFactors = FALSE)
  }

  # Which indicators are missing from at least one stratum is carried out to
  # the footnote rather than typed there: on the current release it is travel
  # insurance in the two GLSS7 strata, but a note that says so in prose would
  # go on saying it after the release changed.
  absent <- character(0)
  for (v in ind) {
    cells <- vapply(slices, function(s) {
      x <- s$loading[s$indicator == v]
      # An indicator with no variation in a stratum is screened out by 000_INDEX
      # before the pca and has no loading there. That is an ABSENT loading, not
      # a zero one, and printing zero would say the indicator was measured and
      # carried no weight. Note S1 discusses the case.
      if (!length(x)) "\u2014" else sprintf("%.3f", x)
    }, character(1))
    if (any(cells == "\u2014")) absent <- c(absent, unname(.S6_LABS[[v]]))
    add(unname(.S6_LABS[[v]]), "0", cells)
  }

  # Footer rows. N and rho are stratum-level, posted on every row of the
  # stratum, so they must be constant within it; if they are not, the file is
  # not what this builder thinks it is.
  one <- function(s, col, what) {
    u <- unique(s[[col]])
    if (length(u) != 1)
      stop("exhibit_helpers_tables.R: Table S6 found ", length(u), " values of ",
           what, " within one stratum (", paste(u, collapse = ", "),
           "); expected one.", call. = FALSE)
    u
  }
  k <- vapply(slices, nrow, integer(1))
  add("Component diagnostics", "1", blank)
  add("Indicators entering the component", "0", as.character(k))
  add("Observations", "0",
      vapply(slices, function(s) .fmt_n(one(s, "N", "N")), character(1)))
  add("Share of variance explained", "0",
      vapply(slices, function(s) sprintf("%.3f", one(s, "rho", "rho")),
             character(1)))

  out <- do.call(rbind, rows)
  out$header <- as.character(out$header)
  out <- out[, c("label", "header", paste0("c", seq_along(.S6_STRATA)))]
  .guard_filled(out, "Table S6")   # shared backstop; its hint names the
                                   # descriptive cache, which is not this
                                   # table's input -- the checks above are the
                                   # ones that will actually fire here.
  attr(out, "indicators") <-
    stats::setNames(k, vapply(.S6_STRATA, `[[`, character(1), "head"))
  attr(out, "absent") <- unique(absent)
  out
})

ft_tableS6 <- function() {
  d <- .tblS6_live()
  k <- attr(d, "indicators")
  # Indicator counts by round, and the indicators screened out of some stratum:
  # both read off the build, so the note cannot drift from the table above it.
  ab <- attr(d, "absent")
  kr <- function(a, b) if (identical(k[[a]], k[[b]])) as.character(k[[a]]) else
    paste0(k[[a]], " (rural) and ", k[[b]], " (urban)")
  n6 <- kr("GLSS6 Rural", "GLSS6 Urban")
  n7 <- kr("GLSS7 Rural", "GLSS7 Urban")
  ft <- .ft_build(d,
    vapply(.S6_STRATA, `[[`, character(1), "head"),
    first_lab = "Indicator", size = 8,
    spanner = c("", "Pooled", "Survey round and locality"),
    spanwidths = c(1, 1, length(.S6_STRATA) - 1),
    notes = c(
      paste0("Entries are loadings on the first principal component, estimated ",
             "over the pooled sample and separately within each survey-round-",
             "by-locality stratum by 000_INDEX_financial_inclusion_study.do. ",
             "GLSS6 is the 2012/13 round and GLSS7 the 2016/17 round."),
      paste0("Rows are ordered by the pooled loading, largest first. An em dash ",
             "marks an indicator that does not enter a stratum's component: it ",
             "has no variation in that stratum and is screened out before ",
             "estimation, so the loading is absent rather than zero",
             if (length(ab))
               paste0(", as with ", paste(tolower(ab), collapse = ", ")) else "",
             ". The component rests on ", n6, " indicators in 2012/13 and ",
             n7, " in 2016/17",
             if (!identical(n6, n7))
               ", so its content is not identical across the two rounds"
             else "", "."),
      paste0("Loadings are as estimated within each stratum, before the stratum ",
             "scores are placed on the pooled component's metric. The sign and ",
             "scale of a principal component are arbitrary, so a sign pattern ",
             "is informative within a column and not across columns; in the ",
             "pooled fit every loading is positive, so a higher index score ",
             "means greater financial inclusion, and the rescaling carries that ",
             "orientation into every stratum (Note S1)."),
      paste0("The share of variance explained is the proportion of the total ",
             "variance of the indicator set carried by the first component. ",
             "With ", k[["Pooled"]], " indicators, a component carrying no ",
             "more than an equal share would account for about ",
             formatC(1 / k[["Pooled"]], format = "f", digits = 3), "."),
      paste0("Observations are household-member records. The index is estimated ",
             "on more records than the analysis sample of farm operators uses; ",
             "the matching draws the values it needs (Note S1)."),
      .SRC_NOTE))
  ft
}

# ==============================================================================
# Registration
# ==============================================================================
# .live_table() is what tbl_num()/tbl_pct()/tbl_diff() route through, and its
# own comment is right that a table left out of the switch is a section of the
# paper quietly citing a frozen value. This block cannot edit that switch --
# it is appended after it -- so it wraps it, delegating every id it does not
# add. Fold the two ids into the switch by hand and delete this wrapper the
# next time that function is touched; behaviour is identical either way.
#
# The guard makes re-evaluating this block idempotent: without it, sourcing the
# appended tail twice would wrap the wrapper.
if (!isTRUE(attr(.live_table, "round2_S5_S6"))) {
  .live_table_base <- .live_table
  .live_table <- function(id) {
    switch(id,
      "tableS5" = .tblS5_live(),
      "tableS6" = .tblS6_live(),
      .live_table_base(id))
  }
  attr(.live_table, "round2_S5_S6") <- TRUE
}
