# 305_tables.R
# Build manuscript tables as flextable objects, straight from the estimation
# objects, so the tables and the in-text numbers share one source.
#
# Renders natively in both .docx (real Word table) and .html.
# Requires: flextable (add to DESCRIPTION Suggests).
#
# Keying reminder (see 301): the extraction group is TCHLvel
#   "0" = No extraction, "1" = Some extraction, "National", "Meta".
# Table 4 uses the MATCHED sample (opt_sample).
# NB: don't use requireNamespace(quietly = TRUE) here -- it returns FALSE for ANY
# load failure and hides the reason, which turns a broken dependency (e.g. a
# half-installed systemfonts/gdtools) into a misleading "please install" message.
.ft_ok <- tryCatch({ loadNamespace("flextable"); TRUE },
                   error = function(e) conditionMessage(e))
if (!isTRUE(.ft_ok))
  stop("305_tables.R could not load 'flextable'.\n",
       "  Reason: ", .ft_ok, "\n",
       "  If the package is missing:  install.packages(\"flextable\")\n",
       "  If a dependency is broken (systemfonts / gdtools / textshaping), restart R\n",
       "  in a clean session and run:\n",
       "    install.packages(c(\"systemfonts\",\"textshaping\",\"gdtools\",\"flextable\"))",
       call. = FALSE)
suppressPackageStartupMessages(library(flextable))

# Manuscript font: Times New Roman for every table (docx + html), matching the
# document body (reference.docx theme + css/tables.css).
set_flextable_defaults(font.family = "Times New Roman")

# Self-contained path resolution: this file is sourced BOTH from the repo root
# (via run_article.R) and from narrative/ (the Rmd's knit_root_dir), so it must
# not rely on 300_article_helpers.R's repo-root-relative paths.
.STUDY_ROOT <- if (dir.exists("output/estimations")) {
  "."                              # sourced from the study directory
} else if (dir.exists("../output/estimations")) {
  ".."                             # sourced from narrative/ (the knit_root_dir)
} else {
  "studies/resource_extraction"    # sourced from the repo root
}

.EST <- file.path(.STUDY_ROOT, "output", "estimations")
.read_est <- function(tag)
  readRDS(file.path(.EST, sprintf("CropID_Pooled_%s_TL_hnormal_optimal.rds", tag)))

.se_path <- file.path(.STUDY_ROOT, "data", "resource_extraction_study_environment.rds")
.mspecs  <- if (file.exists(.se_path)) readRDS(.se_path)$match_specification_optimal else NULL
.opt     <- if (!is.null(.mspecs)) ifelse(is.na(.mspecs$link), .mspecs$distance, .mspecs$link) else NA

.stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}
# Reproduce the published cell format: 0.819*** (0.011)
.cell <- function(est, se, p) {
  if (length(est) == 0 || is.na(est)) return("-")
  sprintf("%.3f%s (%.3f)", est, .stars(p), se)
}

# One activity x metric -> c(No extraction, Some extraction, Difference)
.row_for <- function(tag, metric) {
  ef <- .read_est(tag)$ef_mean
  b  <- ef[ef$estType %in% "teBC" & ef$stat %in% "wmean" &
           ef$Survey  %in% "GLSS0" & ef$restrict %in% "Restricted" &
           ef$type    %in% metric, ]
  if (!is.na(.opt) && "sample" %in% names(b)) b <- b[b$sample %in% .opt, ]
  lv <- b[b$CoefName %in% "efficiency", ]
  # NB: take the STORED gap (it carries its own SE and p-value) rather than
  # differencing the levels, so the stars match the published table.
  gp <- b[b$CoefName %in% "efficiencyGap_lvl", ]
  g  <- as.character(lv$TCHLvel)
  n  <- lv[g %in% "0", ]; a <- lv[g %in% "1", ]
  c(.cell(n$Estimate[1], n$Estimate.sd[1], n$jack_pv[1]),
    .cell(a$Estimate[1], a$Estimate.sd[1], a$jack_pv[1]),
    .cell(gp$Estimate[1], gp$Estimate.sd[1], gp$jack_pv[1]))
}

.ACT <- c("Any type of resource extraction"        = "extraction_any",
          "Any scale of mineral mining"            = "mining_any",
          "Informal or small-scale mineral mining" = "mining_gala",
          "Quarrying"                              = "quarrying",
          "Sand winning"                           = "sand")
.BLOCKS <- c("Technology gap ratio (TGR)"              = "TGR",
             "Pure farmer technical efficiency (TE)"   = "TE",
             "Meta-frontier technical efficiency (MTE)" = "MTE")

ft_table4 <- function() {
  rows <- do.call(rbind, lapply(names(.BLOCKS), function(bl) {
    body <- do.call(rbind, lapply(names(.ACT), function(a) {
      v <- .row_for(.ACT[[a]], .BLOCKS[[bl]])
      data.frame(item = a, A = v[1], B = v[2], D = v[3], grp = bl,
                 stringsAsFactors = FALSE)
    }))
    rbind(data.frame(item = bl, A = "", B = "", D = "", grp = bl,
                     stringsAsFactors = FALSE), body)   # block header row
  }))
  hdr <- which(rows$item %in% names(.BLOCKS))
  rows$grp <- NULL

  ft <- flextable(rows)
  ft <- set_header_labels(ft, item = "", A = "No extraction",
                          B = "Some extraction", D = "Difference")
  ft <- add_header_row(ft, values = c("", "[A]", "[B]", "[B-A]"), top = FALSE)
  ft <- bold(ft, i = hdr, j = 1, part = "body")      # TGR / TE / MTE block rows
  ft <- bold(ft, part = "header")
  ft <- align(ft, j = 2:4, align = "right", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  ft <- fontsize(ft, size = 9, part = "all")
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft <- add_footer_lines(ft, values = c(
    "Significance levels: * p<0.10, ** p<0.05, *** p<0.01. Jackknife standard errors in parentheses.",
    "Meta Stochastic Frontier Analysis jointly performed on Ghana Living Standards Survey [waves 4-7]."))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

# ---- Table 3 ----------------------------------------------------------------
# Column key, decoded from the results workbook's Table3 sheet formulas
# (=VLOOKUP(CONCAT($F:$K, col$1:$4), msf!$Q:$U, $C, 0)):
#
#   column      sample      TCHLvel    level_type
#   naive       unmatched   National   level
#   none  [A]   unmatched   0          level
#   any   [B]   unmatched   1          level
#   gap  [B-A]  MATCHED     1          Gap_lvl     <- NB: different sample from [A]/[B]
#   meta_m      matched     Meta       level
#   meta_u      unmatched   Meta       level
#
# The efficiency rows (TGR/TE/MTE) key differently: the workbook takes their
# sample from column L (the Matched/Unmatched row label) and only restrict/
# level_type/Tech from the column, which is why each metric has two rows.
#
# INPUT LABELS: el5 = fertilizer, el6 = pesticide. This is the positional map
# input_variables = c("Area","SeedKg","HHLaborAE","HirdHr","FertKg","PestLt")
# -> stochastic_frontier-core.R:1103  data[, paste0("I", i)] <- data[, input_variables[i]]
# The workbook's Table3 sheet paired el6="Fertilizer" / el5="Pesticide" by hand,
# under the older I5=PestLt / I6=FertKg convention (see the commented-out lines
# at helpers_tech_inefficiency.R:40-41). That pairing is stale, so the published
# Table 3 has these two rows transposed. This function uses the code's mapping.

.samp_id <- function(s) if (identical(s, "matched")) .opt else "unmatched"

.T3COLS <- list(
  naive  = list(samp = "unmatched", lv = "National", lt = "level"),
  none   = list(samp = "unmatched", lv = "0",        lt = "level"),
  any    = list(samp = "unmatched", lv = "1",        lt = "level"),
  gap    = list(samp = "matched",   lv = "1",        lt = "Gap_lvl"),
  meta_m = list(samp = "matched",   lv = "Meta",     lt = "level"),
  meta_u = list(samp = "unmatched", lv = "Meta",     lt = "level")
)

# Formatters mirroring the workbook's $C column selector
.f_es <- function(e, se, p) if (length(e) == 0 || is.na(e)) "-" else
  sprintf("%.3f%s (%.3f)", e, .stars(p), se)
.f_st <- function(e, p) if (length(e) == 0 || is.na(e)) "-" else
  sprintf("%.3f%s", e, .stars(p))
.f_nm <- function(e, d = 0) if (length(e) == 0 || is.na(e)) "-" else
  formatC(e, format = "f", digits = d)

# Resolve the "<stub>Gap_lvl" CoefName actually present, rather than assuming it
.gap_coef <- function(df, stub) {
  cn <- unique(as.character(df$CoefName))
  hit <- cn[grepl("Gap_lvl$", cn) & grepl(stub, cn, ignore.case = TRUE)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# Elasticity cell (el_mean). code = el1..el7
.t3_el <- function(el, code, cc) {
  want <- if (identical(cc$lt, "level")) "elasticity" else .gap_coef(el, "elasticit")
  if (is.na(want)) return("-")
  b <- el[el$input %in% code & el$CoefName %in% want &
          as.character(el$TCHLvel) %in% cc$lv &
          el$sample %in% .samp_id(cc$samp), ]
  .f_es(b$Estimate[1], b$Estimate.sd[1], b$jack_pv[1])
}

# Efficiency cell (ef_mean). metric = TGR/TE/MTE; samp is the row's Matched/Unmatched
.t3_ef <- function(ef, metric, cc, samp) {
  want <- if (identical(cc$lt, "level")) "efficiency" else .gap_coef(ef, "efficienc")
  if (is.na(want)) return("-")
  b <- ef[ef$estType %in% "teBC" & ef$stat %in% "wmean" &
          ef$Survey %in% "GLSS0" & ef$restrict %in% "Restricted" &
          ef$type %in% metric & ef$CoefName %in% want &
          as.character(ef$TCHLvel) %in% cc$lv &
          ef$sample %in% .samp_id(samp), ]
  .f_es(b$Estimate[1], b$Estimate.sd[1], b$jack_pv[1])
}

# Diagnostic cell (sf_estm)
.t3_sf <- function(sf, coef, cc, how, survey = "GLSS0") {
  b <- sf[sf$CoefName %in% coef & sf$restrict %in% "Restricted" &
          sf$Survey %in% survey &
          as.character(sf$TCHLvel) %in% cc$lv &
          sf$sample %in% .samp_id(cc$samp), ]
  switch(how,
         es  = .f_es(b$Estimate[1], b$Estimate.sd[1], b$jack_pv[1]),
         st  = .f_st(b$Estimate[1], b$jack_pv[1]),
         n0  = .f_nm(b$Estimate[1], 0),
         n2  = .f_nm(b$Estimate[1], 2),
         n3  = .f_nm(b$Estimate[1], 3))
}

# Sample size = sum of the per-wave Nobs (the workbook does =O49+O50+O51+O52)
.t3_nobs <- function(sf, cc) {
  b <- sf[sf$CoefName %in% "Nobs" & sf$restrict %in% "Restricted" &
          sf$Survey %in% c("GLSS4", "GLSS5", "GLSS6", "GLSS7") &
          as.character(sf$TCHLvel) %in% cc$lv &
          sf$sample %in% .samp_id(cc$samp), ]
  if (nrow(b) == 0) return("-")
  .f_nm(sum(b$Estimate, na.rm = TRUE), 0)
}

# No. of parameters = nXvar + nuZUvar + nvZVvar (workbook: =O39+O40+O41)
.t3_npar <- function(sf, cc) {
  b <- sf[sf$CoefName %in% c("nXvar", "nuZUvar", "nvZVvar") &
          sf$restrict %in% "Restricted" & sf$Survey %in% "GLSS0" &
          as.character(sf$TCHLvel) %in% cc$lv &
          sf$sample %in% .samp_id(cc$samp), ]
  if (nrow(b) == 0) return("-")
  .f_nm(sum(b$Estimate, na.rm = TRUE), 0)
}

.T3_EL <- c("Land" = "el1", "Planting material" = "el2", "Family labor" = "el3",
            "Hired labor" = "el4", "Fertilizer" = "el5", "Pesticide" = "el6",
            "Returns to scale" = "el7")

ft_table3 <- function() {
  p  <- .read_est("extraction_any")
  el <- p$el_mean; ef <- p$ef_mean; sf <- p$sf_estm
  el <- el[el$stat %in% "wmean" & el$Survey %in% "GLSS0" &
           el$restrict %in% "Restricted", ]

  CO <- .T3COLS
  blank <- function(lab) c(lab, "", "", "", "", "", "")
  # a body row: label + one cell per column, via getter g(cc)
  mk <- function(lab, g) c(lab, vapply(CO, g, character(1)))

  rows <- list(blank("Elasticity"))
  for (nm in names(.T3_EL)) {
    code <- .T3_EL[[nm]]
    rows[[length(rows) + 1]] <- mk(nm, function(cc) .t3_el(el, code, cc))
  }

  rows[[length(rows) + 1]] <- blank("Technology/efficiency")
  EFB <- c("Technology gap ratio (TGR)" = "TGR",
           "Pure farmer technical efficiency (TE)" = "TE",
           "Meta-frontier technical efficiency (MTE)" = "MTE")
  for (nm in names(EFB)) {
    metric <- EFB[[nm]]
    rows[[length(rows) + 1]] <- blank(nm)
    for (sm in c("matched", "unmatched")) {
      lab <- if (sm == "matched") "Matched" else "Unmatched"
      # naive/meta columns are "-" for efficiency rows (workbook: literal "-")
      g <- function(cc) if (cc$lv %in% c("National", "Meta")) "-" else
        .t3_ef(ef, metric, cc, sm)
      rows[[length(rows) + 1]] <- mk(lab, g)
    }
  }

  rows[[length(rows) + 1]] <- blank("Model diagnostics")
  gap_dash <- function(f) function(cc) if (identical(cc$lt, "Gap_lvl")) "-" else f(cc)
  D <- list(
    list("Sample size",                      gap_dash(function(cc) .t3_nobs(sf, cc))),
    list("Monotonicity satisfaction rate",   gap_dash(function(cc) .t3_sf(sf, "mono", cc, "n2"))),
    list("Curvature satisfaction rate",      gap_dash(function(cc) .t3_sf(sf, "curv", cc, "n2"))),
    list("Schmidt & Lin (1984)",             gap_dash(function(cc) .t3_sf(sf, "olsSkew", cc, "st"))),
    list("Coelli (1995)",                    gap_dash(function(cc) .t3_sf(sf, "CoelliM3Test", cc, "st"))),
    list("Gutierrez (2001)",                 gap_dash(function(cc) .t3_sf(sf, "LRInef", cc, "st"))),
    list("Log likelihood",                   gap_dash(function(cc) .t3_sf(sf, "mlLoglik", cc, "n0"))),
    list("No. of parameters",                gap_dash(function(cc) .t3_npar(sf, cc))),
    # LR test only exists for the meta-frontier columns
    list("Meta frontier LR test",            function(cc) if (!identical(cc$lv, "Meta")) "-" else
                                               .t3_sf(sf, "LRT", cc, "n3")),
    list("Ratio variance due to inefficiency", gap_dash(function(cc) .t3_sf(sf, "Gamma", cc, "es")))
  )
  for (d in D) rows[[length(rows) + 1]] <- mk(d[[1]], d[[2]])

  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c("item", "naive", "none", "any", "gap", "meta_m", "meta_u")

  blocks <- c("Elasticity", "Technology/efficiency", "Model diagnostics")
  subhdr <- names(EFB)
  ib <- which(m$item %in% blocks)
  is <- which(m$item %in% subhdr)

  ft <- flextable(m)
  ft <- set_header_labels(ft, item = "", naive = "Naïve national frontier",
                          none = "No extraction", any = "Some extraction",
                          gap = "Difference", meta_m = "Matched", meta_u = "Unmatched")
  ft <- add_header_row(ft, top = TRUE,
                       values = c("", "", "Group frontier", "Meta-frontier"),
                       colwidths = c(1, 1, 3, 2))
  ft <- add_header_row(ft, top = FALSE,
                       values = c("", "", "[A]", "[B]", "[B-A]", "", ""))
  ft <- bold(ft, i = ib, j = 1, part = "body")
  ft <- italic(ft, i = is, j = 1, part = "body")
  ft <- padding(ft, i = c(is, setdiff(seq_len(nrow(m)), c(ib, is))), j = 1,
                padding.left = 12, part = "body")
  ft <- padding(ft, i = ib, j = 1, padding.left = 2, part = "body")
  ft <- bold(ft, part = "header")
  ft <- align(ft, j = 2:7, align = "right", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- align(ft, i = 1, align = "center", part = "header")
  ft <- padding(ft, padding.top = 0, padding.bottom = 0, part = "all")
  ft <- line_spacing(ft, space = 1, part = "all")
  ft <- fontsize(ft, size = 9, part = "all")
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft <- add_footer_lines(ft, values = c(
    "Significance levels: * p<0.10, ** p<0.05, *** p<0.01. Jackknife standard errors in parentheses.",
    "Schmidt & Lin (1984), Coelli (1995) and Gutierrez (2001) test the null of no one-sided error (no inefficiency).",
    "Meta Stochastic Frontier Analysis jointly performed on Ghana Living Standards Survey [waves 4-7].",
    "Standard errors estimated by jackknife over 100 resamples, each excluding one enumeration area per survey."))
  ft <- fontsize(ft, size = 6, part = "footer")
  ft
}

# ---- Descriptive tables (1, 2, A1, A2) --------------------------------------
# These come from the Stata path, not the estimation objects, so their values are
# extracted from output/resource_extraction_results.xlsx into data/tables/*.csv
# (labels curated: "Tomatoe"->"Tomato", "All crops (maize kg/ha)"->"real GH<cedi>/ha",
# "Seed (GHC/ha)"->"Seed (real GH<cedi>/ha)", section heads unbolded/renamed).
# Refresh them by re-running the extraction, not by editing the CSVs by hand.
# TODO Tier 2: rebuild these from the analysis dataset so they parametrize like 3/4.

.TBL_DIR <- file.path(.STUDY_ROOT, "data", "tables")

.read_tbl <- function(nm) {
  p <- file.path(.TBL_DIR, nm)
  if (!file.exists(p)) stop("305_tables.R: missing ", p, call. = FALSE)
  utils::read.csv(p, check.names = FALSE, colClasses = "character",
                  encoding = "UTF-8")
}

# The *_header.csv files are a single headerless line of column titles, so they
# must be read with header = FALSE -- otherwise read.csv() eats the only row as
# column names and hands back a zero-row frame.
.read_hdr <- function(nm) {
  p <- file.path(.TBL_DIR, nm)
  if (!file.exists(p)) stop("305_tables.R: missing ", p, call. = FALSE)
  h <- utils::read.csv(p, header = FALSE, check.names = FALSE,
                       colClasses = "character", encoding = "UTF-8")
  as.character(unlist(h, use.names = FALSE))
}

# Shared look for the descriptive tables: bold section rows, indent the rest.
.style_desc <- function(ft, hdr, ncol, size = 6) {
  ft <- bold(ft, i = hdr, j = 1, part = "body")
  body <- setdiff(seq_len(ncol), hdr)
  ft <- padding(ft, i = body, j = 1, padding.left = 8, part = "body")
  ft <- bold(ft, part = "header")
  ft <- align(ft, j = 1, align = "left", part = "all")
  # Title + table + notes must fit ONE page. The binding case is the landscape
  # pages: only 6.31in tall, and Tables 1/2 are 41 rows (A1/A2 are 39), leaving
  # ~0.145in per row. At that row count the vertical padding costs more than the
  # font -- each 1pt of padding adds ~0.06in over the whole table -- so it goes
  # to 0 and the font drops to 6pt.
  ft <- padding(ft, padding.top = 0, padding.bottom = 0, part = "all")
  ft <- line_spacing(ft, space = 1, part = "all")
  ft <- fontsize(ft, size = size, part = "all")
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft
}

ft_table1 <- function() {
  d <- .read_tbl("table1.csv")
  hdr <- which(d$header == "1")
  m <- d[, c("label","pooled_mean","none_mean","any_mean",
             "pooled_trend","none_trend","any_trend")]
  names(m) <- c("item","pm","nm","am","pt","nt","at")
  ft <- flextable(m)
  ft <- set_header_labels(ft, item = "Variable",
                          pm = "Pooled (n=26,811)", nm = "No extraction (n=23,957)",
                          am = "Some extraction (n=2,854)",
                          pt = "Pooled (n=26,811)", nt = "No extraction (n=23,957)",
                          at = "Some extraction (n=2,854)")
  ft <- add_header_row(ft, top = TRUE, values = c("", "Mean (SD)", "Trend (%)"),
                       colwidths = c(1, 3, 3))
  ft <- align(ft, j = 2:7, align = "right", part = "all")
  ft <- align(ft, i = 1, align = "center", part = "header")
  ft <- .style_desc(ft, hdr, nrow(m), size = 8)
  ft <- add_footer_lines(ft, values = c(
    "Significance levels: * p<0.10, ** p<0.05, *** p<0.01.",
    "Standard deviations in parentheses; standard errors in brackets. † denotes a statistically significant difference from the pooled sample.",
    "The trend was estimated as the annual percentage change via a generalised linear model.",
    "Data source: Ghana Living Standards Survey [waves 4-7]."))
  fontsize(ft, size = 6, part = "footer")
}

# Table 2 and A1/A2 share a shape: label + 7 value columns.
.ft_wide <- function(csv, hdr_csv, caption, notes, first_lab = "Variable",
                     size = 8) {
  d <- .read_tbl(csv)
  hdr <- which(d$header == "1")
  cols <- .read_hdr(hdr_csv)
  stopifnot(length(cols) == 7L)
  m <- d[, c("label", paste0("c", 1:7))]
  ft <- flextable(m)
  ft <- set_header_labels(ft, values = stats::setNames(
    as.list(c(first_lab, cols)), c("label", paste0("c", 1:7))))
  ft <- align(ft, j = 2:8, align = "right", part = "all")
  ft <- valign(ft, valign = "bottom", part = "header")
  ft <- .style_desc(ft, hdr, nrow(m), size = size)
  ft <- add_footer_lines(ft, values = notes)
  fontsize(ft, size = 6, part = "footer")
}

ft_table2 <- function()
  .ft_wide("table2.csv", "table2_header.csv",
    "Table 2. The prevalence of natural resource extraction in Ghanaian crop-producing communities",
    c("Standard deviations in parentheses; standard errors in brackets.",
      "Data source: Ghana Living Standards Survey [waves 4-7]."),
    first_lab = "Crop")

ft_tableA2 <- function()
  .ft_wide("tableA2.csv", "tableA2_header.csv", size = 7,
    "Table A2. Summary statistics of crop producers in Ghana by extraction activity",
    c("Standard deviations in parentheses.",
      "A dagger denotes a statistically significant difference from the pooled sample.",
      "Data source: Ghana Living Standards Survey [waves 4-7]."))

ft_tableA3 <- function()
  .ft_wide("tableA3.csv", "tableA3_header.csv", size = 7,
    "Table A3. Trends in the characteristics of crop producers in Ghana by extraction activity",
    c("Significance levels: * p<0.10, ** p<0.05, *** p<0.01. Standard errors in brackets.",
      "A dagger denotes a statistically significant difference from the pooled sample.",
      "Data source: Ghana Living Standards Survey [waves 4-7]."))

# ---- Appendix Tables A4-A9 --------------------------------------------------
# Extracted from the workbook's TableS3-S7 / TableS8 sheets into data/tables/.
# A4-A8 are one wave each (A8 = the GLSS0 average); columns are
# Naive | No extraction | Some extraction | Meta Matched | Meta Unmatched.
#
# The fertilizer/pesticide rows are RELABELLED relative to the sheet: it pairs
# "Fertilizer [lnI6]" / "Pesticide [lnI5]", but the code maps lnI5 = FertKg and
# lnI6 = PestLt, so the labels are swapped and the rows reordered to keep
# Fertilizer ahead of Pesticide. Values are untouched.
.ft_msf <- function(csv, caption, notes, size = 8) {
  d <- .read_tbl(csv)
  hdr <- which(d$header == "1")
  m <- d[, c("label", "naive", "none", "any", "meta_m", "meta_u")]
  ft <- flextable(m)
  ft <- set_header_labels(ft, label = "", naive = "Naïve national frontier",
                          none = "No extraction", any = "Some extraction",
                          meta_m = "Matched", meta_u = "Unmatched")
  ft <- add_header_row(ft, top = TRUE,
                       values = c("", "", "Group frontier", "Meta-frontier"),
                       colwidths = c(1, 1, 2, 2))
  ft <- align(ft, j = 2:6, align = "right", part = "all")
  ft <- align(ft, i = 1, align = "center", part = "header")
  ft <- valign(ft, valign = "bottom", part = "header")
  ft <- .style_desc(ft, hdr, nrow(m), size = size)
  ft <- add_footer_lines(ft, values = notes)
  fontsize(ft, size = 6, part = "footer")
}

.MSF_NOTE <- c(
  "Significance levels: * p<0.10, ** p<0.05, *** p<0.01. Jackknife standard errors in parentheses.",
  "Fertilizer is lnI5 and pesticide is lnI6, following input_variables order in 004_MSF_resource_extraction_study.R.")

ft_tableA4 <- function() .ft_msf("tableA4.csv",
  "Table A4. Meta stochastic frontier analysis results for Ghanaian crop producers for 1998/99",
  c(.MSF_NOTE, "Data source: Ghana Living Standards Survey [wave 4]."))
ft_tableA5 <- function() .ft_msf("tableA5.csv",
  "Table A5. Meta stochastic frontier analysis results for Ghanaian crop producers for 2005/06",
  c(.MSF_NOTE, "Data source: Ghana Living Standards Survey [wave 5]."))
ft_tableA6 <- function() .ft_msf("tableA6.csv",
  "Table A6. Meta stochastic frontier analysis results for Ghanaian crop producers for 2012/13",
  c(.MSF_NOTE, "Data source: Ghana Living Standards Survey [wave 6]."))
ft_tableA7 <- function() .ft_msf("tableA7.csv",
  "Table A7. Meta stochastic frontier analysis results for Ghanaian crop producers for 2016/17",
  c(.MSF_NOTE, "Data source: Ghana Living Standards Survey [wave 7]."))
ft_tableA8 <- function() .ft_msf("tableA8.csv",
  "Table A8. Average meta stochastic frontier analysis results for Ghanaian crop producers, 1998/99-2016/17",
  c(.MSF_NOTE, "Data source: Ghana Living Standards Survey [waves 4-7]."))
ft_tableA9 <- function() .ft_msf("tableA9.csv", size = 10,
  "Table A9. Determinants of crop production technical inefficiency and extraction-driven technology gaps in Ghana",
  c("Significance levels: * p<0.10, ** p<0.05, *** p<0.01. Jackknife standard errors in parentheses.",
    "A positive coefficient means the variable moves the farm away from its efficient frontier.",
    "Data source: Ghana Living Standards Survey [waves 4-7]."))

# ---- Table A1: construction of the extraction indicators --------------------
# Question-level mapping (wave x source variables x wording x options), mirroring
# the disability study's Table S1. Content sourced from data-raw/okwaayeli_DATA.do
# (lines 1488-1649) and the foundational GLSS community files' variable/value labels.
ft_tableA1 <- function() {
  d <- .read_tbl("tableA1.csv")
  ft <- flextable(d)
  ft <- set_header_labels(ft, round = "Survey round", source = "Source variables",
                          question = "Question", options = "Extractive response options",
                          mapping = "Mapping to analysis categories")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "left", part = "all")
  ft <- valign(ft, valign = "top", part = "body")
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  ft <- fontsize(ft, size = 8, part = "all")
  ft <- width(ft, j = 1, width = 1.0)
  ft <- width(ft, j = 2, width = 1.5)
  ft <- width(ft, j = 3, width = 1.7)
  ft <- width(ft, j = 4, width = 2.4)
  ft <- width(ft, j = 5, width = 2.4)
  ft <- add_footer_lines(ft, values = c(
    "In each community, the questionnaire is administered to up to 20 opinion leaders selected to reflect the community's occupational, ethnic, gender, and religious diversity.",
    "An enumeration area is classified into an extraction category if any listed variable records a corresponding extractive response; indicators are aggregated to the enumeration-area level (maximum).",
    "GLSS4 and GLSS5 record only undifferentiated mining, so commercial versus informal or small-scale mining is distinguished only in GLSS6 and GLSS7.",
    "Salt mining enters the aggregate extraction indicator but is too rare to support separate analysis (Table 2)."))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

# ---- Page sections ----------------------------------------------------------
# officedown's <!---BLOCK_LANDSCAPE_START---> markers emit type="oddPage", which
# makes Word start each landscape section on the next ODD page and inject a blank
# whenever one would land on an even page. These helpers use type="nextPage"
# instead, so a section starts on the very next page.
#
# NB: officer::page_size() defaults to LETTER and prop_section() defaults to 1in
# margins. reference.docx is US Letter with 1in margins, so both are passed explicitly
# -- omitting them silently reflows the whole document.
# The margins constructor is page_mar() (page_margins() does not exist).
.PAGE <- c(w = 8.5, h = 11)            # US Letter portrait, inches
                                       # (matches the submitted draft r2_AE_...docx:
                                       #  6.50in text portrait / 9.00in landscape)
.MAR <- function() officer::page_mar(top = 1, bottom = 1, left = 1,
                                     right = 1, header = 0.5,
                                     footer = 0.5, gutter = 0)
.sec <- function(orient) {
  if (!identical(knitr::opts_knit$get("rmarkdown.pandoc.to"), "docx"))
    return(invisible(NULL))            # inert for the html render
  officer::block_section(officer::prop_section(
    page_size    = officer::page_size(width = .PAGE[["w"]], height = .PAGE[["h"]],
                                      orient = orient),
    page_margins = .MAR(),
    type         = "nextPage"))
}
# A section's properties apply to the content BEFORE it, so:
#   sec_portrait()  closes the preceding portrait run, opening the landscape one
#   [exhibit]
#   sec_landscape() closes the landscape run
sec_portrait  <- function() .sec("portrait")
sec_landscape <- function() .sec("landscape")
