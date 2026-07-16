# 110_exhibit_tables.R
# Build manuscript tables as flextable objects. Architecture ported from
# studies/resource_extraction/scripts/110_exhibit_tables.R.
#
# Tables 3 and 4 build from the estimation objects (single source with the
# in-text numbers); until the keying is verified on a local run they FALL BACK
# to data/tables/table3.csv / table4.csv, which hold the published values
# extracted from the v001 draft. The fallback prints a message so a silent
# drift is impossible. Descriptive and supplementary tables read curated CSVs
# in data/tables/ (extracted from the v001 draft; refresh by re-running the
# extraction once the results workbook recalculates -- its display sheets
# currently cache #N/A values).
#
# Keying reminder (see 301): ownership group is TCHLvel
#   "0" = no ownership, "1" = some ownership, "National", "Meta".
# Table 4 uses the MATCHED sample (opt_sample).
.ft_ok <- tryCatch({ loadNamespace("flextable"); TRUE },
                   error = function(e) conditionMessage(e))
if (!isTRUE(.ft_ok))
  stop("110_exhibit_tables.R could not load 'flextable'.\n",
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
  "studies/land_tenure"
}

.EST <- file.path(.STUDY_ROOT, "output", "estimations")
.read_est <- function(tag)
  readRDS(file.path(.EST, sprintf("CropID_Pooled_%s_TL_hnormal_optimal.rds", tag)))

.se_path <- file.path(.STUDY_ROOT, "data", "land_tenure_study_environment.rds")
.mspecs  <- if (file.exists(.se_path)) readRDS(.se_path)$match_specification_optimal else NULL
.opt     <- if (!is.null(.mspecs)) ifelse(is.na(.mspecs$link), .mspecs$distance, .mspecs$link) else NA

.stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}
# Published cell format: 0.683*** (0.004)
.cell <- function(est, se, p) {
  if (length(est) == 0 || is.na(est)) return("-")
  sprintf("%.3f%s (%.3f)", est, .stars(p), se)
}

# ---- Curated CSVs -----------------------------------------------------------
.TBL_DIR <- file.path(.STUDY_ROOT, "data", "tables")

.read_tbl <- function(nm) {
  p <- file.path(.TBL_DIR, nm)
  if (!file.exists(p)) stop("110_exhibit_tables.R: missing ", p, call. = FALSE)
  utils::read.csv(p, check.names = FALSE, colClasses = "character",
                  encoding = "UTF-8")
}
.read_hdr <- function(nm) {
  p <- file.path(.TBL_DIR, nm)
  if (!file.exists(p)) stop("110_exhibit_tables.R: missing ", p, call. = FALSE)
  h <- utils::read.csv(p, header = FALSE, check.names = FALSE,
                       colClasses = "character", encoding = "UTF-8")
  as.character(unlist(h, use.names = FALSE))
}

# Shared look for the descriptive tables: bold section rows, indent the rest,
# zero vertical padding so title + table + notes fit one page.
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

# Generic builder: label + value columns from a data.frame (label, header, c1..cN)
# plus a character vector of column headers. Kept separate from .ft_csv so the
# same styling serves both the curated CSVs and the live workbook builders.
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

# Back-compat wrapper: curated-CSV path (still used by the S-tables).
.ft_csv <- function(csv, hdr_csv, ...) .ft_build(.read_tbl(csv), .read_hdr(hdr_csv), ...)

# ---- Live workbook layer ------------------------------------------------------
# Tables 1 and 2 are built from output/land_tenure_results.xlsx, written by
# scripts/100_exhibits.do. Two sheets are tidy VALUE sheets (export excel ...
# firstrow(variables)) and are the source of truth:
#
#   means        CropIDx, Equ, Coef, Beta, SE, Tv, Pv, Min, Max, SD, N   -> Table 1
#   land_tenure  Variable, crop, mesure, Beta, SE, Tv, Pv, Min, Max, SD, N -> Table 2
#
# The workbook also carries hand-built display sheets (Table1, Table2, ...) whose
# formulas cache #N/A. Those are NOT used. Only the first 11 columns of each tidy
# sheet are real; trailing columns are display-formula spill and are dropped.
#
# NB requires the 2026-07-15 fix to 100_exhibits.do (`mat roweq A= `Var'`). Before
# it, the OwnLnd0/OwnLnd1 rows were mis-tagged Equ=="Female", so Equ=="Female"
# carried two Mean_OwnLnd0 rows and a lookup could silently return the ownership
# share instead of the Female mean. .xl_pick() below errors on any such duplicate
# rather than guessing.
# The reading/lookup/formatting layer is packaged: see R/exhibits-workbook.R
#   read_exhibit_sheet()  exhibit_value()  exhibit_stars()
#   exhibit_group_sizes() exhibit_cell()
# Only the land-specific row maps live in this script.
# NB use .STUDY_ROOT, NOT paths from 300_article_helpers.R. Those are
# repo-root-relative, but knitr sets the working directory to narrative/ during
# the render, so they resolve to nothing there. .STUDY_ROOT is probed above for
# exactly this reason.
.DESC <- file.path(.STUDY_ROOT, "data", "descriptive_exhibits.rds")

# Tables 1, 2 and S1-S4 come from scripts/100_exhibit_descriptive_stats.R, which
# computes them in R from study_raw_data. Stata and Excel are out of this path:
# no land_tenure_results.xlsx, and none of the failure modes that came with it --
# the `mat roweq A = Female` collision, `cap{ }` writing collapsed fits as
# estimates, or `export excel ... sheetmodify` leaving stale rows behind.
#
# The engine is validated against both studies' Stata workbooks: ~15,000
# assertions across resource_extraction (7 treatments x 28 crops, both engines)
# and land_tenure (two families in one table, the wave_diff trend).
# See tests/testthat/test-descriptive-exhibits-*.R.
.desc <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      if (!file.exists(.DESC))
        stop("110_exhibit_tables.R: missing ", .DESC,
             "\n  Run: Rscript studies/land_tenure/scripts/100_exhibit_descriptive_stats.R",
             call. = FALSE)
      cache <<- readRDS(.DESC)
    }
    cache
  }
})

# Single value from the long frame. Errors on duplicates: a keyed schema should
# never produce two rows for one cell, and silently taking the first is how the
# v001 extraction survived the roweq collision -- by luck rather than by design.
.pick <- function(d, keys, col) {
  ok <- rep(TRUE, nrow(d))
  for (k in names(keys)) ok <- ok & !is.na(d[[k]]) & d[[k]] == keys[[k]]
  v <- d[[col]][ok]
  if (length(v) == 0) return(NA_real_)
  if (length(v) > 1)
    stop("110_exhibit_tables.R: ", length(v), " rows matched ",
         paste(sprintf("%s=%s", names(keys), unlist(keys)), collapse = ", "),
         "; expected 1.", call. = FALSE)
  as.numeric(v[1])
}
.stars <- okwaayeli::exhibit_stars

.SRC_NOTE <- "Data source: Ghana Living Standards Survey [waves 3-7]."
.SIG_NOTE <- "Significance levels: * p<0.10, ** p<0.05, *** p<0.01."

# ---- Main-text tables --------------------------------------------------------
# Table 1 row map: display label -> (Equ, CropIDx) in the means sheet.
# header == 1 marks a bold section row with no values.
.T1_MAP <- data.frame(
  label = c("Farmer a",
            "Female farmer (dummy)", "Age (years)", "Education (years)",
            "Selected crop production (real GH₵/ha)",
            "All crops", "Maize", "Rice", "Millet", "Sorghum", "Beans", "Peanut",
            "Cassava", "Yam", "Cocoyam", "Plantain", "Pepper", "Okra", "Tomato",
            "Cocoa", "Palm",
            "Land (ha)", "Crop diversification (index)", "Seed (maize kg/ha)",
            "Household labor (AE)", "Hired labor (man-days/ha)",
            "Fertilizer (kg/ha)", "Pesticide (liter/ha)",
            "Mechanization (dummy)", "Irrigation (dummy)", "Credit (dummy)",
            "Extension (dummy)",
            "Household b",
            "Size (AE)", "Dependency (ratio)"),
  header = c(1, 0,0,0, 1, rep(0, 16), rep(0, 11), 1, 0,0),
  Equ = c(NA,
          "Female", "AgeYr", "YerEdu",
          NA,
          rep("Yield", 16),
          "Area", "CrpMix", "SeedKg", "HHLaborAE", "HirdHr", "FertKg", "PestLt",
          "EqipMech", "EqipIrig", "Credit", "Extension",
          NA,
          "HHSizeAE", "Depend"),
  crop = c(NA,
           "Pooled", "Pooled", "Pooled",
           NA,
           "Pooled", "Maize", "Rice", "Millet", "Sorghum", "Beans", "Peanut",
           "Cassava", "Yam", "Cocoyam", "Plantain", "Pepper", "Okra",
           "Tomatoe",          # sheet spelling
           "Cocoa", "Palm",
           rep("Pooled", 11),
           NA,
           "Pooled", "Pooled"),
  stringsAsFactors = FALSE)

# Dagger convention. The footnote reads "a statistically significant difference
# from the pooled sample". With two groups this is the group contrast from the
# c.Trend##i.OwnLnd regression: CATDif (testparm i.OwnLnd -> level difference at
# Trend==0) flags the mean columns, TrendDif (testparm c.Trend#i.OwnLnd) flags
# the trend columns. Both group columns are flagged together, since with two
# groups the contrast is symmetric.
# VERIFY at first knit: Equ=="Female" has CATDif p=0.983 and TrendDif p=0.480,
# so the Female row should carry no daggers at all.
.T1_DAG <- 0.05

# Group labels in the engine's schema are the treatment's own levels ("0"/"1"),
# not the sheet's Mean_OwnLnd0 / Mean_OwnLnd1 strings.
.tbl1_live <- function() {
  m <- .desc()$table1
  fmt_mean <- function(eq, cr, g, dag) {
    k <- list(outcome = eq, crop = cr, group = g, wave = "all", statistic = "mean")
    b <- .pick(m, k, "estimate"); s <- .pick(m, k, "sd")
    if (is.na(b)) return("")
    sprintf("%.2f (%.2f)%s", b, s, if (isTRUE(dag)) " †" else "")
  }
  fmt_trend <- function(eq, cr, g, dag) {
    k <- list(outcome = eq, crop = cr, group = g, statistic = "trend_pct")
    b <- .pick(m, k, "estimate"); s <- .pick(m, k, "se"); p <- .pick(m, k, "p")
    if (is.na(b)) return("")
    sprintf("%.2f%s [%.2f]%s", b, .stars(p), s, if (isTRUE(dag)) " †" else "")
  }
  out <- .T1_MAP
  for (cc in paste0("c", 1:6)) out[[cc]] <- ""
  for (i in seq_len(nrow(out))) {
    if (out$header[i] == 1) next
    eq <- out$Equ[i]; cr <- out$crop[i]
    dcat <- .pick(m, list(outcome = eq, crop = cr, statistic = "cat_diff"),   "p")
    dtrd <- .pick(m, list(outcome = eq, crop = cr, statistic = "trend_diff"), "p")
    dcat <- !is.na(dcat) && dcat < .T1_DAG
    dtrd <- !is.na(dtrd) && dtrd < .T1_DAG
    out$c1[i] <- fmt_mean(eq, cr, "pooled", FALSE)
    out$c2[i] <- fmt_mean(eq, cr, "0", dcat)
    out$c3[i] <- fmt_mean(eq, cr, "1", dcat)
    out$c4[i] <- fmt_trend(eq, cr, "pooled", FALSE)
    out$c5[i] <- fmt_trend(eq, cr, "0", dtrd)
    out$c6[i] <- fmt_trend(eq, cr, "1", dtrd)
  }
  out$header <- as.character(out$header)
  out[, c("label", "header", paste0("c", 1:6))]
}

# Group sizes, live. Supersedes the hardcoded N_ALL/N_OWN/N_NON.
# Counts CropID=="Pooled" ROWS (the analysis sample), not distinct farmers:
# (Surveyx, EaId, HhId, Mid) is not unique within Pooled -- 28,411 keys against
# 35,185 rows, because a farmer can appear in more than one season. So
# "observations of farm households" is accurate; "35,185 farm households" is not.
.tbl1_n <- function() {
  m <- .desc()$table1
  g <- function(gr) .pick(m, list(outcome = "Yield", crop = "Pooled",
                                  group = gr, wave = "all", statistic = "mean"), "n")
  c(all = g("pooled"), non = g("0"), own = g("1"))
}

.tbl1_hdr <- function() {
  n <- .tbl1_n()
  lab <- c(sprintf("Pooled (n=%s)", format(n[["all"]], big.mark = ",")),
           sprintf("Land not owned (n=%s)", format(n[["non"]], big.mark = ",")),
           sprintf("Land partly or fully owned (n=%s)", format(n[["own"]], big.mark = ",")))
  c(lab, lab)
}

ft_table1 <- function()
  .ft_build(.tbl1_live(), .tbl1_hdr(), size = 8,
    spanner = c("", "Sample means (sample standard deviation)",
                "Annual trend from 1991 to 2017 (%)"),
    spanwidths = c(1, 3, 3),
    notes = c(.SIG_NOTE,
      "Standard deviations in parentheses; standard errors in brackets. A dagger denotes a statistically significant difference from the pooled sample.",
      "The trend was estimated as the annual percentage change via a generalised linear model.",
      "Trends span the 1984-frame rounds (GLSS3-GLSS4) and the 2000/2010-frame rounds (GLSS5-GLSS7); see Section 2 on comparability across that boundary.",
      .SRC_NOTE))

# Table 2 row map: display label -> Variable in the land_tenure sheet.
# Value labels are set in data-raw/okwaayeli_DATA.do:
#   LndOwn 1 "Not owned" 2 "Owned w/o deed" 3 "Owned w/ deed"
#   LndAq  1 Free 2 Sharecropping 3 Rented 4 Purchased 5 Kinship 6 Other
#   LndRgt 1 None 2 Security 3 Sell 4 Both
#   ShrCrpCat 1 "0" 2 "1-49" 3 "50-100"
# LndAq_6 ("Other") is absent from the sheet: it exists only in GLSS3/GLSS4 and
# this block keeps GLSS6/GLSS7, so its logit fails and the cap{} swallows it.
.T2_MAP <- data.frame(
  label = c("Land ownership status",
            "Not owned", "Owned without documentation", "Owned with documentation",
            "Farmland mode of acquisition",
            "Distributed by village/family", "Use free of charge", "Sharecropping",
            "Rented (cash or in kind)", "Purchased",
            "Farmland ownership rights",
            "No rights", "Can be used as collateral security", "Can be sold",
            "Can be used as collateral security and sold",
            "Sharecropping agreement percentage",
            "0", "1-49", "50-100"),
  header = c(1, 0,0,0, 1, 0,0,0,0,0, 1, 0,0,0,0, 1, 0,0,0),
  Variable = c(NA, "LndOwn_1", "LndOwn_2", "LndOwn_3",
               NA, "LndAq_5", "LndAq_1", "LndAq_2", "LndAq_3", "LndAq_4",
               NA, "LndRgt_1", "LndRgt_2", "LndRgt_3", "LndRgt_4",
               NA, "ShrCrpCat_1", "ShrCrpCat_2", "ShrCrpCat_3"),
  stringsAsFactors = FALSE)

.tbl2_live <- function() {
  s <- .desc()$shares
  out <- .T2_MAP
  for (cc in paste0("c", 1:3)) out[[cc]] <- ""
  for (i in seq_len(nrow(out))) {
    if (out$header[i] == 1) next
    v <- out$Variable[i]
    sh <- function(w) {
      k <- list(outcome = v, crop = "Pooled", wave = w)
      b <- .pick(s, k, "estimate"); sd <- .pick(s, k, "sd")
      if (is.na(b)) "" else sprintf("%.3f (%.3f)", b, sd)
    }
    b <- .pick(s, list(outcome = v, crop = "Pooled", wave = "trend"), "estimate")
    out$c1[i] <- sh("GLSS6")
    out$c2[i] <- sh("GLSS7")
    # wave_diff: percentage POINTS, GLSS6 - GLSS7. No SE -- Stata's nlcom
    # reported one, but it is a difference of two margins, and the sheet's
    # bracket was never used by the narrative.
    out$c3[i] <- if (is.na(b)) "" else sprintf("%.3f", b)
  }
  out$header <- as.character(out$header)
  out[, c("label", "header", paste0("c", 1:3))]
}

.tbl2_hdr <- function() {
  s <- .desc()$shares
  n <- function(w) .pick(s, list(outcome = "LndOwn_1", crop = "Pooled",
                                 wave = w), "n")
  f <- function(x) if (is.na(x)) "?" else format(round(x), big.mark = ",")
  c(sprintf("GLSS6 (2012/13) (n=%s)", f(n("GLSS6"))),
    sprintf("GLSS7 (2016/17) (n=%s)", f(n("GLSS7"))),
    sprintf("Change (2012/13 to 2016/17, pp) (n=%s)", f(n("pooled"))))
}

ft_table2 <- function()
  .ft_build(.tbl2_live(), .tbl2_hdr(), first_lab = "Outcome", size = 8,
    spanner = c("", "Mean (standard deviation)", ""),
    spanwidths = c(1, 2, 1),
    notes = c("Standard deviations in parentheses; standard errors in brackets.",
      "Ownership detail modules are only administered in GLSS6 and GLSS7.",
      "Data source: Ghana Living Standards Survey [waves 6-7]."))

# ---- Table 3: elasticities / variability (estimation objects) ----------------
# Column layout (mirrors RE's Table 3 and the draft's Table 3):
#   naive | none [A] | any [B] | gap [B-A] | meta matched | meta unmatched
.samp_id <- function(s) if (identical(s, "matched")) .opt else "unmatched"
.T3COLS <- list(
  naive  = list(samp = "unmatched", lv = "National", lt = "level"),
  none   = list(samp = "unmatched", lv = "0",        lt = "level"),
  any    = list(samp = "unmatched", lv = "1",        lt = "level"),
  gap    = list(samp = "matched",   lv = "1",        lt = "Gap_lvl"),
  meta_m = list(samp = "matched",   lv = "Meta",     lt = "level"),
  meta_u = list(samp = "unmatched", lv = "Meta",     lt = "level")
)
.f_es <- function(e, se, p) if (length(e) == 0 || is.na(e)) "-" else
  sprintf("%.3f%s (%.3f)", e, .stars(p), se)
.f_st <- function(e, p) if (length(e) == 0 || is.na(e)) "-" else
  sprintf("%.3f%s", e, .stars(p))
.f_nm <- function(e, d = 0) if (length(e) == 0 || is.na(e)) "-" else
  formatC(e, format = "f", digits = d)
.gap_coef <- function(df, stub) {
  cn <- unique(as.character(df$CoefName))
  hit <- cn[grepl("Gap_lvl$", cn) & grepl(stub, cn, ignore.case = TRUE)]
  if (length(hit) == 0) NA_character_ else hit[1]
}
.t3_el <- function(el, code, cc) {
  want <- if (identical(cc$lt, "level")) "elasticity" else .gap_coef(el, "elasticit")
  if (is.na(want)) return("-")
  b <- el[el$input %in% code & el$CoefName %in% want &
          as.character(el$TCHLvel) %in% cc$lv &
          el$sample %in% .samp_id(cc$samp), ]
  .f_es(b$Estimate[1], b$Estimate.sd[1], b$jack_pv[1])
}
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
.t3_sf <- function(sf, coef, cc, how, survey = "GLSS0") {
  b <- sf[sf$CoefName %in% coef & sf$restrict %in% "Restricted" &
          sf$Survey %in% survey &
          as.character(sf$TCHLvel) %in% cc$lv &
          sf$sample %in% .samp_id(cc$samp), ]
  switch(how,
         es = .f_es(b$Estimate[1], b$Estimate.sd[1], b$jack_pv[1]),
         st = .f_st(b$Estimate[1], b$jack_pv[1]),
         n0 = .f_nm(b$Estimate[1], 0),
         n2 = .f_nm(b$Estimate[1], 2),
         n3 = .f_nm(b$Estimate[1], 3))
}
.t3_nobs <- function(sf, cc) {
  b <- sf[sf$CoefName %in% "Nobs" & sf$restrict %in% "Restricted" &
          sf$Survey %in% c("GLSS3", "GLSS4", "GLSS5", "GLSS6", "GLSS7") &
          as.character(sf$TCHLvel) %in% cc$lv &
          sf$sample %in% .samp_id(cc$samp), ]
  if (nrow(b) == 0) return("-")
  .f_nm(sum(b$Estimate, na.rm = TRUE), 0)
}
.t3_npar <- function(sf, cc) {
  b <- sf[sf$CoefName %in% c("nXvar", "nuZUvar", "nvZVvar") &
          sf$restrict %in% "Restricted" & sf$Survey %in% "GLSS0" &
          as.character(sf$TCHLvel) %in% cc$lv &
          sf$sample %in% .samp_id(cc$samp), ]
  if (nrow(b) == 0) return("-")
  .f_nm(sum(b$Estimate, na.rm = TRUE), 0)
}
# input_variables = c("Area","SeedKg","HHLaborAE","HirdHr","FertKg","PestLt")
# => el5 = fertilizer, el6 = pesticide (code mapping, not the workbook's).
.T3_EL <- c("Land" = "el1", "Planting material" = "el2", "Family labor" = "el3",
            "Hired labor" = "el4", "Fertilizer" = "el5", "Pesticide" = "el6",
            "Returns to scale" = "el7")

.ft_table3_est <- function() {
  p  <- .read_est("OwnLnd")
  el <- p$el_mean; ef <- p$ef_mean; sf <- p$sf_estm
  el <- el[el$stat %in% "wmean" & el$Survey %in% "GLSS0" &
           el$restrict %in% "Restricted", ]
  CO <- .T3COLS
  blank <- function(lab) c(lab, "", "", "", "", "", "")
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
      g <- function(cc) if (cc$lv %in% c("National", "Meta")) "-" else
        .t3_ef(ef, metric, cc, sm)
      rows[[length(rows) + 1]] <- mk(lab, g)
    }
  }
  rows[[length(rows) + 1]] <- blank("Model diagnostics")
  gap_dash <- function(f) function(cc) if (identical(cc$lt, "Gap_lvl")) "-" else f(cc)
  D <- list(
    list("Sample size",                        gap_dash(function(cc) .t3_nobs(sf, cc))),
    list("Monotonicity satisfaction rate",     gap_dash(function(cc) .t3_sf(sf, "mono", cc, "n2"))),
    list("Curvature satisfaction rate",        gap_dash(function(cc) .t3_sf(sf, "curv", cc, "n2"))),
    list("Schmidt & Lin (1984)",               gap_dash(function(cc) .t3_sf(sf, "olsSkew", cc, "st"))),
    list("Coelli (1995)",                      gap_dash(function(cc) .t3_sf(sf, "CoelliM3Test", cc, "st"))),
    list("Gutierrez (2001)",                   gap_dash(function(cc) .t3_sf(sf, "LRInef", cc, "st"))),
    list("Log likelihood",                     gap_dash(function(cc) .t3_sf(sf, "mlLoglik", cc, "n0"))),
    list("No. of parameters",                  gap_dash(function(cc) .t3_npar(sf, cc))),
    list("Meta frontier LR test",              function(cc) if (!identical(cc$lv, "Meta")) "-" else
                                                 .t3_sf(sf, "LRT", cc, "n3")),
    list("Ratio variance due to inefficiency", gap_dash(function(cc) .t3_sf(sf, "Gamma", cc, "es")))
  )
  for (d in D) rows[[length(rows) + 1]] <- mk(d[[1]], d[[2]])

  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c("item", "naive", "none", "any", "gap", "meta_m", "meta_u")
  # If the keying is off, nearly every cell is "-": treat that as failure.
  vals <- unlist(m[, -1])
  if (mean(vals %in% c("-", "")) > 0.9) stop("table3 keying unresolved")

  blocks <- c("Elasticity", "Technology/efficiency", "Model diagnostics")
  subhdr <- names(EFB)
  ib <- which(m$item %in% blocks); is <- which(m$item %in% subhdr)
  ft <- flextable(m)
  ft <- set_header_labels(ft, item = "", naive = "Naive national frontier",
                          none = "No ownership", any = "Land owned",
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
    paste(.SIG_NOTE, "Jackknife standard errors in parentheses."),
    "Schmidt & Lin (1984), Coelli (1995) and Gutierrez (2001) test the null of no one-sided error (no inefficiency).",
    "Meta Stochastic Frontier Analysis jointly performed on Ghana Living Standards Survey [waves 3-7]."))
  ft <- fontsize(ft, size = 6, part = "footer")
  ft
}

.ft_table3_csv <- function()
  .ft_csv("table3.csv", "table3_header.csv", first_lab = "", size = 9,
    spanner = c("", "", "Group frontier", "Meta-frontier"),
    spanwidths = c(1, 1, 3, 2),
    notes = c(paste(.SIG_NOTE, "Jackknife standard errors in parentheses."),
      "Values from the published v001 draft (fallback while estimation-object keying is unverified).",
      "Meta Stochastic Frontier Analysis jointly performed on Ghana Living Standards Survey [waves 3-7]."))

ft_table3 <- function()
  tryCatch(.ft_table3_est(), error = function(e) {
    message("ft_table3: estimation-object build failed (", conditionMessage(e),
            "); falling back to data/tables/table3.csv")
    .ft_table3_csv()
  })

# ---- Table 4: ownership-detail parity (estimation objects) -------------------
# Rows are ownership-detail categories (documentation, rights, acquisition,
# sharecropping) read from the disaggregated gap scores of the OwnLnd object,
# blocks as in the draft. VERIFY at first knit; falls back to table4.csv.
.ft_table4_est <- function() {
  # Category labels per data-raw/okwaayeli_DATA.do (lab define LndAq / ShrCrpCat).
  .T4_LAB <- list(
    LndAq = c("1" = "Use free of charge", "2" = "Sharecropping",
              "3" = "Rented (cash or in kind)", "4" = "Purchased",
              "5" = "Distributed by village/family", "6" = "Other (GLSS3-4 only)"),
    ShrCrpCat = c("1" = "0%", "2" = "1-49%", "3" = "50-100%"))

  res <- .read_est("OwnLnd")$disagscors
  res$disasg <- as.character(res$disagscors_var)
  res$level  <- as.character(res$disagscors_level)
  res <- res[res$estType %in% "teBC" & res$Survey %in% "GLSS0" &
             res$restrict %in% "Restricted" & res$stat %in% "mean" &
             !res$sample %in% "unmatched" &
             res$CoefName %in% "disag_efficiencyGap_lvl", ]
  if (nrow(res) == 0) stop("table4 keying unresolved (no disagscors rows)")
  DIMS <- c("Farmland mode of acquisition" = "LndAq",
            "Sharecropping share of land (%)" = "ShrCrpCat")
  METS <- c("TGR", "TE", "MTE")
  rows <- list()

  # Documentation and rights blocks: published Table 4 (source of record;
  # these categories are not in the OwnLnd disaggregation object).
  pub <- .read_tbl("table4.csv")
  cut <- which(pub$label %in% "GLSS7")[1]
  if (!is.na(cut)) pub <- pub[seq_len(cut - 1), ]
  pub <- pub[!pub$label %in% "GLSS3-GLSS7", ]
  for (i in seq_len(nrow(pub)))
    rows[[length(rows) + 1]] <- c(pub$label[i], pub$c1[i], pub$c2[i], pub$c3[i])

  # Acquisition and sharecropping blocks: ownership gap within each category
  # (no ownership minus some ownership), from the estimation objects.
  for (dn in names(DIMS)) {
    d <- res[res$disasg %in% DIMS[[dn]], ]
    if (nrow(d) == 0) next
    rows[[length(rows) + 1]] <- c(dn, "", "", "")
    labs <- .T4_LAB[[DIMS[[dn]]]]
    for (lv in sort(unique(d$level))) {
      dd <- d[d$level %in% lv, ]
      v <- vapply(METS, function(mm) {
        b <- dd[dd$input %in% mm, ]
        .cell(b$Estimate[1], b$Estimate.sd[1], b$jack_pv[1])
      }, character(1))
      lab <- if (lv %in% names(labs)) labs[[lv]] else lv
      rows[[length(rows) + 1]] <- c(lab, v)
    }
  }
  if (length(rows) < 3) stop("table4 keying unresolved (no dimension blocks)")
  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c("item", "tgr", "te", "mte")
  hdr <- which(m$tgr == "" & m$te == "" & m$mte == "")
  ft <- flextable(m)
  ft <- set_header_labels(ft, item = "",
                          tgr = "Technology gap ratio (TGR)",
                          te  = "Pure farmer technical efficiency (TE)",
                          mte = "Meta-frontier technical efficiency (MTE)")
  ft <- add_header_row(ft, top = TRUE, values = c("", "Level difference"),
                       colwidths = c(1, 3))
  ft <- bold(ft, i = hdr, j = 1, part = "body")
  ft <- bold(ft, part = "header")
  ft <- align(ft, j = 2:4, align = "right", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- align(ft, i = 1, align = "center", part = "header")
  ft <- padding(ft, i = setdiff(seq_len(nrow(m)), hdr), j = 1,
                padding.left = 8, part = "body")
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  ft <- fontsize(ft, size = 9, part = "all")
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft <- add_footer_lines(ft, values = c(
    paste(.SIG_NOTE, "Jackknife standard errors in parentheses."),
    "Documentation and rights blocks: level difference of each ownership category relative to matched non-owners (published Table 4).",
    "Acquisition and sharecropping blocks: matched-sample ownership gap (no ownership minus some ownership) within each category, from the disaggregated estimation objects.",
    "Meta Stochastic Frontier Analysis jointly performed on Ghana Living Standards Survey [waves 3-7]."))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

.ft_table4_csv <- function()
  .ft_csv("table4.csv", "table4_header.csv", first_lab = "", size = 9,
    spanner = c("", "Level difference"), spanwidths = c(1, 3),
    notes = c(paste(.SIG_NOTE, "Jackknife standard errors in parentheses."),
      "Values from the published v001 draft (fallback while estimation-object keying is unverified).",
      "Meta Stochastic Frontier Analysis jointly performed on Ghana Living Standards Survey [waves 3-7]."))

ft_table4 <- function()
  tryCatch(.ft_table4_est(), error = function(e) {
    message("ft_table4: estimation-object build failed (", conditionMessage(e),
            "); falling back to data/tables/table4.csv")
    .ft_table4_csv()
  })

# ---- Supplementary tables -----------------------------------------------------
.HC_NOTE <- c("Cells are headcount ratios over 2012/13-2016/17.",
              "Ownership detail modules are only administered in GLSS6 and GLSS7.",
              "Data source: Ghana Living Standards Survey [waves 6-7].")

# Table S0: construction of the tenure indicators.
# Unlike every other exhibit here, this one is NOT extracted from the v001 draft
# and has no fallback -- it is built from the GLSS3-7 Section 8b files and the
# questionnaire instruments read directly. GLSS3 carries no variable or value
# labels and GLSS4 carries no value labels, so both waves' codes were verified
# against G3QPartB.pdf / G4QPartB.pdf; all hand-coded mappings in
# data-raw/okwaayeli_DATA.do check out. Provenance and the full verification
# trail: narrative/diagnostics/tenure_variable_documentation.md
ft_tableS0 <- function() {
  d <- .read_tbl("tableS0.csv")
  ft <- flextable(d)
  ft <- set_header_labels(ft,
                          round = "Survey round", frame = "Census frame",
                          source = "Source variables", question = "Question wording",
                          options = "Response options", mapping = "Mapping / comparability")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "left", part = "all")
  ft <- valign(ft, valign = "top", part = "body")
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  ft <- fontsize(ft, size = 7, part = "all")
  ft <- width(ft, j = 1, width = 0.85)
  ft <- width(ft, j = 2, width = 0.60)
  ft <- width(ft, j = 3, width = 1.60)
  ft <- width(ft, j = 4, width = 1.90)
  ft <- width(ft, j = 5, width = 2.35)
  ft <- width(ft, j = 6, width = 1.90)
  ft <- add_footer_lines(ft, values = c(
    "Farmer-level tenure is the category holding the largest share of cultivated area across the farmer's plots, not 'owns any land'. Ties resolve toward the more secure category and affect under 0.5% of farmers in every wave.",
    "Non-owners are skipped past the rights question in every wave, so 'no right' is assigned to them by construction rather than measured.",
    "Purchase is not a response option in GLSS3 or GLSS4; 'inherited' is unique to GLSS7 and is folded into kinship.",
    "Poles and Ropes are local, respondent-defined units. GSS instructs interviewers to record them as given and publishes no conversion; the hectare equivalents used here are analyst assumptions covering 11-36% of plots by wave.",
    "GSS drew GLSS3 and GLSS4 from the 1984 census frame, which the GLSS4 report describes as 'quite old' and 'inadequate', and GLSS5 onward from the 2000 Population and Housing Census frame. Each wave is internally representative; comparisons spanning the GLSS4/GLSS5 boundary are not drawn from a common population.",
    "Data source: Ghana Living Standards Survey [waves 3-7], Part B Section 8b, and the corresponding questionnaire instruments."))
  ft <- fontsize(ft, size = 6, part = "footer")
  ft
}

# ---- Tables S1-S4: the crop-disaggregated analogues of Table 2 ----------------
# Same sheet as Table 2, sliced by crop instead of restricted to "Pooled".
#   S1 LndOwn_1..3     ownership status
#   S2 LndAq_1..5      mode of acquisition   (LndAq_6 "Other" is GLSS3/4 only)
#   S3 LndRgt_1..4     ownership rights
#   S4 ShrCrpCat_1..3  sharecropping intensity
#
# Row order is editorial. The v001 CSVs used two different arbitrary orders and
# buried "All crops listed" mid-table; we sort by the first column descending and
# put the pooled row last, which is at least a stated rule.
#
# NB the v001 CSVs carried a second block headed "Percentage change in Headcount
# ratio from 2012/13 to 2016/17" whose values were an exact copy of the headcount
# block -- the trend was never populated. Rebuilt here from mesure=="Trend".
# 03_land_tenure_context.Rmd reads the headcount block only (tbl_pct takes the
# first match), so its numbers were unaffected.
.S_CROPS <- c(Millet = "Millet", Sorghum = "Sorghum", Rice = "Rice", Okra = "Okra",
              Maize = "Maize", Beans = "Beans", Peanut = "Peanut", Cocoa = "Cocoa",
              Cassava = "Cassava", Banana = "Banana", Plantain = "Plantain",
              Pepper = "Pepper", Yam = "Yam", Cocoyam = "Cocoyam",
              Tomato = "Tomatoe",   # display label -> sheet spelling
              Eggplant = "Eggplant", Palm = "Palm")

.tblS_live <- function(vars) {
  s <- .desc()$shares
  one <- function(w, digits, fmt_sd) {
    rows <- c(unname(.S_CROPS), "Pooled")
    labs <- c(names(.S_CROPS), "All crops listed")
    t <- data.frame(label = labs, header = "0", stringsAsFactors = FALSE)
    for (j in seq_along(vars))
      t[[paste0("c", j)]] <- vapply(rows, function(cr) {
        k <- list(outcome = vars[j], crop = cr, wave = w)
        b <- .pick(s, k, "estimate")
        if (is.na(b)) return("")
        if (fmt_sd) sprintf(paste0("%.", digits, "f (%.", digits, "f)"),
                            b, .pick(s, k, "sd"))
        else sprintf(paste0("%.", digits, "f"), b)
      }, character(1))
    # Row order is editorial. The v001 CSVs used two different arbitrary orders
    # and buried "All crops listed" mid-table; sort by the first column
    # descending with the pooled row last, which is at least a stated rule.
    ord <- order(suppressWarnings(as.numeric(sub(" .*", "", t$c1[-nrow(t)]))),
                 decreasing = TRUE, na.last = TRUE)
    rbind(t[-nrow(t), ][ord, ], t[nrow(t), ])
  }
  hc <- one("pooled", 3, TRUE)    # headcount ratio across 2012/13-2016/17
  tr <- one("trend",  2, FALSE)   # wave_diff: percentage points, GLSS6 - GLSS7
  sep <- hc[1, ]
  sep$label <- "Change in headcount ratio, 2012/13 to 2016/17 (pp)"
  sep$header <- "1"; sep[paste0("c", seq_along(vars))] <- ""
  out <- rbind(hc, sep, tr)
  rownames(out) <- NULL
  out
}

.S_HDR <- function(labs) labs

ft_tableS1 <- function()
  .ft_build(.tblS_live(c("LndOwn_1", "LndOwn_2", "LndOwn_3")),
    c("Not owned", "Owned without documentation", "Owned with documentation"),
    first_lab = "Crop", size = 8,
    spanner = c("", "Headcount ratio over 2012/13-2016/17"), spanwidths = c(1, 3),
    notes = .HC_NOTE)
ft_tableS2 <- function()
  .ft_build(.tblS_live(c("LndAq_5", "LndAq_1", "LndAq_2", "LndAq_3", "LndAq_4")),
    c("Distributed by village/family", "Use free of charge", "Sharecropping",
      "Rented (cash or in kind)", "Purchased"),
    first_lab = "Crop", size = 8,
    spanner = c("", "Headcount ratio over 2012/13-2016/17"), spanwidths = c(1, 5),
    notes = .HC_NOTE)
ft_tableS3 <- function()
  .ft_build(.tblS_live(c("LndRgt_1", "LndRgt_2", "LndRgt_3", "LndRgt_4")),
    c("No rights", "Can be used as collateral security", "Can be sold",
      "Can be used as collateral security and sold"),
    first_lab = "Crop", size = 8,
    spanner = c("", "Headcount ratio over 2012/13-2016/17"), spanwidths = c(1, 4),
    notes = .HC_NOTE)
ft_tableS4 <- function()
  .ft_build(.tblS_live(c("ShrCrpCat_1", "ShrCrpCat_2", "ShrCrpCat_3")),
    c("0", "1-49", "50-100"),
    first_lab = "Crop", size = 8,
    spanner = c("", "Headcount ratio over 2012/13-2016/17"), spanwidths = c(1, 3),
    notes = .HC_NOTE)

.MSF_NOTE <- c(
  paste(.SIG_NOTE, "Jackknife standard errors in parentheses."),
  "Fertilizer is lnI5 and pesticide is lnI6, following input_variables order in 004_MSF_land_tenure_study.R.",
  "Data source: Ghana Living Standards Survey [waves 3-7].")

ft_tableS5 <- function()
  .ft_csv("tableS5.csv", "tableS5_header.csv", first_lab = "", size = 7,
    spanner = c("", "", "Group frontier", "", "Meta-frontier"),
    spanwidths = c(1, 1, 2, 1, 2),
    notes = .MSF_NOTE)
ft_tableS6 <- function()
  .ft_csv("tableS6.csv", "tableS6_header.csv", first_lab = "", size = 9,
    spanner = c("", "Level difference (unmatched estimates)"), spanwidths = c(1, 3),
    notes = c(paste(.SIG_NOTE, "Jackknife standard errors in parentheses."),
      "Unmatched-sample counterpart of Table 4.",
      "Data source: Ghana Living Standards Survey [waves 3-7]."))
ft_tableS7 <- function()
  .ft_csv("tableS7.csv", "tableS7_header.csv", first_lab = "", size = 9,
    spanner = c("", "", "Group frontier", "", "Meta-frontier"),
    spanwidths = c(1, 1, 2, 1, 2),
    notes = c(paste(.SIG_NOTE, "Jackknife standard errors in parentheses."),
      "A positive coefficient means the variable moves the farm away from its efficient frontier.",
      "Data source: Ghana Living Standards Survey [waves 3-7]."))

# ---- Inline text lookups (Tier 2) --------------------------------------------
# Pull single numbers out of the exhibits so the narrative can cite descriptive
# values without hard-coding them.
# tbl_num("table1.csv", "Age (years)", "c1")          -> 46.64  (leading number)
# tbl_num("table1.csv", "Age (years)", "c1", "paren") -> 15.09  (inside (...))
# tbl_num(..., "bracket")                             -> value inside [...]
#
# THE `csv` ARGUMENT IS NOW A TABLE ID, NOT A FILE. It still reads "table1.csv"
# etc. so the ~90 call sites in 02_data.Rmd and 03_land_tenure_context.Rmd did
# not have to change, but the numbers come from the SAME built table the exhibit
# renders -- .tbl1_live(), .tbl2_live(), .tblS_live() -- which in turn read
# data/descriptive_exhibits.rds from 100_exhibit_descriptive_stats.R.
#
# WHY. Until 2026-07-15 these lookups read data/tables/*.csv, curated by hand
# from the v001 Word draft, while the exhibits were rebuilt from the pipeline.
# The prose could then describe a number the table beside it did not print, and
# nothing would flag it. Routing both through one builder makes prose and exhibit
# agree by construction rather than by coincidence.
#
# The S-tables are stacked: a headcount block, a bold separator, then a change
# block, with the SAME crop labels in both. Lookups therefore take the FIRST
# match, i.e. the headcount block -- which is what 03_land_tenure_context.Rmd has
# always cited. Pass block = "change" for the lower one.
# Tables rebuilt from data/descriptive_exhibits.rds. Everything else -- Table 3,
# Table 4, S5-S7 -- is frontier/robustness output and still reads a curated CSV.
.LIVE_IDS <- c("table1", "table2", "tableS1", "tableS2", "tableS3", "tableS4")

.live_table <- function(id) {
  switch(sub("\\.csv$", "", id),
    table1  = .tbl1_live(),
    table2  = .tbl2_live(),
    tableS1 = .tblS_live(c("LndOwn_1", "LndOwn_2", "LndOwn_3")),
    tableS2 = .tblS_live(c("LndAq_5", "LndAq_1", "LndAq_2", "LndAq_3", "LndAq_4")),
    tableS3 = .tblS_live(c("LndRgt_1", "LndRgt_2", "LndRgt_3", "LndRgt_4")),
    tableS4 = .tblS_live(c("ShrCrpCat_1", "ShrCrpCat_2", "ShrCrpCat_3")),
    .read_tbl(id))
}

tbl_num <- function(csv, label, col, part = c("first", "paren", "bracket"),
                    block = c("first", "change")) {
  d <- .live_table(csv)
  live <- sub("\\.csv$", "", csv) %in% .LIVE_IDS
  r <- d[trimws(d$label) == label & d$header != "1", , drop = FALSE]
  if (nrow(r) == 0)
    stop("tbl_num: label not found: ", label, " in ", csv, call. = FALSE)
  block <- match.arg(block)
  i <- if (block == "change" && nrow(r) > 1) nrow(r) else 1L
  # Strict ONLY for the rebuilt tables. Their rows come from a keyed schema, so a
  # duplicate label means a bug -- and taking the first silently is how the
  # `roweq` collision survived for years. The curated CSVs are a different case:
  # table4.csv repeats "Owned with documentation" across four panels by design,
  # and first-match has always been the contract there.
  if (live && nrow(r) > 2)
    stop("tbl_num: ", nrow(r), " rows for '", label, "' in ", csv,
         "; expected at most 2 (headcount + change).", call. = FALSE)
  s <- as.character(r[[col]][i])
  if (is.na(s) || !nzchar(s))
    stop("tbl_num: empty cell for '", label, "' / ", col, " in ", csv,
         ". The engine may have refused that fit -- see descriptive_trend_model().",
         call. = FALSE)
  part <- match.arg(part)
  pat <- switch(part,
    first   = "^\\s*(-?[0-9][0-9.,]*).*$",
    paren   = "^.*\\((-?[0-9][0-9.,]*)\\).*$",
    bracket = "^.*\\[(-?[0-9][0-9.,]*)\\].*$")
  if (!grepl(pat, s)) stop("tbl_num: no ", part, " number in '", s, "'", call. = FALSE)
  as.numeric(gsub(",", "", sub(pat, "\\1", s)))
}
# Percentage view of a proportion cell (Table 2 / S1-S4 store shares).
tbl_pct <- function(csv, label, col, digits = 1, block = c("first", "change"))
  fmt_num(100 * tbl_num(csv, label, col, block = match.arg(block)), digits)

# Matched input/output gaps behind Figure 1 (output/figure_data/input_TE_data.csv).
.FIGDAT <- file.path(.STUDY_ROOT, "output", "figure_data")
fig1_est <- function(outcome, level = "Full sample") {
  d <- utils::read.csv(file.path(.FIGDAT, "input_TE_data.csv"))
  v <- d$est[d$outC == outcome & d$level == level]
  if (length(v) == 0) stop("fig1_est: not found: ", outcome, " / ", level, call. = FALSE)
  as.numeric(v[1])
}
fig1_range <- function(outcomes, fun) {
  d <- utils::read.csv(file.path(.FIGDAT, "input_TE_data.csv"))
  v <- d$est[d$outC %in% outcomes]
  if (length(v) == 0) stop("fig1_range: no rows", call. = FALSE)
  fun(as.numeric(v))
}

# Ownership gap trend behind Figure 2 (output/figure_data/score_trend.csv).
# trend_gap("TGR", "GLSS 3") -> gap in percentage points (no-own minus own).
trend_gap <- function(metric, wave) {
  d <- utils::read.csv(file.path(.FIGDAT, "score_trend.csv"))
  v <- d$Estimate[d$CoefName == metric & grepl(wave, d$Survey, fixed = TRUE)]
  if (length(v) == 0) stop("trend_gap: not found: ", metric, " / ", wave, call. = FALSE)
  100 * as.numeric(v[1])
}
trend_range <- function(metric, waves, fun) {
  d <- utils::read.csv(file.path(.FIGDAT, "score_trend.csv"))
  keep <- d$CoefName == metric & Reduce(`|`, lapply(waves, function(w) grepl(w, d$Survey, fixed = TRUE)))
  v <- d$Estimate[keep]
  if (length(v) == 0) stop("trend_range: no rows", call. = FALSE)
  100 * fun(as.numeric(v))
}

# Sample sizes -- now read live from the workbook rather than hardcoded, so the
# prose in 00_abstract / 01_introduction / 02_data / 06_conclusion cannot drift
# from the analysis sample.
#
# These count CropID=="Pooled" ROWS (the analysis sample), not distinct farmers:
# (Surveyx, EaId, HhId, Mid) is NOT unique within Pooled -- 28,411 distinct keys
# against 35,185 rows -- because a farmer can appear in more than one season.
# The wave group sizes in the means sheet sum to exactly 13,099 / 22,086 / 35,185.
# "observations of farm households" in 02_data.Rmd is therefore the accurate
# phrasing; "35,185 farm households" is not.
.N <- local({
  n <- tryCatch(.tbl1_n(), error = function(e) NULL)
  if (is.null(n) || any(is.na(n))) {
    warning("110_exhibit_tables.R: could not read sample sizes from ", .XLSX,
            "; falling back to the 2026-07-15 values. Re-run 100_exhibits.do.",
            call. = FALSE)
    c(all = 35185, non = 13099, own = 22086)
  } else n
})
N_ALL <- .N[["all"]]
N_NON <- .N[["non"]]
N_OWN <- .N[["own"]]

# ---- Page sections ----------------------------------------------------------
# Ported from resource_extraction: officer::block_section with type="nextPage"
# (officedown's BLOCK_LANDSCAPE markers emit type="oddPage", which injects blank
# pages). reference.docx is US Letter with 1in margins; both are passed
# explicitly because officer defaults would silently reflow the document.
.PAGE <- c(w = 8.5, h = 11)
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
# A section's properties apply to the content BEFORE it:
#   sec_portrait() closes the preceding portrait run, sec_landscape() the landscape run.
sec_portrait  <- function() .sec("portrait")
sec_landscape <- function() .sec("landscape")
