# exhibit_helpers_tables.R
# Build the manuscript tables as flextable objects.
#
# A LIBRARY, NOT A STEP: no position in a sequence, hence no number. Sourced by
# narrative/land-tenure.Rmd at knit time and by 102. See scripts/README.md.
#
# SOURCES
#   Tables 1, 2, S1-S4   data/descriptive_exhibits.rds        (100)
#   Table 3              sf_estm / el_mean / ef_mean, OwnLnd
#   Table 4              ef_mean (LndOwn, LndRgt) + disagscors (OwnLnd)
#   Table S5             sf_estm, production + risk function
#   Table S6             Table 4's blocks, unmatched, per wave
#   Table S7             sf_estm, Zu_ (inefficiency)
#   Table S0             data/tables/tableS0.csv -- curated permanently, a
#                        questionnaire transcription, not computable
#
# tableS0.csv is the ONLY file in data/tables/ that anything reads.
#
# NO FALLBACKS. Every builder errors rather than degrading to a stored value.
# Tables 3 and 4 once fell back to a frozen CSV on a keying error: the knit
# "succeeded" while printing last year's numbers beside prose citing this
# year's. A failed render is the cheaper failure.
#
# KEYING. Ownership group is TCHLvel, not Tech -- sf_estm carries both for the
# same concept and they disagree (Tech 1 == TCHLvel "0" == non-owners). Key on
# TCHLvel:
#   "0" = no ownership, "1" = some ownership, "National" = naive, "Meta"
# Tables 4 and S6 differ only by sample: matched (opt_sample) vs unmatched.
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
  "studies/land_tenure"
}

# ---- Memoization --------------------------------------------------------------
# The Rmd makes 138 tbl_num()/tbl_pct() calls resolving to 7 distinct tables,
# and each build reads estimation objects that are ~50 MB compressed. Uncached,
# the 27 table4 lookups alone re-read 137 MB apiece -- gigabytes of gzip to
# produce numbers that cannot change mid-render.
#
# Session-lived. Nothing can rewrite the estimations while knitr reads them, but
# in an interactive session after re-running a stage, call exhibit_cache_clear().
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

# Keep only the four summary components. The objects also carry ef_samp/ef_dist
# (one row per farmer per draw), which dwarf everything else and which no table
# here touches -- fig_distribution() reads those from the *_fullset* file in 101.
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
# tableS0.csv is the only file here, and the only one read. See SOURCES above.
.TBL_DIR <- file.path(.STUDY_ROOT, "data", "tables")

.read_tbl <- function(nm) {
  p <- file.path(.TBL_DIR, nm)
  if (!file.exists(p)) stop("exhibit_helpers_tables.R: missing ", p, call. = FALSE)
  utils::read.csv(p, check.names = FALSE, colClasses = "character",
                  encoding = "UTF-8")
}
# .read_hdr() and .ft_csv() lived here until 2026-07-16 and are gone. Every
# table that had a *_header.csv is built live now, and its column titles are
# set_header_labels() calls inside the builder. Reaching for a header CSV means
# reintroducing a frozen exhibit.

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
# plus a character vector of column headers. Serves Tables 1, 2 and S1-S4, whose
# builders pass a live frame from data/descriptive_exhibits.rds.
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

# ---- Descriptive layer --------------------------------------------------------
# Tables 1, 2 and S1-S4 come from 100_exhibit_descriptive_stats.R, which computes
# them in R from study_raw_data. The engine is validated against the Stata
# workbooks it replaced -- ~15,000 assertions across both studies; see
# tests/testthat/test-descriptive-exhibits-*.R.
#
# NB use .STUDY_ROOT, NOT article_helpers.R's constants: those are
# repo-root-relative, and knitr's working directory is narrative/ during a
# render. .STUDY_ROOT is probed above for exactly this reason.
.DESC <- file.path(.STUDY_ROOT, "data", "descriptive_exhibits.rds")
.desc <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      if (!file.exists(.DESC))
        stop("exhibit_helpers_tables.R: missing ", .DESC,
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
    stop("exhibit_helpers_tables.R: ", length(v), " rows matched ",
         paste(sprintf("%s=%s", names(keys), unlist(keys)), collapse = ", "),
         "; expected 1.", call. = FALSE)
  as.numeric(v[1])
}
.stars <- okwaayeli::exhibit_stars

.SRC_NOTE <- "Data source: Ghana Living Standards Survey [waves 3-7]."
.SIG_NOTE <- "Significance levels: * p<0.10, ** p<0.05, *** p<0.01."

# Why the tenure-detail tables stop at GLSS6/GLSS7.
#
# This footnote used to read "Ownership detail modules are only administered in
# GLSS6 and GLSS7." That is FALSE, and Table S0 -- printed in the same appendix,
# transcribed from the questionnaires -- contradicted it on its face: all five
# rounds carry ownership, rights, acquisition and sharecropping (GLSS3 q6/q7/q9/
# q11; GLSS4-GLSS7 q5/q6/q8/q10). A reviewer reading the appendix would have
# found the contradiction immediately.
#
# The restriction itself is sound; only its stated reason was wrong. The real
# constraint is comparability, documented in Table S0: acquisition has no
# purchase option before GLSS5 and an "Inherited" option unique to GLSS7 that is
# folded into kinship (where it is the larger component), and GLSS5's
# sharecropping fractions are a narrower coded set than GLSS6/GLSS7's. The
# GLSS3/GLSS4 rounds are additionally excluded from every temporal claim in the
# study because they were fielded on the 1984 frame.
#
# See 100_exhibit_descriptive_stats.R for why documentation and rights -- which
# ARE identically coded in all five rounds -- are held to the same window.
.TENURE_WINDOW_NOTE <- paste(
  "Restricted to GLSS6-GLSS7 for comparability, not availability: all five",
  "rounds administer the ownership detail modules (Table S0), but the",
  "acquisition categories and sharecropping bins are not comparable across",
  "earlier rounds, and GLSS3-GLSS4 were fielded on the 1984 census frame.")

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
      .TENURE_WINDOW_NOTE,
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

ft_table3 <- function() {
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
  # If the keying is off, nearly every cell is "-". That used to fall back to the
  # v001 CSV; it now stops the knit, because a table that silently prints last
  # year's numbers is worse than a render that fails.
  vals <- unlist(m[, -1])
  if (mean(vals %in% c("-", "")) > 0.9)
    stop("ft_table3: keying unresolved -- >90% of cells are empty. ",
         "Check .T3COLS / .samp_id against the sample and TCHLvel values in ",
         "output/estimations/CropID_Pooled_OwnLnd_TL_hnormal_optimal.rds.",
         call. = FALSE)

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

# ---- Table 4: ownership-detail parity (estimation objects) -------------------
# Rows are ownership-detail categories: documentation, rights, acquisition,
# sharecropping. They come from TWO different places in the estimation output,
# and that is not a stylistic choice -- it is what 004 emits.
#
#   documentation / rights   SEPARATE FRONTIERS. 004 fits them via
#                            technology_variables = c("OwnLnd","LndOwn","LndRgt"),
#                            so they live in CropID_Pooled_LndOwn_*.rds and
#                            CropID_Pooled_LndRgt_*.rds, in `ef_mean`, keyed by
#                            TCHLvel. Level 1 is the reference (not owned / no
#                            rights) and carries no gap row, which is exactly the
#                            footnote's "relative to matched non-owners".
#
#   acquisition / sharecrop  DISAGGREGATIONS WITHIN the OwnLnd frontier, in
#                            `disagscors`, keyed by disagscors_var / _level.
#
# WEIGHTING IS NOT UNIFORM, AND CANNOT BE (2026-07-15):
#   ef_mean      stat = mean | median | mode | WMEAN
#   disagscors   stat = mean | median | mode          <- no weighted variant
# The v001 draft used `wmean` for documentation/rights (reproduced exactly:
# LndOwn TCHLvel=2 MTE teBC/Restricted/robust_mahalanobis/wmean = -0.005220
# (0.002938), p = 0.0787 -> the draft's "-0.005* (0.003)"), and `mean` for
# acquisition, because that is the only statistic disagscors carries. So the
# table mixes weighted and unweighted blocks. That was true of the published
# draft too; it is now stated in the footnote rather than left implicit.
# To make it uniform, 004 would have to emit wmean into disagscors.
.T4_STAT_EF   <- "wmean"   # documentation / rights   (ef_mean)
.T4_STAT_DISA <- "mean"    # acquisition / sharecrop  (disagscors; only option)

# Table 4 as DATA: label + tgr/te/mte cells, panel headers as rows with empty
# cells. ft_table4() formats this; .live_table("table4") serves the same build
# to tbl_num(), so the 27 inline citations in sections 1, 5 and 6 cannot drift
# from the printed table.
# Category labels per data-raw/okwaayeli_DATA.do (lab define LndOwn / LndRgt /
# LndAq / ShrCrpCat). Level 1 is the reference in the two frontier blocks and
# therefore has no row.
#
# File scope, not a closure: Table S6 reuses these, and a second copy is how the
# two tables would drift apart.
.T4_LAB <- list(
  LndOwn = c("2" = "Owned without documentation", "3" = "Owned with documentation"),
  LndRgt = c("2" = "Can be used as collateral security", "3" = "Can be sold",
             "4" = "Can be used as collateral security and sold"),
  LndAq = c("1" = "Use free of charge", "2" = "Sharecropping",
            "3" = "Rented (cash or in kind)", "4" = "Purchased",
            "5" = "Distributed by village/family", "6" = "Other (GLSS3-4 only)"),
  ShrCrpCat = c("1" = "0%", "2" = "1-49%", "3" = "50-100%"))

# Gap rows from a tenure-detail frontier: TCHLvel 2..n against the level-1
# reference. `type` carries TGR / TE / MTE (and TE0, unused here).
#
#   tag      "LndOwn" | "LndRgt" -- which frontier to read
#   dim_name the panel header row's label
#   survey   "GLSS0" = pooled across waves; else "GLSS5".."GLSS7"
#   matched  TRUE  -> the optimal matched sample (Table 4)
#            FALSE -> sample == "unmatched"  (Table S6)
.tenure_rows <- function(tag, dim_name, survey = "GLSS0", matched = TRUE) {
  e <- try(.read_est(tag)$ef_mean, silent = TRUE)
  if (inherits(e, "try-error") || is.null(e)) return(NULL)
  e <- e[e$estType %in% "teBC" & e$Survey %in% survey &
         e$restrict %in% "Restricted" & e$stat %in% .T4_STAT_EF &
         e$CoefName %in% "efficiencyGap_lvl", ]
  e <- if (isTRUE(matched)) e[!e$sample %in% "unmatched", ]
       else                 e[e$sample %in% "unmatched", ]
  if (!nrow(e)) return(NULL)
  labs <- .T4_LAB[[tag]]
  out <- list(c(dim_name, "", "", ""))
  for (lv in names(labs)) {
    d <- e[as.character(e$TCHLvel) %in% lv, ]
    if (!nrow(d)) next
    v <- vapply(c("TGR", "TE", "MTE"), function(mm) {
      b <- d[d$type %in% mm, ]
      .cell(b$Estimate[1], b$Estimate.sd[1], b$jack_pv[1])
    }, character(1))
    out[[length(out) + 1]] <- c(labs[[lv]], v)
  }
  if (length(out) < 2) NULL else out
}

.tbl4_data <- function() {
  res <- .read_est("OwnLnd")$disagscors
  res$disasg <- as.character(res$disagscors_var)
  res$level  <- as.character(res$disagscors_level)
  res <- res[res$estType %in% "teBC" & res$Survey %in% "GLSS0" &
             res$restrict %in% "Restricted" & res$stat %in% .T4_STAT_DISA &
             !res$sample %in% "unmatched" &
             res$CoefName %in% "disag_efficiencyGap_lvl", ]
  if (nrow(res) == 0) stop("table4 keying unresolved (no disagscors rows)")
  DIMS <- c("Farmland mode of acquisition" = "LndAq",
            "Sharecropping share of land (%)" = "ShrCrpCat")
  METS <- c("TGR", "TE", "MTE")
  rows <- list()

  # Documentation and rights: from their OWN frontiers, not from the draft.
  # These were read out of data/tables/table4.csv until 2026-07-15 on the stated
  # grounds that "these categories are not in the OwnLnd disaggregation object"
  # -- true, but they are in CropID_Pooled_LndOwn_*.rds and
  # CropID_Pooled_LndRgt_*.rds, which nothing was reading. 301 hit the same wall,
  # which is why objs$tenures$documents / $rights carried nulls for `none` and
  # `gap` while section 5 narrated them from the draft.
  for (blk in list(.tenure_rows("LndOwn", "Farmland ownership documentation"),
                   .tenure_rows("LndRgt", "Farmland ownership rights")))
    if (!is.null(blk)) rows <- c(rows, blk)
  if (!length(rows))
    stop("table4 keying unresolved (no LndOwn/LndRgt gap rows)")

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
  m
}

ft_table4 <- function() {
  m <- .tbl4_data()
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
    "Documentation and rights: level difference of each category relative to matched farmers in the reference category (not owned; no rights), estimated on that category's own meta-stochastic frontier. Scores are sampling-weighted means.",
    "Acquisition and sharecropping: matched-sample ownership gap (no ownership minus some ownership) within each category, from the disaggregated scores of the ownership frontier. Scores are unweighted means; the disaggregated output carries no weighted statistic.",
    "The two groups of blocks therefore differ in weighting. This reflects what the estimation routine reports, not a difference in the samples.",
    "Meta Stochastic Frontier Analysis jointly performed on Ghana Living Standards Survey [waves 3-7]."))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

# ---- Supplementary tables -----------------------------------------------------
.HC_NOTE <- c("Cells are headcount ratios over 2012/13-2016/17.",
              .TENURE_WINDOW_NOTE,
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

# ---- Tables S5 / S7: frontier coefficients ----------------------------------
# Row vocabulary, recovered from unique(sf_estm$CoefName) on 2026-07-16. These
# maps are display-label -> CoefName and are exact; the only thing standing
# between them and a live S5/S7 is the column keying (see .S57_COLS below).
#
# Row ORDER reproduces the v001 draft, including its quirk of listing the lnI6
# cross-term before lnI5 within each block (lnI1*lnI6 then lnI1*lnI5, and so on).
# That ordering is arbitrary but published; preserving it keeps the tables
# diffable against the draft.
.S5_ROWS <- list(
  list(hdr = "Production function"),
  list(lab = "Land [lnI1]",              cf = "lnI1"),
  list(lab = "Planting material [lnI2]", cf = "lnI2"),
  list(lab = "Family labor [lnI3]",      cf = "lnI3"),
  list(lab = "Hired labor [lnI4]",       cf = "lnI4"),
  list(lab = "Fertilizer [lnI5]",        cf = "lnI5"),
  list(lab = "Pesticide [lnI6]",         cf = "lnI6"),
  list(lab = "1/2 * lnI1 * lnI1", cf = "I(1/2 * lnI1 * lnI1)"),
  list(lab = "lnI1*lnI2", cf = "lnI1:lnI2"),
  list(lab = "lnI1*lnI3", cf = "lnI1:lnI3"),
  list(lab = "lnI1*lnI4", cf = "lnI1:lnI4"),
  list(lab = "lnI1*lnI6", cf = "lnI1:lnI6"),
  list(lab = "lnI1*lnI5", cf = "lnI1:lnI5"),
  list(lab = "1/2 * lnI2 * lnI2", cf = "I(1/2 * lnI2 * lnI2)"),
  list(lab = "lnI2*lnI3", cf = "lnI2:lnI3"),
  list(lab = "lnI2*lnI4", cf = "lnI2:lnI4"),
  list(lab = "lnI2*lnI6", cf = "lnI2:lnI6"),
  list(lab = "lnI2*lnI5", cf = "lnI2:lnI5"),
  list(lab = "1/2 * lnI3 * lnI3", cf = "I(1/2 * lnI3 * lnI3)"),
  list(lab = "lnI3*lnI4", cf = "lnI3:lnI4"),
  list(lab = "lnI3*lnI6", cf = "lnI3:lnI6"),
  list(lab = "lnI3*lnI5", cf = "lnI3:lnI5"),
  list(lab = "1/2 * lnI4 * lnI4", cf = "I(1/2 * lnI4 * lnI4)"),
  list(lab = "lnI4*lnI6", cf = "lnI4:lnI6"),
  list(lab = "lnI4*lnI5", cf = "lnI4:lnI5"),
  list(lab = "1/2 * lnI5 * lnI5", cf = "I(1/2 * lnI5 * lnI5)"),
  list(lab = "lnI5*lnI6", cf = "lnI5:lnI6"),
  list(lab = "1/2 * lnI6 * lnI6", cf = "I(1/2 * lnI6 * lnI6)"),
  # base = maize, hence no Area_Maize row.
  #
  # Palm and Pepper ARE printed here; the v001 draft omitted them with no stated
  # reason. Both are estimated, both are in the crop_list the frontier was fitted
  # over, and a reader comparing this block to Table 1's crop rows would notice
  # two missing. "Other" last so the residual category reads as a residual.
  list(hdr = "Proportion of area under listed crop (base=maize)"),
  list(lab = "Cassava",  cf = "Area_Cassava"),
  list(lab = "Peanut",   cf = "Area_Peanut"),
  list(lab = "Plantain", cf = "Area_Plantain"),
  list(lab = "Rice",     cf = "Area_Rice"),
  list(lab = "Millet",   cf = "Area_Millet"),
  list(lab = "Sorghum",  cf = "Area_Sorghum"),
  list(lab = "Beans",    cf = "Area_Beans"),
  list(lab = "Yam",      cf = "Area_Yam"),
  list(lab = "Cocoa",    cf = "Area_Cocoa"),
  list(lab = "Palm",     cf = "Area_Palm"),
  list(lab = "Pepper",   cf = "Area_Pepper"),
  list(lab = "Other",    cf = "Area_Other"),
  list(hdr = "Ecological zone [base = Coastal Savannah]"),
  list(lab = "Forest",         cf = "factor(Ecozon)Forest Zone"),
  list(lab = "Guinea Savanah", cf = "factor(Ecozon)Guinea Savanah"),
  list(lab = "Sudan Savanah",  cf = "factor(Ecozon)Sudan Savanah"),
  list(lab = "Transitional",   cf = "factor(Ecozon)Transitional Zone"),
  list(lab = "Intercept",      cf = "(Intercept)"),
  # Zv_ is the production RISK (noise, v) function; Zu_ is inefficiency and
  # belongs to S7.
  list(hdr = "Production risk function"),
  list(lab = "Intercept", cf = "Zv_(Intercept)")
)

# S7: determinants of technical inefficiency -- the Zu_ block.
#
# TWO DEPARTURES FROM THE v001 DRAFT, both corrections:
#
#   Age / education labels. The draft called Zu_lnAgeYr "Age (years)" and
#   Zu_lnYerEdu "Education (years)". Both regressors are lnAgeYr and lnYerEdu --
#   LOGGED -- so the coefficients are elasticities, not per-year effects, and the
#   draft's labels invite the reader to interpret 0.110 as "a year of age raises
#   inefficiency by 0.11". Relabelled to say the unit, with a footnote as well.
#
#   Credit. Zu_factor(Credit)1 is estimated and the draft did not print it. It is
#   printed here, beside the other access dummies where it belongs.
#
# .s57_report_omitted() should now be silent for this table. If it names
# something, the model gained a regressor and this map has not kept up.
.S7_ROWS <- list(
  list(lab = "Female farmer (dummy)",         cf = "Zu_factor(Female)1"),
  list(lab = "Age (log years)",               cf = "Zu_lnAgeYr"),
  list(lab = "Education (log years)",         cf = "Zu_lnYerEdu"),
  list(lab = "Crop diversification (index)",  cf = "Zu_CrpMix"),
  list(lab = "Mechanization (dummy)",         cf = "Zu_factor(EqipMech)1"),
  list(lab = "Extension (dummy)",             cf = "Zu_factor(Extension)1"),
  list(lab = "Credit (dummy)",                cf = "Zu_factor(Credit)1"),
  list(hdr = "Ecological zone [base = Coastal Savannah]"),
  list(lab = "Forest",         cf = "Zu_factor(Ecozon)Forest Zone"),
  list(lab = "Guinea Savanah", cf = "Zu_factor(Ecozon)Guinea Savanah"),
  list(lab = "Sudan Savanah",  cf = "Zu_factor(Ecozon)Sudan Savanah"),
  list(lab = "Transitional",   cf = "Zu_factor(Ecozon)Transitional Zone"),
  list(lab = "Intercept",      cf = "Zu_(Intercept)")
)

# Column keying: .T3COLS minus the `gap` column. Verified against the data, not
# inferred -- for CoefName "lnI1" at GLSS0/Restricted the five columns take five
# distinct values, which pins the mapping:
#
#   TCHLvel   sample               Estimate
#   National  unmatched            0.7560089   naive
#   0         unmatched            0.7212759   no land ownership
#   1         unmatched            0.7797309   some land ownership
#   Meta      robust_mahalanobis   0.7267899   matched
#   Meta      unmatched            0.7292953   unmatched
#
# Key on TCHLvel, never Tech. Tech 1 is TCHLvel "0" (non-owners), so keying on
# Tech and assuming 1 = first column transposes "no" and "some ownership" --
# every cell populated, every star right, the table saying the opposite.
.S57_COLS <- list(
  naive  = list(samp = "unmatched", lv = "National"),
  none   = list(samp = "unmatched", lv = "0"),
  any    = list(samp = "unmatched", lv = "1"),
  meta_m = list(samp = "matched",   lv = "Meta"),
  meta_u = list(samp = "unmatched", lv = "Meta")
)

.s57_cell <- function(sf, cf, cc) {
  b <- sf[sf$CoefName %in% cf & as.character(sf$TCHLvel) %in% cc$lv &
          sf$sample %in% .samp_id(cc$samp), ]
  if (!nrow(b)) return("-")
  .cell(b$Estimate[1], b$Estimate.sd[1], b$jack_pv[1])
}

# Drift detector: coefficients the model estimates that this table does not
# print. Should name NOTHING. If it does, the frontier gained a regressor and
# .S5_ROWS/.S7_ROWS have not kept up -- which is how the v001 draft silently
# lost Area_Palm, Area_Pepper and Zu_factor(Credit)1.
.s57_report_omitted <- function(sf, rowspec, pattern, label) {
  mapped <- vapply(Filter(function(r) !is.null(r$cf), rowspec),
                   function(r) r$cf, character(1))
  have   <- unique(as.character(sf$CoefName))
  extra  <- setdiff(grep(pattern, have, value = TRUE), c(mapped, "Area_Maize"))
  if (length(extra))
    message(label, ": ", length(extra), " coefficient(s) estimated but not ",
            "printed (the v001 draft omitted them): ",
            paste(extra, collapse = ", "))
  invisible(extra)
}

# label + c1..c6, c4 blank (the gutter between Group and Meta-frontier spanners).
.tblS57_data <- function(rowspec, pattern, label) {
  sf <- .read_est("OwnLnd")$sf_estm
  sf <- sf[sf$restrict %in% "Restricted" & sf$Survey %in% "GLSS0", ]
  if (!nrow(sf)) stop(label, ": no Restricted/GLSS0 rows in sf_estm", call. = FALSE)
  .s57_report_omitted(sf, rowspec, pattern, label)

  rows <- lapply(rowspec, function(r) {
    if (!is.null(r$hdr)) return(c(r$hdr, "", "", "", "", "", ""))
    v <- vapply(.S57_COLS, function(cc) .s57_cell(sf, r$cf, cc), character(1))
    c(r$lab, v[["naive"]], v[["none"]], v[["any"]], "",
      v[["meta_m"]], v[["meta_u"]])
  })
  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c("label", "c1", "c2", "c3", "c4", "c5", "c6")
  # A keying break empties the table rather than erroring. Same guard as Table 3.
  vals <- unlist(m[!m$c1 %in% "" | !m$c2 %in% "", c("c1", "c2", "c3", "c5", "c6")])
  if (length(vals) && mean(vals %in% c("-", "")) > 0.9)
    stop(label, ": keying unresolved -- >90% of cells are empty. Check ",
         ".S57_COLS against unique(sf_estm$TCHLvel) and unique(sf_estm$sample).",
         call. = FALSE)
  m
}

.s57_flextable <- function(m, size) {
  hdr <- which(m$c1 == "" & m$c2 == "" & m$c3 == "")
  ft <- flextable(m)
  ft <- set_header_labels(ft, label = "",
    # i-diaeresis via intToUtf8: keeps this file ASCII. See exhibits-figures.R.
    c1 = paste0("Na", intToUtf8(239L), "ve national frontier"),
    c2 = "No land ownership", c3 = "Some land ownership", c4 = "",
    c5 = "Matched", c6 = "Unmatched")
  ft <- add_header_row(ft, top = TRUE,
                       values = c("", "", "Group frontier", "", "Meta-frontier"),
                       colwidths = c(1, 1, 2, 1, 2))
  ft <- bold(ft, i = hdr, j = 1, part = "body")
  ft <- bold(ft, part = "header")
  ft <- align(ft, j = 2:7, align = "right", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- align(ft, i = 1, align = "center", part = "header")
  ft <- padding(ft, i = setdiff(seq_len(nrow(m)), hdr), j = 1,
                padding.left = 8, part = "body")
  ft <- padding(ft, padding.top = 0, padding.bottom = 0, part = "all")
  ft <- line_spacing(ft, space = 1, part = "all")
  ft <- fontsize(ft, size = size, part = "all")
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft
}

.tblS5_data <- function() .tblS57_data(.S5_ROWS, "^Area_", "tableS5")
.tblS7_data <- function() .tblS57_data(.S7_ROWS, "^Zu_",   "tableS7")

ft_tableS5 <- function() {
  ft <- .s57_flextable(.tblS5_data(), size = 7)
  ft <- add_footer_lines(ft, values = .MSF_NOTE)
  ft <- fontsize(ft, size = 6, part = "footer")
  ft
}
# Table S6: Table 4's documentation and rights blocks on the UNMATCHED sample,
# repeated per wave. Same .tenure_rows() as Table 4 -- only `matched` and
# `survey` differ -- so the two tables cannot drift in labels or keying.
#
# Waves: the pooled block plus GLSS5-GLSS7. The v001 draft carried exactly these
# four and no GLSS3/GLSS4, which is consistent with the tenure-detail modules
# (Section 8b's documentation and rights questions) not being administered in the
# earlier rounds. We emit whichever of the four the estimation object actually
# has rather than asserting the list.
.S6_WAVES <- c("GLSS3-GLSS7" = "GLSS0", "GLSS7" = "GLSS7",
               "GLSS6" = "GLSS6", "GLSS5" = "GLSS5")

.tblS6_data <- function() {
  rows <- list()
  for (nm in names(.S6_WAVES)) {
    sv  <- .S6_WAVES[[nm]]
    blk <- list(.tenure_rows("LndOwn", "Land ownership status",
                             survey = sv, matched = FALSE),
                .tenure_rows("LndRgt", "Farmland ownership rights",
                             survey = sv, matched = FALSE))
    blk <- Filter(Negate(is.null), blk)
    if (!length(blk)) next
    rows[[length(rows) + 1]] <- c(nm, "", "", "")   # wave header
    for (b in blk) rows <- c(rows, b)
  }
  if (!length(rows))
    stop("tableS6: keying unresolved -- no unmatched LndOwn/LndRgt gap rows in ",
         "ef_mean. Check that 004 emits sample == \"unmatched\" for the ",
         "tenure-detail frontiers.", call. = FALSE)
  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c("item", "tgr", "te", "mte")
  m
}

ft_tableS6 <- function() {
  m   <- .tblS6_data()
  hdr <- which(m$tgr == "" & m$te == "" & m$mte == "")
  # Wave rows are the outer level, dimension rows the inner one.
  wave <- which(m$item %in% names(.S6_WAVES))
  dim_ <- setdiff(hdr, wave)
  ft <- flextable(m)
  ft <- set_header_labels(ft, item = "",
                          tgr = "Technology gap ratio (TGR)",
                          te  = "Pure farmer technical efficiency (TE)",
                          mte = "Meta-frontier technical efficiency (MTE)")
  ft <- add_header_row(ft, top = TRUE,
                       values = c("", "Level difference (unmatched estimates)"),
                       colwidths = c(1, 3))
  ft <- bold(ft, i = hdr, j = 1, part = "body")
  ft <- italic(ft, i = dim_, j = 1, part = "body")
  ft <- bold(ft, part = "header")
  ft <- align(ft, j = 2:4, align = "right", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- align(ft, i = 1, align = "center", part = "header")
  ft <- padding(ft, i = dim_, j = 1, padding.left = 8, part = "body")
  ft <- padding(ft, i = setdiff(seq_len(nrow(m)), hdr), j = 1,
                padding.left = 16, part = "body")
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  ft <- fontsize(ft, size = 9, part = "all")
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft <- add_footer_lines(ft, values = c(
    paste(.SIG_NOTE, "Jackknife standard errors in parentheses."),
    "Unmatched-sample counterpart of Table 4: level difference of each category relative to farmers in the reference category (not owned; no rights), estimated on that category's own meta-stochastic frontier. Scores are sampling-weighted means.",
    "Because these are unmatched, the comparison groups are not balanced on observables; read alongside Table 4 rather than instead of it.",
    "Meta Stochastic Frontier Analysis jointly performed on Ghana Living Standards Survey [waves 3-7]."))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

ft_tableS7 <- function() {
  ft <- .s57_flextable(.tblS7_data(), size = 9)
  ft <- add_footer_lines(ft, values = c(
    paste(.SIG_NOTE, "Jackknife standard errors in parentheses."),
    "A positive coefficient means the variable moves the farm away from its efficient frontier.",
    "Age and education enter the inefficiency function in logs; their coefficients are therefore elasticities, not per-year effects.",
    "Data source: Ghana Living Standards Survey [waves 3-7]."))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

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
# Tables 1, 2 and S1-S4 are rebuilt from data/descriptive_exhibits.rds; table4
# is rebuilt from the estimation objects via .tbl4_data(). Only S5-S7 still read
# a curated CSV -- and S0, which is curated by design.
.LIVE_IDS <- c("table1", "table2", "table4",
               "tableS1", "tableS2", "tableS3", "tableS4")

# Table 4 in the label/header/c1..c3 shape tbl_num() expects. `header` marks the
# panel rows ("Farmland ownership documentation" etc.), which carry no cells and
# must not be matched as data rows.
.tbl4_live <- function() {
  m <- .tbl4_data()
  data.frame(
    label  = m$item,
    header = ifelse(m$tgr == "" & m$te == "" & m$mte == "", "1", "0"),
    c1     = m$tgr, c2 = m$te, c3 = m$mte,
    stringsAsFactors = FALSE)
}

# Cached per id: this is the hot path. 138 tbl_num()/tbl_pct() calls resolve to
# 7 distinct tables, so 7 builds instead of 138. The built frames are a few KB
# each; what they cost to produce is the estimation objects behind them.
.live_table <- function(id) {
  key <- sub("\\.csv$", "", id)
  .memo(paste0("tbl:", key), function()
    switch(key,
      table1  = .tbl1_live(),
      table2  = .tbl2_live(),
      table4  = .tbl4_live(),
      tableS1 = .tblS_live(c("LndOwn_1", "LndOwn_2", "LndOwn_3")),
      tableS2 = .tblS_live(c("LndAq_5", "LndAq_1", "LndAq_2", "LndAq_3", "LndAq_4")),
      tableS3 = .tblS_live(c("LndRgt_1", "LndRgt_2", "LndRgt_3", "LndRgt_4")),
      tableS4 = .tblS_live(c("ShrCrpCat_1", "ShrCrpCat_2", "ShrCrpCat_3")),
      .read_tbl(id)))
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
  # `roweq` collision survived for years. The curated CSVs (S5-S7) are a
  # different case: first-match has always been the contract there.
  #
  # table4 is now rebuilt too. Its labels are unique across the four panels --
  # .T4_LAB gives each category its own string -- so the guard should never fire.
  # If it does, two panels have collided and the table is wrong.
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

# Matched input/output gaps behind Figure 1 (output/figures/input_TE_data.csv).
# v2 layout: a plot and the data behind it share output/figures/. Resolved from
# .STUDY_ROOT rather than study_dir_figures() because this file is sourced by the
# Rmd at knit time, when there is no study_environment in scope.
.FIGDAT <- file.path(.STUDY_ROOT, "output", "figures")
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
    warning("exhibit_helpers_tables.R: could not read sample sizes from ", .XLSX,
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
