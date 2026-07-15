# 305_tables.R
# Build manuscript tables as flextable objects. Architecture ported from
# studies/resource_extraction/scripts/305_tables.R.
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
  stop("305_tables.R could not load 'flextable'.\n",
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
  if (!file.exists(p)) stop("305_tables.R: missing ", p, call. = FALSE)
  utils::read.csv(p, check.names = FALSE, colClasses = "character",
                  encoding = "UTF-8")
}
.read_hdr <- function(nm) {
  p <- file.path(.TBL_DIR, nm)
  if (!file.exists(p)) stop("305_tables.R: missing ", p, call. = FALSE)
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

# Generic builder: label + value columns from CSV, labels from *_header.csv,
# optional top spanner row.
.ft_csv <- function(csv, hdr_csv, first_lab = "Variable", size = 8,
                    spanner = NULL, spanwidths = NULL, notes = NULL) {
  d <- .read_tbl(csv)
  hdr <- which(d$header == "1")
  cols <- .read_hdr(hdr_csv)
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

.SRC_NOTE <- "Data source: Ghana Living Standards Survey [waves 3-7]."
.SIG_NOTE <- "Significance levels: * p<0.10, ** p<0.05, *** p<0.01."

# ---- Main-text tables --------------------------------------------------------
ft_table1 <- function()
  .ft_csv("table1.csv", "table1_header.csv", size = 8,
    spanner = c("", "Sample means (sample standard deviation)",
                "Annual trend from 1991 to 2017 (%)"),
    spanwidths = c(1, 3, 3),
    notes = c(.SIG_NOTE,
      "Standard deviations in parentheses; standard errors in brackets. A dagger denotes a statistically significant difference from the pooled sample.",
      "The trend was estimated as the annual percentage change via a generalised linear model.",
      .SRC_NOTE))

ft_table2 <- function()
  .ft_csv("table2.csv", "table2_header.csv", first_lab = "Outcome", size = 8,
    spanner = c("", "Mean (standard deviation)", ""),
    spanwidths = c(1, 2, 1),
    notes = c("Standard deviations in parentheses.",
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
  res <- .read_est("OwnLnd")$disagscors
  res$disasg <- as.character(res$disagscors_var)
  res$level  <- as.character(res$disagscors_level)
  res <- res[res$estType %in% "teBC" & res$Survey %in% "GLSS0" &
             res$restrict %in% "Restricted" & res$stat %in% "mean" &
             !res$sample %in% "unmatched" &
             res$CoefName %in% "disag_efficiencyGap_lvl", ]
  if (nrow(res) == 0) stop("table4 keying unresolved (no disagscors rows)")
  DIMS <- c("Land ownership status" = "LndOwn",
            "Farmland ownership rights" = "LndRgt",
            "Farmland mode of acquisition" = "LndAq",
            "Sharecropping share" = "ShrCrpCat")
  METS <- c("TGR", "TE", "MTE")
  rows <- list()
  for (dn in names(DIMS)) {
    d <- res[res$disasg %in% DIMS[[dn]], ]
    if (nrow(d) == 0) next
    rows[[length(rows) + 1]] <- c(dn, "", "", "")
    for (lv in unique(d$level)) {
      dd <- d[d$level %in% lv, ]
      v <- vapply(METS, function(mm) {
        b <- dd[dd$input %in% mm, ]
        .cell(b$Estimate[1], b$Estimate.sd[1], b$jack_pv[1])
      }, character(1))
      rows[[length(rows) + 1]] <- c(lv, v)
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
    "Cells are matched-sample level differences (no ownership minus some ownership) in each metric.",
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

ft_tableS1 <- function()
  .ft_csv("tableS1.csv", "tableS1_header.csv", first_lab = "Crop", size = 8,
    spanner = c("", "Headcount ratio over 2012/13-2016/17"), spanwidths = c(1, 3),
    notes = .HC_NOTE)
ft_tableS2 <- function()
  .ft_csv("tableS2.csv", "tableS2_header.csv", first_lab = "Crop", size = 8,
    spanner = c("", "Headcount ratio over 2012/13-2016/17"), spanwidths = c(1, 5),
    notes = .HC_NOTE)
ft_tableS3 <- function()
  .ft_csv("tableS3.csv", "tableS3_header.csv", first_lab = "Crop", size = 8,
    spanner = c("", "Headcount ratio over 2012/13-2016/17"), spanwidths = c(1, 4),
    notes = .HC_NOTE)
ft_tableS4 <- function()
  .ft_csv("tableS4.csv", "tableS4_header.csv", first_lab = "Crop", size = 8,
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
