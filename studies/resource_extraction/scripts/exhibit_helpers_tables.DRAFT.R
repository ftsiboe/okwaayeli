# =============================================================================
# exhibit_helpers_tables.R  —  DESCRIPTIVE BUILDER MODULE  (DRAFT, untested)
# =============================================================================
# Step 2 of the AGENT_PROMPT refactor: the descriptive tables (1, 2, A2, A3)
# built live off data/descriptive_exhibits.rds instead of the frozen
# data/tables/*.csv. Ported from studies/land_tenure/scripts/exhibit_helpers_tables.R.
#
# HOW TO INTEGRATE (do this only AFTER parity_check_descriptive.R comes back
# clean — see the run order in the chat):
#   1. Rename scripts/305_tables.R  ->  scripts/exhibit_helpers_tables.R
#      (a number promises a position in a sequence; a library has none).
#   2. In that file, DELETE the CSV-driven descriptive code:
#        - .read_hdr()            (only served .ft_wide)
#        - .ft_wide()             (CSV-driven wide builder)
#        - the CSV versions of ft_table1(), ft_table2(), ft_tableA2(), ft_tableA3()
#        - the "Descriptive tables come from the Stata path / TODO rebuild" note
#      Then PASTE the code below in their place.
#   3. KEEP (do NOT delete):
#        - .read_tbl()            !!! STILL NEEDED — .ft_msf() (Tables A4–A9) and
#                                 ft_tableA1() read frozen CSVs today; A4–A9 are
#                                 NOT yet live from estimation objects. Deleting
#                                 .read_tbl() breaks them. (Making A4–A9 live is
#                                 separate, later work.)
#        - .ft_msf(), ft_tableA4()–ft_tableA9(), ft_tableA1()  (analytical/curated)
#        - ft_table3(), ft_table4() and their helpers  (estimation objects; these
#          key on the LABELLED column, never the numeric one — leave untouched)
#        - infrastructure reused below: .STUDY_ROOT, .style_desc(), .stars(),
#          library(flextable). fmt_num() comes from article_helpers.R (the
#          renamed 300_article_helpers.R).
#   4. Update the source() of this file in narrative/resource-extraction.Rmd and
#      in scripts/301_article_objects.R (305_tables.R -> exhibit_helpers_tables.R),
#      and this file's own self-identifying stop() strings.
#      (302_render_article.R, run_article.R, 100_exhibits.R, 001_*.R do NOT
#      reference 305_tables by name — grep-confirmed.)
#
# ⚠ ONE DECISION FOR YOU (Table A3 column c1):
#   The frozen tables are internally inconsistent. tableA2.csv c1 is the
#   extraction_any POOLED mean (header "Pooled (n=26811)"), but tableA3.csv c1 is
#   the extraction_any "any"-GROUP trend WITH a trend_diff dagger — not the
#   pooled trend. This module makes A3 c1 = pooled (matching A2 and the header),
#   which is the consistent reading. If you actually want A3 c1 to stay the
#   "any" trend, change the c1 group in .tblA_live() for kind "A3". Confirm
#   against a freshly built descriptive_exhibits.rds either way.
#
# Cache schema (fixed): list(table1=<t1 long>, shares=<t2 long>, meta=...).
#   t1 cols: study,treatment,crop,outcome,family,wave,group,statistic,estimate,
#            se,t,p,min,max,sd,n. Always key wave=="all"; group∈{pooled,0,1};
#            statistic∈{mean,trend_pct,cat_diff,trend_diff}; cat/ trend_diff have
#            group=NA (do NOT key group for those).
#   t2 cols: outcome(=indicator),wave,group,statistic,estimate,se,min,max,sd,n,crop.
#            share: wave=="pooled",statistic=="mean"; trend: wave=="trend",
#            statistic=="trend_pct".
# =============================================================================

# ---- cache + keyed lookup ---------------------------------------------------
.CACHE <- new.env(parent = emptyenv())
.memo <- function(key, f) {
  if (!exists(key, envir = .CACHE, inherits = FALSE))
    assign(key, f(), envir = .CACHE)
  get(key, envir = .CACHE)
}
exhibit_cache_clear <- function() {
  rm(list = ls(.CACHE, all.names = TRUE), envir = .CACHE)
  invisible(TRUE)
}

.DESC <- file.path(.STUDY_ROOT, "data", "descriptive_exhibits.rds")
.desc <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      if (!file.exists(.DESC))
        stop("exhibit_helpers_tables.R: missing ", .DESC,
             "\n  Run: Rscript studies/resource_extraction/scripts/",
             "100_exhibit_descriptive_stats.R", call. = FALSE)
      cache <<- readRDS(.DESC)
    }
    cache
  }
})

# One value from a long frame; errors on a duplicate key (a keyed schema must
# never yield two rows for one cell — taking the first hides the bug).
.pick_desc <- function(d, keys, col) {
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

# ---- formatting -------------------------------------------------------------
# .stars: ***<.01 **<.05 *<.10 (same as the existing 305_tables .stars).
.stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}
.DAG_ALPHA <- 0.05
.dagger <- function(p) if (!is.na(p) && p < .DAG_ALPHA) " †" else ""

.fmt_mean_sd <- function(est, sd, digits = 2L, dagger = "", missing = "") {
  if (is.na(est)) return(missing)
  sprintf("%.*f (%.*f)%s", digits, est, digits, sd, dagger)
}
.fmt_trend <- function(est, se, p, digits = 2L, dagger = "", missing = "",
                       stars = TRUE) {
  if (is.na(est)) return(missing)
  sprintf("%.*f%s [%.*f]%s", digits, est, if (stars) .stars(p) else "",
          digits, se, dagger)
}

# >90%-blank guard: if keying is off, nearly every value cell is ""/"-".
.guard_desc <- function(m, vcols, who) {
  vals <- unlist(m[m$header != "1", vcols], use.names = FALSE)
  if (length(vals) && mean(vals %in% c("", "-")) > 0.9)
    stop(who, ": >90% of value cells are blank — keying unresolved against ",
         .DESC, ". Check treatment/outcome/crop/group/wave against the schema.",
         call. = FALSE)
}

# ---- shared row spec: Table 1 / A2 / A3 (identical 35 rows) ------------------
# label (verbatim from the frozen CSVs) -> outcome, crop. header==1 = section
# row, no cells. Non-crop rows use crop "Pooled"; crop block is outcome="Yield".
# Data spelling: Tomato -> "Tomatoe".
.DESC_ROWS <- data.frame(
  label = c(
    "Farmer",
    "Female farmer (dummy)", "Age (years)", "Education (years)",
    "Selected crop production",
    "All crops (real GH₵/ha)",
    "Maize (Kg/ha)", "Rice (Kg/ha)", "Millet (Kg/ha)", "Sorghum (Kg/ha)",
    "Beans (Kg/ha)", "Peanut (Kg/ha)", "Cassava (Kg/ha)", "Yam (Kg/ha)",
    "Cocoyam (Kg/ha)", "Plantain (Kg/ha)", "Pepper (Kg/ha)", "Okra (kg/ha)",
    "Tomato (kg/ha)", "Cocoa (Kg/ha)", "Palm (Kg/ha)",
    "Land (ha)", "Land owned (dummy)", "Crop diversification (index)",
    "Seed (real GH₵/ha)", "Household labor (AE)",
    "Hired labor (man-days/ha)", "Fertilizer (Kg/ha)", "Pesticide (Liter/ha)",
    "Mechanization (dummy)", "Irrigation (dummy)", "Credit (dummy)",
    "Household",
    "Size (AE)", "Dependency (ratio)"),
  header = c(1, 0,0,0, 1, rep(0, 16), rep(0, 11), 1, 0,0),
  outcome = c(NA,
    "Female", "AgeYr", "YerEdu",
    NA,
    "Yield","Yield","Yield","Yield","Yield","Yield","Yield","Yield","Yield",
    "Yield","Yield","Yield","Yield","Yield","Yield","Yield",
    "Area", "OwnLnd", "CrpMix", "SeedKg", "HHLaborAE", "HirdHr", "FertKg",
    "PestLt", "EqipMech", "EqipIrig", "Credit",
    NA,
    "HHSizeAE", "Depend"),
  crop = c(NA,
    "Pooled","Pooled","Pooled",
    NA,
    "Pooled","Maize","Rice","Millet","Sorghum","Beans","Peanut","Cassava",
    "Yam","Cocoyam","Plantain","Pepper","Okra","Tomatoe","Cocoa","Palm",
    "Pooled","Pooled","Pooled","Pooled","Pooled","Pooled","Pooled","Pooled",
    "Pooled","Pooled","Pooled",
    NA,
    "Pooled","Pooled"),
  stringsAsFactors = FALSE)

.DESC_TREATS <- c("extraction_any", "mining_any", "mining_comm", "mining_gala",
                  "quarrying", "sand", "salt")

# ---- Table 1 (treatment "extraction_any") -----------------------------------
.tbl1_live <- function() {
  t1  <- .desc()$table1
  trt <- "extraction_any"
  mean_cell <- function(eq, cr, grp, dag) {
    k <- list(treatment = trt, outcome = eq, crop = cr, group = grp,
              wave = "all", statistic = "mean")
    .fmt_mean_sd(.pick_desc(t1, k, "estimate"), .pick_desc(t1, k, "sd"),
                 dagger = dag, missing = "")
  }
  trend_cell <- function(eq, cr, grp, dag) {
    k <- list(treatment = trt, outcome = eq, crop = cr, group = grp,
              wave = "all", statistic = "trend_pct")
    .fmt_trend(.pick_desc(t1, k, "estimate"), .pick_desc(t1, k, "se"),
               .pick_desc(t1, k, "p"), dagger = dag, missing = "")
  }
  out <- .DESC_ROWS
  for (cc in paste0("c", 1:6)) out[[cc]] <- ""
  for (i in seq_len(nrow(out))) {
    if (out$header[i] == 1) next
    eq <- out$outcome[i]; cr <- out$crop[i]
    dcat <- .dagger(.pick_desc(t1, list(treatment = trt, outcome = eq, crop = cr,
              wave = "all", statistic = "cat_diff"),   "p"))
    dtrd <- .dagger(.pick_desc(t1, list(treatment = trt, outcome = eq, crop = cr,
              wave = "all", statistic = "trend_diff"), "p"))
    out$c1[i] <- mean_cell(eq, cr, "pooled", "")
    out$c2[i] <- mean_cell(eq, cr, "0", dcat)
    out$c3[i] <- mean_cell(eq, cr, "1", dcat)
    out$c4[i] <- trend_cell(eq, cr, "pooled", "")
    out$c5[i] <- trend_cell(eq, cr, "0", dtrd)
    out$c6[i] <- trend_cell(eq, cr, "1", dtrd)
  }
  out$header <- as.character(out$header)
  out[, c("label", "header", paste0("c", 1:6))]
}

.tbl1_n <- function() {
  t1 <- .desc()$table1
  g <- function(grp) .pick_desc(t1, list(treatment = "extraction_any",
         outcome = "Yield", crop = "Pooled", group = grp, wave = "all",
         statistic = "mean"), "n")
  c(all = g("pooled"), non = g("0"), any = g("1"))
}

ft_table1 <- function() {
  m <- .tbl1_live()
  .guard_desc(m, paste0("c", 1:6), "ft_table1")
  hdr <- which(m$header == "1")
  n   <- .tbl1_n()
  f   <- function(x) format(x, big.mark = ",")
  mm  <- m[, c("label", paste0("c", 1:6))]
  names(mm) <- c("item", "pm", "nm", "am", "pt", "nt", "at")
  ft <- flextable(mm)
  ft <- set_header_labels(ft, item = "Variable",
    pm = sprintf("Pooled (n=%s)", f(n[["all"]])),
    nm = sprintf("No extraction (n=%s)", f(n[["non"]])),
    am = sprintf("Some extraction (n=%s)", f(n[["any"]])),
    pt = sprintf("Pooled (n=%s)", f(n[["all"]])),
    nt = sprintf("No extraction (n=%s)", f(n[["non"]])),
    at = sprintf("Some extraction (n=%s)", f(n[["any"]])))
  ft <- add_header_row(ft, top = TRUE, values = c("", "Mean (SD)", "Trend (%)"),
                       colwidths = c(1, 3, 3))
  ft <- align(ft, j = 2:7, align = "right", part = "all")
  ft <- align(ft, i = 1, align = "center", part = "header")
  ft <- .style_desc(ft, hdr, nrow(mm), size = 8)
  ft <- add_footer_lines(ft, values = c(
    "Significance levels: * p<0.10, ** p<0.05, *** p<0.01.",
    "Standard deviations in parentheses; standard errors in brackets. † denotes a statistically significant difference from the pooled sample.",
    "The trend was estimated as the annual percentage change via a generalised linear model.",
    "Data source: Ghana Living Standards Survey [waves 4-7]."))
  fontsize(ft, size = 6, part = "footer")
}

# ---- Tables A2 (means) / A3 (trend_pct) -------------------------------------
# c1 = extraction_any POOLED (no dagger); c2..c7 = group "1" of the six subtypes
# with that treatment's cat_diff (A2) / trend_diff (A3) dagger. "-" where absent.
.tblA_live <- function(kind) {
  t1      <- .desc()$table1
  is_mean <- identical(kind, "A2")
  stat    <- if (is_mean) "mean"     else "trend_pct"
  dstat   <- if (is_mean) "cat_diff" else "trend_diff"
  groups  <- c("pooled", rep("1", 6))
  cell <- function(trt, eq, cr, grp, dag) {
    k <- list(treatment = trt, outcome = eq, crop = cr, group = grp,
              wave = "all", statistic = stat)
    if (is_mean)
      .fmt_mean_sd(.pick_desc(t1, k, "estimate"), .pick_desc(t1, k, "sd"),
                   dagger = dag, missing = "-")
    else
      .fmt_trend(.pick_desc(t1, k, "estimate"), .pick_desc(t1, k, "se"),
                 .pick_desc(t1, k, "p"), dagger = dag, missing = "-")
  }
  out <- .DESC_ROWS
  for (cc in paste0("c", 1:7)) out[[cc]] <- ""
  for (i in seq_len(nrow(out))) {
    if (out$header[i] == 1) next
    eq <- out$outcome[i]; cr <- out$crop[i]
    for (j in seq_along(.DESC_TREATS)) {
      trt <- .DESC_TREATS[j]; grp <- groups[j]
      dag <- if (j == 1L) "" else
        .dagger(.pick_desc(t1, list(treatment = trt, outcome = eq, crop = cr,
                  wave = "all", statistic = dstat), "p"))
      out[[paste0("c", j)]][i] <- cell(trt, eq, cr, grp, dag)
    }
  }
  out$header <- as.character(out$header)
  out[, c("label", "header", paste0("c", 1:7))]
}
.tblA2_live <- function() .tblA_live("A2")
.tblA3_live <- function() .tblA_live("A3")

.A_HEAD <- function() {
  n <- .pick_desc(.desc()$table1, list(treatment = "extraction_any",
        outcome = "Yield", crop = "Pooled", group = "pooled", wave = "all",
        statistic = "mean"), "n")
  c(sprintf("Pooled (n=%s)", format(n, big.mark = ",")),
    "Any scale of mineral mining", "Commercial mineral mining",
    "Informal or small-scale mineral mining", "Quarrying", "Sand winning",
    "Salt mining")
}

# ---- Table 2 (shares + %-change, crops x 7 activities) ----------------------
# Row order is editorial (verbatim from table2.csv), NOT a sort. Activity is the
# t2 `outcome`; crop is `crop`. "All crops listed" -> "Pooled"; "Tomato" -> "Tomatoe".
.T2_LAB <- c("Banana","Cocoa","Plantain","Tomato","Cassava","All crops listed",
             "Cocoyam","Palm","Millet","Eggplant","Pepper","Sorghum","Maize",
             "Beans","Peanut","Rice","Yam","Okra")
.T2_KEY <- c("Banana","Cocoa","Plantain","Tomatoe","Cassava","Pooled",
             "Cocoyam","Palm","Millet","Eggplant","Pepper","Sorghum","Maize",
             "Beans","Peanut","Rice","Yam","Okra")
.T2_HEAD <- c("Any type of resource extraction", "Any scale of mineral mining",
              "Commercial mineral mining", "Informal or small-scale mineral mining",
              "Quarrying", "Sand winning", "Salt mining")

.tbl2_live <- function() {
  t2 <- .desc()$shares
  emit <- function(head_lab, w, statc, fmt) {
    m <- data.frame(label = c(head_lab, .T2_LAB),
                    header = c("1", rep("0", length(.T2_LAB))),
                    stringsAsFactors = FALSE)
    for (j in seq_along(.DESC_TREATS)) {
      col <- character(nrow(m)); col[1] <- ""
      for (r in seq_along(.T2_LAB)) {
        k <- list(outcome = .DESC_TREATS[j], crop = .T2_KEY[r],
                  wave = w, statistic = statc)
        col[r + 1L] <- fmt(k)
      }
      m[[paste0("c", j)]] <- col
    }
    m
  }
  hc <- emit("Headcount ratio over the periods 1998/99 to 2016/17",
             "pooled", "mean",
             function(k) .fmt_mean_sd(.pick_desc(t2, k, "estimate"),
                                      .pick_desc(t2, k, "sd"),
                                      digits = 3L, missing = "-"))
  ch <- emit("Percentage change in Headcount ratio from 1998/99 to 2016/17",
             "trend", "trend_pct",
             function(k) .fmt_trend(.pick_desc(t2, k, "estimate"),
                                    .pick_desc(t2, k, "se"), NA_real_,
                                    digits = 3L, stars = FALSE, missing = "-"))
  out <- rbind(hc, ch); rownames(out) <- NULL
  out
}

# Generic wide builder (label + 7 value cols + char header), replacing .ft_wide.
.ft_desc_wide <- function(m, cols, notes, first_lab = "Variable", size = 8) {
  stopifnot(length(cols) == 7L)
  hdr <- which(m$header == "1")
  mm  <- m[, c("label", paste0("c", 1:7))]
  ft  <- flextable(mm)
  ft  <- set_header_labels(ft, values = stats::setNames(
           as.list(c(first_lab, cols)), c("label", paste0("c", 1:7))))
  ft  <- align(ft, j = 2:8, align = "right", part = "all")
  ft  <- valign(ft, valign = "bottom", part = "header")
  ft  <- .style_desc(ft, hdr, nrow(mm), size = size)
  ft  <- add_footer_lines(ft, values = notes)
  fontsize(ft, size = 6, part = "footer")
}

ft_table2 <- function() {
  m <- .tbl2_live()
  .guard_desc(m, paste0("c", 1:7), "ft_table2")
  .ft_desc_wide(m, .T2_HEAD, first_lab = "Crop", size = 8,
    notes = c("Standard deviations in parentheses; standard errors in brackets.",
      "Data source: Ghana Living Standards Survey [waves 4-7]."))
}
ft_tableA2 <- function() {
  m <- .tblA2_live()
  .guard_desc(m, paste0("c", 1:7), "ft_tableA2")
  .ft_desc_wide(m, .A_HEAD(), first_lab = "Variable", size = 7,
    notes = c("Standard deviations in parentheses.",
      "A dagger denotes a statistically significant difference from the pooled sample.",
      "Data source: Ghana Living Standards Survey [waves 4-7]."))
}
ft_tableA3 <- function() {
  m <- .tblA3_live()
  .guard_desc(m, paste0("c", 1:7), "ft_tableA3")
  .ft_desc_wide(m, .A_HEAD(), first_lab = "Variable", size = 7,
    notes = c("Significance levels: * p<0.10, ** p<0.05, *** p<0.01. Standard errors in brackets.",
      "A dagger denotes a statistically significant difference from the pooled sample.",
      "Data source: Ghana Living Standards Survey [waves 4-7]."))
}

# ---- inline text lookups (tbl_num / tbl_pct) --------------------------------
# csv arg is a TABLE ID, not a file: .live_table() returns the SAME build the
# exhibit prints, so prose and table agree by construction. Every live table
# MUST be in the switch — an omission is a section quietly citing a frozen value.
.LIVE_IDS <- c("table1", "table2", "tableA2", "tableA3")
.live_table <- function(id) {
  key <- sub("\\.csv$", "", id)
  .memo(paste0("tbl:", key), function()
    switch(key,
      table1  = .tbl1_live(),
      table2  = .tbl2_live(),
      tableA2 = .tblA2_live(),
      tableA3 = .tblA3_live(),
      .read_tbl(id)))          # default: curated CSV (tableA1 / A4–A9) — kept
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
  if (live && nrow(r) > 2)
    stop("tbl_num: ", nrow(r), " rows for '", label, "' in ", csv,
         "; expected at most 2 (headcount + change).", call. = FALSE)
  s <- as.character(r[[col]][i])
  if (is.na(s) || !nzchar(s))
    stop("tbl_num: empty cell for '", label, "' / ", col, " in ", csv, call. = FALSE)
  part <- match.arg(part)
  pat <- switch(part,
    first   = "^\\s*(-?[0-9][0-9.,]*).*$",
    paren   = "^.*\\((-?[0-9][0-9.,]*)\\).*$",
    bracket = "^.*\\[(-?[0-9][0-9.,]*)\\].*$")
  if (!grepl(pat, s)) stop("tbl_num: no ", part, " number in '", s, "'", call. = FALSE)
  as.numeric(gsub(",", "", sub(pat, "\\1", s)))
}

tbl_pct <- function(csv, label, col, digits = 1, block = c("first", "change"))
  fmt_num(100 * tbl_num(csv, label, col, block = match.arg(block)), digits)
