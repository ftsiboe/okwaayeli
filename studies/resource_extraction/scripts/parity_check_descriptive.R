# parity_check_descriptive.R  (VERIFICATION SCRATCH -- not part of the numbered
# pipeline; delete once the CSVs are retired)
#
# Diffs the LIVE engine build (data/descriptive_exhibits.rds, produced by
# 100_exhibit_descriptive_stats.R) cell-by-cell against the frozen
# data/tables/*.csv files, for the four descriptive tables (1, 2, A2, A3).
#
# Per AGENT_PROMPT.md: build everything first, diff before deleting, treat
# differences as FINDINGS (the pipeline wins, but say so). This script deletes
# nothing. It writes output/parity_diff.csv and prints a per-table summary.
#
# Run from the okwaayeli repo root, AFTER 100_exhibit_descriptive_stats.R:
#   source("studies/resource_extraction/scripts/100_exhibit_descriptive_stats.R")
#   source("studies/resource_extraction/scripts/parity_check_descriptive.R")

STUDY    <- "studies/resource_extraction"
CACHE    <- file.path(STUDY, "data", "descriptive_exhibits.rds")
TBL_DIR  <- file.path(STUDY, "data", "tables")
OUT_DIFF <- file.path(STUDY, "output", "parity_diff.csv")

stopifnot(file.exists(CACHE))
cache  <- readRDS(CACHE)
t1     <- cache$table1     # long: treatment, crop, outcome, group, statistic, estimate, se, sd, ...
shares <- cache$shares     # long: outcome, wave, statistic, estimate, se, sd, crop, ...

# ---- tolerances --------------------------------------------------------------
# Allow last-printed-digit rounding, flag anything larger. A cell is a MATCH when
# |live - frozen| <= abs_tol + rel_tol*|frozen|.
ABS_TOL <- 0.02
REL_TOL <- 0.01

# ---- cell parsers ------------------------------------------------------------
# "0.26 (0.44)"        -> c(est=0.26, disp=0.44)   (disp = sd)
# "-1.50*** [0.18]"    -> c(est=-1.50, disp=0.18)  (disp = se; stars dropped)
# "-" / "" / NA        -> c(est=NA, disp=NA)
.num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", x)))
parse_cell <- function(s) {
  s <- trimws(as.character(s))
  if (!length(s) || is.na(s) || s %in% c("", "-", "—")) return(c(est = NA, disp = NA))
  # strip significance stars and daggers
  core <- gsub("[*†]", "", s)
  m <- regmatches(core, regexec("^\\s*([-0-9.eE+]+)\\s*[\\(\\[]\\s*([-0-9.eE+]+)", core))[[1]]
  if (length(m) == 3) return(c(est = .num(m[2]), disp = .num(m[3])))
  c(est = .num(core), disp = NA)   # bare number, no paren/bracket
}

# engine lookup: one estimate + its companion (sd for means, se for trends)
pick <- function(df, ..., value = "estimate", disp = "sd") {
  keys <- list(...)
  ok <- rep(TRUE, nrow(df))
  for (k in names(keys)) ok <- ok & as.character(df[[k]]) == as.character(keys[[k]])
  r <- df[ok, , drop = FALSE]
  if (nrow(r) == 0) return(c(est = NA, disp = NA))
  if (nrow(r) > 1)  stop("ambiguous key (", nrow(r), " rows): ",
                         paste(names(keys), unlist(keys), sep = "=", collapse = ", "))
  c(est = as.numeric(r[[value]][1]),
    disp = if (disp %in% names(r)) as.numeric(r[[disp]][1]) else NA)
}

matches <- function(a, b) {
  if (is.na(a) && is.na(b)) return(TRUE)          # both empty ("-") -> agree
  if (is.na(a) ||  is.na(b)) return(FALSE)         # one present, one not -> finding
  abs(a - b) <= ABS_TOL + REL_TOL * abs(b)
}

findings <- list()
record <- function(table, row, col, field, live, frozen) {
  if (matches(live, frozen)) return(invisible())
  findings[[length(findings) + 1]] <<- data.frame(
    table = table, row = row, column = col, field = field,
    live = live, frozen = frozen,
    delta = if (is.na(live) || is.na(frozen)) NA_real_ else live - frozen,
    stringsAsFactors = FALSE)
}

read_tbl <- function(nm)
  read.csv(file.path(TBL_DIR, nm), check.names = FALSE,
           colClasses = "character", encoding = "UTF-8")

# ---- shared row spec for Table 1 / A2 / A3 (identical row order) --------------
# label -> (outcome, crop). All non-crop rows use the "Pooled" pseudo-crop; the
# crop-production block is outcome = Yield across crops. NB "Tomato" -> "Tomatoe".
ROWS <- list(
  list("Female farmer (dummy)",        "Female",    "Pooled"),
  list("Age (years)",                  "AgeYr",     "Pooled"),
  list("Education (years)",            "YerEdu",    "Pooled"),
  list("All crops (real GH¢/ha)", "Yield",     "Pooled"),
  list("Maize (Kg/ha)",                "Yield",     "Maize"),
  list("Rice (Kg/ha)",                 "Yield",     "Rice"),
  list("Millet (Kg/ha)",               "Yield",     "Millet"),
  list("Sorghum (Kg/ha)",              "Yield",     "Sorghum"),
  list("Beans (Kg/ha)",                "Yield",     "Beans"),
  list("Peanut (Kg/ha)",               "Yield",     "Peanut"),
  list("Cassava (Kg/ha)",              "Yield",     "Cassava"),
  list("Yam (Kg/ha)",                  "Yield",     "Yam"),
  list("Cocoyam (Kg/ha)",              "Yield",     "Cocoyam"),
  list("Plantain (Kg/ha)",             "Yield",     "Plantain"),
  list("Pepper (Kg/ha)",               "Yield",     "Pepper"),
  list("Okra (kg/ha)",                 "Yield",     "Okra"),
  list("Tomato (kg/ha)",               "Yield",     "Tomatoe"),
  list("Cocoa (Kg/ha)",                "Yield",     "Cocoa"),
  list("Palm (Kg/ha)",                 "Yield",     "Palm"),
  list("Land (ha)",                    "Area",      "Pooled"),
  list("Land owned (dummy)",           "OwnLnd",    "Pooled"),
  list("Crop diversification (index)", "CrpMix",    "Pooled"),
  list("Seed (real GH¢/ha)",      "SeedKg",    "Pooled"),
  list("Household labor (AE)",         "HHLaborAE", "Pooled"),
  list("Hired labor (man-days/ha)",    "HirdHr",    "Pooled"),
  list("Fertilizer (Kg/ha)",           "FertKg",    "Pooled"),
  list("Pesticide (Liter/ha)",         "PestLt",    "Pooled"),
  list("Mechanization (dummy)",        "EqipMech",  "Pooled"),
  list("Irrigation (dummy)",           "EqipIrig",  "Pooled"),
  list("Credit (dummy)",               "Credit",    "Pooled"),
  list("Size (AE)",                    "HHSizeAE",  "Pooled"),
  list("Dependency (ratio)",           "Depend",    "Pooled"))
# Match on the stem before " (" so the cedi/cent unit glyph inside the
# parenthetical never has to round-trip through encodings.
.stem <- function(x) trimws(sub("\\s*\\(.*$", "", as.character(x)))
row_for <- function(label) {
  s <- .stem(label)
  for (r in ROWS) if (identical(.stem(r[[1]]), s)) return(r)
  NULL
}

# ================= Table 1 (extraction_any: pooled / none / any) ===============
try({
  csv <- read_tbl("table1.csv")
  data_rows <- csv[csv$header != "1", , drop = FALSE]
  # (csv column, engine group, statistic, disp field)
  cols <- list(
    list("pooled_mean",  "pooled", "mean",      "sd"),
    list("none_mean",    "0",      "mean",      "sd"),
    list("any_mean",     "1",      "mean",      "sd"),
    list("pooled_trend", "pooled", "trend_pct", "se"),
    list("none_trend",   "0",      "trend_pct", "se"),
    list("any_trend",    "1",      "trend_pct", "se"))
  for (i in seq_len(nrow(data_rows))) {
    lab <- data_rows$label[i]; rs <- row_for(lab)
    if (is.null(rs)) { message("Table 1: unmapped row '", lab, "'"); next }
    for (cc in cols) {
      frozen <- parse_cell(data_rows[[cc[[1]]]][i])
      live <- pick(t1, treatment = "extraction_any", crop = rs[[3]],
                   outcome = rs[[2]], group = cc[[2]], statistic = cc[[3]],
                   wave = "all", disp = cc[[4]])
      record("table1", lab, cc[[1]], "est",  live["est"],  frozen["est"])
      record("table1", lab, cc[[1]], "disp", live["disp"], frozen["disp"])
    }
  }
}, silent = FALSE)

# ================= Tables A2 (means) and A3 (trend_pct) ========================
# c1 = extraction_any POOLED; c2..c7 = group 1 of the six subtypes, in order.
ACT_COLS <- list(
  list("c1", "extraction_any", "pooled"),
  list("c2", "mining_any",     "1"),
  list("c3", "mining_comm",    "1"),
  list("c4", "mining_gala",    "1"),
  list("c5", "quarrying",      "1"),
  list("c6", "sand",           "1"),
  list("c7", "salt",           "1"))
check_appendix <- function(file, tbl, statistic, disp) try({
  csv <- read_tbl(file)
  data_rows <- csv[csv$header != "1", , drop = FALSE]
  for (i in seq_len(nrow(data_rows))) {
    lab <- data_rows$label[i]; rs <- row_for(lab)
    if (is.null(rs)) { message(tbl, ": unmapped row '", lab, "'"); next }
    for (cc in ACT_COLS) {
      frozen <- parse_cell(data_rows[[cc[[1]]]][i])
      live <- pick(t1, treatment = cc[[2]], crop = rs[[3]], outcome = rs[[2]],
                   group = cc[[3]], statistic = statistic, wave = "all", disp = disp)
      record(tbl, lab, cc[[1]], "est",  live["est"],  frozen["est"])
      record(tbl, lab, cc[[1]], "disp", live["disp"], frozen["disp"])
    }
  }
}, silent = FALSE)
check_appendix("tableA2.csv", "tableA2", "mean",      "sd")
check_appendix("tableA3.csv", "tableA3", "trend_pct", "se")

# ================= Table 2 (shares + % change, crops x 7 activities) ===========
try({
  csv <- read_tbl("table2.csv")
  crop_map <- function(lab) switch(lab, "All crops listed" = "Pooled",
                                   "Tomato" = "Tomatoe", lab)
  IND <- c(c1 = "extraction_any", c2 = "mining_any", c3 = "mining_comm",
           c4 = "mining_gala", c5 = "quarrying", c6 = "sand", c7 = "salt")
  # two stacked blocks, split on the section header rows
  hdr <- which(csv$header == "1")
  block_of <- findInterval(seq_len(nrow(csv)), hdr)   # 1 = shares, 2 = % change
  for (i in seq_len(nrow(csv))) {
    if (csv$header[i] == "1") next
    crop <- crop_map(csv$label[i])
    wave <- if (block_of[i] == 1) "pooled" else "trend"
    stat <- if (block_of[i] == 1) "mean"   else "trend_pct"
    dfld <- if (block_of[i] == 1) "sd"     else "se"
    for (cn in names(IND)) {
      frozen <- parse_cell(csv[[cn]][i])
      live <- pick(shares, crop = crop, outcome = IND[[cn]],
                   wave = wave, statistic = stat, disp = dfld)
      tag <- if (block_of[i] == 1) "table2/share" else "table2/pctchg"
      record(tag, csv$label[i], cn, "est",  live["est"],  frozen["est"])
      record(tag, csv$label[i], cn, "disp", live["disp"], frozen["disp"])
    }
  }
}, silent = FALSE)

# ---- report ------------------------------------------------------------------
diff <- if (length(findings)) do.call(rbind, findings) else
  data.frame(table = character(), row = character(), column = character(),
             field = character(), live = numeric(), frozen = numeric(),
             delta = numeric())
dir.create(dirname(OUT_DIFF), recursive = TRUE, showWarnings = FALSE)
write.csv(diff, OUT_DIFF, row.names = FALSE)

cat("\n==================== PARITY SUMMARY ====================\n")
if (!nrow(diff)) {
  cat("All descriptive cells agree within tolerance",
      sprintf("(abs<=%.3g, rel<=%.3g).\n", ABS_TOL, REL_TOL))
  cat("Safe to retire data/tables/{table1,table2,tableA2,tableA3}.csv.\n")
} else {
  cat(nrow(diff), "cell mismatch(es). Each is a FINDING to investigate\n")
  cat("(pipeline wins, but confirm each is real transposition/drift, not tol):\n\n")
  print(as.data.frame(table(diff$table)))
  cat("\nFirst 25:\n"); print(utils::head(diff, 25))
  cat("\nFull diff -> ", OUT_DIFF, "\n", sep = "")
}
invisible(diff)
