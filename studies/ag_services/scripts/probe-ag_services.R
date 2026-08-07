#!/usr/bin/env Rscript
# scripts/probe-ag_services.R
#
# Standalone probe + verification runner for ft_table7().
# Run from the REPO ROOT:
#
#     Rscript studies/ag_services/scripts/probe-ag_services.R
#
# It does five things, in order, and stops at the first that fails:
#
#   1. checks the working directory and the files it needs
#   2. DIAGNOSES the real ef_mean object -- prints the actual column names and
#      the actual level codes, because exhibit_helpers_tables.R's keying was
#      pinned from a workbook, not from the data. If step 4 fails, the answer
#      is in step 2's output.
#   3. loads exhibit_helpers_tables.R (works with or without flextable)
#   4. builds Table 7 from the live estimation objects
#   5. diffs every cell against the frozen workbook reference
#
# Flags: --schema stop after the probe  --raw dump study_raw_data
#        --treat  resolve what services0-3 are
#        --save    write output/tables/table7_live.csv
#
# NO PACKAGE DEPENDENCIES. Base R only. flextable is optional (needed to render,
# not to verify); the expected cells are embedded below rather than read from
# JSON so the probe runs anywhere.
#
# Exit status 0 = every cell matches. 1 = anything else.
#
# This script only READS. It writes nothing except an optional CSV of the
# built table when you pass --save.

options(warn = 1, stringsAsFactors = FALSE)
ARGS  <- commandArgs(trailingOnly = TRUE)
SAVE  <- "--save"    %in% ARGS
QUIET <- "--quiet"   %in% ARGS

STUDY <- "studies/ag_services"
HELP  <- file.path(STUDY, "scripts", "exhibit_helpers_tables.R")
EST   <- file.path(STUDY, "output", "estimations")
TAGS  <- c("services0", "services1", "services2", "services3")

hr  <- function(ch = "-") cat(strrep(ch, 78), "\n", sep = "")
hdr <- function(n, s) { cat("\n"); hr("="); cat("  ", n, ". ", s, "\n", sep = ""); hr("=") }
SCHEMA_ONLY <- "--schema" %in% ARGS
die <- function(...) { cat("\nFAILED: ", ..., "\n", sep = ""); quit(status = 1) }

# ---- 1. environment ----------------------------------------------------------
hdr(1, "ENVIRONMENT")
cat("working directory : ", getwd(), "\n", sep = "")
if (!dir.exists(STUDY))
  die("not at the repo root. cd to the okwaayeli repo root and re-run:\n",
      "    Rscript ", STUDY, "/scripts/verify_table7.R")

need <- c(HELP)
for (f in need) {
  cat(sprintf("%-58s %s\n", f, if (file.exists(f)) "found" else "MISSING"))
  if (!file.exists(f)) die("missing ", f)
}
missing_est <- character(0)
for (tg in TAGS) {
  f <- file.path(EST, sprintf("CropID_Pooled_%s_TL_hnormal_optimal.rds", tg))
  ok <- file.exists(f)
  cat(sprintf("%-58s %s\n", f, if (ok) sprintf("found (%.0f MB)",
      file.size(f) / 1048576) else "MISSING"))
  if (!ok) missing_est <- c(missing_est, tg)
}
if (length(missing_est))
  die("missing estimation objects for: ", paste(missing_est, collapse = ", "),
      "\n  Run scripts/004_MSF_ag_services_study.R first.")

has_ft <- requireNamespace("flextable", quietly = TRUE)
cat(sprintf("\n%-58s %s\n", "package flextable (needed only to RENDER)",
            if (has_ft) "available" else "absent - data path still verifiable"))
cat(sprintf("%-58s %s\n", "R version", paste(R.version$major, R.version$minor, sep = ".")))

# ---- 2. diagnose the real object ---------------------------------------------
hdr(2, "WHAT THE ESTIMATION OBJECT ACTUALLY CONTAINS")
cat("The level keying in exhibit_helpers_tables.R was pinned from the retired\n",
    "workbook, NOT from these objects. If step 4 fails, compare what follows\n",
    "against the KEYING block at the top of that file.\n\n", sep = "")

# ---- --treat : resolve what services0-3 actually ARE ------------------------
# ft_table7()'s row labels were inferred from ROW ORDER (workbook vs draft), not
# from the data. The study carries real columns named farm_association,
# community_cooperative and Extension, so the inference is testable. If it is
# wrong, every Table 7 number is right and every row label is wrong -- the same
# failure class as a transposition, and just as invisible.
if ("--treat" %in% ARGS) {
  sp <- file.path(STUDY, "data", "ag_services_study_environment.rds")
  if (!file.exists(sp)) die("no study environment at ", sp)
  d <- readRDS(sp)$study_raw_data
  pl <- function(v) if (inherits(v, c("haven_labelled","labelled")))
    as.vector(unclass(v)) else if (is.factor(v)) as.character(v) else v

  TREAT <- intersect(paste0("services", 0:3), names(d))
  CAND  <- intersect(c("farm_association", "community_cooperative", "Extension",
                       "extension", "ag_services", "extension_agency_mofa",
                       "extension_agency_ngo", "extension_agency_coop"), names(d))
  wave <- if ("Surveyx" %in% names(d)) as.character(pl(d$Surveyx)) else rep("all", nrow(d))

  cat("\n=== coverage by wave (non-missing count / rows) ===\n")
  cov <- sapply(c(TREAT, CAND), function(nm)
    tapply(!is.na(pl(d[[nm]])), wave, sum))
  print(t(cov))

  cat("\n=== mean where defined, by wave ===\n")
  mn <- sapply(c(TREAT, CAND), function(nm)
    round(tapply(pl(d[[nm]]), wave, function(z) mean(z, na.rm = TRUE)), 4))
  print(t(mn))

  cat("\n=== AGREEMENT: each servicesN vs each candidate ===\n")
  cat("   pct = share of rows where BOTH are non-missing and EQUAL.\n")
  cat("   A 100.0 means that candidate IS that treatment.\n\n")
  ag <- matrix(NA_real_, length(TREAT), length(CAND), dimnames = list(TREAT, CAND))
  for (t_ in TREAT) for (c_ in CAND) {
    a <- pl(d[[t_]]); b <- pl(d[[c_]])
    ok <- !is.na(a) & !is.na(b)
    if (any(ok)) ag[t_, c_] <- round(100 * mean(a[ok] == b[ok]), 1)
  }
  print(ag)

  cat("\n=== best match per treatment ===\n")
  LAB <- c(services0 = "Any source", services1 = "Agricultural/fishing association",
           services2 = "Agricultural cooperative", services3 = "Agricultural extension")
  for (t_ in TREAT) {
    r <- ag[t_, ]; r <- r[!is.na(r)]
    if (!length(r)) { cat("  ", t_, ": no overlap\n"); next }
    b <- names(r)[which.max(r)]
    cat(sprintf("  %-10s -> %-24s (%.1f%% agreement)   ft_table7 label: %s\n",
                t_, b, max(r), LAB[[t_]]))
  }
  cat("\nIf a best match is not 100%, servicesN is DERIVED, not a copy -- read\n",
      "001_DATA_ag_services_study.R for its construction before trusting the label.\n", sep = "")
  cat("\n--treat given: stopping.\n"); quit(status = 0)
}

# ---- --raw : dump study_raw_data instead of the estimation objects -----------
# Run this AFTER 001/002 and BEFORE anyone writes 100_exhibit_descriptive_stats.R.
# That script needs the outcome list, the treatment indicators and the survey
# waves, and every one of those is a guess until it is read off the data.
if ("--raw" %in% ARGS) {
  sp <- file.path(STUDY, "data", "ag_services_study_environment.rds")
  if (!file.exists(sp)) die("no study environment at ", sp, " -- run 001 first.")
  se <- readRDS(sp)
  cat("study environment components: ", paste(names(se), collapse = ", "), "\n\n", sep = "")
  if (!"study_raw_data" %in% names(se))
    die("no study_raw_data in the environment. 001 must run WITH 002 -- ",
        "DATA without MATCHING re-saves the environment stripped.")
  d <- se$study_raw_data
  cat("study_raw_data: ", nrow(d), " rows x ", ncol(d), " cols\n\n", sep = "")

  # The data comes from Stata via haven, so columns are haven_labelled: they
  # carry a `labels` attribute and refuse as.character()/mtfrm(). Strip to the
  # underlying vector before testing anything, and wrap every per-column probe
  # in tryCatch so one awkward class cannot kill the whole dump.
  .plain <- function(v) {
    if (inherits(v, c("haven_labelled", "labelled"))) return(as.vector(unclass(v)))
    if (is.factor(v)) return(as.character(v))
    v
  }
  .safe <- function(f, default) function(v) tryCatch(f(.plain(v)), error = function(e) default)

  cls <- vapply(d, function(v) paste(class(v), collapse = "/"), character(1))
  nun <- vapply(d, .safe(function(v) length(unique(v)), NA_integer_), integer(1))
  bin <- vapply(d, .safe(function(v) {
           u <- stats::na.omit(unique(v))
           is.numeric(u) && length(u) > 0 && length(u) <= 2 && all(u %in% c(0, 1))
         }, FALSE), logical(1))
  lab <- vapply(d, function(v) {
           l <- attr(v, "labels"); if (is.null(l)) return("")
           paste(sprintf("%s=%s", l, names(l)), collapse = "; ") }, character(1))
  vlb <- vapply(d, function(v) { l <- attr(v, "label"); if (is.null(l)) "" else as.character(l) },
                character(1))

  info <- data.frame(column = names(d), class = cls, n_distinct = nun,
                     binary01 = bin, label = substr(vlb, 1, 44), row.names = NULL)

  tr <- grepl("servic|extens|coop|assoc|advis|mechan|irrig|credit", names(d), ignore.case = TRUE)
  cat("--- TREATMENT candidates (name matches service/extension/coop/assoc/...) ---\n")
  if (any(tr)) {
    print(info[tr, ], row.names = FALSE)
    cat("\n  value labels on those columns:\n")
    for (nm in names(d)[tr]) if (nzchar(lab[[nm]]))
      cat("    ", nm, ": ", substr(lab[[nm]], 1, 150), "\n", sep = "")
  } else cat("  none matched -- widen the pattern\n")

  cat("\n--- binary 0/1 (binomial family in descriptive_specifications) ---\n")
  print(info[bin & !tr, ], row.names = FALSE)
  cat("\n--- low-cardinality categoricals (candidates for descriptive_expand_category) ---\n")
  lo <- !bin & !tr & !is.na(nun) & nun <= 12 & nun > 1
  print(info[lo, ], row.names = FALSE)
  for (nm in names(d)[lo]) if (nzchar(lab[[nm]]))
    cat("    ", nm, ": ", substr(lab[[nm]], 1, 150), "\n", sep = "")
  cat("\n--- continuous (gaussian family) ---\n")
  print(info[!bin & !tr & !is.na(nun) & nun > 12 &
             grepl("numeric|integer|double|labelled", cls), ], row.names = FALSE)

  for (v in intersect(c("Surveyx", "Survey", "CropID"), names(d))) {
    u <- tryCatch(sort(unique(as.character(.plain(d[[v]])))), error = function(e) "<unreadable>")
    cat("\n", v, " (", length(u), "): ", paste(utils::head(u, 30), collapse = " | "), "\n", sep = "")
  }

  cat("\n--- matching covariates declared in the environment ---\n")
  for (k in c("match_variables_exact", "match_variables_factor", "match_variables_scaler"))
    if (!is.null(se[[k]])) cat("  ", k, ": ", paste(se[[k]], collapse = ", "), "\n", sep = "")
  if (!is.null(se$match_specification_optimal)) {
    cat("\nmatch_specification_optimal:\n"); print(se$match_specification_optimal)
  }
  if (!is.null(se$estimation_data))
    cat("\nestimation_data: ", nrow(se$estimation_data), " rows x ",
        ncol(se$estimation_data), " cols\n", sep = "")

  cat("\n--raw given: stopping.\n"); quit(status = 0)
}

probe <- readRDS(file.path(EST, "CropID_Pooled_services0_TL_hnormal_optimal.rds"))
cat("components of services0: ", paste(names(probe), collapse = ", "), "\n\n", sep = "")
if (!"ef_mean" %in% names(probe))
  die("services0 has no ef_mean component. Table 7 is built from ef_mean.")

em <- probe$ef_mean
cat("ef_mean: ", nrow(em), " rows x ", ncol(em), " cols\n", sep = "")
cat("columns: ", paste(names(em), collapse = ", "), "\n\n", sep = "")

show_codes <- function(col, d = em) {
  if (!col %in% names(d)) { cat(sprintf("  %-24s <ABSENT>\n", col)); return(invisible()) }
  v <- d[[col]]
  if (is.numeric(v) && length(unique(v)) > 30) {
    cat(sprintf("  %-24s numeric, %d distinct, range [%.4g, %.4g]\n",
                col, length(unique(v)), min(v, na.rm = TRUE), max(v, na.rm = TRUE)))
    return(invisible())
  }
  u <- unique(as.character(v))
  if (length(u) > 12) u <- c(u[1:12], sprintf("... (%d total)", length(u)))
  cat(sprintf("  %-24s %s\n", col, paste(u, collapse = " | ")))
}

cat("EVERY column, with its distinct values:\n")
for (cl in names(em)) show_codes(cl)

if (all(c("Tech", "TCHLvel") %in% names(em))) {
  cat("\nTech x TCHLvel:\n"); print(table(Tech = em$Tech, TCHLvel = em$TCHLvel))
}

# ---- 2b. REVERSE LOOKUP -------------------------------------------------------
# The decisive step. Rather than guess which columns carry TGR / TE / MTE, search
# for cells whose published values we already know from the workbook and print
# the FULL row that produced each. Whatever distinguishes those rows IS the key.
hdr("2b", "REVERSE LOOKUP: FINDING THE KNOWN CELLS IN THE RAW DATA")
cat("Searching services0 for the three Table 7 'No services' values, plus the\n",
    "'Any source' difference. The columns that differ between the hits are the\n",
    "ones the builder must key on.\n\n", sep = "")

sd_col <- intersect(c("Estimate.sd", "Std..Error", "SE"), names(em))
p_col  <- intersect(c("jack_pv", "Pr...z..", "pvalue"), names(em))
cat("candidate SE column      : ", if (length(sd_col)) sd_col[1] else "NONE FOUND", "\n", sep = "")
cat("candidate p-value column : ", if (length(p_col))  p_col[1]  else "NONE FOUND", "\n\n", sep = "")

TARGETS <- list(
  list(lab = "TGR  / No services   0.941 (0.002)", est = 0.941, se = 0.002),
  list(lab = "TGR  / Some services 0.852 (0.007)", est = 0.852, se = 0.007),
  list(lab = "TE   / No services   0.560 (0.006)", est = 0.560, se = 0.006),
  list(lab = "TE   / Some services 0.609 (0.001)", est = 0.609, se = 0.001),
  list(lab = "MTE  / No services   0.525 (0.005)", est = 0.525, se = 0.005),
  list(lab = "MTE  / Some services 0.534 (0.006)", est = 0.534, se = 0.006),
  list(lab = "TGR  / Difference   -0.088 (0.009)", est = -0.088, se = 0.009),
  list(lab = "TE   / Difference    0.049 (0.007)", est = 0.049, se = 0.007),
  list(lab = "MTE  / Difference    0.009 (0.002)", est = 0.009, se = 0.002))

# Columns worth printing: everything low-cardinality, plus the value columns.
key_cols <- names(em)[vapply(em, function(v)
  is.character(v) || is.factor(v) || (is.numeric(v) && length(unique(v)) <= 30),
  logical(1))]
key_cols <- setdiff(key_cols, c("Estimate", "Estimate.mean", "Estimate.sd",
                                "Estimate.length", "jack_zv", "jack_pv"))

for (tg in TARGETS) {
  hit <- which(abs(em$Estimate - tg$est) < 5e-4)
  if (length(sd_col))
    hit <- hit[abs(em[[sd_col[1]]][hit] - tg$se) < 5e-4]
  cat("--- ", tg$lab, "  ->  ", length(hit), " row(s)\n", sep = "")
  if (!length(hit)) { cat("      not found in services0\n"); next }
  if (length(hit) > 6) { cat("      (showing first 6 of ", length(hit), ")\n", sep = ""); hit <- hit[1:6] }
  print(em[hit, key_cols, drop = FALSE], row.names = FALSE)
  if (length(sd_col) && length(p_col))
    cat("      ", sd_col[1], "=", paste(signif(em[[sd_col[1]]][hit], 3), collapse = ", "),
        "   ", p_col[1], "=", paste(signif(em[[p_col[1]]][hit], 3), collapse = ", "), "\n", sep = "")
  cat("\n")
}

cat("HOW TO READ THIS: take any two hits with the SAME estimate meaning but\n",
    "different labels (e.g. TGR vs TE), and note which column changed. That\n",
    "column carries the metric. Do the same for No services vs Some services to\n",
    "confirm the level column, and for level vs Difference to find the gap flag.\n",
    "Then update the KEYING block and .t7_cell() in exhibit_helpers_tables.R.\n\n", sep = "")

se_p <- file.path(STUDY, "data", "ag_services_study_environment.rds")
if (file.exists(se_p)) {
  ms <- readRDS(se_p)$match_specification_optimal
  opt <- if (!is.null(ms)) ifelse(is.na(ms$link), ms$distance, ms$link) else NA
  cat("\nmatched-sample id resolved from the study environment: ",
      paste(opt, collapse = ", "), "\n", sep = "")
  if ("sample" %in% names(em) && !any(as.character(em$sample) %in% as.character(opt)))
    cat("  WARNING: that id does not appear in ef_mean$sample. The matched\n",
        "  columns will come back empty and the builder will stop.\n", sep = "")
} else {
  cat("\nWARNING: ", se_p, " not found. .opt will be NA and the matched\n",
      "  sample cannot be keyed.\n", sep = "")
}
rm(probe, em); invisible(gc(FALSE))

if (SCHEMA_ONLY) { cat("\n--schema given: stopping before the build.\n"); quit(status = 0) }

# ---- 3. load the helper ------------------------------------------------------
hdr(3, "LOADING exhibit_helpers_tables.R")
# Evaluated expression by expression so the data path is testable even when
# flextable is unavailable. The ft_*() bodies are not run at definition time.
exprs <- parse(HELP)
skipped <- 0L; last_msg <- NULL
for (e in exprs) {
  msg <- NULL
  ok <- tryCatch({ eval(e, envir = globalenv()); TRUE },
                 error = function(err) { msg <<- conditionMessage(err); FALSE })
  if (!ok) {
    skipped <- skipped + 1L
    if (!grepl("flextable", msg, fixed = TRUE)) last_msg <- msg
  }
}
cat("evaluated ", length(exprs) - skipped, " of ", length(exprs),
    " top-level expressions\n", sep = "")
if (skipped > 0L && !has_ft)
  cat("  (", skipped, " skipped because flextable is absent -- expected)\n", sep = "")
if (!is.null(last_msg))
  cat("  NOTE: an expression failed for a reason unrelated to flextable:\n    ",
      last_msg, "\n  Inspect before trusting the result.\n", sep = "")
if (!exists(".tbl7_live"))
  die("exhibit_helpers_tables.R did not define .tbl7_live().")

# ---- 4. build ----------------------------------------------------------------
hdr(4, "BUILDING TABLE 7 FROM THE LIVE ESTIMATION OBJECTS")
t0 <- Sys.time()
m <- tryCatch(.tbl7_live(), error = function(e) {
  cat("\nthe builder stopped with:\n\n  ", conditionMessage(e), "\n\n", sep = "")
  cat("Compare that against step 2's output. The usual causes are:\n",
      "  - TCHLvel carries different labels than \"0\"/\"1\"\n",
      "  - the value columns are not Estimate / Std..Error / Pr...z..\n",
      "  - the matched-sample id does not match ef_mean$sample\n",
      "Re-pin the keying in exhibit_helpers_tables.R; do NOT switch to Tech.\n", sep = "")
  quit(status = 1)
})
cat("built ", nrow(m), " rows x ", ncol(m), " cols in ",
    round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s\n\n", sep = "")

pr <- m[, setdiff(names(m), ".key"), drop = FALSE]
for (i in seq_len(nrow(pr)))
  cat(sprintf("%-36s %-20s %-20s %s\n", pr[i, 1], pr[i, 2], pr[i, 3], pr[i, 4]))

if (SAVE) {
  out <- file.path(STUDY, "output", "tables", "table7_live.csv")
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(m, out, row.names = FALSE)
  cat("\nwrote ", out, "\n", sep = "")
}

# ---- 5. diff against the frozen workbook -------------------------------------
hdr(5, "CELL-BY-CELL DIFF AGAINST THE RETIRED WORKBOOK")
# Expected cells, transcribed from the `Table4` sheet of the retired
# ag_services_results-msf.xlsx on 2026-08-07. The full archive of that workbook
# (13 sheets, every cell) is
#   narrative/diagnostics/verification_reference_2026-08-07.json
# which this probe deliberately does NOT read -- embedding the 36 cells keeps
# the probe dependency-free. If you re-freeze the reference, update both.
WB <- rbind(
  c("TGR/Any source",                        "0.941*** (0.002)", "0.852*** (0.007)", "-0.088*** (0.009)"),
  c("TGR/Agricultural/fishing association",  "0.890*** (0.031)", "0.610*** (0.039)", "-0.280*** (0.015)"),
  c("TGR/Agricultural cooperative",          "0.933*** (0.008)", "0.941*** (0.011)", "0.008 (0.011)"),
  c("TGR/Agricultural extension",            "0.941*** (0.002)", "0.854*** (0.008)", "-0.087*** (0.009)"),
  c("TE/Any source",                         "0.560*** (0.006)", "0.609*** (0.001)", "0.049*** (0.007)"),
  c("TE/Agricultural/fishing association",   "0.560*** (0.008)", "0.679*** (0.008)", "0.119*** (0.012)"),
  c("TE/Agricultural cooperative",           "0.560*** (0.009)", "0.559*** (0.002)", "-0.001 (0.009)"),
  c("TE/Agricultural extension",             "0.560*** (0.006)", "0.621*** (0.001)", "0.060*** (0.007)"),
  c("MTE/Any source",                        "0.525*** (0.005)", "0.534*** (0.006)", "0.009*** (0.002)"),
  c("MTE/Agricultural/fishing association",  "0.509*** (0.019)", "0.448*** (0.024)", "-0.061*** (0.011)"),
  c("MTE/Agricultural cooperative",          "0.509*** (0.006)", "0.523*** (0.008)", "0.014*** (0.003)"),
  c("MTE/Agricultural extension",            "0.521*** (0.006)", "0.540*** (0.007)", "0.019*** (0.002)"))
want <- stats::setNames(split(WB[, 2:4], seq_len(nrow(WB))), WB[, 1])
COL <- c("A", "B", "diff"); CNM <- c("No services", "Some services", "Difference")

cat("expected rows: ", length(want), " (12 = 3 blocks x 4 service sources)\n\n", sep = "")

bad <- 0L; tot <- 0L
for (k in names(want)) {
  i <- which(m$.key == k)
  if (length(i) != 1L) {
    cat(sprintf("  ROW MISSING  %-40s built %d rows\n", k, length(i)))
    bad <- bad + 3L; tot <- tot + 3L; next
  }
  for (j in seq_along(COL)) {
    tot <- tot + 1L
    got <- m[[COL[j]]][i]; exp <- want[[k]][j]
    if (!identical(got, exp)) {
      bad <- bad + 1L
      cat(sprintf("  DIFF  %-36s %-14s live=%-20s workbook=%s\n",
                  k, CNM[j], got, exp))
    }
  }
}
hr()
if (bad == 0L) {
  cat("PASS: all ", tot, " cells match the workbook exactly.\n", sep = "")
  cat("\nft_table7() is verified. The pattern it proves -- .level_key() on the\n",
      "labelled column, .pick() with a uniqueness guard, .memo() per table and\n",
      "per estimation object, a .key column where row labels repeat -- is the\n",
      "pattern for the remaining 14 builders.\n", sep = "")
  quit(status = 0)
}
cat("MISMATCH: ", bad, " of ", tot, " cells differ.\n\n", sep = "")
cat("Read this before changing the builder. The v000 DRAFT already disagrees\n",
    "with this workbook in 6 of 36 cells (all standard errors, all point\n",
    "estimates identical) -- see narrative/diagnostics/migration_2026-08-07.md.\n",
    "So there are three possible answers, not two:\n\n",
    "  live == workbook            -> PASS, nothing to do\n",
    "  live differs only in SEs    -> the workbook is itself a stale run.\n",
    "                                 The pipeline wins. Re-freeze the reference\n",
    "                                 and say so explicitly in diagnostics.\n",
    "  live differs in ESTIMATES   -> the keying is wrong. Do NOT 'fix' it by\n",
    "                                 switching to the numeric Tech column;\n",
    "                                 re-pin against step 2's cross-tab.\n", sep = "")
quit(status = 1)
