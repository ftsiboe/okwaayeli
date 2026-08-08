# probes/probe_exhibits.R
# ==============================================================================
# Exhaustive read-only probe of everything the exhibit and narrative layers need.
#
# WHY THIS EXISTS
# Every wrong guess in this study's build has had the same shape: a name or a
# level inferred from the v005 draft's prose instead of read from the data.
# NonBanked_Why was guessed as NoAccRsn, Use as LoanPurp, Refusal as RejRsn,
# Bank_Info missed entirely, WhyNoLoan declared absent when it exists, and Tech
# read as the credit group when Tech == 1 is the NO-credit group. Each cost a
# round trip. This script is the alternative: dump the whole surface once.
#
# READ-ONLY. Touches nothing outside probes/. Safe to run at any time.
#
# USAGE, from the repo root:
#   source("studies/financial_inclusion/scripts/../probes/probe_exhibits.R")
# or
#   Rscript studies/financial_inclusion/probes/probe_exhibits.R
#
# OUTPUTS (both under probes/logs/)
#   probe_exhibits.log    human-readable transcript of everything below
#   probe_exhibits.json   the same content, machine-readable
# ==============================================================================

PROBE_DIR <- "studies/financial_inclusion/probes"
LOG_DIR   <- file.path(PROBE_DIR, "logs")
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

STUDY <- "studies/financial_inclusion"
DATA  <- file.path(STUDY, "data")
OUT   <- file.path(STUDY, "output")
EST   <- file.path(OUT, "estimations")
NARR  <- file.path(STUDY, "narrative")

LOG  <- file.path(LOG_DIR, "probe_exhibits.log")
JSON <- file.path(LOG_DIR, "probe_exhibits.json")

con <- file(LOG, open = "wt", encoding = "UTF-8")
sink(con, split = TRUE); sink(con, type = "message")
on.exit({sink(type = "message"); sink(); close(con)}, add = TRUE)

J <- list()   # accumulates the machine-readable mirror

# %||% is base only from R 4.4; define it so the probe runs on older installs.
`%||%` <- function(a, b) if (is.null(a)) b else a

hr <- function(t) cat("\n\n", strrep("=", 78), "\n== ", t, "\n", strrep("=", 78), "\n", sep = "")
sub <- function(t) cat("\n---- ", t, " ", strrep("-", max(0, 60 - nchar(t))), "\n", sep = "")
# Level sets, capped so a 2,000-level factor cannot flood the log.
lv <- function(x, cap = 80) {
  u <- sort(unique(as.character(x)))
  if (length(u) > cap) c(u[seq_len(cap)], sprintf("... +%d more", length(u) - cap)) else u
}
show_lv <- function(nm, x, cap = 80) { v <- lv(x, cap); cat(sprintf("  %-22s (%d) ", nm, length(unique(x)))); cat(paste(v, collapse = " | "), "\n"); v }

cat("probe_exhibits.R\n")
cat("generated: ", format(Sys.time()), "\n", sep = "")
cat("R: ", R.version.string, "\n", sep = "")
cat("wd: ", getwd(), "\n", sep = "")
J$generated <- format(Sys.time()); J$wd <- getwd()

# ==============================================================================
hr("1. FILES ON DISK")
# ==============================================================================
inv <- function(dir, pat = NULL) {
  if (!dir.exists(dir)) { cat("  MISSING DIR: ", dir, "\n", sep = ""); return(character(0)) }
  f <- list.files(dir, pattern = pat)
  cat(sprintf("  %-46s %d files\n", dir, length(f)))
  f
}
J$files <- list(
  data       = inv(DATA),
  output     = inv(OUT),
  figures    = inv(file.path(OUT, "figures")),
  tables     = inv(file.path(OUT, "tables")),
  scripts    = inv(file.path(STUDY, "scripts")),
  narrative  = inv(NARR),
  sections   = inv(file.path(NARR, "sections")),
  diagnostics= inv(file.path(NARR, "diagnostics")))

sub("estimation objects by technology_variable")
est_files <- list.files(EST, pattern = "\\.rds$")
tags <- unique(sub("^CropID_Pooled_(.*)_TL_hnormal_.*$", "\\1",
                   grep("^CropID_Pooled_.*_TL_hnormal_", est_files, value = TRUE)))
cat("  total .rds: ", length(est_files), "\n", sep = "")
cat("  tags: ", paste(sort(tags), collapse = ", "), "\n", sep = "")
cat("  optimal-suffix files:\n")
for (f in grep("_optimal\\.rds$", est_files, value = TRUE)) cat("    ", f, "\n", sep = "")
J$est_files <- est_files; J$est_tags <- sort(tags)

sub("figures: does every .png have data beside it?")
figs <- list.files(file.path(OUT, "figures"))
png  <- grep("\\.png$", figs, value = TRUE)
stem <- sub("\\.png$", "", png)
dat  <- sub("\\.(rds|csv)$", "", grep("\\.(rds|csv)$", figs, value = TRUE))
for (s0 in stem)
  cat(sprintf("  %-42s %s\n", s0,
      if (any(grepl(sub("_data$", "", s0), dat, fixed = TRUE))) "data present" else "NO DATA"))
J$figures <- list(png = png, data = unique(dat))

# ==============================================================================
hr("2. STUDY ENVIRONMENT")
# ==============================================================================
SE_P <- file.path(DATA, "financial_inclusion_study_environment.rds")
if (file.exists(SE_P)) {
  se <- readRDS(SE_P)
  cat("  top-level names:\n    ", paste(names(se), collapse = ", "), "\n", sep = "")
  cat("  layout: ", se$layout %||% "(unset)", "\n", sep = "")
  cat("  wd entries:\n"); for (n in names(se$wd)) cat(sprintf("    %-18s %s\n", n, se$wd[[n]]))
  sub("match_specification_optimal")
  print(se$match_specification_optimal)
  OPT <- with(se$match_specification_optimal, ifelse(is.na(link), distance, link))
  cat("  -> matched sample name used by builders: '", OPT, "'\n", sep = "")
  J$opt_sample <- OPT
  J$se_names <- names(se); J$wd <- as.list(se$wd)

  sub("study_raw_data")
  d <- se$study_raw_data
  cat("  rows: ", nrow(d), "  cols: ", ncol(d), "\n", sep = "")
  cat("  columns:\n"); print(sort(names(d)))
  J$raw_cols <- sort(names(d)); J$raw_nrow <- nrow(d)

  sub("estimation_data")
  ed <- se$estimation_data
  if (is.null(ed)) cat("  ABSENT -- 002 attaches it; 001 alone strips it.\n") else {
    cat("  rows: ", nrow(ed), "  pooled rows: ",
        sum(as.character(ed$CropID) %in% "Pooled"), "\n", sep = "")
    if ("Treat" %in% names(ed))
      print(table(ed$Treat[as.character(ed$CropID) %in% "Pooled"], useNA = "ifany"))
    J$est_nrow <- nrow(ed)
  }

  sub("credit-like and index columns in study_raw_data")
  for (c0 in sort(grep("redit|FinIdx|Bank|Insur|Loan|Refus|Accep|Proces|Applied|WhyNo|Use_|Source_|Collat|InstTyp|AccTyp|PrdTyp|Km$",
                       names(d), value = TRUE))) {
    x <- d[[c0]]
    cat(sprintf("    %-20s %-10s n_uniq=%-5d  %s\n", c0, class(x)[1],
        length(unique(x)), paste(utils::head(lv(x, 8)), collapse = ",")))
  }
} else cat("  MISSING: ", SE_P, "\n", sep = "")

# ==============================================================================
hr("3. DESCRIPTIVE CACHE  (Tables 1, 2, 3, S1, S2)")
# ==============================================================================
DP <- file.path(DATA, "descriptive_exhibits.rds")
if (file.exists(DP)) {
  D <- readRDS(DP)
  cat("  components: ", paste(names(D), collapse = ", "), "\n", sep = "")
  str(D$meta)
  J$desc_meta <- D$meta
  for (nm in c("table1", "shares")) {
    sub(paste0("$", nm))
    x <- D[[nm]]
    cat("  dim: ", nrow(x), " x ", ncol(x), "\n  cols: ",
        paste(names(x), collapse = ", "), "\n", sep = "")
    keys <- intersect(c("treatment","crop","outcome","family","wave","group","statistic"), names(x))
    J[[paste0("desc_", nm, "_levels")]] <- setNames(lapply(keys, function(k) show_lv(k, x[[k]], 200)), keys)
    # Uniqueness of the key the builders use -- a duplicate here makes .pick() stop.
    kk <- if (nm == "table1")
      c("treatment","outcome","crop","group","wave","statistic") else
      c("outcome","crop","wave")
    kk <- intersect(kk, names(x))
    dup <- sum(duplicated(x[, kk, drop = FALSE]))
    cat("  duplicate rows on builder key (", paste(kk, collapse = "+"), "): ", dup, "\n", sep = "")
    if (dup) print(utils::head(x[duplicated(x[, kk, drop = FALSE]), kk, drop = FALSE], 10))
    J[[paste0("desc_", nm, "_dupes")]] <- dup
  }
} else cat("  MISSING: ", DP, " -- run DESCRIPTIVE = TRUE\n", sep = "")

# ==============================================================================
hr("4. ESTIMATION OBJECTS  (Tables 4, 5, 6, S3, S4)")
# ==============================================================================
probe_est <- function(tag) {
  p <- file.path(EST, sprintf("CropID_Pooled_%s_TL_hnormal_optimal.rds", tag))
  if (!file.exists(p)) { cat("\n  MISSING: ", basename(p), "\n", sep = ""); return(NULL) }
  E <- readRDS(p)
  hr(paste0("4.", tag, "  ", basename(p)))
  cat("  components: ", paste(names(E), collapse = ", "), "\n", sep = "")
  out <- list(components = names(E))
  for (nm in c("sf_estm", "el_mean", "ef_mean", "disagscors")) {
    x <- E[[nm]]; if (is.null(x)) next
    sub(paste0("$", nm, "   ", nrow(x), " x ", ncol(x)))
    cat("  cols: ", paste(names(x), collapse = ", "), "\n", sep = "")
    keys <- intersect(c("sample","restrict","Survey","Tech","TCHLvel","type",
                        "estType","stat","input","CoefName","disagscors_var"),
                      names(x))
    out[[nm]] <- list(dim = dim(x), cols = names(x),
      levels = setNames(lapply(keys, function(k) show_lv(k, x[[k]], 120)), keys))
    # THE cross-tab. Tech vs TCHLvel is the transposition trap.
    if (all(c("Tech","TCHLvel") %in% names(x))) {
      cat("\n  Tech x TCHLvel:\n"); print(table(x$Tech, as.character(x$TCHLvel), useNA = "ifany"))
    }
    if (all(c("TCHLvel","sample") %in% names(x))) {
      cat("\n  TCHLvel x sample:\n"); print(table(as.character(x$TCHLvel), x$sample, useNA = "ifany"))
    }
    if (all(c("TCHLvel","restrict") %in% names(x))) {
      cat("\n  TCHLvel x restrict:\n"); print(table(as.character(x$TCHLvel), x$restrict, useNA = "ifany"))
    }
    if (nm == "disagscors" && "disagscors_var" %in% names(x)) {
      sub("disagscors_var -> levels present")
      for (v in sort(unique(as.character(x$disagscors_var))))
        cat(sprintf("    %-20s %s\n", v,
            paste(lv(x$disagscors_level[as.character(x$disagscors_var) == v], 12), collapse = ",")))
    }
  }
  # ---- Table 4 / S3 / S4 column semantics: sample sizes pin the columns -------
  if (!is.null(E$sf_estm)) {
    sub("Nobs by TCHLvel x sample  (pins Table 4's 'Sample size' row)")
    n <- E$sf_estm[E$sf_estm$CoefName == "Nobs", , drop = FALSE]
    if (nrow(n)) print(n[, intersect(c("TCHLvel","sample","restrict","Survey","Estimate"), names(n))])
    else cat("    no CoefName == 'Nobs' rows\n")
    sub("diagnostics rows available (Table 4's model-diagnostics block)")
    diag_names <- c("Nobs","nXvar","mlLoglik","AIC","BIC","HQIC","mono","curv",
                    "olsSkew","CoelliM3Test","LRT","LRInef","Gamma","Sigma",
                    "sigmauSq","sigmavSq","Varu","Eu","Expu")
    cat("    present: ", paste(intersect(diag_names, unique(E$sf_estm$CoefName)), collapse = ", "), "\n", sep = "")
  }
  out
}
J$est <- list()
for (tg in c("credit_hh", "credit_self", "credit_spouse", "credit_child",
             "credit_close", "credit_member"))
  J$est[[tg]] <- probe_est(tg)

# ==============================================================================
hr("5. PARITY SPOT-CHECKS AGAINST v005")
# ==============================================================================
# The cross-tab proves Tech and TCHLvel correspond. It does NOT prove which one
# the draft's column headers followed. These pull the exact cells v005 prints so
# the two can be diffed by eye, once, before anything goes to print.
#
# v005 Table 4, "Land" elasticity row:
#   Naive national 0.753*** (0.002) | No credit [A] 0.751*** (0.002)
#   Some credit [B] 0.572*** (0.060) | Meta matched 0.650*** (0.018)
# v005 Table 5, TGR "Anyone including farmer":
#   No credit 0.795*** (0.007) | Some credit 0.810*** (0.028)
E <- tryCatch(readRDS(file.path(EST, "CropID_Pooled_credit_hh_TL_hnormal_optimal.rds")),
              error = function(e) NULL)
if (!is.null(E)) {
  sub("el_mean: every row for input el1 (v005 Table 4 'Land')")
  x <- E$el_mean[E$el_mean$input == "el1", , drop = FALSE]
  print(x[, intersect(c("TCHLvel","Tech","sample","stat","restrict","Survey",
                        "Estimate","Estimate.sd","jack_pv"), names(x))])
  sub("ef_mean: TGR, mean, GLSS0 (v005 Tables 4 and 5 TGR block)")
  y <- E$ef_mean[E$ef_mean$type == "TGR" & E$ef_mean$stat == "mean" &
                 E$ef_mean$Survey == "GLSS0", , drop = FALSE]
  print(y[, intersect(c("TCHLvel","Tech","sample","estType","restrict",
                        "Estimate","Estimate.sd","jack_pv"), names(y))])
  sub("sf_estm: lnI1 (v005 Table S3 'Land [lnI1]')")
  z <- E$sf_estm[E$sf_estm$CoefName == "lnI1", , drop = FALSE]
  print(z[, intersect(c("TCHLvel","Tech","sample","restrict","Survey",
                        "Estimate","StdError","Estimate.sd","jack_pv","Pvalue"), names(z))])
  sub("disagscors: Banked, disag_efficiencyGap_lvl (v005 Table 6 row 1)")
  w <- E$disagscors[E$disagscors$disagscors_var == "Banked" &
                    E$disagscors$CoefName == "disag_efficiencyGap_lvl" &
                    E$disagscors$stat == "mean", , drop = FALSE]
  print(w[, intersect(c("input","disagscors_level","TCHLvel","sample","restrict",
                        "estType","Survey","Estimate","Estimate.sd","jack_pv"), names(w))])
}

# ==============================================================================
hr("6. TREATMENT-EFFECT SUMMARY  (narrative)")
# ==============================================================================
TP <- file.path(OUT, "te_summary.rds")
if (file.exists(TP)) {
  te <- readRDS(TP)
  cat("  dim: ", nrow(te), " x ", ncol(te), "\n  cols: ",
      paste(names(te), collapse = ", "), "\n", sep = "")
  for (k in intersect(c("method","distance","link","outcome","level"), names(te)))
    show_lv(k, te[[k]], 40)
  sub("head")
  print(utils::head(te, 12))
  J$te_cols <- names(te)
} else cat("  MISSING: ", TP, "\n", sep = "")

# ==============================================================================
hr("7. FIGURE DATA  (numbers the prose reads off figures)")
# ==============================================================================
for (f in list.files(file.path(OUT, "figures"), pattern = "\\.rds$", full.names = TRUE)) {
  x <- tryCatch(readRDS(f), error = function(e) NULL)
  sub(basename(f))
  if (is.data.frame(x)) {
    cat("  dim: ", nrow(x), " x ", ncol(x), "\n  cols: ",
        paste(names(x), collapse = ", "), "\n", sep = "")
    print(utils::head(x, 4))
  } else cat("  class: ", paste(class(x), collapse = "/"), "\n", sep = "")
}

# ==============================================================================
hr("8. NARRATIVE STATE")
# ==============================================================================
OJ <- file.path(NARR, "article_objects.json")
if (file.exists(OJ) && requireNamespace("jsonlite", quietly = TRUE)) {
  cat("  article_objects.json keys:\n")
  print(jsonlite::fromJSON(OJ))
} else cat("  no article_objects.json (run OBJECTS = TRUE)\n")

sub("sections: size and how much is real content vs scaffold comment")
for (f in list.files(file.path(NARR, "sections"), full.names = TRUE)) {
  L <- readLines(f, warn = FALSE)
  body <- L[!grepl("^\\s*(<!--|-->|#|$)", L)]
  cat(sprintf("  %-38s %3d lines, %3d non-comment, %s\n", basename(f), length(L),
      length(body), if (any(grepl("`r ", L))) "HAS inline R" else "no inline R"))
}

sub("references.bib")
BB <- file.path(NARR, "references.bib")
if (file.exists(BB)) {
  b <- readLines(BB, warn = FALSE)
  keys <- sub("^@[A-Za-z]+\\{([^,]+),.*$", "\\1", grep("^@", b, value = TRUE))
  cat("  entries: ", length(keys), "\n  keys: ", paste(utils::head(keys, 40), collapse = ", "), "\n", sep = "")
} else cat("  MISSING references.bib\n")

# ==============================================================================
hr("9. WRITE MACHINE-READABLE MIRROR")
# ==============================================================================
if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(J, JSON, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "string", digits = NA)
  cat("  wrote ", JSON, "\n", sep = "")
} else cat("  jsonlite absent; log only\n")

cat("\n\nprobe_exhibits.R: complete.\n")
cat("  log:  ", LOG, "\n", sep = "")
cat("  json: ", JSON, "\n", sep = "")
invisible(TRUE)
