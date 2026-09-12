# 103_exhibit_index_summary.R
# Summarize the financial-inclusion index over the analysis sample and cache it
# for Table S7, which Section 3 reads.
#
# WHY A SEPARATE STEP, AND NOT PART OF 100: 100 refits a model per
# (treatment x crop x outcome) and takes minutes; this is one pass over one
# column and takes seconds. Keeping it apart means a change to the index
# summary does not cost a descriptive re-run, and a descriptive re-run does not
# have to know about the index.
#
# INPUT   data/financial_inclusion_study_environment.rds  (DATA + MATCHING)
# OUTPUT  data/financial_inclusion_index_summary.rds
#
# The index itself is built by 000_INDEX_financial_inclusion_study.R and is
# carried into the estimation frame as FinIdxSi, the min-max scaled score that
# enters the matching distance (Note S1). Nothing here re-derives it.
#
# Run from the repo root.

tryCatch({rm(list = ls()[!(ls() %in% c(Keep.List))]); gc()}, error = function(e) {
  rm(list = ls(all = TRUE)); gc()
})

STUDY   <- "studies/financial_inclusion"
SE_RDS  <- file.path(STUDY, "data", "financial_inclusion_study_environment.rds")
OUT_RDS <- file.path(STUDY, "data", "financial_inclusion_index_summary.rds")

stopifnot(file.exists(SE_RDS))
se <- readRDS(SE_RDS)

# ---- the frame ---------------------------------------------------------------
# estimation_data first: it is the sample the paper describes and the one the
# matching draws on, and it is where FinIdxSi is guaranteed to sit. One row per
# operator means CropID == "Pooled"; the crop-specific rows repeat the operator.
.frames <- list(estimation_data = se$estimation_data,
                study_raw_data  = se$study_raw_data)
d <- NULL; src <- NA_character_
for (nm in names(.frames)) {
  f <- .frames[[nm]]
  if (is.null(f) || !nrow(f) || !"FinIdxSi" %in% names(f)) next
  if ("CropID" %in% names(f)) f <- f[as.character(f$CropID) %in% "Pooled", , drop = FALSE]
  if (nrow(f)) { d <- f; src <- nm; break }
}
if (is.null(d))
  stop("103: no frame in the study environment carries FinIdxSi on a pooled ",
       "operator sample. estimation_data is attached by 002_MATCHING; ",
       "001_DATA re-saves the environment without it.", call. = FALSE)
message("103: index summary from ", src, " (", nrow(d), " operator rows)")

x <- suppressWarnings(as.numeric(d$FinIdxSi))
ok <- !is.na(x)
if (!any(ok)) stop("103: FinIdxSi is entirely missing on that frame.", call. = FALSE)
if (any(!ok)) message("103: dropping ", sum(!ok), " rows with a missing index")
d <- d[ok, , drop = FALSE]; x <- x[ok]

# Top fifth of the POOLED distribution, unweighted, as the descriptive tables
# are. This is a cut of the index in this sample, not FinIdxCat: FinIdxCat is
# weighted quintiles of the unscaled score over the index's own (larger) member
# file, so the two are not the same cut and are deliberately not mixed.
thr <- stats::quantile(x, 0.8, names = FALSE)

.chr <- function(v) {
  if (inherits(v, "haven_labelled") && requireNamespace("haven", quietly = TRUE))
    as.character(haven::as_factor(v)) else as.character(v)
}
.bin <- function(v) {
  z <- suppressWarnings(as.numeric(v)); z[is.na(z)] <- 0; z > 0
}
.row <- function(group, level, i, detail = NA_character_) {
  i <- i & !is.na(i)
  if (!any(i)) return(NULL)
  data.frame(group = group, level = level, detail = detail,
             n = sum(i), mean = mean(x[i]), sd = stats::sd(x[i]),
             p_top = mean(x[i] >= thr), stringsAsFactors = FALSE)
}

rows <- list(.row("All crop farmers", "All crop farmers", rep(TRUE, length(x))))

# ---- survey round ------------------------------------------------------------
if ("Surveyx" %in% names(d)) {
  w <- .chr(d$Surveyx)
  rows <- c(rows, list(.row("Survey round", "2012/13", w %in% "GLSS6"),
                       .row("Survey round", "2016/17", w %in% "GLSS7")))
}

# ---- locality ----------------------------------------------------------------
# 000_INDEX strata are survey-round x locality, so the spelling that matters is
# whichever of these the estimation frame carries. Absent all of them the block
# is dropped rather than guessed at: no prose cites it.
if ("Locality" %in% names(d)) {
  lc <- .chr(d$Locality)
  for (v in c("Rural", "Urban"))
    rows <- c(rows, list(.row("Locality", v, lc %in% v)))
} else if ("Urban" %in% names(d)) {
  u <- .bin(d$Urban)
  rows <- c(rows, list(.row("Locality", "Rural", !u), .row("Locality", "Urban", u)))
} else {
  message("103: no locality column found; that block is omitted from Table S7")
}

# ---- household credit use ----------------------------------------------------
if ("credit_hh" %in% names(d)) {
  cr <- .bin(d$credit_hh)
  rows <- c(rows, list(.row("Household credit use", "No credit", !cr),
                       .row("Household credit use", "Some credit", cr)))
}

# ---- formal account ----------------------------------------------------------
if ("Banked" %in% names(d)) {
  bk <- .bin(d$Banked)
  rows <- c(rows, list(.row("Formal account", "No account", !bk),
                       .row("Formal account", "Has account", bk)))
}

# ---- region ------------------------------------------------------------------
reg_rows <- list()
if ("Region" %in% names(d)) {
  rg <- .chr(d$Region)
  for (v in sort(unique(rg[!is.na(rg) & nzchar(rg)])))
    reg_rows <- c(reg_rows, list(.row("Region", v, rg %in% v)))
  reg_rows <- Filter(Negate(is.null), reg_rows)
  if (length(reg_rows)) {
    rr <- do.call(rbind, reg_rows)
    rr <- rr[order(-rr$mean), , drop = FALSE]
    reg_rows <- split(rr, seq_len(nrow(rr)))
    # Dispersion rows carry the extremes so the prose can cite the spread
    # without typing a region name that the data spells its own way; the names
    # travel in `detail` and are printed in the table's footnote.
    hi <- rr[1, , drop = FALSE]; lo <- rr[nrow(rr), , drop = FALSE]
    hi$group <- "Regional extremes"; hi$detail <- hi$level; hi$level <- "Highest region"
    lo$group <- "Regional extremes"; lo$detail <- lo$level; lo$level <- "Lowest region"
    reg_rows <- c(reg_rows, list(hi), list(lo))
  }
} else {
  message("103: no Region column found; that block is omitted from Table S7")
}

out <- do.call(rbind, c(Filter(Negate(is.null), rows), reg_rows))
rownames(out) <- NULL
attr(out, "meta") <- list(generated = as.character(Sys.time()),
                          source    = src,
                          n_rows    = nrow(d),
                          threshold = thr,
                          weights   = "none")
saveRDS(out, OUT_RDS)
message("Wrote ", OUT_RDS, "  (", nrow(out), " rows; top-fifth cut at ",
        formatC(thr, format = "f", digits = 3), ")")
invisible(TRUE)
