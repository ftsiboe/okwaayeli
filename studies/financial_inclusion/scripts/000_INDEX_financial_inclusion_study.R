# ==============================================================================
# studies/financial_inclusion/scripts/000_INDEX_financial_inclusion_study.R
#
# R port of 000_INDEX_financial_inclusion_study.do, so the financial-inclusion
# index can be built on Beocat, which has no Stata.
#
# WHY THIS EXISTS
#   The Stata original writes the only study input that is not a published
#   release asset, and its output path is a hardcoded Windows local. That made
#   the index the one file every machine had to receive by hand -- and on
#   2026-09-03 a stale copy on Beocat silently dropped 3,214 farm operators
#   (16,235 index members against 12,933) because 001 inner-joins on it. The
#   run completed and looked healthy. This port removes the Windows dependency;
#   publishing the output to the hh_data release removes the hand-copy.
#
# READS  (via get_household_data(), i.e. the hh_data release + cache, NOT
#         data-raw/ -- deliberately, so Beocat obeys the release convention)
#   harmonized_crop_farmer_data
#   harmonized_financial_inclusion_data
#
# WRITES
#   studies/financial_inclusion/data/financial_inclusion_index.rds
#       FinIdx     rescaled first principal component (unbounded, mean ~0)
#       FinIdxSi   FinIdx min-max scaled over the pooled sample, [0,1]
#       FinIdxCat  weighted quintiles of FinIdx over the pooled sample, 1-5
#       keys: Surveyx EaId HhId Mid
#   studies/financial_inclusion/data/financial_inclusion_index_diagnostics.rds
#       one row per stratum x indicator: loading, stratum N, explained variance
#
# Run from the repo root:  Rscript studies/financial_inclusion/scripts/000_INDEX_financial_inclusion_study.R
#
# EQUIVALENCE NOTES -- read these before trusting the output
#   * Stata `pca x, vce(nor) com(1)` factors the CORRELATION matrix and e(L)
#     holds unit-norm eigenvectors (verified against the shipped diagnostics:
#     the pooled loadings sum of squares is 1.000). prcomp(scale. = TRUE)
#     $rotation is the same object.
#   * e(rho) with com(1) is the first eigenvalue over the number of indicators.
#   * `predict` after pca scores the STANDARDISED variables on that eigenvector,
#     which is prcomp()$x[, 1].
#   * A component's sign is arbitrary in both packages. Stata happened to emit
#     an all-positive pooled loading vector; we orient deterministically to
#     match (see .orient()), so the released numbers stay comparable.
#   * xtile with pweights: implemented as the weighted empirical CDF. Assignment
#     can differ from Stata for observations sitting exactly on a cut point.
#     FinIdxCat is a heterogeneity dimension, not a matching covariate, so a
#     handful of boundary cases does not move the estimates -- but verify with
#     the harness at the foot of this file rather than assuming.
# ==============================================================================

suppressPackageStartupMessages({
  library(haven); library(dplyr)   # tidyr is NOT used and is NOT in DESCRIPTION Imports
})
devtools::load_all(".", quiet = TRUE)

# The index is a STUDY artefact, not a shared harmonized release: only this
# study reads it, and it is derived from two releases rather than being one.
# Writing it here (a) scopes it correctly, (b) removes it from the set of files
# that had to be hand-carried between machines, since this script can now
# regenerate it anywhere R runs.
REL <- file.path("studies", "financial_inclusion", "data")
if (!dir.exists(REL)) dir.create(REL, recursive = TRUE)

# ---- .do line 3-9 : ingest, collapse to member level, merge -----------------
farmer <- get_household_data("harmonized_crop_farmer_data")

# collapse (mean) YerEdu EduLevel AgeYr Female WeightHH,
#          by(Surveyx EaId HhId Mid Locality Head)
#
# Locality and Head sit in the by() list. Anything in a by() that varies WITHIN
# a member splits that member into several rows -- the defect that put duplicate
# Pooled rows into harmonized_crop_farmer_data. Assert it here rather than
# discover it three stages downstream.
KEYS <- c("Surveyx", "EaId", "HhId", "Mid")
farmer <- farmer %>%
  mutate(across(all_of(c("EduLevel", "Locality", "Head")), ~ as.character(haven::as_factor(.x))))

collapsed <- farmer %>%
  group_by(across(all_of(c(KEYS, "Locality", "Head")))) %>%
  summarise(across(c(YerEdu, AgeYr, Female, WeightHH), ~ mean(.x, na.rm = TRUE)),
            EduLevel = dplyr::first(EduLevel), .groups = "drop")

dup <- sum(duplicated(collapsed[KEYS]))
if (dup > 0)
  stop(sprintf(paste0("000_INDEX: the collapse produced %d duplicate member keys.\n",
                      "  Locality or Head varies within a member, so the by() splits it.\n",
                      "  Fix that upstream; an index with two rows per member will\n",
                      "  silently fan out every downstream join."), dup), call. = FALSE)

financial <- get_household_data("harmonized_financial_inclusion_data")
keepusing <- c(KEYS, "FinWorker", "HHFinWorker", "Banked",
               grep("^Insured_", names(financial), value = TRUE))
dat <- inner_join(collapsed, financial[keepusing], by = KEYS)   # keep if _merge==3

# ---- .do line 11-19 : the indicator set, complete cases ---------------------
FACTORS <- c("YerEdu", "FinWorker", "HHFinWorker",
             grep("^Insured_", names(dat), value = TRUE), "Banked")
FACTORS <- FACTORS[FACTORS %in% names(dat)]

n_before <- nrow(dat)
dat <- dat[stats::complete.cases(dat[FACTORS]), ]
message(sprintf("complete-case filter: %d -> %d rows", n_before, nrow(dat)))

# ---- helpers ----------------------------------------------------------------
# Sign of a component is arbitrary. Fix it so the majority of loadings are
# positive, which reproduces Stata's all-positive pooled vector and makes
# "higher = more included" true by construction rather than by luck.
.orient <- function(rot) if (sum(rot) < 0) -rot else rot

# One PCA on the correlation matrix, component 1 only.
.pca1 <- function(x) {
  keep <- vapply(x, function(v) { s <- stats::sd(v); !is.na(s) && s > 0 }, logical(1))
  x <- x[, keep, drop = FALSE]                       # .do line 37-40 screen
  p <- stats::prcomp(x, scale. = TRUE, center = TRUE)
  rot <- .orient(p$rotation[, 1])
  list(loading = rot,
       rho     = p$sdev[1]^2 / sum(p$sdev^2),        # e(rho)
       N       = nrow(x),
       score   = as.numeric(scale(x) %*% rot))       # predict
}

# ---- .do line 23-33 : pooled component = the reference metric ---------------
pooled <- .pca1(dat[FACTORS])
dat$COM <- pooled$score

# COLUMN NAME. The Stata postfile called the count column N. haven's
# write_dta() refuses a bare "N" as a Stata variable name, so this build writes
# Nobs; .tblS6_live() accepts either and normalises, so a workbook built from
# the Stata release and one built from here are identical.
diag_rows <- list(data.frame(round = "POOLED", locality = "POOLED",
                             indicator = names(pooled$loading),
                             loading = as.numeric(pooled$loading),
                             Nobs = pooled$N, rho = pooled$rho,
                             stringsAsFactors = FALSE))

# ---- .do line 34-53 : stratified components, rescaled onto COM --------------
dat$FinIdx <- NA_real_
for (sur in c("GLSS6", "GLSS7")) {
  for (lc in c("Rural", "Urban")) {
    i <- which(dat$Surveyx == sur & dat$Locality == lc)
    if (!length(i)) next
    fit <- .pca1(dat[i, FACTORS])
    diag_rows[[length(diag_rows) + 1]] <-
      data.frame(round = sur, locality = lc, indicator = names(fit$loading),
                 loading = as.numeric(fit$loading), Nobs = fit$N, rho = fit$rho,
                 stringsAsFactors = FALSE)
    # reg COM temp1 ; predict  -- puts the stratum score on COM's scale AND
    # fixes its orientation: a sign-flipped stratum gets a negative coefficient
    # and the fitted values still align with COM.
    m <- stats::lm(dat$COM[i] ~ fit$score)
    dat$FinIdx[i] <- stats::fitted(m)
  }
}
if (anyNA(dat$FinIdx))
  stop(sprintf("000_INDEX: %d rows fell outside every round x locality stratum.",
               sum(is.na(dat$FinIdx))), call. = FALSE)

# ---- .do line 55-61 : the three released variables --------------------------
rng <- range(dat$FinIdx, na.rm = TRUE)
dat$FinIdxSi <- (dat$FinIdx - rng[1]) / (rng[2] - rng[1])

# xtile FinIdxCat = FinIdx [pw=WeightHH], nq(5)
.wtd_ntile <- function(x, w, nq = 5L) {
  o  <- order(x); xs <- x[o]; ws <- w[o]
  cw <- cumsum(ws) / sum(ws)
  cuts <- vapply(seq_len(nq - 1L) / nq,
                 function(p) xs[which(cw >= p)[1]], numeric(1))
  as.integer(cut(x, breaks = c(-Inf, unique(cuts), Inf), labels = FALSE))
}
dat$FinIdxCat <- .wtd_ntile(dat$FinIdx, dat$WeightHH, 5L)

# ---- .do line 62-67 : write -------------------------------------------------
out <- dat[c("HhId", "EaId", "Mid", "Surveyx", "FinIdx", "FinIdxSi", "FinIdxCat")]
attr(out$FinIdx,    "label") <- "Financial inclusion index (first principal component, rescaled)"
attr(out$FinIdxSi,  "label") <- "Financial inclusion index, min-max scaled over the pooled sample"
attr(out$FinIdxCat, "label") <- "Financial inclusion index, weighted pooled quintiles"
# .rds, not .dta. The Stata original wrote .dta because Stata wrote it; nothing
# reads this in Stata now. .rds keeps exact doubles (no float downcasting on
# save), needs no haven round-trip, and is the convention this folder already
# uses for study artefacts (financial_inclusion_study_environment.rds,
# descriptive_exhibits.rds). It also retires a naming rule that only ever
# applied to Stata: haven::write_dta() rejects a bare "N", which is why the
# diagnostics count column below is Nobs.
saveRDS(out, file.path(REL, "financial_inclusion_index.rds"))

diagnostics <- do.call(rbind, diag_rows)
saveRDS(diagnostics, file.path(REL, "financial_inclusion_index_diagnostics.rds"))

message(sprintf("wrote %d index rows and %d diagnostic rows",
                nrow(out), nrow(diagnostics)))

# ==============================================================================
# VALIDATION -- run this on the Windows machine, where the Stata outputs exist,
# BEFORE trusting the port anywhere. It compares the two builds directly.
# ==============================================================================
# sta <- haven::read_dta("<path to the Stata-built>/financial_inclusion_index.dta")
# new <- readRDS("studies/financial_inclusion/data/financial_inclusion_index.rds")
# m   <- merge(sta, new, by = c("Surveyx","EaId","HhId","Mid"), suffixes = c(".sta",".r"))
# c(rows_stata = nrow(sta), rows_r = nrow(new), merged = nrow(m))
# c(cor_FinIdx   = cor(m$FinIdx.sta,   m$FinIdx.r),
#   cor_FinIdxSi = cor(m$FinIdxSi.sta, m$FinIdxSi.r),
#   max_abs_diff = max(abs(m$FinIdxSi.sta - m$FinIdxSi.r)))
# table(m$FinIdxCat.sta, m$FinIdxCat.r)          # off-diagonal = boundary cases
#
# dsta <- haven::read_dta("<path>/financial_inclusion_index_diagnostics.dta")
# dnew <- readRDS("studies/financial_inclusion/data/financial_inclusion_index_diagnostics.rds")
# names(dnew)[names(dnew) == "Nobs"] <- "N"      # see the COLUMN NAME note above
# dm <- merge(dsta, dnew, by = c("round","locality","indicator"), suffixes = c(".s",".r"))
# c(n = nrow(dm), max_loading_diff = max(abs(dm$loading.s - dm$loading.r)),
#   max_rho_diff = max(abs(dm$rho.s - dm$rho.r)))
#
# PASS looks like: cor_FinIdxSi ~ 1, max_loading_diff < 1e-6, max_rho_diff < 1e-9,
# and FinIdxCat agreeing off the diagonal only for a handful of ties.
# FinIdxSi is the variable that enters the matching distance, so it is the one
# that must match; FinIdxCat is only a heterogeneity label.
# ==============================================================================
