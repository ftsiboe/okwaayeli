# 301_article_objects.R  (3## = article; see scripts/README.md)
# Assemble the numbers the narrative pulls from and write
# narrative/article_objects.json. Working directory is always the repo root.
#
# Everything here is extracted from the same estimation objects that
# 101_exhibit_figures.R and exhibit_helpers_tables.R read, so the manuscript
# text and the exhibits cannot drift apart.
#
# WHAT BELONGS HERE, AND WHAT DOES NOT
# ------------------------------------
# A number that a TABLE prints does NOT belong here. Those come from the
# builders, through tbl_num() / tbl_pct(), so the sentence and the cell are the
# same build. This file is for the numbers the prose quotes that no table
# prints: prevalences, the frontier diagnostics, the disaggregated gaps behind
# Figures 2-4, and the crop/region rankings the discussion reads out in order.
#
# KEYING -- the same rules as exhibit_helpers_tables.R, and for the same reason.
#   TCHLvel identifies the frontier: "National" (naive pooled), "0" (no
#   services, the REFERENCE), "1" (some services), "Meta" (meta-frontier).
#   `Tech` is a numeric analysis label with a different coding for the same
#   concept -- keying on it silently transposes the groups with every value
#   still looking plausible. Never substitute it.
#
#   Efficiency comparisons use the MATCHED sample (opt_sample). Frontier
#   parameters -- elasticities, gamma, the property-satisfaction rates -- use
#   the UNMATCHED sample for the group frontiers and the matched sample for the
#   meta-frontier, because that is how they are estimated.
#
#   services0-3 are FOUR SEPARATE BINARY TREATMENTS, four estimation objects,
#   each with its own two-group frontier. They are not one multi-level
#   treatment, and a "level 2/3" lookup against them returns nothing.
#
# Run from the repo root, AFTER 004 (MSF). 100/101 are not inputs.

tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

if (!exists("OBJECTS_JSON"))
  source("studies/ag_services/scripts/article_helpers.R")
if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("301: package 'jsonlite' is required.", call. = FALSE)
suppressPackageStartupMessages(library(jsonlite))

EST <- file.path(OUTPUT, "estimations")

# The four service sources, in the order Table 7 prints them.
SERVICE_TAGS <- c(services0 = "Any source",
                  services1 = "Agricultural/fishing association",
                  services2 = "Agricultural cooperative",
                  services3 = "Agricultural extension")

# Optimal matched-sample id -- the same selection every other stage makes.
se_path <- file.path(DATA, "ag_services_study_environment.rds")
if (!file.exists(se_path))
  stop("301: no study environment at ", se_path, call. = FALSE)
study_environment <- readRDS(se_path)
mspecs <- study_environment$match_specification_optimal
opt_sample <- if (!is.null(mspecs))
  ifelse(is.na(mspecs$link), mspecs$distance, mspecs$link) else NA_character_
if (is.na(opt_sample))
  stop("301: match_specification_optimal is missing from the study ",
       "environment.\n  Run the MATCHING stage (002); without it every ",
       "matched-sample lookup below\n  silently returns nothing.", call. = FALSE)

read_est <- function(tag) {
  f <- file.path(EST, sprintf("CropID_Pooled_%s_TL_hnormal_optimal.rds", tag))
  if (!file.exists(f))
    stop("301: missing estimation object\n  ", f,
         "\n  Run scripts/004_MSF_ag_services_study.R (or the MSF stage of ",
         "run_article.R).", call. = FALSE)
  readRDS(f)
}

# Read once. These are tens of MB each; four of them re-read per lookup is the
# difference between a fast step and a slow one.
EST_OBJ <- stats::setNames(lapply(names(SERVICE_TAGS), read_est),
                           names(SERVICE_TAGS))

# ---- Level keying, refusing to fall back to Tech -----------------------------
LEVEL_LABELS <- c("0", "1", "National", "Meta")

grp_of <- function(df) {
  if (!"TCHLvel" %in% names(df))
    stop("301: no labelled level column (TCHLvel).\n  Columns present: ",
         paste(names(df), collapse = ", "),
         "\n  Do NOT substitute the numeric Tech column -- it disagrees with ",
         "the labelled one.", call. = FALSE)
  present <- sort(unique(as.character(df$TCHLvel)))
  if (!any(LEVEL_LABELS %in% present))
    stop("301: TCHLvel carries none of the expected labels.\n  Expected any ",
         "of: ", paste(LEVEL_LABELS, collapse = ", "),
         "\n  Actually present:  ", paste(present, collapse = ", "),
         call. = FALSE)
  as.character(df$TCHLvel)
}

# pick() takes the FIRST match and records how many there were, so a key that
# has quietly stopped being unique shows up in objs$meta$ambiguous instead of
# silently choosing. A .pick() that stops outright is right for a table cell;
# here it would take down the whole json over one unused field.
.AMBIG <- character(0)
pick <- function(df, keep, value = "Estimate", what = NA_character_) {
  v <- df[[value]][keep]
  if (length(v) > 1L && !is.na(what))
    .AMBIG <<- c(.AMBIG, sprintf("%s (%d rows)", what, length(v)))
  if (!length(v)) NA_real_ else as.numeric(v[1])
}
gap_of <- function(a, n) if (is.na(a) || is.na(n)) NA_real_ else a - n

# =============================================================================
#  1) Efficiency -- the service comparison behind Table 7
# =============================================================================
# Level columns come from CoefName == "efficiency" at TCHLvel 0 and 1; the
# published difference is the STORED efficiencyGap_lvl at TCHLvel 1, which
# carries its own jackknife SE and p-value. `gap` (any - none) is emitted beside
# it as an arithmetic check: the two should agree to rounding, and where they do
# not that is a finding, not a preference.
eff_for <- function(tag) {
  ef <- EST_OBJ[[tag]]$ef_mean
  ef <- ef[ef$estType   %in% "teBC"       & ef$stat     %in% "wmean" &
           ef$Survey    %in% "GLSS0"      & ef$restrict %in% "Restricted" &
           ef$sample    %in% opt_sample, ]
  g <- grp_of(ef)
  mk <- function(metric) {
    n  <- pick(ef, ef$type %in% metric & g %in% "0" &
                   ef$CoefName %in% "efficiency", what = paste(tag, metric, "none"))
    a  <- pick(ef, ef$type %in% metric & g %in% "1" &
                   ef$CoefName %in% "efficiency", what = paste(tag, metric, "any"))
    gs <- pick(ef, ef$type %in% metric & g %in% "1" &
                   ef$CoefName %in% "efficiencyGap_lvl",
               what = paste(tag, metric, "gap"))
    gp <- pick(ef, ef$type %in% metric & g %in% "1" &
                   ef$CoefName %in% "efficiencyGap_pct",
               what = paste(tag, metric, "gap_pct"))
    se <- pick(ef, ef$type %in% metric & g %in% "1" &
                   ef$CoefName %in% "efficiencyGap_lvl", value = "Estimate.sd")
    pv <- pick(ef, ef$type %in% metric & g %in% "1" &
                   ef$CoefName %in% "efficiencyGap_lvl", value = "jack_pv")
    list(none = n, any = a, gap = gs, gap_se = se, gap_p = pv,
         gap_pct = gp, gap_arith = gap_of(a, n))
  }
  list(label = unname(SERVICE_TAGS[[tag]]),
       tgr = mk("TGR"), te = mk("TE"), mte = mk("MTE"))
}

eff <- stats::setNames(lapply(names(SERVICE_TAGS), eff_for), names(SERVICE_TAGS))

# =============================================================================
#  2) Elasticities and returns to scale -- Table 6
# =============================================================================
# 004's input_variables = c("Area","SeedKg","HHLaborAE","HirdHr","FertKg","PestLt")
# => el1..el6 are those inputs, in that order; el7 is their sum, the returns to
# scale. The mapping is positional in the estimation object, so it is written
# out here rather than inferred.
EL <- c(el1 = "land", el2 = "planting_materials", el3 = "family_labour",
        el4 = "hired_labour", el5 = "fertilizer", el6 = "pesticide",
        el7 = "rts")

pooled <- EST_OBJ[["services0"]]

el <- pooled$el_mean
el <- el[el$stat %in% "wmean" & el$Survey %in% "GLSS0" &
         el$restrict %in% "Restricted" & el$CoefName %in% "elasticity", ]
elg <- pooled$el_mean
elg <- elg[elg$stat %in% "wmean" & elg$Survey %in% "GLSS0" &
           elg$restrict %in% "Restricted" &
           elg$CoefName %in% "elasticityGap_lvl", ]

el_at <- function(inp, lv, samp, what)
  pick(el, el$input %in% inp & as.character(el$TCHLvel) %in% lv &
           el$sample %in% samp, what = what)

elasticities <- stats::setNames(lapply(names(EL), function(i) {
  n <- el_at(i, "0", "unmatched", paste("el", i, "none"))
  a <- el_at(i, "1", "unmatched", paste("el", i, "any"))
  # The stored gap is on the MATCHED sample and carries its own SE; it is the
  # figure to quote, not any(unmatched) - none(unmatched).
  gm <- pick(elg, elg$input %in% i & as.character(elg$TCHLvel) %in% "1" &
                  elg$sample %in% opt_sample, what = paste("el", i, "gapm"))
  list(naive = el_at(i, "National", "unmatched", paste("el", i, "naive")),
       none  = n,
       any   = a,
       meta  = el_at(i, "Meta", opt_sample, paste("el", i, "meta")),
       gap   = gap_of(a, n),
       gapm  = gm)
}), unname(EL))

# =============================================================================
#  3) Frontier diagnostics -- gamma and the theoretical-property rates
# =============================================================================
sf  <- pooled$sf_estm
sfg <- sf[sf$CoefName %in% "Gamma" & sf$restrict %in% "Restricted", ]
g_at <- function(lv, samp, what)
  pick(sfg, as.character(sfg$TCHLvel) %in% lv & sfg$sample %in% samp, what = what)
gamma <- list(naive  = g_at("National", "unmatched", "gamma naive"),
              none   = g_at("0",        "unmatched", "gamma none"),
              any    = g_at("1",        "unmatched", "gamma any"),
              meta   = g_at("Meta",     opt_sample,  "gamma meta"),
              meta_u = g_at("Meta",     "unmatched", "gamma meta_u"))

sfr <- sf[sf$restrict %in% "Restricted" & sf$Survey %in% "GLSS0", ]
r_at <- function(coef, lv, samp)
  pick(sfr, sfr$CoefName %in% coef & as.character(sfr$TCHLvel) %in% lv &
            sfr$sample %in% samp)
rates <- function(coef) list(
  naive  = r_at(coef, "National", "unmatched"),
  none   = r_at(coef, "0",        "unmatched"),
  any    = r_at(coef, "1",        "unmatched"),
  meta_m = r_at(coef, "Meta",     opt_sample),
  meta_u = r_at(coef, "Meta",     "unmatched"))
mono <- rates("mono")
curv <- rates("curv")

# =============================================================================
#  3c) The gap by survey round -- Figure 1
# =============================================================================
# The pooled comparison in section 5.3 averages over three rounds that do not
# look alike. Figure 1 plots the round-by-round gap; these are the numbers
# behind it, so the prose and the figure cannot disagree.
#
# Matched sample, weighted mean, Battese-Coelli scores, stored efficiencyGap_lvl
# at TCHLvel 1: the same keying the pooled figure uses, with Survey left free
# instead of fixed at GLSS0. TE0 is dropped because the naive single-frontier
# score has no group gap to report.
by_wave <- local({
  ef <- pooled$ef_mean
  ef <- ef[ef$estType %in% "teBC" & ef$stat %in% "wmean" &
           ef$restrict %in% "Restricted" & ef$sample %in% opt_sample &
           ef$CoefName %in% "efficiencyGap_lvl" &
           !is.na(ef$TCHLvel) & as.character(ef$TCHLvel) == "1" &
           !ef$Survey %in% "GLSS0" & !ef$type %in% "TE0", , drop = FALSE]
  if (!nrow(ef)) {
    message("301: no by-wave gaps; objs$by_wave not emitted.")
    return(NULL)
  }
  out <- list()
  for (ty in c("TGR", "TE", "MTE")) {
    r <- ef[as.character(ef$type) == ty, , drop = FALSE]
    if (!nrow(r)) next
    r <- r[order(as.character(r$Survey)), , drop = FALSE]
    out[[tolower(ty)]] <- list(
      wave = as.character(r$Survey),
      gap  = stats::setNames(as.list(round(as.numeric(r$Estimate), 4)),
                             as.character(r$Survey)),
      se   = stats::setNames(as.list(round(as.numeric(r$Estimate.sd), 4)),
                             as.character(r$Survey)),
      p    = stats::setNames(as.list(as.numeric(r$jack_pv)),
                             as.character(r$Survey)))
  }
  if (!length(out)) return(NULL)
  message("301: by-wave TGR gap ",
          paste(sprintf("%s %+.3f", out$tgr$wave, unlist(out$tgr$gap)),
                collapse = ", "))
  out
})

# =============================================================================
#  3b) Functional form -- Cobb-Douglas against translog
# =============================================================================
# The paper claims the translog is preferred. This is the test behind that
# claim: Cobb-Douglas is the translog with every second-order term restricted to
# zero, so LR = 2 x (loglik_TL - loglik_CD) on df = the number of restrictions.
#
# DF IS COUNTED, NOT READ. sf_estm carries nXvar, and it is NOT the number of
# estimated frontier coefficients: on the translog fit nXvar is 20 against 46
# frontier coefficients actually reported, and on the Cobb-Douglas fit it is 25
# -- larger, though Cobb-Douglas is the nested, smaller model. Differencing
# nXvar gives df = -5. Counting the coefficient rows gives 61 against 40, a
# difference of 21, which is exactly the 6 own-squares plus 15 cross terms a
# six-input translog adds. Count the rows.
.CD_PATH <- file.path(EST, "CropID_Pooled_services0_CD_hnormal_optimal.rds")

functional_form <- local({
  if (!file.exists(.CD_PATH)) {
    message("301: no Cobb-Douglas fit at ", .CD_PATH,
            "; objs$functional_form not emitted. The translog-versus-",
            "Cobb-Douglas claim in\n     section 4.2 has nothing behind it ",
            "until 004 produces that cell.")
    return(NULL)
  }
  tl <- pooled$sf_estm
  cd <- readRDS(.CD_PATH)$sf_estm

  # Everything sf_estm reports that is a fit statistic rather than a coefficient.
  # Kept in step with .SF_STATS in exhibit_helpers_tables.R, which counts the
  # same thing for Table 6's "No. of parameters" row.
  STATS <- c("AIC", "BIC", "HQIC", "Nobs", "mlLoglik", "nXvar", "nuZUvar",
             "nvZVvar", "mono", "curv", "olsSkew", "olsM3Okay", "CoelliM3Test",
             "AgostinoSkw", "AgostinoKrt", "AgostinoOmn", "LRT", "LRInef",
             "Gamma", "Sigma", "Varu", "Eu", "Expu", "sigmauSq", "sigmavSq")

  cell <- function(d, lvl, samp)
    d[d$restrict %in% "Restricted" & !is.na(d$TCHLvel) &
      as.character(d$TCHLvel) == lvl & as.character(d$sample) == samp, ,
      drop = FALSE]
  ll <- function(d, lvl, samp) {
    r <- cell(d, lvl, samp)
    v <- as.numeric(r$Estimate[as.character(r$CoefName) == "mlLoglik"])
    if (!length(v)) NA_real_ else v[1]
  }
  k <- function(d, lvl, samp) {
    r <- cell(d, lvl, samp)
    if (!nrow(r)) return(NA_real_)
    length(setdiff(unique(as.character(r$CoefName)), STATS))
  }

  one <- function(lvl, samp) {
    a <- ll(tl, lvl, samp); b <- ll(cd, lvl, samp)
    df <- k(tl, lvl, samp) - k(cd, lvl, samp)
    if (is.na(a) || is.na(b) || is.na(df)) return(NULL)
    lr <- 2 * (a - b)
    # A negative df or a negative LR means the two fits are not nested the way
    # this test assumes -- almost certainly a changed specification rather than
    # a numerical accident. Say so instead of emitting a p-value from it.
    if (df <= 0 || lr < 0) {
      message("301: functional-form test at ", lvl, "/", samp,
              " is degenerate (LR = ", round(lr, 1), ", df = ", df,
              "). Not emitted.")
      return(NULL)
    }
    list(loglik_tl = a, loglik_cd = b, k_tl = k(tl, lvl, samp),
         k_cd = k(cd, lvl, samp), lr = lr, df = df,
         p = stats::pchisq(lr, df, lower.tail = FALSE))
  }

  out <- list(naive = one("National", "unmatched"),
              none  = one("0",        "unmatched"),
              any   = one("1",        "unmatched"),
              meta  = one("Meta",     opt_sample))
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(NULL)
  for (nm in names(out))
    message("301: Cobb-Douglas vs translog, ", nm, ": LR = ",
            round(out[[nm]]$lr, 1), " on ", out[[nm]]$df, " df, p = ",
            format.pval(out[[nm]]$p, digits = 3))
  out
})

# =============================================================================
#  4) Disaggregated gaps -- Figures 2, 3 and 4
# =============================================================================
# disagscors is built for ONE cell only: services0 / optimal / TL / hnormal /
# CropID Pooled (see 004, the disagscors_list block). Nothing else carries it.
dg <- pooled$disagscors
if (is.null(dg) || !nrow(dg))
  stop("301: services0 carries no disagscors. 004 builds it only for the ",
       "services0 / optimal /\n  TL / hnormal cell -- if that cell did not run, ",
       "Figures 2-4 and this block have no input.", call. = FALSE)
dg$disasg <- as.character(dg$disagscors_var)
dg$level  <- as.character(dg$disagscors_level)
dg <- dg[dg$estType %in% "teBC" & dg$Survey %in% "GLSS0" &
         dg$restrict %in% "Restricted" & dg$stat %in% "mean" &
         !dg$sample %in% "unmatched", ]

dg_lvl <- dg[dg$CoefName %in% "disag_efficiencyGap_lvl", ]
dg_pct <- dg[dg$CoefName %in% "disag_efficiencyGap_pct", ]

.dgv <- function(d, var, lv, metric)
  pick(d, d$disasg %in% var & d$level %in% lv & d$input %in% metric)
.dgset <- function(d, var, lv) list(tgr = .dgv(d, var, lv, "TGR"),
                                    te  = .dgv(d, var, lv, "TE"),
                                    mte = .dgv(d, var, lv, "MTE"))

# ---- 4a) By type of service (Figure 2) ---------------------------------------
# disagscors is TWO-WAY: `level` is the level of the disaggregating variable,
# TCHLvel is still the services0 frontier group. So `provided` below is the
# served-minus-unserved gap AMONG communities that provide that service, and
# `not_provided` is the same gap among those that do not.
#
# BOTH levels are emitted, because only four of the eight services have a
# level-1 gap at all: within credit, husbandry, labour and records there is no
# untreated group to compare against (a community providing the service is
# served by construction), so 004 emits nothing there and `provided` is null.
# Figure 2 plots the level-1 gaps, which is why it has four bars where
# disagscors has eight services. Emitting only level 1 here would have made the
# other four look absent rather than inestimable.
.svc <- sort(unique(dg_lvl$disasg[grepl("^services_", dg_lvl$disasg)]))
by_service <- stats::setNames(lapply(.svc, function(v) list(
  provided     = .dgset(dg_lvl, v, "1"),
  not_provided = .dgset(dg_lvl, v, "0"))), .svc)
.plotted <- .svc[vapply(.svc, function(v)
  any(!is.na(unlist(by_service[[v]]$provided))), logical(1))]
message("301: ", length(.svc), " service types in disagscors; ",
        length(.plotted), " with an estimable level-1 gap (",
        paste(.plotted, collapse = ", "), ").")
if (length(setdiff(.svc, .plotted)))
  message("     level-1 gap inestimable for: ",
          paste(setdiff(.svc, .plotted), collapse = ", "),
          " -- `provided` is null for these, `not_provided` is not.")

# ---- 4b) By farmer characteristic, crop and region (Figures 3 and 4) ---------
# Level labels are whatever the harmonizer wrote; they are carried through
# verbatim rather than re-spelled here, so a relabelling upstream shows up as a
# changed key instead of a silently mismatched one.
.by_var <- function(d, var) {
  lv <- sort(unique(d$level[d$disasg %in% var]))
  if (!length(lv)) return(NULL)
  stats::setNames(lapply(lv, function(l) .dgset(d, var, l)), lv)
}
heterogeneity <- list(
  female   = .by_var(dg_lvl, "Female"),
  age      = .by_var(dg_lvl, "AgeCat"),
  edu      = .by_var(dg_lvl, "EduLevel"),
  ecozone  = .by_var(dg_lvl, "Ecozon"),
  region   = .by_var(dg_lvl, "Region"),
  crop     = .by_var(dg_lvl, "CROP"))

# ---- 4c) Percent gaps, ranked -- the discussion reads these out in order -----
# The retired script printed these with paste0() to a console nobody captured.
# Emitted as ordered vectors so the prose can name the extremes without anyone
# re-deriving the ordering by hand.
.ranked <- function(var) {
  r <- dg_pct[dg_pct$disasg %in% var & dg_pct$input %in% "MTE", ]
  if (!nrow(r)) return(NULL)
  r <- r[order(r$Estimate), ]
  list(level = as.character(r$level), pct = round(as.numeric(r$Estimate), 4))
}
ranked <- list(region = .ranked("Region"), crop = .ranked("CROP"))

# =============================================================================
#  5) Service prevalence by wave -- feeds 02_data and the trend claims
# =============================================================================
# Emitted rather than hardcoded in the prose so the text cannot drift from the
# analysis data. The analysis sample is CropID == "Pooled"; that is the row set
# every published n refers to (22,519 in the 2026-08 build).
#
# Do NOT de-duplicate on (wave, EaId, HhId, Mid). That key is not unique within
# Pooled, so deduping drops observations and moves every prevalence by a point
# or two -- the same trap land_tenure documents.
prevalence <- local({
  d <- if (!is.null(study_environment$estimation_data))
    study_environment$estimation_data else study_environment$study_raw_data
  if (is.null(d)) {
    message("301: no estimation_data or study_raw_data in the study ",
            "environment; objs$prevalence not emitted. Run 001 (and 002).")
    return(NULL)
  }
  wv <- intersect(c("Surveyx", "Survey"), names(d))[1]
  if (is.na(wv)) {
    message("301: no wave column; objs$prevalence not emitted.")
    return(NULL)
  }
  dd <- d[as.character(d$CropID) %in% "Pooled", ]
  if (!nrow(dd)) {
    message("301: no CropID == 'Pooled' rows; objs$prevalence not emitted.")
    return(NULL)
  }
  # services1/2/3 are 0 / 1 / NA, where NA means "served, but not by this
  # source". mean(na.rm = TRUE) on them is therefore a share of the SERVED-BY-
  # THIS-SOURCE-OR-NOT-SERVED-AT-ALL subsample, not of the analysis sample. The
  # denominators are emitted alongside so nobody has to guess which one a share
  # is over.
  shr <- function(v) {
    x <- as.numeric(dd[[v]])
    tapply(x, as.character(dd[[wv]]),
           function(z) round(mean(z, na.rm = TRUE), 4))
  }
  den <- function(v) {
    x <- as.numeric(dd[[v]])
    tapply(x, as.character(dd[[wv]]), function(z) sum(!is.na(z)))
  }
  vars <- intersect(c("services0", "services1", "services2", "services3",
                      "farm_association", "community_cooperative",
                      "extension_officer_visit"), names(dd))
  list(
    share = stats::setNames(lapply(vars, function(v) as.list(shr(v))), vars),
    n_obs = stats::setNames(lapply(vars, function(v) as.list(den(v))), vars),
    n     = as.list(table(as.character(dd[[wv]]))),
    n_all = nrow(dd),
    waves = sort(unique(as.character(dd[[wv]]))),
    note  = paste("Shares are over CropID == 'Pooled'. services1/2/3 are coded",
                  "0 = no services at all, 1 = this source, NA = served by",
                  "another source, so their denominator (n_obs) is smaller",
                  "than n."))
})

# =============================================================================
#  6) Implausible-value suppression -- section 2.1
# =============================================================================
# 11_ag_services.do sets extension_distance to missing at 999 km and above and
# community_tractors at 99 and above, on the COMMUNITY record and before the
# collapse that produces the release. Downstream those records are
# indistinguishable from genuine item non-response, so the counts cannot be
# recovered here and are measured by a probe instead:
#
#   data-raw/data-prep/glss/_probe_11_suppression.do
#     -> data-raw/data-prep/glss/logs/11_suppression_counts.csv
#
# READING A CSV IS THE EXCEPTION, NOT THE PATTERN. Every other number in this
# file comes from an estimation object. This one describes a step that happens
# before any object exists, in a language R does not run, so a measured artefact
# is the only honest source. It is treated like the curated exhibits in
# data/tables/: read, schema-checked, and never silently regenerated.
.SUPP_CSV <- file.path("data-raw", "data-prep", "glss",
                       "logs", "11_suppression_counts.csv")

suppression <- local({
  if (!file.exists(.SUPP_CSV)) {
    message("301: no ", .SUPP_CSV, " -- objs$suppression not emitted.\n",
            "     Run data-raw/data-prep/glss/_probe_11_suppression.do once ",
            "(needs Stata).")
    return(NULL)
  }
  d <- utils::read.csv(.SUPP_CSV, stringsAsFactors = FALSE)
  need <- c("round", "rule", "n_suppressed", "n_already_missing", "n_valid",
            "n_records")
  miss <- setdiff(need, names(d))
  if (length(miss)) {
    message("301: ", .SUPP_CSV, " lacks ", paste(miss, collapse = ", "),
            " -- objs$suppression not emitted. The probe's output format ",
            "changed.")
    return(NULL)
  }
  # GLSS4 is harmonized upstream but is not in this study's estimation sample.
  # The probe counts it anyway; the paper must not.
  WAVES <- c("GLSS5", "GLSS6", "GLSS7")
  d <- d[d$round %in% WAVES, , drop = FALSE]
  if (!nrow(d)) return(NULL)

  by_rule <- function(rl) {
    r <- d[d$rule == rl, , drop = FALSE]
    if (!nrow(r)) return(NULL)
    list(
      by_round   = stats::setNames(as.list(r$n_suppressed), r$round),
      suppressed = sum(r$n_suppressed),
      missing    = sum(r$n_already_missing),
      valid      = sum(r$n_valid),
      records    = sum(r$n_records))
  }
  out <- list(distance = by_rule("extension_distance"),
              tractors = by_rule("community_tractors"),
              records  = sum(d$n_records[d$rule == "extension_distance"]),
              waves    = WAVES,
              source   = .SUPP_CSV)
  message("301: suppression over ", paste(WAVES, collapse = "/"), " -- ",
          "distance ", out$distance$suppressed, " of ", out$records,
          " community records, tractors ", out$tractors$suppressed,
          "; item non-response is larger (", out$distance$missing, " and ",
          out$tractors$missing, ").")
  out
})

# =============================================================================
#  NOT EXTRACTED HERE, deliberately
# =============================================================================
#  * Every number a table prints. Those go through tbl_num() / tbl_pct() so the
#    sentence and the cell are one build. Adding a duplicate here is how the two
#    start disagreeing.
#  * sf_estm's Nobs. It is the estimating model's N after the matched-sample
#    restriction, not the analysis sample the text quotes. prevalence$n_all is
#    the latter; the two are different numbers and must not be swapped.

# ---- Diagnostics -------------------------------------------------------------
.unresolved <- function(x, nm) {
  u <- unlist(x)
  if (!length(u) || !anyNA(u)) return(character(0))
  paste0(nm, "$", names(u)[is.na(u)])
}
NA_FIELDS <- c(.unresolved(eff, "eff"),
               .unresolved(elasticities, "elasticities"),
               .unresolved(gamma, "gamma"))
if (length(NA_FIELDS)) {
  message("301: ", length(NA_FIELDS), " field(s) did not resolve:")
  message("  ", paste(utils::head(NA_FIELDS, 20), collapse = "\n  "))
  message("  Codes actually present:")
  message("    ef_mean TCHLvel  : {", paste(sort(unique(as.character(pooled$ef_mean$TCHLvel))), collapse = ", "), "}")
  message("    el_mean input    : {", paste(sort(unique(as.character(pooled$el_mean$input))), collapse = ", "), "}")
  message("    el_mean CoefName : {", paste(sort(unique(as.character(pooled$el_mean$CoefName))), collapse = ", "), "}")
  message("    el_mean sample   : {", paste(sort(unique(as.character(pooled$el_mean$sample))), collapse = ", "), "}")
  message("    sf_estm sample   : {", paste(sort(unique(as.character(sf$sample))), collapse = ", "), "}")
  message("  An NA here becomes `null` in the json. A prose chunk that cites it ",
          "fails at knit\n  time rather than printing a blank -- which is the ",
          "intended behaviour, but fix the key.")
}
if (length(.AMBIG))
  message("301: ", length(.AMBIG), " lookup(s) matched more than one row; the ",
          "first was taken:\n  ", paste(unique(.AMBIG), collapse = "\n  "),
          "\n  A key that is no longer unique is a FINDING. Re-pin it.")

objs <- list(
  meta = list(
    generated      = as.character(Sys.time()),
    study          = "ag_services",
    source         = "output/estimations/CropID_Pooled_<services0-3>_TL_hnormal_optimal.rds",
    matched_sample = opt_sample,
    service_tags   = as.list(SERVICE_TAGS),
    unresolved     = NA_FIELDS,
    ambiguous      = unique(.AMBIG)
  ),
  prevalence    = prevalence,     # service take-up by wave (02_data)
  suppression   = suppression,    # implausible-value rules, from the Stata probe (02_data)
  eff           = eff,            # the four service sources (Table 7's content)
  by_wave       = by_wave,        # the gap round by round (Figure 1, section 5.3)
  elasticities  = elasticities,   # Table 6: el1..el6 inputs + el7 = returns to scale
  functional_form = functional_form,  # Cobb-Douglas vs translog LR test (section 4.2)
  by_service    = by_service,     # gap by TYPE of service (Figure 2)
  heterogeneity = heterogeneity,  # gap by farmer, crop, region (Figures 3, 4)
  ranked        = ranked,         # MTE percent gaps, ordered (the discussion)
  diagnostics   = list(gamma = gamma, mono = mono, curv = curv)
)

jsonlite::write_json(objs, OBJECTS_JSON, auto_unbox = TRUE, pretty = TRUE,
                     na = "null")
message("Wrote ", OBJECTS_JSON)
invisible(TRUE)
