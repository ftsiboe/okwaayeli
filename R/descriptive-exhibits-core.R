# descriptive-exhibits-core.R
# Descriptive exhibit engine: computes Tables 1 and 2 and their appendix
# analogues directly from a study's merged analysis data, replacing the
# 100_*.do -> .xlsx -> 305_tables.R chain.
#
# Two engines, one schema:
#   Engine A  outcome x treatment group   (means, per-wave means, trends, diff tests)
#   Engine B  category x wave             (shares by wave, pooled share, % change)
# Engine B is Engine A with a dummy outcome and no treatment grouping, so both
# emit the same long frame; see draw_descriptive_summary().
#
# Parity target: studies/resource_extraction/output/*.xlsx sheets "means" and
# "resource_extraction". RE is the reference because it is the only study whose
# means sheet is keyed by outcome, not by matrix row name.

#' Build a Descriptive Exhibit Specification Grid
#'
#' @description
#' Enumerates the (treatment x crop x outcome) combinations a study's descriptive
#' exhibits require. Mirrors `sf_model_specifications()` for the frontier layer.
#'
#' @details
#' The land tenure study's `100_exhibits.do` hardcodes `i.OwnLnd` at every site
#' and has no treatment loop, which is why it cannot vary its treatment without
#' editing code. Every other study generalises to a `disagCat` column. This
#' function makes the loop a data structure, so a study varies its treatment by
#' passing a longer `treatments` vector.
#'
#' @param data `data.frame` of the merged analysis sample, normally
#'   `study_environment$study_raw_data`.
#' @param outcomes Character vector of outcome columns (Engine A), e.g.
#'   `c("Yield","Area","SeedKg", ...)`.
#' @param treatments Character vector of binary treatment columns, e.g.
#'   `"OwnLnd"` or `c("extraction_any","mining_any", ...)`.
#' @param crops Character vector of crops to enumerate, or `NULL` (default) to
#'   take every level present in `crop_var`.
#' @param crop_var Character. Column holding the crop. Default `"CropID"`.
#' @param families Either a single family applied to every outcome
#'   (`"gaussian"`, the default), or a **named** character vector mapping outcome
#'   to family. See *Outcomes do not all share a model*.
#'
#' @section Outcomes do not all share a model:
#' A study's Table 1 is usually estimated with more than one model, and the
#' do-files express that as separate loops. In `land_tenure/scripts/100_exhibits.do`:
#'
#' \preformatted{
#' line  51  foreach Var of var Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt
#'                              AgeYr YerEdu HHSizeAE Depend CrpMix
#'           -> reg `Var' c.Trend##i.OwnLnd            (gaussian)
#'
#' line 103  foreach Var of var Female EqipMech Credit Extension EqipIrig
#'           -> logit `Var' c.Trend##i.OwnLnd          (binomial)
#' }
#'
#' Both write to the same sheet, so the distinction is invisible downstream --
#' `Equ == "Female"` and `Equ == "Yield"` sit side by side with no record that one
#' came from a logit. Passing a named `families` vector makes it explicit and
#' carries it into the emitted schema:
#'
#' \preformatted{
#' descriptive_specifications(
#'   d, outcomes = c(cont, bin), treatments = "OwnLnd",
#'   families = c(setNames(rep("gaussian", length(cont)), cont),
#'                setNames(rep("binomial",  length(bin)),  bin)))
#' }
#'
#' @return A `data.frame` with columns `treatment`, `crop`, `outcome`, `family`.
#'
#' @family descriptive exhibits
#' @export
descriptive_specifications <- function(data, outcomes, treatments,
                                       crops = NULL, crop_var = "CropID",
                                       families = "gaussian") {
  if (!crop_var %in% names(data))
    stop("descriptive_specifications(): no column '", crop_var, "'.", call. = FALSE)
  if (is.null(crops)) crops <- unique(as.character(data[[crop_var]]))
  miss <- setdiff(c(outcomes, treatments), names(data))
  if (length(miss))
    stop("descriptive_specifications(): missing columns: ",
         paste(miss, collapse = ", "), call. = FALSE)

  if (length(families) == 1L && is.null(names(families))) {
    fam <- stats::setNames(rep(families, length(outcomes)), outcomes)
  } else {
    if (is.null(names(families)))
      stop("descriptive_specifications(): `families` must be length one or named.",
           call. = FALSE)
    unset <- setdiff(outcomes, names(families))
    if (length(unset))
      stop("descriptive_specifications(): no family given for: ",
           paste(unset, collapse = ", "),
           ". Name every outcome, or pass a single family for all.", call. = FALSE)
    fam <- families[outcomes]
  }
  bad <- setdiff(unique(fam), c("gaussian", "binomial"))
  if (length(bad))
    stop("descriptive_specifications(): unsupported family: ",
         paste(bad, collapse = ", "), call. = FALSE)

  g <- expand.grid(outcome = outcomes, crop = crops, treatment = treatments,
                   stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  g$family <- unname(fam[g$outcome])
  g[, c("treatment", "crop", "outcome", "family")]
}

#' Add the Trend and Cluster Columns Used by the Descriptive Engines
#'
#' @description
#' Reproduces the two derived columns every `100_*.do` builds before estimating:
#' `Trend = Season - min(Season)` and a cluster id grouping
#' `Survey Ecozon EaId HhId`.
#'
#' @details
#' `Trend` is computed **within the subset passed in**, matching the Stata code,
#' which recomputes `sum Season` / `r(min)` after `keep if CropIDx == "<crop>"`.
#' A crop whose earliest observation is not GLSS3 therefore has its trend origin
#' at its own first wave, not the study's. This is faithful, not incidental --
#' change it only deliberately.
#'
#' @section Season must be a year, not a factor code:
#' `Season` arrives from `get_household_data()` as an R **factor**, because the
#' harmonized `.dta` labels it and `haven::as_factor()` is applied on read. Its
#' levels span the whole GLSS series:
#'
#' \preformatted{
#' 1987/88 1988/89 1990/91 1991/92 1997/98 1998/99 2004/05 2005/06
#' 2009/10 2011/12 2012/13 2013/14 2014/15 2015/16 2016/17
#' }
#'
#' `as.numeric()` on that returns the **level code**, not the year. For
#' resource_extraction the four waves sit at codes 5, 6, 7, 8, 10, 11, 14, 15, so
#' a naive conversion yields a trend of `0,1,2,3,5,6,9,10` where Stata --- whose
#' `Season` is the numeric year --- gets `0,1,7,8,14,15,18,19`. The trend
#' coefficient then comes out roughly twice too large, and every "annual
#' percentage change" with it. The error is silent: the numbers look reasonable.
#'
#' This function therefore parses the leading four-digit year out of the label
#' (`"1997/98" -> 1997`) whenever `Season` is not already numeric, so the unit of
#' `.trend` is a year and the exhibits' "annual" label is true.
#'
#' @param data `data.frame` subset (normally one crop).
#' @param season_var Character. Default `"Season"`. Numeric years, or a
#'   factor/character whose labels begin with a four-digit year.
#' @param cluster_vars Character vector. Default
#'   `c("Survey","Ecozon","EaId","HhId")`.
#'
#' @return `data` with `.trend` (years since the subset's first wave) and
#'   `.clust` added.
#'
#' @family descriptive exhibits
#' @export
descriptive_prepare <- function(data, season_var = "Season",
                                cluster_vars = c("Survey", "Ecozon", "EaId", "HhId")) {
  if (!season_var %in% names(data))
    stop("descriptive_prepare(): no column '", season_var, "'.", call. = FALSE)
  cl <- intersect(cluster_vars, names(data))
  if (!length(cl))
    stop("descriptive_prepare(): none of the cluster columns present: ",
         paste(cluster_vars, collapse = ", "), call. = FALSE)
  data$.trend <- descriptive_season_year(data[[season_var]]) -
    min(descriptive_season_year(data[[season_var]]), na.rm = TRUE)
  data$.clust <- interaction(data[cl], drop = TRUE)
  data
}

#' Coerce a Season Column to a Numeric Year
#'
#' @description
#' Returns the calendar year for a `Season` column, whether it arrives as a
#' number or as a labelled factor such as `"2016/17"`.
#'
#' @details
#' Guards against `as.numeric(factor)`, which silently returns the level index.
#' See the *Season must be a year* section of `descriptive_prepare()`.
#'
#' @param x Numeric, factor or character season column.
#'
#' @return A numeric vector of four-digit years.
#'
#' @examples
#' descriptive_season_year(factor(c("1997/98", "2016/17")))
#'
#' @family descriptive exhibits
#' @export
descriptive_season_year <- function(x) {
  if (is.numeric(x) && !is.factor(x)) {
    y <- as.numeric(x)
    if (all(is.na(y) | (y > 1900 & y < 2100)))
      return(y)
    stop("descriptive_season_year(): numeric season outside 1900-2100; ",
         "range = ", paste(range(y, na.rm = TRUE), collapse = "-"),
         ". These look like factor codes, not years.", call. = FALSE)
  }
  lab <- as.character(x)
  y <- suppressWarnings(as.numeric(sub("^[^0-9]*([0-9]{4}).*$", "\\1", lab)))
  bad <- is.na(y) & !is.na(lab)
  if (any(bad))
    stop("descriptive_season_year(): no four-digit year in: ",
         paste(unique(lab[bad])[1:min(5, sum(bad))], collapse = ", "),
         call. = FALSE)
  y
}

#' Weighted Summary Statistics by Group
#'
#' @description
#' Engine A's aggregation step: mean, standard error of the mean, min, max,
#' standard deviation and n, computed overall and by treatment group, and again
#' within each survey wave. Replaces Stata's `tabstat ..., stat(mean sem min max
#' sd n) by()`.
#'
#' @details
#' With `weights = NULL` (default) this reproduces the Stata path exactly: no
#' `100_*.do` in the repository applies sampling weights, despite `WeightHH`
#' being available. Supplying `weights = "WeightHH"` yields
#' population-representative means; see the *Divergences* section of
#' `studies/README_descriptive_exhibits_plan.md`.
#'
#' @section Rows with a missing treatment are dropped:
#' When `treatment` is supplied, **every** row it emits -- including the pooled
#' row -- is restricted to observations with a non-missing treatment. This
#' matches `tabstat <var>, by(<treatment>)`, which excludes observations whose
#' by-variable is missing, and it keeps the pooled row on the same sample as the
#' group rows.
#'
#' It matters. In resource_extraction the commercial/informal mining split exists
#' only in GLSS6/GLSS7, so `mining_comm` and `mining_gala` are missing for all
#' 10,416 GLSS4/GLSS5 observations. Pooling over every row regardless gives
#' n = 26,811 against Stata's 16,395, and a mean drawn from a different
#' population than the groups it sits beside.
#'
#' @param data `data.frame`, already subset to one crop.
#' @param outcome Character. Outcome column.
#' @param treatment Character or `NULL`. Binary treatment column. `NULL` emits
#'   pooled rows only (Engine B).
#' @param wave_var Character. Default `"Surveyx"`.
#' @param weights Character or `NULL`. Column of sampling weights.
#'
#' @return A `data.frame` with `wave`, `group`, `statistic = "mean"`,
#'   `estimate`, `se`, `min`, `max`, `sd`, `n`.
#'
#' @family descriptive exhibits
#' @export
descriptive_group_summary <- function(data, outcome, treatment = NULL,
                                      wave_var = "Surveyx", weights = NULL) {
  one <- function(d, wave, group) {
    y <- as.numeric(d[[outcome]])
    w <- if (is.null(weights)) rep(1, length(y)) else as.numeric(d[[weights]])
    ok <- !is.na(y) & !is.na(w) & w > 0
    y <- y[ok]; w <- w[ok]
    n <- length(y)
    if (!n) return(NULL)
    m  <- sum(w * y) / sum(w)
    # Weighted variance with an (n-1)/n bias correction on the weight total.
    # With w == 1: sum(w) - sum(w)/n == n - 1, so this reduces exactly to the
    # ordinary sample variance and matches Stata's tabstat sd -- which is the
    # parity case, since no 100_*.do applies weights.
    v  <- if (n > 1) sum(w * (y - m)^2) / (sum(w) - sum(w) / n) else NA_real_
    sd <- sqrt(v)
    data.frame(wave = wave, group = group, statistic = "mean",
               estimate = m, se = if (n > 1) sd / sqrt(n) else NA_real_,
               min = min(y), max = max(y), sd = sd, n = n,
               stringsAsFactors = FALSE)
  }
  grp <- function(d, wave) {
    out <- list(one(d, wave, "pooled"))
    if (!is.null(treatment)) {
      g <- as.character(d[[treatment]])
      for (lv in sort(unique(g[!is.na(g)])))
        out[[length(out) + 1]] <- one(d[!is.na(g) & g == lv, , drop = FALSE], wave, lv)
    }
    do.call(rbind, out)
  }
  # Drop rows with a missing treatment BEFORE anything else, so the pooled row
  # sits on the same sample as the group rows -- see "Rows with a missing
  # treatment are dropped". Without this, mining_comm's pooled n is 26,811
  # against Stata's 16,395.
  if (!is.null(treatment)) {
    if (!treatment %in% names(data))
      stop("descriptive_group_summary(): no column '", treatment, "'.", call. = FALSE)
    data <- data[!is.na(data[[treatment]]), , drop = FALSE]
    if (!nrow(data)) return(NULL)
  }
  res <- list(grp(data, "all"))
  if (wave_var %in% names(data)) {
    for (w in sort(unique(as.character(data[[wave_var]]))))
      res[[length(res) + 1]] <-
        grp(data[as.character(data[[wave_var]]) == w, , drop = FALSE], w)
  }
  do.call(rbind, res)
}

#' Trend and Difference Tests for One Outcome
#'
#' @description
#' Engine A's estimation step. Fits `outcome ~ trend * treatment` with clustered
#' standard errors and returns the per-group percentage trend plus the two Wald
#' tests the exhibits report. Replaces Stata's
#' `reg y c.Trend##i.g, vce(cluster C)` + `testparm` + `margins ... eydx` +
#' `nlcom (... * 100)`.
#'
#' @details
#' Emitted statistics:
#'
#' \describe{
#'   \item{`trend_pct`}{Average semi-elasticity of `outcome` with respect to
#'     `trend`, times 100 -- i.e. the annual percentage change. Stata computes
#'     this as `margins <g>, eydx(Trend)` followed by `nlcom (_b[...] * 100)`;
#'     here it is `marginaleffects::avg_slopes()` with `slope = "eydx"`. Emitted
#'     per group and pooled.}
#'   \item{`cat_diff`}{`testparm i.g` -- the treatment main effect, i.e. the
#'     group level difference **at `trend == 0`**, not the difference in overall
#'     means. This is why an outcome can show a large raw gap yet an
#'     insignificant `cat_diff`.}
#'   \item{`trend_diff`}{`testparm c.Trend#i.g` -- the interaction.}
#' }
#'
#' Only `p` is populated for the two Wald rows, matching the sheet, where
#' `CATDif` / `TrendDif` carry a p-value and nothing else.
#'
#' @param data `data.frame` from `descriptive_prepare()`, subset to one crop.
#' @param outcome Character. Outcome column.
#' @param treatment Character or `NULL`. Binary treatment column.
#' @param family `"gaussian"` (default) or `"binomial"` for a dummy outcome.
#' @param weights Character or `NULL`.
#' @param ssc `fixest::ssc()` object controlling the small-sample correction.
#'   Defaults to `NULL` (fixest default), which needs no tuning to match the
#'   reference: point estimates and p-values both agree.
#' @param min_events Integer. For `family = "binomial"`, the minimum number of
#'   observations in the rarer class -- overall and within each treatment group --
#'   below which no model is fitted and `NULL` is returned. Guards against the
#'   near-separation fits that produce nonsense trends for rare indicators inside
#'   small crops. Default 10.
#' @param min_group_n Integer. Minimum observations in the smaller treatment
#'   group, for either family. Default 30.
#'
#' @section Refusing an unestimable trend:
#' A trend is only fitted where the data can carry it. Both guards return `NULL`
#' rather than a number, and `descriptive_workhorse()` logs the refusal.
#'
#' This is a deliberate divergence from the reference implementation, and the
#' only one in this file. Wrapping an estimation in a swallow-all handler leaves
#' the exported cell at whatever it held, and the consequences reach the page:
#'
#' \preformatted{
#' Pineapple Depend Trend_disagCat1   -669,487,338,753,097,984  (percent/year)
#' Pineapple FertKg Trend_disagCat1   -153,054,440,947,974,016
#' Onion     PestLt Trend_disagCat1      8,109,369,589,760
#' salt      x Tomatoe  Trend                              0    (fit failed)
#' }
#'
#' Those are collapsed fits recorded as estimates. Reproducing them would be
#' faithful and wrong: an annual percentage change of -6.7e17 is not a finding,
#' and a crop-by-treatment trend estimated off a handful of farms should not be
#' printed at all. Where a cell is genuinely estimable this function matches
#' Stata exactly (14,724 parity assertions, both engines, 7 treatments, 28 crops).
#'
#' Raise `min_group_n` / `min_events` to be stricter, or lower them to 0 to
#' reproduce the Stata behaviour including its failures.
#'
#' @return A `data.frame` with `wave = "all"`, `group`, `statistic`, `estimate`,
#'   `se`, `t`, `p`; or `NULL` if the data cannot support the model.
#'
#' @family descriptive exhibits
#' @export
descriptive_trend_model <- function(data, outcome, treatment = NULL,
                                    family = c("gaussian", "binomial"),
                                    weights = NULL, ssc = NULL,
                                    min_events = 10L, min_group_n = 30L) {
  family <- match.arg(family)
  for (p in c("fixest", "marginaleffects"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("descriptive_trend_model(): package '", p, "' is required.", call. = FALSE)

  d <- data
  d$.y <- as.numeric(d[[outcome]])
  d$.w <- if (is.null(weights)) 1 else as.numeric(d[[weights]])
  has_t <- !is.null(treatment)
  if (has_t) d$.g <- factor(as.character(d[[treatment]]))
  keep <- !is.na(d$.y) & !is.na(d$.trend) & !is.na(d$.w) & d$.w > 0
  if (has_t) keep <- keep & !is.na(d$.g)
  d <- d[keep, , drop = FALSE]
  if (nrow(d) < 10 || (has_t && nlevels(droplevels(d$.g)) < 2)) return(NULL)
  if (has_t) d$.g <- droplevels(d$.g)

  # Refuse a fit the data cannot support, rather than emitting a number that
  # merely looks like one. See "Refusing an unestimable trend".
  if (length(unique(d$.trend)) < 2) return(NULL)

  # Group size, either family. Below this there is too little within-crop
  # variation for `y ~ trend * g`, and the eydx slope divides by fitted values
  # near zero. Small crops are where this bites.
  if (has_t && min(table(d$.g)) < min_group_n) return(NULL)

  if (family == "binomial") {
    # Near-separation: a rare indicator inside a small crop.
    tb <- table(d$.y)
    if (length(tb) < 2 || min(tb) < min_events) return(NULL)
    if (has_t)
      for (lv in levels(d$.g)) {
        t2 <- table(d$.y[d$.g == lv])
        if (length(t2) < 2 || min(t2) < min_events) return(NULL)
      }
  } else if (stats::var(d$.y, na.rm = TRUE) == 0) return(NULL)

  fml <- if (has_t) stats::as.formula(".y ~ .trend * .g") else stats::as.formula(".y ~ .trend")
  fit <- try({
    if (family == "gaussian")
      fixest::feols(fml, data = d, cluster = ~ .clust, weights = ~ .w,
                    ssc = ssc, notes = FALSE)
    else
      fixest::feglm(fml, data = d, family = "logit", cluster = ~ .clust,
                    weights = ~ .w, ssc = ssc, notes = FALSE)
  }, silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)

  # trend_pct: average semi-elasticity x 100, by group and pooled.
  #
  # Stata's `margins <g>, eydx(Trend)` is COUNTERFACTUAL ("at"), not a subgroup
  # average ("over"): for each level of <g> it sets <g> to that level for EVERY
  # observation, computes the semi-elasticity, and averages over the whole
  # sample. marginaleffects' `by=` does the opposite -- it averages within the
  # rows that actually have that level. The two estimands coincide only for the
  # pooled row.
  #
  # The difference is large and worst for the minority group -- with `by=`, a
  # group holding 11% of rows came out at 12.7 against a true 44.9, while the
  # pooled row was exact. An easy error to miss for exactly that reason.
  #
  # The counterfactual frame is built by hand rather than with
  # marginaleffects::datagridcf(), which is absent in older versions.
  sl <- try({
    by_g <- NULL
    if (has_t) {
      lv <- levels(d$.g)
      cf <- do.call(rbind, lapply(lv, function(l) {
        z <- d; z$.g <- factor(l, levels = lv); z
      }))
      by_g <- marginaleffects::avg_slopes(fit, variables = ".trend", by = ".g",
                                          slope = "eydx", newdata = cf)
    }
    all_g <- marginaleffects::avg_slopes(fit, variables = ".trend",
                                         slope = "eydx", newdata = d)
    list(by_g = by_g, all_g = all_g)
  }, silent = TRUE)
  if (inherits(sl, "try-error")) return(NULL)

  mk <- function(x, group) data.frame(
    wave = "all", group = group, statistic = "trend_pct",
    estimate = as.numeric(x$estimate) * 100,
    se = as.numeric(x$std.error) * 100,
    t = as.numeric(x$statistic), p = as.numeric(x$p.value),
    stringsAsFactors = FALSE)

  out <- list(mk(sl$all_g, "pooled"))
  if (has_t && !is.null(sl$by_g))
    for (i in seq_len(nrow(sl$by_g)))
      out[[length(out) + 1]] <- mk(sl$by_g[i, ], as.character(sl$by_g$.g[i]))

  if (has_t) {
    wald <- function(pat, stat) {
      w <- try(fixest::wald(fit, pat, print = FALSE), silent = TRUE)
      # fixest::wald() does not always return a list: with no matching term, or a
      # degenerate fit, it can hand back an atomic vector or NULL. Dereferencing
      # w$stat then errors with "$ operator is invalid for atomic vectors" and
      # takes the whole sweep down -- reachable on the crop dimension.
      if (inherits(w, "try-error") || is.null(w) || !is.list(w) ||
          is.null(w$stat) || is.null(w$p)) return(NULL)
      data.frame(wave = "all", group = NA_character_, statistic = stat,
                 estimate = NA_real_, se = NA_real_,
                 t = as.numeric(w$stat)[1], p = as.numeric(w$p)[1],
                 stringsAsFactors = FALSE)
    }
    # "^.g" matches the treatment main effect(s); "\\.trend:" the interaction.
    out[[length(out) + 1]] <- wald("^\\.g",        "cat_diff")
    out[[length(out) + 1]] <- wald("^\\.trend:\\.g", "trend_diff")
  }
  do.call(rbind, Filter(Negate(is.null), out))
}

#' Expand a Categorical Column into Indicator Dummies
#'
#' @description
#' Reproduces Stata's `tab <var>, gen(<var>_)`: one 0/1 column per level, named
#' `<var>_1`, `<var>_2`, ... in level order.
#'
#' @param data `data.frame`.
#' @param category_var Character. Categorical column, e.g. `"LndOwn"`.
#'
#' @return `data` with the dummy columns appended. The new names are returned in
#'   `attr(x, "indicators")`.
#'
#' @family descriptive exhibits
#' @export
descriptive_expand_category <- function(data, category_var) {
  v <- as.character(data[[category_var]])
  lv <- sort(unique(v[!is.na(v)]))
  nm <- paste0(category_var, "_", seq_along(lv))
  for (i in seq_along(lv)) data[[nm[i]]] <- as.numeric(!is.na(v) & v == lv[i])
  attr(data, "indicators") <- nm
  data
}

#' Indicator Shares by Wave (Engine B)
#'
#' @description
#' Returns the share of each binary indicator overall, optionally by wave, plus a
#' trend. Produces the rows behind Table 2 and its crop-disaggregated appendix
#' analogues (land's Tables S1--S4, resource_extraction's extraction table).
#'
#' @section Two trend flavors:
#' The studies do **not** use the same estimator here, and the difference is
#' substantive rather than cosmetic:
#'
#' \describe{
#'   \item{`trend = "continuous"` (resource_extraction)}{
#'     `logit <ind> Trend` then `margins, eydx(Trend) predict(pr)` and
#'     `nlcom (_b[Trend]*100)`. Emits a **semi-elasticity**: percent change in the
#'     probability per year. Only the pooled share (`GLSS0`) accompanies it.}
#'   \item{`trend = "wave_diff"` (land_tenure)}{
#'     `logit <ind> i.Survey` then `margins Survey` and
#'     `nlcom ((_b[6bn.Survey]-_b[7.Survey])*100)`. Emits a **percentage-point**
#'     difference between two waves, and per-wave shares accompany it.
#'     Note the sign: it is **earlier minus later**, so an indicator that grew
#'     reports a negative value. Land's Table 2 shows `Not owned = -9.743` while
#'     the share rose from 0.290 to 0.388. That is the convention, not an error.}
#' }
#'
#' Mixing them up yields plausible numbers on the wrong scale -- percent per year
#' versus percentage points -- so `trend` has no default.
#'
#' @param data `data.frame` from `descriptive_prepare()`, subset to one crop.
#' @param indicators Character vector of binary columns. Use
#'   `descriptive_expand_category()` first for a categorical source.
#' @param trend `"continuous"`, `"wave_diff"` or `"none"`. See *Two trend flavors*.
#' @param waves Character vector of length 2, `c(from, to)`, required when
#'   `trend = "wave_diff"`.
#' @param per_wave Logical. Emit per-wave shares as well as the pooled share.
#'   `TRUE` for land, `FALSE` for resource_extraction (whose do-file collects
#'   only `StatTotal`).
#' @param wave_var Character. Default `"Surveyx"`.
#' @param weights Character or `NULL`.
#'
#' @return A `data.frame` with `outcome`, `wave`, `group = NA`, `statistic`,
#'   `estimate`, `se`, `min`, `max`, `sd`, `n`. The pooled share is emitted with
#'   `wave = "pooled"` (the sheets call it `GLSS0`); the trend with
#'   `wave = "trend"`.
#'
#' @family descriptive exhibits
#' @export
descriptive_indicator_shares <- function(data, indicators,
                                         trend = c("continuous", "wave_diff", "none"),
                                         waves = NULL, per_wave = TRUE,
                                         wave_var = "Surveyx", weights = NULL) {
  trend <- match.arg(trend)
  if (trend == "wave_diff" && (is.null(waves) || length(waves) != 2))
    stop("descriptive_indicator_shares(): trend='wave_diff' needs waves=c(from,to).",
         call. = FALSE)
  out <- list()
  for (v in indicators) {
    if (!v %in% names(data)) next
    s <- descriptive_group_summary(data, v, treatment = NULL,
                                   wave_var = wave_var, weights = weights)
    if (is.null(s)) next
    s$wave[s$wave == "all"] <- "pooled"
    if (!per_wave) s <- s[s$wave == "pooled", , drop = FALSE]
    s$outcome <- v
    out[[length(out) + 1]] <- s

    if (trend == "continuous") {
      m <- descriptive_trend_model(data, v, treatment = NULL,
                                   family = "binomial", weights = weights)
      if (!is.null(m)) {
        m <- m[m$statistic == "trend_pct", , drop = FALSE]
        if (nrow(m)) out[[length(out) + 1]] <- data.frame(
          wave = "trend", group = NA_character_, statistic = "trend_pct",
          estimate = m$estimate[1], se = m$se[1], min = NA_real_, max = NA_real_,
          sd = NA_real_, n = NA_real_, outcome = v, stringsAsFactors = FALSE)
      }
    } else if (trend == "wave_diff") {
      a <- s$estimate[s$wave == waves[1]]
      b <- s$estimate[s$wave == waves[2]]
      if (length(a) && length(b)) out[[length(out) + 1]] <- data.frame(
        wave = "trend", group = NA_character_, statistic = "change_pp",
        estimate = (a[1] - b[1]) * 100, se = NA_real_, min = NA_real_,
        max = NA_real_, sd = NA_real_, n = NA_real_, outcome = v,
        stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

#' Run One Descriptive Specification
#'
#' @param spec_row One row of `descriptive_specifications()`.
#' @param data `data.frame` of the merged analysis sample.
#' @param crop_var,wave_var,season_var,cluster_vars Column names.
#' @param weights Character or `NULL`.
#' @param family Passed to `descriptive_trend_model()`. Ignored when `spec_row`
#'   carries a `family` column, which `descriptive_specifications()` emits.
#' @param ssc Passed to `descriptive_trend_model()`.
#' @param quiet Logical. If `FALSE` (default) a specification that yields nothing
#'   emits a message. The Stata path wrapped every specification in `cap{ }`,
#'   which swallowed failures silently -- `LndAq_6` is missing from the land sheet
#'   for exactly this reason. Do not restore that behaviour.
#'
#' @return A long `data.frame`, or `NULL`.
#'
#' @family descriptive exhibits
#' @export
descriptive_workhorse <- function(spec_row, data, crop_var = "CropID",
                                  wave_var = "Surveyx", season_var = "Season",
                                  cluster_vars = c("Survey", "Ecozon", "EaId", "HhId"),
                                  weights = NULL, family = "gaussian",
                                  ssc = NULL, quiet = FALSE) {
  # The grid's per-outcome family wins over the argument default: land's Table 1
  # is a reg for 12 outcomes and a logit for 5, in one table.
  if (!is.null(spec_row$family) && !is.na(spec_row$family))
    family <- as.character(spec_row$family)
  d <- data[as.character(data[[crop_var]]) == spec_row$crop, , drop = FALSE]
  if (!nrow(d)) {
    if (!quiet) message("descriptive_workhorse(): no rows for crop '",
                        spec_row$crop, "'.")
    return(NULL)
  }
  d <- descriptive_prepare(d, season_var = season_var, cluster_vars = cluster_vars)

  s <- descriptive_group_summary(d, spec_row$outcome, spec_row$treatment,
                                 wave_var = wave_var, weights = weights)
  m <- descriptive_trend_model(d, spec_row$outcome, spec_row$treatment,
                               family = family, weights = weights, ssc = ssc)
  if (is.null(m) && !quiet)
    message("descriptive_workhorse(): no trend model for ", spec_row$treatment,
            " / ", spec_row$crop, " / ", spec_row$outcome,
            " (n = ", nrow(d), ").")

  res <- rbind(
    if (!is.null(s)) data.frame(s[setdiff(names(s), c("t", "p"))],
                                t = NA_real_, p = NA_real_, stringsAsFactors = FALSE),
    if (!is.null(m)) data.frame(m, min = NA_real_, max = NA_real_,
                                sd = NA_real_, n = NA_real_, stringsAsFactors = FALSE)
  )
  if (is.null(res)) return(NULL)
  res$treatment <- spec_row$treatment
  res$crop <- spec_row$crop
  res$outcome <- spec_row$outcome
  res$family <- family
  res[, c("treatment", "crop", "outcome", "family", "wave", "group", "statistic",
          "estimate", "se", "t", "p", "min", "max", "sd", "n")]
}

#' Compute a Study's Descriptive Exhibits
#'
#' @description
#' Runs every specification and binds the results into the long frame the table
#' builders consume. This is the Stata-free, Excel-free replacement for a study's
#' `100_*.do` descriptive output.
#'
#' @details
#' The returned frame is keyed, not positional. Builders filter it on
#' `(treatment, crop, outcome, wave, group, statistic)`. Addressing results
#' positionally -- by row name or matrix equation -- lets two outcomes collide
#' under one label and produces a plausible wrong number rather than an error.
#'
#' `attr(x, "weights")` records whether weights were applied, so a downstream
#' table can state it rather than assume it.
#'
#' @param specifications From `descriptive_specifications()`.
#' @param data `data.frame`, normally `study_environment$study_raw_data`.
#' @param study Character. Study name, emitted as a column.
#' @param ... Passed to `descriptive_workhorse()`.
#'
#' @return A long `data.frame`: `study`, `treatment`, `crop`, `outcome`, `wave`,
#'   `group`, `statistic`, `estimate`, `se`, `t`, `p`, `min`, `max`, `sd`, `n`.
#'
#' @examples
#' \dontrun{
#' se <- readRDS("studies/resource_extraction/data/resource_extraction_study_environment.rds")
#' d  <- se$study_raw_data
#' sp <- descriptive_specifications(
#'   d,
#'   outcomes   = c("Yield","Area","SeedKg","HHLaborAE","HirdHr","FertKg",
#'                  "PestLt","AgeYr","YerEdu","HHSizeAE","Depend","CrpMix"),
#'   treatments = c("extraction_any","mining_any","mining_comm","mining_gala",
#'                  "quarrying","sand","salt"))
#' res <- draw_descriptive_summary(sp, d, study = "resource_extraction")
#' }
#'
#' @family descriptive exhibits
#' @export
draw_descriptive_summary <- function(specifications, data, study = NA_character_, ...) {
  out <- vector("list", nrow(specifications))
  for (i in seq_len(nrow(specifications)))
    out[[i]] <- descriptive_workhorse(specifications[i, , drop = FALSE], data, ...)
  res <- do.call(rbind, Filter(Negate(is.null), out))
  if (is.null(res)) return(NULL)
  res$study <- study
  res <- res[, c("study", setdiff(names(res), "study"))]
  w <- list(...)$weights
  attr(res, "weights") <- if (is.null(w)) "none" else w
  res
}
