# Summarize multi-draw Meta Stochastic Frontier (MSF) results

Aggregates results from multiple stochastic frontier analysis (SFA) /
MSF draws (e.g., from a bootstrap or jackknife procedure) and computes
jackknife-style summary statistics for coefficients, efficiency scores,
elasticities, risk measures, and their distributions.

## Usage

``` r
draw_msf_summary(res, technology_legend)
```

## Arguments

- res:

  List of draw-level result objects. Each element is expected to be a
  list with (at least) the following components:

  - `sf_estm`: coefficient-level estimates and diagnostics for naive,
    group, and meta-frontiers, including a `draw` index.

  - `ef_mean`: aggregated efficiency statistics (e.g., TE0, TE, TGR,
    MTE) by sample, technology, survey, and estimation type.

  - `el_mean`: aggregated elasticity statistics by input, technology,
    and survey.

  - `rk_mean`: aggregated risk statistics, if available.

  - `ef_dist`: histogram-style efficiency distributions (counts/weights)
    over value ranges (`binned_range_name`, `binned_range_level`).

  - `rk_dist`: histogram-style risk distributions, if available.

  - `disagscors`: optional disaggregated efficiency summaries by
    subgroup levels.

  - `el_samp`, `ef_samp`, `rk_samp`: observation-level elasticities,
    efficiencies, and risk measures for at least the baseline draw
    (usually `draw == 0`).

  Each component must include a `draw` column that distinguishes the
  baseline draw (typically `draw == 0`) from resampled draws
  (`draw != 0`).

- technology_legend:

  Data.frame providing the mapping between numeric technology codes and
  their labels, typically with at least:

  - `Tech`: numeric technology index used internally in MSF estimation,
    and

  - a corresponding label column (e.g., the original
    `technology_variable`).

  This is used to compute technology gaps (e.g., efficiency or
  disaggregated efficiency gaps) relative to the reference technology
  (smallest value in ` technology_legend$Tech`).

## Value

A list with the following components:

- `sf_estm`: Data.frame of jackknife summary statistics for frontier
  coefficients and diagnostics across draws, with columns such as
  `Estimate`, `Estimate.mean`, `Estimate.sd`, `Estimate.length`,
  `jack_zv`, and `jack_pv`, indexed by `Survey`, `CoefName`, `Tech`,
  `sample`, and `restrict`.

- `ef_mean`: Jackknife summaries for efficiency statistics (TE0, TE,
  TGR, MTE), by sample, technology, survey, type, estimation type,
  statistic, and restriction.

- `el_mean`: Jackknife summaries for elasticity statistics, by input,
  technology, survey, statistic, and restriction.

- `rk_mean`: (If available) jackknife summaries for risk statistics,
  with `type = "risk"`.

- `ef_dist`: Jackknife summaries of efficiency distributions (counts and
  weights) over ranges, with `stat` indicating whether the row refers to
  counts or weights.

- `rk_dist`: (If available) jackknife summaries of risk distributions
  over ranges, with `type = "risk"`.

- `disagscors`: (If available) jackknife summaries for disaggregated
  efficiency statistics and their gaps by subgroup level and technology.

- `el_samp`: Observation-level elasticity outputs from the first draw in
  `res` (typically the baseline draw).

- `ef_samp`: Observation-level efficiency scores from the first draw in
  `res`.

- `rk_samp`: Observation-level risk measures from the first draw in
  `res`.

## Details

The function assumes each element of `res` is the output of a single
draw from an MSF pipeline (e.g.,
[`msf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/msf_workhorse.md)
followed by
[`draw_msf_estimations()`](https://ftsiboe.github.io/okwaayeli/reference/draw_msf_estimations.md)),
containing components such as: `sf_estm`, `ef_mean`, `el_mean`,
`rk_mean`, `ef_dist`, `rk_dist`, and (optionally) `disagscors`.

For each component, the function:

1.  Stacks all draws using
    [`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html).

2.  Drops non-finite values (`NA`, `Inf`, `-Inf`, `NaN`).

3.  For each statistic (e.g., coefficient estimate, mean efficiency),
    identifies the baseline draw (`draw == 0`) and joins it with
    resampled draws (`draw != 0`) aggregated via
    [`doBy::summaryBy()`](https://rdrr.io/pkg/doBy/man/by-summary.html)
    to compute:

    - `Estimate.mean`: mean across non-baseline draws,

    - `Estimate.sd`: standard deviation across non-baseline draws,

    - `Estimate.length`: number of non-baseline draws.

4.  Computes a jackknife-style test statistic and p-value:

    - `jack_zv = Estimate / Estimate.sd`,

    - `jack_pv = 2 * (1 - pt(|jack_zv|, df = Estimate.length))`,

    where `Estimate` is the baseline draw statistic and the distribution
    of resampled draws is treated as a reference distribution.

For disaggregated scores (`disagscors`), the function also:

- Computes technology-specific disaggregated efficiency gaps (level and
  percent) relative to the reference technology in ` technology_legend`,
  and

- Aggregates these gaps across draws in the same jackknife fashion as
  for the main efficiency statistics.
