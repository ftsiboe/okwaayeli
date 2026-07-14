# Summarize Treatment Effect Estimates Across Matching Specifications

Aggregates and summarizes treatment effect results stored as `.rds`
files in a specified output directory. The function reads each RDS file,
combines all results into a single data frame, filters out invalid
estimates, and computes jackknife-style summary statistics (mean,
standard error, sample size, t-value, and p-value) by matching
specification and outcome.

## Usage

``` r
treatment_effect_summary(treatment_effects_output_directory)
```

## Arguments

- treatment_effects_output_directory:

  Character string giving the path to the directory containing `.rds`
  files with treatment effect results (e.g., outputs of
  [`treatment_effect_calculation()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_calculation.md)
  saved across specifications).

## Value

A `data.frame` containing summarized treatment effect estimates with
columns:

- `method, distance, link, outcome, level`

- `est` (original estimate for non-bootstrap sample)

- `jack_mean, jack_se, jack_n, jack_tvl, jack_pvl`

## Details

The function proceeds through the following steps:

1.  Lists and reads all `.rds` files in
    `treatment_effects_output_directory`.

2.  Combines them into a single long-format data frame using
    [`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html).

3.  Removes rows with invalid estimates (`NA`, `NaN`, `Inf`, or `-Inf`).

4.  Separates base (non-bootstrap) results where `boot == 0` and joins
    these with jackknife summary statistics computed via
    [`doBy::summaryBy()`](https://rdrr.io/pkg/doBy/man/by-summary.html)
    across method, distance, link, outcome, and level.

5.  Renames summary columns to:

    - `jack_mean`: mean estimate

    - `jack_se`: standard error

    - `jack_n`: sample size (number of replicates)

    - `jack_tvl`: t-value (`jack_mean / jack_se`)

    - `jack_pvl`: two-sided p-value

## See also

Other matching:
[`covariate_balance()`](https://ftsiboe.github.io/okwaayeli/reference/covariate_balance.md),
[`draw_matched_samples()`](https://ftsiboe.github.io/okwaayeli/reference/draw_matched_samples.md),
[`match_sample_specifications()`](https://ftsiboe.github.io/okwaayeli/reference/match_sample_specifications.md),
[`treatment_effect_calculation()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_calculation.md),
[`write_match_formulas()`](https://ftsiboe.github.io/okwaayeli/reference/write_match_formulas.md)
