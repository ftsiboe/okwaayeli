# Compute Covariate Balance Summaries Across Matching Specs

Compute Covariate Balance Summaries Across Matching Specs

## Usage

``` r
covariate_balance(match_specifications, matching_output_directory)
```

## Arguments

- match_specifications:

  data.frame/data.table with at least columns: `boot`, `ARRAY`, and the
  spec fields stored in RDS (e.g., method, distance, link).

- matching_output_directory:

  Directory containing RDS files named as "0001.rds", "0002.rds", etc.

## Value

A list with:

- rate:

  data.frame of mean balance metrics by spec (Adj sample only) with a
  composite `rate`.

- bal_tab:

  long-format balance table per covariate/stat/sample/spec.

## Details

Reads each matching result RDS (expected to contain `m.out` and
`match_specifications`), extracts balance via
[`cobalt::bal.tab`](https://ngreifer.github.io/cobalt/reference/bal.tab.html),
reshapes, computes a composite balance \\rate = mean( (Diff-0)^2,
(KS-0)^2, (V_Ratio-1)^2 )\\, and averages by
`ARRAY, method, distance, link, sample`.

## See also

Other matching:
[`draw_matched_samples()`](https://ftsiboe.github.io/okwaayeli/reference/draw_matched_samples.md),
[`match_sample_specifications()`](https://ftsiboe.github.io/okwaayeli/reference/match_sample_specifications.md),
[`treatment_effect_calculation()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_calculation.md),
[`treatment_effect_summary()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_summary.md),
[`write_match_formulas()`](https://ftsiboe.github.io/okwaayeli/reference/write_match_formulas.md)
