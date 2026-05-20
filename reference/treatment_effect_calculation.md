# Compute log-linear treatment effects (ATE/ATET/ATEU) for multiple outcomes

For a given matching specification index `i`, this function:

1.  Loads the corresponding matched weights file from
    `matching_output_directory`,

2.  Merges those weights into the analysis `data` via
    `unique_identifier`,

3.  Optionally normalizes each outcome by the first element of
    `outcome_variables` (e.g., to construct per-hectare or per-unit
    measures),

4.  Fits a weighted log-linear model for each outcome with interactions
    between `Treat` and the supplied matching covariate formulas,

5.  Transforms fitted differences into percentage treatment effects,
    trims extreme values, and computes weighted ATE, ATET, and ATEU
    alongside model fit statistics.

## Usage

``` r
treatment_effect_calculation(
  data,
  outcome_variables,
  normalize = NULL,
  i,
  match_specifications,
  matching_output_directory,
  match_formulas
)
```

## Arguments

- data:

  A `data.frame` containing at least:

  - `unique_identifier`: unit identifier used for merging matched
    weights,

  - `Treat`: treatment indicator (logical or 0/1),

  - `Area`: plot size (positive; used for input checks and typical
    scaling),

  - all variables named in `outcome_variables`,

  - any covariates referenced in `match_formulas$general_match` and
    `match_formulas$exact_match`.

- outcome_variables:

  Character vector of outcome variable names (e.g.,
  `c("HrvstKg","Area","SeedKg","HHLaborAE","HirdHr","FertKg","PestLt")`).

- normalize:

  Logical scalar. If `TRUE`, each outcome in `outcome_variables` is
  divided by the first element of `outcome_variables` in the merged data
  (e.g., to obtain outcomes per hectare or per unit).

- i:

  Integer index selecting the row of `match_specifications` and the
  corresponding matching result file (zero-padded via `ARRAY` to
  `"NNNN.rds"`).

- match_specifications:

  A `data.frame` with at least a column `ARRAY`. The `i`-th row is used
  to locate the matching output file and is cbind-ed to the returned
  results.

- matching_output_directory:

  Directory containing per-specification matching results named
  `"match_0001.rds"`, `"match_0002.rds"`, etc. Each RDS is expected to
  contain an element `$md` with columns `unique_identifier` and
  `weights`.

- match_formulas:

  A list with:

  - `general_match`: a formula; the RHS is accessed via
    `as.character(...)[3]`,

  - `exact_match`: a formula; the RHS is accessed via
    `as.character(...)[2]`.

## Value

A `data.frame` (`atet_scalar`) with the columns from
`match_specifications[i, ]` repeated across rows, and the additional
columns:

- `outcome`: outcome variable name,

- `treatment`: name of the treatment indicator (always `"Treat"`),

- `level`: one of `"ATE"`, `"ATET"`, `"ATEU"`, `"aic"`, `"ll"`, `"R2"`,
  `"N"`, `"Ft"`, `"R2a"`,

- `est`: numeric estimate corresponding to `level`.

One block of rows is returned per outcome in `outcome_variables`. If a
model fails for a given outcome, that outcome is skipped.

## Details

For each outcome `oc` in `outcome_variables`, the function:

1.  Filters to observations with non-missing `Treat`, the outcome, and
    `weights`,

2.  Constructs a log-linear model of the form


        log(oc + eps) ~ Treat * (general_match_rhs + exact_match_rhs)

    where `eps = min(oc[oc > 0]) * 1/100` in the estimation sample,

3.  Fits the model using [`lm()`](https://rdrr.io/r/stats/lm.html) with
    weights given by the matched `weights`,

4.  Predicts fitted log outcomes for treated (`Treat = 1`) and untreated
    (`Treat = 0`) counterfactuals,

5.  Computes individual percentage treatment effects \$\$TE_OLS = \\
    \exp(\hat{y}\_1 - \hat{y}\_0) - 1 \\ \times 100,\$\$ trims them to
    the interval \\\[-100, 100\]\\, and then forms:

    - ATE: weighted mean of `TE_OLS` over all units,

    - ATET: weighted mean of `TE_OLS` among treated units,

    - ATEU: weighted mean of `TE_OLS` among untreated units.

In addition, for each outcome the function records model diagnostics:
AIC, log-likelihood, \\R^2\\, adjusted \\R^2\\, F-statistic, and the
sample size used in the regression.

## See also

Other matching:
[`covariate_balance()`](https://ftsiboe.github.io/okwaayeli/reference/covariate_balance.md),
[`draw_matched_samples()`](https://ftsiboe.github.io/okwaayeli/reference/draw_matched_samples.md),
[`match_sample_specifications()`](https://ftsiboe.github.io/okwaayeli/reference/match_sample_specifications.md),
[`treatment_effect_summary()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_summary.md),
[`write_match_formulas()`](https://ftsiboe.github.io/okwaayeli/reference/write_match_formulas.md)
