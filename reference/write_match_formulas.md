# Construct Matching Formulas for Exact and General Matching

Builds two types of matching formulas commonly used in propensity score
or covariate matching workflows:

- An **exact match** formula specifying categorical variables to match
  exactly.

- A **general match** formula specifying numeric and factor covariates
  for distance-based matching.

The function returns both formulas as character strings that can be used
in matching functions such as
[`MatchIt::matchit()`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
or [`Matching::Match()`](https://rdrr.io/pkg/Matching/man/Match.html).

## Usage

``` r
write_match_formulas(
  match_variables_exact,
  match_variables_scaler,
  match_variables_factor
)
```

## Arguments

- match_variables_exact:

  A character vector of variable names to be included in the exact match
  component (e.g., `"gender"`, `"region"`).

- match_variables_scaler:

  A character vector of continuous or scalar variable names to include
  as covariates in the general match formula (e.g., `"age"`,
  `"income"`).

- match_variables_factor:

  A character vector of factor variable names to be included as
  dummy-coded categorical covariates in the general match formula.

## Value

A named list with two elements:

- `exact_match`:

  A string representing the exact match formula (factor variables only).

- `general_match`:

  A string representing the general match formula, including both
  continuous and factor covariates on the right-hand side of `Treat ~`.

## See also

Other matching:
[`covariate_balance()`](https://ftsiboe.github.io/okwaayeli/reference/covariate_balance.md),
[`draw_matched_samples()`](https://ftsiboe.github.io/okwaayeli/reference/draw_matched_samples.md),
[`match_sample_specifications()`](https://ftsiboe.github.io/okwaayeli/reference/match_sample_specifications.md),
[`treatment_effect_calculation()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_calculation.md),
[`treatment_effect_summary()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_summary.md)
