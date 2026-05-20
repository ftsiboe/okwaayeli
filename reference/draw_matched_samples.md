# Draw matched samples (stratified bootstrap + matching)

Performs stratified resampling at the `Surveyx, EaId` level (excluding
EaIds listed for the current bootstrap `ID`) and computes adjusted
sampling weights used for matching. Then fits a matching model per
`match_specifications[i, ]` using MatchIt, with exact matching on `Emch`
and distance/link from `match_specifications`.

## Usage

``` r
draw_matched_samples(
  i,
  data,
  match_variables_exact,
  match_variables_scaler,
  match_variables_factor,
  match_specifications,
  sample_draw_list,
  verbose = FALSE
)
```

## Arguments

- i:

  Integer index selecting the row of `match_specifications` to use.

- data:

  A data.frame/data.table containing at least:
  `Surveyx, EaId, HhId, Mid, unique_identifier, Weight, Treat`, plus
  columns named in `Emch, Scle, Fixd`.

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

- match_specifications:

  Data frame of matching specifications with columns `boot`, `method`,
  `distance`, and optionally `link`.

- sample_draw_list:

  Data frame where column `ID` identifies the bootstrap draw, and each
  remaining column corresponds to a `Survey` containing the sampled
  `EaId`.

- verbose:

  Logical; if `TRUE`, prints the chosen matching method and timing.
  Default `FALSE`.

## Value

A list with:

- `match_specifications`:

  The selected row from `match_specifications`.

- `m.out`:

  The `matchit` object.

- `md`:

  Matched data:
  `Surveyx, EaId, HhId, Mid, unique_identifier, weights, pWeight`.

- `df`:

  The analysis data with adjusted weights:
  `Surveyx, EaId, HhId, Mid, unique_identifier, pWeight`.

## Details

Adjusted weights are computed as \\pWeight = Weight \times
(alloc/allocj)\\, where `alloc` is the pre-exclusion sum of `Weight` by
`Surveyx, EaId` and `allocj` is the post-exclusion sum.

Exact matching is performed on variables in `Emch`. The distance model
formula is constructed as `Treat ~ Scle + Fixd`. When
`distance == "glm"`, `m.order = "largest"` is used; otherwise
`"closest"`.

## See also

Other matching:
[`covariate_balance()`](https://ftsiboe.github.io/okwaayeli/reference/covariate_balance.md),
[`match_sample_specifications()`](https://ftsiboe.github.io/okwaayeli/reference/match_sample_specifications.md),
[`treatment_effect_calculation()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_calculation.md),
[`treatment_effect_summary()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_summary.md),
[`write_match_formulas()`](https://ftsiboe.github.io/okwaayeli/reference/write_match_formulas.md)
