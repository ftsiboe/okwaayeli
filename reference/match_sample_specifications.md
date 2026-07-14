# Draw / Match Sample Specifications

Generates (i) a draw list by sampling `EaId` within each unique `Survey`
group and (ii) a grid of matching specifications for each bootstrap
draw.

## Usage

``` r
match_sample_specifications(number_of_draws = NULL, data, myseed = NULL)
```

## Arguments

- number_of_draws:

  Integer. The number of draws to perform per `Survey`.

- data:

  A data.frame or data.table containing at least the columns `Survey`
  and `EaId`.

- myseed:

  Integer. Seed for random number generation (default `03242025`).

## Value

A list with two elements:

- `m.specs`:

  A data.frame of matching specifications with columns `boot`, `method`,
  `distance`, `link`, `ARRAY`.

- `drawlist`:

  A data.frame in wide format where each `Survey` is a column and rows
  correspond to draw `ID` (0:`number_of_draws`).

## Details

**Draw list:** For each unique value of `Survey`, the function samples
`number_of_draws` `EaId` values with replacement and prepends a 0 row
(ID = 0) for the baseline. The result is then spread to wide format with
one column per `Survey`.

**Matching specs:** For each draw `ID`, creates a set of matching model
specifications that include:

- Nearest neighbor with distances: `"euclidean"`, `"scaled_euclidean"`,
  `"mahalanobis"`, `"robust_mahalanobis"`.

- Nearest neighbor with distance `"glm"` and links: `"logit"`,
  `"probit"`, `"cloglog"`, `"cauchit"`.

An `ARRAY` index is added for convenience.

## Note

Requires that `data` contain `Survey` and `EaId`.
[`tidyr::spread()`](https://tidyr.tidyverse.org/reference/spread.html)
is used for wide reshaping (consider
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
in new code).

## See also

Other matching:
[`covariate_balance()`](https://ftsiboe.github.io/okwaayeli/reference/covariate_balance.md),
[`draw_matched_samples()`](https://ftsiboe.github.io/okwaayeli/reference/draw_matched_samples.md),
[`treatment_effect_calculation()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_calculation.md),
[`treatment_effect_summary()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_summary.md),
[`write_match_formulas()`](https://ftsiboe.github.io/okwaayeli/reference/write_match_formulas.md)
