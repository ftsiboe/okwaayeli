# Compute Jenks-Binned Shares by Aggregation Groups

This function bins a numeric variable using Jenks (Fisher) natural
breaks and computes the relative share of *counts* and/or *weights*
within each aggregation group across the binned ranges.

It is useful for summarizing the distribution of a variable (e.g.,
dealer density, yield, acreage, distances) across spatial or categorical
groups.

## Usage

``` r
compute_jenks_binned_shares(
  dt,
  output_variable,
  aggregation_points,
  jenks = NULL,
  jenks_number = NULL,
  weight_variable = NULL
)
```

## Arguments

- dt:

  A `data.table` containing the data.

- output_variable:

  Character name of the numeric column to bin.

- aggregation_points:

  Character vector of grouping variables (e.g., `"region"`,
  `"district"`, `"grid_id"`).

- jenks:

  Optional numeric vector of breakpoints. If `NULL`, Jenks breaks are
  computed automatically.

- jenks_number:

  Integer number of Jenks classes to compute when `jenks` is `NULL`.

- weight_variable:

  Optional character name of a weighting column. If `NULL`, a weight of
  `1` is assumed for each row.

## Value

A `data.table` with:

- grouping variables

- `binned_range_name` - character label of the Jenks bin

- `binned_range_level` - numeric factor level of the bin

- `estimate_count` - share of counts within the group

- `estimate_weight` - share of weights within the group

## Details

Internally the function:

1.  Computes Jenks natural breaks (if not provided)

2.  Creates `count` and `weight` totals within each bin by group

3.  Merges with total counts/weights per group

4.  Computes shares

## See also

Other helpers:
[`get_crop_area_list()`](https://ftsiboe.github.io/okwaayeli/reference/get_crop_area_list.md),
[`harmonized_data_prep()`](https://ftsiboe.github.io/okwaayeli/reference/harmonized_data_prep.md)
