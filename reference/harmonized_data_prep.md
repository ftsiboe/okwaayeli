# Prepare Data for Agricultural Productivity Analysis

Cleans and transforms a dataset by creating new variables, applying log
transformations, converting selected variables to factors or characters,
and recoding education levels. The function is designed to standardize
inputs for further analysis of agricultural productivity.

## Usage

``` r
harmonized_data_prep(data)
```

## Arguments

- data:

  A `data.frame` or `data.table` containing household- and farm-level
  variables such as weights, demographic information, and agricultural
  inputs.

## Value

A cleaned and transformed `data.frame` or `data.table` with additional
variables ready for analysis.

## See also

Other helpers:
[`compute_jenks_binned_shares()`](https://ftsiboe.github.io/okwaayeli/reference/compute_jenks_binned_shares.md),
[`get_crop_area_list()`](https://ftsiboe.github.io/okwaayeli/reference/get_crop_area_list.md)
