# Extract Matching Crop Area Columns from a Dataset

Identifies and returns the column names in a dataset corresponding to
crop area variables for a specified set of crops. The function looks for
column names that start with `"Area_"` and match any of the crops
provided in `selected_crops`.

## Usage

``` r
get_crop_area_list(
  data,
  selected_crops = c("Beans", "Cassava", "Cocoa", "Cocoyam", "Maize", "Millet", "Okra",
    "Palm", "Peanut", "Pepper", "Plantain", "Rice", "Sorghum", "Tomatoe", "Yam")
)
```

## Arguments

- data:

  A `data.frame` or `data.table` containing crop-related variables.
  Column names are expected to include fields prefixed with `"Area_"`,
  such as `"Area_Maize"`.

- selected_crops:

  A character vector specifying the crop names to filter. Defaults to a
  common set of crops including `"Beans"`, `"Cassava"`, `"Cocoa"`,
  `"Cocoyam"`, `"Maize"`, `"Millet"`, `"Okra"`, `"Palm"`, `"Peanut"`,
  `"Pepper"`, `"Plantain"`, `"Rice"`, `"Sorghum"`, `"Tomatoe"`, and
  `"Yam"`.

## Value

A character vector containing the names of columns in `data` that
correspond to the specified crop area variables (e.g., `"Area_Maize"`,
`"Area_Rice"`). Returns an empty vector if no matching columns are
found.

## See also

Other helpers:
[`compute_jenks_binned_shares()`](https://ftsiboe.github.io/okwaayeli/reference/compute_jenks_binned_shares.md),
[`harmonized_data_prep()`](https://ftsiboe.github.io/okwaayeli/reference/harmonized_data_prep.md)
