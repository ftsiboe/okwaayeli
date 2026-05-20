# Download and Load Harmonized Household Data from the GHAgricProductivityLab Repository

Retrieves a harmonized household- or farm-level dataset from the
**GHAgricProductivityLab** GitHub repository using **piggyback**, stores
it in a package-specific cache directory, and returns the dataset as a
`data.frame`. The file is downloaded only once and reused from the local
cache on future calls.

## Usage

``` r
get_household_data(
  dataset = "harmonized_crop_farmer_data",
  github_token = NULL,
  force = TRUE
)
```

## Arguments

- dataset:

  Character string. Base name of the dataset to retrieve (without file
  extension). Must correspond to a `.dta` file in the `hh_data` GitHub
  release (e.g., `"harmonized_crop_farmer_data"`).

- github_token:

  Optional GitHub personal access token (PAT). If `NULL`, the function
  checks the environment variable `GHProdLab_TOKEN`. If that is also
  missing, the piggyback download will use default authentication
  behavior.

- force:

  force re download

## Value

A `data.frame` containing the requested harmonized dataset.

## Details

The function downloads a Stata `.dta` file associated with the chosen
dataset from the GitHub release labeled `hh_data`. It uses the
package-specific cache directory determined by:


    tools::R_user_dir("GHAgricProductivityLab", which = "cache")

If the file already exists locally, it is not re-downloaded.

**GitHub Authentication**

- If `github_token` is supplied, it is used.

- Otherwise, the function looks for environment variable
  `GHProdLab_TOKEN`.

- If neither is available, the function falls back to default GitHub
  credentials (e.g., from `gh` CLI or cached credentials).
