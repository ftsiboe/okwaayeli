# Initialize Study Environment and Directory Structure

Initialize Study Environment and Directory Structure

## Usage

``` r
study_setup(
  myseed = 1980632,
  project_name,
  local_directories = list(home = file.path("replications", project_name), output =
    file.path("replications", project_name, "output"), matching =
    file.path("replications", project_name, "output", "matching"), treatment_effects =
    file.path("replications", project_name, "output", "treatment_effects"), estimations =
    file.path("replications", project_name, "output", "estimations"), figure_data =
    file.path("replications", project_name, "output", "figure_data"), figure =
    file.path("replications", project_name, "output", "figure"))
)
```

## Arguments

- myseed:

  Numeric/integer scalar seed. Will be coerced to integer. Default:
  1980632.

- project_name:

  Length-1, non-NA character project name (required).

- local_directories:

  Named list of character paths to create. Defaults use `project_name`.

## Value

List with `wd` (directories) and `seed`.
