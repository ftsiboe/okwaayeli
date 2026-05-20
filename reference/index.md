# Package index

## Study setup

Helpers to scaffold and configure a new study.

- [`study_setup()`](https://ftsiboe.github.io/okwaayeli/reference/study_setup.md)
  : Initialize Study Environment and Directory Structure
- [`okwaayeli_control()`](https://ftsiboe.github.io/okwaayeli/reference/okwaayeli_control.md)
  : Control Settings (Internal)
- [`run_only_for()`](https://ftsiboe.github.io/okwaayeli/reference/run_only_for.md)
  : Restrict Execution of an R Script to Allowed SLURM Job Conditions

## Data access

Download and prepare the harmonized GLSS datasets.

- [`get_household_data()`](https://ftsiboe.github.io/okwaayeli/reference/get_household_data.md)
  : Download and Load Harmonized Household Data from the
  GHAgricProductivityLab Repository
- [`harmonized_data_prep()`](https://ftsiboe.github.io/okwaayeli/reference/harmonized_data_prep.md)
  : Prepare Data for Agricultural Productivity Analysis
- [`get_crop_area_list()`](https://ftsiboe.github.io/okwaayeli/reference/get_crop_area_list.md)
  : Extract Matching Crop Area Columns from a Dataset

## Matching

Build, draw and balance matched samples.

- [`write_match_formulas()`](https://ftsiboe.github.io/okwaayeli/reference/write_match_formulas.md)
  : Construct Matching Formulas for Exact and General Matching
- [`match_sample_specifications()`](https://ftsiboe.github.io/okwaayeli/reference/match_sample_specifications.md)
  : Draw / Match Sample Specifications
- [`draw_matched_samples()`](https://ftsiboe.github.io/okwaayeli/reference/draw_matched_samples.md)
  : Draw matched samples (stratified bootstrap + matching)
- [`covariate_balance()`](https://ftsiboe.github.io/okwaayeli/reference/covariate_balance.md)
  : Compute Covariate Balance Summaries Across Matching Specs

## Stochastic frontier

The MSF estimation engine.

- [`msf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/msf_workhorse.md)
  : Meta stochastic frontier (MSF) workhorse
- [`sf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md)
  : Stochastic frontier workhorse
- [`draw_msf_estimations()`](https://ftsiboe.github.io/okwaayeli/reference/draw_msf_estimations.md)
  : Run a single MSF draw and summarize stochastic frontier results
- [`draw_msf_summary()`](https://ftsiboe.github.io/okwaayeli/reference/draw_msf_summary.md)
  : Summarize multi-draw Meta Stochastic Frontier (MSF) results

## Treatment effects

- [`treatment_effect_calculation()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_calculation.md)
  : Compute log-linear treatment effects (ATE/ATET/ATEU) for multiple
  outcomes
- [`treatment_effect_summary()`](https://ftsiboe.github.io/okwaayeli/reference/treatment_effect_summary.md)
  : Summarize Treatment Effect Estimates Across Matching Specifications

## Utilities

- [`compute_jenks_binned_shares()`](https://ftsiboe.github.io/okwaayeli/reference/compute_jenks_binned_shares.md)
  : Compute Jenks-Binned Shares by Aggregation Groups
- [`mode()`](https://ftsiboe.github.io/okwaayeli/reference/mode.md) :
  Mode of a Vector (Internal)
