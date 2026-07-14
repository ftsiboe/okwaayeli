# Run a single MSF draw and summarize stochastic frontier results

Performs one iteration of multi-stage stochastic frontier (MSF)
estimation for a given `draw`. The function (i) filters the analysis
sample using a draw-specific exclusion list, (ii) calls
[`msf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/msf_workhorse.md)
to estimate production, inefficiency, and risk components by survey, and
(iii) optionally builds disaggregated efficiency score summaries.

## Usage

``` r
draw_msf_estimations(
  draw,
  drawlist,
  surveyy = FALSE,
  data,
  output_variable,
  input_variables,
  inefficiency_covariates = NULL,
  risk_covariates = NULL,
  weight_variable = NULL,
  production_slope_shifters = NULL,
  intercept_shifters = NULL,
  f,
  d,
  identifiers,
  include_trend = FALSE,
  technology_variable = NULL,
  matching_type = NULL,
  adoption_covariates = NULL,
  intercept_shifters_meta = NULL,
  disagscors_list = NULL,
  match_specifications,
  match_specification_optimal,
  match_path
)
```

## Arguments

- draw:

  Integer (or numeric coercible to integer). Draw iteration index.
  Typically `0` is used for the full baseline sample and positive
  integers for resampled or matched draws.

- drawlist:

  A data.frame or similar object containing exclusion information by
  draw. Rows correspond to draws, with column `ID` identifying the draw
  and subsequent columns containing `EaId` values to remove from `data`
  for that draw.

- surveyy:

  Logical; if `TRUE`, uses the survey labels stored in `data$Surveyx`.
  If `FALSE` (default), all observations are assigned to a synthetic
  survey labeled `"GLSS0"` for estimation.

- data:

  A data.frame or data.table containing the estimation sample. Must
  include, at minimum, the columns referenced in other arguments (e.g.,
  `output_variable`, `input_variables`, `identifiers`,
  `weight_variable`, technology and matching variables, and any
  variables used in `disagscors_list`).

- output_variable:

  Character scalar. Name of the dependent (output) variable used in the
  production frontier.

- input_variables:

  Character vector of input (production) variables entering the
  stochastic frontier.

- inefficiency_covariates:

  Optional named list specifying variables in the inefficiency function.
  Typical structure: `list(Svarlist = c(...), Fvarlist = c(...))`.

- risk_covariates:

  Optional named list specifying variables in the production risk
  (noise) function. If `NULL`, a homoskedastic noise term is usually
  implied.

- weight_variable:

  Optional character scalar giving the name of the sampling weight
  variable in `data`. Used when computing weighted summaries (e.g.,
  weighted means) of efficiency scores.

- production_slope_shifters:

  Character scalar naming a variable that shifts the production-function
  slope (e.g., technology shifter). Defaults to `NULL` to indicate no
  slope shifter.

- intercept_shifters:

  Optional named list of intercept shifter variables for the baseline
  (unmatched) sample. Typical structure:
  `list(Svarlist = c(...), Fvarlist = c(...))`.

- f:

  Functional form identifier passed to
  [`msf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/msf_workhorse.md)
  (e.g., Cobb-Douglas, translog). The exact meaning is handled by the
  workhorse function.

- d:

  Distributional form identifier for the inefficiency term (e.g.,
  half-normal, truncated-normal), passed through to
  [`msf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/msf_workhorse.md).

- identifiers:

  Character vector of variable names that uniquely identify units (e.g.,
  household, plot, or observation IDs). These are used to merge
  efficiency scores back to `data` and for disaggregated summaries.

- include_trend:

  Logical; if `TRUE`, includes a technology trend variable (given by
  `technology_variable`) in the frontier. Defaults to `FALSE`.

- technology_variable:

  Optional character scalar naming the technology (trend) variable to be
  used when `include_trend = TRUE`.

- matching_type:

  Optional variable or object controlling nearest-neighbor matching;
  passed to
  [`msf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/msf_workhorse.md).
  The expected type and structure depend on the implementation of that
  workhorse function.

- adoption_covariates:

  Optional named list specifying inefficiency-function variables for the
  matched sample (post-matching specification). Same structure as
  `inefficiency_covariates`.

- intercept_shifters_meta:

  Optional named list of intercept shifters for the matched sample. Same
  structure as `intercept_shifters`.

- disagscors_list:

  Optional character vector of variable names for which disaggregated
  efficiency score summaries should be computed. For each variable in
  this list, the function builds weighted and unweighted summaries of
  TE/TE0/TGR/MTE by survey, sample, and disaggregation level.

- match_specifications:

  match specifications

- match_specification_optimal:

  match specifications

- match_path:

  match path

## Value

A named list with components:

- `sf_estm` - Combined frontier parameter estimates across surveys (rows
  tagged with `Surveyy` and `draw`).

- `el_mean`, `ef_mean`, `rk_mean` - Mean/summary statistics for
  elasticities, efficiency, and risk metrics.

- `ef_dist`, `rk_dist` - Distributions of efficiency and risk
  quantities.

- `el_samp`, `ef_samp`, `rk_samp` - Sample-level results (only retained
  when `draw == 0`).

- `disagscors` - Optional disaggregated efficiency summary table if
  `disagscors_list` is not `NULL`; otherwise `NULL`.

If an error occurs anywhere in the pipeline, the function returns
`NULL`.

## Details

The main outputs include model estimates, mean/percentile summaries, and
(optionally) disaggregated efficiency statistics by user-specified
grouping variables. For draws other than zero, sample-level results are
dropped to reduce storage.

1.  **Draw-specific filtering:** Using `drawlist`, the function removes
    any `EaId` values associated with the current `draw` from `data`.
    This allows for bootstrap, jackknife, or matched-sample resampling.

2.  **Survey-specific estimation:** After setting the `Surveyy` label
    (either from `Surveyx` or collapsed to `"GLSS0"`), the function
    loops over unique survey labels and calls
    [`msf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/msf_workhorse.md)
    for each. The workhorse is expected to return a list with components
    such as `sf_estm`, `el_mean`, `ef_mean`, `rk_mean`, `ef_dist`,
    `rk_dist`, `el_samp`, `ef_samp`, and `rk_samp`.

3.  **Disaggregated efficiency summaries:** If `disagscors_list` is
    provided, the function constructs weighted and unweighted efficiency
    statistics (e.g., weighted mean, mean, median, mode) for
    TE/TE0/TGR/MTE by survey, sample, restriction status, technology,
    and disaggregation level. Results are stored in `res$disagscors`.

4.  **Draw-specific pruning:** When `draw != 0`, sample-level objects
    (`el_samp`, `ef_samp`, `rk_samp`) are removed from the returned list
    to reduce memory usage, keeping only aggregate results.

Internally, the function uses data.table, dplyr, tidyr, doBy, and crayon
for data manipulation and progress messages. All estimation is delegated
to
[`msf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/msf_workhorse.md).
