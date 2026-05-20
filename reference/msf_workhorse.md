# Meta stochastic frontier (MSF) workhorse

Performs Meta Stochastic Frontier (MSF) analysis in several stages:

1.  **Naive SF:** Fits a pooled stochastic frontier and computes
    baseline technical efficiency (TE) measures.

2.  **Group SF:** If a technology variable `technology_variable` is
    supplied, splits the sample into technology groups and fits
    group-specific SF models.

3.  **Meta SF (unmatched and matched):** Constructs a meta-frontier
    using fitted values from group models and, optionally,
    nearest-neighbor matching specifications. From this, it derives
    technology gap ratios (TGR) and meta-technical efficiency (MTE).

4.  **Summaries and distributions:** Produces weighted and unweighted
    summaries and distributional statistics for efficiency, elasticity,
    and risk across technologies, samples (unmatched/matched), and
    surveys.

## Usage

``` r
msf_workhorse(
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
  match_path,
  match_specifications,
  match_specification_optimal
)
```

## Arguments

- data:

  A data.frame or data.table containing the estimation sample. Must
  include the dependent variable `output_variable`, the inputs in
  `input_variables`, the weight variable `weight_variable`, unique ID
  variables in `identifiers`, any inefficiency and risk covariates in
  `inefficiency_covariates` and `risk_covariates`, the technology
  variable `technology_variable` (if used), and any variables required
  by matching objects on disk (e.g., `"Surveyx"`, `"EaId"`, `"HhId"`,
  `"Mid"`, `"unique_identifier"`).

- output_variable:

  Character scalar. Name of the dependent (output) variable used in the
  production frontier.

- input_variables:

  Character vector of input variable names (e.g., land, labor, capital)
  passed through to
  [`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md).

- inefficiency_covariates:

  Optional named list specifying variables in the inefficiency function
  for the naive and group frontiers. Typical structure:
  `list(scalar_variables = c(...), factor_variables = c(...))`.

- risk_covariates:

  Optional named list specifying variables in the production risk
  (noise) function for the naive and group frontiers. Same structure as
  `inefficiency_covariates`. If `NULL`, a homoskedastic noise term is
  usually implied.

- weight_variable:

  Character scalar. Name of the sampling/observation weight variable in
  `data`. Observations with zero weight are dropped.

- production_slope_shifters:

  Character scalar giving the name of a slope-shifter variable in `data`
  (e.g., technology shifter, policy dummy). Defaults to `NULL` for no
  slope shifter.

- intercept_shifters:

  Optional named list of intercept shifter variables for the naive and
  group frontiers. Typical structure:
  `list(scalar_variables = c(...), factor_variables = c(...))`, passed
  to
  [`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md).

- f:

  Functional form index for the production frontier (e.g., Cobb-Douglas,
  translog, quadratic). The index is passed to
  [`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md)
  and ultimately to the internal form-selection utilities (e.g.,
  `sf_functional_forms`).

- d:

  Distributional form index for the inefficiency term (e.g.,
  half-normal, truncated-normal). Passed to
  [`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md).

- identifiers:

  Character vector of variable names that uniquely identify observations
  (e.g.,
  `c("unique_identifier","Survey","CropID","HhId","EaId","Mid")`). These
  IDs are used when merging scores and summarizing by technology or
  sample.

- include_trend:

  Logical; if `TRUE`, the last element in `input_variables` is treated
  as a trend/technology variable in the production function. Passed
  through to
  [`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md).
  Defaults to `FALSE`.

- technology_variable:

  Optional character scalar naming the technology variable (e.g.,
  region, system, period) used to define groups for group SF and
  meta-frontier estimation. If `NULL`, no technology groups are formed
  and only the naive frontier and TE are computed.

- matching_type:

  Optional character scalar controlling which nearest-neighbor matching
  specifications to use for meta-frontier estimation. When not `NULL`,
  the function reads matching specifications and matched samples from
  `"results/matching/"` and related RDS files. Setting
  `matching_type = "optimal"` restricts attention to the subset of
  matching specifications labeled as optimal.

- adoption_covariates:

  Optional named list specifying inefficiency-function covariates for
  the meta-frontier (matched/unmatched) estimation. If `NULL`, defaults
  to `inefficiency_covariates`.

- intercept_shifters_meta:

  Optional named list of intercept shifters for the meta-frontier
  estimation. If `NULL`, defaults to `intercept_shifters`.

- match_path:

  match path

- match_specifications:

  match specifications

- match_specification_optimal:

  match specifications

## Value

A named list with the following components:

- `sf_estm`: Data.frame of coefficient-level summaries from all naive,
  group, and meta-frontiers (including LR tests), augmented with
  technology identifiers (`Tech`) and `sample` labels (e.g.,
  `"unmatched"`, matching-spec names).

- `ef_mean`: Data.frame of aggregated efficiency statistics (TE0, TE,
  TGR, MTE), including weighted/unweighted means, medians, and modes, by
  survey, sample, technology, type, estimation type, and restriction
  status. When `technology_variable` is supplied, also includes
  efficiency gap levels and percentages.

- `ef_dist`: Data.frame of efficiency distributions (histogram counts
  and weights) over efficiency ranges, by survey, sample, technology,
  type, estimation type, and restriction.

- `rk_dist`: Data.frame of risk distributions (histogram counts and
  weights) analogous to `ef_dist`, with `type = "risk"`.

- `el_mean`: Data.frame of aggregated elasticity statistics
  (weighted/unweighted means, medians, modes) by input, survey, sample,
  technology, and restriction, including elasticity-gap measures when
  `technology_variable` is provided.

- `rk_mean`: Data.frame of aggregated risk statistics (weighted and
  unweighted) by survey, sample, technology, and restriction, with
  risk-gap measures when `technology_variable` is provided.

- `el_samp`: Data.frame of observation-level elasticities for all models
  and samples (excluding the synthetic `"GLSS0"` survey rows).

- `ef_samp`: Data.frame of observation-level efficiency scores (TE0, TE,
  TGR, MTE) including IDs, weights, technologies, and sample labels.

- `rk_samp`: Data.frame of observation-level risk measures for all
  models and samples (excluding the synthetic `"GLSS0"` survey rows).

## Details

The function is designed as a high-level orchestrator around
[`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md),
assembling naive, group-level, and meta-frontier results into a unified
output structure suitable for MSF analysis.

The workflow can be summarized as:

- **Naive SFA (TE0):** Calls
  [`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md)
  on the full sample to obtain baseline efficiencies (`teBC`, `teJLMS`,
  `teMO`). If no technology variable is specified
  (`technology_variable = NULL`), these naive efficiencies are the final
  scores and only TE-related summaries are produced.

- **Group SFA (TE):** When `technology_variable` is provided, the sample
  is recoded into numeric technology groups (`Tech`), and
  [`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md)
  is applied separately to each group. Group-level efficiencies are
  combined into TE measures by group and survey.

- **Meta-frontier (TGR and MTE):** Using fitted group-frontier values
  (`Yhat`) from the restricted models, the function fits meta-frontiers
  (unmatched and, optionally, matched) again via
  [`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md).
  Technology gap ratios (TGR) are derived from these meta-frontiers, and
  meta-technical efficiency (MTE) is computed as `TE * TGR`.

- **Matching layer (optional):** If `matching_type` is given, the
  function loads matching specifications (`mspecs`, `mspecs_optimal`)
  and corresponding matched samples from disk (e.g.,
  `"results/matching/"`), fits additional meta-frontiers by matching
  specification, and augments the set of scores and summaries with these
  matched samples.

- **Summaries and distributions:**

  - Scores::

    TE0, TE, TGR, and MTE are reshaped into long form and aggregated to
    compute weighted/unweighted means, medians, modes, and distribution
    histograms (both count-based and weight-based) across surveys,
    samples, technologies, and estimation types. When
    `technology_variable` is not `NULL`, the function also computes
    technology gaps (level and percent) relative to the minimum
    technology in ` technology_legend`.

  - Elasticities::

    Elasticities from the underlying SF models (naive, group, meta) are
    combined and summarized in a similar way, including technology-gap
    metrics for elasticities when `technology_variable` is provided.

  - Risk::

    Risk measures derived from `sf_workhorse` are combined, summarized,
    and used to build distributional statistics analogous to those for
    efficiency.

- **LR tests:** When `technology_variable` is not `NULL`, the function
  builds likelihood ratio test statistics comparing a naive pooled
  frontier to the combination of group and meta-frontiers, storing these
  as rows with `CoefName = "LRT"` in the `sf_estm` output.

Throughout, the function uses dplyr, tidyr, data.table, doBy, and
related helper functions for reshaping and summarizing the output of
[`sf_workhorse`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md).

## See also

Other frontier analysis:
[`sf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/sf_workhorse.md)
