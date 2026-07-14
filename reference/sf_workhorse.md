# Stochastic frontier workhorse

Fits a stochastic frontier model for a given production specification
and, when monotonicity is sufficiently violated, re-estimates a
shape-constrained (restricted) frontier using minimum-distance methods.
The function wraps the underlying `sfaR` estimation routines and a set
helper utilities (e.g., `sf_functional_forms()`, `equation_editor()`,
`fit_organizer()`, `translogEla()`, curvature/monotonicity checks) to
produce:

1.  Unrestricted stochastic frontier estimates and diagnostics.

2.  Shape-constrained estimates that enforce monotonicity (and related
    regularity conditions) when needed.

3.  Observation-level efficiencies, fitted values, and risk measures.

4.  Observation-level elasticities, including a "returns-to-scale" term.

## Usage

``` r
sf_workhorse(
  data,
  output_variable,
  input_variables,
  weight_variable = NULL,
  d,
  f,
  production_slope_shifters = NULL,
  intercept_shifters = NULL,
  inefficiency_covariates = NULL,
  risk_covariates = NULL,
  identifiers,
  include_trend = FALSE
)
```

## Arguments

- data:

  A data.frame or data.table containing all variables required for
  estimation, including the dependent variable `output_variable`, inputs
  `input_variables`, the weight variable `weight_variable` (if used),
  the unique ID variables listed in `identifiers`, and any variables
  referenced in `production_slope_shifters`, `intercept_shifters`,
  `inefficiency_covariates`, or `risk_covariates`.

- output_variable:

  Character scalar. Name of the dependent/output variable in `data` used
  for the production frontier.

- input_variables:

  Character vector of input variable names (e.g., land, labor, capital).
  The order of variables in `input_variables` determines how they map
  into the generic input labels `I1`, `I2`, ... used in the
  functional-form utilities.

- weight_variable:

  Optional character scalar. Name of the sampling/observation weight
  variable. If `NULL`, all observations are given unit weight.

- d:

  Distributional form index for the inefficiency term. This is passed to
  `sf_functional_forms()` and ultimately to `sfacross()` (e.g., to
  select half-normal, truncated-normal, etc.). The exact mapping is
  determined by `sf_functional_forms()`.

- f:

  Functional form index for the production frontier (e.g., Cobb-Douglas,
  translog, quadratic), used to select from the list returned by
  `sf_functional_forms()`. The name of the chosen form (e.g., `"CD"`,
  `"TL"`, `"QD"`, `"LN"`) influences logging of variables and regularity
  checks.

- production_slope_shifters:

  Character scalar giving the name of a slope-shifter variable in `data`
  (e.g., technology shift, policy dummy). Defaults to `NULL` for no
  slope shifter and is passed to `equation_editor()`.

- intercept_shifters:

  Optional named list of intercept shifter variables for the production
  function. Typical structure:
  `list(scalar_variables = c(...), factor_variables = c(...))`, where
  the specific interpretation is handled inside `equation_editor()`.

- inefficiency_covariates:

  Optional named list describing the inefficiency-function covariates.
  Typical structure:
  `list(scalar_variables = c(...), factor_variables = c(...))`, passed
  as `uhet` and `muhet` to `sfacross()` when non-`NULL`.

- risk_covariates:

  Optional named list describing the production-risk (noise) covariates.
  When non-`NULL`, used as `vhet` in `sfacross()` to allow
  heteroskedasticity of the noise term.

- identifiers:

  Character vector of variable names that uniquely identify observations
  (e.g.,
  `c("unique_identifier","Survey","CropID","HhId","EaId","Mid")`). These
  IDs are carried through to efficiency, elasticity, and risk outputs.

- include_trend:

  Logical; if `TRUE`, the last element in `input_variables` is treated
  as a trend/technology variable and handled accordingly in the
  functional-form and elasticity calculations. Defaults to `FALSE`.

## Value

A named list with four data.frames:

- `sf`: Coefficient-level results combining unrestricted and, when
  applicable, restricted SFA estimates, plus monotonicity/curvature
  diagnostics. Columns include (at minimum) `CoefName`, `Estimate`,
  `StdError`, `Zvalue`, `Pvalue`, and `restrict`.

- `ef`: Observation-level efficiency results (unrestricted and
  restricted), including the ID variables in `identifiers`, weights,
  efficiency measures (e.g., `u`), model-fitted values (`mlFitted`), and
  `restrict`.

- `el`: Observation-level elasticities and returns-to-scale-type
  measures. Includes IDs, weights, elasticity columns (e.g., `el1`,
  `el2`, ..., and the summed elasticity), and `restrict`.

- `rk`: Observation-level risk measures (unrestricted and restricted),
  including IDs, weights, and `risk`, plus `restrict`.

## Details

The function proceeds in several steps:

1.  **Setup and functional form selection:** Using
    `sf_functional_forms()`, the function determines the appropriate
    production functional form and distributional specification based on
    the number of inputs (`number_of_inputs`), the `f` and `d` indices,
    and the `include_trend` flag. This includes whether the model is
    specified in logs (e.g., CD/TL/GP/TP) or levels.

2.  **Variable construction and equation building:** Inputs in
    `input_variables` are mapped to generic labels (`I1`, `I2`, ...) and
    their log transforms are created when needed. The outcome variable
    is set to `Y` or `lnY`. The function then calls `equation_editor()`
    to construct the production, inefficiency, and risk equations used
    by `sfacross()`.

3.  **Unrestricted stochastic frontier estimation:** The function
    iterates over a grid of optimization methods (`nr`, `nm`, `bfgs`,
    `bhhh`, ...) and tolerance settings, calling `sfacross()` until a
    successful convergence is reported. It extracts observation-level
    efficiencies via
    [`sfaR::efficiencies()`](https://rdrr.io/pkg/sfaR/man/efficiencies.html),
    constructs fitted values, and builds an observation-level risk
    metric based on squared deviations from mean output.

4.  **Elasticities and regularity checks:** Using `fit_organizer()` and
    `translogEla()`, the function computes elasticities and
    returns-to-scale-type measures, and evaluates monotonicity and
    curvature via `translogCheckMono()` and `translogCheckCurvature()`
    (or a coefficient-sign check for CD/LN forms).

5.  **Shape-constrained frontier (if needed):** If the monotonicity
    measure `mono` falls below 0.80, the function constructs a set of
    linear restrictions using `translogMonoRestr()` and solves a
    quadratic programming problem (via
    [`quadprog::solve.QP`](https://rdrr.io/pkg/quadprog/man/solve.QP.html)
    and fall-back matrix inversions using Matrix, MASS, corpcor) to
    obtain constrained coefficients. For TL/QD forms a constrained
    frontier (`lcFitted`) is computed with `translogCalc()`; for CD/LN
    forms, constrained coefficients are obtained via a SEM
    representation using lavaan.

6.  **Re-estimation under constraints:** Given the constrained frontier,
    the function rebuilds the production equation with
    `equation_editor()` and re-estimates a stochastic frontier (`sfc`)
    using the same optimization grid. Constrained efficiencies, risks,
    and elasticities are computed and summarized, and regularity checks
    are repeated.

Unrestricted and restricted summaries are then stacked, with a
`restrict` flag (`"Unrestricted"` vs. `"Restricted"`), for downstream
comparison.

## See also

Other frontier analysis:
[`msf_workhorse()`](https://ftsiboe.github.io/okwaayeli/reference/msf_workhorse.md)
