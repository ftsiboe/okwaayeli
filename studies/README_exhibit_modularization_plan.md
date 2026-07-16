# Plan: modularize the `100_*` exhibit engine into R under `okwaayeli`

Date: 2026-07-15

## 1. What is actually there

Seven near-identical Stata files, each carrying 30–31 of the same modelling
constructs. They are one template, copy-pasted seven times and then drifted.

| Study | File | Repo path in the file | Treatment loop | `disagCat` |
|---|---|---|---|---|
| land_tenure | `scripts/100_exhibits.do` | `$GitHub\ghana\okwaayeli` ✓ | **none** | **none — hardcodes `i.OwnLnd`** |
| resource_extraction | `scripts/100_exhibits.do` | `$GitHub\ghana\okwaayeli` ✓ | `extraction_any mining_any mining_comm mining_gala quarrying sand salt` | ✓ |
| ag_services | `100_FIGTAB_ag_services.do` | `$GitHub\ghana\okwaayeli` (fixed 2026-07-15) | `services0 farm_association community_cooperative extension0` | ✓ |
| financial_inclusion | `100_FIGTAB_financial_inclusion_study.do` | `$GitHub\ghana\okwaayeli` (fixed 2026-07-15) | (via `` `disab' ``) | ✓ |
| disability | `100_FIGTAB_disability_study.do` | `$GitHub\labs\GHAgricProductivityLab` ✗ | `disabled disabled_self disabled_spouse disabled_child disabled_close disabled_member` | ✓ |
| time_poverty | `100_FIGTAB_time_poverty_study.do` | `$GitHub\labs\GHAgricProductivityLab` ✗ | (via `` `disab' ``) | ✓ |
| education | `100_FIGTAB_education_study.do` | `$GitHub\my_packages\GHAgricProductivityLab` ✗ | (via `` `edu' ``) | ✓ |

### Two independent drifts

**Path drift — four generations of the same repository:**

1. `$GitHub\my_packages\GHAgricProductivityLab` (education)
2. `$GitHub\labs\GHAgricProductivityLab` (disability, time_poverty)
3. `$GitHub\labs\okwaayeli` (ag_services, financial_inclusion — repointed 2026-07-15)
4. `$GitHub\ghana\okwaayeli` (land_tenure, resource_extraction) ← current

Every rename left the older studies reading and writing somewhere else. None of
them fail loudly; Stata's `saveold` to a stale-but-existing path succeeds.

**Abstraction drift — six of seven converged, one did not.**
Six studies generalise the treatment to a single column:

```stata
qui foreach disag in extraction_any mining_any ... {
    gen disagCat = `disag'
    reg `Var' c.Trend##i.disagCat, vce(cluster Clust)
```

`land_tenure` alone hardcodes `i.OwnLnd` at every site and has **no treatment
loop at all**. It is the only study that cannot vary its treatment without
editing code — which is exactly the study that now needs `OwnLndAny/Maj/Full`
sensitivity arms. The abstraction the port needs already exists; land just never
received it.

The loop macro is named `disag`, `disab`, or `edu` depending on the file, but all
seven assign into `disagCat`. That is the seam to cut on.

### Flagged, not fixed

`studies/time_poverty/100_FIGTAB_time_poverty_study.do:3` reads
`harmonized_disability_data`. A time-poverty study reading the disability
extract is either a copy-paste error or a deliberate derivation. Not
investigated.

## 2. What the engine computes

Stripped of the loops, every file does the same thing per
(treatment variant × crop × outcome):

| Stata | Purpose |
|---|---|
| `reg Var c.Trend##i.disagCat, vce(cluster Clust)` | level + trend by treatment group |
| `testparm i.disagCat` | group mean-difference test → `CATDif` |
| `testparm c.Trend#i.disagCat` | group trend-difference test → `TrendDif` |
| `margins disagCat, eydx(Trend) grand coefl post` + `nlcom (...*100)` | % annual trend per group and pooled |
| `tabstat Var, stat(mean sem min max sd n) by(disagCat)` | group means |
| `tabstat Var if Surveyx=="`sx'"`, ... by(disagCat)` | per-wave means |
| `logit Var i.Survey` + `margins Survey` + `nlcom` | binary-outcome variant (Table 2 path) |

Outcomes are a fixed list: `Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt
AgeYr YerEdu HHSizeAE Depend CrpMix`. Clustering is
`egen Clust = group(Survey Ecozon EaId HhId)`.

Output is two **tidy value sheets** written by `export excel ... firstrow(variables)`:

- `means` — `CropIDx, Equ, Coef, Beta, SE, Tv, Pv, Min, Max, SD, N`
- `<study>` — `Variable, crop, mesure, Beta, SE, Tv, Pv, Min, Max, SD, N`

These are values, not formulas. **They are machine-readable and always have
been.** The `#N/A` problem noted in `305_tables.R` belongs to other, hand-built
display sheets in the same workbook — not to these.

## 3. Why this matters beyond tidiness

`305_tables.R` builds Tables 1, 2 and S1–S7 from **frozen CSVs extracted from the
v001 Word draft**, not from the pipeline. Consequences observed today:

- `N_ALL <- 35185` (hardcoded, from the draft's Table 1 header) against **28,411**
  farmers in the current `estimation_data` — a 24% discrepancy that survived a
  full re-run, because re-running cannot touch a photograph.
- Every descriptive number in §2 and all of §3 is a 2025 snapshot.
- Table 1's "Annual trend from 1991 to 2017" column is fitted across the
  1984-frame break that §2 now disavows.

The parameterised article architecture is decorative for those sections. The
modularization is what makes it real.

## 4. Existing R assets — build on, do not rebuild

The package is already mature and conventional:

- `R/` — 9 files, 16 exports, roxygen throughout, 19 man pages
- Established naming: `*_workhorse`, `draw_*`, `*_specifications`, `@family`
- `R/stochastic_frontier-core.R` — `sf_model_specifications()`, `sf_workhorse()`,
  `msf_workhorse()`
- `R/matching-core.R`, `R/helpers.R`, `R/study_setup.R`, `R/get_household_data.R`
- Imports already include `data.table`, `sfaR`, `MatchIt`, `stats`
- **Figures are already in R**: `data-raw/scripts/figures_and_tables.R` provides
  `tab_main_specification()`, `fig_heterogeneity00()`, `fig_robustness()`,
  `fig_input_te()`, `fig_covariate_balance()`, `fig_dsistribution()` — but it is
  `source()`d from `100_exhibits.R` rather than packaged.

So the gap is narrow: **the descriptive/trend engine is the only part still in
Stata.** Figures already crossed over; tables did not.

### Stata → R mapping

| Stata | R |
|---|---|
| `reg y c.Trend##i.g, vce(cluster C)` | `fixest::feols(y ~ Trend * g, cluster = ~C)` |
| `testparm i.g` | `car::linearHypothesis()` / `fixest::wald()` |
| `margins g, eydx(Trend)` | `marginaleffects::avg_slopes(m, variables="Trend", by="g", slope="eydx")` |
| `logit y Trend, vce(cluster C)` | `fixest::feglm(y ~ Trend, family="logit", cluster=~C)` |
| `tabstat y, stat(mean sem min max sd n) by(g)` | `data.table` aggregation |

`marginaleffects` supports `slope = "eydx"` directly, which is the one construct
with a real risk of silent divergence. It is worth pinning that package version.

## 5. Proposed design

New file `R/exhibits-core.R`, mirroring the frontier core's conventions:

```r
exhibit_specifications(data, treatment_variables, outcome_variables,
                       crop_list, wave_variable = "Surveyx")
# -> data.table grid: one row per (treatment × crop × outcome). Mirrors
#    sf_model_specifications(). Makes the land study's missing treatment loop
#    a data structure rather than a copy-paste.

exhibit_group_summary(data, outcome, treatment, wave, weights = NULL)
# -> mean/sem/min/max/sd/n by group, overall and by wave. Replaces tabstat.

exhibit_trend_model(data, outcome, treatment, trend = "Trend",
                    cluster = c("Survey","Ecozon","EaId","HhId"),
                    family = c("gaussian","binomial"))
# -> level, %-trend per group + pooled, CATDif, TrendDif. Replaces the
#    reg/testparm/margins/nlcom block. family="binomial" covers the Table 2 path.

exhibit_workhorse(spec_row, data, ...)
# -> tidy data.table for ONE specification. Mirrors sf_workhorse().

draw_exhibit_summary(specifications, data, ...)
# -> binds all rows. Mirrors draw_msf_summary().
```

**Output schema** (the contract — supersedes both xlsx sheets):

```
study, treatment, crop, outcome, wave, group, statistic,
estimate, se, t, p, min, max, sd, n
```

`statistic ∈ {mean, trend_pct, cat_diff, trend_diff}`; `group ∈ {0, 1, pooled}`;
`wave ∈ {GLSS3..GLSS7, all}`. Long format, one row per number, so table builders
filter rather than index by matrix position.

## 6. Phases

**Phase 0 — Golden reference (do first, non-negotiable).**
Freeze the current `means` and `<study>` sheets from each study's existing
workbook as CSV under `tests/testthat/golden/`. A port with no reference to
reproduce is a rewrite, and a rewrite of an econometric pipeline is how numbers
change silently. Land's workbook was regenerated 2026-07-15 and is the natural
first reference.

**Phase 1 — Path triage (hours, independent of everything else).**
Repoint education, disability, time_poverty to `ghana\okwaayeli`. Add the
`capture confirm file ... / exit 601` guard already added to
`land_tenure/100_exhibits.do` and `data-raw/okwaayeli_DATA.do`. Resolve the
time_poverty/disability data question.

**Phase 2 — Contract.** Pin the schema above; write it as a roxygen `@return`
block before writing code.

**Phase 3 — Core.** Implement `R/exhibits-core.R`. Roxygen, `@family exhibits`,
`@keywords internal` where appropriate. Export via NAMESPACE.

**Phase 4 — Parity test.** `tests/testthat/test-exhibits-parity.R`: run the R
core on the land data, compare to the Phase 0 golden CSV within tolerance
(suggest `1e-6` for means, `1e-4` for margins-derived quantities). **Do not
proceed to Phase 5 until land passes.**

**Phase 5 — Migrate, one study at a time.** land_tenure → resource_extraction →
disability → ag_services → financial_inclusion → education → time_poverty.
Delete each `.do` only after its parity test is green. Land first because it is
active and because it is the study that gains the most (it acquires the
treatment loop it never had).

**Phase 6 — Rewire `305_tables.R`.** Point `ft_table1()`/`ft_table2()`/`ft_tableS1..S7()`
at `draw_exhibit_summary()` output instead of the frozen CSVs. Derive
`N_ALL/N_OWN/N_NON` from the data. This is what actually fixes the 35,185 problem
and makes §2–3 live.

**Phase 7 — Promote figures.** Move `data-raw/scripts/figures_and_tables.R` into
`R/exhibits-figures.R` so `100_exhibits.R` stops `source()`ing a script from
`data-raw/`.

## 7. Risks

- **Numerical parity.** `vce(cluster)` small-sample corrections differ between
  Stata and `fixest` defaults (`ssc()`), and `margins ... eydx` averaging can
  diverge. Phase 4 exists for this. Expect to spend real time on `ssc()`.
- **Silent failure inheritance.** Every `.do` wraps its work in `cap{ }`, which
  swallows errors. Any specification that has been failing silently for years
  will surface as a hard error in R. That is a feature, but it will look like the
  port broke something. Log rather than swallow.
- **Land's output shape changes.** Giving land a treatment loop adds rows
  (one block per treatment variant). Table 1 gains structure it did not have.
  Decide whether Table 1 stays `OwnLnd`-only for the current submission.
- **`marginaleffects` version drift.** Pin it.

## 8. Interim fix — available now, survives the port

Phases 3–5 are weeks. Phase 6 does not depend on them, because the Stata tidy
sheets **already have the schema the R core will produce**. So:

Point `.ft_csv()` at `readxl::read_excel(RESULTS_XLSX, sheet = "means")` and
`sheet = "land_tenure"` instead of `data/tables/table1.csv` and `table2.csv`,
reshaping to the same `label/c1..c6` layout the builders expect.

That makes Tables 1 and 2 live from `land_tenure_results.xlsx` today, kills the
35,185 discrepancy, and throws nothing away: when the R core lands, only the data
source behind the same reshaping changes.

**Recommended sequence: Phase 0 → interim fix → Phase 1 → Phases 2–7 when the
land manuscript is off your desk.**
