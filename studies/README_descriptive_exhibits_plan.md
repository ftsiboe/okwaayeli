# Plan: descriptive exhibits without Stata or Excel

Date: 2026-07-15 · Status: **engine delivered and validated; consolidation approved, not yet done**

---

# PART II — Consolidation (approved 2026-07-15, do NOT start before land renders)

The engine is built and validated (Part I below). This part removes the
duplication that building it in two passes created.

## 1. Naming: by functionality, never by table number

`.tbl2_live()` is a bad name: land's Table 2 is resource_extraction's Table 3, and
the appendix numbering differs again. A function is named for the **shape it
produces**, not the exhibit it happens to feed.

    exhibit_<what-it-is>_table()

## 2. One schema

The duplication has a single cause — two vocabularies for the same thing:

    workbook   CropIDx, Equ, Coef      ("Mean_disagCat0", "Trend_Pooled", "CATDif")
    engine     crop, outcome, group, statistic, wave

Rather than parameterise every builder over both, **`read_exhibit_sheet()`
translates into the engine schema on read**. The Stata workbook becomes just
another source; one vocabulary survives. This also collapses `parse_coef()`,
currently duplicated in both parity test files, into the adapter.

## 3. Target layout

| File | Exports |
|---|---|
| `R/exhibits-cells.R` | `exhibit_value()` · `exhibit_stars()` · `exhibit_cell()` |
| `R/exhibits-tables.R` | `exhibit_group_summary_table()` · `exhibit_wave_share_table()` · `exhibit_crop_share_table()` · `exhibit_group_sizes()` |
| `R/descriptive-exhibits-core.R` | the engine — unchanged |
| `R/exhibits-figures.R` | figures — unchanged |
| `R/exhibits-workbook.R` | `read_exhibit_sheet()` only — legacy adapter, deleted with the last Stata study |

| Shape | Rows | Columns | Feeds |
|---|---|---|---|
| `exhibit_group_summary_table()` | outcomes | group × {mean, trend} + diff tests | land T1; RE T1/A2/A3 |
| `exhibit_wave_share_table()` | indicators | waves + change | land T2 |
| `exhibit_crop_share_table()` | crops | one indicator family | land S1–S4; RE appendix |

Study scripts keep **only** row maps, labels and `ft_*` wrappers. Deleted from
`110_exhibit_tables.R`: `.pick`, `.tbl1_live`, `.tbl2_live`, `.tblS_live`,
`.tbl1_n`, `.tbl1_hdr`, `.tbl2_hdr`.

## 4. Output layout

Everything a study emits goes under `output/`, one directory per artefact kind.
A figure and the data behind it live together — they are one exhibit.

    output/figures/     *.png AND the *.csv / *.rds behind them
    output/tables/      *.rds AND *.csv          (see below)
    output/estimations/ frontier objects        (unchanged)
    output/matching/    match objects           (unchanged)
    output/treatment_effects/                   (unchanged)

Retired: `output/figure/` (singular) and `output/figure_data/` merge into
`output/figures/`. `data/descriptive_exhibits.rds` moves to `output/tables/`.

**`data/tables/*.csv` stays put** — those are hand-curated *inputs* (Table 3,
Table 4, S5–S7), not emissions. Inputs live in `data/`, outputs in `output/`.
That distinction is the point of the split.

### Every table emits `.rds` and `.csv`

Matching the figure builders, which already write `.rds` **and** `.csv` for the
data behind each plot. Two kinds of CSV, and they answer different questions:

    output/tables/descriptive_exhibits.rds     the cache 110_exhibit_tables.R reads
    output/tables/descriptive_exhibits.csv     the same long frame, inspectable
    output/tables/rendered/table1.csv          the built table, as it appears in the paper
    output/tables/rendered/table2.csv
    output/tables/rendered/tableS1.csv ...

- **The long frame** (`descriptive_exhibits.csv`) is the audit trail: one row per
  number, keyed, greppable, diffable without R. This is what you open when you
  want to know why a cell is what it is.
- **The rendered tables** (`rendered/*.csv`) are the manuscript's cells —
  `label, c1..cN`, formatted, daggers and all. This is what you diff between runs
  to see what actually moved in the paper.

The second is worth having for its own sake. `data/tables/table1.csv` existed
because somebody needed the table as data and the only way to get it was to
extract it from a Word draft by hand. Emitting it closes that loop: the same
artefact, generated rather than transcribed, and never able to disagree with the
table beside it.

`110_exhibit_tables.R` gains a small writer for this — it already builds the
`label/header/c1..cN` frames, so it is a `fwrite()` per builder, not new logic.
Guard it so a knit does not write: emit only when sourced from
`100_exhibit_descriptive_stats.R` or an explicit call, never mid-render.

### Route through `wd$`, don't just rename

`study_setup()` already defines `wd$figure` and `wd$figure_data` — and **every
builder ignores them**, hardcoding `file.path(wd$output, "figure_data", ...)`
instead. Twelve literals across `R/exhibits-figures.R` where there should be one
lookup. So:

```r
# study_setup()
figures = file.path("studies", project_name, "output", "figures"),
tables  = file.path("studies", project_name, "output", "tables"),
```

and every write becomes `file.path(study_environment$wd$figures, "robustness.png")`.
Renaming the directories without this leaves the same trap for the next rename.

Also update: the Rmd image paths (`../output/figure/*.png` → `../output/figures/`),
`100_exhibit_descriptive_stats.R`'s `OUT_RDS`, and `110_exhibit_tables.R`'s
`.DESC` / `.FIGDAT`.

## 5. Order

1. **`read_exhibit_sheet()` emits engine schema.** Land this ALONE and confirm
   both parity suites stay green before touching anything else — it changes what
   ~15,000 assertions compare against, so a bug here converts them into false
   confidence.
2. `exhibit_value/stars/cell` → `R/exhibits-cells.R`.
3. The three shapes, engine schema → `R/exhibits-tables.R`.
4. `wd$figures` / `wd$tables` in `study_setup()`; route all writes through them.
5. Point land's `110` at the packaged shapes; delete the locals.
6. Port RE's `305`.
7. Delete `R/exhibits-workbook.R`.

## 6. Risks

- **Step 1 is the dangerous one** — see above. Alone, verified, then stop.
- Step 4 moves files that already exist. Old `output/figure*/` contents are not
  migrated automatically; re-run `101_exhibit_figures.R` rather than moving PNGs
  by hand, or the figure data and the figures drift apart.
- Step 5 churns `110` a third time in one session. Do it once, deliberately.

## 7. Not today

Land's §2–4 has not rendered yet. This is the next work, not the current work.

---

# PART I — the engine (delivered 2026-07-15)

## 1. Objective

Replace the `100_*.do` → `.xlsx` → `305_tables.R` chain with an R layer that
computes Tables 1 and 2 and their appendix analogues directly from
`study_environment$study_raw_data`.

Stata and Excel drop out of the descriptive path entirely. `okwaayeli_DATA.do`
stays — it harmonises raw GLSS and hands R a `.dta`, which is a clean boundary.
The MSF pipeline is already R (`sfaR`).

## 2. Input contract

Single input: **`study_environment$study_raw_data`** — `harmonized_<study>_data`
inner-joined to `harmonized_crop_farmer_data` on `(Surveyx, EaId, HhId, Mid)`,
restricted to the study's waves, built by each study's `001_DATA_*.R`.

This is not a new dependency. `100_exhibits.do` opens by performing *the same
merge* and writing `<study>_study_data.dta`:

```stata
use  harmonized_land_tenure_data
merg 1:m Surveyx EaId HhId Mid using harmonized_crop_farmer_data
keep if _merge==3
drop _merge EduWhyNo
keep if inlist(Surveyx,"GLSS3",...,"GLSS7")
```

which is line-for-line `001_DATA_land_tenure_study.R`. The `.dta` the exhibit
engine reads is a duplicate of an object R already holds. The port removes the
duplication.

Required columns: the outcome list, the treatment column(s), `Surveyx`,
`Season`, `Ecozon`, `EaId`, `HhId`, `CropID`, `WeightHH`.

## 3. What the engine actually computes

Two shapes, not one.

### Engine A — outcome × treatment group

Per (treatment × crop × outcome), from `reg y c.Trend##i.disagCat, vce(cluster Clust)`:

| Quantity | Stata | Emitted as |
|---|---|---|
| group means | `tabstat y, stat(mean sem min max sd n) by(disagCat)` | `Mean_<g>` |
| per-wave means | same, `if Surveyx=="<w>"` | `<wave>_<g>` |
| group trends (%) | `margins disagCat, eydx(Trend)` + `nlcom (…*100)` | `Trend_<g>` |
| level difference | `testparm i.disagCat` | `CATDif` |
| trend difference | `testparm c.Trend#i.disagCat` | `TrendDif` |

Outcomes: `Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt AgeYr YerEdu
HHSizeAE Depend CrpMix` (+ `Female Credit Extension EqipIrig EqipMech` in the
land sheet). Cluster: `group(Survey Ecozon EaId HhId)`. `Trend = Season - min(Season)`.

### Engine B — category × wave

Per (categorical variable × crop), after `tab X, gen(X_)`:

| Quantity | Stata | Emitted as |
|---|---|---|
| share by wave | `tabstat X_k, by(Surveyx)` | `mesure = GLSS6 / GLSS7` |
| share pooled | `tabstat X_k` | `mesure = GLSS0` |
| change (%) | `logit X_k i.Survey` + `margins Survey` + `nlcom (b6-b7)*100` | `mesure = Trend` |

Land: `LndOwn_1..3`, `LndRgt_1..4`, `LndAq_1..5`, `ShrCrpCat_1..3`, `OwnLnd`.
Resource extraction: the extraction indicators.

**Engine B is Engine A with a dummy outcome and no treatment grouping.** One
schema carries both.

## 4. The flavors

| Table | Engine | Rows | Columns |
|---|---|---|---|
| **1** | A | outcomes | group × {mean, trend} + diff tests |
| **2** | B | categories | waves + trend |
| **S1–S4** (land) | B | **crops** | one variable family, `GLSS0` + trend |
| **A2/A3** (extraction) | A | outcomes | **each of 7 treatment variants** |

So the appendix analogues are the same two engines re-sliced: S1–S4 vary the
crop axis, A2/A3 vary the treatment axis. Nothing new is computed — only the
projection changes. This is why the builders take a *row map* and a *column
spec* rather than being written per table.

## 5. Output schema (the contract)

Long, one row per number, superseding both xlsx sheets:

```
study      character
treatment  character   # e.g. "OwnLnd", "extraction_any"; NA for Engine B
crop       character   # "Pooled" or a crop
outcome    character   # Yield, Area, ... or a category dummy (LndOwn_1, ...)
wave       character   # GLSS3..GLSS7, or "all"
group      character   # "0", "1", "pooled"; NA for Engine B
statistic  character   # mean | trend_pct | cat_diff | trend_diff
estimate, se, t, p, min, max, sd, n   numeric
```

Table builders filter this frame; they never index by matrix position. That is
the defect the Stata path has — `mat rownames` / `roweq` are positional, which is
how the `Female` collision went unnoticed for years.

## 6. Function inventory — `R/descriptive-exhibits-core.R`

Mirrors the frontier core's conventions (`*_specifications`, `*_workhorse`,
`draw_*`, roxygen, `@family descriptive exhibits`).

```r
descriptive_specifications(data, outcomes, treatments, crops, wave_var)
  # -> data.table grid: one row per (treatment × crop × outcome). Gives the land
  #    study the treatment loop it never had.

descriptive_group_summary(data, outcome, treatment, wave_var, weights = NULL)
  # -> mean/sem/min/max/sd/n by group, overall and per wave. Replaces tabstat.

descriptive_trend_model(data, outcome, treatment, trend_var, cluster_vars,
                        family = c("gaussian", "binomial"), weights = NULL)
  # -> Trend_<g>, CATDif, TrendDif. Replaces reg/testparm/margins/nlcom.
  #    family="binomial" serves Engine B.

descriptive_category_shares(data, category_var, wave_var, waves, cluster_vars)
  # -> Engine B: dummy expansion, shares by wave, pooled share, % change.

descriptive_workhorse(spec_row, data, ...)      # one specification -> tidy rows
draw_descriptive_summary(specifications, data, ...)  # bind all -> tidy frame
```

Assembly/formatting already exists in `R/exhibits-workbook.R` and is reused
unchanged: `exhibit_value()`, `exhibit_stars()`, `exhibit_cell()`,
`exhibit_crop_table()`, `exhibit_wave_table()`, `exhibit_group_sizes()`. Those
take a data source and a map; swapping the workbook for `draw_descriptive_summary()`
output is a one-line change per builder.

### Stata → R

| Stata | R |
|---|---|
| `reg y c.Trend##i.g, vce(cluster C)` | `fixest::feols(y ~ Trend * g, cluster = ~C)` |
| `logit y Trend, vce(cluster C)` | `fixest::feglm(y ~ Trend, family = "logit", cluster = ~C)` |
| `testparm i.g` / `testparm c.Trend#i.g` | `fixest::wald()` |
| `margins g, eydx(Trend)` | `marginaleffects::avg_slopes(slope = "eydx", by = g)` |
| `tabstat y, stat(...) by(g)` | `data.table` aggregation |

New Imports: `fixest`, `marginaleffects`. Both pinned.

## 7. Validation — resource_extraction as the golden

**Why RE.** It is the only study whose `means` sheet was never corrupted by the
`roweq` bug (fixed repo-wide 2026-07-15; RE alone already had `roweq A = \`Var'`).
It exercises the treatment loop hardest — seven variants (`extraction_any`,
`mining_any`, `mining_comm`, `mining_gala`, `quarrying`, `sand`, `salt`) — spans
GLSS3–7, and is not currently being run, so a parity harness cannot disturb
in-flight work.

**Harness.** `tests/testthat/test-descriptive-exhibits-parity.R`:

1. Freeze RE's current `means` + `resource_extraction` sheets to
   `tests/testthat/golden/resource_extraction_*.csv`.
2. Run `draw_descriptive_summary()` on RE's `study_raw_data`.
3. Join on `(crop, outcome, group, statistic, wave)` and compare.

Tolerances: `1e-6` for means/sd/n (pure aggregation, should be near-exact);
`1e-4` for regression coefficients; **`1e-3` for anything from `margins ... eydx`**,
which is the construct most likely to drift.

**Land is not the reference.** Its workbook predates today's `roweq` fix, so its
`Equ=="Female"` rows are colliding. Re-run land's `100_exhibits.do` before using
it for anything comparative.

## 8. Divergences to settle before coding

These are decisions, not bugs. Each changes the numbers.

1. **Weights — DECIDED 2026-07-15: optional, `NULL` by default.**
   No `100_*.do` in the repo uses any weight: no `[aw=]`, no `[pw=]`, no
   `svyset`. Yet `WeightHH` is present in the data (`harmonized_data_prep()`
   copies it to `Weight`), and resource_extraction §2.1 states that "all
   descriptive statistics … are computed using the GLSS household sampling
   weights". **The descriptive tables are unweighted and that manuscript sentence
   says otherwise.**

   Resolution: every engine takes `weights = NULL`, which reproduces the Stata
   path exactly and keeps the parity test meaningful. Passing
   `weights = "WeightHH"` produces population-representative estimates. The
   choice is therefore explicit at the call site and recorded in the emitted
   schema (`attr(x, "weights")`), instead of being an unstated property of the
   code.

   This does **not** settle the manuscript claim. Either resource_extraction §2.1
   is corrected to say the descriptives are unweighted, or the tables are rebuilt
   with `weights = "WeightHH"` and every descriptive number in that paper moves.
   That is an editorial decision, tracked separately from the port.
2. **Cluster small-sample correction.** Stata's `vce(cluster)` and `fixest`
   defaults differ; `fixest::ssc()` must be tuned to match. Expect real time here.
3. **`cap{ }` silent failures.** Every `.do` swallows errors — `LndAq_6` is absent
   from land's sheet because "Other" acquisition exists only in GLSS3/4 while that
   block keeps GLSS6/7, so its logit failed quietly. R must log and continue, not
   swallow. Specs that have been failing for years will surface.
4. **Dagger rule.** `305_tables.R` currently infers CATDif → mean columns,
   TrendDif → trend columns at p<0.05. Consistent with the frozen table
   (`Female`: CATDif p=0.983, TrendDif p=0.480, no daggers) but never stated
   anywhere. Fix the convention in the schema.

## 9. Phases

| # | Phase | Output |
|---|---|---|
| 0 | Freeze RE golden CSVs | `tests/testthat/golden/` |
| 1 | Schema + roxygen skeleton (no logic) | `R/descriptive-exhibits-core.R` |
| 2 | Engine A + Engine B | working `draw_descriptive_summary()` |
| 3 | Parity harness vs RE golden | green test |
| 4 | Tune `ssc()` / `eydx` to tolerance | green test |
| 5 | Repoint RE's table builders at the R output | RE tables Stata-free |
| 6 | Repoint land's (after its `.do` re-run + golden) | land tables Stata-free |
| 7 | Retire `100_*.do` + `.xlsx` per validated study | — |

Phases 5–7 are per-study and independent; a study keeps its Stata path until its
parity test is green.

## 10. Risks

- **Untested-code debt.** Everything written today is written-not-run (no R in
  the authoring environment). This port changes *numbers*, not plumbing. Phase 3
  is the control, and it should exist before Phase 2 is finished, not after.
- **Parity may be the wrong target.** If a divergence turns out to be a Stata bug
  (as `roweq` was), matching it is wrong. Any tolerance failure needs
  adjudication, not tightening.
- **Scope creep into Engine A's appendix flavors.** RE's A2/A3 layouts are not
  yet read; confirm at Phase 5 rather than assuming they mirror land's Table 1.
