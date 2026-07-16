# `studies/land_tenure/scripts` — naming convention

    <band><seq>_<phase>_<what>.R

## Bands

| Band | Purpose | When it runs |
|---|---|---|
| `0##` | Estimation pipeline | when the data changes |
| `1##` | Exhibits | when the estimates change |
| `3##` | Article assembly | every render |

## The `1##` exhibit band

The second digit is the **phase**, and it exists so that the run order can be
read off the number:

| Digit | Phase | Contract |
|---|---|---|
| `10x` | **compute** | reads data or estimates, writes a cache. Minutes. Run explicitly. |
| `11x` | **render** | reads only caches, produces tables/figures. Milliseconds. Safe to source at knit time. |

Everything in `10x` runs before anything in `11x`. That is the whole point of the
split: the previous layout (`101` compute, `102` tables, `103` figures) implied
`102` ran before `103`, when in fact `102` reads `output/figure_data/` which
`103` writes. The numbers contradicted the dependency.

## Current

| Script | Phase | Reads | Writes |
|---|---|---|---|
| `000_initialize.R` | | | |
| `001_DATA_land_tenure_study.R` | | harmonized releases | `study_raw_data` |
| `002_MATCHING_land_tenure_study.R` | | `study_raw_data` | `estimation_data`, matches |
| `003_TREATMENT_land_tenure_study.R` | | `estimation_data` | `te_summary.rds` |
| `004_MSF_land_tenure_study.R` | | `estimation_data` | `output/estimations/` (HPC) |
| `100_exhibit_descriptive_stats.R` | compute | `study_raw_data` | `data/descriptive_exhibits.rds` |
| `101_exhibit_figures.R` | compute | `output/estimations/` | `output/figure/`, `output/figure_data/` |
| `110_exhibit_tables.R` | render | both caches | flextables + `tbl_num()`; sourced by the Rmd |
| `300_article_helpers.R` | | | paths, formatters, `assert_present()` |
| `301_article_objects.R` | | estimations, environment | `narrative/article_objects.json` |
| `302_render_article.R` | | | `.docx` / `.html` |
| `run_article.R` | | | wraps 300 → 301 → 302 |

## Run order

    001 → 002 → 003 → 004        (estimation)
    100 → 101                    (exhibit caches)
    run_article.R                (300 → 301 → 302; the Rmd sources 110)

`run_article.R` does **not** source the `1##` band. `100` fits a model per
(treatment × crop × outcome) and `101` re-reads every estimation object; neither
belongs in a render. `110` is the exception — it only reads caches, so the Rmd
sources it directly, and it errors with the name of the script to run if a cache
is missing.

## Adding a script

Pick the band by **contract, not by subject matter**. A script that fits a model
is `10x` even if it feels like a figure; a script that only formats is `11x` even
if it feels like analysis. If it writes a cache, it is `10x`. If it can be
sourced mid-render without cost, it is `11x`.

Gaps in the sequence are deliberate — `102_exhibit_robustness.R` slots in without
renumbering anything.

## Retired

`old-codes/` holds scripts nothing reads. See its README for why each was retired;
`100_exhibits.do` in particular is worth reading before trusting any Stata-era
workbook.
