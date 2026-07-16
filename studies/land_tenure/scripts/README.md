# `studies/land_tenure/scripts` — naming convention

**A number means a position in a sequence. If a file has no position, it has no
number.**

## Steps — numbered, run in order

| Band | Purpose | When |
|---|---|---|
| `0##` | estimation | the data changes |
| `1##` | exhibits | the estimates change |
| `3##` | article | every render |

| Script | Reads | Writes |
|---|---|---|
| `000_initialize.R` | | |
| `001_DATA_land_tenure_study.R` | harmonized releases | `study_raw_data` |
| `002_MATCHING_land_tenure_study.R` | `study_raw_data` | `estimation_data`, matches |
| `003_TREATMENT_land_tenure_study.R` | `estimation_data` | `te_summary.rds` |
| `004_MSF_land_tenure_study.R` | `estimation_data` | `output/estimations/` (HPC) |
| `100_exhibit_descriptive_stats.R` | `study_raw_data` | `data/descriptive_exhibits.rds` |
| `101_exhibit_figures.R` | `output/estimations/` | `output/figure/`, `output/figure_data/` |
| `301_article_objects.R` | estimations, environment | `narrative/article_objects.json` |
| `302_render_article.R` | | `.docx` / `.html` |
| `run_article.R` | | wraps `article_helpers` → 301 → 302 |

## Libraries — unnumbered, sourced by whatever needs them

| File | Provides | Sourced by |
|---|---|---|
| `article_helpers.R` | paths, `fmt_*`, `assert_present()` | 301, 302, the Rmd |
| `exhibit_helpers_tables.R` | flextable builders, `tbl_num()`, `tbl_pct()` | the Rmd |

These define things; they do not do things. They have no position in a sequence,
so a number on them is a false promise — and it invites exactly the wrong
question. `110_exhibit_tables.R` and `300_article_helpers.R` were both numbered
until 2026-07-15, and both were repeatedly asked "when do I run this?" The answer
was always "you don't."

The tell: **if `run_article.R` or a runner would `source()` it and nothing would
happen, it is a library.**

## Run order

    001 → 002 → 003 → 004        estimation
    100 → 101                    exhibit caches
    run_article.R                301 → 302; the Rmd sources both libraries

`run_article.R` does **not** source `100`/`101`. `100` fits a model per
(treatment × crop × outcome) and `101` re-reads every estimation object; neither
belongs in a render. Both libraries error with the name of the script to run if a
cache is missing.

## Adding a file

1. **Does it *do* something — write a cache, an estimate, a figure?** It is a
   step. Number it by band: `0##` estimation, `1##` exhibits, `3##` article.
2. **Does it only *define* things?** It is a library. Name it
   `<domain>_helpers[_<what>].R`, no number.

Pick the band by **contract, not subject**: a script that fits a model is `1##`
even if it feels like a figure. Gaps are deliberate —
`102_exhibit_robustness.R` slots in without renumbering.

## History

`100_exhibits.do` (Stata) → `100_exhibit_descriptive_stats.R`; the workbook
round-trip is gone. `300_article_helpers.R` → `article_helpers.R`,
`305_tables.R` → `110_exhibit_tables.R` → `exhibit_helpers_tables.R`.
`resource_extraction` is still on the old layout; the two studies will disagree
until it is ported.

## Retired

`old-codes/` holds scripts nothing reads. See its README for why each was retired;
`100_exhibits.do` in particular is worth reading before trusting any Stata-era
workbook.
