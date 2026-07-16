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
| `000_initialize.R` | | the directory tree (`study_dirs()`) |
| `001_DATA_land_tenure_study.R` | harmonized releases | `study_raw_data` |
| `002_MATCHING_land_tenure_study.R` | `study_raw_data` | `estimation_data`, matches |
| `003_TREATMENT_land_tenure_study.R` | `estimation_data` | `te_summary.rds` |
| `004_MSF_land_tenure_study.R` | `estimation_data` | `output/estimations/` (HPC) |
| `100_exhibit_descriptive_stats.R` | `study_raw_data` | `data/descriptive_exhibits.rds` |
| `101_exhibit_figures.R` | `output/estimations/` | `output/figures/`, `output/tables/` |
| `102_exhibit_table_workbook.R` | the table builders | `output/tables/land_tenure_tables.xlsx` |
| `301_article_objects.R` | estimations, environment | `narrative/article_objects.json` |
| `302_render_article.R` | | `.docx` / `.html` |
| `run_article.R` | | the entry point; stage levers |

## Libraries — unnumbered, sourced by whatever needs them

| File | Provides | Sourced by |
|---|---|---|
| `article_helpers.R` | paths, `fmt_*`, `assert_present()` | 301, 302, the Rmd |
| `exhibit_helpers_tables.R` | flextable builders, `tbl_num()`, `tbl_pct()` | the Rmd, 102 |

These define things; they do not do things. A number on them would be a false
promise, and invites the wrong question — "when do I run this?" You don't.

The tell: **if a runner would `source()` it and nothing would happen, it is a
library.**

## Run order

    001 → 002 → 003 → 004        estimation
    100 → 101 → 102              exhibit caches and deliverables
    301 → 302                    the article

`run_article.R` drives all of it behind stage levers. Set a stage `TRUE` to run
it; the guards there explain the couplings that are not obvious (chiefly that
`DATA` without `MATCHING` strips `estimation_data` from the environment).

## Where exhibits come from

Every table and figure is built from the pipeline. The single exception is
`data/tables/tableS0.csv`, which documents how each GLSS round asked the tenure
questions — transcribed from the questionnaires, not computable from any object.
It is the only file in `data/tables/`.

Stata's remaining job is upstream: `data-raw/okwaayeli_DATA.do` harmonizes the
raw GLSS files. Nothing downstream of `001` touches Stata or Excel, except `102`,
which *writes* an .xlsx as a deliverable and reads nothing back.

## Adding a file

1. **Does it *do* something — write a cache, an estimate, a figure?** It is a
   step. Number it by band: `0##` estimation, `1##` exhibits, `3##` article.
2. **Does it only *define* things?** It is a library. Name it
   `<domain>_helpers[_<what>].R`, no number.

Pick the band by **contract, not subject**: a script that fits a model is `1##`
even if it feels like a figure. Gaps are deliberate — `103_exhibit_*.R` slots in
without renumbering.

Sibling studies do not all follow this layout yet; `resource_extraction` is the
closest and is worth reading before porting another.
