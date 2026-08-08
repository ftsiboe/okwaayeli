# `studies/resource_extraction/scripts` — naming convention

**A number means a position in a sequence. If a file has no position, it has no
number.**

This mirrors `studies/land_tenure/scripts/README.md`. Where the two studies
differ, it is noted — and the difference is work still outstanding, not a
deliberate divergence. See `../AGENT_PROMPT.md` for what remains.

## Steps — numbered, run in order

| Band | Purpose | When |
|---|---|---|
| `0##` | estimation | the data changes |
| `1##` | exhibits | the estimates change |
| `3##` | article | every render |

| Script | Reads | Writes |
|---|---|---|
| `000_initialize.R` | | the directory tree (`study_dirs()`) |
| `001_DATA_resource_extraction_study.R` | harmonized releases | `study_raw_data` |
| `002_MATCHING_resource_extraction_study.R` | `study_raw_data` | `estimation_data`, matches |
| `003_TREATMENT_resource_extraction_study.R` | `estimation_data` | `te_summary.rds` |
| `004_MSF_resource_extraction_study.R` | `estimation_data` | `output/estimations/` (HPC) |
| `100_exhibit_descriptive_stats.R` | `study_raw_data` | `data/descriptive_exhibits.rds` |
| `101_exhibit_figures.R` | `output/estimations/` | `output/figures/`, `output/tables/` |
| `102_exhibit_table_workbook.R` | the table builders | `output/tables/resource_extraction_tables.xlsx` |
| `301_article_objects.R` | estimations, environment | `narrative/article_objects.json` |
| `302_render_article.R` | | `.docx` / `.html` |
| `run_article.R` | | the entry point; stage levers |

## Libraries — unnumbered, sourced by whatever needs them

| File | Provides | Sourced by |
|---|---|---|
| `article_helpers.R` | paths, `fmt_*`, `assert_present()` | 301, 302, the Rmd |
| `exhibit_helpers_tables.R` | flextable builders, `.read_est()`, `.STUDY_ROOT` | the Rmd, 102 |

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

## Directory layout

This study is on `layout = "v2"`: plots and their data share `output/figures/`,
table data goes to `output/tables/`. `001` passes it to `study_setup()`; `000`,
`101` and `102` pass it to `study_dirs()` so they resolve correctly even before
`001` next re-runs and bakes it into the environment.

`002`, `003` and `004` deliberately do **not** call `study_dirs()`: they only
touch `matching/`, `treatment_effects/` and `estimations/`, which are named the
same under both layouts.

Reach for `study_dir_figures()` / `study_dir_figure_data()` /
`study_dir_tables()`. Never paste a directory literal next to `wd$output` — `wd`
in the `.rds` is a snapshot frozen by whichever run last called `study_setup()`,
and a literal silently outlives a layout change.

The narrative embeds figures by relative path (`../output/figures/*.png` in
`98_tables_and_figures.Rmd` and `99_appendix.Rmd`). Those are not resolved
through the accessors, so a future layout change has to update them too.

## Where exhibits come from

The target — and `land_tenure`'s standing rule — is that **every number the
paper prints comes from the pipeline, and prose and exhibits come from the same
build.** The one legitimate exception is an exhibit transcribing something no
object can compute (questionnaire wording, response options, per-wave mappings);
`land_tenure`'s Table S0 is the model.

**This study is not there yet.** `data/tables/*.csv` (`table1`, `table2`,
`tableA1`–`tableA9`, plus some `_header.csv`) are **frozen values extracted from
a workbook**, and `exhibit_helpers_tables.R` still reads them.
`100_exhibit_descriptive_stats.R` is the engine-backed replacement for tables 1,
2, A2 and A3 — written, but its output is not yet wired into the builders. The
replacement builders are drafted in `exhibit_helpers_tables.DRAFT.R`; integrate
them only after `parity_check_descriptive.R` comes back clean and the diff has
been read cell by cell. Tables A1 and A4–A9 stay CSV-driven until they are made
live off the estimation objects — separate, later work.

`100_exhibits.do` (Stata) is the legacy exhibit path. It is superseded once the
descriptive engine lands, and should be deleted then, not before. Stata's
remaining job is upstream: `data-raw/scripts/data-prep/glss/07_resource_extraction.do` harmonizes the raw GLSS
files.

Nothing downstream of `001` reads Excel. `102` *writes* an `.xlsx` as a
deliverable and reads nothing back.

`output/resource_extraction_results.xlsx` is a deliberate exception: it is **the
parity reference for the whole repo** (`tests/testthat/golden/_freeze.R`
regenerates the descriptive engine's goldens from it via
`read_exhibit_sheet()`). It stopped being a pipeline dependency when `101`
replaced its `loadWorkbook` → `writeData` → `saveWorkbook` round trip into the
`msf` sheet with a plain CSV; it remains evidence. Do not delete it, and do not
delete `R/exhibits-workbook.R`.

## Remaining gap vs `land_tenure`

Descriptive tables. `land_tenure` builds Tables 1/2/A2/A3 live off
`data/descriptive_exhibits.rds`; here they still come from the frozen CSVs.
Everything else — layout, stage levers, the `102` workbook, Excel's absence from
the middle of the pipeline, the naming convention — is now the same.

## Adding a file

1. **Does it *do* something — write a cache, an estimate, a figure?** It is a
   step. Number it by band: `0##` estimation, `1##` exhibits, `3##` article.
2. **Does it only *define* things?** It is a library. Name it
   `<domain>_helpers[_<what>].R`, no number.

Pick the band by **contract, not subject**: a script that fits a model is `1##`
even if it feels like a figure. Gaps are deliberate — `103_exhibit_*.R` slots in
without renumbering.

## Scratch files

`parity_check_descriptive.R` and `exhibit_helpers_tables.DRAFT.R` are
verification and staging scratch, not part of the pipeline. Both are deleted
once the descriptive tables go live.
