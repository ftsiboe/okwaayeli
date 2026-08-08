# `studies/land_tenure/scripts` — naming convention

**A number means a position in a sequence. If a file has no position, it has no
number.**

This is the file that `studies/resource_extraction/scripts/README.md` says it
mirrors, and that `../README.md`, `../land_tenure.Rmd`, `run_article.R` and
`studies/financial_inclusion/AGENT_PROMPT.md` all point at. It was missing until
2026-08-08; the text below is reconstructed from `resource_extraction`'s copy and
from what the scripts here actually do.

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
| `102_exhibit_table_workbook.R` | the table builders | `output/tables/*.xlsx` |
| `301_article_objects.R` | estimations, environment | `narrative/article_objects.json` |
| `302_render_article.R` | | `narrative/land-tenure.docx` / `.html` |
| `run_article.R` | | the entry point; stage levers |

## Libraries — unnumbered, sourced by whatever needs them

| File | Provides | Sourced by |
|---|---|---|
| `article_helpers.R` | paths, `fmt_*`, `assert_present()` | 301, 302, the Rmd |
| `exhibit_helpers_tables.R` | flextable builders, `tbl_num()`, `.live_table()`, `.STUDY_ROOT` | the Rmd, 102 |

These define things; they do not do things. A number on them would be a false
promise, and invites the wrong question — "when do I run this?" You don't.

The tell: **if a runner would `source()` it and nothing would happen, it is a
library.**

`exhibit_helpers_tables.R` is deliberately **not** sourced by `run_article.R`.
The Rmd sources it during the render, at which point knitr's working directory
is `narrative/` — which is why it resolves its own paths via `.STUDY_ROOT`
rather than trusting `article_helpers.R`'s repo-root-relative constants.

## Run order

    001 → 002 → 003 → 004        estimation
    100 → 101 → 102              exhibit caches and deliverables
    301 → 302                    the article

`run_article.R` drives all of it behind stage levers. Set a stage `TRUE` to run
it; the guards there explain the couplings that are not obvious — chiefly that
**`DATA` without `MATCHING` strips `estimation_data` from the environment**,
because `001` re-saves a fresh environment and only `002` attaches that.

Each stage is sourced into its own environment
(`local = new.env(parent = parent.frame())`). Several stage scripts open with
`rm(list = ls(all = TRUE))`, which sourced into the global environment would
delete the runner itself.

## Directory layout

This study is on `layout = "v2"`: plots and their data share `output/figures/`,
table data goes to `output/tables/`. `001` passes it to `study_setup()`; `000`,
`101` and `102` pass it to `study_dirs()` so they resolve correctly even before
`001` next re-runs and bakes it into the environment.

Reach for `study_dir_figures()` / `study_dir_figure_data()` /
`study_dir_tables()`. Never paste a directory literal next to `wd$output` — `wd`
in the `.rds` is a snapshot frozen by whichever run last called `study_setup()`,
and a literal silently outlives a layout change.

The narrative embeds figures by relative path (`../output/figures/*.png` in
`98_tables_and_figures.Rmd` and `99_appendix.Rmd`). Those are not resolved
through the accessors, so a future layout change has to update them too.

## Where exhibits come from

**Every number the paper prints comes from the pipeline, and prose and exhibits
come from the same build.** This study is the reference implementation of that
rule for the repo.

The one exception is `data/tables/tableS0.csv` — Table S0 documents how the
tenure indicators were constructed from each round's questionnaire wording,
response options and per-wave mappings. No object can compute that, so it is
transcribed. It is a genuine input, not a cached output. **Curated CSVs are the
exception, not the pattern:** if you find yourself extracting numbers from a
published draft or a results workbook into a CSV, you are creating the next
drift.

**No fallbacks. Every builder errors.** A builder that catches a keying failure
and drops back to a stored CSV lets the knit "succeed" while printing last
year's numbers beside prose citing this year's. A failed render is the cheaper
failure and the designed one. The same rule covers sample-size constants and any
other value the prose cites: `stop()` naming the script to run, never a
hardcoded default.

`tbl_num()` routes through `.live_table(id)` — a switch returning the same build
the exhibit prints, not a stored file. Check every table is in that switch; one
left out is one section of the paper quietly citing a frozen value. The builders
are memoized per table id and per estimation object, because the manuscript's
~140 `tbl_num()` calls would otherwise re-read tens of MB of compressed
estimation objects that many times.

The audit trail for how the tenure variables were built lives in
`../narrative/diagnostics/`.

## Adding a file

1. **Does it *do* something — write a cache, an estimate, a figure?** It is a
   step. Number it by band: `0##` estimation, `1##` exhibits, `3##` article.
2. **Does it only *define* things?** It is a library. Name it
   `<domain>_helpers[_<what>].R`, no number.

Pick the band by **contract, not subject**: a script that fits a model is `1##`
even if it feels like a figure. Gaps are deliberate — `103_exhibit_*.R` slots in
without renumbering.

## Scratch files

`surgery.R` is a diagnostic harness for dissecting one specification — neither a
numbered step nor a library, and gitignored (`studies/**/surgery.R`). It does
not belong in `scripts/` permanently.
