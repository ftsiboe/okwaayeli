# `studies/time_poverty/scripts` — naming convention

**A number means a position in a sequence. If a file has no position, it has no
number.**

This mirrors `studies/land_tenure/scripts/README.md`. Where the two studies
differ, it is noted — and the difference is work still outstanding, not a
deliberate divergence. See `../AGENT_PROMPT.md` for what remains, and
`../narrative/diagnostics/scaffold_2026-08-07.md` for what the scaffold does and
does not contain.

## Steps — numbered, run in order

| Band | Purpose | When |
|---|---|---|
| `0##` | estimation | the data changes |
| `1##` | exhibits | the estimates change |
| `3##` | article | every render |

| Script | Reads | Writes | Status |
|---|---|---|---|
| `000_initialize.R` | | the directory tree (`study_dirs()`, `v2`) | works |
| `001_DATA_time_poverty_study.R` | harmonized releases | `study_raw_data` | works |
| `002_MATCHING_time_poverty_study.R` | `study_raw_data` | `estimation_data`, matches | works — **and must be re-run**, see below |
| `003_TREATMENT_time_poverty_study.R` | `estimation_data` | `te_summary.rds` | **blocked on 002** |
| `004_MSF_time_poverty_study.R` | `estimation_data` | `output/estimations/` (HPC) | **blocked on 002**, never run |
| `100_exhibit_descriptive_stats.R` | `study_raw_data` | `data/descriptive_exhibits.rds` | **stub** |
| `101_exhibit_figures.R` | `output/estimations/` | `output/figures/`, `output/tables/` | repaired, **unverified** |
| `102_exhibit_table_workbook.R` | the table builders | `output/tables/time_poverty_tables.xlsx` | **stub** |
| `301_article_objects.R` | estimations, environment | `narrative/article_objects.json` | **partial** |
| `302_render_article.R` | | `.docx` / `.html` | works (renders stubs) |
| `run_article.R` | | the entry point; stage levers | works |

Every stub stops with an error naming what to do next. None emits partial
output: a half-populated `article_objects.json` renders as a document full of
blanks that reads like a formatting bug rather than a missing pipeline.

## Libraries — unnumbered, sourced by whatever needs them

| File | Provides | Sourced by |
|---|---|---|
| `article_helpers.R` | paths, `fmt_*`, `assert_present()` | 301, 302, the Rmd |
| `exhibit_helpers_tables.R` | `.STUDY_ROOT`, `.read_est()`, `tbl_num/pct/stars` | the Rmd, 102 |

These define things; they do not do things. A number on them would be a false
promise, and invites the wrong question — "when do I run this?" You don't.

The tell: **if a runner would `source()` it and nothing would happen, it is a
library.**

`exhibit_helpers_tables.R` carries **no `ft_*()` builders**. The sibling studies'
copies are ~28 KB because they have manuscripts and therefore exhibit lists.
This study has neither, and inventing builders would be inventing the paper.
The plumbing above them is complete and does not change when they are added.

## Stata — unnumbered by role, not by band

| File | Role |
|---|---|
| `time_poverty_DATA.do` | **upstream of `001`** — builds the paid/unpaid time-use datasets and writes the `harmonized_time_poverty_data` release. Run by hand from the repo root; inputs are the `.dta` files in `../time-poverty-assets/`, reached through `$TPAssets`. |
| `100_exhibits.do` | **unconverted disability code, hard-stopped.** See below. |

## Run order

    001 → 002 → 003 → 004        estimation
    100 → 101 → 102              exhibit caches and deliverables
    301 → 302                    the article

`run_article.R` drives all of it behind stage levers, all defaulting `FALSE` —
there is no cheap "rebuild from caches" path here because there are no caches
yet.

`004` is a SLURM array — `sbatch studies/time_poverty/scripts/job_msf.sbatch`.
The `--array=1-18` bound must match `nrow(model_specifications)` or the trailing
specifications never run. The scheduler entry is case `7` in
`studies/run_data_and_match_for_all.sbatch`, and `run_only_for(id = 7, ...)` in
`001`–`003` must agree with it.

## The environment on disk is incomplete

Verified by reading `data/time_poverty_study_environment.rds`: it holds only
`wd`, `myseed` and `study_raw_data`. `estimation_data`, `match_specifications`,
`sample_draw_list`, `match_specification_optimal`, `match_specification_ranking`
and `balance_table` are all absent — so the 808 files in `output/matching/` are
orphaned from the specification table that indexes them.

`002` is the unblocking step, not `004`. The seed is fixed (1980632), so the
redraw should reproduce the same samples.

## The unblocking order

1. `MATCHING` — repopulates the environment. **Everything below waits on this**,
   including `004`.
2. `sbatch scripts/job_msf.sbatch` — fills `output/estimations/`.
3. `TREATMENT` — fills `output/treatment_effects/` and `te_summary.rds`.
4. `FIGURES` — the first real test of `101`.
5. Write `100` and `102` against what `101` actually produced.
6. Write the manuscript, then `301` and `RENDER`.

`100_exhibit_descriptive_stats.R` is the one stage not blocked on step 1 — its
input, `study_raw_data`, already exists. It is a stub because *what* to describe
is a manuscript decision, not because it cannot run.

## `_legacy_disability_copies/`

`005_Release_disability_copy.R` sat at the study root named
`005_Release_time_poverty_study.R` while its contents released the **disability**
study (`project_name <- "disability"`). Quarantined, not repaired: `land_tenure`
has no `005` stage at all.

## Two things that are not yet true here

**`100_exhibits.do` is still the disability study's Stata script.** It cannot be
repaired by renaming: it computes GLSS6-vs-GLSS7 **trend** differences and loops
over eight disability sub-indicators (`disabCat1`–`disabCat7`). This study is
**GLSS7 only** (see `001`) and has three indicators (`tpoor0150`, `tpoor0125`,
`tpoorweai`), two of which are set to missing wherever they disagree with the
first. It carries a hard stop at the top; deleting the stop does not make it
correct.

**`tpoor0150` does not measure what its label says.** `time_poverty_DATA.do`
computes a committed-time cutoff, saves it, then does `keep if s1q3==1` and
recomputes the same variable names off **paid** time, overwriting both the
in-memory version and `PaidTimepoverty.dta`. The released treatment is the
paid-time one; the label still says "Committed Time". Flagged in that file, in
`101`, in `301`, and in `../narrative/sections/02_data.Rmd`. Nothing was changed.

There is no `output/*_results.xlsx` for this study, so there is no frozen
reference to diff a new exhibit build against. Absence of differences is not a
pass.
