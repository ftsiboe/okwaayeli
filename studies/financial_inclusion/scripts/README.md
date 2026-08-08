# `studies/financial_inclusion/scripts` — naming convention

**A number means a position in a sequence. If a file has no position, it has no
number.**

This mirrors `studies/resource_extraction/scripts/README.md`. Where the studies
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
| `000_INDEX_financial_inclusion_study.do` | harmonized releases | `data-raw/releases/harmonized_data/financial_inclusion_index.dta` |
| `001_DATA_financial_inclusion_study.R` | harmonized releases + the index | `study_raw_data` |
| `002_MATCHING_financial_inclusion_study.R` | `study_raw_data` | `estimation_data`, matches |
| `003_TREATMENT_financial_inclusion_study.R` | `estimation_data` | `te_summary.rds` |
| `004_MSF_financial_inclusion_study.R` | `estimation_data` | `output/estimations/` (HPC) |
| `100_exhibits.do` | `data/*_study_data.dta` | legacy Stata exhibit path |
| `101_exhibit_figures.R` | `output/estimations/` | `output/figures/` |

## Not yet written

The exhibit and article layers are still missing. Against
`resource_extraction`, this study lacks:

| Missing | Would do |
|---|---|
| `100_exhibit_descriptive_stats.R` | descriptives off `R/descriptive-exhibits-core.R` → `data/descriptive_exhibits.rds` |
| `102_exhibit_table_workbook.R` | the same builders → `output/tables/financial_inclusion_tables.xlsx` |
| `301_article_objects.R` | estimations → `narrative/article_objects.json` |
| `302_render_article.R` | → `.docx` / `.html` |
| `run_article.R` | the entry point; stage levers |
| `article_helpers.R` | paths, `fmt_*`, `assert_present()` |
| `exhibit_helpers_tables.R` | flextable builders, `.read_est()`, `.STUDY_ROOT` |

Until `run_article.R` exists there is no single entry point: run the numbered
steps directly, in order.

## Libraries — unnumbered, sourced by whatever needs them

None yet — see above. When they arrive they take no number. They define things;
they do not do things. A number on them would be a false promise, and invites
the wrong question — "when do I run this?" You don't.

The tell: **if a runner would `source()` it and nothing would happen, it is a
library.**

## Run order

    000_INDEX → 001 → 002 → 003 → 004        estimation
    101                                       figures

`000_INDEX_financial_inclusion_study.do` is Stata and builds the
**financial-inclusion index — the treatment variable itself**. It writes a
harmonized release that `001` reads back with `haven::read_dta()`, so it is
upstream of everything here and is the one script whose output is not confined
to this study. It kept its `000` prefix and its place in `scripts/` by decision;
by contract it is closer to `data-raw/scripts/data-prep/glss/03_financial_inclusion.do`. **Its construction is
undocumented** — `land_tenure` documents its tenure indicators in
`narrative/diagnostics/`, and a constructed index needs that more than a survey
question does, not less.

## Directory layout

This study is on `layout = "v2"`: plots and their data share `output/figures/`,
table data goes to `output/tables/`. `001` passes it to `study_setup()`; `000`
and `101` pass it to `study_dirs()` so they resolve correctly even before `001`
next re-runs and bakes it into the environment.

Migrated from `legacy` on 2026-08-08. The old `output/figure/` and
`output/figure_data/` were emptied into `output/figures/` and moved to
`_to_delete/` — unlike `resource_extraction`, which left both pairs in place.

`002`, `003` and `004` **do** call `study_dirs()` here, unlike in
`resource_extraction`: they read the environment back from `data/`, which is new
enough that any `.rds` written before 2026-08-08 predates it.

Reach for `study_dir_figures()` / `study_dir_figure_data()` /
`study_dir_tables()`. Never paste a directory literal next to `wd$output` — `wd`
in the `.rds` is a snapshot frozen by whichever run last called `study_setup()`,
and a literal silently outlives a layout change. `101_exhibit_figures.R` had
seven such literals until 2026-08-08.

## Where exhibits come from

The target — and `land_tenure`'s standing rule — is that **every number the
paper prints comes from the pipeline, and prose and exhibits come from the same
build.** The one legitimate exception is an exhibit transcribing something no
object can compute (questionnaire wording, response options, per-wave mappings);
`land_tenure`'s Table S0 is the model.

**This study is a long way from there.** There is no table architecture at all:
`output/financial_inclusion_results.xlsx` is written by `101` and by
`100_exhibits.do`, and the Word drafts in `narrative/` were typed against it by
hand. Nothing is wired live.

`101_exhibit_figures.R` also does not yet persist the data behind every figure.
`output/figures/` holds 7 `.png` but only 9 data files — at least
`heterogeneity_financial_inclusion`, `heterogeneity_crop_region`,
`heterogeneity_genderAge`, `Covariate_balance_variance` and
`score_distributions` have no saved `.rds`/`.csv`. Any number the manuscript
quotes off those figures is unverifiable today.

## Scratch and legacy

`101_heterogeneity_financial_inclusion_figures.R` is **legacy and broken**, kept
by decision rather than deleted. It cannot run: it sources a `codes/` directory
that does not exist in this repo, `setwd()`s into
`studies/tech_inefficiency_financial_inclusion` (the study's name in the old
`GH-Agric-Productivity-Lab` repo), and reads `results/estimations/` rather than
`output/estimations/`. It is an ancestor of `101_exhibit_figures.R`. Note the
prefix collision: two files at `101`, only one of which is a step. Diff it
against `101_exhibit_figures.R`, port anything unique, then delete it.

`100_exhibits.do` (Stata) is the legacy exhibit path, superseded once a
descriptive engine lands here — not before. Stata's remaining job is upstream.

## Adding a file

1. **Does it *do* something — write a cache, an estimate, a figure?** It is a
   step. Number it by band: `0##` estimation, `1##` exhibits, `3##` article.
2. **Does it only *define* things?** It is a library. Name it
   `<domain>_helpers[_<what>].R`, no number.

Pick the band by **contract, not subject**: a script that fits a model is `1##`
even if it feels like a figure. Gaps are deliberate — `103_exhibit_*.R` slots in
without renumbering.
