# `studies/ag_services/scripts` — naming convention

**A number means a position in a sequence. If a file has no position, it has no
number.**

Ported from `studies/land_tenure/scripts/README.md` on 2026-08-07. The
convention is identical; the differences below are real differences in this
study, not drift.

## Steps — numbered, run in order

| Band | Purpose | When |
|---|---|---|
| `0##` | estimation | the data changes |
| `1##` | exhibits | the estimates change |
| `3##` | article | every render |

| Script | Reads | Writes |
|---|---|---|
| `000_initialize.R` | | the directory tree (`study_dirs()`, layout `v2`) |
| `001_DATA_ag_services_study.R` | harmonized releases | `study_raw_data`, `data/*_study_environment.rds` |
| `002_MATCHING_ag_services_study.R` | `study_raw_data` | `estimation_data`, `output/matching/` |
| `003_TREATMENT_ag_services_study.R` | `estimation_data` | `output/te_summary.rds` |
| `004_MSF_ag_services_study.R` | `estimation_data` | `output/estimations/` (HPC) |
| `100_exhibit_descriptive_stats.R` | `study_raw_data` | `data/descriptive_exhibits.rds` |
| `100_exhibits.do` | `data/tech_inefficiency_*.dta` | `output/tables/` (Stata-only descriptives) |
| `101_exhibit_figures.R` | `output/estimations/` | `output/figures/`, `output/tables/` |
| `102_exhibit_table_workbook.R` | the table builders | `output/tables/ag_services_tables.xlsx` |
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

    001 -> 002 -> 003 -> 004        estimation
    100 -> 101 -> 102               exhibit caches and deliverables
    301 -> 302                      the article

`run_article.R` drives all of it behind stage levers. Set a stage `TRUE` to run
it; the guards there explain the couplings that are not obvious (chiefly that
`DATA` without `MATCHING` strips `estimation_data` from the environment).

## Where exhibits come from

Every table and figure is built from the pipeline. The exceptions are the two
curated inputs in `data/tables/`, which transcribe questionnaire content and
cannot be computed from any object:

- **Table S1** — final agricultural service delivery variables and their GLSS7
  source references
- **Table S2** — harmonization of the agricultural service questions across
  GLSS rounds

Both were recovered on 2026-08-07 from sheets named `Sheet1` and `Sheet2` in
`ag_services_results-msf.xlsx`. Nothing else belongs in `data/tables/`.

## Stata's job here

Unlike `land_tenure`, this study still has a downstream Stata stage.
`100_exhibits.do` (was `100_FIGTAB_ag_services.do`) reads
`data/tech_inefficiency_ag_services_data.dta` and writes descriptive exhibits.
Upstream, `data-raw/okwaayeli_DATA.do` harmonizes the raw GLSS files.

The `$GitHub` globals in `100_exhibits.do` already point at this repo, so no
repointing is needed — only the `output\` -> `data\` correction applied on
2026-08-07, plus the removal of a `putexcel` line left over from the
Farmer-Age-Productivity study.

`100_exhibits.do` is a candidate for retirement once `102` owns the workbook.

## Known pending work (2026-08-07)

`100_FIGTAB_ag_services.R` is still present and is **not** canonical. It is the
source for the decomposition into `100_exhibit_descriptive_stats.R`,
`101_exhibit_figures.R` and `102_exhibit_table_workbook.R`, and it retains two
defects until then:

1. it `source()`s `data-raw/scripts/figures_and_tables.R`, now a shim that only
   loads the package — the builders live in `R/exhibits-figures.R` and should be
   called through the namespace;
2. it writes figures with directory literals instead of `study_dir_figures()` /
   `study_dir_figure_data()` / `study_dir_tables()`.

Retire it once the three replacements are in and verified.

## Adding a file

1. **Does it *do* something — write a cache, an estimate, a figure?** It is a
   step. Number it by band: `0##` estimation, `1##` exhibits, `3##` article.
2. **Does it only *define* things?** It is a library. Name it
   `<domain>_helpers[_<what>].R`, no number.

Pick the band by **contract, not subject**: a script that fits a model is `1##`
even if it feels like a figure. Gaps are deliberate — `103_exhibit_*.R` slots in
without renumbering.

The reference for this layout is `studies/land_tenure`.

## Stata's place in this study

**Upstream only.** `000_HARMONIZE_ag_services_data.do` builds the community-level
release from the raw GLSS files and runs *before* `001`. Nothing downstream of
`001` touches Stata. The exhibit band (`1##`) is pure R, as in `land_tenure`.

`resource_extraction` still carries a `100_exhibits.do`; `land_tenure` does not.
Where the two siblings disagree about the exhibit layer, `land_tenure` is the
newer answer, and this study follows it.

## Retired 2026-08-07 — in `_to_delete/`, kept as specifications

Neither file was ever a stage in `run_article.R`. Both are superseded, but they
are the record of what the replacements must reproduce, so read them before
writing the new scripts and delete them only afterwards.

| Retired | Superseded by | What it specifies |
|---|---|---|
| `100_exhibits.do` (was `100_FIGTAB_ag_services.do`) | `100_exhibit_descriptive_stats.R` | The descriptive exhibits: workbook sheets `Table1`, `Table2-services`, `TableS2`, `TableS3` — the draft's Tables 1, 4, S3, S4. The replacement builds these from `R/descriptive-exhibits-core.R` instead of Stata. |
| `100_FIGTAB_ag_services.R` | `101_exhibit_figures.R` + `exhibit_helpers_tables.R` | The eight figures (`score_trend`, `score_by_services`, `score_distributions`, `heterogeneity_crop_region`, `heterogeneity_genderAge`, `input_TE`, `robustness`, `Covariate_balance_variance`) and the `msf` / `CovBalDATA` / `ranking` machine sheets. |

Two consequences of retiring the `.do`:

1. **`data/tech_inefficiency_ag_services_data.dta` is orphaned.** It was written
   by `100_exhibits.do` line 12 and read back by the same file. Nothing else
   reads it. It should go to `_to_delete/` once `100_exhibit_descriptive_stats.R`
   is confirmed to need no Stata intermediate.
2. **The three legacy workbooks are the verification reference, not inputs.**
   Every cell is frozen in
   `narrative/diagnostics/verification_reference_2026-08-07.json`, which is what
   the new builders are diffed against. See the `ft_table7()` precedent: 36/36.

Also retired from `100_exhibits.do` and NOT to be carried forward: line 200's
`putexcel set "Results\Farmer_Age_Productivity_Ghana_Results.xlsx"`, a leftover
from a different study writing to a relative path outside the repo.
