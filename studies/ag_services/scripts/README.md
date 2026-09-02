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

## Known pending work

**Every script and every builder now exists** (written 2026-08-13). All fifteen
`ft_*()` functions in `exhibit_helpers_tables.R` build from the pipeline;
`.not_yet()` and `AG_PREVIEW` are kept only so a *newly added* exhibit can be
stubbed the same way.

Running order for a first full build: `100` (the descriptive cache — roughly
1,600 model fits, so allow for it), then `101`, `102`, `301`, `302`.
Tables 1, 2, 3, 4, 5, S3 and S4 read `data/descriptive_exhibits.rds` and will
stop with a pointer to `100` until it exists.

What is left is **the prose**. `00_abstract`, `01_introduction`, `05_results`
and `06_conclusion` are commented stubs, and nothing yet reads
`article_objects.json`. The exhibits are ready to be written about.

Exhibit layouts — row labels, column headers, section breaks, cell formats —
were recovered from `narrative/v000_AgricServicesProdGapGhana_FT.docx` itself
(15 tables, read with `python-docx`), not from the workbook. The numbers are the
pipeline's.

Three defects the replacements did NOT inherit from the retired scripts:

1. `100_FIGTAB` `source()`s `data-raw/scripts/figures_and_tables.R`, now a shim
   that only loads the package — the builders live in `R/exhibits-figures.R` and
   are reached through the namespace;
2. it writes figures with directory literals instead of `study_dir_figures()` /
   `study_dir_figure_data()` / `study_dir_tables()`;
3. it wrote the `ranking` sheet from columns `Diff.mean` / `V_Ratio.mean` /
   `KS.mean` / `rate.mean`, which `match_specification_ranking` does not carry
   (they are `Diff` / `V_Ratio` / `KS` / `rate`). `101` checks the names rather
   than assuming them.

### Findings carried by the new scripts

Recorded here because each is a decision someone will otherwise re-litigate:

- **Table 7 no longer matches the frozen workbook.** The 2026-08-12 estimation
  objects give TE `0.561 / 0.606 / 0.045` against `msf::Table4`'s
  `0.560 / 0.609 / 0.049`, and MTE difference `0.006` vs `0.009`. Six of nine
  `services0` cells moved. `run_article.R`'s note that the frontier estimates
  "CANNOT change, and Table 7's 36/36 verification stands" no longer holds.
- **Draft Table S3's "Extension" column is `services3`** (n = 11,752), not the
  `.do`'s `extension0 = extension > 2` (n = 12,004). `100` computes both and
  substitutes neither.
- **`output/figures/` is empty**, so the 2026-08-07 `.docx` embeds six figures
  that are not on disk. `302` now stops on that instead of rendering empty image
  frames, which pandoc does without an error.
- **Draft Table 4 is titled "(2012–2017)" but its numbers span 2005–2017.**
  Verified: Millet `services_planting` is `0.515 (0.500)` over GLSS5–7 and
  `0.478 (0.500)` over GLSS6–7; the reference is the former. Tables 2, 3 and 5
  carry the same range off the same sheet.
- **`extension` carries eight levels but only seven value labels.** The `.do`'s
  `extensionCat1..7` becomes `extension_1..8`; draft Table 4's
  `extensionCat2..7` are levels 1–6 either way, so the printed columns are
  unaffected.
- **Figure 2 has four bars because only four services have an estimable level-1
  gap.** `disagscors` is two-way — `disagscors_level` is the disaggregator's
  level, `TCHLvel` is still the frontier group — and within credit, husbandry,
  labour and records there is no untreated group, so `004` emits no level-1 gap.
  `101` reports the dropped services and writes their level-0 gaps to
  `score_by_services_all_levels.csv`.
- **The draft's "Female farmer (dummy)" row is a Stata collision, in Tables 1,
  S3 and S4.** Its group cells are group *shares*, not Female's group means:
  8,304/22,519 = 0.37 and 14,215/22,519 = 0.63 in Table 1; 11,752/22,519 = 0.52,
  3,690/22,519 = 0.16, 5,713/22,519 = 0.25 in Table S3. That is the
  `mat roweq A = Female` bug the `.do` fixed on 2026-07-15, and the v000 draft
  predates the fix. The live builders print Female's actual group means
  (0.24 / 0.25).
- **Inputs 5 and 6 — fertilizer and pesticide — are exchanged between the draft
  and this build**, in Table 6 and Table S7 alike and by exactly a swap
  (draft T6 `0.021 / 0.012` against the object's `el5 0.012 / el6 0.021`; draft
  S7 `lnI5 0.031 / lnI6 0.024` against `lnI5 0.024 / lnI6 0.031`). The mapping
  is positional — `stochastic_frontier-core.R:1112` assigns
  `data[, paste0("I", i)] <- data[, input_variables[i]]` — so on the current code
  path `el5`/`lnI5` is fertilizer. Either the draft mislabelled the pair or
  `input_variables` was ordered `..., "PestLt", "FertKg"` when it was built;
  settling it means looking at `004` as of the draft. Do not "fix" it by
  swapping the labels.
- **`sf_estm`'s `StdError` and `Pvalue` are unusable.** `StdError` is `NA` on 64%
  of rows and `Pvalue` returns an exact `1.000` on the naive frontier. Tables 6,
  S7 and S8 read `Estimate.sd` and `jack_pv`, like every other summary in the
  object — verified against the draft (`Gamma` naive `0.604*** (0.001)`, which is
  `Estimate.sd`). Reading the other pair yields a coefficient table with no
  dispersion and no stars on a whole column, which renders cleanly.
- **Table 6's naive column is not one lookup.** The TE block reads `TE0` at
  `TCHLvel == "National"`; the MTE block reads `MTE` at `"National"`. Using `TE0`
  for both — the obvious simplification — puts `0.586` where `0.528` belongs and
  the cell still looks reasonable. The draft's unmatched cells (`0.598`, `0.555`)
  pin the distinction exactly.
- **Table 4's crop order is the draft's and is not derivable.** The draft sorts
  its two blocks by different columns, so its share block and trend block are in
  different row orders; the builders use one order for both, and a diff against
  the draft has to match on the crop label rather than the row number.

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

**Upstream only, and not in this folder.** The community-level release is built
by `data-raw/scripts/data-prep/glss/11_ag_services.do`, which lives with the
other GLSS harmonizers rather than with this study. `001` calls it directly
where Stata is available and falls back to the saved release where it is not;
either way the schema contract in `001` decides whether the release is usable.

Nothing downstream of `001` touches Stata. The exhibit band (`1##`) is pure R,
as in `land_tenure`.

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
