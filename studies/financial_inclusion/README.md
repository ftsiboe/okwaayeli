<!-- README.md is generated from financial_inclusion.Rmd. Please edit that file. -->

![Status: Complete draft, pipeline-generated](https://img.shields.io/badge/status-complete%20draft-brightgreen)

For an overview of the broader project context, please refer to the main
[okwaayeli README](https://github.com/ftsiboe/okwaayeli/blob/main/README.md)
in the repository root.

See the
[LICENSE](https://github.com/ftsiboe/okwaayeli/blob/main/LICENSE)
file in the repository root for details.

**Status**: Complete draft, 2026-09-07. Every section is written and every
study-derived number in the text is an inline R lookup against the pipeline's
own table builds -- nothing is hand-typed, and a lookup that cannot resolve
stops the knit rather than printing a stale value.

The whole pipeline was rebuilt on a single cluster run of 2026-09-07, so
matching, treatment effects and frontier estimates share one vintage, and every
exhibit is pinned to one specification (`.RESTRICT = "Restricted"`,
`.STAT = "wmean"`, declared together in `scripts/exhibit_helpers_tables.R`).

The headline runs against the hypothesis that motivated the study.
Credit-using households operate a technology set that lies *farther* from the
best-practice frontier than that of matched non-users, not nearer to it; the
technical-efficiency difference is small and its sign is a property of whether
the comparison group is balanced; and the meta-frontier shortfall is therefore a
technology gap rather than a management gap. The gap is absent in one case worth
naming: when the farmer is the borrower. It is widest when the loan is held by
the spouse.

Results and discussion are integrated in `narrative/sections/05_results.Rmd`;
`06_discussion.Rmd` is a tombstone and holds no content. The revision record,
including what the 2026-09-07 re-run changed and what remains open, is in
[`narrative/revision/`](narrative/revision/) -- start at its `README.md`, then
`revision_notes.md` section 10.

**Before submission**: `narrative/sections/96_declarations.Rmd` carries two
`REPLACE` markers, Funding and Author contributions. They are HTML comments, so
they neither fail the build nor appear in the rendered document, and the
`.Rmd` validator strips comments before its placeholder scan. Nothing will
catch them for you.

### Contributors

- [Mark Appiah-Twumasi](https://scholar.google.com/citations?user=SrQaedsAAAAJ&hl=en)
- [Francis Tsiboe](https://scholar.google.com/citations?user=ox2t_YIAAAAJ&hl=en)
- [Aisha Adam](https://scholar.google.com/citations?user=Yf2zSWAAAAAJ&hl=en)
- [Sylvanus Gaku](https://people.extension.wisc.edu/#home/person/66ed8e9bc09f6f031b8fa85e/)
- [Peter Quartey](https://scholar.google.com/citations?user=rEPtTzoAAAAJ&hl=en)

### What this study asks

Financial inclusion — access to savings, credit, digital payments and insurance
— is central to the development strategy of agro-based economies in sub-Saharan
Africa, and recent reports document a marked expansion of (in)formal financial
services in Ghana, including the spread of mobile money. In theory, relaxing
liquidity constraints should let farmers undertake lumpy, yield-enhancing
investments and operate nearer to the production frontier. This study builds a
financial inclusion index for Ghanaian farm households from the Ghana Living
Standards Survey, then applies statistical matching within a meta-stochastic
frontier framework to identify how financial inclusion and credit access affect
technology adoption and technical efficiency in crop production.

<!--
NO ABSTRACT HERE, DELIBERATELY. Once narrative/ is scaffolded it lives in
narrative/sections/00_abstract.Rmd, where every figure is inline R against the
pipeline. A second copy here would be hand-typed and would drift from the paper
-- silently, and on the headline result. Link to it; do not restate it.
-->

The full text, tables and figures are in [`narrative/`](narrative/), written
as `.Rmd` sections under [`narrative/sections/`](narrative/sections/) and knit
to `narrative/financial-inclusion.docx` and `.html` by the pipeline. The Word
drafts in `narrative/legacy/` and `narrative/old/` are the pre-migration
history and are **not** the paper; their numbers were hand-typed and several of
them are now wrong.

**Keywords**: financial inclusion; credit; mobile money; technical efficiency;
meta-stochastic frontier; Ghana

**JEL Classification**: G21, G51, O13, Q12, Q14

### Reproducing

Run from the repository root. `scripts/run_article.R` is the single entry
point: set a stage `TRUE` to run it, and the guards in that file will stop you
from running a stage without the stage it depends on.

```
INITIALIZE    000_initialize.R                       scaffolding          fast
INDEX         000_INDEX_financial_inclusion_study.R  -> data/*.rds        ~1 min
DATA          001_DATA_...                           releases -> raw      fast
MATCHING      002_MATCHING_...                       -> estimation_data   EXPENSIVE
TREATMENT     003_TREATMENT_...                      -> treatment effects EXPENSIVE
MSF           004_MSF_...                            -> output/estimations/  HPC
DESCRIPTIVE   100_exhibit_descriptive_stats.R        -> data/*.rds        ~5-10 min
FIGURES       101_exhibit_figures.R                  -> output/figures/   moderate
WORKBOOK      102_exhibit_table_workbook.R           -> output/tables/    minutes
OBJECTS       301_article_objects.R                  -> article_objects.json  fast
RENDER        302_render_article.R                   -> .docx / .html     fast
```

On a cluster, `004` is submitted as an array job (`scripts/job_msf.sbatch`)
rather than through the runner, and the other stages are run on either side of
it. **The array can fail silently**: `004` wraps every specification in a
`tryCatch` that discards the error, so a specification that dies leaves no file
and the task still exits 0. Run
`probes/probe_missing_estimations.R` afterwards to see which of the
specifications actually produced output, and
`probes/probe_capture_spec_error.R` to read the error for one that did not.

Two coupling rules that are easy to get wrong, and that the guards in
`run_article.R` enforce:

- `DATA` re-saves the study environment **without** `estimation_data`; only
  `MATCHING` attaches it. Running `001` alone leaves everything downstream
  broken.
- The frontier fits are estimated against a specific set of matched draws.
  Re-running `MATCHING` after `004` invalidates the fits without changing a
  single file date that would tell you.

Data preparation upstream of `001` is Stata
(`data-raw/data-prep/glss/03_financial_inclusion.do`), which harmonizes the raw
GLSS files into the release assets that `001` retrieves with
`get_household_data()`.

The financial-inclusion index -- a *covariate*, not the treatment -- is built by
`scripts/000_INDEX_financial_inclusion_study.R`, an R port of the original
Stata do-file (kept in `scripts/old-codes/`) validated against it to 1e-13 on
the loadings and exactly on the released values. It writes
`data/financial_inclusion_index.rds` and its diagnostics beside it. The port
exists because the index was once the one study input that had to be hand-copied
between machines, and a stale copy on the cluster silently dropped 3,214 farm
operators through the inner join in `001` while the run reported success. Any
machine with R can now rebuild it. Its construction is documented in the paper
as Note S1.

The treatment is `credit_hh`. Its construction, and that of every other credit
and financial-service variable, is documented in the paper as Table S0 and in
[`narrative/diagnostics/credit_variable_documentation.md`](narrative/diagnostics/credit_variable_documentation.md).

---

*Maintained by [ftsiboe](https://github.com/ftsiboe)*
