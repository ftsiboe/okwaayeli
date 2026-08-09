Financial Inclusion and Credit Impacts on Crop Production in Ghana
================

<!-- README.md is generated from financial_inclusion.Rmd. Please edit that file. -->

![Status: Drafting (v005)](https://img.shields.io/badge/status-drafting-lightgrey)

For an overview of the broader project context, please refer to the main
[okwaayeli README](https://github.com/ftsiboe/okwaayeli/blob/main/README.md)
in the repository root.

See the
[LICENSE](https://github.com/ftsiboe/okwaayeli/blob/main/LICENSE)
file in the repository root for details.

**Status**: Drafting (v005). Consolidated into the canonical study layout on
2026-08-08; the automated narrative system landed the same day, and the sections
in [`narrative/sections/`](narrative/sections/) render from the pipeline.

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

The full text, tables and figures are in [`narrative/`](narrative/), built by the
same automated manuscript system `resource_extraction` and `land_tenure` use:
`narrative/sections/*.Rmd` assembled by `302_render_article.R` into
`financial-inclusion.docx` / `.html`. The superseded Word drafts are kept in
`narrative/legacy/` and `narrative/old/` for provenance only.

Measured quantities in the sections resolve at knit time against
`narrative/article_objects.json` and the exhibit cache, with no fallback — a
lookup that cannot resolve stops the knit. The exceptions are listed in each
section's banner. **`03_financial_inclusion_context.Rmd` is the one section not
yet fully live**; the literals that remain are enumerated in its banner.

**Keywords**: financial inclusion; credit; mobile money; technical efficiency;
meta-stochastic frontier; Ghana

**JEL Classification**: G21, G51, O13, Q12, Q14

### Reproducing

Run from the repository root, via `scripts/run_article.R` (stage flags) or by
calling the numbered steps directly, in order.

    000_initialize   scaffolding                       fast
    000_INDEX        -> harmonized financial_inclusion_index (Stata)
    001_DATA         harmonized releases -> raw data   fast
    002_MATCHING     -> estimation_data                EXPENSIVE
    003_TREATMENT    -> treatment effects              EXPENSIVE
    004_MSF          -> output/estimations/            HPC (job_msf.sbatch)
    100/101/102_*    -> output/tables/, output/figures/  moderate
    301_article_objects -> narrative/article_objects.json
    302_render_article  -> narrative/financial-inclusion.docx/.html

Data preparation upstream of `001` is Stata
(`data-raw/data-prep/glss/03_financial_inclusion.do`), which harmonizes the raw
GLSS files.

### The treatment variable

**The treatment is `credit_hh`, household access to credit** — 1 if the farm
operator, spouse, a child, or another household member applied for a loan in the
past 12 months, was granted it, and recorded a positive amount. It is built in
`data-raw/data-prep/glss/03_financial_inclusion.do:1339-1416`, set as the
estimation flag at `002_MATCHING:63`
(`Treat <- as.integer(as.numeric(DATA$credit_hh > 0))`), and used as the frontier
grouping variable at `004_MSF:86`. Every estimation object on disk is named
`*_credit_hh_*`.

**The financial inclusion index is not the treatment.**
`scripts/000_INDEX_financial_inclusion_study.do` builds `FinIdx` and writes it as
a harmonized release that `001` reads back; it enters as a *covariate* —
`FinIdxSi` in the matching distance (`002_MATCHING:69`) and `FinIdxCat` as a
heterogeneity dimension (`004_MSF:187`). The two play different roles and should
not be conflated. Earlier revisions of this README described the index as "the
treatment variable itself"; that was wrong and is corrected here (2026-08-09).

Full documentation:
[`narrative/diagnostics/credit_variable_documentation.md`](narrative/diagnostics/credit_variable_documentation.md)
for the treatment, and
[`narrative/diagnostics/financial_inclusion_index_documentation.md`](narrative/diagnostics/financial_inclusion_index_documentation.md)
for the index.

See [`scripts/README.md`](scripts/README.md) for the naming convention and for
what this study still lacks against `resource_extraction` and `land_tenure`.

------------------------------------------------------------------------

*Maintained by [ftsiboe](https://github.com/ftsiboe)*
