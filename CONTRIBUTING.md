# Contributing to okwaayeli

Thank you for your interest in contributing. `okwaayeli` is a research
package that decomposes Ghanaian crop-production shortfalls into farmer
technical inefficiency and ecological / socio-economic technology gaps.
Contributions are welcome from researchers, graduate students,
methodologists, and documentation editors alike.

This document covers:

1.  Code of conduct
2.  Repository layout — where to put things
3.  How to set up a working environment
4.  The standard study pipeline (001 → 100)
5.  Adding a new study
6.  Adding or modifying a harmonized dataset
7.  Coding and style conventions
8.  Documentation conventions
9.  Submitting a pull request
10. Reporting bugs and suggesting features

------------------------------------------------------------------------

## 1. Code of conduct

Participation in this project is governed by the [Contributor
Covenant](https://ftsiboe.github.io/okwaayeli/code_of_conduct.md). By
participating, you agree to abide by its terms. Report unacceptable
behaviour to <ftsiboe@hotmail.com>.

## 2. Repository layout

    okwaayeli/
    ├── R/                              # Package source — the analytical engine
    ├── man/                            # Auto-generated function documentation
    ├── data-raw/                       # Sources used to build the harmonized data
    │   └── releases/harmonized_data/   # 11 published .dta files + codebook
    ├── replications/                   # One folder per study
    │   └── <study_name>/
    │       ├── 001_DATA_<study>.R
    │       ├── 002_MATCHING_<study>.R
    │       ├── 003_TREATMENT_<study>.R
    │       ├── 004_MSF_<study>.R
    │       ├── 100_FIGTAB_<study>.R
    │       ├── 005_Release_<study>.R   # optional, prepares the public bundle
    │       ├── job_msf.sbatch          # optional, SLURM job script
    │       ├── <study>.Rmd / README.md
    │       └── output/
    ├── manuscripts/                    # Manuscript drafts, exhibits, literature
    ├── tests/                          # testthat harness
    ├── vignettes/                      # Long-form documentation
    └── .github/                        # CI, issue templates, PR template

Where to put things:

| Type of change | Path |
|----|----|
| Reusable function (MSF, matching, helper) | `R/` + `man/` + a unit test in `tests/testthat/` |
| Study-specific analysis | `replications/<study>/` |
| Public data harmonization | `data-raw/` and the GitHub `hh_data` release |
| Manuscript text, figures, lit | `manuscripts/<study>/` (not part of the R package build) |
| Long-form how-tos | `vignettes/` |

## 3. Setting up a working environment

You need:

- R ≥ 4.1
- A Stata installation (the data harmonization in
  `data-raw/okwaayeli_DATA.do` is Stata; the rest of the pipeline is R)
- The system libraries required by `MatchIt`, `frontier`, `micEcon`,
  `rgenoud`, `quadprog`, `optmatch`, `dbarts`, `CBPS`. On Debian/Ubuntu
  the usual `build-essential`, `libgmp-dev`, `libssl-dev`, `libxml2-dev`
  set suffices.

Install the development version:

``` r

# install.packages("devtools")
devtools::install_github("ftsiboe/okwaayeli")
```

Or, for development from a clone:

``` r

devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
```

Harmonized data are not bundled with the package. They are downloaded on
first call to
[`get_household_data()`](https://ftsiboe.github.io/okwaayeli/reference/get_household_data.md)
from the `hh_data` GitHub release:

``` r

farmer_data <- okwaayeli::get_household_data("harmonized_crop_farmer_data")
```

Authentication uses the `GHProdLab_TOKEN` environment variable if set,
otherwise standard `gh` CLI / git credential helpers.

## 4. The standard study pipeline

Every active study follows the same five-stage pipeline. Each stage is a
numbered R script. Output is written to `replications/<study>/output/`.

    001_DATA      Pull the relevant harmonized .dta files, restrict the sample,
                  build the treatment indicator(s) and the covariate set.

    002_MATCHING  For every matching specification declared by
                  write_match_formulas(), build matched samples via
                  match_sample_specifications() and write covariate balance
                  statistics with covariate_balance().

    003_TREATMENT Compute per-spec average treatment effects on the treated
                  (and other heterogeneity dimensions) via
                  treatment_effect_calculation() / treatment_effect_summary().

    004_MSF       Run the meta-stochastic-frontier estimation across draws
                  (see draw_msf_estimations() and draw_msf_summary()). Outputs
                  go to output/estimations/ and output/te_summary.rds.

    100_FIGTAB    Render the final figures and tables (.R + .do siblings).
                  Writes the canonical *_results.xlsx exhibit.

    005_Release   (optional) Package the public replication bundle.

SLURM users: each replication folder ships a `job_msf.sbatch` for array
execution. The
[`run_only_for()`](https://ftsiboe.github.io/okwaayeli/reference/run_only_for.md)
helper in `R/helpers.R` lets you target a specific subset of draw IDs.

## 5. Adding a new study

The cleanest way to bootstrap a new study is to copy
`replications/disability/` as a template, then:

1.  Rename all scripts from `*_disability_study.R` to
    `*_<your_study>_study.R`.
2.  Edit `001_DATA_<your_study>_study.R` to pull the harmonized datasets
    you need (use `get_household_data("harmonized_<…>_data")`).
3.  Edit `002_MATCHING_<your_study>_study.R` to declare your treatment
    variable and the variables you want to balance on.
4.  Add a `replications/<your_study>/<your_study>.Rmd` describing the
    study, its contributors, and (eventually) the abstract.
5.  Open a [Study proposal
    issue](https://ftsiboe.github.io/okwaayeli/.github/ISSUE_TEMPLATE/study_proposal.yml)
    so it appears on the lab roadmap and reviewers can flag duplication
    risks.
6.  Submit a PR adding the study folder and a row in the main README’s
    “Work in progress” / “Scheduled studies” list.

If your study needs a new harmonized dataset, see §6.

## 6. Adding or modifying a harmonized dataset

The harmonized datasets live in `data-raw/releases/harmonized_data/` and
are published as the `hh_data` GitHub release. Updating one is a
multi-step process:

1.  Modify the Stata harmonization in `data-raw/okwaayeli_DATA.do` (or
    the per-topic block within it).
2.  Run the harmonization end-to-end and put the new `.dta` file in
    `data-raw/releases/harmonized_data/`.
3.  Update `data-raw/releases/harmonized_data/codebook.md` with the new
    / changed variable definitions.
4.  Run `data-raw/scripts/001_harmonized_data_release.R` to push the new
    release asset via `piggyback`.
5.  Bump the `Version:` field in `DESCRIPTION` and add a `NEWS.md` entry
    describing the data change.

Existing studies that consume the dataset should be re-run and their
`*_results.xlsx` exhibits regenerated as a separate commit.

## 7. Coding and style conventions

- **Line length**: aim for ≤ 100 columns.
- **Naming**: snake_case for functions and data, lowerCamelCase only for
  short loop / index variables when readability requires it.
- **Pipes**: native R `|>` is preferred over `%>%`; `magrittr` is not a
  dependency.
- **Comments**: prefer roxygen comments on every exported function
  (`@param`, `@return`, `@examples`, `@export`). All exported functions
  must have at least one runnable `@examples` block (use `\dontrun{}` if
  it requires external data).
- **Globals**: data.table / dplyr free-variable warnings should be
  suppressed via
  [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  in `R/zzz.R`, not by adding `.data$` pronouns inline.
- **Random seeds**: every stochastic operation should be reproducible.
  Use `okwaayeli_control()$myseed` (currently `11122025`) and pass it
  into [`set.seed()`](https://rdrr.io/r/base/Random.html) at the top of
  each `draw_*` call.
- **No [`library()`](https://rdrr.io/r/base/library.html) calls in
  `R/`**. Use `pkg::fn()` or import via roxygen `@importFrom`.

## 8. Documentation conventions

- **Function docs**: every exported function carries a roxygen block.
  Run `devtools::document()` before committing.
- **READMEs**: study-level `README.md` files are generated from `*.Rmd`.
  Edit the `.Rmd`, then render with
  `rmarkdown::render("…/README.Rmd", output_format = "github_document", output_file = "README.md")`.
- **Vignettes**: long-form how-tos go in `vignettes/`. New vignettes
  need `\VignetteIndexEntry{}` and should build under
  `devtools::check()`.
- **NEWS.md**: every PR touching `R/`, `data-raw/`, or a public function
  must add an entry under the appropriate version heading.

## 9. Submitting a pull request

1.  Fork the repository or branch from `main` if you have write access.
2.  Use a descriptive branch name: `feature/<short-name>`,
    `fix/<short-name>`, `docs/<short-name>`, `study/<study-name>`.
3.  Make focused commits — one logical change per commit. Prefer
    imperative-mood, ≤ 72-char subject lines
    (e.g. `Add land-tenure MSF spec`).
4.  Run `devtools::check()` locally. PRs are gated by R-CMD-check on
    GitHub Actions; failing CI blocks merge.
5.  If you changed exported functions or examples, run
    `devtools::document()` and commit the regenerated `man/` files in
    the same PR.
6.  Open the PR, fill out the template, and request a review from
    [@ftsiboe](https://github.com/ftsiboe) (or the relevant CODEOWNERS).

## 10. Reporting bugs and suggesting features

Use the [issue
templates](https://ftsiboe.github.io/okwaayeli/.github/ISSUE_TEMPLATE/)
— there is one for bug reports, one for new-study proposals, and one for
data requests.

For methodology questions or open-ended discussions, please use [GitHub
Discussions](https://github.com/ftsiboe/okwaayeli/discussions) rather
than opening an issue.

Sensitive security reports should follow
[SECURITY.md](https://ftsiboe.github.io/okwaayeli/SECURITY.md).

------------------------------------------------------------------------

Constructive feedback is appreciated. Thank you for helping make
agricultural-productivity research in Ghana more reproducible.
