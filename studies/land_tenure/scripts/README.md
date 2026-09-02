# scripts/

Everything that builds the land tenure study, from the harmonized survey
releases through to the manuscript.

## The naming convention

A **number** means a position in a sequence. An **unnumbered** file is a
library: sourced by whatever needs it, never "run".

| Prefix | What it is | Cost |
|---|---|---|
| `0xx` | Analysis. Data preparation through estimation. | Minutes to days |
| `1xx` | Exhibits. Tables and figures computed once and cached. | Minutes |
| `3xx` | Manuscript. Assemble the numbers, render the outputs. | Seconds |
| *(none)* | Libraries and harnesses. | — |

The gap between `1xx` and `3xx` is deliberate: `2xx` is reserved for anything
that sits between the exhibits and the write-up.

## Running it

`run_article.R` is the entry point. Open it, set the stage flags, source it:

```r
setwd("c:/nextcloud/Documents/Research work/Working papers/Manuscripts/land_tenure")
source("scripts/run_article.R")
```

The defaults are the cheap path — `OBJECTS` and `RENDER` only, which rebuilds
the article from the caches already on disk. Turn a stage on when its inputs
change.

Typical runs:

| You changed | Turn on |
|---|---|
| The prose only | `OBJECTS` + `RENDER` *(the default)* |
| A descriptive table | `DESCRIPTIVE` + `OBJECTS` + `RENDER` |
| Re-estimated on the cluster | `FIGURES` + `OBJECTS` + `RENDER` |
| The harmonized data | `DATA` + `MATCHING` + `TREATMENT`, then 004 on the cluster, then `DESCRIPTIVE` + `FIGURES` + `OBJECTS` + `RENDER` |
| Sending tables to a co-author | `WORKBOOK` (after 100 and 101 are current) |

Each stage runs in its own environment, so the `rm(list = ls(all = TRUE))` at
the top of the older scripts cannot delete the runner.

Scripts can also be run one at a time — `Rscript scripts/301_article_objects.R`
— from the study root or from the okwaayeli monorepo root. `_paths.R` resolves
either.

## What depends on what

```
001 ──> 002 ──> 003 ──> 101 ──┐
                 └──> 004 ────┤
001 ──> 100 ─────────────────┬┴──> 301 ──> 302 (.docx/.html)
                             │            └─> 303 (.tex/.pdf)
                             └──> 102 (.xlsx)
```

102 needs 100 and 101 for the same reason the Rmd does: it calls the same
`ft_*()` builders, and those read the descriptive cache and the figure data.

## The files

### Libraries

**`_paths.R`** — where the study lives, and every path derived from it.

Resolves `PROJECT_ROOT` from its own file location (or `LAND_TENURE_ROOT`, or a
search from the working directory) and defines `STUDY`, `DATA`, `OUTPUT`,
`FIGURE`, `TABLES`, `NARRATIVE`, `SCRIPTS`, `SE_RDS`, `OBJECTS_JSON` — all
absolute, so they survive knitr moving the working directory mid-render.

Three functions matter:

- `study_env()` — load the saved study environment with its `$wd` rebased onto
  this checkout
- `rebase_wd(se)` — do that rebasing to an environment you loaded or recomputed
- `okwaayeli_load()` — `devtools::load_all(".")` inside the package repo,
  `library(okwaayeli)` otherwise

This file is what de-couples the project from the okwaayeli monorepo. The
pipeline was written with this study at `<repo>/studies/land_tenure/`, and both
`study_dirs()` and the saved `.rds` bake that in. Rather than fork the package
or rewrite an `.rds` the cluster and co-authors share, the real root is
resolved here and everything rebased onto it at load time.

**`article_helpers.R`** — the article layer. Sources `_paths.R`, then defines
`fmt_num()`, `fmt_pct()`, `fmt_abs_pct()` (sprintf-based, so inline text matches
the tables' cells exactly) and `assert_present()`, which stops a knit rather
than letting a missing number render as `NA`. Sourced by 301, 302 and the Rmd.

**`exhibit_helpers_tables.R`** — every `ft_*()` flextable builder in the paper,
plus the inline lookups (`fig1_range()`, `trend_gap()`) that read the figure
data. Sourced by the Rmd at knit time and by 102. Resolves its own paths through
`.STUDY_ROOT` because it is called both from the project root and from
`narrative/` mid-knit.

### Analysis (`0xx`)

**`000_initialize.R`** — creates the `output/` tree, sets `scipen` and the seed.
Delegates the folder names to `study_dirs()`; do not list directories here. A
second copy of the names drifts from the first, and the failure is silent — the
tree looks right while every write lands elsewhere.

**`001_DATA_land_tenure_study.R`** — merges the harmonized farmer file with the
land tenure module on `Surveyx/EaId/HhId/Mid`, restricts to GLSS3–GLSS7, adds
`CensusFrame` and `TrendSample`, saves
`data/land_tenure_study_environment.rds`.

The census sampling frame block in this file is substantive, not housekeeping.
GSS re-based the frame twice (1984 census → GLSS3/4, 2000 PHC → GLSS5, 2010 PHC
→ GLSS6/7), and measured ownership jumps 31.7% → 79.1% across the GLSS4/GLSS5
break in two independently administered questions. Each wave stays internally
representative of its own frame, so all five are kept in the pooled estimation,
but temporal claims are restricted to GLSS5–GLSS7. See
`narrative/sections/05_results.Rmd`.

**`002_MATCHING_land_tenure_study.R`** — defines the treatment indicator
(`OwnLnd`), the exact / scalar / factor covariate sets, draws 100 matching
specifications, runs each to `output/matching/match_NNNN.rds`, scores covariate
balance and ranks the specifications. Attaches `estimation_data` to the study
environment. **Expensive.**

**`003_TREATMENT_land_tenure_study.R`** — ATE / ATET / ATEU over the seven input
and output variables for every matching specification →
`output/treatment_effects/te_NNNN.rds`, summarised into `te_summary.rds`.
**Expensive.**

**`004_MSF_land_tenure_study.R`** — the meta-stochastic frontier estimation.
Builds the specification grid across functional forms, distributions,
disaggregation levels and the six technology variables, then fits each one.
Reads `SLURM_ARRAY_TASK_ID` and estimates only that row when present. **Hours to
days; normally a cluster job.**

**`job_msf.sbatch`** — the SLURM submission for 004. `--array=1-60`, 28 GB,
50-hour wall clock. If you edit `technology_variables`, update the array size to
match `nrow(model_specifications)` or the tail specifications silently never
run.

**`surgery.R`** — a debugging harness, not a stage. 004's `lapply` body unrolled
into the global environment with the `tryCatch()`s removed, so a failing
frontier surfaces its actual error and leaves its intermediates behind for
inspection. `RUN_TO` controls how far it goes. Not called by `run_article.R`.

### Exhibits (`1xx`)

**`100_exhibit_descriptive_stats.R`** — Tables 1, 2 and S1–S4, cached to
`data/descriptive_exhibits.rds`. Separate from the knit because it fits a model
per treatment × crop × outcome; far too slow to repeat every render.

Tables 2 and S1–S4 are GLSS6–GLSS7 only. That is a **comparability**
restriction, not an availability one — all five rounds administer all four
items. See the header for the per-variable reasoning.

**`101_exhibit_figures.R`** — every `output/figures/*.png` plus the `.csv`/`.rds`
behind each. Must run **before** a knit: the table helpers read the figure data
for inline lookups.

**`102_exhibit_table_workbook.R`** — every table *as printed* (stars, jackknife
SEs, `sprintf` rounding, headers, spanners, footnotes) to
`output/tables/land_tenure_tables.xlsx`, one sheet each. A deliverable for
co-authors. **Not a round trip** — nothing reads it back, and nothing should.

### Manuscript (`3xx`)

**`301_article_objects.R`** — extracts every number the prose cites from the
same estimation objects the figures use, and writes
`narrative/article_objects.json`. This is what stops the text and the exhibits
from drifting apart.

Keying is easy to get wrong. `TCHLvel` identifies the frontier — `"National"`,
`"0"`, `"1"`, `"Meta"`. `Tech` is an analysis label only and must **not** be
used to split groups: it carries a different coding for the same concept and
silently transposes them. The aggregate ownership comparison uses `OwnLnd`
alone, deliberately — `LndOwn`/`LndRgt` key `1` as the reference category, so
the binary coding would return a real number meaning its opposite.

**`302_render_article.R`** — the main render:
`narrative/output/land-tenure.docx` and `.html`.

**`303_render_tex.R`** — the LaTeX export. Knits each section separately,
concatenates, runs citeproc **once** across the whole manuscript, then splits
back apart. That is how you get both a compilable `main.tex` and per-section
`.tex` files without the reference list duplicating into every citing section.
Writes `narrative/output/main.tex`, `output/sections_tex/*.tex` and a compiled
`main.pdf`. Not called by `run_article.R`.

**`304_preview_section.R`** — a reading copy of one section while it is being
drafted:

```
Rscript scripts/304_preview_section.R 05_results
```

Re-knits the section rather than reusing 303's fragment, so what you read is
what the `.Rmd` currently says — a preview showing pre-edit text is the one
failure mode a preview must not have. Writes
`narrative/output/_preview_<id>.tex` and `.pdf`.

**`run_article.R`** — the entry point. See *Running it* above.

## Things that will bite

**`DATA = TRUE` requires `MATCHING = TRUE`.** 001 saves a fresh study
environment holding `study_raw_data` but **not** `estimation_data` — only 002
attaches that. Running 001 alone silently strips `estimation_data` from the
`.rds`, and everything downstream then fails or quietly reads nothing.
`run_article.R` refuses this combination outright.

**302 and 303 cannot run at the same time.** Both write intermediates into
`narrative/`. Run them sequentially.

**Citation style is set in four places and they must agree.** The target style
is **IEEE** (numbered). It is the default in `run_article.R`
(`CITATION_STYLE <- "ieee"`), in 302, in 304, and in the master Rmd's own YAML
fallback. Change one without the others and the same manuscript ships with
different reference formatting depending on how it was built — no warning either
way.

**In-text citations are written as `Author [-@Key]`, deliberately.** IEEE is
numbered, so a bare `@Fenske2011` renders as "[12] finds the link…" — the author
name vanishes and the sentence breaks. `[-@Key]` suppresses the author from the
rendered citation and the name is spelled out in the prose, giving
"Fenske [12] finds…" under IEEE and "Fenske (2011) finds…" under an author-date
style. There are 16 of these, in sections 01, 03, 04 and 05. Do not "simplify"
them back to `@Key`. Bracketed citations (`[@a; @b]`) are unaffected and stay as
they are.

**`$wd` in the saved `.rds` is a snapshot** frozen by whichever run last called
`study_setup()`. Stages that read it should call
`rebase_wd(study_dirs(se, layout = "v2"))` rather than trusting it — 101 and 102
do. This study is on the `v2` layout (plots and their data share
`output/figures/`, table data goes to `output/tables/`); the sibling studies are
on `legacy`, which is why it is a parameter rather than a rename.

**Reach for the accessors, not directory literals.** `study_dir_figures()`,
`study_dir_figure_data()`, `study_dir_tables()` — never a path constructed next
to `wd$output`.

**`narrative/` is not under version control.** The git repository root is
`Manuscripts/`, and it does not track this study's narrative sources. Deletions
are recoverable only through the Recycle Bin or Nextcloud version history.

## The package

The analysis functions — `study_setup()`, `draw_matched_samples()`,
`treatment_effect_calculation()`, `draw_msf_estimations()`,
`draw_msf_summary()`, the figure builders — live in
[okwaayeli](https://github.com/ftsiboe/okwaayeli), not here. Install it with:

```r
install.packages("remotes")
remotes::install_github("ftsiboe/okwaayeli")
```

`okwaayeli_load()` picks up the working tree inside the package's own repo (so
edits apply without reinstalling) and the installed copy everywhere else.
