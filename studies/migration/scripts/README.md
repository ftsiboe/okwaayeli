# scripts/

Same conventions as `studies/land_tenure/scripts/README.md`: a **number** is a
position in a sequence, an **unnumbered** file is a library.

| File | What it does |
|---|---|
| `_paths.R` | Resolves the study root (`MIGRATION_ROOT` overrides) and defines `study_env()`, `rebase_wd()`, `okwaayeli_load()` |
| `000_initialize.R` | Creates the `output/` tree via `study_dirs(layout = "v2")` |
| `001_DATA_migration_study.R` | Farmer × migration merge, GLSS3–7, rural only; builds `MigUrban`, `MigStatus`, `MigUrbanType`, `RemitUrban` and the heterogeneity variables |
| `002_MATCHING_migration_study.R` | `Treat = MigUrban`; 100 matching specifications; covariate balance |
| `003_TREATMENT_migration_study.R` | Treatment effects on area, output and inputs |
| `004_MSF_migration_study.R` | Meta-stochastic frontier per specification (SLURM array) |
| `job_msf.sbatch` | Submits 004 |
| `run_article.R` | Stage runner for 000–004 |

## Running

From the okwaayeli repo root:

1. Build the release in Stata: `do data-raw/data-prep/glss/00_run_all.do 13`
2. Data + matching + treatment effects, locally (`run_article.R` with
   `INITIALIZE`, `DATA_PREP`, `MATCHING`, `TREATMENT` on) or on the cluster:
   `sbatch --array=10 studies/run_data_and_match_for_all.sbatch`
3. Frontier on the cluster: `sbatch studies/migration/scripts/job_msf.sbatch`

## Before submitting 004

`job_msf.sbatch` carries a **placeholder** `--array=1-60`. Set it to the number
of model specifications once 002 has saved the environment:

```r
source("studies/migration/scripts/_paths.R"); okwaayeli_load()
se <- study_env()
nrow(sf_model_specifications(data = se$estimation_data,
     technology_variables = c("MigUrban","MigStatus","MigUrbanType","RemitUrban")))
```

## Guards (inherited from land_tenure)

- 001 re-saves the environment **without** `estimation_data`; only 002 attaches
  it. Never run 001 without 002.
- The cluster task id is **10** in both `run_only_for()` (001–003) and
  `studies/run_data_and_match_for_all.sbatch`. Change one, change both.
