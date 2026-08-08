# `data-raw/scripts/data-prep/glss`

Every GLSS harmonization script in the project. One file per release, plus a
master that runs them in order.

Each script reads the raw GLSS survey files and writes one `.dta` into
`data-raw/releases/harmonized_data/`. That `.dta` is what the study pipelines
read — no study reaches back to the raw survey files.

## Running

```stata
do data-raw/scripts/data-prep/glss/00_run_all.do           // everything
do data-raw/scripts/data-prep/glss/00_run_all.do 11        // just ag services
do data-raw/scripts/data-prep/glss/00_run_all.do 01 02 03  // a subset
```

Any script also runs on its own (`do data-raw/scripts/data-prep/glss/07_resource_extraction.do`).
Both entry points work from the repo root or from this folder.

**A full run rewrites every release.** Pass a subset when you only meant to
rebuild one.

## The files

| # | Script | Writes |
|---|---|---|
| — | `_paths.do` | nothing — shared globals and path guards, run by every script |
| — | `00_run_all.do` | nothing — the master |
| 01 | `01_crop_farmer.do` | `harmonized_crop_farmer_data` |
| 02 | `02_income_transfer.do` | `harmonized_income_transfer_data` |
| 03 | `03_financial_inclusion.do` | `harmonized_financial_inclusion_data` |
| 04 | `04_nonfarm_enterprise.do` | `harmonized_nonfarm_enterprise_data` |
| 05 | `05_offfarm_work.do` | **nothing** — `saveold` is commented out; not in the default run list |
| 06 | `06_education.do` | `harmonized_education_data` |
| 07 | `07_resource_extraction.do` | `harmonized_resources_extraction_data` |
| 08 | `08_disability.do` | `harmonized_disability_data` |
| 09 | `09_societal_peace_and_cohesion.do` | `harmonized_societal_peace_and_cohesion_data` |
| 10 | `10_land_tenure.do` | `harmonized_land_tenure_data` |
| 11 | `11_ag_services.do` | `harmonized_ag_services_data` |
| 12 | `12_time_poverty.do` | `harmonized_time_poverty_data` |

## Order matters, once

`02` and `03` read `harmonized_crop_farmer_data.dta` back from
`$LabGitHub`, so `01` has to have written it. Both stop with a clear message if
it is not there. Everything else is independent, but the numbering is stable so
a subset argument means the same thing tomorrow.

## Paths

`_paths.do` is the single place the drive letters and checkout location live.
It defines `$DATABASE`, `$COLLATED`, `$Supplementaries` (sources), `$REPO`,
`$GLSS` and `$LabGitHub` (this repo), and refuses to continue if `$LabGitHub`
or `$DATABASE/GLSS` does not resolve. A wrong `$LabGitHub` is the failure that
hurts most: `saveold` happily writes into the wrong checkout, the run reports
success, and the study scripts go on reading a stale release.

## Two scripts also have a study-side caller

- `11_ag_services.do` is called by `studies/ag_services/scripts/001_DATA_ag_services_study.R`
  where Stata is available. `001` scans this script's log for `^r(NNN);`,
  because Stata batch mode exits 0 even on error — which is why the script
  opens a *named* log, so it behaves the same standalone and under the master.
- `12_time_poverty.do` reads its `.dta` inputs from
  `studies/time_poverty/time-poverty-assets/` through `$TPAssets`. Only the
  script moved here; the inputs stayed with the study. Read the flag at the head
  of that file before trusting `tpoor0150`.
