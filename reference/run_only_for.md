# Restrict Execution of an R Script to Allowed SLURM Job Conditions

`run_only_for()` controls when an R script is allowed to run based on:

- the SLURM array index (`SLURM_ARRAY_TASK_ID`)

- the SLURM job name (`SLURM_JOB_NAME`)

- whether the script is being run locally on Windows

The function is designed for workflows where multiple R scripts run in a
single SLURM array job; each script decides independently whether it
should run.

## Usage

``` r
run_only_for(id = NULL, run_on_windows = TRUE, allowed_jobnames = NULL)
```

## Arguments

- id:

  Integer or `NULL`. Expected SLURM array index, or `NULL` to skip
  checking (useful for Windows/local runs).

- run_on_windows:

  Logical. When `TRUE` (default), do not restrict when running on
  Windows.

- allowed_jobnames:

  Character vector of SLURM job names allowed to run this script. If
  `NULL`, job-name filtering is skipped.

## Value

Invisibly returns `NULL` when allowed; otherwise exits R.

## Details

Execution logic:

1.  **Windows override** If running on Windows and
    `run_on_windows = TRUE`, return immediately.

2.  **Allowed job names** If `allowed_jobnames` is set, the script runs
    only when SLURM job name is in that vector.

3.  **Array ID restriction**

    - If `id = NULL`, skip array-index filtering.

    - If numeric, match against `SLURM_ARRAY_TASK_ID`.

Any violation -\> `quit(save = "no")`.
