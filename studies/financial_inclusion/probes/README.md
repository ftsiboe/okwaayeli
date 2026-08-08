# `probes/`

Read-only diagnostic scripts. Nothing here is part of the pipeline: no numbered
prefix, not driven by `run_article.R`, and no writes outside `probes/logs/`.

## Why

Every wrong guess in this study's build had one shape — a name or a level
inferred from the v005 draft's prose instead of read from the data:

| Inferred | Actual |
|---|---|
| `NoAccRsn` | `NonBanked_Why` |
| `LoanPurp` | `Use` |
| `RejRsn` | `Refusal` |
| *(missed)* | `Bank_Info` |
| "no counterpart" | `WhyNoLoan_1..5` |
| `Tech == 1` is the credit group | `Tech == 1` is the **no-credit** group |

Each cost a round trip. Dumping the whole surface once is cheaper than
discovering it a column at a time.

## Contents

- `probe_exhibits.R` — everything the exhibit and narrative layers key against:
  file inventory, study environment, descriptive-cache schema and level sets,
  all four estimation frames per technology variable, the `Tech` x `TCHLvel`
  cross-tabs, parity spot-checks against the cells v005 prints, treatment-effect
  summary, figure data, and narrative state.

## Running

From the repo root:

```r
source("studies/financial_inclusion/probes/probe_exhibits.R")
```

or

```
Rscript studies/financial_inclusion/probes/probe_exhibits.R
```

## Output

`logs/probe_exhibits.log` — full transcript.
`logs/probe_exhibits.json` — the same, machine-readable.

Both are overwritten on each run and are gitignored (`studies/**/logs/`).
The log is the artifact to send when asking someone to write a builder.
