# Rebuild handoff — `12_time_poverty.do`

**Date:** 2026-08-09 · **Status:** executed in Stata 16.1 and diffed against an independent rebuild — see Run log

---

## What was done

`12_time_poverty.do` no longer opens `$TPAssets\TimeGLSS7.dta`. It builds its own person-level input
directly from the GSS-published GLSS microdata under `$DATABASE`. The co-author dependency is gone.

Provenance of the old file, established by diffing it against the raw GLSS7 files:

| block | source | match |
|---|---|---|
| spine, 59,864 persons keyed `clust nh pid` | `g7sec1.dta` | identical key set |
| Section-1 demographics | `g7sec1.dta` | exact |
| `s4aq*`, `sex`, `age`, `s4achk`, `s4aqpid` | `g7sec4.dta` | **~1% of cells differ** |
| `hhsize`, `loc2`, `district` | `g7loc_upd.dta` | exact |
| `_merge` | leftover from the last merge | no meaning |

The ~1% discrepancy (663 cells in `s4aq7`, 2,216 in `age`, 1,196 in `s4aqpid`), with values shifted between
adjacent `pid`s inside a household, means the co-author's file came from a different GSS vintage or carried
hand edits. `g7sec4-reviewed.dta` is a worse match, not a better one. **Exact reproduction is not
achievable**, which is why this is a clean rebuild rather than a bug-compatible one.

---

## The finding that reshaped the job

GLSS5 and GLSS6 carry a full unpaid domestic and care module that GLSS7 does not:

- **GLSS6 `SEC4h.dta`** (62,287 rows) — 14 activities × hours and minutes, including childcare, elderly
  care and care of the sick. Section 4 is titled "Employment **and Time Use**"; Part H asks "How much time
  **in the last 7 days**", the same window as the employment hours, so the two are directly additive.
- **GLSS5 `sec4h.dta`** (29,366 rows) — 13 activities × hours and minutes.
- **GLSS7** — none. Confirmed by scanning all 205 columns of `g7sec4.dta`; GSS dropped the module.
- **GLSS4 `SEC4J.DTA`** — has the housekeeping hours but its employment module is **weeks**-based with no
  hours variable anywhere in Section 4, so no comparable measure can be formed. GLSS3 likewise. Both
  excluded. GLSS8 microdata is absent from `$DATABASE`.

So the script is **two-tier**, and the tiers must never be pooled silently:

| `tp_basis` | waves | committed time |
|---|---|---|
| `econ_plus_care` | GLSS5, GLSS6 | total hours in all jobs + housekeeping and care hours |
| `econ_only` | GLSS7 | the eight economic-activity items only — **not** time poverty |

---

## Three label traps, all verified against the questionnaire and the data

These are the reason the old measure pointed the wrong way. Do not "fix" them back.

| variable | .dta label | what it actually is | evidence |
|---|---|---|---|
| GLSS7 `s4aq5` | "hours worked for wage/salary" | follow-up to `s4aq4` — hours as a **paid domestic worker** | non-missing for 249 of 59,864 people (0.42%); if it were housework most adult women would be positive |
| GLSS7 `s4aq16` | renamed "non-productive agriculture" | "catch fish, prawns, wildlife or collect natural products" — own-use production of goods, inside the SNA boundary | `s4aq15` gate wording |
| **GLSS6 `s4aq5`** | "Number of jobs done" | **total hours worked across all jobs, last 7 days** (questionnaire Part 4A col. 5) | median 40, p95 78, max 120; ≥ `s4aq9` (main-job hours) for **100%** of respondents; `s4aq4` is the job count (1–5) |

The same structure holds in GLSS5: `s4aq3` is the total, `s4aq7` the main job. **Adding the two double
counts** — that is what produced an earlier read of 57 paid hours/week. The script uses the total only.

---

## Expected output

Adults 15+, module respondents, weighted. **Run the do-file and check against these.**

| wave | basis | base n | weighted median h/wk | 1.25× | 1.50× | 2.00× | WEAI 73.5 h/wk |
|---|---|---|---|---|---|---|---|
| GLSS5 | `econ_plus_care` | 21,624 | 48.3 | 0.357 | 0.237 | 0.099 | 0.229 |
| GLSS6 | `econ_plus_care` | 42,585 | 43.0 | 0.366 | 0.239 | 0.072 | 0.156 |
| GLSS7 | `econ_only` | 22,213 | 40.0 | 0.225 | 0.092 | 0.019 | 0.029 |

Gender split on the 1.5× line — the headline diagnostic:

| wave | men | women |
|---|---|---|
| GLSS5 | 0.143 | **0.320** |
| GLSS6 | 0.196 | **0.276** |
| GLSS7 (`econ_only`) | **0.110** | 0.075 ← inverted |

Mean care hours per week: GLSS5 men 10.5 / women 31.9; GLSS6 men 5.7 / women 17.9.

Joining to `harmonized_crop_farmer_data` (100% key match on all three waves):

| wave | farmer-members | usable |
|---|---|---|
| GLSS5 | 5,461 | 5,455 (99.9%) |
| GLSS6 | 9,671 | 9,665 (99.9%) |
| GLSS7 | 6,688 | 5,652 (84.5%) |
| **total** | | **20,772** |

Against the old release's 6,331, all household heads. The GLSS7 figure is *lower* than before because
module non-respondents are no longer zero-filled — that is the fix, not a regression.

---

## What you must change downstream

1. **`001_DATA_time_poverty_study.R` line 60** still filters `Surveyx %in% c("GLSS7")` (its comment claims
   GLSS6 and GLSS7). Widen it to the waves you want.
2. **Carry `tp_basis` into every model.** `econ_only` and `econ_plus_care` are different constructs with
   opposite gender gradients. Interact it with the wave, or estimate the tiers separately.
3. **`002_MATCHING` line 58** sets `Treat` from `tpoor0150`. That still works, and now means something —
   but on GLSS5/6 only. Decide whether GLSS7 belongs in the treatment sample at all.
4. **`004_MSF` line 83** passes `technology_variables = c("tpoor0150","tpoor0125")`. Both are now intact
   (the old code blanked `tpoor0125` wherever it disagreed with `tpoor0150`, making the "robustness" run a
   subsample copy of the main run). `tpoor0200`, `tpoorweai`, `tpgap` and `tpsev` are also available.
5. **The release schema changed.** New: `tp_basis`, `econ_hw`, `care_hw`, `UnpaidTime`, `has_econ`,
   `has_care`, `valid_tp`, `tp_base`, `CommMedian`, `Cutoff200`, `tpoor0200`, `tpgap`, `tpsev`, `Female`,
   `Head`, `AgeYrTP`, `region`, `district`, `loc2`, `ez`, `month`, `year`, `weight`. Gone: the `Day*`
   per-activity variables, `UPTimPov*`, `CommTimeWEAI`.
6. **`weight` is now in the release.** Use it. And `month`/`year` let you finally control for interview
   season, which matters given Martey et al. (2024) find Ghanaian farm time poverty is strongly seasonal.

---

## Run log — 2026-08-09

The script was executed in Stata 16.1 and **reproduced every expected figure**: medians 48.33 / 43.00 /
40.00, base sizes 21,624 / 42,585 / 22,213, both guards passed. The independent Python rebuild then diffed
the output row by row: 169,364 of 169,364 keys matched, with **zero** mismatches on `CommTime`,
`tpoor0125`, `tpoorweai` and `CommMedian`.

Two issues surfaced and are fixed in the current version:

**1. Float knife-edge at the threshold (39 rows, all GLSS5).** `_pctile` returned GLSS5's median from a
*float* variable as 48.333332, so the 1.5× cutoff was 72.49999809 rather than 72.5. Thirty-nine
respondents sit at exactly 72.5 h/week, so `72.5 > 72.49999809` made them time poor in Stata while the
reference said not. Every hours figure here is a multiple of 1/60, which binary floating point cannot
represent, so anyone sitting exactly on the line lands on whichever side the rounding error falls — and
that answer can change across Stata builds. **The threshold arithmetic is now done in whole minutes**,
which is exact: `CommMn` is an integer, the median is in minutes, and the cutoffs are exact multiples.
Hours are derived afterwards for reporting only. The validator was changed to match. Convention at an exact
tie is Bardasi–Wodon's: committed time must *exceed* the line, so a person exactly on it is **not** time
poor.

**2. `r(608)` on save.** The target `.dta` was locked; the directory was not (the fallback wrote a 19.7 MB
file into the same folder at the same instant). The script now pre-flights directory writability up front
and, if the target file itself is locked, parks the result as
`harmonized_time_poverty_data_PENDING_<date>.dta` and exits non-zero rather than discarding the build.

## What I could not verify

The do-file has now been run in Stata 16.1 (see Run log above) and its output diffed row by row against the
Python rebuild, so the earlier "not executed" caveat is discharged. Residual notes:

- **Syntax.** Reviewed by inspection; two real bugs were caught and fixed before shipping (`levelsof` on a
  string variable needs `clean` or the values arrive double-quoted; `r(N)` was being read after `_pctile`
  had already clobbered `r()`). `version 12` was removed because it would block `saveold`'s `version()`
  option, which was added in Stata 13.
- **Semantics.** `_validate_12_time_poverty.py` is shipped alongside for exactly this. Run:

  ```
  python _validate_12_time_poverty.py --gss "C:/Users/ftsib/OneDrive/Research/Database/Ghana/Surveys/Database/GLSS/datasets/GSS" \
                                      --release "<repo>/data-raw/releases/harmonized_data/harmonized_time_poverty_data.dta"
  ```

  It rebuilds the reference independently and diffs it against whatever the do-file wrote, cell by cell, on
  `CommTime`, `tpoor0150`, `tpoor0125`, `tpoorweai` and `CommMedian`. Any mismatch is a Stata bug.

The script also carries two hard guards that abort rather than ship a degenerate measure: it exits if any
wave's weighted median committed time is ≤ 0 (the exact failure mode of the old code), and if the threshold
base falls below 50,000 observations.

---

## Open questions worth settling before the manuscript

1. **The GLSS Section 4H recall window.** The questionnaire says "in the last 7 days", but per-activity
   hours top out at 7–9 with a visible spike at 8 for cooking, which is consistent with some respondents
   answering per day. Women's total care time of 17.9 h/week in GLSS6 is well below the 5h31/day Charmes
   (2006) reports for Ghana from GLSS3/4. Worth a paragraph either way — it bounds how much of the true
   burden the measure captures, and it biases toward zero.
2. **Whether GLSS7 stays in at all.** It cannot measure the construct the paper is about. Keeping it buys
   recency and the fine-grained activity battery; dropping it buys one clean construct throughout.
3. **The 10.5-hour WEAI rule** is defined on a 24-hour diary. Applied to a 7-day window as 73.5 h/week it
   is a related but different construct. Say so rather than calling it WEAI.
4. **`$TPAssets`** — `PaidTimepoverty.dta` and `UnpaidTimepoverty.dta` are now orphaned. `TimeGLSS7.dta`
   should be kept for provenance but is no longer read by anything.
