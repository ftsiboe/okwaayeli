# Fidelity review — `data-raw/data-prep/glss/12_time_poverty.do`

**Reviewed:** 2026-08-09 · script mtime 2026-08-08 · release `harmonized_time_poverty_data.dta` (14,009 × 28)
**Method:** line-by-line read, then a full re-implementation of the script's logic in Python against the
actual input (`TimeGLSS7.dta`, 59,864 × 32) and verification against the shipped release. Every number
below is reproduced from the data, not inferred from the code.
**Scope:** investigation and report only. No files were changed.

---

## CORRECTION — issued 2026-08-09, after first publication

**I got one thing wrong in the first version of this report, and it is consequential.** I wrote that no
unpaid domestic or care time module exists in GLSS4–GLSS7. That is wrong for GLSS4, GLSS5 and GLSS6. I had
checked those waves' Section 1 and GLSS6's `SEC4hs.dta` (which is occupational **health and safety**) and
concluded from their absence there. I did not check `SEC4h.dta`. The module exists:

| wave | file | content |
|---|---|---|
| GLSS3 | `S4J.DTA` / `S4K.DTA` | unlabelled in the archive; almost certainly the housekeeping module Charmes (2006) analysed — needs the questionnaire to map |
| GLSS4 | **`SEC4J.DTA`** (20,654 rows) | 11 activities, participation + hours: fetching wood, fetching water, ironing, caring for children, washing vehicles, sweeping, disposing garbage, cooking, shopping, running errands, washing dishes, other housekeeping |
| GLSS5 | **`sec4h.dta`** (29,366 rows) | 13 activities × (hours, minutes); also carries `weight` and `ez` |
| GLSS6 | **`SEC4h.dta`** (62,287 rows) | 14 activities × (hours, minutes): firewood, water, washing clothes, ironing, cleaning, cooking, shopping, errands, dishes, **childcare, elderly care, care of the sick**, collecting food from garden, helping children with schoolwork |
| GLSS7 | — | **none.** Confirmed by an exhaustive scan of all 205 columns of `g7sec4.dta` (Sections 4A–4F). GSS dropped the module. |

**What this changes.** The conclusion that the GLSS7 pipeline contains no unpaid domestic or care work stands
— GLSS7 genuinely lacks it. But the conclusion that Ghana's living-standards surveys cannot support a
genuine paid-plus-unpaid time-poverty measure was wrong. **GLSS5 and GLSS6 can.** The study is currently
built on the one recent wave that cannot.

Verified on the raw files, adults 15+ with a Section 4H record (unweighted, GLSS6 n=42,531; GLSS5 n=21,572):

| | GLSS6 men | GLSS6 women | GLSS5 men | GLSS5 women |
|---|---|---|---|---|
| paid hours/week | 33.2 | 28.4 | 61.7 | 52.9 |
| **unpaid hours/week** | **5.7** | **17.9** | **10.5** | **32.0** |
| total | 38.9 | 46.2 | 72.3 | 85.0 |
| time poor, 1.5× median **total** | 18.0% | **28.5%** | 19.6% | **27.4%** |
| time poor, 1.5× median **paid only** (current `tpoor0150` logic) | **33.7%** | 25.4% | — | — |

Adding the unpaid module **flips the gender gradient** — women 58% more likely to be time poor in GLSS6,
against men 33% more likely under the paid-only rule. That is precisely the reversal Bardasi & Wodon (2010)
describe, reproduced on Ghanaian data. Everything in §3 below about the *direction* of the current measure
is confirmed; the remedy in §3 is superseded by this correction.

*(Reference-period caveat: GLSS6 `s4hq*h` values top out at 7–8 hours per activity, so the module's recall
window needs confirming against the questionnaire before these magnitudes are quoted. The direction of the
gender gap is not sensitive to that.)*

---

## 0. Verdict in one paragraph

The script runs, and the release it produces is internally consistent with the code. But the variable it
ships as the study's treatment — `tpoor0150`, consumed by `002_MATCHING` as `Treat` and by `004_MSF` as the
meta-frontier technology group — **is not a time-poverty measure**. It is a relative cutoff on *paid* work
hours only, computed on household heads, from a module that contains **no household chores, no cooking, no
water or fuelwood collection, and no childcare or elder care**. In the analysis sample it classifies
**26.1% of men and 18.1% of women as "time poor."** The study's own abstract says the opposite is the point
("unpaid domestic and care work absorbs a disproportionate share of women's time"). Fixing the overwrite bug
flagged in the file header does not repair this: the underlying GLSS7 section-4A battery has no unpaid
domestic or care component to restore.

---

## 1. What the script is measuring — the input module

`TimeGLSS7.dta` is built from **GLSS7 Section 4A**, which is the *employment* module. All eight questions are
gates on economic activity in the past 7 days. Verified variable labels from `g7sec4.dta`:

| do-file name | GLSS7 var | actual question (label) | non-missing in file |
|---|---|---|---|
| `Wage_Salary` | s4aq3 | hours worked for wage/salary | 5,458 (9.1%) |
| `Domestic` | s4aq5 | hours worked **as a domestic worker for a wage/salary** (gate = s4aq4) | **249 (0.42%)** |
| `FarmEnt` | s4aq7 | hours on this farm or other agricultural activity | 13,551 (22.6%) |
| `NonFarmEnt` | s4aq10 | hours running/managing a non-farm enterprise | 6,677 (11.2%) |
| `FHNonFarmEnt` | s4aq13 | hours helping in a non-farm enterprise | 1,527 (2.6%) |
| `NonPdtAgric` | s4aq16 | hours **catching fish/prawns/wildlife or collecting other natural products** (gate = s4aq15) | 636 (1.1%) |
| `Apprentice` | s4aq18 | hours worked as an apprentice | 903 (1.5%) |
| `Voluntary` | s4aq20 | hours of voluntary work for a non-household member | 960 (1.6%) |

### 1.1 `Domestic` is paid domestic service, not housework — **[CRITICAL]**

s4aq4 asks whether the person "worked **as domestic worker for a wage, salary**…". s4aq5 is the hours for
that job. The script renames it `Domestic` and places it in `UnpaidTime`. The data settle it: **0.42% of
individuals** have any value. If this were household chores, most adult women would be positive. In the
crop-farmer analysis sample, mean `DayDomestic` is **0.018 h/day** and only **0.49%** are non-zero.

The consequence is that `UnpaidTime` — the variable named for the study's central construct — is
`paid domestic servants + fishing/gathering + apprenticeship + volunteering`. In the analysis sample it is
**3.8% of total committed time**. There is no unpaid household or care work in this pipeline at all.

This is also worth flagging against the literature: the Martey *et al.* GLSS papers describe their unpaid
component as "domestic work, non-productive agriculture, apprenticeship and voluntary activities," which
maps onto exactly these variables. If those papers used the same mapping, the same critique applies to them.
Worth verifying against their replication files before citing their unpaid-work results.

### 1.2 `NonPdtAgric` is mislabelled

s4aq15/16 is fishing, hunting and gathering of natural products — own-use production of *goods*, squarely
inside the SNA production boundary (Charmes, in Blackden & Wodon 2006, is explicit that collection
activities have been inside the boundary since SNA 1968). Calling it "non-productive agriculture" and
placing it on the unpaid side is a naming error, though placing own-consumption production in "unpaid" is at
least arguable.

---

## 2. Critical errors, in order of severity

### E1 — The shipped treatment is a **paid-hours** cutoff, not committed time — **[CRITICAL]**

Line 92 saves `PaidTimepoverty.dta` with `TimPov125`/`TimPov15` built on `CommTime` (all 8 activities).
Lines 97–111 then `drop` those, restrict the sample, rebuild `TimPov125`/`TimPov15` on **`PaidTime`**
(= wage + farm + non-farm + help-in-non-farm), and **overwrite the same file**. Lines 149–150 set
`tpoor0150 = TimPov15` and `tpoor0125 = TimPov125` — i.e. the paid-hours versions. The header comment
already flags this; the point of this review is that it is not a bookkeeping slip, it changes what the paper
is about.

Verified in the release: `tpoor0150` mean **0.2615**, exactly `PaidTime > 1.5 × median(PaidTime|heads)`
(= 6.43 h/day = 45 h/week).

### E2 — `CommTime` median is **zero**, so the committed-time measure degenerates — **[CRITICAL, latent]**

`sum CommTime, detail` runs over all **59,864** rows, of which **55.5% have `CommTime == 0`** (children aged
5–14 are 34.5% of the file; non-workers make up the rest, and the merge that built `TimeGLSS7` brought in
8,569 people never administered section 4A, whose missings `recode (.=0)` turns into zeros).

Therefore `median = 0`, `Cutoff125 = Cutoff15 = 0`, and both indicators collapse to

```
TimPov125 = TimPov15 = (CommTime > 0)   →  headcount 0.4452 for both
```

which is simply *"did any economic activity in the past 7 days."* This is what gets written to
`PaidTimepoverty.dta` at line 92. It is overwritten later, so it does not reach the release — but anyone who
"fixes" the overwrite bug without also fixing the median base will silently ship an employment dummy
labelled as time poverty.

The identical failure **does** reach the release on the unpaid side. Among heads, **93.7% have
`UnpaidTime == 0`**, so `median2 = 0` and:

```
UPTimPov125 = UPTimPov15 = (UnpaidTime > 0)   →  both exactly 0.0626 in the shipped file
```

Two nominally different thresholds are byte-identical. That is confirmed in the release.

### E3 — `keep if s1q3==1` restricts everything downstream to household heads — **[HIGH]**

Line 98 is placed to serve the paid-time block, but it persists through the unpaid block, the WEAI block and
the save. The release is **14,009 rows = exactly one per household**, 68.8% male.

Practical impact on this study is smaller than it looks, because `harmonized_crop_farmer_data` is already at
farm-operator level: the inner join retains **6,331 of 6,688** GLSS7 farmer-members (94.7%). But the 357
dropped are precisely the wrong ones — 200 of them are spouse-farmers, **91.5% female**. Female share of the
analysis sample falls from 25.8% to 23.2%. For a paper whose stated contribution is the gender dimension of
unpaid work, dropping the spouses is the wrong 5% to lose.

### E4 — `for var tpoor0125 tpoorweai: replace X = . if X != tpoor0150` destroys the robustness checks — **[HIGH]**

Line 153 sets the two alternative indicators to missing wherever they disagree with `tpoor0150`. Verified in
the release:

| variable | mean before | mean after | missings introduced |
|---|---|---|---|
| `tpoor0125` | 0.4088 (`TimPov125`) | 0.3067 | 2,063 (14.7%) |
| `tpoorweai` | 0.0296 (`TimePovWEAI`) | 0.0373 | 3,279 (23.4%) |

After the step, the survivors correlate **1.000** with `tpoor0150` by construction. `004_MSF` passes
`technology_variables = c("tpoor0150","tpoor0125")` — so the "alternative threshold" specification is not a
robustness check, it is the main specification re-run on a non-random 85% subsample. Any sensitivity claim
built on it is circular.

(`for` is a legacy Stata command superseded by `foreach`. It clearly executed here — the missings are in the
shipped file — but it should be replaced regardless.)

### E5 — The 84-hour top-code is dead code — **[MEDIUM]**

```stata
foreach var of varlist Wage_Salary-Voluntary { gen Day`var'=(`var'/7) }   // line 66-69
foreach var of varlist Wage_Salary-Voluntary { replace `var'=84 if `var'>84 }  // line 71-73
```

The `Day*` variables are created **before** the cap is applied, and everything downstream uses `Day*`. The
cap therefore has no effect. Uncapped values survive: in the full file `CommTime` reaches **30 h/day**
(210 h/week); in the analysis sample the max is 22.4 h/day and 0.17% exceed 16 h/day. Swapping the two loops
fixes it. Note also that a per-activity cap of 84 h/week does not bound the *sum*, so a cap on `CommTime`
itself (e.g. 24 h/day) is what is actually needed.

### E6 — `CommTimeWEAI` is not the WEAI construct — **[MEDIUM]**

```stata
gen CommTimeWEAI = DayWage_Salary + DayFarmEnt + DayNonFarmEnt + DayVoluntary
                 + DayDomestic + DayFHNonFarmEnt + DayNonPdtAgric
```

Three problems. (i) It silently **drops `DayApprentice`**, so there are two undocumented definitions of
"committed time" in one file. (ii) The WEAI workload indicator is defined on a **24-hour recall of the
previous day**; here it is applied to a 7-day recall divided by 7, so `>10.5 h/day` is really
`>73.5 h/week` — a different and much stricter construct, which is why the headcount is 2.96%. (iii) WEAI's
10.5-hour threshold counts domestic and care work; this one cannot.

### E7 — No survey weights anywhere — **[MEDIUM]**

`TimeGLSS7.dta` carries no weight, and none is merged in. GLSS7's `WTA_S` sits in `g7sec1.dta` and is
therefore available. All medians and headcounts here are unweighted, so the "relative" threshold is relative
to the *sample* median rather than the population median. (`WeightHH` exists in the crop-farmer release, so
weights re-enter downstream — but after the threshold has already been fixed unweighted.)

### E8 — `recode (.=0)` conflates "not asked" with "zero hours" — **[MEDIUM]**

Legitimate for people who answered "no" to a gate question. Wrong for the ~8,569 people in `TimeGLSS7` who
were never administered section 4A. Combined with no age restriction (the file runs from age 5), this is the
proximate cause of E2.

### E9 — No age restriction — **[MEDIUM]**

34.5% of the input is under 15. Bardasi & Wodon compute separate lines for adults 15+ and children 6–14
precisely because pooling them destroys the median; Zacharias *et al.* restrict to 15–70. This script pools
everyone.

### E10 — Cosmetic / hygiene

* Line 60 and line 62 are the same `recode` twice.
* Line 97: `drop Cutoff125 TimPov15 TimPov125 Cutoff15 TimPov15` lists `TimPov15` twice.
* Lines 76–78: a `sum` loop whose output is never captured.
* Lines 87 and 91 label the **CommTime**-based indicators as "Paid Time Poverty".
* `keep Surveyx EaId HhId Mid DayWage_Salary-TimePovWEAI` relies on variable *order*; it happens to be
  correct today, but any inserted `gen` between `DayWage_Salary` and `TimePovWEAI` silently changes the
  release schema. `sex`, `age`, `loc2`, `hhsize`, `district` and the original weekly-hours variables are
  dropped — no sex or age survives in the release.
* `PaidTime`, `UnpaidTime` and `CommTimeWEAI` are built with `gen a+b+c` rather than `egen rowtotal`. Safe
  today only because `recode (.=0)` ran first; if that line ever changes, missing propagates and
  `(missing > cutoff)` evaluates to **1** in Stata — silently classifying missing-hours people as time poor.

---

## 3. Does the measure capture the intent of *time poverty*?

Against the definitions assembled in `literature/literature_overall_summary.Rmd`:

| Required element | Status here |
|---|---|
| Total work = paid **plus unpaid** (Bardasi & Wodon 2006/2010; ILO 19th ICLS own-use production work) | **Absent.** `tpoor0150` is paid-only. Even `CommTime` has no chores/care. |
| Domestic and care work (cooking, cleaning, water, fuelwood, childcare, elder care) | **Absent from GLSS7 s4A entirely.** |
| Relative threshold on a meaningful reference distribution | Median computed on a base that is 55% zeros (E2), unweighted (E7), no age floor (E9). |
| "No choice" / consumption condition (Bardasi & Wodon 2010) | Not implemented. |
| Depth/severity (FGT α=1, 2) | Not implemented; binary only. |
| Simultaneity / supervisory care | Not measurable in GLSS7 stylized recall — should be stated as a downward bias, per UNSD (2024) and ILO (2018). |
| Individual unit of analysis | Head-only (E3). |

**The gender test is the clearest diagnostic.** In the 6,331-member analysis sample:

| measure | all | male | female |
|---|---|---|---|
| `tpoor0150` (shipped treatment) | 0.242 | **0.261** | **0.181** |
| `tpoor0125` (after E4) | 0.280 | 0.303 | 0.205 |
| `tpoorweai` | 0.033 | 0.033 | 0.035 |
| mean `PaidTime` (h/day) | — | 4.43 | 3.88 |
| mean `UnpaidTime` (h/day) | — | 0.194 | 0.096 |

Men are 44% more likely to be flagged. This is exactly the inversion Bardasi & Wodon (2010) describe:
*"once domestic work is added to market work, the identification of who is time poor is reversed."* Here
domestic work was never added.

**Fixing E1 alone does not fix the direction.** Recomputing a proper committed-time line on the same release
(median `CommTime` among crop-farmer heads = 4.29 h/day = 30 h/week) gives 1.5× headcount 0.256 — but
**0.277 for men vs 0.187 for women**. The measure stays male-skewed because the missing component is
structural, not a coding slip.

### What GLSS7 *can* support

GLSS7 Section 7D contains the only genuinely unpaid, gendered time variables in the survey, and the pipeline
uses none of them:

* `s7dq2b1` — **time in minutes to get drinking water and back**
* `s7dq2b2` — time to general-use water source
* `s7dq2d` — **who in the household goes to collect water** (assignable to a person)
* `s7dq3a1`, `s7dq4a1`, `s7dq4b1` — supply regularity and daily quantity needed (→ trips per day)
* `s7dq19`, `s7dq20a–i`, `s7dq21a/b` — cooking fuel and stove type (the Martey *et al.* 2021/2022 energy
  margin)
* `s7dq14`, `s7dq15` — hours of electricity per day

A water-collection-time component (minutes per trip × trips per day, assigned via `s7dq2d`) plus the
fuel/stove and electricity variables would give a defensible, if partial, unpaid-burden dimension. It would
not be a full domestic and care module — that does not exist in GLSS7 — and the paper would have to say so.

Options, revised in light of the correction above:

1. **Move the study to GLSS5 + GLSS6**, where `sec4h` / `SEC4h` supply a genuine unpaid domestic and care
   module. This yields a real paid-plus-unpaid measure with the correct gender gradient, and a larger
   farmer sample than GLSS7 (GLSS6 9,671 + GLSS5 5,461 = 15,132 farmer-members vs GLSS7's 6,688). Cost: the
   paid side is coarser (main-job hours rather than the eight-activity battery), and the most recent wave is
   dropped.
2. **Keep GLSS7 and build a partial measure** from s4A economic hours + Section 7D water/fuel collection
   time (`s7dq2b1`, `s7dq2d`, `s7dq4a1`), reframing around an infrastructure-driven time constraint rather
   than unpaid care work.
3. **Pool GLSS4–GLSS7** with an explicitly two-tier measure: full paid-plus-unpaid for GLSS4–6, paid-only
   for GLSS7, with the wave interacted so the two are never silently compared.
4. **Match GLSS7 to a time-use survey**, as the Levy Institute did (Rios-Avila 2016 documents the GLSS6 ×
   Ghana Time Use Survey 2009 match). Most defensible for GLSS7 specifically, and the precedent a referee
   will raise — but far more work than option 1 now that the native module has been found.

---

## 4. Portability to other GLSS waves

**The script is GLSS7-only and cannot run on any other wave without a rewrite.** `gen Surveyx = "GLSS7"` is
hardcoded, but that is the least of it — the input variables do not exist elsewhere. Verified from the raw
files:

| wave | employment module | 8-activity past-7-days battery? | domestic/care time module? |
|---|---|---|---|
| GLSS1–2 | — | no | none found |
| GLSS3 | `S4A.DTA` | no | `S4J.DTA` / `S4K.DTA` — unlabelled, probably housekeeping |
| GLSS4 | `SEC4A.DTA` | **no** | **`SEC4J.DTA` — yes**, 11 activities, participation + hours |
| GLSS5 | `sec4a.dta` (46 vars) | **no** — single `s4aq3` "number of hours worked", `s4aq7` "hours worked" | **`sec4h.dta` — yes**, 13 activities × (h, m), plus `weight` and `ez` |
| GLSS6 | `SEC4a.dta` (55 vars) | **no** — `s4aq9` "hours worked on main job" + `s4bq4` secondary job | **`SEC4h.dta` — yes**, 14 activities × (h, m), incl. childcare, elderly and sick care |
| **GLSS7** | `g7sec4.dta` (205 vars) | **yes** — s4aq2–s4aq20 | **none** — module dropped |
| GLSS8 | — | — | **`Data/PARTA` is empty on this machine** — GLSS8 microdata not present |

The two capabilities are in **complementary distribution**: GLSS7 has the fine-grained eight-activity
economic battery but no unpaid work; GLSS4–GLSS6 have the unpaid work but only coarse "hours worked on main
job". No wave has both. A paid-plus-unpaid measure is therefore available for GLSS4–6 at a coarser
resolution on the paid side, and not available at all for GLSS7.

So the battery the measure depends on appears in **GLSS7 only**. The variable *numbering* also collides:
GLSS6 `s4aq3` is "temporarily absent from work" and GLSS5 `s4aq3` is "number of hours worked" — so pointing
this script at another wave's `sec4a` would not error, it would compute a threshold on the wrong variables.

This matters for sample size. `harmonized_crop_farmer_data` has farmer-members in every wave —
GLSS1 1,710 · GLSS2 1,922 · GLSS3 2,957 · GLSS4 3,956 · GLSS5 5,461 · GLSS6 9,671 · GLSS7 6,688 (32,365
total) — but only GLSS7's 6,688 are reachable. `001_DATA` already hard-filters to `Surveyx %in% c("GLSS7")`
(its comment still says "GLSS6 and GLSS7"), which is correct given the input, but means the study is a
single cross-section, not the multi-wave panel the harmonization layer implies.

**A multi-wave measure would need a different construct** — a comparable single "hours worked" variable
exists in GLSS5 (`s4aq3`) and GLSS6 (`s4aq9`), which could support a *paid-hours-only* long-hours indicator
across waves. That is what `tpoor0150` currently is anyway, so it would be internally consistent — but it
would make the paid-only framing explicit rather than accidental, and it still would not be time poverty.

**Seasonality caveat if waves are ever pooled.** GLSS7 was fielded over 12 months. Martey, Etwire & Krah
(2024) show Ghanaian farm-household time poverty is strongly seasonal (wet vs dry), and Wodon & Beegle
(in Blackden & Wodon 2006) show Malawian hours peak >5 h/week above the annual mean in the cropping season.
Nothing in the pipeline controls for interview month, and no month variable survives into the release.

---

## 5. Downstream consequences already baked in

* `002_MATCHING` line 58: `DATA$Treat <- as.integer(as.numeric(DATA$tpoor0150 %in% 1))`. The entire matching
  design is built on the paid-hours dummy, exact-matched on `Female`.
* `004_MSF` line 83: `technology_variables = c("tpoor0150","tpoor0125")`. The **meta-frontier technology
  groups are defined by time-poverty status**, so the technology gap ratio is being estimated between
  long-paid-hours and short-paid-hours farmers. Whatever that identifies, it is not the intended
  "time-poor vs time-rich technology set."
* The `tpoor0125` specification in `004_MSF` will silently run on 85% of the sample and reproduce the
  `tpoor0150` result (E4).
* `Female` appears in both `inefficiency_covariates` and `adoption_covariates` in `004_MSF` **and** as an
  exact-match stratum in `002_MATCHING`. With a male-skewed treatment, the gender story will be carried
  entirely by the covariate, not by the treatment.

---

## 6. Prioritised fix list

| # | Fix | Severity | Effort |
|---|---|---|---|
| 1 | Decide what the construct is. If it stays paid-hours-only, rename it and rewrite the abstract. If it is time poverty, source unpaid work (Section 7D water/fuel at minimum; GTUS matching ideally). | Critical | Design |
| 2 | Remove the `PaidTimepoverty.dta` overwrite; make the CommTime and PaidTime measures separate, separately named files. | Critical | 10 min |
| 3 | Compute every median on a defensible base: adults 15+ (or 15–70), non-missing section 4A only, weighted by `WTA_S`. Never on a base that is majority zeros. | Critical | 30 min |
| 4 | Delete the `for var … replace X = . if X != tpoor0150` line entirely. Keep the alternative thresholds intact. | High | 1 min |
| 5 | Move `keep if s1q3==1` to the end, or drop it and carry all individuals so spouse-farmers survive; let the study script choose the unit. | High | 10 min |
| 6 | Swap the top-code loop before the `Day*` loop, and cap `CommTime` at 24 h/day rather than capping components at 84 h/week. | Medium | 5 min |
| 7 | Add `DayApprentice` to `CommTimeWEAI` or document why not; state plainly that the 10.5 h rule is being applied to a 7-day average, not a 24-hour diary. | Medium | 5 min |
| 8 | Replace `recode (.=0)` with a gate-aware fill: zero only where the gate question was answered "no". | Medium | 20 min |
| 9 | Keep `sex`, `age`, `loc2`, weight and interview month in the release; replace the order-dependent `keep … A-B` with an explicit varlist. | Medium | 10 min |
| 10 | Add an FGT time-poverty gap and squared gap so the study is not limited to a binary — the literature review flags the coarse binary as the likely cause of the one existing null in this space (Diiro et al. 2018). | Medium | 30 min |
| 11 | Parameterise the wave instead of `gen Surveyx = "GLSS7"`, and add a hard guard that errors if the expected s4aq variables are absent — so pointing it at GLSS5/6 fails loudly rather than computing on the wrong variables. | Medium | 20 min |
| 12 | Replace `for` with `foreach`; drop the duplicate `recode`, the duplicate `TimPov15` in `drop`, and the no-op `sum` loop; fix the "Paid Time Poverty" labels on the CommTime indicators. | Low | 10 min |

---

## 7. Reproduction

All figures above come from re-implementing the do-file's logic against `TimeGLSS7.dta` and checking the
result against the shipped release. Agreement is exact or near-exact:

| statistic | replication | shipped release |
|---|---|---|
| `tpoor0150` mean | 0.2602 | 0.2615 |
| `TimPov125` mean | 0.4087 | 0.4088 |
| `TimePovWEAI` mean | 0.0296 | 0.0296 |
| `UPTimPov125` = `UPTimPov15` | 0.0626 = 0.0626 | 0.0626 = 0.0626 |
| rows in release | 14,009 (heads) | 14,009 |

The residual 0.13pp gap on `tpoor0150` is a floating-point tie at the median cutoff and does not affect any
conclusion here.
