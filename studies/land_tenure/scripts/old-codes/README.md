# Retired scripts — land_tenure

Kept for provenance. Nothing in the active pipeline reads them.

## `100_exhibits.do` — retired 2026-07-15

Replaced by **`scripts/100_exhibit_descriptive_stats.R`**, which computes Tables 1, 2
and S1–S4 in R directly from `study_environment$study_raw_data`.

It was the only reader or writer of `output/land_tenure_results.xlsx` and
`output/land_tenure_study_study_data.dta`. Both are now orphaned. Note the `.dta`
it built was a duplicate of `study_raw_data` — its opening merge
(`harmonized_land_tenure_data` + `harmonized_crop_farmer_data` on
`Surveyx EaId HhId Mid`, GLSS3–7, drop `EduWhyNo`) is line-for-line what
`001_DATA_land_tenure_study.R` already does in R.

`101_exhibit_figures.R` (formerly `100_exhibits.R`) is **not** retired: it still builds the figures and
`output/figure_data/`, reading the estimation `.rds` files rather than the `.dta`.

### Why it was retired rather than fixed

Four failure modes, all silent, all found on 2026-07-15:

1. **`mat roweq A = Female`** (lines 175/192 as shipped). Hardcoded inside the
   loop over the `OwnLnd0`/`OwnLnd1` dummies, so the ownership-share rows were
   written to the `means` sheet tagged `Equ == "Female"`, colliding with the real
   Female outcome. `Equ=="Female"` carried two `Mean_OwnLnd0` rows — `N=35,185`
   (the share) and `N=13,099` (the mean). A lookup by `(Equ, Coef)` could return
   either. The v001 extraction picked correctly by luck. Present in six of seven
   studies; only `resource_extraction` was clean.
2. **`cap{ }` around every specification.** A failed fit left the exported cell at
   whatever it held. `resource_extraction`'s sheet carries
   `Pineapple Depend Trend_disagCat1 = -669,487,338,753,097,984` percent/year and
   `salt × Tomatoe Trend = 0`. `LndAq_6` is simply absent from land's sheet
   because "Other" acquisition exists only in GLSS3/4 while that block keeps
   GLSS6/7.
3. **Positional addressing.** `mat rownames` / `roweq` index results by matrix
   position, which is what let (1) go unnoticed for years. The R engine emits a
   keyed long frame; `.pick()` errors on a duplicate rather than guessing.
4. **`export excel ... sheetmodify`** overwrites cells without clearing the sheet,
   so a shorter run leaves stale rows behind.

Beyond the code: Stata's do-editor runs the **buffer**, not the file on disk. The
workbook of 2026-07-15 15:21 was produced without the `roweq` fix that was
already saved to disk, and saving that buffer would have reverted it.

### Replacement, and how it is trusted

`100_exhibit_descriptive_stats.R` → `okwaayeli::draw_descriptive_summary()` and
`descriptive_indicator_shares()` → cached to `data/descriptive_exhibits.rds`,
read by `110_exhibit_tables.R`.

The engine is validated against **both** studies' Stata workbooks, ~15,000
assertions: `resource_extraction` (7 treatments × 28 crops, both engines) and
`land_tenure` (two families in one table, the `wave_diff` trend flavor). See
`tests/testthat/test-descriptive-exhibits-*.R`.

It matches Stata exactly wherever a cell is estimable. The one deliberate
divergence is that it **refuses** fits the data cannot support
(`descriptive_trend_model(min_group_n, min_events)`) instead of emitting the
numbers in (2).

### If you need to run it again

It still works. `$GitHub` is guarded at the top and the paths point at
`ghana\okwaayeli`. Run it from a **fresh Stata session**
(`do "studies/land_tenure/scripts/old-codes/100_exhibits.do"`), not the editor's
Run button.
