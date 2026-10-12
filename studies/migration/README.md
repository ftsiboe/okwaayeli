# Rural–Urban Migration and Farm Productivity in Ghana

**Status:** staged (analysis pipeline written, not yet run) — 2026-10-08

## Question

Does having lived in a Ghanaian town or city — or belonging to a household tied
to urban migrants — change a rural farmer's **technology gap** (distance of the
group frontier from the meta-frontier) and **technical efficiency**?

## Data

- `harmonized_crop_farmer_data` — the project's farm-level base.
- `harmonized_migration_data` — built by
  `data-raw/data-prep/glss/13_migration.do` from the GLSS3–GLSS7 migration
  modules (Section 5/5A), remittances by sender location (Section 11B) and the
  household roster (months away). One row per crop farmer.
- GLSS1/GLSS2 have no migration module and are not used.
- Sample: farms in **rural** localities, GLSS3–GLSS7.

Expected sizes from the extract (rural farmers ≈ 24,400; urban-to-rural
farmers ≈ 6,500 — 751 / 1,045 / 1,242 / 2,176 / 1,295 across GLSS3–GLSS7).
See `data-raw/data-prep/glss/logs/13_migration_expected_python.csv`.

## Design

| Piece | Choice |
|---|---|
| Primary treatment | `MigUrban`: urban-origin migrant (1) vs never moved (0) |
| Robustness contrast | `MigStatus`: non-migrant / rural origin / urban origin — separates *moving* from *urban exposure* |
| Mover type | `MigUrbanType`: urban returnee vs urban in-migrant |
| Household channel | `RemitUrban`: household receives remittances from urban Ghana |
| Heterogeneity | duration (`MigDurCat`), reason (`MigReason`), city vs town (`MigPrevSettle`), farm work before the move (`MigPrevAgric`, GLSS5–7), round |
| Matching | 002 on `MigUrban`; exact on Survey/Region/Ecozon/Female; `OwnLnd` excluded (land access is a channel, not a confounder) |
| Frontier | 004 meta-stochastic frontier, same inputs/shifters as land_tenure |

## Known limits

- Selection into migration and return is handled on observables only.
- GLSS7's previous-place type is respondent-assessed (city/town/village).
- Previous occupation is coded only in GLSS5–GLSS7.
- Remittance values are nominal GHS (GLSS3–5 converted from old cedis);
  deflate before using values rather than indicators.
- Sampling frames change across rounds; `CensusFrame` / `TrendSample` follow
  the land_tenure convention.

## Layout

```
scripts/      analysis pipeline (see scripts/README.md)
data/         migration_study_environment.rds (ignored by git)
output/       matching/, treatment_effects/, estimations/, figures/, tables/
narrative/    manuscript (to come)
literature/   references (to come)
submissions/  journal submissions (to come)
```
