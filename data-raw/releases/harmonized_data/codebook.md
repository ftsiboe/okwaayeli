# Harmonized data codebook

This codebook documents the harmonized datasets released by the
[`okwaayeli`](https://github.com/ftsiboe/okwaayeli) project. Each dataset
is published as a `.dta` (Stata) file under the `hh_data` release on
GitHub and downloaded by `okwaayeli::get_household_data()`.

All datasets are derived from the
[Ghana Living Standards Surveys](https://statsghana.gov.gh/) (GLSS1–7)
and follow the harmonization documented in
[Tsiboe (2022)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4134518).

> **Status:** this codebook is currently a stub. Every dataset is listed
> with its source, coverage and key identifiers; full variable-level
> documentation is added as each study is finalised. Issues / PRs adding
> rows to the variable tables are welcome — please use the
> [data request template](../../../.github/ISSUE_TEMPLATE/data_request.yml).

## Shared identifiers

All harmonized datasets share the same four-level identifier:

| Variable | Description | Type |
|---|---|---|
| `Surveyx` | GLSS round label (`GLSS1`–`GLSS7`) | character |
| `EaId` | Enumeration area | numeric |
| `HhId` | Household identifier | numeric |
| `Mid` | Household member identifier | numeric |

These identifiers join cleanly across every harmonized table.

Common weighting / temporal columns:

| Variable | Description |
|---|---|
| `Weight` | Sampling weight |
| `Region` | GLSS region code |
| `Ecozon` | Agro-ecological zone (recoded) |
| `TrendY` | Survey year (first half of the `Season` label) |

## Datasets

### `harmonized_crop_farmer_data.dta` (≈ 34 MB)

- **Source:** GLSS1–GLSS7 agriculture modules.
- **Coverage:** 1987–2017, nationally representative, all major crops.
- **Granularity:** farmer × crop × season.
- **Status:** Published as the data paper underlying the project
  (SSRN 4134518).

Variables (illustrative — see the data paper for the full list):

| Variable | Description | Unit |
|---|---|---|
| `CropIDx` | Crop label (Maize, Sorghum, Cocoa, …) | character |
| `CropCatIDx` | Crop category (Cereal, Starchy staple, Legume, …) | character |
| `Area` | Planted area | hectares |
| `HrvstKg` | Harvest quantity | kg |
| `SeedKg` | Seed used | kg/ha |
| `FertKg` | Fertiliser used | kg/ha |
| `PstcdHr` | Pesticide cost | constant GH¢/ha |
| `LabHr` | Labour input | hours/ha |
| `EqipMech` | Mechanisation indicator | 0/1 |
| `CrpMix` | Transformed Herfindahl (Simpson) Index of crop diversification | 0–1 |

### `harmonized_disability_data.dta` (≈ 13 MB)

- **Source:** GLSS6 + GLSS7 disability modules.
- **Coverage:** 2012/13 and 2016/17.
- **Granularity:** household member.
- **Status:** Powers the disability paper (under review at Agricultural
  Economics).

Key variables: `disabled`, `disabled_self`, `disabled_relation`, and a
range of disability-type indicators (mobility, sight, hearing, cognition,
self-care, communication).

### `harmonized_education_data.dta` (≈ 7.7 MB)

- **Source:** GLSS education modules.
- **Coverage:** 1987–2017.
- **Granularity:** household member.

Key variables: `YerEdu` (completed years of education), `EduLevel`
(highest level), literacy and numeracy indicators where collected.

### `harmonized_societal_peace_and_cohesion_data.dta` (≈ 8.5 MB)

- **Source:** GLSS6–GLSS7 community / social-capital modules.
- **Coverage:** 2012/13 and 2016/17.
- **Granularity:** community.

Key variables: the Peace & Social Cohesion Index (PCSI) components and
the aggregate index. See the conflict-paper draft for the full scoring
table.

### `harmonized_financial_inclusion_data.dta` (≈ 1.8 MB)

- **Source:** GLSS finance / mobile-money modules.
- **Coverage:** Multi-wave.
- **Granularity:** household.

Companion file `financial_inclusion_index.dta` contains the derived
financial-inclusion index used in the credit paper.

### `harmonized_land_tenure_data.dta` (≈ 688 KB)

- **Source:** GLSS land / tenure variables.
- **Coverage:** GLSS3–7.
- **Granularity:** farmer × parcel.

Key variables: `LndAqKin`, `LndAqBuy`, `LndAqRnt`, `LndAqFre`, `LndRgtSll`,
`LndRgtSec`, `LndRgtBth`, `LndDed`, `LndOwn`, `LndNOwn`, `LndFrgMid`.

### `harmonized_resources_extraction_data.dta` (≈ 81 KB)

- **Source:** Community-level extraction indicators (gold, bauxite, sand,
  stone quarrying).
- **Coverage:** GLSS waves with community-level data.
- **Granularity:** community.

Powers the resource-extraction paper.

### `harmonized_ag_services_data.dta` (≈ 75 KB)

- **Source:** Community-level extension / cooperative / service indicators.
- **Coverage:** GLSS waves with community-level data.
- **Granularity:** community.

Powers the agricultural-services and (planned) extension-services papers.

### `harmonized_income_transfer_data.dta` (≈ 667 KB)

- **Source:** GLSS income-transfer module.
- **Coverage:** Multi-wave.
- **Granularity:** household.

Released for the income-transfer paper; study folder is not yet
scaffolded.

### `harmonized_time_poverty_data.dta` (≈ 1.5 MB)

- **Source:** GLSS7 time-use harmonization.
- **Coverage:** GLSS7.
- **Granularity:** household member.

Companion files in `data-raw/`: `PaidTimepoverty.dta`,
`UnpaidTimepoverty.dta`, `TimeGLSS7.dta`.

### `harmonized_nonfarm_enterprise_data.dta` (≈ 1.9 MB)

- **Source:** GLSS non-farm enterprise module.
- **Coverage:** Multi-wave.
- **Granularity:** household.

No study folder yet — included in the release for downstream users.

### `ghana_agro_dealer_sale_points.rds` (≈ 161 KB)

- **Source:** Compiled agro-dealer GIS points.
- **Coverage:** Cross-section.
- **Granularity:** sale point.

Used by `studies/input_dealers/`.

## Updating this codebook

When you add or modify a harmonized dataset (see
[CONTRIBUTING.md §6](../../../CONTRIBUTING.md)), please update the
corresponding section above in the same PR.
