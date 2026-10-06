# CLAUDE.md — County Race & Ethnicity Mapping Project

## Project Overview

This project uses the **tidycensus** R package to pull ACS 5-year estimates of racial and ethnic composition, nativity, and citizenship for all U.S. counties from the **DP05** and **DP02** Data Profile tables, joined to USDA Rural-Urban Continuum Codes. The output is an interactive Shiny app (choropleth map, LISA explorer, exploratory ICE activity index) covering all 50 states and DC, with one data file per ACS release.

---

## Data Source

### American Community Survey (ACS) 5-Year Estimates — DP05 and DP02

**Publisher:** U.S. Census Bureau
**Survey:** `acs5` (5-year pooled estimates)
**Releases in the app:** every ACS 5-year release from 2009 (2005–2009) to 2024 (2020–2024); one parquet per end year
**Geographic unit:** U.S. counties (3,142–3,144 depending on release); 5-digit FIPS code

**Variable resolution:** both IDs and label wording change between releases (Asian alone: `DP05_0039PE` 2009–16, `0044PE` 2017–22, `0047PE` 2023, `0061PE` 2024; DP02 renumbered in 2019 and 2020). `build_lookup.R` fetches Census `variables.json` for each year, normalizes cosmetic label differences (`Number!!Estimate!!` / `Percent Estimate!!` prefixes, inserted `Total population!!` and `Foreign-born population!!` segments, "Foreign born"/"Foreign-born", case), requires exactly one match per variable per year (only `pct_two_or_more` has an allowed duplicate; lowest ID used), and writes **`data/acs_profile_lookup.csv`**. `prep_data.R` takes IDs only from that CSV and stops if the live label for an ID differs from the recorded one. Do not reintroduce partial/last-segment label matching or hardcoded fallback IDs: both previously produced silently wrong data (e.g. "White" matched the alone-or-in-combination row; 2024 IDs were applied to 2023).

To add a new release: `Rscript build_lookup.R`, review the new rows, then `Rscript prep_data.R <year>`.

| Column | Meaning |
|--------|---------|
| `pct_white` … `pct_two_or_more` | 7 race-alone groups, % of total pop. (sum ≈ 100) |
| `pct_hispanic`, `pct_nh_white` | Hispanic (any race); non-Hispanic White alone (%) |
| `total_pop` | DP05 total population |
| `pct_foreign_born` | Foreign-born, % of total pop. |
| `pct_noncitizen_pop` | Non-citizen count / DP02 place-of-birth total population × 100 |
| `pct_naturalized`, `pct_noncitizen` | **% of foreign-born** (sum to 100) |
| `rucc_code`, `rucc_vintage` | USDA RUCC (1–9) and edition: 2013 for releases ≤ 2021, 2023 for ≥ 2022 |
| `shannon_diversity` | Shannon H over the 7 race-alone groups (0 to ln 7) |

**Annotation codes:** the 2009 release returns positive `666666666` ("cannot be computed") that tidycensus does not recode; `prep_data.R` sets all ±111111111…±999999999 values to NA.

**Geography by release:** county FIPS sets change (Bedford city VA gone from 2014; Shannon/Wade Hampton → Oglala Lakota 46102 / Kusilvak 02158 from 2015; Valdez-Cordova → Chugach 02063 / Copper River 02066 from 2020; CT planning regions from 2022). `app.R` uses tigris cartographic boundaries for the release end year (2013+) or 2010 (releases 2009–2012) via `geo_vintage()` / `get_counties()`; all releases join 100%. RUCC 2013 is cross-walked for the AK/SD changes.

**Race vs. Ethnicity note:**
Hispanic/Latino is an ethnicity, not a race. "White alone" and "Non-Hispanic White alone" overlap with Hispanic/Latino. Only the seven race-alone groups sum to ~100%.

**Denominator note:** `pct_naturalized` and `pct_noncitizen` are shares of the foreign-born population (they sum to 100). Use `pct_noncitizen_pop` for cross-county comparisons of non-citizen population size (the ICE index uses it).

**Year comparability:** race coding changes entered the ACS with 2020 data. Releases ≤ 2019 use the old coding; 2020–2023 mix; 2024 is fully new. Nationally Two+ races goes 3.3% (2019) → 12.6% (2024) and White alone 72.5% → 61.0%, while Hispanic, NH White, and foreign-born are smooth. Treat race-alone shares and Shannon H across the 2019/2020 boundary with caution.

---

## Data Workflow

```
Rscript build_lookup.R         # Census variables.json → data/acs_profile_lookup.csv (no key)
        ↓
Rscript prep_data.R <year>     # IDs from lookup (label-checked), pulls DP05 + DP02, joins RUCC,
        ↓                      # computes Shannon H, runs validation checks
data/processed/county_race_ethnicity_<year>.parquet
        ↓
   app.R (Shiny)               # loads all valid year files at startup; county geometry per
                               # release vintage, loaded on first use and cached
```

A Census Bureau API key is required (tidycensus ≥ 1.8 also needs it for `load_variables()`). Register at <https://api.census.gov/data/key_signup.html> and set with `tidycensus::census_api_key("KEY", install = TRUE)`.

The validation block in `prep_data.R` refuses to write a file unless the race-alone groups sum to 100 (±1.5), percents are within 0–100, naturalized + non-citizen = 100 (±1), etc. `app.R` skips (with a warning) any parquet missing required columns.

---

## Application Structure

`app.R` has four pages plus a navbar ACS year selector:

1. **Choropleth Map** — UTSA color ramps, classInt classes; RUCC mapped categorically; click a county for a detail table + CSV (follows the selected year)
2. **Spatial Autocorrelation (LISA)** — Local Moran's I (`spdep::localmoran`), Local G / G* (`spdep::localG`; G* via `include.self`), k = 4 KNN weights, optional BH FDR adjustment, global Moran's I
3. **ICE Activity Risk Index** — exploratory weighted composite of min-max normalized log population, `pct_noncitizen_pop`, Shannon H, inverted RUCC
4. **Data Sources** — documentation, per-year variable IDs, methodology, links

Colors follow the UTSA palette (constants `UTSA`, `UTSA_RAMPS`, `lisa_palette()` in `app.R`).

---

## Key Files

| File | Purpose |
|------|---------|
| `build_lookup.R` | Builds the per-year variable ID lookup from Census metadata |
| `prep_data.R` | Pulls ACS DP05/DP02 + RUCC for one year, validates, saves parquet |
| `app.R` | Shiny application (also reads the lookup for the Data Sources table) |
| `data/acs_profile_lookup.csv` | year × variable × ID × original label, 2009–2024 |
| `data/processed/county_race_ethnicity_<year>.parquet` | Wide-format output (~3,143 rows × 20 cols), 2009–2024 |

---

## Mapping Approach

- **Geometry:** TIGER/Line cartographic boundaries (1:20M) via `tigris`, vintage = release end year (2013+) or 2010; state outlines 2022
- **Basemap:** Esri World Light Gray Canvas (CARTO tiles now require an API key)
- **Projection:** WGS 84 (EPSG:4326) for interactive Leaflet display
- **Classification:** quantile (default), Jenks natural breaks, or equal interval via `classInt`
- **Spatial weights:** K = 4 nearest neighbors via `sfdep::st_knn()`, row-standardized
- **LISA inference:** asymptotic normal approximation (not permutation-based)

---

## Reference Links

- tidycensus documentation: <https://walker-data.com/tidycensus/>
- ACS DP05 table (data.census.gov): <https://data.census.gov/table/ACSDP5Y2024.DP05>
- ACS methodology: <https://www.census.gov/programs-surveys/acs/methodology.html>
- Census API key registration: <https://api.census.gov/data/key_signup.html>
- TIGER/Line shapefiles: <https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html>
- Census race/ethnicity guidance: <https://www.census.gov/topics/population/race/about.html>
