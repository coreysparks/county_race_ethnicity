# CLAUDE.md — County Race & Ethnicity Mapping Project

## Project Overview

This project uses the **tidycensus** R package to pull ACS 5-year percent estimates of racial and ethnic composition for all U.S. counties from the **DP05** (Demographic and Housing Estimates) Data Profile table. The primary output is an interactive Shiny choropleth map and LISA spatial autocorrelation explorer at the county level spanning all 50 states and the District of Columbia.

---

## Data Source

### American Community Survey (ACS) 5-Year Estimates — DP05

**Publisher:** U.S. Census Bureau
**Survey:** `acs5` (5-year pooled estimates)
**Year (end year):** 2023 (covering 2019–2023)
**Table:** DP05 — ACS Demographic and Housing Estimates (Data Profile)
**Geographic unit:** U.S. counties (~3,100+); 5-digit FIPS code
**API access:** via `tidycensus::get_acs(geography = "county", survey = "acs5", table = "DP05")`

**Why DP05 instead of B tables?**
- DP05 provides **percent estimate (PE)** columns pre-computed by the Census Bureau, avoiding manual division and denominator selection errors.
- Data Profile tables aggregate related subject-area tables, reducing API call complexity.
- PE variables in DP05 are internally consistent — all percentages are over the same population denominator.

**Variables used (2023 5-year ACS):**

| Column name | ACS variable | Label |
|-------------|-------------|-------|
| `pct_white` | `DP05_0037PE` | White alone (%) |
| `pct_black` | `DP05_0038PE` | Black or African American alone (%) |
| `pct_aian` | `DP05_0039PE` | American Indian & Alaska Native alone (%) |
| `pct_asian` | `DP05_0040PE` | Asian alone (%) |
| `pct_nhopi` | `DP05_0041PE` | Native Hawaiian & Other Pacific Islander alone (%) |
| `pct_other` | `DP05_0044PE` | Some other race alone (%) |
| `pct_two_or_more` | `DP05_0045PE` | Two or more races (%) |
| `pct_hispanic` | `DP05_0071PE` | Hispanic or Latino, any race (%) |
| `pct_nh_white` | `DP05_0077PE` | Non-Hispanic White alone (%) |

> **Important:** Variable IDs are release-year specific. Always verify with `load_variables(YEAR, "acs5/profile")` before changing `YEAR` in `prep_data.R`. The prep script prints matching definitions for this purpose.

**Race vs. Ethnicity note:**
Hispanic/Latino is an ethnicity, not a race. The "White alone" (race) and "Non-Hispanic White alone" (race × ethnicity) categories therefore overlap with the Hispanic/Latino category. Do not sum all nine variables — they do not add to 100%.

---

## Data Workflow

```
tidycensus::get_acs()          # pulls DP05 PE variables for all counties
        ↓
   prep_data.R                 # reshapes to wide parquet
        ↓
data/processed/county_race_ethnicity.parquet
        ↓
   app.R (Shiny)               # loads parquet + tigris geometry
```

A Census Bureau API key is required. Register at <https://api.census.gov/data/key_signup.html> and set with `tidycensus::census_api_key("KEY", install = TRUE)`.

---

## Application Structure

The Shiny app (`app.R`) has three pages:

1. **Choropleth Map** — county fill by selected race/ethnicity group %; click for detail table
2. **Spatial Autocorrelation (LISA)** — Local Moran's I, Local G, Local G* with k=4 KNN weights; global Moran's I summary
3. **Data Sources** — documentation, methodology, variable definitions, links

All values displayed with `%` suffix. Tooltips show `rounded(value, 1)%`.

---

## Key Files

| File | Purpose |
|------|---------|
| `prep_data.R` | Pulls ACS DP05 from Census API, saves parquet |
| `app.R` | Shiny application |
| `data/processed/county_race_ethnicity.parquet` | Wide-format output (~3,100 rows × 12 cols) |

---

## Mapping Approach

- **Geometry:** U.S. Census TIGER/Line 2022 (2020-vintage boundaries) via `tigris`
- **Projection:** WGS 84 (EPSG:4326) for interactive Leaflet display
- **Classification:** quantile (default), Jenks natural breaks, or equal interval via `classInt`
- **Spatial weights:** K = 4 nearest neighbors via `sfdep::st_knn()`
- **LISA inference:** asymptotic normal approximation (not permutation-based)

---

## Reference Links

- tidycensus documentation: <https://walker-data.com/tidycensus/>
- ACS DP05 table (data.census.gov): <https://data.census.gov/table/ACSDP5Y2023.DP05>
- ACS methodology: <https://www.census.gov/programs-surveys/acs/methodology.html>
- Census API key registration: <https://api.census.gov/data/key_signup.html>
- TIGER/Line shapefiles: <https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html>
- Census race/ethnicity guidance: <https://www.census.gov/topics/population/race/about.html>
