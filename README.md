# County Race & Ethnicity Explorer

An interactive R Shiny application for mapping and spatially analyzing county-level race and ethnic composition across the United States, built by [Corey Sparks, Ph.D.](https://www.linkedin.com/in/corey-sparks-ph-d/)

---

## Overview

This application uses **American Community Survey (ACS) 5-Year Estimates** to map the racial and ethnic composition, nativity, and citizenship of U.S. counties. Data are pulled from the Census Bureau API via the [tidycensus](https://walker-data.com/tidycensus/) R package from the **DP05** (Demographic and Housing Estimates) and **DP02** (Selected Social Characteristics) Data Profile tables, and joined to USDA Rural-Urban Continuum Codes. Each ACS release is stored as its own parquet file, and the app has a year selector in the navbar covering every release from 2005–2009 through 2020–2024.

---

## Features

### Page 1: Choropleth Map
- Map any race/ethnicity, nativity/citizenship, or index variable
- UTSA color ramps, with quantile, Jenks, or equal-interval classes; RUCC is mapped as categories
- Click a county to see all its values for the selected year and download them as CSV

### Page 2: Spatial Autocorrelation (LISA)
- Local Moran's I (HH, LL, HL, LH), Local G, and Local G* (hot and cold spots)
- K = 4 nearest-neighbor weights; asymptotic normal p-values
- Optional Benjamini–Hochberg false discovery rate adjustment across the ~3,100 local tests (on by default)
- Global Moran's I summary and cluster count table

### Page 3: ICE Activity Risk Index
- Exploratory composite of four min-max normalized components with adjustable weights: log population, non-citizens as % of total population, Shannon diversity, and urbanicity (inverted RUCC)
- Based on demographic correlates only; it uses no ICE operational data and is not a prediction

### Page 4: Data Sources
- Documentation, variable IDs by release year, methodology notes, and references

---

## Variables

ACS variable IDs **and label wording** change between releases. For example, Asian alone is `DP05_0039PE` in 2009–2016, `DP05_0044PE` in 2017–2022, `DP05_0047PE` in 2023 and `DP05_0061PE` in 2024. `build_lookup.R` reads the Census API variable metadata for every release and normalizes the cosmetic label differences:

- prefixes: `Number!!Estimate!!`, `Percent Estimate!!`, `Estimate!!`
- inserted `Total population!!` and `Foreign-born population!!` path segments
- spelling: "Foreign born" vs "Foreign-born", and capitalization

It then requires exactly one match per variable per year and writes the IDs to **`data/acs_profile_lookup.csv`** (year × variable × ID × original label). `prep_data.R` reads its IDs from that table, and stops if the Census label for an ID no longer matches the one recorded.

| Column | Meaning |
|--------|---------|
| `pct_white`, `pct_black`, `pct_aian`, `pct_asian`, `pct_nhopi`, `pct_other`, `pct_two_or_more` | Race alone (% of total population); the seven sum to 100 |
| `pct_hispanic` | Hispanic or Latino, any race (%) |
| `pct_nh_white` | Non-Hispanic White alone (%) |
| `total_pop` | Total population (DP05) |
| `pct_foreign_born` | Foreign-born, % of total population |
| `pct_noncitizen_pop` | Not a U.S. citizen, % of total population (non-citizen count ÷ DP02 total population) |
| `pct_naturalized` | Naturalized citizen, % of **foreign-born** |
| `pct_noncitizen` | Not a U.S. citizen, % of **foreign-born** |
| `rucc_code`, `rucc_vintage` | USDA Rural-Urban Continuum Code and the RUCC edition used (2013 or 2023) |
| `shannon_diversity` | Shannon H over the seven race-alone groups (range 0 to ln 7 ≈ 1.95) |

> **Notes**
> - Hispanic/Latino is an ethnicity, not a race. "White alone" and "Non-Hispanic White alone" overlap with "Hispanic or Latino", so the variables do not sum to 100%. The seven race-alone categories do.
> - `pct_naturalized` and `pct_noncitizen` use the foreign-born population as the denominator. They sum to 100% and can be extreme in counties with few immigrants. Use `pct_noncitizen_pop` to compare the size of the non-citizen population across counties.
> - Race question and coding changes entered the ACS with 2020 data. Releases through 2015–2019 use the earlier coding, and 2020–2024 is the first entirely under the new one. Nationally, Two or more races rises from 3.3% (2015–2019) to 12.6% (2020–2024) and White alone falls from 72.5% to 61.0%, while Hispanic, non-Hispanic White and foreign-born shares change smoothly. Compare race-alone shares and Shannon H across the 2019/2020 boundary with caution.
> - In the 2005–2009 release, the Census API reports "cannot be computed" as a positive `666666666`; `prep_data.R` recodes all annotation codes, of either sign, to NA.

---

## Data Sources

| Dataset | Publisher | Vintage | Geographic unit |
|---------|-----------|---------|----------------|
| [ACS 5-Year Data Profiles DP05 and DP02](https://data.census.gov/table/ACSDP5Y2024.DP05) | U.S. Census Bureau | 2005–2009 through 2020–2024 | ~3,142–3,144 counties, 50 states + DC |
| [Rural-Urban Continuum Codes](https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/) | USDA ERS | 2013 (releases ending 2009–2021), 2023 (releases ending 2022+) | Counties |
| [TIGER/Line cartographic boundaries](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) | U.S. Census Bureau | Release end year (2013+); 2010 for releases ending 2009–2012 | County and state geometries |

County codes change during the period, so geometry and RUCC editions follow each release. The changes are:

- Bedford city VA is merged into Bedford County from the 2010–2014 release.
- Shannon SD and Wade Hampton AK are renamed Oglala Lakota and Kusilvak (new FIPS codes) from 2011–2015.
- Valdez-Cordova AK is split into Chugach and Copper River from 2016–2020.
- Connecticut's 8 counties are replaced by 9 planning regions from 2018–2022.

The 2013 RUCC file predates the Alaska and South Dakota changes, so those counties take their predecessor's code. Every county joins to a geometry and a RUCC code in every release.

---

## Project Structure

```
county_race_ethnicity/
├── app.R                                         # Shiny application
├── build_lookup.R                                # Builds the per-year ACS variable ID lookup
├── prep_data.R                                   # Builds one parquet per ACS year
├── README.md
├── data/
│   ├── acs_profile_lookup.csv                    # Variable IDs, 2009–2024
│   └── processed/
│       ├── county_race_ethnicity_2009.parquet    # 2005–2009 ACS
│       ├── ...
│       └── county_race_ethnicity_2024.parquet    # 2020–2024 ACS
```

---

## Setup & Usage

### Requirements

R 4.x with the following packages:

```r
install.packages(c(
  "tidycensus", "shiny", "bslib", "leaflet", "sf", "dplyr", "readr", "readxl", "jsonlite",
  "arrow", "tigris", "classInt", "sfdep", "spdep",
  "DT", "shinycssloaders"
))
```

### Census API key

tidycensus needs a free key, both for data and for `load_variables()`. Register at <https://api.census.gov/data/key_signup.html>, then:

```r
tidycensus::census_api_key("YOUR_KEY", install = TRUE)
```

Restart R afterwards so `CENSUS_API_KEY` is set.

### Prepare the data

The variable lookup is already in `data/acs_profile_lookup.csv`. Rebuild it only when a new ACS release comes out; this step needs no API key:

```sh
Rscript build_lookup.R            # all releases available from the Census API
```

Then build one data file per ACS end year (2009–2024):

```sh
Rscript prep_data.R 2013
for y in $(seq 2009 2024); do Rscript prep_data.R $y; done   # all years (bash)
```

The script prints the resolved variable IDs and runs validation checks before it writes anything. The checks are: the race-alone groups sum to 100, percentages fall within 0–100, naturalized plus non-citizen equals 100, and so on. If any check fails, the parquet is not written.

### Launch the app

```r
shiny::runApp("app.R")
```

The app loads every `county_race_ethnicity_<year>.parquet` in `data/processed/` at startup. It skips files that are missing required columns, such as files from older versions of `prep_data.R`, and warns you about them. County shapefiles for each release's geometry vintage are downloaded with `tigris` the first time that year is selected, then cached.

---

## Spatial Methods

- **Local Moran's I:** Anselin, L. (1995). Local indicators of spatial association—LISA. *Geographical Analysis*, 27(2), 93–115. <https://doi.org/10.1111/j.1538-4632.1995.tb00338.x>
- **Local G / G\*:** Getis, A., & Ord, J. K. (1992). The analysis of spatial association by use of distance statistics. *Geographical Analysis*, 24(3), 189–206. <https://doi.org/10.1111/j.1538-4632.1992.tb00261.x>
- Implemented with [`sfdep`](https://sfdep.josiahparry.com/) for neighbors and weights and [`spdep`](https://r-spatial.github.io/spdep/) for `localmoran()` and `localG()`. G* adds each county to its own neighbor set.

---

## Deployment

To publish to [shinyapps.io](https://www.shinyapps.io):

```r
install.packages("rsconnect")
rsconnect::deployApp(
  appDir   = ".",
  appFiles = c("app.R", "data/acs_profile_lookup.csv",
               list.files("data/processed", pattern = "\\.parquet$", full.names = TRUE)),
  appName  = "county-race-ethnicity-explorer"
)
```
