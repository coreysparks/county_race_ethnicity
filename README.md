# County Race & Ethnicity Explorer

An interactive R Shiny application for mapping and spatially analyzing county-level race and ethnic composition across the United States, built by [Corey Sparks, Ph.D.](https://www.linkedin.com/in/corey-sparks-ph-d/)

---

## Overview

This application uses the **American Community Survey (ACS) 5-Year Estimates** (2019–2023) to map the racial and ethnic composition of U.S. counties as percentages of the total population. Data are pulled directly from the Census Bureau API via the [tidycensus](https://walker-data.com/tidycensus/) R package using the **DP05** (Demographic and Housing Estimates) Data Profile table.

---

## Features

### Page 1 — Choropleth Map
- Select any of 9 race/ethnicity groups as percent of total county population
- Choose color palette, classification scheme (quantile, Jenks natural breaks, equal interval), and number of classes
- Light grey county boundaries with narrow black state boundaries
- Click any county to view a full table of all its race/ethnicity estimates

### Page 2 — Spatial Autocorrelation (LISA)
- Three local spatial statistics using asymptotic normal approximation:
  - **Local Moran's I** — identifies HH, LL, HL, LH clusters and spatial outliers
  - **Local G (Getis-Ord)** — identifies hot spots and cold spots
  - **Local G\*** (Getis-Ord) — includes focal unit in neighbor set
- K = 4 nearest-neighbor spatial weights
- Adjustable significance level
- Global Moran's I summary (I, E[I], Var[I], z-score, p-value)
- Cluster count table

### Page 3 — Data Sources
- Full documentation, methodology notes, and links for all data sources and spatial methods

---

## Race/Ethnicity Groups Mapped

| Column | Label | ACS Variable |
|--------|-------|-------------|
| `pct_white` | White Alone | DP05_0037PE |
| `pct_black` | Black or African American Alone | DP05_0038PE |
| `pct_aian` | American Indian & Alaska Native Alone | DP05_0039PE |
| `pct_asian` | Asian Alone | DP05_0040PE |
| `pct_nhopi` | Native Hawaiian & Other Pacific Islander Alone | DP05_0041PE |
| `pct_other` | Some Other Race Alone | DP05_0044PE |
| `pct_two_or_more` | Two or More Races | DP05_0045PE |
| `pct_hispanic` | Hispanic or Latino (any race) | DP05_0071PE |
| `pct_nh_white` | Non-Hispanic White Alone | DP05_0077PE |

> **Note:** Hispanic/Latino is an ethnicity, not a race. The "White alone" and "Non-Hispanic White alone" categories therefore overlap with the "Hispanic or Latino" category. Variable IDs are specific to the 2023 5-year ACS; use `tidycensus::load_variables(2023, "acs5/profile")` to verify for other years.

---

## Data Sources

| Dataset | Publisher | Vintage | Geographic unit |
|---------|-----------|---------|----------------|
| [ACS 5-Year Estimates, DP05](https://data.census.gov/table/ACSDP5Y2023.DP05) | U.S. Census Bureau | 2019–2023 | ~3,100 counties, 50 states + DC |
| [TIGER/Line Shapefiles](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) | U.S. Census Bureau | 2022 (2020-vintage boundaries) | County and state geometries |

Data are retrieved live from the Census API at prep time — no raw CSV download required.

---

## Project Structure

```
county_race_ethnicity/
├── app.R                                    # Shiny application
├── prep_data.R                              # Data preparation script (run once)
├── README.md
├── data/
│   └── processed/
│       └── county_race_ethnicity.parquet    # Wide-format ACS output
```

---

## Setup & Usage

### Requirements

R 4.x with the following packages:

```r
install.packages(c(
  "tidycensus", "shiny", "bslib", "leaflet", "sf", "dplyr", "tidyr",
  "arrow", "tigris", "classInt", "RColorBrewer", "sfdep", "spdep",
  "DT", "shinycssloaders"
))
```

### Census API key

A free key is required for tidycensus. Register at <https://api.census.gov/data/key_signup.html>, then:

```r
tidycensus::census_api_key("YOUR_KEY", install = TRUE)
```

### Prepare the data

Run once to pull ACS data and build the parquet file:

```r
source("prep_data.R")
```

`prep_data.R` also prints the DP05 variable definitions so you can verify the variable IDs before committing. Expect ~3,100 county rows in the output.

### Launch the app

```r
shiny::runApp("app.R")
```

County and state shapefiles are downloaded automatically via `tigris` on first run and cached locally.

---

## Spatial Methods

- **Local Moran's I:** Anselin, L. (1995). Local indicators of spatial association—LISA. *Geographical Analysis*, 27(2), 93–115. <https://doi.org/10.1111/j.1538-4632.1995.tb00338.x>
- **Local G / G\*:** Getis, A., & Ord, J. K. (1992). The analysis of spatial association by use of distance statistics. *Geographical Analysis*, 24(3), 189–206. <https://doi.org/10.1111/j.1538-4632.1992.tb00261.x>
- Implemented via [`sfdep`](https://sfdep.josiahparry.com/) (Parry 2023) and [`spdep`](https://r-spatial.github.io/spdep/) (Bivand et al.)

---

## Deployment

To publish to [shinyapps.io](https://www.shinyapps.io):

```r
install.packages("rsconnect")
rsconnect::deployApp(
  appDir   = ".",
  appFiles = c("app.R", "data/processed/county_race_ethnicity.parquet"),
  appName  = "county-race-ethnicity-explorer"
)
```

---

## Notes

- All percent estimates come from the ACS DP05 Data Profile table (PE-suffix variables), which provides pre-computed percentages over the Census Bureau's internal population denominator — no manual division is needed.
- The 2022 TIGER/Line shapefiles use 2020 Census county definitions, including Connecticut's 9 planning regions, consistent with the 2019–2023 ACS geography.
- All spatial statistics use asymptotic normal approximation, not permutation-based inference.
