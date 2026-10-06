library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(arrow)
library(tigris)
library(classInt)
library(sfdep)
library(spdep)
library(DT)
library(shinycssloaders)

# Resolve DT vs shiny naming conflict
dataTableOutput <- DT::dataTableOutput
renderDataTable <- DT::renderDataTable

options(tigris_use_cache = TRUE)

# ---------------------------------------------------------------------------
# UTSA brand colors
# ---------------------------------------------------------------------------
UTSA <- c(
  midnight = "#032044", orange = "#F15A22", river_mist = "#C8DCFF",
  talavera_blue = "#265BF7", mission_clay = "#DBB485", brass = "#A06620",
  limestone = "#F8F4F1", concrete = "#EBE6E2", smoke = "#D5CFC8",
  white = "#FFFFFF", accessible_orange = "#D3430D"
)
UTSA_RAMPS <- list(
  "UTSA Blues"   = unname(UTSA[c("limestone", "river_mist", "talavera_blue", "midnight")]),
  "UTSA Oranges" = unname(UTSA[c("limestone", "mission_clay", "orange", "accessible_orange")])
)
ramp_colors <- function(name, n, reverse = FALSE) {
  cols <- grDevices::colorRampPalette(UTSA_RAMPS[[name]])(n)
  if (reverse) rev(cols) else cols
}

# ---------------------------------------------------------------------------
# Variable metadata
# ---------------------------------------------------------------------------
RACE_VARS <- c(
  pct_white       = "White Alone",
  pct_black       = "Black or African American Alone",
  pct_aian        = "American Indian & Alaska Native Alone",
  pct_asian       = "Asian Alone",
  pct_nhopi       = "Native Hawaiian & Other Pacific Islander Alone",
  pct_other       = "Some Other Race Alone",
  pct_two_or_more = "Two or More Races",
  pct_hispanic    = "Hispanic or Latino (any race)",
  pct_nh_white    = "Non-Hispanic White Alone"
)

# Nativity & citizenship percent estimates (from ACS DP02). Labels carry their
# denominator because they differ: naturalized / non-citizen PEs are % of the
# foreign-born population, not of the total population.
NATIVITY_VARS <- c(
  pct_foreign_born   = "Foreign-Born (% of total pop.)",
  pct_noncitizen_pop = "Not a U.S. Citizen (% of total pop.)",
  pct_naturalized    = "Naturalized U.S. Citizen (% of foreign-born)",
  pct_noncitizen     = "Not a U.S. Citizen (% of foreign-born)"
)
NATIVITY_UNITS <- c(
  pct_foreign_born   = "% of total population",
  pct_noncitizen_pop = "% of total population",
  pct_naturalized    = "% of foreign-born",
  pct_noncitizen     = "% of foreign-born"
)

RUCC_LABELS <- c(
  "1" = "1 Metro, 1M+",              "2" = "2 Metro, 250K–1M",
  "3" = "3 Metro, < 250K",           "4" = "4 Urban 20K+, adjacent",
  "5" = "5 Urban 20K+, not adj.",    "6" = "6 Urban 5–20K, adjacent",
  "7" = "7 Urban 5–20K, not adj.",   "8" = "8 Urban < 5K, adjacent",
  "9" = "9 Urban < 5K, not adj."
)

# Non-percentage index / count variables
IDX_VARS <- c(
  total_pop         = "Total Population",
  shannon_diversity = "Shannon Diversity Index",
  rucc_code         = "USDA Rural-Urban Continuum Code"
)

# Grouped selectInput choices (creates <optgroup> in HTML)
var_choices <- list(
  "Race & Ethnicity (%)"       = setNames(names(RACE_VARS),     paste0(RACE_VARS,     " (%)")),
  "Nativity & Citizenship (%)" = setNames(names(NATIVITY_VARS), NATIVITY_VARS),
  "Diversity & Demographics"   = setNames(names(IDX_VARS),      IDX_VARS)
)

is_pct_var <- function(col) col %in% c(names(RACE_VARS), names(NATIVITY_VARS))

col_label <- function(col) {
  if (col %in% names(RACE_VARS))     return(paste0(RACE_VARS[col],     " (%)"))
  if (col %in% names(NATIVITY_VARS)) return(unname(NATIVITY_VARS[col]))
  if (col %in% names(IDX_VARS))      return(unname(IDX_VARS[col]))
  col
}

fmt_val <- function(col, values) {
  if (is_pct_var(col)) {
    ifelse(is.na(values), "No data", paste0(round(values, 1), "%"))
  } else if (col == "total_pop") {
    ifelse(is.na(values), "No data",
           formatC(as.integer(values), format = "d", big.mark = ","))
  } else if (col == "rucc_code") {
    ifelse(is.na(values), "No data", as.character(as.integer(values)))
  } else {
    ifelse(is.na(values), "No data", as.character(round(values, 3)))
  }
}

# ---------------------------------------------------------------------------
# Data loaders
# ---------------------------------------------------------------------------

REQUIRED_COLS <- c("fips", "county", "state", names(RACE_VARS),
                   names(NATIVITY_VARS), names(IDX_VARS))

# Load every year-stamped parquet in data/processed/ (written by prep_data.R).
# Files from older versions of prep_data.R lack required columns and are
# skipped with a warning; re-run prep_data.R for that year to rebuild them.
load_all_years <- function() {
  yr_files <- list.files("data/processed",
                         pattern = "^county_race_ethnicity_\\d{4}\\.parquet$",
                         full.names = TRUE)
  out <- list()
  for (f in yr_files) {
    yr <- sub(".*_(\\d{4})\\.parquet$", "\\1", f)
    df <- as.data.frame(arrow::read_parquet(f))
    missing <- setdiff(REQUIRED_COLS, names(df))
    if (length(missing) > 0) {
      warning("Skipping ", basename(f), " (missing ", paste(missing, collapse = ", "),
              "). Re-run: Rscript prep_data.R ", yr)
      next
    }
    df$fips <- formatC(as.character(df$fips), width = 5, flag = "0")
    out[[yr]] <- df
  }
  if (length(out) == 0) {
    stop("No usable data in data/processed/. Run `Rscript prep_data.R <year>` first.")
  }
  out[order(names(out), decreasing = TRUE)]   # most-recent first
}

TERRITORIES <- c("60", "66", "69", "72", "78")   # AS, GU, MP, PR, VI

# County boundaries must match each release's county FIPS codes (Connecticut
# planning regions from 2022, Oglala Lakota / Kusilvak from 2015, Bedford city VA
# until 2013, Valdez-Cordova AK until 2019). Cartographic boundary files exist
# for 2010 and 2013+, so 2009–2012 use 2010 (no county changes in that span).
geo_vintage <- function(year) if (as.integer(year) >= 2013) as.integer(year) else 2010L

GEO_CACHE <- new.env()
get_counties <- function(year) {
  key <- as.character(geo_vintage(year))
  if (is.null(GEO_CACHE[[key]])) {
    co <- tigris::counties(cb = TRUE, resolution = "20m", year = geo_vintage(year),
                           progress_bar = FALSE)
    co <- co[!co$STATEFP %in% TERRITORIES, ]
    # The 2010 file has STATEFP/COUNTYFP but no GEOID
    co$GEOID <- paste0(co$STATEFP, co$COUNTYFP)
    # 2010–2014 files store some names as Latin-1 (e.g. Doña Ana NM). Invalid
    # UTF-8 sent over the websocket makes the browser drop the session.
    bad <- !validUTF8(co$NAME)
    co$NAME[bad] <- iconv(co$NAME[bad], from = "latin1", to = "UTF-8")
    GEO_CACHE[[key]] <- sf::st_transform(co[, c("GEOID", "NAME", "STATEFP")], 4326)
  }
  GEO_CACHE[[key]]
}

# Loaded once per R process and shared by all sessions
RACE_DATA       <- load_all_years()
available_years <- names(RACE_DATA)
ACS_LOOKUP      <- read.csv("data/acs_profile_lookup.csv", stringsAsFactors = FALSE)
STATES <- sf::st_transform(
  subset(tigris::states(cb = TRUE, resolution = "20m", year = 2022, progress_bar = FALSE),
         !STATEFP %in% TERRITORIES),
  4326)
invisible(get_counties(available_years[1]))   # warm the default year

# Tooltip name: the ACS name (clean UTF-8, includes "County"/"Parish"), falling
# back to the boundary file's name for shapes without data
county_label <- function(csf) {
  paste0(ifelse(is.na(csf$county), csf$NAME, csf$county), ", ", csf$state)
}

# Shared map pieces
# Basemap: Esri World Light Gray Canvas (no API key; CARTO basemaps now require one)
base_map <- function() {
  leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
    addProviderTiles(providers$Esri.WorldGrayCanvas) |>
    setView(lng = -96, lat = 38, zoom = 4)
}

county_border <- function(map, data, fill, labels, layer_id = NULL) {
  map |>
    addPolygons(
      data             = data,
      fillColor        = fill,
      fillOpacity      = 0.8,
      color            = UTSA[["smoke"]],
      weight           = 0.4,
      opacity          = 0.8,
      smoothFactor     = 0.3,
      label            = labels,
      layerId          = layer_id,
      highlightOptions = highlightOptions(
        weight = 2, color = UTSA[["midnight"]], fillOpacity = 0.95, bringToFront = TRUE
      )
    ) |>
    addPolylines(
      data         = STATES,
      color        = UTSA[["midnight"]],
      weight       = 0.9,
      opacity      = 0.9,
      smoothFactor = 0.3
    )
}

# LISA classes on the UTSA diverging scheme: high = Orange, low = Midnight,
# spatial outliers in the lighter tint of their own value's side
lisa_palette <- function(stat) {
  if (stat == "moran") {
    c("HH" = UTSA[["orange"]], "LL" = UTSA[["midnight"]],
      "HL" = UTSA[["mission_clay"]], "LH" = UTSA[["river_mist"]],
      "Not significant" = UTSA[["concrete"]])
  } else {
    c("Hot Spot" = UTSA[["orange"]], "Cold Spot" = UTSA[["midnight"]],
      "Not significant" = UTSA[["concrete"]])
  }
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_navbar(
  title = div(
    style = "display:flex; flex-direction:column; line-height:1.2;",
    span("County Race & Ethnicity Explorer", style = "font-weight:600;"),
    tags$a(
      "Corey Sparks, Ph.D.",
      href   = "https://www.linkedin.com/in/corey-sparks-ph-d/",
      target = "_blank",
      style  = "font-size:0.72em; font-weight:400; color:rgba(255,255,255,0.72);
                text-decoration:none;"
    )
  ),
  theme    = bs_theme(
    bootswatch   = "flatly",
    bg           = UTSA[["white"]],
    fg           = UTSA[["midnight"]],
    primary      = UTSA[["midnight"]],
    secondary    = UTSA[["smoke"]],
    "link-color" = UTSA[["talavera_blue"]],
    "navbar-bg"  = UTSA[["midnight"]]
  ) |>
    bs_add_rules(sprintf(".navbar { border-bottom: 4px solid %s; }", UTSA[["orange"]])),
  fillable = FALSE,

  # ---- PAGE 1: Choropleth map ---------------------------------------------
  nav_panel(
    "Choropleth Map",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("p1_indicator", "Variable",
          choices  = var_choices,
          selected = "pct_hispanic"),
        selectInput("p1_palette", "Color palette",
          choices  = names(UTSA_RAMPS),
          selected = "UTSA Blues"),
        selectInput("p1_class", "Classification",
          choices  = c("quantile", "jenks", "equal"),
          selected = "quantile"),
        sliderInput("p1_n", "Number of classes", min = 3, max = 9, value = 5, step = 1),
        checkboxInput("p1_rev", "Reverse palette",      value = FALSE),
        checkboxInput("p1_na",  "Show missing as grey", value = TRUE),
        hr(),
        tags$small(tags$b("Click a county to see all its race/ethnicity estimates.")),
        tags$small(class = "text-muted d-block mt-2",
          "RUCC is mapped as categories; classification settings do not apply to it.")
      ),
      withSpinner(leafletOutput("choro_map", height = "72vh"), type = 4),
      uiOutput("county_panel")
    )
  ),

  # ---- PAGE 2: LISA spatial autocorrelation --------------------------------
  nav_panel(
    "Spatial Autocorrelation (LISA)",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("p2_indicator", "Variable",
          choices  = var_choices,
          selected = "pct_hispanic"),
        selectInput("p2_stat", "Statistic",
          choices = c(
            "Local Moran's I" = "moran",
            "Local G"         = "g",
            "Local G*"        = "gstar"
          ),
          selected = "moran"),
        tags$p(tags$b("Spatial weights:"), " K-nearest neighbors (k = 4)",
               class = "mb-1 mt-2", style = "font-size:0.9em"),
        numericInput("p2_alpha", "Significance level", value = 0.05, min = 0.001, max = 0.1, step = 0.005),
        selectInput("p2_adjust", "Multiple-testing adjustment",
          choices = c("False discovery rate (Benjamini–Hochberg)" = "BH",
                      "None"                                     = "none"),
          selected = "BH"),
        actionButton("p2_run", "Run LISA", class = "btn-primary w-100 mt-2"),
        hr(),
        uiOutput("p2_legend_key")
      ),
      fluidRow(
        column(8,
          withSpinner(leafletOutput("lisa_map", height = "72vh"), type = 4)
        ),
        column(4,
          uiOutput("p2_global_header"),
          withSpinner(verbatimTextOutput("moran_out"), type = 4),
          hr(),
          h5("LISA cluster counts"),
          withSpinner(DTOutput("lisa_counts"), type = 4),
          hr(),
          tags$small(
            "Significance based on asymptotic normal approximation (two-sided),",
            "optionally adjusted for ~3,100 simultaneous local tests."
          )
        )
      )
    )
  ),

  # ---- PAGE 3: ICE Activity Risk Index ------------------------------------
  nav_panel(
    "ICE Activity Risk Index",
    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        tags$p(class = "text-muted mb-1", style = "font-size:0.85em;",
          "Composite index based on four county-level characteristics associated",
          "with ICE enforcement activity. Each component is min-max normalized",
          "to [0, 1] before weighting. Adjust weights and click", tags$b("Update."),
          "Setting a weight to 0 excludes that component."
        ),
        hr(class = "my-2"),
        sliderInput("w_pop",       "Population size (log₁₀)",
                    min = 0, max = 1, value = 0.25, step = 0.05),
        sliderInput("w_noncit",    "Non-citizens (% of total pop.)",
                    min = 0, max = 1, value = 0.35, step = 0.05),
        sliderInput("w_diversity", "Racial diversity (Shannon H)",
                    min = 0, max = 1, value = 0.20, step = 0.05),
        sliderInput("w_urban",     "Urbanicity (metro → rural inverted)",
                    min = 0, max = 1, value = 0.20, step = 0.05),
        actionButton("run_risk", "Update Map", class = "btn-primary w-100 mt-2"),
        hr(class = "my-2"),
        selectInput("risk_palette", "Color palette",
          choices  = names(UTSA_RAMPS),
          selected = "UTSA Oranges"),
        checkboxInput("risk_rev", "Reverse palette", value = FALSE),
        tags$small(class = "text-muted",
          tags$b("Note:"), " Index values are relative within the displayed dataset.",
          "This is an exploratory tool — it does not reflect actual ICE operational data."
        )
      ),
      fluidRow(
        column(8,
          withSpinner(leafletOutput("risk_map", height = "72vh"), type = 4)
        ),
        column(4,
          h5("Top 30 Counties by Risk Index"),
          withSpinner(DTOutput("risk_table"), type = 4)
        )
      )
    )
  ),

  # ---- PAGE 4: Data Sources -----------------------------------------------
  nav_panel(
    "Data Sources",
    div(class = "container mt-4", style = "max-width:860px;",

      h3("Data Sources & Documentation"),
      p(class = "text-muted",
        "This application maps county-level race and ethnic composition across the United States",
        "using survey-based estimates from the U.S. Census Bureau's American Community Survey.",
        "Data are retrieved directly from the Census Bureau API via the",
        tags$a("tidycensus", href = "https://walker-data.com/tidycensus/", target = "_blank"),
        "R package."),

      hr(),

      # ACS ------------------------------------------------------------------
      h4(tags$a("American Community Survey (ACS) 5-Year Estimates",
                href   = "https://www.census.gov/programs-surveys/acs",
                target = "_blank")),
      p(tags$b("Publisher:"), " U.S. Census Bureau."),
      uiOutput("release_info"),
      p(tags$b("Geographic unit:"), " U.S. counties; 5-digit FIPS code (3,100+ counties across 50 states + DC)."),
      p(tags$b("Why Data Profile tables?")),
      tags$ul(
        tags$li("The DP05 table provides pre-computed ", tags$b("percent estimates (PE)"),
          " alongside margins of error — no manual division required."),
        tags$li("Data Profile tables aggregate several related subject-area tables,",
          " reducing the number of API calls needed."),
        tags$li("Percent estimates in DP05 are computed over the total population",
          " denominator used by the Census Bureau, ensuring internal consistency.")
      ),
      p(tags$b("Variables for the selected release"),
        " (race/ethnicity and foreign-born are % of total population):"),
      uiOutput("var_id_table"),
      p(class = "text-muted", tags$small(
        "Race and Hispanic/Latino origin are collected as two separate questions on the ACS.",
        "Hispanic or Latino is an ethnicity category, not a race category; individuals of",
        "Hispanic or Latino origin may be of any race. The 'White alone' and 'Non-Hispanic",
        "White alone' categories therefore overlap with the 'Hispanic or Latino' category.",
        "Race categories follow the 1997 OMB standards as used by the Census Bureau.",
        "Variable IDs and label wording change between releases (IDs were renumbered in",
        "2017, 2019–2020, 2022, 2023 and 2024). IDs for each release come from",
        tags$code("data/acs_profile_lookup.csv"), ", built by", tags$code("build_lookup.R"),
        "from the Census API variable metadata by matching each variable's label path."
      )),
      p(class = "text-muted", tags$small(
        tags$b("Comparing years:"),
        "race question and coding changes entered the ACS with 2020 data. Releases through",
        "2015–2019 use the earlier coding; each later release adds another year of new-coding",
        "responses, and 2020–2024 is the first entirely under it. Nationally, 'Two or more races'",
        "rises from 3.3% (2015–2019) to 12.6% (2020–2024) and 'White alone' falls from 72.5% to",
        "61.0%, while Hispanic, non-Hispanic White, and foreign-born shares change smoothly.",
        "Compare race-alone shares and the Shannon index across the 2019/2020 boundary with caution."
      )),
      tags$ul(
        tags$li(tags$a("ACS methodology",
          href = "https://www.census.gov/programs-surveys/acs/methodology.html", target = "_blank")),
        tags$li(tags$a("Census Bureau race/ethnicity guidance",
          href = "https://www.census.gov/topics/population/race/about.html", target = "_blank"))
      ),

      hr(),

      # tidycensus -----------------------------------------------------------
      h4(tags$a("tidycensus R package",
                href   = "https://walker-data.com/tidycensus/",
                target = "_blank")),
      p(tags$b("Author:"), " Kyle Walker (Texas Christian University)."),
      p("tidycensus provides tidy-formatted access to Census Bureau APIs including",
        "the ACS, Decennial Census, and Population Estimates Program.",
        "The", tags$code("get_acs()"), "function with", tags$code("geography = 'county'"),
        "and", tags$code("survey = 'acs5'"), "retrieves estimates for all ~3,100 counties",
        "in a single API call per variable batch."),
      p(class = "text-muted", tags$small(
        "A free Census Bureau API key is required. Register at ",
        tags$a("https://api.census.gov/data/key_signup.html",
               href = "https://api.census.gov/data/key_signup.html", target = "_blank"),
        " and install with ",
        tags$code("tidycensus::census_api_key('YOUR_KEY', install = TRUE)"), "."
      )),

      hr(),

      # Geography ------------------------------------------------------------
      h4(tags$a("U.S. Census Bureau TIGER/Line Shapefiles",
                href   = "https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html",
                target = "_blank")),
      uiOutput("geo_info"),
      p(tags$b("Projection:"), " WGS 84 (EPSG:4326) for interactive display."),
      p(class = "text-muted", tags$small(
        "County boundaries are matched to each release's county codes. Changes by first",
        "release affected: Bedford city VA merged into Bedford County (2010–2014);",
        "Shannon County SD and Wade Hampton Census Area AK renamed Oglala Lakota and",
        "Kusilvak with new FIPS codes (2011–2015); Valdez-Cordova AK split into Chugach",
        "and Copper River (2016–2020); Connecticut's 8 counties replaced by 9 planning",
        "regions (2018–2022)."
      )),

      hr(),

      # Nativity & citizenship -----------------------------------------------
      h4(tags$a("Nativity & Citizenship Status — ACS DP02",
                href   = "https://data.census.gov/table/ACSDP5Y2024.DP02",
                target = "_blank")),
      p(tags$b("Table:"), " DP02 — Selected Social Characteristics in the United States."),
      p("Variables from the Place of Birth and U.S. Citizenship Status sections of DP02",
        "(IDs for the selected release are in the table above). The denominators differ:"),
      tags$ul(
        tags$li(tags$b("Foreign-born (% of total population)")),
        tags$li(tags$b("Not a U.S. citizen (% of total population)"),
                " — computed as non-citizen count ÷ place-of-birth total population × 100"),
        tags$li(tags$b("Naturalized U.S. citizen (% of foreign-born)")),
        tags$li(tags$b("Not a U.S. citizen (% of foreign-born)"))
      ),
      p(class = "text-muted", tags$small(
        "The two '% of foreign-born' measures sum to 100% and can be extreme in counties",
        "with very few foreign-born residents; use the '% of total population' measures",
        "to compare how large these groups are across counties."
      )),
      p(class = "text-muted", tags$small(
        "Foreign-born persons are those born outside the United States who are not U.S. citizens at birth.",
        "A person is a naturalized citizen if they completed the naturalization process.",
        "The 'Not a U.S. Citizen' category includes lawful permanent residents, visa holders, and undocumented persons."
      )),

      hr(),

      # USDA RUCC ------------------------------------------------------------
      h4(tags$a("USDA Rural-Urban Continuum Codes",
                href   = "https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/",
                target = "_blank")),
      p(tags$b("Publisher:"), " U.S. Department of Agriculture, Economic Research Service (USDA ERS)."),
      p("The Rural-Urban Continuum Codes (Beale Codes) classify all U.S. counties on a 1–9 scale",
        "based on degree of urbanization and adjacency to a metro area."),
      tags$ul(
        tags$li(tags$b("1"), " — Metro, ≥ 1 million population"),
        tags$li(tags$b("2"), " — Metro, 250,000–1 million"),
        tags$li(tags$b("3"), " — Metro, < 250,000"),
        tags$li(tags$b("4"), " — Nonmetro, urban ≥ 20,000, adjacent to metro"),
        tags$li(tags$b("5"), " — Nonmetro, urban ≥ 20,000, not adjacent"),
        tags$li(tags$b("6"), " — Nonmetro, urban 5,000–20,000, adjacent"),
        tags$li(tags$b("7"), " — Nonmetro, urban 5,000–20,000, not adjacent"),
        tags$li(tags$b("8"), " — Nonmetro, urban < 5,000, adjacent"),
        tags$li(tags$b("9"), " — Nonmetro, urban < 5,000, not adjacent")
      ),
      uiOutput("rucc_info"),

      hr(),

      # ICE Risk Index -------------------------------------------------------
      h4("ICE Activity Risk Index"),
      p("An exploratory composite index estimating county-level characteristics",
        "associated with ICE enforcement activity. The index combines four components,",
        "each min-max normalized to [0, 1] across all counties and then weighted:"),
      tags$ul(
        tags$li(tags$b("Population size (log₁₀):"), " larger counties have more potential targets in absolute terms."),
        tags$li(tags$b("Non-citizens (% of total population):"), " size of the non-citizen population relative to the county."),
        tags$li(tags$b("Racial diversity (Shannon H):"), " correlated with immigrant community presence."),
        tags$li(tags$b("Urbanicity:"), " metro areas have higher enforcement activity; lower RUCC = more urban = higher score.")
      ),
      p(style = sprintf("color:%s;", UTSA[["accessible_orange"]]), tags$small(tags$b("Important caveat:"),
        " This index is a hypothetical exploratory tool based on demographic correlates only.",
        " It does not incorporate actual ICE operational data, and should not be used for",
        " prediction, policy, or legal purposes."
      )),

      hr(),

      # Shannon diversity ----------------------------------------------------
      h4("Shannon Diversity Index"),
      p("The Shannon diversity index (H) summarizes racial composition into a single",
        "measure of evenness across the seven mutually-exhaustive race-alone categories",
        "(White, Black, AIAN, Asian, NHOPI, Some Other Race, Two or More Races)."),
      p(tags$b("Formula:"), tags$code("H = −∑ pᵢ · ln(pᵢ)"), ", where", tags$em("pᵢ"),
        "is the proportion of the population in race category", tags$em("i"),
        "(zero-proportion groups contribute 0 by convention)."),
      tags$ul(
        tags$li(tags$b("Minimum (H = 0):"), " county is entirely one racial group."),
        tags$li(tags$b("Maximum (H ≈ 1.946 = ln 7):"), " population is distributed equally across all 7 groups.")
      ),
      p(class = "text-muted", tags$small(
        "Hispanic/Latino origin is an ethnicity, not a race, and is not included in the Shannon calculation",
        "because it is not mutually exclusive with the race-alone categories.",
        "The Shannon index is computed from the ACS percent estimates; rounding in those estimates",
        "means the seven race-alone percents may not sum to exactly 100%."
      )),

      hr(),

      # Spatial stats --------------------------------------------------------
      h4("Spatial Statistics Methods"),
      tags$ul(
        tags$li(
          tags$b("Local Moran's I: "),
          "Anselin, L. (1995). Local indicators of spatial association—LISA. ",
          tags$i("Geographical Analysis"), ", 27(2), 93–115. ",
          tags$a("https://doi.org/10.1111/j.1538-4632.1995.tb00338.x",
                 href = "https://doi.org/10.1111/j.1538-4632.1995.tb00338.x", target = "_blank")
        ),
        tags$li(
          tags$b("Local G / G*: "),
          "Getis, A., & Ord, J. K. (1992). The analysis of spatial association by use of distance statistics. ",
          tags$i("Geographical Analysis"), ", 24(3), 189–206. ",
          tags$a("https://doi.org/10.1111/j.1538-4632.1992.tb00261.x",
                 href = "https://doi.org/10.1111/j.1538-4632.1992.tb00261.x", target = "_blank")
        ),
        tags$li(
          tags$b("R packages: "),
          tags$a("sfdep", href = "https://sfdep.josiahparry.com/", target = "_blank"), " (Parry 2023); ",
          tags$a("spdep", href = "https://r-spatial.github.io/spdep/", target = "_blank"),
          " (Bivand et al.)"
        )
      ),

      hr(),
      p(class = "text-muted", tags$small(
        "Built with R/Shiny · ",
        tags$a("Corey Sparks, Ph.D.",
               href = "https://www.linkedin.com/in/corey-sparks-ph-d/",
               target = "_blank"),
        " · Data: ACS 5-year DP05 (select year in navbar)"
      ))
    )
  ),

  nav_spacer(),
  nav_item(
    div(style = "display:flex; align-items:center; gap:6px; padding:2px 0;",
      tags$span("ACS 5-year:",
                style = "color:rgba(255,255,255,0.85); font-size:0.85em; white-space:nowrap;"),
      div(style = "width:120px;",
        selectInput("acs_year", NULL,
          choices  = setNames(available_years,
                              paste0(as.integer(available_years) - 4, "–", available_years)),
          selected = available_years[1],
          width    = "100%"
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # -- Reactive data ---------------------------------------------------------
  race_data <- reactive({
    req(input$acs_year %in% available_years)
    RACE_DATA[[input$acs_year]]
  })
  county_sf <- reactive({
    left_join(get_counties(input$acs_year), race_data(), by = c("GEOID" = "fips"))
  })
  selected_fips <- reactiveVal(NULL)
  # The clicked county's row in the *current* year, so the detail panel and
  # CSV follow the year selector instead of keeping stale values.
  selected_county_row <- reactive({
    req(selected_fips())
    rd  <- race_data()
    row <- rd[rd$fips == selected_fips(), ]
    if (nrow(row) == 0) NULL else row
  })

  output$var_id_table <- renderUI({
    lk <- ACS_LOOKUP[ACS_LOOKUP$year == as.integer(input$acs_year), ]
    labels <- c(
      total_pop = "Total population (count)", pct_white = "White alone",
      pct_black = "Black or African American alone",
      pct_aian = "American Indian & Alaska Native alone", pct_asian = "Asian alone",
      pct_nhopi = "Native Hawaiian & Other Pacific Islander alone",
      pct_other = "Some other race alone", pct_two_or_more = "Two or more races",
      pct_hispanic = "Hispanic or Latino, any race", pct_nh_white = "Non-Hispanic White alone",
      pct_foreign_born = "Foreign-born (% of total pop.)",
      pct_naturalized = "Naturalized U.S. citizen (% of foreign-born)",
      pct_noncitizen = "Not a U.S. citizen (% of foreign-born)",
      noncitizen_count = "Not a U.S. citizen (count)",
      dp02_total_pop = "Place-of-birth total population (count)"
    )
    lk <- lk[match(names(labels), lk$variable), ]
    tags$table(class = "table table-sm", style = "max-width:640px;",
      tags$thead(tags$tr(tags$th("Measure"), tags$th("Column"), tags$th("ACS variable"))),
      tags$tbody(lapply(seq_len(nrow(lk)), function(i)
        tags$tr(tags$td(labels[[lk$variable[i]]]), tags$td(tags$code(lk$variable[i])),
                tags$td(lk$id[i], title = lk$label[i]))))
    )
  })

  output$geo_info <- renderUI({
    gv <- geo_vintage(input$acs_year)
    p(tags$b("Vintage:"), sprintf(" %d", gv),
      " cartographic boundary files (1:20,000,000) via the",
      tags$a("tigris", href = "https://github.com/walkerke/tigris", target = "_blank"),
      if (gv == 2010) "R package; releases ending 2009–2012 use the 2010 files."
      else "R package, matching the release's end year.")
  })

  output$rucc_info <- renderUI({
    v <- unique(race_data()$rucc_vintage)
    p(class = "text-muted", tags$small(
      if (identical(v, 2023L))
        "This release uses the 2023 codes (2020 Census population, 2023 OMB metro definitions)."
      else paste("This release uses the 2013 codes (2010 Census population, 2013 OMB metro",
                 "definitions), matching its pre-2022 county geography. Counties created or",
                 "renamed after 2013 (Oglala Lakota SD, Kusilvak AK, Chugach and Copper River AK)",
                 "take their predecessor county's code."),
      " Codes are fixed per vintage, so they do not track urbanization between vintages.",
      " Downloaded from ",
      tags$a("USDA ERS", href = "https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/",
             target = "_blank"), "."
    ))
  })

  output$release_info <- renderUI({
    yr <- as.integer(input$acs_year)
    tagList(
      p(tags$b("Release shown:"), sprintf(" %d–%d ACS 5-Year Estimates", yr - 4, yr),
        " (change with the ACS Year selector; available: ",
        paste(available_years, collapse = ", "), ")."),
      p(tags$b("Table:"),
        tags$a("DP05 — ACS Demographic and Housing Estimates",
               href   = sprintf("https://data.census.gov/table/ACSDP5Y%d.DP05", yr),
               target = "_blank"),
        " (Data Profile table).")
    )
  })

  # =========================================================================
  # PAGE 1 — Choropleth map
  # =========================================================================
  output$choro_map <- renderLeaflet({
    base_map()
  })

  observeEvent(
    list(input$p1_indicator, input$p1_palette, input$p1_class,
         input$p1_n, input$p1_rev, input$p1_na, county_sf()),
    ignoreNULL = TRUE, {

    req(input$p1_indicator, county_sf())
    csf    <- county_sf()
    col    <- input$p1_indicator
    values <- csf[[col]]

    valid_vals <- values[!is.na(values) & is.finite(values)]
    if (length(unique(valid_vals)) < 2) return()

    na_col <- if (input$p1_na) UTSA[["concrete"]] else "#00000000"

    labels <- sprintf(
      "<b>%s</b><br/>%s: %s",
      county_label(csf),
      col_label(col),
      fmt_val(col, values)
    ) |> lapply(htmltools::HTML)

    map <- leafletProxy("choro_map") |> clearShapes() |> clearControls()

    if (col == "rucc_code") {
      # Categorical: one color per code, metro (1) darkest unless reversed
      codes  <- as.character(1:9)
      cols   <- ramp_colors(input$p1_palette, 9, reverse = !input$p1_rev)
      fills  <- ifelse(is.na(values), na_col, cols[match(as.character(values), codes)])
      map |>
        county_border(csf, fills, labels, layer_id = csf$GEOID) |>
        addLegend(
          position = "bottomright",
          colors   = c(cols, if (input$p1_na) na_col),
          labels   = c(unname(RUCC_LABELS), if (input$p1_na) "No data"),
          title    = col_label(col),
          opacity  = 0.9
        )
      return()
    }

    brks <- tryCatch(
      classIntervals(valid_vals, n = input$p1_n, style = input$p1_class)$brks,
      error = function(e) quantile(valid_vals, probs = seq(0, 1, length.out = input$p1_n + 1),
                                    na.rm = TRUE)
    )
    brks   <- unique(brks)
    pal_fn <- colorBin(ramp_colors(input$p1_palette, length(brks) - 1, input$p1_rev),
                       domain = values, bins = brks, na.color = na_col)

    map |>
      county_border(csf, pal_fn(values), labels, layer_id = csf$GEOID) |>
      addLegend(
        position  = "bottomright",
        pal       = pal_fn,
        values    = values,
        title     = col_label(col),
        opacity   = 0.9,
        na.label  = "No data",
        labFormat = if (is_pct_var(col)) labelFormat(suffix = "%")
                    else if (col == "total_pop") labelFormat(big.mark = ",", digits = 0)
                    else labelFormat(digits = 3)
      )
  })

  # County detail on click
  observeEvent(input$choro_map_shape_click, {
    req(input$choro_map_shape_click$id)
    selected_fips(input$choro_map_shape_click$id)
  })

  output$county_panel <- renderUI({
    row <- selected_county_row()
    req(row)

    make_row <- function(label, val, fmt = "pct") {
      v <- if (fmt == "pct")   { if (is.null(val) || is.na(val)) "—" else paste0(round(val, 1), "%") }
           else if (fmt == "idx") { if (is.null(val) || is.na(val)) "—" else as.character(round(val, 3)) }
           else if (fmt == "int") { if (is.null(val) || is.na(val)) "—" else formatC(as.integer(val), format = "d", big.mark = ",") }
           else                   { if (is.null(val) || is.na(val)) "—" else as.character(val) }
      data.frame(Group = label, Value = v, stringsAsFactors = FALSE)
    }

    df <- do.call(rbind, c(
      # Race & ethnicity
      lapply(names(RACE_VARS), function(m) make_row(paste0(RACE_VARS[m], " (%)"), row[[m]], "pct")),
      # Nativity & citizenship
      list(make_row("—— Nativity & Citizenship ——", NA, "raw")),
      lapply(names(NATIVITY_VARS), function(m) make_row(unname(NATIVITY_VARS[m]), row[[m]], "pct")),
      # Demographics & indices
      list(make_row("—— Demographics & Indices ——", NA, "raw")),
      list(make_row("Total Population",              row[["total_pop"]],         "int")),
      list(make_row("Shannon Diversity Index (H)",   row[["shannon_diversity"]], "idx")),
      list(make_row("USDA Rural-Urban Continuum Code", row[["rucc_code"]],       "raw"))
    ))

    div(class = "mt-3",
      wellPanel(
        h5(paste0(row$county[1], ", ", row$state[1],
                  " (FIPS: ", row$fips[1], ") — ACS ", input$acs_year), style = "margin-top:0"),
        DT::datatable(df, rownames = FALSE,
                      options = list(pageLength = 20, dom = "t", scrollY = "300px"),
                      class = "compact stripe"),
        div(class = "mt-2",
          downloadButton("download_county", "Download CSV",
                         class = "btn-sm btn-outline-primary")
        )
      )
    )
  })

  output$download_county <- downloadHandler(
    filename = function() {
      r <- selected_county_row()
      if (is.null(r)) return("county_data.csv")
      paste0(gsub("[^A-Za-z0-9]", "_", r$county[1]), "_",
             gsub("[^A-Za-z0-9]", "_", r$state[1]), "_",
             r$fips[1], "_acs", input$acs_year, ".csv")
    },
    content = function(file) {
      r <- selected_county_row()
      req(!is.null(r))
      get_val <- function(col) { v <- r[[col]]; if (is.null(v) || length(v) == 0 || is.na(v)) NA_real_ else as.numeric(v) }
      all_vars <- c(
        setNames(rep("% of total population", length(RACE_VARS)), names(RACE_VARS)),
        NATIVITY_UNITS[names(NATIVITY_VARS)],
        c(total_pop = "count", shannon_diversity = "index (0–ln7)",
          rucc_code = "code (1–9)")
      )
      all_labels <- c(unname(RACE_VARS), unname(NATIVITY_VARS),
                      "Total Population", "Shannon Diversity Index (H)",
                      "USDA Rural-Urban Continuum Code")
      pct_rows <- data.frame(
        acs_year = input$acs_year,
        fips     = r$fips[1],
        county   = r$county[1],
        state    = r$state[1],
        variable = all_labels,
        column   = names(all_vars),
        value    = sapply(names(all_vars), get_val),
        unit     = unname(all_vars),
        stringsAsFactors = FALSE
      )
      write.csv(pct_rows, file, row.names = FALSE)
    }
  )

  # =========================================================================
  # PAGE 2 — LISA
  # =========================================================================
  lisa_rv <- reactiveVal(NULL)

  output$lisa_map <- renderLeaflet({
    base_map()
  })

  observeEvent(input$p2_run, {
    req(input$p2_indicator, county_sf())

    withProgress(message = "Running LISA analysis...", value = 0, {

      csf    <- county_sf()
      col    <- input$p2_indicator
      values <- csf[[col]]

      valid <- !is.na(values) & is.finite(values)
      if (sum(valid) < 20) {
        showNotification("Too few valid observations for LISA.", type = "warning")
        return()
      }
      csf_v  <- csf[valid, ]
      vals_v <- values[valid]

      setProgress(0.2, detail = "Building k=4 nearest-neighbor weights")
      nb <- tryCatch(
        # Neighbors from interior points in an equal-area projection (Albers),
        # not from lon/lat polygons
        st_knn(st_point_on_surface(st_geometry(st_transform(csf_v, 5070))), k = 4L),
        error = function(e) {
          showNotification(paste("Neighbor error:", e$message), type = "error")
          NULL
        }
      )
      req(!is.null(nb))

      wts   <- st_weights(nb)
      listw <- sfdep::recreate_listw(nb, wts)
      stat  <- input$p2_stat

      setProgress(0.35, detail = "Computing global Moran's I")
      gmt <- tryCatch(global_moran_test(vals_v, nb, wts), error = function(e) NULL)

      setProgress(0.55, detail = paste("Computing local", switch(stat,
        moran = "Moran's I", g = "G", gstar = "G*")))

      if (stat == "moran") {
        lm <- tryCatch(
          spdep::localmoran(vals_v, listw, zero.policy = TRUE),
          error = function(e) {
            showNotification(paste("LISA error:", e$message), type = "error")
            NULL
          }
        )
        req(!is.null(lm))

        z_std <- scale(vals_v)[, 1]
        lag_z <- spdep::lag.listw(listw, z_std, zero.policy = TRUE)
        p_val <- p.adjust(lm[, "Pr(z != E(Ii))"], method = input$p2_adjust)
        sig   <- !is.na(p_val) & p_val <= input$p2_alpha

        cluster <- rep("Not significant", length(vals_v))
        cluster[sig & z_std >  0 & lag_z >  0] <- "HH"
        cluster[sig & z_std <  0 & lag_z <  0] <- "LL"
        cluster[sig & z_std >  0 & lag_z <  0] <- "HL"
        cluster[sig & z_std <  0 & lag_z >  0] <- "LH"

        stat_vals <- lm[, "Ii"]
        stat_z    <- lm[, "Z.Ii"]

      } else {
        # spdep::localG() returns the Getis-Ord statistic as a z-score. For G*
        # the focal county is added to its own neighbor set before weighting.
        g_listw <- if (stat == "gstar") {
          nb_self <- spdep::include.self(nb)
          sfdep::recreate_listw(nb_self, st_weights(nb_self))
        } else listw
        gz <- tryCatch(
          as.numeric(spdep::localG(vals_v, g_listw, zero.policy = TRUE)),
          error = function(e) {
            showNotification(paste("LISA error:", e$message), type = "error")
            NULL
          }
        )
        req(!is.null(gz))

        p_val <- p.adjust(2 * pnorm(-abs(gz)), method = input$p2_adjust)
        sig   <- !is.na(p_val) & p_val <= input$p2_alpha

        cluster <- rep("Not significant", length(vals_v))
        cluster[sig & gz > 0] <- "Hot Spot"
        cluster[sig & gz < 0] <- "Cold Spot"

        stat_vals <- gz
        stat_z    <- gz
      }
      p_sim <- p_val

      setProgress(0.9, detail = "Merging results")

      csf$lisa_cluster <- NA_character_
      csf$lisa_stat    <- NA_real_
      csf$lisa_p_sim   <- NA_real_
      csf$lisa_z       <- NA_real_

      idx <- match(csf_v$GEOID, csf$GEOID)
      csf$lisa_cluster[idx] <- cluster
      csf$lisa_stat[idx]    <- stat_vals
      csf$lisa_p_sim[idx]   <- p_sim
      csf$lisa_z[idx]       <- stat_z

      lisa_rv(list(
        sf             = csf,
        stat           = stat,
        col            = col,
        adjust         = input$p2_adjust,
        gmt            = gmt,
        cluster_counts = table(cluster)
      ))
    })
  })

  # Render LISA map
  observeEvent(lisa_rv(), {
    req(lisa_rv())
    res  <- lisa_rv()
    csf  <- res$sf
    stat <- res$stat

    cluster_pal <- lisa_palette(stat)

    quad  <- ifelse(is.na(csf$lisa_cluster), "Not significant", csf$lisa_cluster)
    fills <- unname(cluster_pal[quad])

    stat_label <- switch(stat, moran = "Local I", g = "Gi (z)", gstar = "Gi* (z)")
    p_label    <- if (res$adjust == "BH") "p (asymp, FDR-adj.)" else "p (asymp)"
    labels <- sprintf(
      "<b>%s</b><br/>%s<br/>Cluster: %s<br/>%s: %s<br/>z: %s<br/>%s: %s",
      county_label(csf),
      col_label(res$col),
      ifelse(is.na(csf$lisa_cluster), "No data", csf$lisa_cluster),
      stat_label,
      ifelse(is.na(csf$lisa_stat),  "—", round(csf$lisa_stat,  4)),
      ifelse(is.na(csf$lisa_z),     "—", round(csf$lisa_z,     4)),
      p_label,
      ifelse(is.na(csf$lisa_p_sim), "—", round(csf$lisa_p_sim, 4))
    ) |> lapply(htmltools::HTML)

    leafletProxy("lisa_map") |>
      clearShapes() |> clearControls() |>
      county_border(csf, fills, labels) |>
      addLegend(
        position = "bottomright",
        colors   = unname(cluster_pal),
        labels   = names(cluster_pal),
        title    = paste0(switch(stat, moran = "Moran's I", g = "Local G", gstar = "Local G*"),
                          " — ", col_label(res$col)),
        opacity  = 0.9
      )
  })

  # Changing the ACS year invalidates LISA results (user re-runs for the new year)
  observeEvent(input$acs_year, ignoreInit = TRUE, {
    lisa_rv(NULL)
    leafletProxy("lisa_map") |> clearShapes() |> clearControls()
  })

  # Sidebar legend key (color swatches; text stays Midnight for contrast)
  output$p2_legend_key <- renderUI({
    stat <- input$p2_stat
    pal  <- lisa_palette(stat)
    swatch <- function(key, text) tagList(
      tags$span(style = sprintf(
        "display:inline-block; width:12px; height:12px; margin-right:6px; vertical-align:middle;
         background:%s; border:1px solid %s;", pal[[key]], UTSA[["midnight"]])),
      tags$b(key), " ", text, tags$br()
    )
    tags$small(
      if (stat == "moran") tagList(
        tags$b("Local Moran's I"), " (Anselin 1995)", tags$br(), tags$br(),
        swatch("HH", "High-High cluster"),
        swatch("LL", "Low-Low cluster"),
        swatch("HL", "High outlier, low neighbors"),
        swatch("LH", "Low outlier, high neighbors")
      ) else tagList(
        if (stat == "g") tags$b("Local G (Getis-Ord)") else tags$b("Local G* (Getis-Ord)"),
        tags$br(), tags$br(),
        swatch("Hot Spot",  "Significant high cluster"),
        swatch("Cold Spot", "Significant low cluster")
      )
    )
  })

  output$p2_global_header <- renderUI({ h5("Global Moran's I") })

  output$moran_out <- renderPrint({
    req(lisa_rv())
    gmt <- lisa_rv()$gmt
    if (is.null(gmt)) { cat("Not computed\n"); return() }
    est <- gmt$estimate
    cat(sprintf("I       : %8.4f\n", est["Moran I statistic"]))
    cat(sprintf("E[I]    : %8.4f\n", est["Expectation"]))
    cat(sprintf("Var[I]  : %8.6f\n", est["Variance"]))
    cat(sprintf("z-score : %8.4f\n", gmt$statistic))
    cat(sprintf("p       : %8.5f\n", gmt$p.value))
  })

  output$lisa_counts <- DT::renderDT({
    req(lisa_rv())
    ct <- lisa_rv()$cluster_counts
    df <- as.data.frame(ct, stringsAsFactors = FALSE)
    names(df) <- c("Cluster", "n")
    df$Pct <- paste0(round(100 * df$n / sum(df$n), 1), "%")
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "t", pageLength = 10),
                  class = "compact stripe")
  })

  # =========================================================================
  # PAGE 3 — ICE Activity Risk Index
  # =========================================================================

  norm01 <- function(x) {
    rng <- range(x, na.rm = TRUE, finite = TRUE)
    if (diff(rng) == 0) return(rep(0.5, length(x)))
    (x - rng[1]) / diff(rng)
  }

  risk_data <- eventReactive(
    list(input$run_risk, county_sf()),
    {
      req(county_sf())
      csf <- county_sf()

      w_total <- input$w_pop + input$w_noncit + input$w_diversity + input$w_urban
      if (w_total == 0) {
        showNotification("All weights are 0 — set at least one weight above 0.", type = "warning")
        return(NULL)
      }

      csf$risk_index <- (
        input$w_pop       * norm01(log10(pmax(csf$total_pop, 1, na.rm = FALSE))) +
        input$w_noncit    * norm01(csf$pct_noncitizen_pop) +
        input$w_diversity * norm01(csf$shannon_diversity) +
        input$w_urban     * norm01(10L - csf$rucc_code)
      ) / w_total

      csf
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  output$risk_map <- renderLeaflet({
    base_map()
  })

  observeEvent(risk_data(), {
    req(risk_data())
    csf      <- risk_data()
    values   <- csf$risk_index
    valid    <- values[!is.na(values) & is.finite(values)]

    brks   <- unique(quantile(valid, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
    pal_fn <- colorBin(ramp_colors(isolate(input$risk_palette), length(brks) - 1,
                                   isolate(input$risk_rev)),
                       domain = values, bins = brks, na.color = UTSA[["concrete"]])

    labels <- sprintf(
      "<b>%s</b><br/>Risk Index: %s<br/>Pop: %s<br/>Non-citizens (%% of pop.): %s<br/>Shannon H: %s<br/>RUCC: %s",
      county_label(csf),
      ifelse(is.na(values), "No data", round(values, 3)),
      ifelse(is.na(csf$total_pop),   "—", formatC(as.integer(csf$total_pop), format = "d", big.mark = ",")),
      ifelse(is.na(csf$pct_noncitizen_pop), "—", paste0(round(csf$pct_noncitizen_pop, 1), "%")),
      ifelse(is.na(csf$shannon_diversity), "—", round(csf$shannon_diversity, 3)),
      ifelse(is.na(csf$rucc_code), "—", as.integer(csf$rucc_code))
    ) |> lapply(htmltools::HTML)

    leafletProxy("risk_map") |>
      clearShapes() |> clearControls() |>
      county_border(csf, pal_fn(values), labels) |>
      addLegend(
        position  = "bottomright",
        pal       = pal_fn,
        values    = values,
        title     = "ICE Risk Index",
        opacity   = 0.9,
        na.label  = "No data",
        labFormat = labelFormat(digits = 3)
      )
  })

  output$risk_table <- DT::renderDT({
    req(risk_data())
    csf <- risk_data()
    df  <- as.data.frame(csf) |>
      filter(!is.na(risk_index)) |>
      arrange(desc(risk_index)) |>
      slice_head(n = 30) |>
      transmute(
        County       = paste0(ifelse(is.na(county), NAME, county), ", ", state),
        `Risk Index` = round(risk_index, 3),
        `Pop.`       = formatC(as.integer(total_pop), format = "d", big.mark = ","),
        `Non-cit. % pop.` = ifelse(is.na(pct_noncitizen_pop), "—", paste0(round(pct_noncitizen_pop, 1), "%")),
        `Shannon H`  = round(shannon_diversity, 3),
        `RUCC`       = rucc_code
      )
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "t", pageLength = 30, scrollY = "60vh"),
                  class = "compact stripe")
  })
}

shinyApp(ui, server)
