library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(arrow)
library(tigris)
library(classInt)
library(RColorBrewer)
library(sfdep)
library(spdep)
library(DT)
library(shinycssloaders)

# Resolve DT vs shiny naming conflict
dataTableOutput <- DT::dataTableOutput
renderDataTable <- DT::renderDataTable

options(tigris_use_cache = TRUE)

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

# Nativity & citizenship percent estimates (from ACS DP02)
NATIVITY_VARS <- c(
  pct_foreign_born = "Foreign-Born Population",
  pct_naturalized  = "Naturalized U.S. Citizen",
  pct_noncitizen   = "Not a U.S. Citizen"
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
  "Nativity & Citizenship (%)" = setNames(names(NATIVITY_VARS), paste0(NATIVITY_VARS, " (%)")),
  "Diversity & Demographics"   = setNames(names(IDX_VARS),      IDX_VARS)
)

is_pct_var <- function(col) col %in% c(names(RACE_VARS), names(NATIVITY_VARS))

col_label <- function(col) {
  if (col %in% names(RACE_VARS))     return(paste0(RACE_VARS[col],     " (%)"))
  if (col %in% names(NATIVITY_VARS)) return(paste0(NATIVITY_VARS[col], " (%)"))
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

# Scan data/processed/ for year-stamped parquet files.
# Falls back to the legacy un-stamped file (labelled as 2023) if present.
list_available_years <- function() {
  yr_files <- list.files("data/processed",
                         pattern = "^county_race_ethnicity_\\d{4}\\.parquet$")
  if (length(yr_files) > 0) {
    years <- sub(".*_(\\d{4})\\.parquet$", "\\1", yr_files)
    return(sort(years, decreasing = TRUE))   # most-recent first
  }
  if (file.exists("data/processed/county_race_ethnicity.parquet")) return("2023")
  character(0)
}

available_years <- list_available_years()

load_race <- function(year = NULL) {
  path <- if (!is.null(year) && nchar(year) == 4) {
    yr_path <- sprintf("data/processed/county_race_ethnicity_%s.parquet", year)
    if (file.exists(yr_path)) yr_path
    else "data/processed/county_race_ethnicity.parquet"   # legacy fallback
  } else {
    "data/processed/county_race_ethnicity.parquet"
  }
  df <- arrow::read_parquet(path)
  df$fips <- formatC(as.character(df$fips), width = 5, flag = "0")
  df
}

load_geo <- function() {
  skip <- c("60", "66", "69", "72", "78")   # territories: AS, GU, MP, PR, VI
  co <- tigris::counties(cb = TRUE, resolution = "20m", year = 2022, progress_bar = FALSE)
  st <- tigris::states( cb = TRUE, resolution = "20m", year = 2022, progress_bar = FALSE)
  co <- co[!co$STATEFP %in% skip, ]
  st <- st[!st$STATEFP %in% skip, ]
  list(
    counties = sf::st_transform(co, 4326),
    states   = sf::st_transform(st, 4326)
  )
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
  theme    = bs_theme(bootswatch = "flatly"),
  fillable = FALSE,

  # ---- PAGE 1: Choropleth map ---------------------------------------------
  nav_panel(
    "Choropleth Map",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("p1_indicator", "Race / Ethnicity group",
          choices  = var_choices,
          selected = "pct_hispanic"),
        selectInput("p1_palette", "Color palette",
          choices  = c("YlOrRd", "YlGnBu", "RdPu", "BuPu", "Greens", "Blues", "Oranges", "Purples"),
          selected = "YlOrRd"),
        selectInput("p1_class", "Classification",
          choices  = c("quantile", "jenks", "equal"),
          selected = "quantile"),
        sliderInput("p1_n", "Number of classes", min = 3, max = 9, value = 5, step = 1),
        checkboxInput("p1_rev", "Reverse palette",      value = FALSE),
        checkboxInput("p1_na",  "Show missing as grey", value = TRUE),
        hr(),
        tags$small(tags$b("Click a county to see all its race/ethnicity estimates."))
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
        selectInput("p2_indicator", "Race / Ethnicity group",
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
            "Significance based on asymptotic normal approximation (two-sided)."
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
        sliderInput("w_noncit",    "% Not a U.S. citizen",
                    min = 0, max = 1, value = 0.35, step = 0.05),
        sliderInput("w_diversity", "Racial diversity (Shannon H)",
                    min = 0, max = 1, value = 0.20, step = 0.05),
        sliderInput("w_urban",     "Urbanicity (metro → rural inverted)",
                    min = 0, max = 1, value = 0.20, step = 0.05),
        actionButton("run_risk", "Update Map", class = "btn-primary w-100 mt-2"),
        hr(class = "my-2"),
        selectInput("risk_palette", "Color palette",
          choices  = c("YlOrRd", "Reds", "OrRd", "RdPu", "YlOrBr"),
          selected = "YlOrRd"),
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
        "using model-based estimates from the U.S. Census Bureau's American Community Survey.",
        "Data are retrieved directly from the Census Bureau API via the",
        tags$a("tidycensus", href = "https://walker-data.com/tidycensus/", target = "_blank"),
        "R package."),

      hr(),

      # ACS ------------------------------------------------------------------
      h4(tags$a("American Community Survey (ACS) 5-Year Estimates",
                href   = "https://www.census.gov/programs-surveys/acs",
                target = "_blank")),
      p(tags$b("Publisher:"), " U.S. Census Bureau."),
      p(tags$b("Release used:"), " 2019–2023 ACS 5-Year Estimates."),
      p(tags$b("Table:"),
        tags$a("DP05 — ACS Demographic and Housing Estimates",
               href   = "https://data.census.gov/table/ACSDP5Y2023.DP05",
               target = "_blank"),
        " (Data Profile table)."),
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
      p(tags$b("Race/Ethnicity measures included:")),
      tags$ul(
        tags$li(tags$b("White alone (%) "), "— DP05_0037PE"),
        tags$li(tags$b("Black or African American alone (%) "), "— DP05_0038PE"),
        tags$li(tags$b("American Indian & Alaska Native alone (%) "), "— DP05_0039PE"),
        tags$li(tags$b("Asian alone (%) "), "— DP05_0040PE"),
        tags$li(tags$b("Native Hawaiian & Other Pacific Islander alone (%) "), "— DP05_0041PE"),
        tags$li(tags$b("Some other race alone (%) "), "— DP05_0044PE"),
        tags$li(tags$b("Two or more races (%) "), "— DP05_0045PE"),
        tags$li(tags$b("Hispanic or Latino, any race (%) "), "— DP05_0071PE"),
        tags$li(tags$b("Non-Hispanic White alone (%) "), "— DP05_0077PE")
      ),
      p(class = "text-muted", tags$small(
        "Race and Hispanic/Latino origin are collected as two separate questions on the ACS.",
        "Hispanic or Latino is an ethnicity category, not a race category; individuals of",
        "Hispanic or Latino origin may be of any race. The 'White alone' and 'Non-Hispanic",
        "White alone' categories therefore overlap with the 'Hispanic or Latino' category.",
        "Race categories follow the 1997 OMB standards as used by the Census Bureau.",
        "Variable IDs are specific to the 2023 5-year ACS release; run",
        tags$code("tidycensus::load_variables(2023, 'acs5/profile')"),
        "to verify if using a different release year."
      )),
      tags$ul(
        tags$li(tags$a("ACS methodology",
          href = "https://www.census.gov/programs-surveys/acs/methodology.html", target = "_blank")),
        tags$li(tags$a("DP05 table on data.census.gov",
          href = "https://data.census.gov/table/ACSDP5Y2023.DP05", target = "_blank")),
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
      p(tags$b("Vintage:"), " 2022 (1:20,000,000 cartographic boundary files via the",
        tags$a("tigris", href = "https://github.com/walkerke/tigris", target = "_blank"),
        "R package)."),
      p(tags$b("Projection:"), " WGS 84 (EPSG:4326) for interactive display."),
      p(class = "text-muted", tags$small(
        "The 2022 TIGER/Line files use 2020 Census county definitions.",
        "Connecticut uses 9 planning regions as county equivalents,",
        "consistent with the 2019–2023 ACS 5-year geography."
      )),

      hr(),

      # Nativity & citizenship -----------------------------------------------
      h4(tags$a("Nativity & Citizenship Status — ACS DP02",
                href   = "https://data.census.gov/table/ACSDP5Y2023.DP02",
                target = "_blank")),
      p(tags$b("Table:"), " DP02 — Selected Social Characteristics in the United States."),
      p("Three percent-estimate variables from the U.S. Citizenship Status section of DP02",
        "are included. All percentages are relative to the total civilian non-institutionalized population."),
      tags$ul(
        tags$li(tags$b("Foreign-Born Population (%) "), "— DP02_0095PE"),
        tags$li(tags$b("Naturalized U.S. Citizen (%) "), "— DP02_0096PE"),
        tags$li(tags$b("Not a U.S. Citizen (%) "), "— DP02_0097PE")
      ),
      p(class = "text-muted", tags$small(
        "Foreign-born persons are those born outside the United States who are not U.S. citizens at birth.",
        "A person is a naturalized citizen if they completed the naturalization process.",
        "The 'Not a U.S. Citizen' category includes lawful permanent residents, visa holders, and undocumented persons."
      )),

      hr(),

      # USDA RUCC ------------------------------------------------------------
      h4(tags$a("USDA Rural-Urban Continuum Codes (2023)",
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
      p(class = "text-muted", tags$small(
        "The 2023 codes are based on 2020 Census population data and 2023 OMB metro area definitions.",
        "Downloaded as CSV from ",
        tags$a("USDA ERS",
               href = "https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/",
               target = "_blank"), "."
      )),

      hr(),

      # ICE Risk Index -------------------------------------------------------
      h4("ICE Activity Risk Index"),
      p("An exploratory composite index estimating county-level characteristics",
        "associated with ICE enforcement activity. The index combines four components,",
        "each min-max normalized to [0, 1] across all counties and then weighted:"),
      tags$ul(
        tags$li(tags$b("Population size (log₁₀):"), " larger counties have more potential targets in absolute terms."),
        tags$li(tags$b("% Not a U.S. Citizen:"), " directly measures the at-risk population."),
        tags$li(tags$b("Racial diversity (Shannon H):"), " correlated with immigrant community presence."),
        tags$li(tags$b("Urbanicity:"), " metro areas have higher enforcement activity; lower RUCC = more urban = higher score.")
      ),
      p(class = "text-danger", tags$small(tags$b("Important caveat:"),
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
      tags$span("ACS Year:",
                style = "color:rgba(255,255,255,0.85); font-size:0.85em; white-space:nowrap;"),
      div(style = "width:88px;",
        selectInput("acs_year", NULL,
          choices  = if (length(available_years) > 0) available_years else "2023",
          selected = if (length(available_years) > 0) available_years[1] else "2023",
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
  race_data <- reactive({ load_race(input$acs_year) })
  geo       <- reactive({ load_geo()  })
  county_sf <- reactive({
    left_join(geo()$counties, race_data(), by = c("GEOID" = "fips"))
  })
  selected_county_row <- reactiveVal(NULL)

  # =========================================================================
  # PAGE 1 — Choropleth map
  # =========================================================================
  output$choro_map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -96, lat = 38, zoom = 4)
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

    pal_name <- if (input$p1_rev) paste0("-", input$p1_palette) else input$p1_palette
    brks <- tryCatch(
      classIntervals(valid_vals, n = input$p1_n, style = input$p1_class)$brks,
      error = function(e) quantile(valid_vals, probs = seq(0, 1, length.out = input$p1_n + 1),
                                    na.rm = TRUE)
    )
    brks   <- unique(brks)
    na_col <- if (input$p1_na) "#AAAAAA88" else "#00000000"
    pal_fn <- colorBin(pal_name, domain = values, bins = brks, na.color = na_col)

    labels <- sprintf(
      "<b>%s</b><br/>%s: %s",
      paste0(csf$NAME, ", ", csf$state),
      col_label(col),
      fmt_val(col, values)
    ) |> lapply(htmltools::HTML)

    leafletProxy("choro_map") |>
      clearShapes() |> clearControls() |>
      addPolygons(
        data             = csf,
        fillColor        = ~pal_fn(get(col)),
        fillOpacity      = 0.75,
        color            = "#BBBBBB",
        weight           = 0.4,
        opacity          = 0.7,
        smoothFactor     = 0.3,
        label            = labels,
        layerId          = ~GEOID,
        highlightOptions = highlightOptions(
          weight = 1.8, color = "#444", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) |>
      addPolylines(
        data         = geo()$states,
        color        = "#111111",
        weight       = 0.9,
        opacity      = 0.9,
        smoothFactor = 0.3
      ) |>
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
    click <- input$choro_map_shape_click
    req(click$id)
    rd  <- race_data()
    row <- rd[rd$fips == click$id, ]
    if (nrow(row) == 0) return()
    selected_county_row(row)

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
      lapply(names(NATIVITY_VARS), function(m) make_row(paste0(NATIVITY_VARS[m], " (%)"), row[[m]], "pct")),
      # Demographics & indices
      list(make_row("—— Demographics & Indices ——", NA, "raw")),
      list(make_row("Total Population",              row[["total_pop"]],         "int")),
      list(make_row("Shannon Diversity Index (H)",   row[["shannon_diversity"]], "idx")),
      list(make_row("USDA Rural-Urban Continuum Code", row[["rucc_code"]],       "raw"))
    ))

    output$county_panel <- renderUI({
      div(class = "mt-3",
        wellPanel(
          h5(paste0(row$county[1], ", ", row$state[1],
                    " (FIPS: ", click$id, ")"), style = "margin-top:0"),
          DT::datatable(df, rownames = FALSE,
                        options = list(pageLength = 10, dom = "t", scrollY = "300px"),
                        class = "compact stripe"),
          div(class = "mt-2",
            downloadButton("download_county", "Download CSV",
                           class = "btn-sm btn-outline-secondary")
          )
        )
      )
    })
  })

  output$download_county <- downloadHandler(
    filename = function() {
      r <- selected_county_row()
      if (is.null(r)) return("county_data.csv")
      paste0(gsub("[^A-Za-z0-9]", "_", r$county[1]), "_",
             gsub("[^A-Za-z0-9]", "_", r$state[1]), "_",
             r$fips[1], ".csv")
    },
    content = function(file) {
      r <- selected_county_row()
      req(!is.null(r))
      get_val <- function(col) { v <- r[[col]]; if (is.null(v) || length(v) == 0 || is.na(v)) NA_real_ else as.numeric(v) }
      all_vars <- c(
        setNames(rep("%",          length(RACE_VARS)),     names(RACE_VARS)),
        setNames(rep("%",          length(NATIVITY_VARS)), names(NATIVITY_VARS)),
        c(total_pop = "count", shannon_diversity = "index (0–ln7)",
          rucc_code = "code (1–9)")
      )
      all_labels <- c(unname(RACE_VARS), unname(NATIVITY_VARS),
                      "Total Population", "Shannon Diversity Index (H)",
                      "USDA Rural-Urban Continuum Code")
      pct_rows <- data.frame(
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
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -96, lat = 38, zoom = 4)
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
        st_knn(st_geometry(csf_v), k = 4L),
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
        p_val <- lm[, "Pr(z != E(Ii))"]
        sig   <- !is.na(p_val) & p_val <= input$p2_alpha

        cluster <- rep("Not significant", length(vals_v))
        cluster[sig & z_std >  0 & lag_z >  0] <- "HH"
        cluster[sig & z_std <  0 & lag_z <  0] <- "LL"
        cluster[sig & z_std >  0 & lag_z <  0] <- "HL"
        cluster[sig & z_std <  0 & lag_z >  0] <- "LH"

        stat_vals <- lm[, "Ii"]
        stat_z    <- lm[, "Z.Ii"]
        p_sim     <- p_val

      } else {
        lg <- tryCatch(
          if (stat == "g") local_g(vals_v, nb, wts)
          else             local_gstar(vals_v, nb, wts),
          error = function(e) {
            showNotification(paste("LISA error:", e$message), type = "error")
            NULL
          }
        )
        req(!is.null(lg))

        p_val <- lg$p_value
        sig   <- !is.na(p_val) & p_val <= input$p2_alpha

        cluster <- rep("Not significant", length(vals_v))
        cluster[sig & lg$cluster == "High"] <- "Hot Spot"
        cluster[sig & lg$cluster == "Low"]  <- "Cold Spot"

        stat_col  <- if (stat == "g") "gi" else "gi_star"
        stat_vals <- lg[[stat_col]]
        stat_z    <- lg$std_dev
        p_sim     <- p_val
      }

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

    cluster_pal <- if (stat == "moran") {
      c("HH" = "#d7191c", "LL" = "#2c7bb6", "HL" = "#fdae61",
        "LH" = "#4dac26", "Not significant" = "#e8e8e8")
    } else {
      c("Hot Spot" = "#d7191c", "Cold Spot" = "#2c7bb6", "Not significant" = "#e8e8e8")
    }

    quad <- ifelse(is.na(csf$lisa_cluster), "Not significant", csf$lisa_cluster)
    csf$fill_color <- unname(cluster_pal[quad])

    stat_label <- switch(stat, moran = "Local I", g = "Gi", gstar = "Gi*")
    p2_col <- isolate(input$p2_indicator)
    labels <- sprintf(
      "<b>%s</b><br/>%s<br/>Cluster: %s<br/>%s: %s<br/>z: %s<br/>p (asymp): %s",
      paste0(csf$NAME, ", ", csf$state),
      col_label(p2_col),
      ifelse(is.na(csf$lisa_cluster), "No data", csf$lisa_cluster),
      stat_label,
      ifelse(is.na(csf$lisa_stat),  "—", round(csf$lisa_stat,  4)),
      ifelse(is.na(csf$lisa_z),     "—", round(csf$lisa_z,     4)),
      ifelse(is.na(csf$lisa_p_sim), "—", round(csf$lisa_p_sim, 4))
    ) |> lapply(htmltools::HTML)

    leafletProxy("lisa_map") |>
      clearShapes() |> clearControls() |>
      addPolygons(
        data             = csf,
        fillColor        = ~fill_color,
        fillOpacity      = 0.75,
        color            = "#BBBBBB",
        weight           = 0.4,
        opacity          = 0.7,
        smoothFactor     = 0.3,
        label            = labels,
        highlightOptions = highlightOptions(
          weight = 1.8, color = "#444", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) |>
      addPolylines(
        data         = geo()$states,
        color        = "#111111",
        weight       = 0.9,
        opacity      = 0.9,
        smoothFactor = 0.3
      ) |>
      addLegend(
        position = "bottomright",
        colors   = unname(cluster_pal),
        labels   = names(cluster_pal),
        title    = paste0(switch(stat, moran = "Moran's I", g = "Local G", gstar = "Local G*"),
                          " — ", col_label(input$p2_indicator)),
        opacity  = 0.9
      )
  })

  # Sidebar legend key
  output$p2_legend_key <- renderUI({
    stat <- input$p2_stat
    tags$small(
      if (stat == "moran") tagList(
        tags$b("Local Moran's I"), " (Anselin 1995)", tags$br(), tags$br(),
        tags$span(style = "color:#d7191c", tags$b("HH")), " High-High cluster", tags$br(),
        tags$span(style = "color:#2c7bb6", tags$b("LL")), " Low-Low cluster",   tags$br(),
        tags$span(style = "color:#fdae61", tags$b("HL")), " High outlier, low neighbors", tags$br(),
        tags$span(style = "color:#4dac26", tags$b("LH")), " Low outlier, high neighbors"
      ) else tagList(
        if (stat == "g") tags$b("Local G (Getis-Ord)") else tags$b("Local G* (Getis-Ord)"),
        tags$br(), tags$br(),
        tags$span(style = "color:#d7191c", tags$b("Hot Spot")), " Significant high cluster", tags$br(),
        tags$span(style = "color:#2c7bb6", tags$b("Cold Spot")), " Significant low cluster"
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
        input$w_noncit    * norm01(csf$pct_noncitizen) +
        input$w_diversity * norm01(csf$shannon_diversity) +
        input$w_urban     * norm01(10L - csf$rucc_code)
      ) / w_total

      csf
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  output$risk_map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -96, lat = 38, zoom = 4)
  })

  observeEvent(risk_data(), {
    req(risk_data())
    csf      <- risk_data()
    pal_name <- if (isolate(input$risk_rev)) paste0("-", input$risk_palette)
                else input$risk_palette
    values   <- csf$risk_index
    valid    <- values[!is.na(values) & is.finite(values)]

    brks   <- unique(quantile(valid, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
    pal_fn <- colorBin(pal_name, domain = values, bins = brks, na.color = "#AAAAAA88")

    labels <- sprintf(
      "<b>%s</b><br/>Risk Index: %s<br/>Pop: %s<br/>Non-citizen: %s<br/>Shannon H: %s<br/>RUCC: %s",
      paste0(csf$NAME, ", ", csf$state),
      ifelse(is.na(values), "No data", round(values, 3)),
      ifelse(is.na(csf$total_pop),   "—", formatC(as.integer(csf$total_pop), format = "d", big.mark = ",")),
      ifelse(is.na(csf$pct_noncitizen), "—", paste0(round(csf$pct_noncitizen, 1), "%")),
      ifelse(is.na(csf$shannon_diversity), "—", round(csf$shannon_diversity, 3)),
      ifelse(is.na(csf$rucc_code), "—", as.integer(csf$rucc_code))
    ) |> lapply(htmltools::HTML)

    leafletProxy("risk_map") |>
      clearShapes() |> clearControls() |>
      addPolygons(
        data             = csf,
        fillColor        = ~pal_fn(risk_index),
        fillOpacity      = 0.75,
        color            = "#BBBBBB",
        weight           = 0.4,
        opacity          = 0.7,
        smoothFactor     = 0.3,
        label            = labels,
        highlightOptions = highlightOptions(
          weight = 1.8, color = "#444", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) |>
      addPolylines(
        data         = geo()$states,
        color        = "#111111",
        weight       = 0.9,
        opacity      = 0.9,
        smoothFactor = 0.3
      ) |>
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
        County       = paste0(NAME, ", ", state),
        `Risk Index` = round(risk_index, 3),
        `Pop.`       = formatC(as.integer(total_pop), format = "d", big.mark = ","),
        `Non-cit. %` = ifelse(is.na(pct_noncitizen), "—", paste0(round(pct_noncitizen, 1), "%")),
        `Shannon H`  = round(shannon_diversity, 3),
        `RUCC`       = rucc_code
      )
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "t", pageLength = 30, scrollY = "60vh"),
                  class = "compact stripe")
  })
}

shinyApp(ui, server)
