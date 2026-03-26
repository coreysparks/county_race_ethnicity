#!/usr/bin/env Rscript
# prep_data.R
# Pulls ACS 5-year race/ethnicity, nativity/citizenship, and total population
# estimates for all US counties from DP05 and DP02 data profile tables via
# tidycensus, merges USDA Rural-Urban Continuum Codes, and saves a wide-format
# parquet file.
#
# Run once before launching app.R:
#   source("prep_data.R")
#
# Requires a Census API key. If you don't have one, register at:
#   https://api.census.gov/data/key_signup.html
# Then call: tidycensus::census_api_key("YOUR_KEY", install = TRUE)

library(tidycensus)
library(dplyr)
library(tidyr)
library(arrow)
library(readr)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
YEAR   <- 2024   # End year of 5-year ACS (2020–2024)
SURVEY <- "acs5"

# ---------------------------------------------------------------------------
# Steps 1 & 2 — Resolve variable IDs dynamically via load_variables()
# ---------------------------------------------------------------------------
# Variable IDs in DP05/DP02 shift between ACS release years. We look up the
# correct IDs by matching label text so the script works for any year without
# manual edits. Hardcoded 2023-validated IDs are used only if the API is down.

# Helper: extract the last !! segment of a hierarchical ACS label
last_seg <- function(x) sub(".*!!", "", x)

# Helper: find a PE (percent estimate) variable by matching its last label segment.
# exclude_label: drop rows where the full label matches this pattern.
# include_label: keep only rows where the full label matches this pattern.
find_pe_var <- function(defs, table_prefix, seg_pattern,
                        exclude_label = NULL, include_label = NULL) {
  hits <- defs |>
    filter(startsWith(name, table_prefix), endsWith(name, "PE")) |>
    filter(grepl(seg_pattern, last_seg(label), ignore.case = TRUE, perl = TRUE))
  if (!is.null(exclude_label))
    hits <- hits |> filter(!grepl(exclude_label, label, ignore.case = TRUE))
  if (!is.null(include_label))
    hits <- hits |> filter(grepl(include_label, label, ignore.case = TRUE))
  if (nrow(hits) == 0) return(NA_character_)
  tail(hits$name, 1)
}

# Helper: find a count estimate (E) variable — used when PE is unavailable.
find_e_var <- function(defs, table_prefix, seg_pattern,
                       exclude_label = NULL, include_label = NULL) {
  hits <- defs |>
    filter(startsWith(name, table_prefix),
           endsWith(name, "E"), !endsWith(name, "PE"), !endsWith(name, "ME")) |>
    filter(grepl(seg_pattern, last_seg(label), ignore.case = TRUE, perl = TRUE))
  if (!is.null(exclude_label))
    hits <- hits |> filter(!grepl(exclude_label, label, ignore.case = TRUE))
  if (!is.null(include_label))
    hits <- hits |> filter(grepl(include_label, label, ignore.case = TRUE))
  if (nrow(hits) == 0) return(NA_character_)
  tail(hits$name, 1)
}

# Hardcoded fallback IDs (validated for 2024 ACS 5-year).
# DP05 race variable IDs shifted significantly in 2024 due to expanded racial detail categories.
# DP02 citizenship vars are now count (E) variables in 2024, not PE.
FALLBACK <- list(
  race_vars = c(
    pct_white       = "DP05_0037PE",  # Estimate!!RACE!!...!!One race!!White
    pct_black       = "DP05_0045PE",  # Estimate!!RACE!!...!!One race!!Black or African American
    pct_aian        = "DP05_0053PE",  # Estimate!!RACE!!...!!One race!!American Indian and Alaska Native
    pct_asian       = "DP05_0061PE",  # Estimate!!RACE!!...!!One race!!Asian
    pct_nhopi       = "DP05_0069PE",  # Estimate!!RACE!!...!!One race!!Native Hawaiian and Other Pacific Islander
    pct_other       = "DP05_0074PE",  # Estimate!!RACE!!...!!One race!!Some other race
    pct_two_or_more = "DP05_0075PE",  # Estimate!!RACE!!...!!Two or More Races
    pct_hispanic    = "DP05_0090PE",  # Estimate!!HISPANIC OR LATINO AND RACE!!...!!Hispanic or Latino (of any race)
    pct_nh_white    = "DP05_0096PE"   # Estimate!!HISPANIC OR LATINO AND RACE!!...!!Not Hispanic or Latino!!White alone
  ),
  count_vars = c(
    total_pop = "DP05_0033E"          # Estimate!!RACE!!Total population
  ),
  citizenship_vars = c(               # DP02 nativity: counts (E) in 2024, not PE
    pct_foreign_born = "DP02_0094PE",  # Estimate!!PLACE OF BIRTH!!Total population!!Foreign-born
    pct_naturalized  = "DP02_0096PE",  # Estimate!!U.S. CITIZENSHIP STATUS!!Foreign-born population!!Naturalized U.S. citizen
    pct_noncitizen   = "DP02_0097PE"   # Estimate!!U.S. CITIZENSHIP STATUS!!Foreign-born population!!Not a U.S. citizen
  ),
  citizenship_is_count = c(
    pct_foreign_born = TRUE,
    pct_naturalized  = TRUE,
    pct_noncitizen   = TRUE
  )
)

cat("Loading ACS variable definitions for", YEAR, SURVEY, "...\n")
all_defs <- tryCatch(
  load_variables(YEAR, paste0(SURVEY, "/profile"), cache = TRUE),
  error = function(e) {
    warning("load_variables() failed: ", e$message,
            "\nUsing hardcoded fallback variable IDs (validated for 2024).")
    NULL
  }
)

if (!is.null(all_defs)) {
  # 2024 labels dropped "alone" and added "One race!!" prefix for individual race categories.
  # Patterns use optional "( alone)?" so they match both 2023 and 2024 label structures.
  # pct_white: exclude the "HISPANIC OR LATINO AND RACE" section (has its own White rows).
  # pct_nh_white: require "Not Hispanic or Latino" in the full label path.
  race_vars <- c(
    pct_white       = find_pe_var(all_defs, "DP05", "^White( alone)?$",
                                  exclude_label = "HISPANIC OR LATINO AND RACE"),
    pct_black       = find_pe_var(all_defs, "DP05", "^Black or African American( alone)?$"),
    pct_aian        = find_pe_var(all_defs, "DP05", "^American Indian and Alaska Native( alone)?$"),
    pct_asian       = find_pe_var(all_defs, "DP05", "^Asian( alone)?$"),
    pct_nhopi       = find_pe_var(all_defs, "DP05",
                                  "^Native Hawaiian and Other Pacific Islander( alone)?$"),
    pct_other       = find_pe_var(all_defs, "DP05", "^Some other race( alone)?$"),
    pct_two_or_more = find_pe_var(all_defs, "DP05", "^Two or [Mm]ore [Rr]aces$"),
    pct_hispanic    = find_pe_var(all_defs, "DP05", "Hispanic or Latino \\(of any race\\)"),
    pct_nh_white    = find_pe_var(all_defs, "DP05", "^White alone$",
                                  include_label = "Not Hispanic or Latino")
  )

  # Total population: first E variable in DP05 whose last segment is "Total population"
  tp_hits <- all_defs |>
    filter(startsWith(name, "DP05"), endsWith(name, "E"),
           !endsWith(name, "PE"), !endsWith(name, "ME"),
           last_seg(label) == "Total population") |>
    arrange(name)
  count_vars <- c(total_pop = if (nrow(tp_hits) > 0) tp_hits$name[1] else "DP05_0033E")

  # Replace any NAs in race_vars with hardcoded fallbacks and warn
  nas <- is.na(race_vars)
  if (any(nas)) {
    warning("Could not find ", YEAR, " race IDs for: ",
            paste(names(race_vars)[nas], collapse = ", "),
            " — using hardcoded fallbacks.")
    race_vars[nas] <- FALLBACK$race_vars[names(race_vars)[nas]]
  }

  # --- DP02 citizenship variables ---
  # Print all DP02 citizenship-related variables so IDs and types can be verified
  cat("\n=== DP02 citizenship/nativity variables for", YEAR,
      "(PE = percent, E = count) ===\n")
  dp02_cit_defs <- all_defs |>
    filter(startsWith(name, "DP02")) |>
    filter(grepl("FOREIGN|BORN|CITIZEN|NATURALI", label, ignore.case = TRUE))
  print(dp02_cit_defs |> select(name, label), n = 40)

  # Try PE (percent) first; if unavailable fall back to E (count).
  # Counts will be divided by total_pop in Step 4 to produce percentages.
  find_cit_var <- function(patterns) {
    for (pat in patterns) {
      id <- find_pe_var(all_defs, "DP02", pat)
      if (!is.na(id)) return(list(id = id, is_count = FALSE))
    }
    for (pat in patterns) {
      id <- find_e_var(all_defs, "DP02", pat)
      if (!is.na(id)) return(list(id = id, is_count = TRUE))
    }
    list(id = NA_character_, is_count = FALSE)
  }

  fb   <- find_cit_var(c("^Foreign.born$", "foreign.born population",
                          "foreign born population", "^Foreign born$"))
  nat  <- find_cit_var(c("Naturalized U\\.S\\. citizen", "Naturalized citizen"))
  nonc <- find_cit_var(c("^Not a U\\.S\\. citizen$", "Not a citizen"))

  citizenship_vars <- c(
    pct_foreign_born = if (is.na(fb$id))   unname(FALLBACK$citizenship_vars["pct_foreign_born"]) else fb$id,
    pct_naturalized  = if (is.na(nat$id))  unname(FALLBACK$citizenship_vars["pct_naturalized"])  else nat$id,
    pct_noncitizen   = if (is.na(nonc$id)) unname(FALLBACK$citizenship_vars["pct_noncitizen"])   else nonc$id
  )
  citizenship_is_count <- c(
    pct_foreign_born = isTRUE(fb$is_count),
    pct_naturalized  = isTRUE(nat$is_count),
    pct_noncitizen   = isTRUE(nonc$is_count)
  )

  cat("\nResolved variable IDs for", YEAR, ":\n")
  for (n in names(race_vars))        cat("  ", n, "=", race_vars[n], "\n")
  for (n in names(citizenship_vars)) cat("  ", n, "=", citizenship_vars[n],
                                         if (isTRUE(citizenship_is_count[n])) "(COUNT)" else "(PCT)", "\n")
  cat("  total_pop =", count_vars["total_pop"], "\n")

} else {
  cat("Using hardcoded fallback variable IDs (validated for 2024).\n")
  race_vars            <- FALLBACK$race_vars
  count_vars           <- FALLBACK$count_vars
  citizenship_vars     <- FALLBACK$citizenship_vars
  citizenship_is_count <- FALLBACK$citizenship_is_count
}

# ---------------------------------------------------------------------------
# Step 3a — Pull DP05 data for all counties (race/ethnicity + total pop)
# ---------------------------------------------------------------------------
cat("\nFetching ACS", YEAR, SURVEY, "DP05 variables for all counties...\n")

# Use output = "wide" so tidycensus returns one column per variable directly.
# Estimate columns are named {friendly_name}E, MOE columns {friendly_name}M.
all_dp05 <- c(race_vars, count_vars)
acs_wide_raw <- get_acs(
  geography   = "county",
  variables   = all_dp05,
  year        = YEAR,
  survey      = SURVEY,
  output      = "wide",
  cache_table = TRUE
)

cat("DP05 records returned:", nrow(acs_wide_raw), "\n")
cat("DP05 columns:", paste(names(acs_wide_raw), collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# Step 3b — Pull DP02 citizenship/nativity data for all counties
# ---------------------------------------------------------------------------
cat("\nFetching ACS", YEAR, SURVEY, "DP02 citizenship variables for all counties...\n")

dp02_wide_raw <- get_acs(
  geography   = "county",
  variables   = citizenship_vars,
  year        = YEAR,
  survey      = SURVEY,
  output      = "wide",
  cache_table = TRUE
)

cat("DP02 records returned:", nrow(dp02_wide_raw), "\n")

# PE vars have no E suffix in wide output; count vars do. Select known columns,
# then rename {name}E -> {name} where needed.
dp02_wide <- dp02_wide_raw |>
  select(GEOID, any_of(names(citizenship_vars)), any_of(paste0(names(citizenship_vars), "E")))

for (vname in names(citizenship_vars)) {
  col_e <- paste0(vname, "E")
  if (col_e %in% names(dp02_wide))
    names(dp02_wide)[names(dp02_wide) == col_e] <- vname
}

# ---------------------------------------------------------------------------
# Step 3c — Download USDA Rural-Urban Continuum Codes (2023)
# ---------------------------------------------------------------------------
# Source: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
# Codes 1–3: metro counties; 4–9: nonmetro (increasing rurality).
# CSV is long-format with columns: FIPS, State, County_Name, Attribute, Value.

cat("\nDownloading USDA Rural-Urban Continuum Codes (2023)...\n")

rucc_url <- "https://www.ers.usda.gov/media/5768/2023-rural-urban-continuum-codes.csv?v=28930"

rucc_codes <- tryCatch({
  raw <- read_csv(rucc_url, show_col_types = FALSE)
  raw |>
    filter(Attribute == "RUCC_2023") |>
    mutate(
      fips      = formatC(as.character(FIPS), width = 5, flag = "0"),
      rucc_code = as.integer(Value)
    ) |>
    select(fips, rucc_code)
}, error = function(e) {
  warning("Failed to download RUCC data: ", e$message,
          "\nrucc_code will be NA for all counties.")
  NULL
})

if (!is.null(rucc_codes)) {
  cat("RUCC codes loaded for", nrow(rucc_codes), "counties\n")
  cat("RUCC distribution:\n")
  print(table(rucc_codes$rucc_code))
}

# ---------------------------------------------------------------------------
# Step 4 — Reshape DP05 to wide and join all datasets
# ---------------------------------------------------------------------------
# With output = "wide", tidycensus returns PE variables using the friendly name
# directly (no E suffix), while count (E) variables get a {name}E column.
# Use a for-loop to rename {name}E -> {name} where needed, then select.
acs_wide <- acs_wide_raw |>
  mutate(
    fips   = formatC(GEOID, width = 5, flag = "0"),
    county = sub(",.*$", "", NAME),
    state  = trimws(sub("^[^,]+,\\s*", "", NAME))
  )

# Rename count estimate columns {name}E -> {name} (PE vars need no rename)
for (vname in names(all_dp05)) {
  col_e <- paste0(vname, "E")
  if (col_e %in% names(acs_wide))
    names(acs_wide)[names(acs_wide) == col_e] <- vname
}

cat("acs_wide columns:", paste(names(acs_wide), collapse = ", "), "\n")

acs_wide <- acs_wide |>
  select(fips, county, state, all_of(names(race_vars)), all_of(names(count_vars)))

# Exclude territories (FIPS state codes outside 01–56, DC = 11)
valid_states <- formatC(c(1:56), width = 2, flag = "0")
acs_wide <- acs_wide |>
  filter(substr(fips, 1, 2) %in% valid_states)

cat("Counties retained (50 states + DC):", nrow(acs_wide), "\n")

# Join DP02 citizenship variables
acs_wide <- acs_wide |>
  left_join(dp02_wide, by = c("fips" = "GEOID"))

# Convert any count-based citizenship variables to percentages using total_pop.
# This handles years where DP02 provides raw counts instead of PE (percent estimates).
if (any(citizenship_is_count)) {
  for (vname in names(citizenship_is_count)[citizenship_is_count]) {
    if (vname %in% names(acs_wide)) {
      cat("Converting", vname, "from count to % of total_pop\n")
      acs_wide[[vname]] <- round((acs_wide[[vname]] / acs_wide$total_pop) * 100, 2)
    }
  }
}

# Join RUCC codes
if (!is.null(rucc_codes)) {
  acs_wide <- acs_wide |> left_join(rucc_codes, by = "fips")
} else {
  acs_wide$rucc_code <- NA_integer_
}

# ---------------------------------------------------------------------------
# Step 4b — Compute Shannon diversity index
# ---------------------------------------------------------------------------
# Uses the 7 mutually-exhaustive race-alone categories (White, Black, AIAN,
# Asian, NHOPI, Other, Two+), which sum to ~100% of the population.
#   H = -sum(p_i * ln(p_i)),  p_i = proportion in group i
# Range: 0 (perfectly homogeneous) to ln(7) ≈ 1.946 (perfectly uniform).
# Zero-proportion groups are excluded (0 * ln(0) → 0 by convention).

cat("\nComputing Shannon diversity index from 7 race-alone categories...\n")

race_cols_shannon <- c("pct_white", "pct_black", "pct_aian",
                       "pct_asian", "pct_nhopi", "pct_other", "pct_two_or_more")

acs_wide <- acs_wide |>
  rowwise() |>
  mutate(
    shannon_diversity = {
      p <- c_across(all_of(race_cols_shannon)) / 100
      p <- p[!is.na(p) & p > 0]
      if (length(p) == 0) NA_real_ else -sum(p * log(p))
    }
  ) |>
  ungroup()

cat("Shannon diversity summary:\n")
print(summary(acs_wide$shannon_diversity))
cat("Columns:\n")
print(names(acs_wide))

# ---------------------------------------------------------------------------
# Step 5 — Save
# ---------------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
out_path <- sprintf("data/processed/county_race_ethnicity_%d.parquet", YEAR)
arrow::write_parquet(acs_wide, out_path)
cat("\nSaved:", out_path, "\n")
cat("To add another year, change YEAR at the top and re-run this script.\n")
