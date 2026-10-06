#!/usr/bin/env Rscript
# prep_data.R
# Pulls ACS 5-year race/ethnicity, nativity/citizenship, and total population
# estimates for all US counties from the DP05 and DP02 data profile tables via
# tidycensus, merges USDA Rural-Urban Continuum Codes, and saves a wide-format
# parquet file for one ACS end year.
#
# Usage (from the project directory):
#   Rscript prep_data.R          # uses YEAR below
#   Rscript prep_data.R 2013     # any end year in data/acs_profile_lookup.csv (2009–2024)
#
# Requires a Census API key. If you don't have one, register at:
#   https://api.census.gov/data/key_signup.html
# Then call: tidycensus::census_api_key("YOUR_KEY", install = TRUE)

library(tidycensus)
library(dplyr)
library(arrow)
library(readr)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
YEAR   <- 2024   # End year of 5-year ACS (2020–2024)
SURVEY <- "acs5"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) YEAR <- as.integer(args[1])
if (is.na(YEAR)) stop("YEAR must be a 4-digit ACS end year, e.g. Rscript prep_data.R 2023")

if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
  stop("No Census API key found. Register at https://api.census.gov/data/key_signup.html ",
       "and run tidycensus::census_api_key('YOUR_KEY', install = TRUE), then restart R.")
}

# ---------------------------------------------------------------------------
# Step 1 — Variable IDs for this release from data/acs_profile_lookup.csv
# ---------------------------------------------------------------------------
# DP05/DP02 variable IDs and label wording change between ACS releases, so IDs
# come from a per-year lookup built (and checked for exactly one match per
# variable) by build_lookup.R. Re-run that script to add new releases.
#   pct_*            percent estimates (PE); race/ethnicity and foreign-born are
#                    % of total population, naturalized / non-citizen are % of
#                    the FOREIGN-BORN population
#   total_pop        DP05 total population (count)
#   dp02_total_pop,  DP02 counts used to compute non-citizens as % of total
#   noncitizen_count population; dropped after use
HELPER_COUNTS <- c("dp02_total_pop", "noncitizen_count")

lookup_path <- "data/acs_profile_lookup.csv"
if (!file.exists(lookup_path)) stop(lookup_path, " not found. Run: Rscript build_lookup.R")
lookup <- read.csv(lookup_path, stringsAsFactors = FALSE) |> filter(year == YEAR)
if (nrow(lookup) == 0) {
  stop("No variable lookup for ", YEAR, ". Available years: ",
       paste(sort(unique(read.csv(lookup_path)$year)), collapse = ", "),
       ". For a new release, run: Rscript build_lookup.R")
}
var_ids <- setNames(lookup$id, lookup$variable)

# Guard against the Census Bureau revising metadata after the lookup was built:
# each ID must still carry the label recorded in the lookup.
cat("Checking variable labels for", YEAR, SURVEY, "...
")
defs <- load_variables(YEAR, paste0(SURVEY, "/profile")) |>
  mutate(id = ifelse(grepl("E$", name), name, paste0(name, "E")))   # load_variables drops the E
current <- defs$label[match(lookup$id, defs$id)]
changed <- is.na(current) | current != lookup$label
if (any(changed)) {
  stop("Variable labels differ from the lookup for ", YEAR, ":
  ",
       paste(lookup$variable[changed], lookup$id[changed], sep = " ", collapse = "
  "),
       "
Re-run build_lookup.R and review the result.")
}

cat("
Variable IDs for", YEAR, ":
")
for (n in names(var_ids)) cat(sprintf("  %-17s %s
", n, var_ids[n]))

# ---------------------------------------------------------------------------
# Step 2 — Pull DP05 + DP02 for all counties
# ---------------------------------------------------------------------------
# Variables are requested unnamed, so wide-output estimate columns carry the
# exact API IDs (e.g. DP05_0037PE, DP05_0033E); they are renamed below.
cat("\nFetching ACS", YEAR, SURVEY, "profile variables for all counties...\n")

acs_raw <- get_acs(
  geography   = "county",
  variables   = unname(var_ids),
  year        = YEAR,
  survey      = SURVEY,
  output      = "wide",
  cache_table = TRUE
)
cat("Records returned:", nrow(acs_raw), "\n")

missing_cols <- setdiff(var_ids, names(acs_raw))
if (length(missing_cols) > 0) {
  stop("get_acs() output is missing expected columns: ", paste(missing_cols, collapse = ", "))
}

acs_wide <- tibble(
  fips   = acs_raw$GEOID,
  county = sub(",.*$", "", acs_raw$NAME),
  state  = trimws(sub("^[^,]+,\\s*", "", acs_raw$NAME))
)
for (n in names(var_ids)) acs_wide[[n]] <- acs_raw[[var_ids[[n]]]]

# The 2009 API returns "Do?a Ana County" (non-ASCII character lost upstream)
acs_wide$county[acs_wide$fips == "35013"] <- "Doña Ana County"
if (any(grepl("?", acs_wide$county, fixed = TRUE))) {
  warning("County names with '?' (lost characters): ",
          paste(acs_wide$county[grepl("?", acs_wide$county, fixed = TRUE)], collapse = ", "))
}

# Census annotation codes (e.g. -666666666 "cannot be computed") mean no
# estimate. tidycensus only recodes the negative forms; the 2009 release also
# uses positive 666666666, so recode both signs.
annotation_codes <- 111111111 * 1:9
for (n in names(var_ids)) {
  acs_wide[[n]][abs(acs_wide[[n]]) %in% annotation_codes] <- NA
}

# Non-citizens as % of total population (the DP02 PE is % of foreign-born)
acs_wide <- acs_wide |>
  mutate(pct_noncitizen_pop = ifelse(dp02_total_pop > 0,
                                     round(100 * noncitizen_count / dp02_total_pop, 2),
                                     NA_real_)) |>
  select(-all_of(HELPER_COUNTS))

# Exclude territories (keep state FIPS 01–56, which includes DC = 11)
valid_states <- formatC(1:56, width = 2, flag = "0")
acs_wide <- acs_wide |> filter(substr(fips, 1, 2) %in% valid_states)
cat("Counties retained (50 states + DC):", nrow(acs_wide), "\n")

# ---------------------------------------------------------------------------
# Step 3 — USDA Rural-Urban Continuum Codes
# ---------------------------------------------------------------------------
# Source: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
# Codes 1–3: metro counties; 4–9: nonmetro (increasing rurality).
# Vintage matched to the county geography of the release:
#   2022+      RUCC 2023 (2020 Census; Connecticut planning regions)
#   2009–2021  RUCC 2013 (2010 Census; legacy Connecticut counties), with
#              later FIPS changes cross-walked to their predecessor's code
RUCC_VINTAGE <- if (YEAR >= 2022) 2023L else 2013L
cat("
Downloading USDA Rural-Urban Continuum Codes (", RUCC_VINTAGE, ")...
", sep = "")

load_rucc <- function(vintage) {
  if (vintage == 2023L) {
    read_csv("https://www.ers.usda.gov/media/5768/2023-rural-urban-continuum-codes.csv?v=52934",
             show_col_types = FALSE) |>
      filter(Attribute == "RUCC_2023") |>
      transmute(fips = formatC(as.character(FIPS), width = 5, flag = "0"),
                rucc_code = as.integer(Value))
  } else {
    f <- tempfile(fileext = ".xls")
    download.file("https://www.ers.usda.gov/media/5769/2013-rural-urban-continuum-codes.xls?v=49872",
                  f, mode = "wb", quiet = TRUE)
    r13 <- readxl::read_excel(f) |>
      transmute(fips = formatC(as.character(FIPS), width = 5, flag = "0"),
                rucc_code = as.integer(RUCC_2013))
    # Counties created or renamed after 2013 inherit the predecessor's code
    xwalk <- c("46102" = "46113",   # Oglala Lakota (formerly Shannon), SD, 2015
               "02158" = "02270",   # Kusilvak (formerly Wade Hampton), AK, 2015
               "02063" = "02261",   # Chugach (from Valdez-Cordova), AK, 2019
               "02066" = "02261")   # Copper River (from Valdez-Cordova), AK, 2019
    bind_rows(r13, tibble(fips = names(xwalk),
                          rucc_code = r13$rucc_code[match(xwalk, r13$fips)]))
  }
}

rucc_codes <- tryCatch(load_rucc(RUCC_VINTAGE), error = function(e) {
  warning("Failed to download RUCC data: ", e$message,
          "
rucc_code will be NA for all counties.")
  NULL
})

if (!is.null(rucc_codes)) {
  acs_wide <- acs_wide |> left_join(rucc_codes, by = "fips")
  no_rucc <- acs_wide$fips[is.na(acs_wide$rucc_code)]
  cat("RUCC codes matched:", sum(!is.na(acs_wide$rucc_code)), "of", nrow(acs_wide), "counties
")
  if (length(no_rucc) > 0) cat("  No RUCC code:", paste(no_rucc, collapse = ", "), "
")
} else {
  acs_wide$rucc_code <- NA_integer_
}
acs_wide$rucc_vintage <- RUCC_VINTAGE

# ---------------------------------------------------------------------------
# Step 4 — Shannon diversity index
# ---------------------------------------------------------------------------
# Uses the 7 mutually-exhaustive race-alone categories (White, Black, AIAN,
# Asian, NHOPI, Other, Two+), which sum to ~100% of the population.
#   H = -sum(p_i * ln(p_i)),  p_i = proportion in group i
# Range: 0 (perfectly homogeneous) to ln(7) ≈ 1.946 (perfectly uniform).
# Zero-proportion groups are excluded (0 * ln(0) → 0 by convention).
cat("\nComputing Shannon diversity index from 7 race-alone categories...\n")

race_cols_shannon <- c("pct_white", "pct_black", "pct_aian",
                       "pct_asian", "pct_nhopi", "pct_other", "pct_two_or_more")

p <- as.matrix(acs_wide[race_cols_shannon]) / 100
plogp <- ifelse(is.na(p) | p <= 0, 0, p * log(p))
acs_wide$shannon_diversity <- ifelse(rowSums(!is.na(p)) == 0, NA_real_, -rowSums(plogp))

print(summary(acs_wide$shannon_diversity))

# ---------------------------------------------------------------------------
# Step 5 — Validate
# ---------------------------------------------------------------------------
# These checks catch the wrong-variable failures that silently produced
# implausible files in earlier versions of this script.
race_sum <- rowSums(acs_wide[race_cols_shannon])
fb_sum   <- acs_wide$pct_naturalized + acs_wide$pct_noncitizen
checks <- c(
  "at least 3,100 counties"                      = nrow(acs_wide) >= 3100,
  "no duplicate FIPS"                            = !anyDuplicated(acs_wide$fips),
  "7 race-alone categories sum to 100 (+/- 1.5)" = all(abs(race_sum - 100) <= 1.5, na.rm = TRUE),
  "percent columns within 0–100"                 = all(unlist(lapply(
    acs_wide[c(race_cols_shannon, "pct_hispanic", "pct_nh_white", "pct_foreign_born",
               "pct_naturalized", "pct_noncitizen", "pct_noncitizen_pop")],
    function(x) all(x >= 0 & x <= 100, na.rm = TRUE)))),
  "naturalized + non-citizen = 100 (+/- 1)"      = all(abs(fb_sum - 100) <= 1, na.rm = TRUE),
  "non-citizen % of pop <= foreign-born %"       = all(acs_wide$pct_noncitizen_pop <=
                                                       acs_wide$pct_foreign_born + 0.5, na.rm = TRUE),
  "NH White <= White alone + Hispanic"           = all(acs_wide$pct_nh_white <=
                                                       acs_wide$pct_white + acs_wide$pct_hispanic + 0.5,
                                                       na.rm = TRUE)
)
cat("\nValidation checks:\n")
for (n in names(checks)) cat(sprintf("  [%s] %s\n", if (checks[[n]]) "ok" else "FAIL", n))
if (!all(checks)) stop("Validation failed; parquet not written. Check the resolved variable IDs above.")

# ---------------------------------------------------------------------------
# Step 6 — Save
# ---------------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
out_path <- sprintf("data/processed/county_race_ethnicity_%d.parquet", YEAR)
arrow::write_parquet(acs_wide, out_path)
cat("\nSaved:", out_path, "(", nrow(acs_wide), "rows x", ncol(acs_wide), "cols )\n")
