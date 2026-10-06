#!/usr/bin/env Rscript
# build_lookup.R
# Builds data/acs_profile_lookup.csv: the ACS 5-year Data Profile variable ID
# for every variable prep_data.R needs, for every release year.
#
# Variable IDs AND label text change between releases, e.g.
#   - label prefixes: "Number!!Estimate!!" (2009), "Percent Estimate!!" (2017-18),
#     "Estimate!!" / "Percent!!" (other years)
#   - "Total population!!" inserted into RACE / HISPANIC paths from 2013 (2009 had it too)
#   - citizenship rows nested under "Foreign-born population!!" from 2013
#   - "Foreign born" -> "Foreign-born" (2023)
#   - "Some other race" -> "Some Other Race", "Two or more" -> "Two or More" (2022)
#   - IDs renumbered in 2017, 2019/2020 (DP02), 2022, 2023, 2024
# Labels are normalized to remove those cosmetic differences, then each
# variable must match exactly one canonical path per year (see ALLOWED_DUPES).
#
# Uses the public Census variables.json endpoints (no API key needed).
# Usage: Rscript build_lookup.R            # all available years
#        Rscript build_lookup.R 2009 2012  # range

suppressMessages({
  library(jsonlite)
  library(dplyr)
})

args  <- as.integer(commandArgs(trailingOnly = TRUE))
YEARS <- if (length(args) == 2) args[1]:args[2] else 2009:as.integer(format(Sys.Date(), "%Y"))

# Canonical (normalized) label path and estimate type for each output column.
SPEC <- tribble(
  ~variable,          ~type,  ~key,
  "total_pop",        "E",    "race!!total population",
  "pct_white",        "PE",   "race!!one race!!white",
  "pct_black",        "PE",   "race!!one race!!black or african american",
  "pct_aian",         "PE",   "race!!one race!!american indian and alaska native",
  "pct_asian",        "PE",   "race!!one race!!asian",
  "pct_nhopi",        "PE",   "race!!one race!!native hawaiian and other pacific islander",
  "pct_other",        "PE",   "race!!one race!!some other race",
  "pct_two_or_more",  "PE",   "race!!two or more races",
  "pct_hispanic",     "PE",   "hispanic or latino and race!!hispanic or latino (of any race)",
  "pct_nh_white",     "PE",   "hispanic or latino and race!!not hispanic or latino!!white alone",
  "dp02_total_pop",   "E",    "place of birth!!total population",
  "pct_foreign_born", "PE",   "place of birth!!foreign born",
  "pct_naturalized",  "PE",   "u.s. citizenship status!!naturalized u.s. citizen",
  "pct_noncitizen",   "PE",   "u.s. citizenship status!!not a u.s. citizen",
  "noncitizen_count", "E",    "u.s. citizenship status!!not a u.s. citizen"
)

# "Two or More Races" appears twice in DP05 with the same value: the RACE
# summary row and the header of the detailed multiracial combinations.
# The lowest ID (the summary row) is used.
ALLOWED_DUPES <- c("pct_two_or_more")

normalize_label <- function(x) {
  x <- tolower(x)
  x <- sub("^(number!!estimate|percent!!estimate|percent estimate|estimate|percent|number)!!", "", x)
  x <- gsub("foreign-born", "foreign born", x, fixed = TRUE)
  x <- gsub("total population!!", "", x, fixed = TRUE)
  x <- gsub("foreign born population!!", "", x, fixed = TRUE)
  x
}

fetch_profile_vars <- function(year) {
  url <- sprintf("https://api.census.gov/data/%d/acs/acs5/profile/variables.json", year)
  v <- tryCatch(fromJSON(url)$variables, error = function(e) NULL)
  if (is.null(v)) return(NULL)
  tibble(id = names(v), label = vapply(v, function(x) x$label, "")) |>
    filter(grepl("^DP0[25]_[0-9]{4}P?E$", id)) |>
    mutate(type = ifelse(endsWith(id, "PE"), "PE", "E"),
           key  = normalize_label(label)) |>
    arrange(id)
}

rows <- list()
problems <- character(0)
for (yr in YEARS) {
  defs <- fetch_profile_vars(yr)
  if (is.null(defs)) { cat(yr, ": no acs5/profile metadata, skipped\n"); next }
  for (i in seq_len(nrow(SPEC))) {
    s <- SPEC[i, ]
    hits <- defs |> filter(key == s$key, type == s$type)
    if (nrow(hits) == 0 || (nrow(hits) > 1 && !s$variable %in% ALLOWED_DUPES)) {
      problems <- c(problems, sprintf("%d %s: %d matches (%s)", yr, s$variable, nrow(hits),
                                      paste(hits$id, collapse = ", ")))
      next
    }
    rows[[length(rows) + 1]] <- tibble(year = yr, variable = s$variable, id = hits$id[1],
                                       label = hits$label[1], n_matches = nrow(hits))
  }
  cat(yr, ": ok\n")
}

if (length(problems) > 0) {
  stop("Lookup incomplete; fix SPEC/normalize_label():\n  ", paste(problems, collapse = "\n  "))
}

lookup <- bind_rows(rows)
dir.create("data", showWarnings = FALSE)
write.csv(lookup, "data/acs_profile_lookup.csv", row.names = FALSE)
cat("\nWrote data/acs_profile_lookup.csv:", nrow(lookup), "rows,",
    length(unique(lookup$year)), "years (", paste(range(lookup$year), collapse = "–"), ")\n")
