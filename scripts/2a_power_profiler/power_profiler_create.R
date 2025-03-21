## -------------------------------
##
## Create Power Profiler
## 
## Purpose: 
## 
## This file creates power profiler data
## 
## Additional notes
##      
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries -----
library(dplyr)
library(readr)
# library(readxl)
# library(stringr)

# Define eGRID year parameter ----------------
# define parameter year if no one is currently assigned using prompted user input
if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.")
  } else { # if params() is defined, but eGRID_year is not, define it here
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year)
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Load necessary data -----

# previous power profiler data (65187 SAME)
power_profiler_old <- read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/power_profiler_old.csv")) %>%
  janitor::clean_names() %>%
  glimpse()

# utility zip codes (80142 SAME) (39133 unique zips)
utility_zipcodes <- read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/utility_zipcodes.csv")) %>%
  janitor::clean_names() %>%
  # group_by(zip) %>%
  # count() %>%
  glimpse()

# combine these two datasets
#01 : (2889 SAME)
zip_codes_from_old_power_profiler_to_add <-
  power_profiler_old %>%
  anti_join(utility_zipcodes, by = "zip") %>%
  glimpse()

# create subregion dataset
#02 (62,195 SAME)
zip_subregion_1 <-
  utility_zipcodes %>%
  select(zip, state, eiaid, utility_name) %>%
  mutate(subregion = NA_character_, predominant_utility = NA_real_, old_zip_code = "no", method = NA_character_) %>%
  group_by_all() %>%
  summarize(count = n()) %>%
  select(-count) %>%
  ungroup() %>%
  glimpse()

# add zipcodes to add
#03 : (65084 SAME)
zip_subregion_2 <-
  zip_subregion_1 %>%
  bind_rows(zip_codes_from_old_power_profiler_to_add %>% mutate(expr1 = "yes", expr2 = "old power profiler")) %>%
  glimpse()

# DATA - NERC NAMES FOR MISSING UTILITY ID - COUNT 1 (how was this calculated or is this raw data?)
# load in example data
nerc_names_for_missing_utility_id <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/nerc_names_for_missing_utility_id.csv"),
           col_names = c("eiaid","utility_name","nerc_region")) %>%
  janitor::clean_names() %>%
  glimpse()

# DATA - NERC CROSSWALK FOR FEW SUBREGIONS
nerc_region_crosswalk <- 
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/nerc_region_crosswalk.csv"),
           col_names = c("nerc","subregion")) %>%
  janitor::clean_names() %>%
  glimpse()

#31: (82 SAME)
zip_utility_update_nerc <-
  nerc_names_for_missing_utility_id %>%
  inner_join(nerc_region_crosswalk, by = join_by("nerc_region" == "nerc")) %>%
  glimpse()

#32: (2962 updated values -> should be 2861)
zip_subregion_3 <-
  zip_subregion_2 %>%
  full_join(zip_utility_update_nerc, by = "eiaid") %>%
  mutate(method = if_else(is.na(subregion.x), "nerc region", NA_character_),
         subregion.x = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  glimpse() %>%
  count(method) %>%
  glimpse()