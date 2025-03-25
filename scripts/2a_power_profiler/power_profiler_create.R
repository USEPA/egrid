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

# load in previous power profiler data which had zip, utility code, predominant utility, etc. 
# previous power profiler data (65187 SAME)
power_profiler_old <- read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/power_profiler_old.csv")) %>%
  janitor::clean_names() %>%
  glimpse()

# load in all utility zip codes
# utility zip codes (80142 SAME) (39133 unique zips)
utility_zipcodes <- read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/utility_zipcodes.csv")) %>%
  janitor::clean_names() %>%
  # group_by(zip) %>%
  # count() %>%
  glimpse()

# select zip codes present in power_profiler_old but not present in utility_zipcodes
# ultimately specifies which zipcodes  need to be added to new power profiler
#01 : (2889 SAME)
zip_codes_from_old_power_profiler_to_add <-
  power_profiler_old %>%
  anti_join(utility_zipcodes, by = "zip") %>%
  rename(utility_name = util_name, eiaid = trim_util_code, subregion = subrgn) %>%
  glimpse()

# create zipcode dataset with all unique combinations of the variables (zip, state, eiaid, utility name)
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

# add zipcodes from previous power profiler to add
#03 : (65084 SAME - 2889 old power profiler SAME)
# right now I have the subregion values from old power profiler into subregion data
# access has them filled as NAs
zip_subregion_2 <-
  zip_subregion_1 %>%
  bind_rows(zip_codes_from_old_power_profiler_to_add %>% mutate(old_zip_code = "yes", method = "old power profiler", subregion = NA_character_)) %>%
  glimpse()
  
zipcount <-
  zip_subregion_2 %>%
  count(method) %>%
  glimpse()

# DATA(30) - NERC NAMES FOR MISSING UTILITY ID - COUNT 1 (how was this calculated or is this raw data?)
# load in example data
nerc_names_for_missing_utility_id <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/nerc_names_for_missing_utility_id.csv"),
           col_names = c("eiaid","utility_name","nerc_region")) %>%
  janitor::clean_names() %>%
  glimpse()

# DATA - NERC CROSSWALK FOR FEW SUBREGIONS (this only has three nerc and subregion combos - what is this?)
nerc_region_crosswalk <- 
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/nerc_region_crosswalk.csv"),
           col_names = c("nerc","subregion")) %>%
  janitor::clean_names() %>%
  glimpse()

# combine the subregion - nerc region match to the nerc names for missing  utility id
# only keep the matches, 82 that have new subregion names
#31: (82 SAME) - now 80 because there were two duplicates
zip_utility_update_nerc <-
  nerc_names_for_missing_utility_id %>%
  inner_join(nerc_region_crosswalk, by = join_by("nerc_region" == "nerc")) %>%
  distinct() %>%
  glimpse()

# we want to update fields that have matching eiaids
# if there's a matching eiaid AND subregion is blank, the method and subregion will be updated


# there were only 82 eidids in zip_utility_update_nerc, but in the subregion data there are multiple zipcodes for each eiaid so we are matching the nerc subregion to that
# should keep original value of 65084
# zips_to_update <-
#   zip_subregion_2 %>%
#   inner_join(zip_utility_update_nerc, by = "eiaid") %>%
#   filter(is.na(subregion.x)) %>%
#   mutate(method = "nerc region", subregion.x = subregion.y) %>%
#   filter(!is.na(subregion.x)) %>%
#   rename_with(~ gsub("\\.x$", "", .), everything()) %>%
#   select(-utility_name.y, -nerc_region, -subregion.y) %>%
#   glimpse()

#32 -(65084 - SAME, 2861 NERC - SAME, 2888 OLD POWER PROFILER - SAME)
# join in utility ids with proper nerc region
# update subregion and specify nerc region
zip_subregion_3 <-
  zip_subregion_2 %>%
  left_join(zip_utility_update_nerc, by = c("eiaid", "utility_name")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

zipcount <-
  zip_subregion_3 %>%
  count(method) %>%
  glimpse()

#/////////////////////////////////////////////////#

#DATA FOR BA CODES FOR MISSING UTILITY IDS
ba_names_for_missing_utility_id <- 
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/ba_names_for_missing_utility_id.csv"),
           col_names = c("eiaid", "utility_name", "ba_code")) %>%
  janitor::clean_names() %>%
  glimpse()

xwalk_balancing_authority <-
  read_csv("data/1_production_model/static_tables/xwalk_balancing_authority.csv") %>%
  janitor::clean_names() %>%
  select(subregion = subrgn, ba_code = balancing_authority_code) %>%
  glimpse()

#36: (1188 SAME) - 1182 as distinct
zip_utility_update_ba <- 
  ba_names_for_missing_utility_id %>%
  inner_join(xwalk_balancing_authority, by = "ba_code") %>%
  distinct() %>%
  glimpse()
  
#37: #32 -(65084 - SAME, 22931 BA, 2861 NERC - SAME, 2796 OLD POWER PROFILER - 2782 in access)

zip_subregion_4 <-
  zip_subregion_3 %>%
  left_join(zip_utility_update_ba, by = c("eiaid", "utility_name")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "balancing authority", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

zipcount <-
  zip_subregion_4 %>%
  count(method) %>%
  glimpse()
  
  
#11: 
  
