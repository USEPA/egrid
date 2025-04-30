## -------------------------------
##
## Create Power Profiler
## 
## Purpose: 
## 
## This file imports utility ids assigned to zipcode and 
## assigns subregions to each utility using a sequence of
## methods with the resulting outputs of:
##    * Predominant utility for each zipcode
##    * Primary (and secondary) subregion for each zipcode
##
## Output datasets include:
##    * zip subregion final
##    * subregion assignments
## 
## Additional notes
##      
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries -----
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

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

# load in eGRID plant data
# plant_file <-
#   readRDS(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/plant_file.RDS"))

plant_file <-
  read_csv(glue::glue("data/2a_power_profiler/static_tables/qa/{params$eGRID_year}/plant_access.csv")) %>%
  janitor::clean_names() %>%
  mutate(transmission_or_distribution_system_owner_id = as.character(transmission_or_distribution_system_owner_id),
         utility_id = as.character(utility_id)) %>%
  rename(egrid_subregion = subrgn,
         system_owner_id = transmission_or_distribution_system_owner_id,
         plant_state = state) %>%
  glimpse()

# load in previous power profiler data which had zip, utility code, predominant utility, etc. 
power_profiler_old <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/power_profiler_old.csv"),
           col_types = "cccccc") %>%
  janitor::clean_names() %>%
  mutate(predominant_utility = as.factor(predominant_utility))

# load in all utility zip codes
utility_zipcodes <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/utility_zipcodes.csv"),
           col_types = "ccccccddd") %>%
  janitor::clean_names()

# load in EIA-861 utility data - with count 
eia_861_utility_data_with_count <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/eia_861_utility_with_count.csv"),
           col_types = "ccccccccccccccccccccccdcccccccccc") %>%
  janitor::clean_names()

# load in EIA-861 utility data
eia_861_sales_ult_cust <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/eia_861_sales_ult_cust.csv"),
           col_types = "cccccccc") %>%
  janitor::clean_names()

# Load necessary crosswalks -----

# subregion to balancing authority crosswalk
xwalk_ba_subregion <-
  read_csv("data/1_production_model/static_tables/xwalk_balancing_authority.csv") %>%
  janitor::clean_names() %>%
  select(subregion = subrgn, ba_code = balancing_authority_code)

# NERC region, BA, transmission, and subregion crosswalk
xwalk_ba_transmission <- # 1204(1160)
  read_csv("data/1_production_model/static_tables/xwalk_subregion_transmission.csv",
           col_types = "cccc") %>%
  janitor::clean_names() %>%
  rename(ba_code = balancing_authority_code, transmission = transmission_or_distribution_system_owner_id, subregion = subrgn)

# Transmission to subregion crosswalk
xwalk_transmissionid_subregion <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/xwalk_transmissionid_subregion.csv"),
           col_types = "cc") %>%
  janitor::clean_names()

# Crosswalk for missing utility ids
xwalk_missing_utilityid <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/xwalk_missing_utilityid.csv"),
           col_types = "cccc") %>%
  janitor::clean_names()

# Manual subregion updates
utility_subregion_manual_updates <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/manual_updates_utility_subregion.csv"),
           col_types = "ccc") %>%
  janitor::clean_names()

# Manual predominant utility updates
predominant_utility_manual_updates <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/manual_updates_predominant_utility.csv"),
           col_types = "ccccc") %>%
  janitor::clean_names() %>%
  select(zip, first_ofeiaid) %>%
  mutate(predominant_utility = as.factor("0"))

# Manual primary subregion updates
primary_subregion_manual_updates <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/manual_updates_primary_subregion.csv"),
           col_types = "ccc") %>%
  janitor::clean_names()

# Create zipcode dataset ------

# create zipcode dataset using utility_zipcodes data
zip_utility_subregion_1 <-
  utility_zipcodes %>%
  select(zip, state, eiaid, utility_name) %>%
  mutate(subregion = NA_character_, 
         predominant_utility = factor("0", levels = c("0", "1")), 
         old_zip_code = factor("no", levels = c("no", "yes")), 
         method = NA_character_) %>% distinct()

### Add zipcodes from old power profiler  -------

# select zip codes in old power profiler not in utility_zipcodes
zip_codes_from_old_power_profiler_to_add <-
  power_profiler_old %>%
  anti_join(utility_zipcodes, by = "zip") %>%
  mutate(predominant_utility = factor(predominant_utility),
         old_zip_code = factor("yes", levels = c("no", "yes")), 
         method = "old power profiler") %>%
  rename(utility_name = util_name, 
         eiaid = trim_util_code, 
         subregion = subrgn)

# append zipcodes from previous power profiler to add
zip_utility_subregion_2 <-
  zip_utility_subregion_1 %>%
  bind_rows(zip_codes_from_old_power_profiler_to_add) %>%
  mutate(subregion = NA_character_)

# Assign subregions to utility using a sequence of methods -------
# Subregions are assigned by grouping data by desired variable and identifying utilities with a singular subregion in the following sequence:
  # Grouped By:
    # 1) NERC Region
    # 2) Balancing Authority Code
    # 3) Plant Transmission ID
    # 4) Plant Utility ID
    # 5) NERC Region, BA Code, and Transmission ID
    # 6) Transmission ID - Subregion Crosswalk
    # 7) Missing Utility ID Crosswalk

### NERC Region assignments --------

# get NERC subregions for eiaid and utility_numbers in zipsubregion data
nerc_region_for_missing_utility_id <-
  zip_utility_subregion_2 %>%
  inner_join(eia_861_utility_data_with_count, by = c("eiaid" = "utility_number")) %>%
  select(eiaid, utility_name = utility_name.x, nerc_region) %>% distinct() %>%
  arrange(as.numeric(eiaid))

# identify records with only one subregion assignment
count_of_one_nerc_region <-
  nerc_region_for_missing_utility_id %>%
  group_by(eiaid, utility_name) %>%
  summarize(subrgn_count = n_distinct(nerc_region)) %>%
  filter(subrgn_count == 1) %>%
  arrange(as.numeric(eiaid)) %>%
  ungroup()

# assign subregion for unique cases with one-to-one NERC-subregion match
zip_utility_update_nerc <-
  nerc_region_for_missing_utility_id %>%
  inner_join(count_of_one_nerc_region, by = "eiaid") %>%
  mutate(subregion = case_when(nerc_region == "TRE" ~ "ERCT",
                               nerc_region == "FRCC" ~ "FRCC",
                               nerc_region == "PR" ~ "PRMS",
                               TRUE ~ NA_character_)) %>%
  select(eiaid, utility_name = utility_name.x, nerc_region, subregion) %>%
  filter(!is.na(subregion)) %>% distinct()

#### Update subregions based on NERC grouping -----

# join in utility ids with proper nerc region
zip_utility_subregion_3 <-
  zip_utility_subregion_2 %>%
  left_join(zip_utility_update_nerc, by = c("eiaid", "utility_name")) %>%
  # update subregion and specify matching method
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -nerc_region)

### Balancing Authority assignment --------

# create ba codes for missing utility id dataset
ba_names_for_missing_utility_id <-
  zip_utility_subregion_3 %>%
  inner_join(eia_861_sales_ult_cust, by = c("eiaid" = "utility_number")) %>%
  select(eiaid, utility_name = utility_name.x, ba_code) %>% distinct() %>%
  group_by(eiaid, utility_name) %>%
  # select utilities with singular BA code
  filter(n_distinct(ba_code) == 1) %>%
  ungroup()

# match ba codes to subregions
zip_utility_update_ba <- 
  ba_names_for_missing_utility_id %>%
  inner_join(xwalk_ba_subregion, by = "ba_code") %>% distinct() %>%
  arrange(as.numeric(eiaid))

#### Update subregion based on BA crosswalk -----

# join in utility ids with proper nerc region
zip_utility_subregion_4 <-
  zip_utility_subregion_3 %>%
  left_join(zip_utility_update_ba, by = c("eiaid", "utility_name")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "balancing authority", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -ba_code)

### Plant Transmission ID assignment ------------

# match plant file subregion data to zipcode utilities
plant_transmission_subregions <-
  plant_file %>%
  select(egrid_subregion, system_owner_id) %>% distinct() %>%
  inner_join(utility_zipcodes, by = join_by("system_owner_id" == "eiaid")) %>%
  select(zip, subregion = egrid_subregion, eiaid = system_owner_id) %>% distinct() %>%
  # select utilities with singluar subregion
  group_by(zip, eiaid) %>%
  filter(n_distinct(subregion) == 1) %>%
  ungroup()

#### Update subregions based on transmission ID plant file -----------

zip_utility_subregion_5 <-
  zip_utility_subregion_4 %>%
  left_join(plant_transmission_subregions, by = c("zip", "eiaid")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), "plant file - transmission ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### Plant Utility ID assignment --------

# assign subregions by plant utility ID
utilityid_subregions <-
  plant_file %>%
  select(egrid_subregion, utility_id) %>% distinct() %>%
  inner_join(utility_zipcodes, by = join_by("utility_id" == "eiaid")) %>%
  select(zip, subregion = egrid_subregion, eiaid = utility_id) %>% distinct() %>%
  group_by(zip, eiaid) %>%
  filter(n_distinct(subregion) == 1) %>%
  ungroup()

#### Update subregions based on plant utility ID ---------
zip_utility_subregion_6 <-
  zip_utility_subregion_5 %>%
  left_join(utilityid_subregions, by = c("zip", "eiaid")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), "plant file - utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### NERC Region / BA / Transmission ID assignment -----

# update subregion using BA transmission crosswalk
zip_utility_subregion_7 <-
  zip_utility_subregion_6 %>%
  left_join(eia_861_sales_ult_cust %>% 
              select(utility_number, ba_code),
            by = c("eiaid" = "utility_number")) %>%
  left_join(eia_861_utility_data_with_count %>% 
              select(utility_number, nerc_region),
            by = c("eiaid" = "utility_number")) %>%
  left_join(xwalk_ba_transmission,
            by = c("nerc_region", "ba_code", "eiaid" = "transmission")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region/ba/transmission ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -ba_code, -nerc_region) %>% distinct()

### Transmission ID -> Subregion Crosswalk assignment --------

# update subregion based on transmission ID subregion crosswalK
zip_utility_subregion_8 <-
  zip_utility_subregion_7 %>%
  left_join(xwalk_transmissionid_subregion, by = c("eiaid" = "transmission_id")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "power profiler transmission ID crosswalk", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct()

### Missing Utility ID crosswalk --------

# update subregions for utility IDs not in eGRID
zip_utility_subregion_9 <-
  zip_utility_subregion_8 %>%
  left_join(xwalk_missing_utilityid %>%
              select(utility_number, subregion), 
            by = c("eiaid" = "utility_number")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "crosswalk for missing utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct()

# Update subregion assignments when necessary -----

### Update missing subregion for zip codes from old power profiler -----
zip_utility_subregion_10 <-
  zip_utility_subregion_9 %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              select(zip, eiaid, subregion), 
            by = c("zip", "eiaid")) %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct()

### Based on states with one subregion -------

# identify states with unique subregion
states_and_single_subregions <-
  plant_file %>%
  select(state = plant_state, subregion = egrid_subregion) %>%
  filter(!is.na(subregion)) %>% distinct() %>%
  group_by(state) %>%
  filter(n_distinct(subregion) == 1)

# update subregion values
zip_utility_subregion_11 <-
  zip_utility_subregion_10 %>%
  left_join(states_and_single_subregions, by = "state") %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "based on state", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### Manual overrides from table --------

# override subregion values for zipcodes in manual updates table
zip_utility_subregion_12 <-
  zip_utility_subregion_11 %>%
  left_join(utility_subregion_manual_updates, by = c("state", "eiaid" = "utility_id")) %>%
  mutate(method = if_else(!is.na(subregion.y), "manual override", method),
         subregion = if_else(!is.na(subregion.y), subregion.y, subregion.x)) %>%
  select(-contains("."))

# Update Predominant Utilities-----

### Assign predominant utility for zips with one utility assignment ------

# identify zipcodes with only one utility
zip_utility_subregion_13 <-
  zip_utility_subregion_12 %>%
  select(zip, eiaid) %>% distinct() %>%
  group_by(zip) %>%
  filter(n_distinct(eiaid) == 1) %>%
  mutate(predominant_utility = as.factor(1)) %>%
  ungroup()

# assign zipcodes to be predominant utility
zip_utility_subregion_14 <-
  zip_utility_subregion_12 %>%
  left_join(zip_utility_subregion_13, by = c("zip", "eiaid")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

### Update zips with no utility assignment to old profiler assignments ------

# select zipcodes and predominant utility for zips with no predominant utility assignment from old profiler 
no_predominant_utility <-
  zip_utility_subregion_14 %>%
  group_by(zip) %>%
  # select zipcodes without predominant utility assignment
  filter(!any(predominant_utility != 0)) %>%
  select(zip) %>% distinct() %>%
  ungroup() %>%
  # gather old power profiler assignment
  inner_join(power_profiler_old, by = "zip") %>%
  select(zip, trim_util_code, predominant_utility)

# update predominant utility assignment
zip_utility_subregion_15 <-
  zip_utility_subregion_14 %>%
  left_join(no_predominant_utility, by = c("zip", "eiaid" = "trim_util_code")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

#### Update as first EIAID from old power profiler ------

# check again for zipcodes without predominant utility assignment
no_predominant_utility_2 <-
  zip_utility_subregion_15 %>%
  group_by(zip) %>%
  filter(!any(predominant_utility != 0)) %>%
  select(zip) %>% distinct() %>%
  arrange(as.numeric(zip)) %>%
  ungroup()
  
# identify 'first' EIAID in zipcode and assign as predominant utility
zips_to_update_first_eiaid <-
  zip_utility_subregion_15 %>%
  arrange(as.numeric(zip), as.numeric(eiaid), predominant_utility) %>%
  inner_join(no_predominant_utility_2, by = "zip") %>%
  group_by(zip) %>%
  summarize(first_of_eia = first(eiaid)) %>%
  mutate(predominant_utility = as.factor("1"))

# Update zipcode dataset
zip_utility_subregion_16 <-
  zip_utility_subregion_15 %>%
  left_join(zips_to_update_first_eiaid, by = c("zip", "eiaid" = "first_of_eia")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

### Manual override from table ------

# override predominant utility assignment for zipcodes in manual updates table
zip_utility_subregion_17 <-
  zip_utility_subregion_16 %>%
  left_join(predominant_utility_manual_updates, by = "zip") %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."), -first_ofeiaid)

# override predominant utility assignment for zipcodes that match first eiaid in manual updates table
zip_utility_subregion_18 <-
  zip_utility_subregion_17 %>%
  left_join(predominant_utility_manual_updates %>%
              mutate(predominant_utility = as.factor("1")), 
            by = c("zip", "eiaid" = "first_ofeiaid")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

# Create Subregion Assignments Data -----

### Primary subregion for zips assigned as predominant utility -----
subregions_primary <-
  zip_utility_subregion_18 %>%
  filter(predominant_utility == 1) %>%
  select(zip, state, subregion) %>% distinct() %>%
  mutate(secondary = as.factor("0"))

### Zips with multiple subregion assignments -----

### Secondary subregions for zips with multiple assignments -----
subregions_secondary <-
  zip_utility_subregion_18 %>%
  filter(!is.na(subregion)) %>%
  group_by(zip) %>%
  filter(n_distinct(subregion) > 1) %>%
  ungroup() %>%
  select(zip) %>% distinct() %>%
  mutate(secondary = as.factor("1"))

# update primary subregion table
zip_primary_subregion_1 <-
  subregions_primary %>%
  left_join(subregions_secondary, by = "zip") %>%
  mutate(secondary = if_else(!is.na(secondary.y), secondary.y, secondary.x)) %>%
  select(-contains("."))

### Manual updates to primary subregions ----

zip_primary_subregion_2 <-
  zip_primary_subregion_1 %>%
  left_join(primary_subregion_manual_updates, by = "zip") %>%
  mutate(subregion = if_else(!is.na(change), change, subregion)) %>%
  select(-current, -change) %>% distinct()

### Fill missing subregions with those from old profiler -----

subregions_to_update_from_old_pp <-
  zip_utility_subregion_18 %>%
  filter(is.na(subregion)) %>%
  select(zip, subregion) %>% distinct() %>%
  inner_join(power_profiler_old, by = "zip") %>%
  group_by(zip) %>%
  filter(n_distinct(subrgn) == 1) %>% distinct() %>%
  select(zip, subrgn) %>% distinct()

# update zipsubregion
zip_utility_subregion_19 <-
  zip_utility_subregion_18 %>%
  left_join(subregions_to_update_from_old_pp %>%
              rename(subregion = subrgn), 
            by = "zip") %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### Format to primary, secondary, and tertiary assignments -----

# identify secondary and tertiary subregions - those not listed in primary zipcode data
zip_additional_subregion <-
  zip_utility_subregion_19 %>%
  select(zip, subregion) %>% distinct() %>%
  anti_join(zip_primary_subregion_2, by = c("zip", "subregion"))

# assign first alphabetical subregion as secondary
zip_secondary_subregion <-
  zip_additional_subregion %>%
  arrange(as.numeric(zip), subregion) %>%
  group_by(zip) %>%
  summarize(subregion_secondary = first(subregion))

# assign remaining subregions as tertiary and create unique columns for each subregion level
zip_secondary_tertiary_subregion <-
  zip_secondary_subregion %>%
  left_join(zip_additional_subregion, by = "zip") %>%
  # identify tertiary subregions if original subregion doesn't match assigned secondary
  mutate(tertiary = if_else(subregion == subregion_secondary, 0, 1)) %>%
  select(zip, subregion, tertiary) %>%
  # create column for each subregion type (primary, secondary, tertiary)
  pivot_wider(
    names_from = tertiary,
    values_from = subregion,
    values_fn = list(subregion = ~first(.))) %>%
  rename(subregion_secondary = "0", subregion_tertiary = "1")

# join secondary and tertiary assignments to primary assignments
zip_subregion_assignments <-
  zip_primary_subregion_2 %>%
  left_join(zip_secondary_tertiary_subregion, by = "zip")

# Create Website Data - assign predominant utilities ------

### Assign predominant utility using first utility name for each zip -----

# reset predominant utility assignments
zip_website_1 <-
  zip_utility_subregion_18 %>%
  # account for capitalization and dashes when alphabetizing
  mutate(utility_name_clean = tolower(str_replace_all(utility_name, "-", "")),
         predominant_utility = as.factor(0)) %>%
  arrange(zip, utility_name_clean) %>%
  select(zip, state, utility_name, trim_util_code = eiaid, subregion,  predominant_utility)

# identify first utility name for each zipcode (alphabetical)
first_of_utility_name <-
  zip_website_1 %>%
  group_by(zip, state) %>%
  summarize(first_of_utility_name = first(utility_name)) %>%
  filter(!is.na(first_of_utility_name)) %>%
  mutate(predominant_utility = as.factor(1)) %>%
  ungroup()

# update predominant utility in zipcodes for website
zip_website_2 <-
  zip_website_1 %>%
  left_join(first_of_utility_name, by = c("utility_name" = "first_of_utility_name", "state", "zip")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

### Assign remaining zips' predominant utility using old power plant assignment -----

zip_website_3 <-
  zip_website_2 %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              filter(is.na(eiaid)) %>%
              select(zip, eiaid, predominant_utility),
            by = "zip") %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."), -eiaid, eiaid = trim_util_code)

# Format final data  -----
# all utility zipcodes and subregions
zip_utility_subregion_final <-
  zip_utility_subregion_19 %>%
  select(zip, state, eiaid, utility_name, subregion, predominant_utility) %>%
  arrange(as.numeric(zip), as.numeric(eiaid))

# zipcode subregion assignments
zip_subregion_assignments_final <-
  zip_subregion_assignments %>%
  mutate(zip_numeric = as.numeric(zip)) %>%
  select(zip, zip_numeric, state, subregion, subregion_secondary, subregion_tertiary) %>%
  arrange(zip_numeric)

# Export data -----
source("scripts/functions/function_save_output_data.R")
output_folder <- "2a_power_profiler"
save_output_data(zip_utility_subregion_final, output_folder, "zip_utility_subregion.RDS")
save_output_data(zip_subregion_assignments_final, output_folder, "zip_subregion_assignments.RDS")