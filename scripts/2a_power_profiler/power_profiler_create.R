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
library(readxl)

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
plant_file <- # 12612(12619)
  readRDS(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/plant_file.RDS")) %>%
  glimpse()

# load in previous power profiler data which had zip, utility code, predominant utility, etc. 
power_profiler_old <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/power_profiler_old.csv"),
           col_types = "cccccc") %>%
  janitor::clean_names() %>%
  mutate(predominant_utility = as.factor(predominant_utility)) %>%
  glimpse()

# load in all utility zip codes
utility_zipcodes <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/utility_zipcodes.csv"),
           col_types = "ccccccddd") %>%
  janitor::clean_names() %>%
  glimpse()

# load in EIA-861 utility data - with count 
eia_861_utility_data_with_count <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/eia_861_utility_with_count.csv"),
           col_types = "ccccccccccccccccccccccdcccccccccc") %>%
  janitor::clean_names() %>%
  glimpse()

# load in EIA-861 utility data
eia_861_sales_ult_cust <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/eia_861_sales_ult_cust.csv"),
           col_types = "cccccccc") %>%
  janitor::clean_names() %>%
  glimpse()

# Load necessary crosswalks -----

# subregion to balancing authority crosswalk
xwalk_ba_subregion <-
  read_csv("data/1_production_model/static_tables/xwalk_balancing_authority.csv") %>%
  janitor::clean_names() %>%
  select(subregion = subrgn, ba_code = balancing_authority_code) %>%
  glimpse()

# NERC region, BA, transmission, and subregion crosswalk
xwalk_ba_transmission <- # 1204(1160)
  read_csv("data/1_production_model/static_tables/xwalk_subregion_transmission.csv",
           col_types = "cccc") %>%
  janitor::clean_names() %>%
  rename(ba_code = balancing_authority_code, transmission = transmission_or_distribution_system_owner_id, subregion = subrgn) %>%
  glimpse()

# NERC to subregion crosswalk
xwalk_nerc_region <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/xwalk_nerc_region.csv"),
           col_names = c("nerc","subregion")) %>%
  janitor::clean_names() %>%
  glimpse()

# Transmission to subregion crosswalk
xwalk_transmissionid_subregion <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/xwalk_transmissionid_subregion.csv"),
           col_types = "cc") %>%
  janitor::clean_names() %>%
  glimpse()

# Crosswalk for missing utility ids
xwalk_missing_utilityid <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/xwalk_missing_utilityid.csv"),
           col_types = "cccc") %>%
  janitor::clean_names() %>%
  glimpse()

# Manual subregion updates
utility_subregion_manual_updates <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/manual_updates_utility_subregion.csv"),
           col_types = "ccc") %>%
  janitor::clean_names() %>%
  glimpse()

# Manual predominant utility updates
predominant_utility_manual_updates <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/manual_updates_predominant_utility.csv"),
           col_types = "ccccc") %>%
  janitor::clean_names() %>%
  select(zip, first_ofeiaid) %>%
  mutate(predominant_utility = as.factor("0")) %>%
  glimpse()

# Manual primary subregion updates
primary_subregion_manual_updates <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/manual_updates_primary_subregion.csv"),
           col_types = "ccc") %>%
  janitor::clean_names() %>%
  glimpse()

# Create zipcode dataset ------

# create zipcode dataset using utility_zipcodes data
zip_subregion_1 <-
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
zip_subregion_2 <-
  zip_subregion_1 %>%
  bind_rows(zip_codes_from_old_power_profiler_to_add) %>%
  mutate(subregion = NA_character_) %>%
  glimpse()

# Assign zipcodes to subregions using a sequence of methods -------

### NERC Region assignments --------

# get NERC subregions for eiaid and utility_numbers in zipsubregion data
nerc_region_for_missing_utility_id <-
  zip_subregion_2 %>%
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

# assign nerc regions to eiaids with singular subregion count
nerc_names_for_missing_utility_id <-
  nerc_region_for_missing_utility_id %>%
  inner_join(count_of_one_nerc_region, by = "eiaid") %>%
  select(eiaid, utility_name = utility_name.x, nerc_region)

# combine the subregion/nerc match to the nerc names for missing utility id
zip_utility_update_nerc <-
  nerc_names_for_missing_utility_id %>%
  inner_join(xwalk_nerc_region, by = join_by("nerc_region" == "nerc")) %>% distinct()

#### Update subregions based on NERC crosswalk -----

# join in utility ids with proper nerc region
zip_subregion_3 <-
  zip_subregion_2 %>%
  left_join(zip_utility_update_nerc, by = c("eiaid", "utility_name")) %>%
  # update subregion and specify matching method
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -nerc_region)

### Balancing Authority assignment --------

# create ba codes for missing utility id dataset
ba_names_for_missing_utility_id <-
  zip_subregion_3 %>%
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
zip_subregion_4 <-
  zip_subregion_3 %>%
  left_join(zip_utility_update_ba, by = c("eiaid", "utility_name")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "balancing authority", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -ba_code) %>%
  glimpse()

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

zip_subregion_5 <-
  zip_subregion_4 %>%
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
zip_subregion_6 <-
  zip_subregion_5 %>%
  left_join(utilityid_subregions, by = c("zip", "eiaid")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), "plant file - utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### NERC Region / BA / Transmission ID assignment -----

# update subregion using BA transmission crosswalk
zip_subregion_7 <-
  zip_subregion_6 %>%
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

### Power Profiler Transmission ID Crosswalk assignment --------

# update subregion bsaed on transmission ID subregion crosswalK
zip_subregion_8 <-
  zip_subregion_7 %>%
  left_join(xwalk_transmissionid_subregion, by = c("eiaid" = "transmission_id")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "power profiler transmission ID crosswalk", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct()

### Crosswalk for Missing Utility ID assignment --------

# update subregions for utility IDs not in eGRID
zip_subregion_9 <-
  zip_subregion_8 %>%
  left_join(xwalk_missing_utilityid %>%
              select(utility_number, subregion), 
            by = c("eiaid" = "utility_number")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "crosswalk for missing utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct()

# Subregion corrections -----

### Update missing subregion for zip codes from old power profiler -----
zip_subregion_10 <-
  zip_subregion_9 %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              select(zip, eiaid, subregion), 
            by = c("zip", "eiaid")) %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct()

 #%%%%%%######
### State subregion one to one match -------

states_and_single_subregions <-
  plant_file %>%
  select(state = plant_state, subregion = egrid_subregion) %>%
  filter(!is.na(subregion)) %>% distinct() %>%
  group_by(state) %>%
  filter(n_distinct(subregion) == 1)

#### Update values in subregion data -----

zip_subregion_11 <-
  zip_subregion_10 %>%
  left_join(states_and_single_subregions, by = "state") %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "based on state", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### Make manual changes to table --------

zip_subregion_12 <-
  zip_subregion_11 %>%
  left_join(utility_subregion_manual_updates, by = c("state", "eiaid" = "utility_id")) %>%
  mutate(method = if_else(!is.na(subregion.y), "manual override", method),
         subregion = if_else(!is.na(subregion.y), subregion.y, subregion.x)) %>%
  select(-contains("."))

# Update Predominant Utilities -----

### Zips with one utility assignment ------
zip_subregion_13 <-
  zip_subregion_12 %>%
  select(zip, eiaid) %>% distinct() %>%
  group_by(zip) %>%
  filter(n_distinct(eiaid) == 1) %>%
  ungroup() %>%
  mutate(predominant_utility = as.factor(1))

zip_subregion_14 <-
  zip_subregion_12 %>%
  left_join(zip_subregion_13, by = c("zip", "eiaid")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

### Zips with no utility assignment ------

#### Update with assignment from old profiler -----

# select zipcodes and predominant utility from old profiler for zips with no predominant utility
no_predominant_utility <-
  zip_subregion_14 %>%
  group_by(zip) %>%
  filter(!any(predominant_utility != 0)) %>%
  select(zip) %>% distinct() %>%
  ungroup() %>%
  inner_join(power_profiler_old, by = "zip") %>%
  select(zip, trim_util_code, predominant_utility)

# update predominant utilties in zipcode data
zip_subregion_15 <-
  zip_subregion_14 %>%
  left_join(no_predominant_utility, by = c("zip", "eiaid" = "trim_util_code")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

#### Update as first EIAID from old power profiler ------

# check again for zipcodes without any predominant utility
no_predominant_utility_2 <-
  zip_subregion_15 %>%
  group_by(zip) %>%
  filter(!any(predominant_utility != 0)) %>%
  select(zip) %>% distinct() %>%
  ungroup() %>%
  arrange(as.numeric(zip))

# identify 'first' EIAID in zipcode
zips_to_update_first_eiaid <-
  zip_subregion_15 %>%
  arrange(as.numeric(zip), as.numeric(eiaid), predominant_utility) %>%
  inner_join(no_predominant_utility_2, by = "zip") %>%
  group_by(zip) %>%
  summarize(first_of_eia = first(eiaid)) %>%
  mutate(predominant_utility = as.factor("1"))

# Update zipcode dataset
zip_subregion_16 <-
  zip_subregion_15 %>%
  left_join(zips_to_update_first_eiaid, by = c("zip", "eiaid" = "first_of_eia")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

### Override from table ------

#### Zipcode matching -----

zip_subregion_17 <-
  zip_subregion_16 %>%
  left_join(predominant_utility_manual_updates, by = "zip") %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."), -first_ofeiaid)

#### Zipcode and first eiaid matching -----
zip_subregion_18 <-
  zip_subregion_17 %>%
  left_join(predominant_utility_manual_updates %>%
              mutate(predominant_utility = as.factor("1")), 
            by = c("zip", "eiaid" = "first_ofeiaid")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

# Update Subregion assignments -----

### Zips with one subregion assignment -----

subregions_primary <-
  zip_subregion_18 %>%
  filter(predominant_utility == 1) %>%
  select(zip, state, subregion) %>% distinct() %>%
  mutate(secondary = as.factor("0"))

### Zips with multiple subregion assignments -----
zips_with_more_than_one_subregion <-
  zip_subregion_18 %>%
  group_by(zip) %>%
  filter(n_distinct(subregion) > 1) %>%
  ungroup() %>%
  select(zip) %>% distinct() %>%
  mutate(secondary = as.factor("1"))

# update primary subregion table
subregions_secondary <-
  subregions_primary %>%
  left_join(zips_with_more_than_one_subregion, by = "zip") %>%
  mutate(secondary = if_else(!is.na(secondary.y), secondary.y, secondary.x)) %>%
  select(-contains("."))

### Override from table -----

subregions_override <-
  subregions_secondary %>%
  left_join(primary_subregion_manual_updates, by = "zip") %>%
  mutate(subregion = if_else(!is.na(change), change, subregion)) %>%
  select(-current, -change) %>% distinct()

# Create data for final website ------

### Update predominant utility using first utility site name -----

website_zip_subregion_1 <-
  zip_subregion_18 %>%
  mutate(predominant_utility = as.factor(0)) %>%
  select(zip, state, utility_name, trim_util_code = eiaid, subregion,  predominant_utility) %>%
  arrange(zip, tolower(utility_name))

first_of_utility_name <-
  website_zip_subregion_1 %>%
  group_by(zip, state) %>%
  summarize(first_of_utility_name = first(utility_name)) %>%
  mutate(predominant_utility = as.factor(1)) %>%
  ungroup() %>%
  filter(!is.na(first_of_utility_name))

# update predominant utility in zipcodes for website
website_zip_subregion_2 <-
  website_zip_subregion_1 %>%
  left_join(first_of_utility_name, by = c("utility_name" = "first_of_utility_name", "state", "zip")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

### Update predominant utility using old power plant assignment -----

website_zip_subregion_final <-
  website_zip_subregion_2 %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              select(zip, eiaid, predominant_utility) %>%
              filter(is.na(eiaid)),
            by = "zip") %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."), -eiaid, eiaid = trim_util_code)

# Update missing subregions with those from old profiler -----

subregions_to_update_from_old_pp <-
  zip_subregion_18 %>%
  filter(is.na(subregion)) %>%
  select(zip, subregion) %>% distinct() %>%
  inner_join(power_profiler_old, by = "zip") %>%
  group_by(zip) %>%
  filter(n_distinct(subrgn) == 1) %>% distinct() %>%
  select(zip, subrgn) %>% distinct()

# update zipsubregion
zip_subregion_final <-
  zip_subregion_18 %>%
  left_join(subregions_to_update_from_old_pp %>%
              rename(subregion = subrgn), 
            by = "zip") %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

print("DONE - Final Zipcode Subregion Data Produced")