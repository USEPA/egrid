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
##    * Primary, secondary, and tertiary subregions for each zipcode
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

# Load necessary functions
source("scripts/functions/function_check_params.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and temporal resolution parameters are already defined.")
}

# Load necessary data -----

# load in eGRID plant data
# plant_file <-
#   readRDS(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/plant_file.RDS"))

plant_file <-
  read_csv(glue::glue("data/2c_power_profiler/static_tables/qa/{params$eGRID_year}/plant_access.csv")) %>%
  janitor::clean_names() %>%
  mutate(transmission_or_distribution_system_owner_id = as.character(transmission_or_distribution_system_owner_id),
         utility_id = as.character(utility_id)) %>%
  rename(egrid_subregion = subrgn,
         system_owner_id = transmission_or_distribution_system_owner_id,
         plant_state = state)

# load in previous power profiler data which has zip, utility code, predominant utility, etc. 
power_profiler_old <-
  read_csv(glue::glue("data/2c_power_profiler/inputs/{params$eGRID_year}/power_profiler_old.csv"),
           col_types = "cccccc") %>%
  janitor::clean_names()

# load utility zipcode data
utility_zipcodes <-
  read_csv(glue::glue("data/2c_power_profiler/inputs/{params$eGRID_year}/utility_zipcodes.csv"),
           col_types = "ccccccddd") %>%
  janitor::clean_names()

# load in EIA-861 data
if(file.exists(glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}/eia_861_clean.RDS"))) { 
  eia_861 <-
    read_rds(glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}/eia_861_clean.RDS"))
} else { 
  stop("eia_861_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")
}

# EIA-861 utility data
eia_861_utility <-
  eia_861$utility_data %>%
  mutate(year = as.character(year),
         utility_number = as.character(utility_number))

# EIA-861 sales ult cust data
eia_861_sales_ult_cust <-
  eia_861$sales_ult_cust %>%
  mutate(year = as.character(year),
         utility_number = as.character(utility_number)) %>%
  filter(year == params$eGRID_year)

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

# Crosswalk for missing utility ids
xwalk_missing_utilityid <-
  read_csv(glue::glue("data/2c_power_profiler/static_tables/xwalk_missing_utilityid.csv"),
           col_types = "cccc") %>%
  janitor::clean_names()

# Create zipcode dataset ------

# create zipcode dataset using utility_zipcodes data
zip_utility_subregion_1 <- #02
  utility_zipcodes %>%
  select(zip, state, eiaid, utility_name) %>%
  mutate(subregion = NA_character_, 
         predominant_utility = "0", 
         old_zip_code = "no", 
         method = NA_character_) %>% distinct()

### Add zipcodes from old power profiler  -------

# select zip codes in old power profiler not in utility_zipcodes
zip_codes_from_old_power_profiler_to_add <- #01
  power_profiler_old %>%
  anti_join(utility_zipcodes, by = "zip") %>%
  mutate(old_zip_code = "yes", 
         method = "old power profiler")

# append zipcodes from previous power profiler to add
zip_utility_subregion_2 <- #03
  zip_utility_subregion_1 %>%
  bind_rows(zip_codes_from_old_power_profiler_to_add) %>%
  mutate(subregion = NA_character_)

# Assign subregions to utility using a sequence of methods -------
#' Subregions are assigned by grouping data by desired variable and identifying utilities with a singular subregion in the following sequence:
#'  Grouped By:
#'    1) NERC Region
#'    2) Balancing Authority Code
#'    3) Plant Transmission ID
#'    4) Plant Utility ID
#'    5) NERC Region, BA Code, and Transmission ID
#'    6) Missing Utility ID Crosswalk

### NERC Region assignments --------

# get NERC subregions for eiaid and utility_numbers in zipsubregion data
nerc_region_for_missing_utility_id <- #28
  zip_utility_subregion_2 %>%
  inner_join(eia_861_utility, by = c("eiaid" = "utility_number")) %>%
  select(eiaid, utility_name = utility_name.x, nerc_region) %>% distinct() %>%
  arrange(as.numeric(eiaid))

# identify records with only one subregion assignment
count_of_one_nerc_region <- #29
  nerc_region_for_missing_utility_id %>%
  group_by(eiaid, utility_name) %>%
  summarize(subrgn_count = n_distinct(nerc_region)) %>%
  filter(subrgn_count == 1) %>%
  arrange(as.numeric(eiaid)) %>%
  ungroup()

# assign subregion for unique cases with one-to-one NERC-subregion match
zip_utility_update_nerc <- #30/31
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
zip_utility_subregion_3 <- #32
  zip_utility_subregion_2 %>%
  left_join(zip_utility_update_nerc, by = c("eiaid", "utility_name")) %>%
  # update subregion and specify matching method
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -nerc_region)

### Balancing Authority assignment --------

# create ba codes for missing utility id dataset
ba_names_for_missing_utility_id <- #33/34/35
  zip_utility_subregion_3 %>%
  inner_join(eia_861_sales_ult_cust, by = c("eiaid" = "utility_number")) %>%
  select(eiaid, utility_name = utility_name.x, ba_code) %>% distinct() %>%
  group_by(eiaid, utility_name) %>%
  # select utilities with singular BA code
  filter(n_distinct(ba_code) == 1) %>%
  ungroup()

# match ba codes to subregions
zip_utility_update_ba <- #36
  ba_names_for_missing_utility_id %>%
  inner_join(xwalk_ba_subregion, by = "ba_code") %>% distinct() %>%
  arrange(as.numeric(eiaid))

#### Update subregion based on BA crosswalk -----

# join in utility ids with proper nerc region
zip_utility_subregion_4 <- #37
  zip_utility_subregion_3 %>%
  left_join(zip_utility_update_ba, by = c("eiaid", "utility_name")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "balancing authority", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -ba_code)

### Plant Transmission ID assignment ------------

# match plant file subregion data to zipcode utilities
plant_transmission_subregions <- #5/6/7/8/8/10/11
  plant_file %>%
  select(egrid_subregion, system_owner_id) %>% distinct() %>%
  inner_join(utility_zipcodes, by = join_by("system_owner_id" == "eiaid")) %>%
  select(zip, subregion = egrid_subregion, eiaid = system_owner_id) %>% distinct() %>%
  # select utilities with singluar subregion
  group_by(zip, eiaid) %>%
  filter(n_distinct(subregion) == 1) %>%
  ungroup()

#### Update subregions based on transmission ID plant file -----------

zip_utility_subregion_5 <- #12
  zip_utility_subregion_4 %>%
  left_join(plant_transmission_subregions, by = c("zip", "eiaid")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), "plant file - transmission ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### Plant Utility ID assignment --------

# assign subregions by plant utility ID
utilityid_subregions <- #17/18/19/20/21/22
  plant_file %>%
  select(egrid_subregion, utility_id) %>% distinct() %>%
  inner_join(utility_zipcodes, by = join_by("utility_id" == "eiaid")) %>%
  select(zip, subregion = egrid_subregion, eiaid = utility_id) %>% distinct() %>%
  group_by(zip, eiaid) %>%
  filter(n_distinct(subregion) == 1) %>%
  ungroup()

#### Update subregions based on plant utility ID ---------
zip_utility_subregion_6 <- #23
  zip_utility_subregion_5 %>%
  left_join(utilityid_subregions, by = c("zip", "eiaid")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), "plant file - utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### NERC Region / BA / Transmission ID assignment -----

# update subregion using BA transmission crosswalk
zip_utility_subregion_7 <- #38
  zip_utility_subregion_6 %>%
  left_join(eia_861_sales_ult_cust %>% 
              select(utility_number, ba_code),
            by = c("eiaid" = "utility_number")) %>%
  left_join(eia_861_utility %>% 
              select(utility_number, nerc_region),
            by = c("eiaid" = "utility_number")) %>%
  left_join(xwalk_ba_transmission,
            by = c("nerc_region", "ba_code", "eiaid" = "transmission")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region/ba/transmission ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -ba_code, -nerc_region) %>% distinct()

### Missing Utility ID crosswalk --------

# update subregions for utility IDs not in eGRID
zip_utility_subregion_8 <- #40
  zip_utility_subregion_7 %>%
  left_join(xwalk_missing_utilityid %>%
              select(utility_number, subregion), 
            by = c("eiaid" = "utility_number")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "crosswalk for missing utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct()

# Update subregion assignments when necessary -----

# ### Update missing subregion for zip codes from old power profiler -----
zip_utility_subregion_9 <- #41
  zip_utility_subregion_8 %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              select(zip, eiaid, subregion),
            by = c("zip", "eiaid")) %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct()

### Based on states with one subregion -------

# identify states with unique subregion
states_and_single_subregions <- #15
  plant_file %>%
  select(state = plant_state, subregion = egrid_subregion) %>%
  filter(!is.na(subregion)) %>% distinct() %>%
  group_by(state) %>%
  filter(n_distinct(subregion) == 1)

# update subregion values
zip_utility_subregion_10 <- #16
  zip_utility_subregion_9 %>%
  left_join(states_and_single_subregions, by = "state") %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "based on state", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### Old power profiler updates --------

# override subregion assignments from old power profiler
zip_utility_subregion_11 <- #42 - updated
  zip_utility_subregion_10 %>%
  left_join(power_profiler_old %>%
              select(zip, eiaid, subregion),
            by = c("zip", "eiaid")) %>%
  mutate(method = if_else(!is.na(subregion.y), "from old power profiler", method),
         subregion = if_else(!is.na(subregion.x) & !is.na(subregion.y), subregion.y, subregion.x)) %>%
  select(-contains("."))

# Update Predominant Utilities-----

### Assign predominant utility for zips with one utility assignment ------

# identify zipcodes with only one utility
zip_utility_subregion_12 <- #46/47
  zip_utility_subregion_11 %>%
  select(zip, eiaid) %>% distinct() %>%
  group_by(zip) %>%
  filter(n_distinct(eiaid) == 1) %>%
  mutate(predominant_utility = "1",
         predominant_utility_method = "one utility") %>%
  ungroup()

# assign zipcodes with one utility to predominant utility
zip_utility_subregion_13 <- #48
  zip_utility_subregion_11 %>%
  left_join(zip_utility_subregion_12, by = c("zip", "eiaid")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."))

### Update zips with no utility assignment to old profiler assignments ------

# select zipcodes and predominant utility for zips with no predominant utility assignment from old profiler 
no_predominant_utility <- #49
  zip_utility_subregion_13 %>%
  group_by(zip) %>%
  # select zipcodes without predominant utility assignment
  filter(!any(predominant_utility != "0")) %>%
  select(zip) %>% distinct() %>%
  ungroup() %>%
  # gather old power profiler assignment
  inner_join(power_profiler_old, by = "zip") %>%
  select(zip, eiaid, predominant_utility) %>%
  mutate(predominant_utility_method = "old power profiler")

# update predominant utility assignment
zip_utility_subregion_14 <- #50
  zip_utility_subregion_13 %>%
  left_join(no_predominant_utility, by = c("zip", "eiaid")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x),
         predominant_utility_method = if_else(!is.na(predominant_utility_method.y), predominant_utility_method.y, predominant_utility_method.x)) %>%
  select(-contains(".")) 

### Update as first EIAID from old power profiler ------
# because data is organized by eiaid, this will be the lowest eiaid number

# check again for zipcodes without predominant utility assignment
no_predominant_utility_2 <- #49
  zip_utility_subregion_14 %>%
  group_by(zip) %>%
  filter(!any(predominant_utility != "0")) %>%
  select(zip) %>% distinct() %>%
  arrange(as.numeric(zip)) %>%
  ungroup()
  
# identify 'first' EIAID in zipcode and assign as predominant utility
zips_to_update_first_eiaid <- #51
  zip_utility_subregion_14 %>%
  arrange(as.numeric(zip), as.numeric(eiaid), predominant_utility) %>%
  inner_join(no_predominant_utility_2, by = "zip") %>%
  group_by(zip) %>%
  summarize(first_of_eia = first(eiaid)) %>%
  mutate(predominant_utility = "1",
         predominant_utility_method = "first eiaid")

# Update zipcode dataset
zip_utility_subregion_15 <- #52
  zip_utility_subregion_14 %>%
  left_join(zips_to_update_first_eiaid, by = c("zip", "eiaid" = "first_of_eia")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x),
         predominant_utility_method = if_else(!is.na(predominant_utility_method.y), predominant_utility_method.y, predominant_utility_method.x)) %>%
  select(-contains("."))

# Create Subregion Assignments Data -----

### Primary subregion for zips assigned as predominant utility -----
subregions_primary <- #53
  zip_utility_subregion_15 %>%
  filter(predominant_utility == "1") %>%
  select(zip, state, subregion) %>% distinct() %>%
  mutate(secondary = "0")

### Zips with multiple subregion assignments -----

### Secondary subregions for zips with multiple assignments -----
subregions_secondary <- #55/56
  zip_utility_subregion_15 %>%
  filter(!is.na(subregion)) %>%
  group_by(zip) %>%
  filter(n_distinct(subregion) > 1) %>%
  ungroup() %>%
  select(zip) %>% distinct() %>%
  mutate(secondary = "1")

# update primary subregion table
zip_primary_subregion <- #57
  subregions_primary %>%
  left_join(subregions_secondary, by = "zip") %>%
  mutate(secondary = if_else(!is.na(secondary.y), secondary.y, secondary.x)) %>%
  select(-contains("."))

### Fill missing subregions with those from old profiler -----

subregions_to_update_from_old_pp <- #71/72/73/74/75
  zip_utility_subregion_15 %>%
  filter(is.na(subregion)) %>%
  select(zip, subregion) %>% distinct() %>%
  inner_join(power_profiler_old, by = "zip") %>%
  group_by(zip) %>%
  filter(n_distinct(subregion.y) == 1) %>% distinct() %>%
  select(zip, subregion = subregion.y) %>% distinct()

# update zipsubregion
zip_utility_subregion_16 <- #76
  zip_utility_subregion_15 %>%
  left_join(subregions_to_update_from_old_pp, 
            by = "zip") %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."))

### Format to primary, secondary, and tertiary assignments -----

# identify secondary and tertiary subregions - those not listed in primary zipcode data
zip_additional_subregion <- #62
  zip_utility_subregion_16 %>%
  select(zip, subregion) %>% distinct() %>%
  anti_join(zip_primary_subregion, by = c("zip", "subregion"))

# assign first alphabetical subregion as secondary
zip_secondary_subregion <- #62
  zip_additional_subregion %>%
  arrange(as.numeric(zip), subregion) %>%
  group_by(zip) %>%
  summarize(subregion_2 = first(subregion))

# assign remaining subregions as tertiary and create unique columns for each subregion level
zip_secondary_tertiary_subregion <- #63
  zip_secondary_subregion %>%
  left_join(zip_additional_subregion, by = "zip") %>%
  # identify tertiary subregions if original subregion doesn't match assigned secondary
  mutate(tertiary = if_else(subregion == subregion_2, 0, 1)) %>%
  select(zip, subregion, tertiary) %>%
  filter(!is.na(tertiary)) %>%
  # create column for each subregion type (primary, secondary, tertiary)
  pivot_wider(
    names_from = tertiary,
    values_from = subregion,
    values_fn = list(subregion = ~first(.))) %>%
  rename(subregion_2 = "0", subregion_3 = "1")

# join secondary and tertiary assignments to primary assignments
zip_subregion_assignments <- #64
  zip_primary_subregion %>%
  left_join(zip_secondary_tertiary_subregion, by = "zip")

# Format final data  -----
# all utility zipcodes and subregions
zip_utility_subregion_final <-
  zip_utility_subregion_16 %>%
  mutate(zip = str_pad(zip, width = 5, side = "left", pad = "0")) %>%
  select(zip, state, eiaid, utility_name, subregion, predominant_utility) %>%
  arrange(as.numeric(zip), as.numeric(eiaid))

# zipcode subregion assignments
zip_subregion_assignments_final <-
  zip_subregion_assignments %>%
  mutate(zip_numeric = as.numeric(zip),
         zip = str_pad(zip, width = 5, side = "left", pad = "0")) %>%
  arrange(zip_numeric) %>%
  select(zip, state, subregion_1 = subregion, subregion_2, subregion_3)

# Export data -----
source("scripts/functions/function_save_output_data.R")
output_folder <- "data/2c_power_profiler/outputs"
save_output_data(zip_utility_subregion_final, output_folder, "zip_utility_subregion.RDS")
save_output_data(zip_subregion_assignments_final, output_folder, "zip_subregion_assignments.RDS")
