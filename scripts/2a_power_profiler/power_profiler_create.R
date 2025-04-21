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
power_profiler_old <- # (FROM ACCESS) 65187
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/power_profiler_old.csv"),
           col_types = "cccccc") %>%
  janitor::clean_names() %>%
  mutate(predominant_utility = as.factor(predominant_utility)) %>%
  glimpse()

# load in all utility zip codes
utility_zipcodes <- # (FROM MARISSA) 80142 (39133 unique zips)
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/utility_zipcodes.csv"),
           col_types = "ccccccddd") %>%
  janitor::clean_names() %>%
  # group_by(zip) %>%
  # count() %>%
  glimpse()

# load in EIA-861 utility data - with count 
eia_861_utility_data_with_count <- # (FROM ACCESS) 1715
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/eia_861_utility_with_count.csv"),
           col_types = "ccccccccccccccccccccccdcccccccccc") %>%
  janitor::clean_names() %>%
  # rename_with(~ paste(., "_eia_count", sep = "")) %>%
  glimpse()

# load in EIA-861 utility data
eia_861_sales_ult_cust <- # (FROM ACCESS) 2822
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/eia_861_sales_ult_cust.csv"),
           col_types = "cccccccc") %>%
  janitor::clean_names() %>%
  glimpse()

# Load necessary crosswalks -----

# subregion to balancing authority crosswalk
ba_subregion_crosswalk <- #75 - access has missing data
  read_csv("data/1_production_model/static_tables/xwalk_balancing_authority.csv") %>%
  janitor::clean_names() %>%
  select(subregion = subrgn, ba_code = balancing_authority_code) %>%
  glimpse()

# NERC region, BA, transmission, and subregion crosswalk
ba_transmission_crosswalk <- # 1204 (access has 1160)
  read_csv("data/1_production_model/static_tables/xwalk_subregion_transmission.csv",
           col_types = "cccc") %>%
  janitor::clean_names() %>%
  rename(ba_code = balancing_authority_code, transmission = transmission_or_distribution_system_owner_id, subregion = subrgn) %>%
  glimpse()

# NERC to subregion crosswalk
#(this only has three nerc and subregion combos - what is this?)
nerc_region_crosswalk <- # (FROM ACCESS) 3
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/nerc_region_crosswalk.csv"),
           col_names = c("nerc","subregion")) %>%
  janitor::clean_names() %>%
  glimpse()

# Transission - subregion crosswalk
transmission_id_subregion_crosswalk <- # (ACCESS)
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/transmissionID_subregion_crosswalk.csv"),
           col_types = "cc") %>%
  janitor::clean_names() %>%
  glimpse()

# Crosswalk for missing utility ids
xwalk_for_missing_utility_id <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/xwalk_for_missing_utility_ID.csv"),
           col_types = "cccc") %>%
  janitor::clean_names() %>%
  glimpse()

# Manual subregion updates
utility_subregion_manual_table <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/utility_subregion_manual_table.csv"),
           col_types = "ccc") %>%
  janitor::clean_names() %>%
  glimpse()

# Manual predominant utility updates
predominant_utility_override <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/predom_util_override_2.csv"),
           col_types = "ccccc") %>%
  janitor::clean_names() %>%
  select(zip, first_ofeiaid) %>%
  mutate(predominant_utility = as.factor("0")) %>%
  glimpse()

# Manual primary subregion updates
primary_subregion_override <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/access/primary_subregion_override.csv"),
           col_types = "ccc") %>%
  janitor::clean_names() %>%
  glimpse()

# Create zipcode dataset ------

# create zipcode dataset using utility_zipcodes data
#02: 62,195
zip_subregion <-
  utility_zipcodes %>%
  select(zip, state, eiaid, utility_name) %>%
  mutate(subregion = NA_character_, 
         predominant_utility = factor("0", levels = c("0", "1")), 
         old_zip_code = factor("no", levels = c("no", "yes")), 
         method = NA_character_) %>% distinct() %>%
  glimpse()

### Add zipcodes from old power profiler  -------

# select zip codes in old power profiler not in utility_zipcodes
#01: 2889
zip_codes_from_old_power_profiler_to_add <-
  power_profiler_old %>%
  anti_join(utility_zipcodes, by = "zip") %>%
  mutate(predominant_utility = factor(predominant_utility),
         old_zip_code = factor("yes", levels = c("no", "yes")), 
         method = "old power profiler") %>%
  rename(utility_name = util_name, 
         eiaid = trim_util_code, 
         subregion = subrgn) %>%
  glimpse()

# append zipcodes from previous power profiler to add
#03 : 65084
zip_subregion_old_pp <-
  zip_subregion %>%
  bind_rows(zip_codes_from_old_power_profiler_to_add) %>%
  mutate(subregion = NA_character_) %>%
  glimpse()
  
# methodcount <- #2889/62195
#   zip_subregion_old_pp %>%
#   count(method) %>%
#   print()
# 
# predom_util_count <- #62655/2429
#   zip_subregion_old_pp %>%
#   count(predominant_utility) %>%
#   print()

# Assign zipcodes to subregions using a sequence of methods -------
### NERC Region assignments --------

# get NERC subregions for eiaid and utility_numbers in zipsubregion data
#28: 1207
nerc_region_for_missing_utility_id <-
  zip_subregion_old_pp %>%
  inner_join(eia_861_utility_data_with_count, by = c("eiaid" = "utility_number")) %>%
  select(eiaid, utility_name = utility_name.x, nerc_region) %>% distinct() %>%
  arrange(as.numeric(eiaid)) %>%
  glimpse()

# identify records with only one subregion assignment
#29: 1207
count_of_one_nerc_region <-
  nerc_region_for_missing_utility_id %>%
  group_by(eiaid, utility_name) %>%
  summarize(subrgn_count = n_distinct(nerc_region)) %>%
  filter(subrgn_count == 1) %>%
  arrange(as.numeric(eiaid)) %>%
  ungroup() %>%
  glimpse()

# assign nerc regions to eiaids with singular subregion count
#30: 1213
nerc_names_for_missing_utility_id <-
  nerc_region_for_missing_utility_id %>%
  inner_join(count_of_one_nerc_region, by = "eiaid") %>%
  select(eiaid, utility_name = utility_name.x, nerc_region) %>%
  glimpse()

# combine the subregion/nerc match to the nerc names for missingutility id
#31: 82 - now 80 because there were two duplicates
zip_utility_update_nerc <-
  nerc_names_for_missing_utility_id %>%
  inner_join(nerc_region_crosswalk, by = join_by("nerc_region" == "nerc")) %>% distinct() %>%
  glimpse()

#### Update subregions based on NERC crosswalk -----

#32: 65084
# join in utility ids with proper nerc region
zip_subregion_nerc_xwalk <-
  zip_subregion_old_pp %>%
  left_join(zip_utility_update_nerc, by = c("eiaid", "utility_name")) %>%
  # update subregion and specify matching method
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -nerc_region) %>%
  glimpse()

# zipcount <- #65084 -2861/2888/59335
#   zip_subregion_nerc_xwalk %>%
#   count(method) %>%
#   print()
# 
# zipsubregion <- #1662/1199/62223
#   zip_subregion_nerc_xwalk %>%
#   count(subregion) %>%
#   print()

### Balancing Authority assignment --------

# create ba codes for missing utility id dataset
#33/34/35: 1194
ba_names_for_missing_utility_id <-
  zip_subregion_nerc_xwalk %>%
  inner_join(eia_861_sales_ult_cust, by = c("eiaid" = "utility_number")) %>%
  select(eiaid, utility_name = utility_name.x, ba_code) %>% distinct() %>%
  group_by(eiaid, utility_name) %>%
  # select utilities with singular BA code
  filter(n_distinct(ba_code) == 1) %>%
  ungroup() %>%
  glimpse()

# match ba codes to subregions
#36: (1188 SAME) - 1182 as distinct
zip_utility_update_ba <- 
  ba_names_for_missing_utility_id %>%
  inner_join(ba_subregion_crosswalk, by = "ba_code") %>% distinct() %>%
  arrange(as.numeric(eiaid)) %>%
  glimpse() 

#### Update subregion based on BA crosswalk -----

# join in utility ids with proper nerc region
#37: 65084
zip_subregion_ba_xwalk <-
  zip_subregion_nerc_xwalk %>%
  left_join(zip_utility_update_ba, by = c("eiaid", "utility_name")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "balancing authority", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -ba_code) %>%
  glimpse()
  
# zipcount <- #65084(65084), ba 22931(57963), nerc 2861(2861), old power profiler 2796(2782), na 36496(1478)
#   zip_subregion_ba_xwalk %>%
#   count(method) %>%
#   print()

# zipsubregion_ba <- #30/1156/3008/2055/1931/2625/2519/578/1286/3064/4645/2895/39292
#   zip_subregion_ba_xwalk %>%
#   count(subregion) %>%
#   print()

### Plant Transmission ID assignment ------------

# match plant file subregion data to zipcode utilities
#5/6/7/8: 35005(35001)
group_zip_utility_subregion_transmission <-
  plant_file %>%
  select(egrid_subregion, system_owner_id) %>% distinct() %>%
  inner_join(utility_zipcodes, by = join_by("system_owner_id" == "eiaid")) %>%
  select(zip, subregion = egrid_subregion, eiaid = system_owner_id) %>% distinct() %>%
  # select utilities with singluar subregion
  group_by(zip, eiaid) %>%
  filter(n_distinct(subregion) == 1) %>%
  ungroup() %>%
  glimpse()

#9/10/11: 35005(35001)
plant_transmission_subregions <-
  group_zip_utility_subregion_transmission %>%
  # keep values with zipcodes within the utility zipcodes
  inner_join(utility_zipcodes %>%
               select(zip, eiaid) %>% distinct(), 
             by = c("zip", "eiaid")) %>%
  select(zip, eiaid, subregion) %>%
  arrange(zip) %>%
  glimpse()

#### Update subregions based on transmission ID plant file -----------
  
#12:
zip_subregion_plant_transmission <-
  zip_subregion_ba_xwalk %>%
  left_join(plant_transmission_subregions, by = c("zip", "eiaid")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), "plant file - transmission ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

# zipcount <-
#   zip_subregion_plant_transmission %>%
#   count(method) %>%
#   print()
# 
# zipsubregion_plnttr <-
#   zip_subregion_plant_transmission %>%
#   count(subregion) %>%
#   print(n = 26)

# zipsubregioncomp <- #112/1440/3008/2096/1931/2737/2698/1054/2792/3151/4832(4828)/4166/16747(16751)
#   zipsubregion_ba %>%
#   full_join(zipsubregion_plnttr, by = "subregion") %>%
#   glimpse() %>%
#   filter(n.x != n.y) %>%
#   print()

### Plant Utility ID assignment --------

# assign subregions by plant utility ID
#17/18/19/20/21/22: 25460
utilityid_subregions <-
  plant_file %>%
  select(egrid_subregion, utility_id) %>% distinct() %>%
  inner_join(utility_zipcodes, by = join_by("utility_id" == "eiaid")) %>%
  select(zip, subregion = egrid_subregion, eiaid = utility_id) %>% distinct() %>%
  group_by(zip, eiaid) %>%
  filter(n_distinct(subregion) == 1) %>%
  ungroup() %>%
  glimpse()

#### Update subregions based on plant utility ID ---------
#23: 65084
zip_subregion_plant_utilityid <-
  zip_subregion_plant_transmission %>%
  left_join(utilityid_subregions, by = c("zip", "eiaid")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), "plant file - utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

# zipcount <-
#   zip_subregion_plant_utilityid %>%
#   count(method) %>%
#   print()
# 
# zipsubregion_plntutilid <-
#   zip_subregion_plant_utilityid %>%
#   count(subregion) %>%
#   print(n = 27)

# zipsubregioncomp <- #112/85/1440/3008/2096/1931/68/69/730/4053/2737/3475/395/4/1129/3277/1304/4870/1087/998/1483/1661/2792/3151/4832(4828)/4166/14131(14135)
#   zipsubregion_plntutilid %>%
#   full_join(zipsubregion_plnttr, by = "subregion") %>%
#   print(n = 28) %>%
#   filter(n.x != n.y) %>%
#   print(n = 28)

### NERC Region / BA / Transmission ID assignment -----

# update subregion using BA transmission crosswalk
#38:
zip_subregion_nerc_ba_transmission <-
  zip_subregion_plant_utilityid %>%
  left_join(eia_861_sales_ult_cust %>%
              select(utility_number,
                     ba_code),
            by = c("eiaid" = "utility_number")) %>%
  left_join(eia_861_utility_data_with_count %>%
              select(utility_number,
                     nerc_region),
            by = c("eiaid" = "utility_number")) %>%
  left_join(ba_transmission_crosswalk,
            by = c("nerc_region", "ba_code", "eiaid" = "transmission")) %>%
  # update subregion and method if data is missing
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region/ba/transmission ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -ba_code, -nerc_region) %>% distinct() %>%
  glimpse()
  
# zipcount <- # ba 22931 (29702), nerc region 2861 (2861), nerc/ba/. 4451 (4455), old profiler 2793 (2782), plant file transm 22545 (22541), plant file utility 2616 (2616), NA 6887 (216)
#   zip_subregion_nerc_ba_transmission %>%
#   count(method) %>%
#   print()
# 
# zipsubregion_nercbatrns <-
#   zip_subregion_nerc_ba_transmission %>%
#   count(subregion) %>%
#   print(n = 27)
# 
# zipsubregioncomp <- #112/85/1440/3008/2096/1931/68/69/730/4148/2737/3501/395/213/2002/3895/1305/6323(6327)/1087/1431/1703/2068/2908/3151/4832(4828)/4166/9860
#   zipsubregion_nercbatrns %>%
#   full_join(zipsubregion_plntutilid, by = "subregion") %>%
#   glimpse() %>%
#   filter(n.x != n.y) %>%
#   print(n = 27)

### Power Profiler Transmission ID Crosswalk assignment --------

# update subregion bsaed on transmission ID subregion crosswal
#39:
zip_subregion_transmission_xwalk <-
  zip_subregion_nerc_ba_transmission %>%
  left_join(transmission_id_subregion_crosswalk, by = c("eiaid" = "transmission_id")) %>%
  glimpse() %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "power profiler transmission ID crosswalk", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct() %>%
  glimpse()

# zipsubregion_transmxwalk <- 
#   zip_subregion_transmission_xwalk %>%
#   count(subregion) %>%
#   print(n = 27)
# 
# zipsubregioncomp <- #112/85/1440/3008/2096/1931/68/69/730/4148/2737/3501/395/213/2002/3895/1305/6323(6327)/1087/1431/1703/2068/2908/3151/4832(4828)/4166/9860
#   zipsubregion_transmxwalk %>%
#   full_join(zipsubregion_nercbatrns, by = "subregion") %>%
#   filter(n.x != n.y) %>%
#   print(n = 27)

### Crosswalk for Missing Utility ID assignment --------

# update subregions for utility IDs not in eGRID
zip_subregion_missing_ids <-
  zip_subregion_transmission_xwalk %>%
  left_join(xwalk_for_missing_utility_id %>%
              select(utility_number, subregion), 
            by = c("eiaid" = "utility_number")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "crosswalk for missing utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct() %>%
  glimpse()

zipsubregion_missing <-
  zip_subregion_missing_ids %>%
  count(subregion) %>%
  print(n = 27)
# 
# zipsubregioncomp <- #112/85/1536/3008/2096/1931/68/69/883/5147/2737/3663/395/213/2060/4465/1415/6358(6362)/1388/1508/2480/2761/3393/3307/5421(5417)/4394/4191
#   zipsubregion_missing %>%
#   full_join(zipsubregion_transmxwalk, by = "subregion") %>%
#   filter(n.x != n.y) %>%
#   print(n = 27)
  
# Subregion corrections -----

### Update missing subregion for zip codes from old power profiler -----
#41:
zip_subregion_missing_subregions <-
  zip_subregion_missing_ids %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              select(zip, eiaid, subregion), 
            by = c("zip", "eiaid")) %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct() %>%
  glimpse()

# zipsubregion_missingsr <-
#   zip_subregion_missing_subregions %>%
#   count(subregion) %>%
#   print(n = 28)
# 
# zipsubregioncomp <- #126/202(181)/1629/3040/2830/1940/68/70/888/5351(5350)/2927(2920)/3835(3831)/400/217/2063/176(0)/4487/1456(1454)/6534(6538)/1450/1518/2502/2782/3524/3397/5438(5434)/4433/1801(2012)
#   zipsubregion_missingsr %>%
#   full_join(zipsubregion_missing, by = "subregion") %>%
#   filter(n.x != n.y) %>%
#   print(n = 28)
  # addition of 176 PRMS
  # extra assignments in 4 more subregions that 
  # remain blank in access data - not sure why

### State subregion one to one match -------

#15/16: 17(16)
states_and_single_subregions <-
  plant_file %>%
  select(state = plant_state, subregion = egrid_subregion) %>%
  filter(!is.na(subregion)) %>% distinct() %>%
  group_by(state) %>%
  filter(n_distinct(subregion) == 1) %>%
  glimpse()

#### Update values in subregion data -----

#16:
zip_subregion_state <-
  zip_subregion_missing_subregions %>%
  left_join(states_and_single_subregions, by = "state") %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "based on state", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

# zipsubregion_state <-
#   zip_subregion_state %>%
#   count(subregion) %>%
#   print(n = 29)
# 
# zipsubregioncomp <- #126/202(181)/1629/3040/2830/1940/68/70/888/6269/2927/3835/400/217/2063/176/4497/1456(1454)/7367(7371)/1450/1518/2502/2782/3524/3397/5438(5434)/4433/40(63)
#   zipsubregion_state %>%
#   full_join(zipsubregion_missingsr, by = "subregion") %>%
#   filter(n.x != n.y) %>%
#   print(n = 27)
#     # stil have the 4 differences
#     # no additional differences from last step

### Make manual changes to table --------

#42:
zip_subregion_manual_changes <-
  zip_subregion_state %>%
  left_join(utility_subregion_manual_table, by = c("state", "eiaid" = "utility_id")) %>%
  mutate(method = if_else(!is.na(subregion.y), "manual override", method),
         subregion = if_else(!is.na(subregion.y), subregion.y, subregion.x)) %>%
  select(-contains(".")) %>%
  glimpse()

# zipsubregion_manual <-
#   zip_subregion_manual_changes %>%
#   count(subregion) %>%
#   print(n = 29)
# 
# zipsubregioncomp <- #126/202(181)/1592/3036/2829/1940/68/70/888/6312/2931/3835/400/217/2063/176/4497/1456(1454)/7368(7372)/1450/1475/2570/2786/3493/3400/5438(5434)/4430/36(59)
#   zipsubregion_manual %>%
#   full_join(zipsubregion_state, by = "subregion") %>%
#   filter(n.x != n.y) %>%
#   print(n = 28)

## DIFFERENCES
# AKMS 202(181)
# RFCM 1456(1454)
# RFCW 7368(7372)
# SRTV 5438(5434)
# NA 36(59)

# Update Predominant Utilities -----

### Zips with one utility assignment ------
#46/47: 25117(24906)
zip_single_utility <-
  zip_subregion_manual_changes %>%
  select(zip, eiaid) %>% distinct() %>%
  group_by(zip) %>%
  filter(n_distinct(eiaid) == 1) %>%
  ungroup() %>%
  mutate(predominant_utility = as.factor(1)) %>%
  glimpse()

#48:
zip_subregion_single_utility <-
  zip_subregion_manual_changes %>%
  left_join(zip_single_utility, by = c("zip", "eiaid")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()

# count <- #39602/25482
#   zip_subregion_single_utility %>%
#   count(predominant_utility) %>%
#   print()

### Zips with no utility assignment ------

#### Update with assignment from old profiler -----

# select zipcodes and predominant utility from old profiler for zips with no predominant utility
#49/50: 39146(39092)
no_predominant_utility <-
  zip_subregion_single_utility %>%
  group_by(zip) %>%
  filter(!any(predominant_utility != 0)) %>%
  select(zip) %>% distinct() %>%
  ungroup() %>%
  inner_join(power_profiler_old, by = "zip") %>%
  select(zip, trim_util_code, predominant_utility) %>%
  glimpse()

#50:
# update predominant utilties in zipcode data
zip_subregion_utility_from_old_pp <-
  zip_subregion_single_utility %>%
  left_join(no_predominant_utility, by = c("zip", "eiaid" = "trim_util_code")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()

# count <- #23531/41553
#   zip_subregion_utility_from_old_pp %>%
#   count(predominant_utility) %>%
#   print()

#### Update as first EIAID from old power profiler ------
#49: 9
# check again for zipcodes without any predominant utility
no_predominant_utility_2 <-
  zip_subregion_utility_from_old_pp %>%
  group_by(zip) %>%
  filter(!any(predominant_utility != 0)) %>%
  select(zip) %>% distinct() %>%
  ungroup() %>%
  arrange(as.numeric(zip)) %>%
  print()

#51: 9
# identify 'first' EIAID in zipcode
zips_to_update_first_eiaid <-
  zip_subregion_utility_from_old_pp %>%
  arrange(as.numeric(zip), as.numeric(eiaid), predominant_utility) %>%
  inner_join(no_predominant_utility_2, by = "zip") %>%
  group_by(zip) %>%
  summarize(first_of_eia = first(eiaid)) %>%
  mutate(predominant_utility = as.factor("1")) %>%
  print()

# Update zipcode dataset
#52:
zip_subregion_first_eiaid <-
  zip_subregion_utility_from_old_pp %>%
  left_join(zips_to_update_first_eiaid, by = c("zip", "eiaid" = "first_of_eia")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()

# count <- #23522/41562
#   zip_subregion_first_eiaid %>%
#   count(predominant_utility) %>%
#   print()

### Override from table ------

#### Zipcode matching -----

#58:
zip_subregion_predominant_override_zip <-
  zip_subregion_first_eiaid %>%
  left_join(predominant_utility_override, by = "zip") %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."), -first_ofeiaid) %>%
  glimpse()

# count <- #23725/41359
#   zip_subregion_predominant_override_zip %>%
#   count(predominant_utility) %>%
#   print()

#### Zipcode and first eiaid matching -----
#59:
zip_subregion_predominant_override_eiaid <-
  zip_subregion_predominant_override_zip %>%
  left_join(predominant_utility_override %>%
              mutate(predominant_utility = as.factor("1")), 
            by = c("zip", "eiaid" = "first_ofeiaid")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()

compare <-
  zip_subregion_predominant_override_eiaid %>%
  inner_join(pred_util, by = c("zip", "eiaid")) %>%
  #glimpse()
  filter(predominant_utility != predominant_utility.y) %>%
  glimpse()

# count <- #41562/23522
#   zip_subregion_predominant_override_eiaid %>%
#   count(predominant_utility) %>%
#   print()

# Update Subregion assignments -----

### Zips with one subregion assignment -----

#53: 41562
subregions_primary <-
  zip_subregion_predominant_override_eiaid %>%
  filter(predominant_utility == 1) %>%
  select(zip, state, subregion) %>% distinct() %>%
  mutate(secondary = as.factor("0")) %>%
  glimpse()

### Zips with multiple subregion assignments -----
#55/56: 2984(2963)
zips_with_more_than_one_subregion <-
  zip_subregion_predominant_override_eiaid %>%
  group_by(zip) %>%
  filter(n_distinct(subregion) > 1) %>%
  ungroup() %>%
  select(zip) %>% distinct() %>%
  mutate(secondary = as.factor("1")) %>%
  glimpse()

# update primary subregion table
#57: 41562
subregions_secondary <-
  subregions_primary %>%
  left_join(zips_with_more_than_one_subregion, by = "zip") %>%
  mutate(secondary = if_else(!is.na(secondary.y), secondary.y, secondary.x)) %>%
  select(-contains(".")) %>%
  glimpse()

count <- #38578(38599)/2984(2963)
  subregions_secondary %>%
  count(secondary) %>%
  print()
  
### Override from table -----

#60: 41562
subregions_override <-
  subregions_secondary %>%
  left_join(primary_subregion_override, by = "zip") %>%
  mutate(subregion = if_else(!is.na(change), change, subregion)) %>%
  select(-current, -change) %>% distinct() %>%
  glimpse()

count <- #92/187(166)/1195/2561/2056/1391/68/70/483/3670/2321/2548/396/198/1582/176/3407/998(996)/5195(5196)/865/1111/1379/1564/1820/1860/1942(1941)/2406/21(44)
  subregions_override %>%
  count(subregion) %>%
  print(n = 28)

# Prepare data for final website ------

### Update predominant utility using first utility site name -----

#65: 65084
zip_subregion_for_website <-
  zip_subregion_predominant_override_eiaid %>%
  mutate(predominant_utility = as.factor(0)) %>%
  select(zip, state, utility_name, trim_util_code = eiaid, subregion,  predominant_utility) %>%
  arrange(zip, tolower(utility_name)) %>%
  glimpse()

#66: 41562 (41351 without NAs)
first_of_utility_name <-
  zip_subregion_for_website %>%
  group_by(zip, state) %>%
  summarize(first_of_utility_name = first(utility_name)) %>%
  mutate(predominant_utility = as.factor(1)) %>%
  ungroup() %>%
  filter(!is.na(first_of_utility_name)) %>%
  glimpse()

# update predominant utility
#67:
zip_subregion_updated_utility_name <-
  zip_subregion_for_website %>%
  left_join(first_of_utility_name, by = c("utility_name" = "first_of_utility_name", "state", "zip")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()

count <- # 41351/23522
  zip_subregion_updated_utility_name %>%
  count(predominant_utility) %>%
  print()

### Update predominant utility using old power plant assignment -----

#67b:
zip_subregion_updated_old_zipcode <-
  zip_subregion_updated_utility_name %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              select(zip, eiaid, predominant_utility) %>%
              filter(is.na(eiaid)),
            by = "zip") %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."), -eiaid, eiaid = trim_util_code) %>%
  glimpse()

count <- #23522/41562
  zip_subregion_updated_old_zipcode %>%
  count(predominant_utility) %>%
  print()

# Update missing subregions with those from old profiler -----

#71/72/73/74/75: 36(59)
subregions_to_update_from_old_pp <-
  zip_subregion_updated_old_zipcode %>%
  filter(is.na(subregion)) %>%
  select(zip, subregion) %>% distinct() %>%
  inner_join(power_profiler_old, by = "zip") %>%
  group_by(zip) %>%
  filter(n_distinct(subrgn) == 1) %>% distinct() %>%
  select(zip, subrgn) %>% distinct() %>%
  glimpse()

#76: update zipsubregion
zip_subregion_final <-
  zip_subregion_updated_old_zipcode %>%
  left_join(subregions_to_update_from_old_pp %>%
              rename(subregion = subrgn), 
            by = "zip") %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

# count <-
#   zip_subregion_final %>%
#   count(subregion) %>%
#   print(n=29)
