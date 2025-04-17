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

# DATA(30) - NERC NAMES FOR MISSING UTILITY ID - COUNT 1 (how was this calculated or is this raw data?)
# load in example data
# nerc_names_for_missing_utility_id <-
#   read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/nerc_names_for_missing_utility_id.csv"),
#            col_names = c("eiaid","utility_name","nerc_region"),
#            col_types = "ccc") %>%
#   janitor::clean_names() %>%
#   glimpse()

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
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/transmissionID_subregion_crosswalk.csv"),
           col_types = "cc") %>%
  janitor::clean_names() %>%
  glimpse()

# Crosswalk for missing utility ids
xwalk_for_missing_utility_id <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/xwalk_for_missing_utility_ID.csv"),
           col_types = "cccc") %>%
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

# create zipcode data from plant utility id
#17/18/19/20: 25460
group_zip_utility_subregion_utilityid <-
  plant_file %>%
  select(egrid_subregion, utility_id) %>% distinct() %>%
  inner_join(utility_zipcodes, by = join_by("utility_id" == "eiaid")) %>%
  select(zip, subregion = egrid_subregion, eiaid = utility_id) %>% distinct() %>%
  group_by(zip, eiaid) %>%
  filter(n_distinct(subregion) == 1) %>%
  ungroup() %>%
  glimpse()

#21/22: 25460
plant_utilityid_subregions <-
  group_zip_utility_subregion_utilityid %>%
  # keep values with zipcodes within the utility zipcodes
  inner_join(utility_zipcodes %>%
               select(zip, eiaid) %>% distinct(), 
             by = c("zip", "eiaid")) %>%
  select(zip, eiaid, subregion) %>%
  arrange(zip) %>%
  glimpse()

#### Update subregions based on plant utility ID ---------
#23: 65084
zip_subregion_plant_utilityid <-
  zip_subregion_plant_transmission %>%
  left_join(plant_utilityid_subregions, by = c("zip", "eiaid")) %>%
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

# zipsubregion_missing <-
#   zip_subregion_missing_ids %>%
#   count(subregion) %>%
#   print(n = 27)
# 
# zipsubregioncomp <- #112/85/1536/3008/2096/1931/68/69/883/5147/2737/3663/395/213/2060/4465/1415/6358(6362)/1388/1508/2480/2761/3393/3307/5421(5417)/4394/4191
#   zipsubregion_missing %>%
#   full_join(zipsubregion_transmxwalk, by = "subregion") %>%
#   filter(n.x != n.y) %>%
#   print(n = 27)
  
###%%%%%%#####

# Corrections -----
### Update missing subregion for zip codes from old power profiler -----
#41:
zip_subregion_missing_subregions <-
  zip_subregion_missing_ids %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              select(zip, eiaid, subregion), 
            by = c("zip", "eiaid")) %>%
  #mutate(subregion = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), subregion.y, subregion.x)) %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>% distinct() %>%
  glimpse()

zipsubregion_missingsr <-
  zip_subregion_missing_subregions %>%
  count(subregion) %>%
  print(n = 27)

zipsubregioncomp <- #126/202(181)/1629/3040/2830/1940/68/70/888/5351(5350)/2927(2920)/3835(3831)/400/217/2063/4487/1456(1454)/6534(6538)/1450/1518/2502/2782/3524/3397/5438(5434)/4433/1801(2012)
  zipsubregion_missingsr %>%
  full_join(zipsubregion_missing, by = "subregion") %>%
  filter(n.x != n.y) %>%
  print(n = 28)

# Update state subregion one to one match -------

#13: 119 (120)
states_and_subregions_grouped <-
  plant_file %>%
  select(state = plant_state, subregion = egrid_subregion) %>%
  filter(!is.na(subregion)) %>%  distinct() %>%
  glimpse()

#14: 52 (52)
count_of_subregions_by_state <-
  states_and_subregions_grouped %>%
  group_by(state) %>%
  summarize(count_subregion = n()) %>%
  glimpse()

#15: 17(16)
states_subregion_one_to_one <-
  states_and_subregions_grouped %>%
  inner_join(count_of_subregions_by_state, by = "state") %>%
  filter(count_subregion == 1) %>%
  select(-count_subregion) %>%
  # select(eiaid, utility_name = utility_name.x, nerc_region) %>%
  glimpse()

### Update values in subregion data -----

#16:
zip_subregion_11 <-
  zip_subregion_10 %>%
  left_join(states_subregion_one_to_one, by = "state") %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "based on state", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

zipsubregion11 <-#126/202(181)/1629/3040/2830/1940/68/70/888/6269/2927/3835/400/217/2063/176/4497/1456(1454)/7367(7371)/1450/1518/2502/2782/3524/3397/5438(5434)/4433/40(63)
  zip_subregion_11 %>%
  count(subregion, name = "n.11") %>%
  print(n = 29)

zipsubregioncomp <-
  zipsubregion11 %>%
  full_join(zipsubregion10, by = "subregion") %>%
  filter(n.10 != n.11) %>%
  print(n = 27)

# Make manual changes to table --------

#42:
utility_subregion_manual_table <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/utility_subregion_manual_table.csv"),
           col_types = "ccc") %>%
  janitor::clean_names() %>%
  glimpse()

zip_subregion_12 <-
  zip_subregion_11 %>%
  left_join(utility_subregion_manual_table, by = c("state", "eiaid" = "utility_id")) %>%
  mutate(method = if_else(!is.na(subregion.y), "manual override", method),
         subregion = if_else(!is.na(subregion.y), subregion.y, subregion.x)) %>%
  select(-contains(".")) %>%
  glimpse()

zipsubregion12 <-#126/202(181)/1592/3036/2829/1940/68/70/888/6312/2931/3835/400/217/2063/176/4497/1456(1454)/7368(7372)/1450/1475/2570/2786/3493/3400/5438(5434)/4430/36(59)
  zip_subregion_12 %>%
  count(subregion, name = "n.12") %>%
  print(n = 29)

zipsubregioncomp <-
  zipsubregion12 %>%
  full_join(zipsubregion11, by = "subregion") %>%
  filter(n.11 != n.12) %>%
  print(n = 28)

# Find zipcodes with one utility ------
#46:
grouped_by_zip <-
  zip_subregion_12 %>%
  select(zip, eiaid) %>% distinct() %>%
  glimpse()

#47: 25117 (24906)
count_grouped_by_zip <-
  grouped_by_zip %>%
  group_by(zip) %>%
  summarize(eiaid_count = n_distinct(eiaid)) %>%
  filter(eiaid_count == 1) %>%
  glimpse()

# Update predominant utility ------
#48:
zip_subregion_13 <-
  zip_subregion_12 %>%
  left_join(count_grouped_by_zip, by = "zip") %>%
  mutate(predominant_utility = if_else(!is.na(eiaid_count), "1", predominant_utility)) %>%
  select(-eiaid_count) %>%
  glimpse()

count <- #39602/25482
  zip_subregion_13 %>%
  count(predominant_utility) %>%
  print()

# Zips with no predominant utility -----

#49: 16445(16080)
zips_with_no_predominant_utility <-
  zip_subregion_13 %>%
  filter(predominant_utility == "0") %>%
  select(zip) %>% distinct() %>%
  glimpse()

# Update with predom util from old PP
#50:

# select zipcodes in power profiler that match those in predominant utility
# 39971(39092)
predominant_utility_from_old_profiler <-
  zips_with_no_predominant_utility %>%
  inner_join(power_profiler_old, by = "zip") %>%
  select(zip, trim_util_code, predominant_utility) %>%
  glimpse()

zip_subregion_14 <-
  zip_subregion_13 %>%
  left_join(predominant_utility_from_old_profiler, by = c("zip", "eiaid" = "trim_util_code")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()

count <- #23531/41553
  zip_subregion_14 %>%
  count(predominant_utility) %>%
  print()

# Zips with no predominant utility ------
#49: 16445
# so far, I don't believe this step repeated is necessary
zips_with_no_predominant_utility_2 <-
  zip_subregion_14 %>%
  filter(predominant_utility == "0") %>%
  select(zip) %>% distinct() %>%
  glimpse()

# Zips to update with predom util from old pp
#51: 16445(16080)
updates_for_predominant_utility <-
  zips_with_no_predominant_utility_2 %>%
  inner_join(zip_subregion_14, by = "zip") %>%
  arrange(zip, eiaid, predominant_utility) %>%
  group_by(zip) %>%
  summarize(first_of_eia = first(eiaid)) %>%
  mutate(predominant_utility = "1") %>%
  glimpse()

# Update predom utility
#52:
zip_subregion_15 <-
  zip_subregion_14 %>%
  left_join(updates_for_predominant_utility, by = c("zip", "eiaid" = "first_of_eia")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()

count <- #7086(17677)/57998(47407)
  zip_subregion_15 %>%
  count(predominant_utility) %>%
  print()

access_comparison <-
  read_csv("data/2a_power_profiler/inputs/2023/updates_for_utility_access.csv") %>%
  janitor::clean_names() %>%
  glimpse()

comparison <-
  updates_for_predominant_utility %>%
  full_join(access_comparison, by = "zip") %>%
  filter(first_of_eia != first_ofeiaid) %>%
  glimpse()

## need to decide how these things are ordered when selecting first of eiaid

# Predom utility override p1
predom_util_override_2 <-
  read_csv("data/2a_power_profiler/inputs/2023/predom_util_override_2.csv",
           col_types = "ccccc") %>%
  janitor::clean_names() %>%
  select(zip, first_ofeiaid) %>%
  mutate(predominant_utility = "0") %>%
  glimpse()

#58:
zip_subregion_16 <-
  zip_subregion_15 %>%
  left_join(predom_util_override_2, by = "zip") %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains("."), -first_ofeiaid) %>%
  glimpse()

count <- #14263(17951)/50821(47133)
  zip_subregion_16 %>%
  count(predominant_utility) %>%
  print()

# predom utility override p2
#59:
zip_subregion_17 <-
  zip_subregion_16 %>%
  left_join(predom_util_override_2 %>%
              mutate(predominant_utility = "1"), 
            by = c("zip", "eiaid" = "first_ofeiaid")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()

count <- #14060(17748)/51024(47336)
  zip_subregion_17 %>%
  count(predominant_utility) %>%
  print()

# Update subregions -----
# subregion of predom utility
#53: 44619(41562)
primary_subregion <-
  zip_subregion_17 %>%
  select(zip, state, subregion) %>% distinct() %>%
  mutate(secondary = "0") %>%
  glimpse()

# Group by zip and subregion, identify zips with more than subregion
#55/56: 2984(2963)
zips_with_more_than_one_subregion <-
  zip_subregion_17 %>%
  group_by(zip) %>%
  summarize(subrgn_count = n_distinct(subregion)) %>%
  filter(subrgn_count > 1) %>%
  mutate(secondary = "1") %>%
  glimpse()

# update primary subregion table
#57: 44619(41562)
primary_subregion_2 <-
  primary_subregion %>%
  left_join(zips_with_more_than_one_subregion, by = "zip") %>%
  mutate(secondary = if_else(!is.na(secondary.y), secondary.y, secondary.x)) %>%
  select(-contains(".")) %>%
  glimpse()

count <- #38578(38599)/6041(2963)
  primary_subregion_2 %>%
  count(secondary) %>%
  print()
  
# primary subregion override

primary_subregion_override <-
  read_csv("data/2a_power_profiler/inputs/2023/primary_subregion_override.csv",
           col_types = "ccc") %>%
  janitor::clean_names() %>%
  glimpse()

#60:
primary_subregion_3 <-
  primary_subregion_2 %>%
  left_join(primary_subregion_override, by = "zip") %>%
  mutate(subregion = if_else(!is.na(change), change, subregion)) %>%
  glimpse()

count <-
  primary_subregion_2 %>%
  count(subregion) %>%
  print(n = 28)

# final power profiler file for website
#65:
zip_subregion_for_website <-
  zip_subregion_17 %>%
  mutate(predominant_utility = "0") %>%
  select(zip, state, utility_name, trim_util_code = eiaid, subregion,  predominant_utility) %>%
  arrange(zip, utility_name) %>%
  glimpse()

# first of utility
#66:
first_of_utility_name <-
  zip_subregion_for_website %>%
  group_by(zip, state) %>%
  summarize(first_of_utility_name = first(utility_name)) %>%
  mutate(predominant_utility = "1") %>%
  glimpse()

# update predominant utility
#67:
zip_subregion_for_website_2 <-
  zip_subregion_for_website %>%
  left_join(first_of_utility_name, by = c("utility_name" = "first_of_utility_name", "state", "zip")) %>%
  mutate(predominant_utility = if_else(!is.na(predominant_utility.y), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()
  
# update old zip code predominant utility
#67b:
zip_subregion_for_website_3 <-
  zip_subregion_for_website_2 %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              select(zip, predominant_utility),
            by = "zip") %>%
  mutate(predominant_utility = if_else(is.na(trim_util_code), predominant_utility.y, predominant_utility.x)) %>%
  select(-contains(".")) %>%
  glimpse()

# 
#71/72/73/74:
compare_to_old_pp_single_subrgn <-
  zip_subregion_17 %>%
  filter(is.na(subregion)) %>%
  select(zip, subregion) %>% distinct() %>%
  inner_join(power_profiler_old, by = "zip") %>%
  group_by(zip) %>%
  summarize(count_subrgn = n_distinct(subrgn)) %>%
  filter(count_subrgn == 1) %>%
  # distinct() %>%
  glimpse()

#75: subregions to update
subregions_to_update_from_old_pp <-
  compare_to_old_pp_single_subrgn %>%
  left_join(power_profiler_old, by = "zip") %>%
  select(zip, subrgn) %>% distinct() %>%
  glimpse()

#76: update zipsubregion
zip_subregion_18 <-
  zip_subregion_17 %>%
  left_join(subregions_to_update_from_old_pp %>%
              rename(subregion = subrgn), 
            by = "zip") %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()
