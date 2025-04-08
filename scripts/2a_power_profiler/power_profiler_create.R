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

# load in eGRID plant data
plant_file <- readRDS(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/plant_file.RDS")) %>%
  glimpse()

# load in previous power profiler data which had zip, utility code, predominant utility, etc. 
# previous power profiler data (FROM ACCESS) (65187 SAME)
power_profiler_old <- 
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/power_profiler_old.csv"),
           col_types = "cccccc") %>%
  janitor::clean_names() %>%
  glimpse()

# load in all utility zip codes
# utility zip codes (FROM MARISSA) (80142 SAME) (39133 unique zips)
utility_zipcodes <- 
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/utility_zipcodes.csv"),
           col_types = "ccccccddd") %>%
  janitor::clean_names() %>%
  # group_by(zip) %>%
  # count() %>%
  glimpse()

# eia-861 utility data - with count 
eia_861_utility_data_with_count <- # (FROM ACCESS)
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/eia_861_utility_with_count.csv"),
           col_types = "ccccccccccccccccccccccdcccccccccc") %>%
  janitor::clean_names() %>%
  # rename_with(~ paste(., "_eia_count", sep = "")) %>%
  glimpse()

# DATA - NERC CROSSWALK FOR FEW SUBREGIONS (this only has three nerc and subregion combos - what is this?)
nerc_region_crosswalk <- # (FROM ACCESS)
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/nerc_region_crosswalk.csv"),
           col_names = c("nerc","subregion")) %>%
  janitor::clean_names() %>%
  glimpse()

eia_861_sales_ult_cust <- # (FROM ACCESS)
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/eia_861_sales_ult_cust.csv"),
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

xwalk_balancing_authority <-
  read_csv("data/1_production_model/static_tables/xwalk_balancing_authority.csv") %>%
  janitor::clean_names() %>%
  select(subregion = subrgn, ba_code = balancing_authority_code) %>%
  glimpse()

# 1204 (access has 1160)
xwalk_subregion_transmission <-
  read_csv("data/1_production_model/static_tables/xwalk_subregion_transmission.csv",
           col_types = "cccc") %>%
  janitor::clean_names() %>%
  rename(ba_code = balancing_authority_code, transmission = transmission_or_distribution_system_owner_id, subregion = subrgn) %>%
  glimpse()

# Old Power Profiler  -------
### Create zipcode dataset -----------

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
  mutate(subregion = NA_character_, predominant_utility = "0", old_zip_code = "no", method = NA_character_) %>%
  group_by_all() %>%
  distinct() %>%
  ungroup() %>%
  glimpse()

### Add zipcodes from old power profiler ------

# add zipcodes from previous power profiler to add (append to bottom)
#03 : (65084 SAME - 2889 old power profiler SAME)
# right now I have the subregion values from old power profiler into subregion data
# access has them filled as NAs
zip_subregion_2 <-
  zip_subregion_1 %>%
  bind_rows(zip_codes_from_old_power_profiler_to_add %>% mutate(old_zip_code = "yes", method = "old power profiler", subregion = NA_character_)) %>%
  glimpse()
  
zipcount <- # old power profiler 2889(2889), na 62195(62195)
  zip_subregion_2 %>%
  count(method) %>%
  print()

# NERC Region --------
### Create nerc names for missing utility id dataset -----

#28: 1794(1207)
nerc_region_for_missing_utility_id <-
  zip_subregion_2 %>%
  left_join(eia_861_utility_data_with_count, by = c("eiaid" = "utility_number")) %>%
  select(eiaid, utility_name = utility_name.x, nerc_region) %>%
  distinct() %>%
  arrange(as.numeric(eiaid)) %>%
  glimpse()

#29: 1794 (1207)
count_of_one_nerc_region <-
  nerc_region_for_missing_utility_id %>%
  group_by(eiaid, utility_name) %>%
  summarize(subrgn_count = n_distinct(nerc_region)) %>%
  filter(subrgn_count == 1) %>%
  arrange(as.numeric(eiaid)) %>%
  glimpse()

#30: 1806(1213)
nerc_names_for_missing_utility_id <-
  nerc_region_for_missing_utility_id %>%
  inner_join(count_of_one_nerc_region, by = "eiaid") %>%
  select(eiaid, utility_name = utility_name.x, nerc_region) %>%
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

### Update subregions based on NERC crosswalk -----

#32 -65084(65084), nerc 2861(2861), old power profiler 2888(2888)
# join in utility ids with proper nerc region
# update subregion and specify nerc region
#updates 2966 rows
zip_subregion_3 <-
  zip_subregion_2 %>%
  left_join(zip_utility_update_nerc, by = c("eiaid", "utility_name")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -nerc_region) %>%
  glimpse()

zipcount <-
  zip_subregion_3 %>%
  count(method) %>%
  print()

zipsubregion <-
  zip_subregion_3 %>%
  count(subregion) %>%
  print()

# Balancing Authority --------
### Create ba codes for missing utility id dataset -----

#33: 1219(1219)
ba_code_for_missing_utility_id <-
  zip_subregion_3 %>%
  inner_join(eia_861_sales_ult_cust, by = c("eiaid" = "utility_number")) %>%
  select(eiaid, utility_name = utility_name.x, ba_code) %>%
  distinct() %>%
  glimpse()

#34: 1194(1194)
count_of_one_ba_code <-
  ba_code_for_missing_utility_id %>%
  group_by(eiaid, utility_name) %>%
  summarize(ba_count = n_distinct(ba_code)) %>%
  filter(ba_count == 1) %>%
  glimpse()

#35: 1200(1200)
ba_names_for_missing_utility_id <-
  ba_code_for_missing_utility_id %>%
  inner_join(count_of_one_ba_code, by = "eiaid") %>%
  select(eiaid, utility_name = utility_name.x, ba_code) %>%
  glimpse()

#36: (1188 SAME) - 1182 as distinct
# match ba codes to subregions
zip_utility_update_ba <- 
  ba_names_for_missing_utility_id %>%
  inner_join(xwalk_balancing_authority, by = "ba_code") %>%
  distinct() %>%
  arrange(as.numeric(eiaid)) %>%
  glimpse() 

### Update subregion based on BA crosswalk -----

#37: 65084(65084), ba 22931(57963), nerc 2861(2861), old power profiler 2796(2782), na 36496(1478)
# included the utility name to remove duplicates
# leaves 65084 records
# all subregions are the same
zip_subregion_4 <-
  zip_subregion_3 %>%
  left_join(zip_utility_update_ba, by = c("eiaid", "utility_name")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "balancing authority", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -ba_code) %>%
  glimpse()
  
zipcount <-
  zip_subregion_4 %>%
  count(method) %>%
  print()

zipsubregion <-
  zip_subregion_4 %>%
  count(subregion) %>%
  print()

# Plant Transmission ID ------------
### Create zipcode data from plant transmission id ------------

#5-7: 62258 (access has 62262)
group_zip_utility_subregion <-
  plant_file %>%
  select(egrid_subregion, system_owner_id) %>%
  distinct() %>%
  inner_join(utility_zipcodes, by = join_by("system_owner_id" == "eiaid")) %>%
  select(zip, subregion = egrid_subregion, eiaid = system_owner_id) %>%
  distinct() %>%
  glimpse()

#8: 35005 (access has 35001)
count_of_subregion_per_utilityid <-
  group_zip_utility_subregion %>%
  group_by(zip, eiaid) %>%
  summarize(subrgn_count = n_distinct(subregion)) %>%
  filter(subrgn_count == 1) %>%
  ungroup() %>%
  glimpse()

#9: 62195 (SAME AS ACCESS)
group_utility_codes_byzip_utilityid <-
  utility_zipcodes %>%
  select(zip, eiaid) %>%
  distinct() %>%
  glimpse()

#10-11: 35005 (access has 35001)
zipsubregion_one_to_one <-
  group_zip_utility_subregion %>%
  # keep values with one subregion type
  inner_join(count_of_subregion_per_utilityid, by = c("zip", "eiaid")) %>%
  # keep values with zipcodes within the utility zipcodes
  inner_join(group_utility_codes_byzip_utilityid, by = c("zip", "eiaid")) %>%
  select(zip, eiaid, subregion) %>%
  arrange(zip) %>%
  glimpse()

### Update subregions based on transmission ID plant file -----------
  
#12: #srtv (4828 in access, 4832 in r)
zip_subregion_5 <-
  zip_subregion_4 %>%
  left_join(zipsubregion_one_to_one, by = c("zip", "eiaid")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), "plant file - transmission ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

zipcount <-
  zip_subregion_5 %>%
  count(method) %>%
  print()

zipsubregion5 <-
  zip_subregion_5 %>%
  count(subregion) %>%
  print(n = 26)

# Plant Utility ID --------
### Create zipcode data from plant utility id -------

#17-19:
group_zip_utility_subregion_2 <-
  plant_file %>%
  select(egrid_subregion, utility_id) %>%
  distinct() %>%
  inner_join(utility_zipcodes, by = join_by("utility_id" == "eiaid")) %>%
  select(zip, subregion = egrid_subregion, eiaid = utility_id) %>%
  distinct() %>%
  glimpse()

#20: 25460 (same as access)
count_of_subregion_per_utilityid_2 <-
  group_zip_utility_subregion_2 %>%
  group_by(zip, eiaid) %>%
  summarize(subrgn_count = n_distinct(subregion)) %>%
  filter(subrgn_count == 1) %>%
  glimpse()

#21-22: 25460 (same as access)
zipsubregion_one_to_one_utility_test <-
  group_zip_utility_subregion_2 %>%
  # keep values with one subregion type
  inner_join(count_of_subregion_per_utilityid_2, by = c("zip", "eiaid")) %>%
  # keep values with zipcodes within the utility zipcodes
  inner_join(group_utility_codes_byzip_utilityid, by = c("zip", "eiaid")) %>%
  select(zip, eiaid, subregion) %>%
  arrange(zip) %>%
  glimpse()

### Update subregions based on utility ID plant file ---------
#23: #srtv (4828 in access, 4832 in r) where are the other 2?
zip_subregion_6 <-
  zip_subregion_5 %>%
  left_join(zipsubregion_one_to_one_utility_test, by = c("zip", "eiaid")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y) | subregion.x == "" & !is.na(subregion.y), "plant file - utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  glimpse()

zipcount <-
  zip_subregion_6 %>%
  count(method) %>%
  print()

zipsubregion6 <-
  zip_subregion_6 %>%
  count(subregion, name = "n.6") %>%
  print(n = 27)

zipsubregioncomp <-
  zipsubregion6 %>%
  full_join(zipsubregion5, by = "subregion") %>%
  glimpse() %>%
  filter(n.6 != n) %>%
  print()

################## HAVE CLEANED UP TO HERE #####################

# NERC Region / BA / Transmission ID -----
#38: # ba 22931 (29702), nerc region 2861 (2861), nerc/ba/. 4451 (4455), old profiler 2793 (2782), plant file transm 22545 (22541), plant file utility 2616 (2616), NA 6887 (216)
zip_subregion_7 <-
  zip_subregion_6 %>%
  left_join(eia_861_sales_ult_cust %>%
              select(utility_number,
                     ba_code),
            by = c("eiaid" = "utility_number")) %>%
  left_join(eia_861_utility_data_with_count %>%
              select(utility_number,
                     nerc_region),
            by = c("eiaid" = "utility_number")) %>%
  left_join(xwalk_subregion_transmission,
            by = c("nerc_region", "ba_code", "eiaid" = "transmission")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "nerc region/ba/transmission ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains("."), -ba_code, -nerc_region) %>%
  distinct() %>%
  glimpse()
  
zipcount <- 
  zip_subregion_7 %>%
  count(method) %>%
  print()

zipsubregion7 <-#112, 85, 1440, 3008, 2096, 1931, 68, 69, 730/4148/2737/3501/395/213/2002/3895/1305/6323(6327)/1087/1431/1703/2068/2908/3151/4832(4828)/4166/9860
  zip_subregion_7 %>%
  count(subregion, name = "n.7") %>%
  print(n = 27)

zipsubregioncomp <-
  zipsubregion7 %>%
  full_join(zipsubregion6, by = "subregion") %>%
  glimpse() %>%
  filter(n.6 != n.7) %>%
  print(n = 27)

# Power Profiler Transmission ID Crosswalk --------

#39:
transmission_id_subregion_crosswalk <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/transmissionID_subregion_crosswalk.csv"),
           col_types = "cc") %>%
  janitor::clean_names() %>%
    glimpse()

# no updates here - not sure what this data is or where it came from
zip_subregion_8 <-
  zip_subregion_7 %>%
  left_join(transmission_id_subregion_crosswalk, by = c("eiaid" = "transmission_id")) %>%
  glimpse() %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "power profiler transmission ID crosswalk", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  distinct() %>%
  glimpse()

zipsubregion8 <-#112, 85, 1440, 3008, 2096, 1931, 68, 69, 730/4148/2737/3501/395/213/2002/3895/1305/6323(6327)/1087/1431/1703/2068/2908/3151/4832(4828)/4166/9860
  zip_subregion_8 %>%
  count(subregion, name = "n.8") %>%
  print(n = 27)

zipsubregioncomp <-
  zipsubregion8 %>%
  full_join(zipsubregion7, by = "subregion") %>%
  filter(n.8 != n.7) %>%
  print(n = 27)

# Crosswalk for Missing Utility ID --------

#40:
xwalk_for_missing_utility_id <-
  read_csv(glue::glue("data/2a_power_profiler/inputs/{params$eGRID_year}/xwalk_for_missing_utility_ID.csv"),
           col_types = "cccc") %>%
  janitor::clean_names() %>%
  glimpse()

### Update subregions for utility IDs not in eGRID ------
zip_subregion_9 <-
  zip_subregion_8 %>%
  left_join(xwalk_for_missing_utility_id %>%
              select(utility_number, subregion), 
            by = c("eiaid" = "utility_number")) %>%
  mutate(method = if_else(is.na(subregion.x) & !is.na(subregion.y), "crosswalk for missing utility ID", method),
         subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  distinct() %>%
  glimpse()

zipsubregion9 <-#112, 85, 1536, 3008, 2096, 1931, 68, 69, 883/5147/2737/3663/395/213/2060/4465/1415/6358(6362)/1388/1508/2480/2761/3393/3307/5421(5417)/4394/4191
  zip_subregion_9 %>%
  count(subregion, name = "n.9") %>%
  print(n = 27)

zipsubregioncomp <-
  zipsubregion9 %>%
  full_join(zipsubregion8, by = "subregion") %>%
  filter(n.8 != n.9) %>%
  print(n = 27)
  
# Update missing subregion for zip codes from old power profiler -----
#41:
zip_subregion_10 <-
  zip_subregion_9 %>%
  left_join(zip_codes_from_old_power_profiler_to_add %>%
              select(zip, eiaid, subregion), 
            by = c("zip", "eiaid")) %>%
  mutate(subregion = coalesce(subregion.x, subregion.x = subregion.y)) %>%
  select(-contains(".")) %>%
  distinct() %>%
  glimpse()

zipsubregion10 <-#126/202(181)/1629/3040/2830/1940/68/70/888/5351(5350)/2927(2920)/3835(3831)/400/217/2063/4487/1456(1454)/6534(6538)/1450/1518/2502/2782/3524/3397/5438(5434)/4433/1801(2012)
  zip_subregion_10 %>%
  count(subregion, name = "n.10") %>%
  print(n = 27)

zipsubregioncomp <-
  zipsubregion10 %>%
  full_join(zipsubregion9, by = "subregion") %>%
  filter(n.10 != n.9) %>%
  print(n = 27)

# Update state subregion one to one match -------

#13: 119 (120)
states_and_subregions_grouped <-
  plant_file %>%
  select(state = plant_state, subregion = egrid_subregion) %>%
  filter(!is.na(subregion)) %>%
  distinct() %>%
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
  select(zip, eiaid) %>%
  distinct() %>%
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
  select(zip) %>%
  distinct() %>%
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
  select(zip) %>%
  distinct() %>%
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
  select(zip, state, subregion) %>%
  distinct() %>%
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