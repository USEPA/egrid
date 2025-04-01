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
  mutate(subregion = NA_character_, predominant_utility = NA_character_, old_zip_code = "no", method = NA_character_) %>%
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
  count(subregion) %>%
  print(n = 27)

zipsubregioncomp <-
  zipsubregion6 %>%
  full_join(zipsubregion5, by = "subregion") %>%
  glimpse() %>%
  filter(n.x != n.y) %>%
  print()

################## HAVE CLEANED UP TO HERE #####################

# NERC Region / BA / Transmission ID -----
#38: CURRENT PROJECT

# test <-
#   zip_subregion_6 %>%
#   inner_join(eia_861_sales_ult_cust, by = join_by("eiaid" == "utility_id")) %>%
#   glimpse()
# 
# test2 <-
#   test %>%
#   inner_join(eia_861_utility_data_with_count, by = join_by("eiaid" == "utility_id")) %>%
#   glimpse()

# xwalk_balancing_authority_2 <-
#   xwalk_balancing_authority %>%
#   inner_join(test2, by = join_by("subregion" == "nerc_region.y", "ba_code" == "ba_code.x")) %>%
#   glimpse()
  
  #/////////////////////////////#
  # currently there is no transission id in the ba authority
  # need to finish number 38
  
# 1204 (access has 1160)
xwalk_subregion_transmission <-
  read_csv("data/1_production_model/static_tables/xwalk_subregion_transmission.csv",
           col_types = "cccc") %>%
  janitor::clean_names() %>%
  rename(ba_code = balancing_authority_code, transmission = transmission_or_distribution_system_owner_id, subregion = subrgn) %>%
  rename_with(~ paste(., "_transmission", sep = "")) %>%
  glimpse()

zip_subregion_7 <-
  zip_subregion_6 %>%
  left_join(eia_861_sales_ult_cust, by = c("eiaid" = "utility_id_eia_sales")) %>%
  # glimpse()
  left_join(eia_861_utility_data_with_count, by = c("eiaid" = "utility_id_eia_count")) %>%
  # glimpse()
  left_join(xwalk_subregion_transmission, by = c("nerc_region_eia_count" = "nerc_region_transmission", "ba_code_eia_sales" = "ba_code_transmission", "eiaid" = "transmission_transmission")) %>%
    # glimpse()
  mutate(method = if_else(is.na(subregion) & !is.na(subregion_transmission) | subregion == "" & !is.na(subregion_transmission), "nerc region/ba/transmission ID", method),
         subregion = coalesce(subregion, subregion = subregion_transmission)) %>%
  select(-contains(".")) %>%
  glimpse()
  
#R (access)
zipcount <- # ba 38981 (29702), nerc region 3004 (2861), nerc/ba/. 8629 (4455), old profiler 2852 (2782), plant file transm 42530 (22541), plant file utility 7762 (2616)
  zip_subregion_7 %>%
  count(method) %>%
  print()

#39:

#40:

#41:

#13:

#14:

#15:

#16:

#42:

#46:

#47:

#48:

#49:
#   
#50:

#49:

#51:

#52:
#   
#58:

#59:

#53:

#55:

#56:

#57:

#60:

#65:
 
#66:

#67:
   
#67b:

#74:

#75:

#76:

 