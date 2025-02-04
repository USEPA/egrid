## -------------------------------
##
## Plant file create 
## 
## Purpose: 
## 
## This file creates the plant file for eGRID. 
## This includes all operating plants for the specified eGRID data year
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Sara Sokolinski, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load required libraries --------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params) & "temporal_res" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year) 
    params$temporal_res <- readline(prompt = "Input temporal resolution (annual or monthly): ")
    params$temporal_res <- as.character(params$temporal_res) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
  params$temporal_res <- readline(prompt = "Input temporal resolution (annual or monthly): ")
  params$temporal_res <- as.character(params$temporal_res) 
}


# Load necessary data ----------

### Load EIA data ------------

# check if each file exists. If they do not, stop the script. 
# check for and load EIA-860
if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS"))) { 
  eia_860 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS"))
} else { 
   stop("eia_860_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

# check for and load EIA-861
if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_861_clean.RDS"))) { 
  eia_861 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_861_clean.RDS"))
} else { 
   stop("eia_861_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

# check for and load EIA-923
if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) { 
  eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))
} else { 
   stop("eia_923_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

### Load lower-level eGRID files (unit and generator files) ------------

# load generator file
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/generator_file.RDS"))) { 
  generator_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/generator_file.RDS"))
} else { 
   stop("generator_file.RDS does not exist. Run generator_file_create.R to obtain.")}

# load unit file
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))) { 
  unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))
} else { 
   stop("unit_file.RDS does not exist. Run unit_file_create.R to obtain.")}

### Load necessary functions --------------------

source("scripts/functions/function_paste_concat.R")
source("scripts/functions/function_update_source.R")

### Load crosswalks and static tables ----------------------

# load in name matches for shorthand to snake_case
if(file.exists("data/static_tables/name_matches.RData")) {
  load("data/static_tables/name_matches.RData")
} else { 
    source("scripts/name_matching.R")
    load("data/static_scripts/name_matches.RData")}

# crosswalk for plant IDs between EPA and EIA data
xwalk_oris_epa <- 
  read_csv("data/static_tables/xwalk_oris_epa.csv") %>% 
  select(eia_plant_id, epa_plant_id) %>% 
  mutate(eia_plant_id = as.character(eia_plant_id), 
         epa_plant_id = as.character(epa_plant_id))

# emission factors for CO2, CH4, and N2O
ef_co2_ch4_n2o <- 
  read_csv("data/static_tables/co2_ch4_n2o_ef.csv") %>% 
  filter(!is.na(eia_fuel_code)) 

# crosswalk that updates county names to match EIA data
xwalk_county_names <- read_csv("data/static_tables/xwalk_fips_names_update.csv") %>% 
  janitor::clean_names() 

# OG/OTH fuel types to update
oth_og_recode <- 
  read_csv("data/static_tables/og_oth_units_to_change_fuel_type.csv") %>% 
  select(plant_id, "fuel_type" = primary_fuel_type, fuel_code) %>% 
  mutate(plant_id = as.character(plant_id))

# global warming potential values are used to calculate CO2e
gwp <- 
  read_csv("data/static_tables/global_warming_potential.csv") %>% 
  janitor::clean_names()

# fuel type categories
fuel_type_categories <- 
  read_csv("data/static_tables/fuel_type_categories.csv", 
           col_types = "cccccccc") 

# manual corrections
manual_corrections <- 
  read_xlsx("data/static_tables/manual_corrections.xlsx", 
            sheet = "plant_file", 
            col_types = c("text", "text", "text"))

# previous eGRID year CHP plants
plant_prev_year <- # plant file of previous year
  read_csv("data/static_tables/egrid_2022_chp.csv", 
           col_types = "cc") %>% 
  janitor::clean_names() %>% 
  rename(plant_id = orispl, 
         prev_egrid_chp = chpflag)

# EPA CHP database
chp_database <- 
  read_csv("data/static_tables/chp_database.csv", 
           col_types = cols_only(`ORIS Code` = "c")) %>% 
  rename(plant_id = `ORIS Code`)

#### Region crosswalk and tables ------------
# state and county FIPS codes matched to each state and county name
state_county_fips <- 
  read_csv("data/static_tables/state_county_fips.csv") %>% janitor::clean_names() %>%  # load in table and clean names to snake_case
  left_join(xwalk_county_names, by = c("postal_state_code" = "state",
                                       "county_name" = "appendix_6_county_name")) %>%
  mutate(county_name = if_else(is.na(eia_county_name), county_name, eia_county_name)) %>% # update county names to match EIA data if applicable
  select("plant_state" = postal_state_code, # rename to match plant file 
         "county" = county_name, 
         fips_state_code, 
         fips_county_code) 

# crosswalk to adjust Alaska county names 
xwalk_alaska_fips <- 
  read_csv("data/static_tables/xwalk_alaska_fips.csv") %>%
  janitor::clean_names() %>% 
  select("plant_state" = state, "plant_id" = plant_code, county, "new_county" = appendix_6_county_name) %>% 
  mutate(plant_id = as.character(plant_id))

# BA codes to update
ba_codes <- 
  read_csv("data/static_tables/ba_codes.csv") %>% janitor::clean_names() %>% 
  rename("ba_code" = bacode, 
         "ba_name"= baname)

# crosswalk that matches ba_code and ba_name to egrid_subregions
xwalk_ba <- 
  read_csv("data/static_tables/xwalk_balancing_authority.csv") %>%
  janitor::clean_names() %>% 
  select(ba_code = "balancing_authority_code",
         ba_name = "balancing_authority_name",
         egrid_subregion = "subrgn") 

# crosswalk that matches BA code and system_owner_id to update egrid_subregions
xwalk_subregion_transmission <- 
  read_csv("data/static_tables/xwalk_subregion_transmission.csv") %>% janitor::clean_names() %>% 
  rename(nerc = "nerc_region",
         ba_code = "balancing_authority_code",
         system_owner_id = "transmission_or_distribution_system_owner_id",
         egrid_subregion = "subrgn") %>% 
  mutate(system_owner_id = as.character(system_owner_id)) %>% distinct()

# crosswalk that matches ba_code, system_owner_id, and utility_id to update NA egrid_subregions
xwalk_subregion_utility <- 
  read_csv("data/static_tables/xwalk_subregion_utility.csv") %>% janitor::clean_names() %>% 
  rename(nerc = "nerc_region",
         ba_code = "balancing_authority_code",
         system_owner_id = "transmission_or_distribution_system_owner_id",
         egrid_subregion = "subrgn") %>% 
  mutate(system_owner_id = as.character(system_owner_id),
         utility_id = as.character(utility_id)) %>% distinct() %>% 
  group_by(nerc, ba_code, system_owner_id, utility_id) %>% filter(n() == 1)

# crosswalk that matches plant_id and plant_state to plant file to update NA egrid_subregions
xwalk_oris_subregion <- 
  read_csv("data/static_tables/xwalk_oris_subregion.csv") %>% janitor::clean_names() %>% 
  select(plant_state = "pstatabb",
         plant_id = "orispl", 
         egrid_subregion = "subrgn") %>% distinct() %>% 
  mutate(plant_id = as.character(plant_id))

# crosswalk that matches plant_id to plant file update NA egrid_subregions
xwalk_nerc_assessment <- 
  read_csv("data/static_tables/nerc_assessment_areas_grouped_by_plant.csv") %>% janitor::clean_names() %>% 
  full_join(read_csv("data/static_tables/xwalk_nerc_assessment.csv") %>% janitor::clean_names()) %>% 
  rename(plant_id = "plant_code",
         egrid_subregion = "subrgn") %>% 
  mutate(plant_id = as.character(plant_id)) %>% 
  filter(!is.na(egrid_subregion) & !is.na(plant_id)) %>%
  select(-assessment_area) %>% distinct() %>% 
  group_by(plant_id) %>% filter(n() == 1) # remove plant_ids with multiple NERC regions listed

# crosswalk that matches egrid_subregions to egrid_subregion_name
egrid_subregions <-  
  read_csv("data/static_tables/egrid_nerc_subregions.csv") %>% janitor::clean_names() %>% 
  rename("egrid_subregion" = "subrgn",
         "egrid_subregion_name" = "srname")

# Aggregate unit file to plant level ------------------------

plant_unit <- 
  unit_file %>% 
  group_by(year, plant_id, plant_state, plant_name) %>%
  summarize(capd_flag = paste_concat(capd_flag),
            num_units = n(), # count units in a plant
            # unadjusted heat input
            across(.cols = c(contains("heat_input"), -contains("source")), # sum all heat_input columns 
                   .fns = ~ if_else(all(is.na(.x)), NA_real_, sum(.x, na.rm = TRUE)),
                   .names = "unadj_{.col}"), 
            unadj_heat_input_source = paste_concat(heat_input_source),
            unadj_heat_input_oz_source = paste_concat(heat_input_oz_source),
            # unadjusted NOx and NOx ozone mass
            across(.cols = c(contains("nox"), -contains("source"), -contains("controls")), # sum all nox_mass columns, units: tons
                   .fns = ~ if_else(all(is.na(.x)), NA_real_, sum(.x, na.rm = TRUE)),
                   .names = "unadj_{.col}"),
            unadj_nox_source = paste_concat(nox_source),
            unadj_nox_oz_source = paste_concat(nox_oz_source),
            # unadjusted SO2 mass
            across(.cols = c(contains("so2_mass"), -contains("source")), # sum all so2_mass columns, units: tons
                   .fns = ~ if_else(all(is.na(.x)), NA_real_, sum(.x, na.rm = TRUE)),
                   .names = "unadj_{.col}"),
            unadj_so2_source = paste_concat(so2_source),
            # unadjusted CO2 mass
            across(.cols = c(contains("co2_mass"), -contains("source")), # sum all co2_mass columns, units: tons
                   .fns = ~ if_else(all(is.na(.x)), NA_real_, sum(.x, na.rm = TRUE)),
                   .names = "unadj_{.col}"),
            unadj_co2_source = paste_concat(co2_source),
            # Hg mass is not carried into plant file, so we assign NAs 
            # if Hg mass is included in a future version of eGRID, summing the mass and carrying over the source like above can be done
            across(.cols = starts_with("hg_mass"), 
                   .fns = ~ NA_real_, 
                   .names = "unadj_{.col}")) %>%
  mutate(unadj_hg_source = "--") %>% 
  ungroup()

# Aggregate generator file to plant level----------------------------

plant_gen <- 
  generator_file %>% 
  filter(!is.na(plant_id)) %>% # remove rows with NA plant ID 
  group_by(year, plant_id, plant_state, plant_name) %>%
  summarize(num_generators = n(), # count
            # assign NA if all values are NA
            nameplate_capacity = if_else(all(is.na(nameplate_capacity)), 
                                         NA_real_, sum(nameplate_capacity, na.rm = TRUE)), # units: MW
            fuel_code = paste_concat(fuel_code), 
            across(.cols = starts_with("generation"), # units: MWh
                   .fns = ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup()

# Combustion heat input ---------------------------------------
# calculate heat input from combustion fuels

unit_heat_input <- 
  unit_file %>% 
  group_by(plant_id, prime_mover, primary_fuel_type) %>%
  summarize(across(.col = c(contains("heat_input"), -contains("source")), 
                   .fns = ~ sum(.x, na.rm = TRUE), 
                   .names = "unadj_{.col}")) %>% ungroup()

combust_heat_input <- 
  unit_heat_input %>% 
  filter(primary_fuel_type %in% fuel_type_categories[["combustion_fuels"]], 
         prime_mover != "FC") %>%
  group_by(plant_id) %>% 
  summarize(# unadjusted combustion heat input 
            across(.cols = contains("unadj_heat_input"), 
                   .fns = ~ sum(.x, na.rm = TRUE), 
                   .names = "{str_replace(.col, 'unadj', 'unadj_combust')}")) %>% ungroup()

# join with aggregated unit file
plant_unit_2 <- 
  plant_unit %>% 
  left_join(combust_heat_input)  

# Update capacity factor  -----------------------------------------------

# create dataframe of number of hours in each year or month
if (params$temporal_res == "annual") { 
  hours <- data.frame("hours_ann" = 8760)}
if (params$temporal_res == "monthly") { 
  hours <- 
    data.frame("hours_ann" = 8760, 
               "hours_january" = 744, 
               "hours_february" = 672, 
               "hours_march" = 744, 
               "hours_april" = 720, 
               "hours_may" = 744, 
               "hours_june" = 720, 
               "hours_july" = 744, 
               "hours_august" = 744, 
               "hours_september" = 720, 
               "hours_october" = 744,
               "hours_november" = 720, 
               "hours_december" = 744)}

plant_gen_2 <- 
  cbind(plant_gen, hours) %>% 
  mutate(across(.cols = c(starts_with("generation"), -generation_oz), 
                .fns = ~ if_else(.x < 0, 0, # some generation values may be negative, set to 0 if so
                                 .x / (nameplate_capacity * get(str_replace(cur_column(), "generation", "hours")))), 
                .names = "{str_replace_all(.col, c('generation' = 'capfac', '_ann' = ''))}")) %>% 
  select(-starts_with("hours"))

# Calculate CH4 emissions N2O emissions  ----------------------------------------

# join with EIA-923 to calculate at plant level

if (params$temporal_res == "annual"){ 
  temporal_res_eia_cols <- 
    c("tot_mmbtu")}
if (params$temporal_res == "monthly") { 
  temporal_res_eia_cols <- 
    c("tot_mmbtu", 
      paste0("tot_mmbtu_", tolower(month.name)))}
    
emissions_ch4_n2o <- 
  eia_923$generation_and_fuel_combined %>% 
  filter(prime_mover != "FC") %>% 
  rename("tot_mmbtu" = total_fuel_consumption_mmbtu) %>% 
  select(plant_id, all_of(temporal_res_eia_cols), fuel_type) %>%
  left_join(ef_co2_ch4_n2o, by = c("fuel_type" = "eia_fuel_code")) %>%
  mutate(across(.cols = all_of(temporal_res_eia_cols), # calculate CH4 mass by fuel type
                .fns = ~ ch4_ef * .x, 
                .names = "{str_replace(.col, 'tot_mmbtu', 'unadj_ch4_mass')}"),
         across(.cols = all_of(temporal_res_eia_cols), # calculate N2O mass by fuel type
                .fns = ~ n2o_ef * .x, 
                .names = "{str_replace(.col, 'tot_mmbtu', 'unadj_n2o_mass')}")) %>%
  group_by(plant_id) %>%
  summarize(across(.cols = starts_with("unadj_ch4_mass"), # sum CH4 mass by plant
                   .fns = ~ if_else(all(is.na(.x)), NA_real_, sum(.x, na.rm = TRUE))), 
            across(.cols = starts_with("unadj_n2o_mass"), # sum N2O mass by plant
                   .fns = ~ if_else(all(is.na(.x)), NA_real_, sum(.x, na.rm = TRUE))), 
            ch4_source = if_else(is.na(unadj_ch4_mass), NA_character_, "EIA"),
            n2o_source = if_else(is.na(unadj_n2o_mass), NA_character_, "EIA")) %>% 
  ungroup() 
  

# Create plant file ------------

# combine generator and unit aggregation files into one plant file 
# calculate unadjusted CO2e 
# CO2 equivalent (CO2e) adds in other greenhouse gas emission masses 
# CH4 and N2O are multiplied by their associated global warming potential value 
# global warming potential for CH4 and N2O are stored in a static table (gwp)

plant_file <- 
  plant_gen_2 %>% 
  select(-plant_name) %>% # names differ between sources, so default to names in unit file
  full_join(plant_unit_2, by = c("plant_id", "plant_state", "year")) %>% 
  left_join(emissions_ch4_n2o) %>% # add in CH4 and N2O emission masses
  mutate(# calculate unadjusted CO2 equivalent 
         across(.cols = starts_with("unadj_co2_mass"),
                .fns = ~ if_else(is.na(.x), 0, .x) + 
                         if_else(is.na(get(str_replace(cur_column(), 'unadj_co2_mass', 'unadj_ch4_mass'))), 
                                 0, gwp$gwp[gwp$gas == "CH4"] * get(str_replace(cur_column(), 'unadj_co2_mass', 'unadj_ch4_mass')) / 2000) + 
                         if_else(is.na(get(str_replace(cur_column(), 'unadj_co2_mass', 'unadj_n2o_mass'))), 
                                 0, gwp$gwp[gwp$gas == "N2O"] * get(str_replace(cur_column(), 'unadj_co2_mass', 'unadj_n2o_mass')) / 2000),
                .names = "{str_replace(.col, 'unadj_co2_mass', 'unadj_co2e_mass')}"), 
         # if all emission masses are NA, fill CO2e mass with NA
         across(.cols = starts_with("unadj_co2e_mass"),
                .fns = ~ if_else(is.na(get(str_replace(cur_column(), 'unadj_co2e_mass', 'unadj_co2_mass'))) & 
                                 is.na(get(str_replace(cur_column(), 'unadj_co2e_mass', 'unadj_ch4_mass'))) & 
                                 is.na(get(str_replace(cur_column(), 'unadj_co2e_mass', 'unadj_n2o_mass'))), 
                                 NA_real_, .x)),
         unadj_co2e_source = if_else(is.na(unadj_co2e_mass), NA_character_, "Calculated")) 

# Pull in plant identifying information from the EIA-860 Plant file  -------------------------------

# join EIA-860 data to the aggregated generator file
plant_file_2 <- 
  plant_file %>% 
  left_join(eia_860$plant %>% 
              select(plant_id, 
                     county, 
                     "lat" = latitude, 
                     "lon" = longitude,
                     utility_name, 
                     utility_id,
                     "system_owner" = transmission_or_distribution_system_owner, 
                     "system_owner_id" = transmission_or_distribution_system_owner_id,
                     "ba_name" = balancing_authority_name,
                     "ba_code" = balancing_authority_code,
                     sector_name, 
                     "nerc" = nerc_region) %>%
              distinct(), 
            by = c("plant_id")) %>% 
  rows_patch(eia_860$operating_pr %>% # update rows with Puerto Rico data 
               select(plant_id, 
                      county, 
                      "lat" = latitude, 
                      "lon" = longitude, 
                      utility_name, 
                      utility_id, 
                      "ba_code" = balancing_authority_code, 
                      sector_name) %>% 
               mutate(lat = as.character(lat), 
                      lon = as.character(lon)) %>% 
               distinct(), 
             by = "plant_id", unmatched = "ignore") %>% 
  mutate(county = if_else(grepl('not in file|Not in file|NOT IN FILE', county), NA_character_ , county)) 

# update plant IDs that do not have EPA plant ID matches to EIA-860 
plant_eia_xwalk <- 
  plant_file_2 %>% 
  filter(plant_id %in% xwalk_oris_epa$epa_plant_id & is.na(county)) %>% # filter for plants that are not matched via EIA-860 due to different plant IDs
  left_join(xwalk_oris_epa, by = c("plant_id" = "epa_plant_id")) %>% # use crosswalk for EIA / EPA plant IDs to match some EPA plants to EIA data
  rows_update(eia_860$plant %>% 
                select("eia_plant_id" = plant_id, 
                       county, 
                       "lat" = latitude, 
                       "lon" = longitude,
                       utility_name, 
                       utility_id,
                       "system_owner" = transmission_or_distribution_system_owner, 
                       "system_owner_id" = transmission_or_distribution_system_owner_id,
                       "ba_name" = balancing_authority_name,
                       "ba_code" = balancing_authority_code,
                       sector_name, 
                       "nerc" = nerc_region) %>%
                distinct(), 
              by = c("eia_plant_id"), unmatched = "ignore") %>%
  rows_patch(eia_860$operating_pr %>% # update rows with Puerto Rico data 
               select(plant_id, 
                      county, 
                      "lat" = latitude, 
                      "lon" = longitude, 
                      utility_name, 
                      utility_id, 
                      "ba_code" = balancing_authority_code, 
                      sector_name) %>% 
               mutate(lat = as.character(lat), 
                      lon = as.character(lon)) %>% 
               distinct(), 
             by = "plant_id", unmatched = "ignore") %>% 
  mutate(county = if_else(grepl('not in file|Not in file|NOT IN FILE', county), NA_character_ , county)) %>% 
  select(-eia_plant_id)  

# bind rows with data from EIA-860 together 
plant_file_3 <- 
  plant_file_2 %>% 
  rows_update(plant_eia_xwalk, by = c("plant_id"))

# Plant region assignments -----------

# assign plants to counties, states, ISORTOs, balancing authorities, and NERC regions

### Add in FIPS codes for state and county ----------------

# match county data to EIA-860 
eia_860_plant_county <- 
  eia_860$plant %>% 
  select(plant_state, county) %>% 
  left_join(state_county_fips, by = c("plant_state", "county")) %>% 
  distinct()

# update plant file with FIPS information 
plant_file_4 <- 
  plant_file_3 %>% 
  left_join(state_county_fips, by = c("plant_state", "county")) %>% 
  rows_patch(state_county_fips %>% select(plant_state, fips_state_code) %>% distinct(), 
             by = c("plant_state"), unmatched = "ignore") %>% # patch plant_state rows that did not match on state and county 
  left_join(xwalk_alaska_fips, by = c("plant_state", "plant_id", "county")) %>% 
  mutate(county = if_else(!is.na(new_county), new_county, county)) %>% 
  select(-new_county)

### Balancing authority assignments --------------

# identify utility IDs that have NA BA codes in the plant file 
eia_861_utility <- 
  eia_861$sales_ult_cust %>% 
  select(plant_state = state, 
         utility_id = utility_number, 
         ba_code) %>% 
  mutate(utility_id = as.character(utility_id)) %>%
  group_by(utility_id) %>% 
  mutate(count = n()) %>% 
  filter(count == 1) %>% # only include utility IDs with 1 matching BA code
  ungroup()

# update BA codes via EIA-860 BA table by matching BA names
eia_861_ba <- 
  eia_861$balancing_authority %>% 
  select(plant_state = state, 
         ba_code, 
         ba_name = balancing_authority_name) 

# update plant_file with the lookup
plant_file_5 <- 
  plant_file_4 %>% 
  rows_patch(eia_861_utility %>% select(-count), by = c("plant_state", "utility_id"), unmatched = "ignore") %>%  # update plants with NA BA codes
  rows_patch(eia_861_utility %>% select(-count) %>% rename("system_owner_id" = utility_id), # match some utility IDs by system owner IDs
             by = c("plant_state", "system_owner_id"), unmatched = "ignore") %>% 
  rows_patch(eia_861_ba, by = c("plant_state", "ba_name"), unmatched = "ignore") %>% # update BA codes that are NA from EIA-861 BA
  rows_patch(eia_861_ba, by = c("plant_state", "ba_code"), unmatched = "ignore") %>% # update BA names that are NA from EIA-861 BA
  mutate(ba_code = case_when(ba_code == "NA" ~ NA_character_, # if BA code is "NA", set as an NA character
                             ba_name == "No BA" ~ NA_character_, 
                             ba_code == "PS" ~ "PSCO", 
                             ba_name == "Hawaiian Electric Co Inc" ~ "HECO",
                             TRUE ~ ba_code)) %>% 
  rows_update(ba_codes, by = c("ba_code"), unmatched = "ignore") # update ba_name by ba_code


### NERC and eGRID subregion assignments ----------------------------------------

# update plant file with NERC and eGRID subregion updates
plant_file_6 <- 
  plant_file_5 %>% 
  mutate(nerc = case_when(plant_state == "PR" ~ "PR",
                          plant_state == "AK" ~ "AK",
                          plant_state == "HI" ~ "HI",
                          is.na(nerc) ~ "NA",
                          nerc == "" ~ "NA",
                          TRUE ~ nerc), 
         egrid_subregion = case_when(nerc == "TRE" ~ "ERCT",
                                     nerc == "FRCC" ~ "FRCC",
                                     nerc == "PR" ~ "PRMS",
                                    TRUE ~ NA_character_),
         system_owner_id = if_else(is.na(system_owner_id), "-9999", system_owner_id)) %>% 
  rows_patch(xwalk_ba, by = c("ba_code"), unmatched = "ignore") %>% 
  rows_patch(xwalk_subregion_utility, by =  c("nerc", "ba_code", "system_owner_id", "utility_id"), unmatched = "ignore") %>% 
  rows_patch(xwalk_subregion_transmission, by = c("nerc", "ba_code", "system_owner_id"), unmatched = "ignore") %>% 
  rows_update(xwalk_oris_subregion, by = c("plant_state", "plant_id"), unmatched = "ignore") %>% 
  rows_patch(xwalk_nerc_assessment, by = c("plant_id"), unmatched = "ignore") %>% 
  left_join(egrid_subregions, by = c("egrid_subregion")) %>% 
  mutate(ba_code = case_when(is.na(ba_code) & plant_state == "AK" ~ "NA - AK", # update NA BAs to state specific NAs
                              is.na(ba_code) & plant_state == "HI" ~ "NA - HI", 
                              is.na(ba_code) & plant_state == "PR" ~ "NA - PR",
                              is.na(ba_code) & !(plant_state %in% c("AK", "HI", "PR")) ~ "NA",
                              TRUE ~ ba_code),
        ba_name = case_when(plant_state == "AK" & # assign Alaska BAs with no BA to NA - AK
                            (is.na(ba_name) | ba_name %in% c("No BA", "No balancing authority")) ~ 
                            "No balancing authority - AK",
                          plant_state == "HI" & # assign Hawaii BAs with no BA to NA - HI 
                            (is.na(ba_name) | ba_name %in% c("No BA", "No balancing authority")) ~ 
                            "No balancing authority - HI", 
                          plant_state == "PR" & # assign Puerto Rico BAs with no BA to NA - PR 
                            (is.na(ba_name) | ba_name %in% c("No BA", "No balancing authority")) ~ 
                            "No balancing authority - PR", 
                          !(plant_state %in% c("AK", "HI", "PR")) &
                            (is.na(ba_name) | ba_name %in% c("No BA", "No balancing authority")) ~
                            "No balancing authority",
                          ba_name == "No BA" ~ NA_character_, 
                          TRUE ~ ba_name))

### ISO/RTO assignments -----------------

plant_file_7 <- 
  plant_file_6 %>% 
  mutate(isorto = case_when(ba_code == "CISO" ~ "CAISO",
                            ba_code == "ERCO" ~ "ERCOT",
                            ba_code == "ISNE" ~ "ISONE",
                            ba_code == "MISO" ~ "MISO",
                            ba_code == "NYIS" ~ "NYISO",
                            ba_code == "PJM" ~ "PJM",
                            ba_code == "SPA" ~ "SPP",
                            TRUE ~ NA_character_)) %>%
  mutate(isorto = if_else(plant_state %in% c("AL","AK","AZ","CO","FL","GA", "HI", "ID", "OR", "SC", "TN", "UT", "WA"),
                          NA_character_, isorto))

# Update plant lat/lon ----------------------------------------

lat_manual_corrections <- 
  manual_corrections %>% 
  filter(column_to_update == "lat") %>% 
  select(plant_id, "lat" = update) %>% 
  mutate(lat = as.numeric(lat))

lon_manual_corrections <- 
  manual_corrections %>% 
  filter(column_to_update == "lon") %>% 
  select(plant_id, "lon" = update) %>% 
  mutate(lon = as.numeric(lon))

plant_file_8 <- 
  plant_file_7 %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         # if any lat/lon coordinates are 0, make them NA 
         lat = if_else(lat == 0, NA_real_, lat),
         lon = if_else(lon == 0, NA_real_, lon)) %>% 
  # manual corrections to lat/lon 
  rows_update(lat_manual_corrections, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(lon_manual_corrections, by = c("plant_id"), unmatched = "ignore")


# Assign plant primary fuel -----------------------

# first summarize the unit file to get the sum of heat input for each fuel type
# then ungroup and filter to the row (fuel type) with the max value
unit_fuel_by_plant <- 
  unit_file %>% 
  group_by(plant_id, primary_fuel_type) %>% 
  summarize(heat_input = sum(heat_input, na.rm = TRUE)) %>%
  mutate(heat_input = if_else(is.na(heat_input), 0, heat_input)) %>% # replace NA with 0 so rows with only NA do not get filtered out
  ungroup() %>% 
  group_by(plant_id) %>% 
  filter(heat_input == max(heat_input)) %>% ungroup()

# do the same for the generator file with nameplate_capacity       
gen_fuel_by_plant <- 
  generator_file %>% group_by(plant_id, fuel_code) %>% 
  summarize(nameplate_capacity = sum(nameplate_capacity, na.rm = TRUE)) %>%
  mutate(nameplate_capacity = if_else(is.na(nameplate_capacity), 0, nameplate_capacity)) %>% # replace NA with 0 so rows with only NA do not get filtered out
  ungroup() %>% 
  group_by(plant_id) %>% 
  filter(nameplate_capacity == max(nameplate_capacity)) %>% ungroup()

# join these tables together
# update primary fuel type as fuel_code (from gen file) if heat_input is 0 and primary_fuel_type (from unit file) otherwise
# drop fuel_code and take distinct values 
fuel_by_plant <- 
  unit_fuel_by_plant %>% 
  full_join(gen_fuel_by_plant, by = c("plant_id"), relationship = "many-to-many") %>%
  mutate(primary_fuel_type = case_when(
                                  heat_input == 0 & is.na(fuel_code) ~ primary_fuel_type, 
                                  heat_input == 0 ~ fuel_code, 
                                  is.na(primary_fuel_type) & !is.na(fuel_code) ~ fuel_code,
                                  TRUE ~ primary_fuel_type)) %>% 
  select(-fuel_code) %>% distinct()

# from EIA-923 replace NA values in total_fuel_consumption_mmbtu with 0 
eia_923_gen_fuel <- 
  eia_923$generation_and_fuel_combined %>% 
  select(plant_id, fuel_type, total_fuel_consumption_mmbtu) %>% 
  mutate(total_fuel_consumption_mmbtu = if_else(is.na(total_fuel_consumption_mmbtu), 0, total_fuel_consumption_mmbtu))

# get plant_ids that are duplicated in fuel_by_plant
# filter to duplicated ids and join with EIA-923 Generation and Fuel
# filter to rows with the max total_fuel_consumption_mmbtu
dup_ids_fuel <- 
  fuel_by_plant$plant_id[which(duplicated(fuel_by_plant$plant_id))] %>% unique() 

fuel_dups <- 
  fuel_by_plant %>% filter(plant_id %in% dup_ids_fuel) %>%
  left_join(eia_923_gen_fuel,
            by = c("plant_id" = "plant_id",
                   "primary_fuel_type" = "fuel_type")) %>% 
  distinct() %>% 
  group_by(plant_id) %>% 
  filter(total_fuel_consumption_mmbtu == max(total_fuel_consumption_mmbtu, na.rm = TRUE))

# drop dups from fuel_by_plant for easier joining later
fuel_by_plant_2 <- 
  fuel_by_plant %>% 
  filter(!plant_id %in% dup_ids_fuel) %>% 
  select(-heat_input, -nameplate_capacity)

# get plant_ids that are STILL duplicated
dup_ids_fuel_2 <- fuel_dups$plant_id[which(duplicated(fuel_dups$plant_id))] %>% unique()

# take first primary fuel for any remaining duplicates
fuel_dups_2 <- 
  fuel_dups %>% 
  group_by(plant_id) %>% 
  slice(1) %>% # take the first primary fuel listed for any with duplicates (usually duplicates of 0 heat input / fuel consumption)
  select(-heat_input, -nameplate_capacity, -total_fuel_consumption_mmbtu) %>% distinct()

# join the duplicates back into fuel_by_plant
fuel_by_plant_3 <- 
  fuel_by_plant_2 %>% full_join(fuel_dups_2)

stopifnot(all(!duplicated(fuel_by_plant_3$plant_id))) # check there are no more duplicates

# Assign plant primary fuel category  ---------------

# identify coal, oil, gas, other fossil, biomass, and other fuel types
coal_fuels <- fuel_type_categories[["coal_fuels"]]
oil_fuels <- fuel_type_categories[["oil_fuels"]] 
gas_fuels <- fuel_type_categories[["gas_fuels"]] 
other_ff <- fuel_type_categories[["other_ff"]] 
other_fuels <- fuel_type_categories[["other_fuels"]]
biomass_fuels <- fuel_type_categories[["biomass_fuels"]]

fuel_by_plant_4 <- 
  fuel_by_plant_3 %>%
  mutate(primary_fuel_category = case_when(primary_fuel_type %in% coal_fuels[!is.na(coal_fuels)] ~ "COAL", 
                                           primary_fuel_type %in% oil_fuels[!is.na(oil_fuels)] ~ "OIL",
                                           primary_fuel_type %in% gas_fuels[!is.na(gas_fuels)] ~ "GAS",
                                           primary_fuel_type %in% other_ff[!is.na(other_ff)] ~ "OFSL", # other fossil fuel
                                           primary_fuel_type == "NUC" ~ "NUCLEAR",
                                           primary_fuel_type == "WAT" ~ "HYDRO",
                                           primary_fuel_type == "SUN" ~ "SOLAR",
                                           primary_fuel_type == "WND" ~ "WIND",
                                           primary_fuel_type == "GEO" ~ "GEOTHERMAL",
                                           primary_fuel_type %in% other_fuels[!is.na(other_fuels)] ~ "OTHF", # derived from waste heat/hydrogen/purchased/unknown
                                           primary_fuel_type %in% biomass_fuels[!is.na(biomass_fuels)] ~ "BIOMASS")) 

primary_fuel_type_corrections <- 
  manual_corrections %>% 
  filter(column_to_update == "primary_fuel_type") %>% 
  select(plant_id, "primary_fuel_type" = update)

primary_fuel_category_corrections <- 
  manual_corrections %>% 
  filter(column_to_update == "primary_fuel_category") %>% 
  select(plant_id, "primary_fuel_category" = update)

plant_file_9 <- 
  plant_file_8 %>% 
  full_join(fuel_by_plant_4, by = c("plant_id")) %>% 
  # manual corrections to primary fuel type and category for plants 55970 and 10154 
  ### Note: check for updates or changes each data year ###
  rows_update(primary_fuel_type_corrections, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(primary_fuel_category_corrections, by = c("plant_id"), unmatched = "ignore")


# Create plant flags ------------

### Create coal flag -----------------
# create a flag that identifies which plants are coal

coal_plants <- 
  eia_923$generation_and_fuel_data %>% 
  group_by(plant_id, fuel_type) %>% 
  summarize(total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) %>%
  full_join(oth_og_recode) %>%
  mutate(coal_flag = if_else((fuel_type %in% coal_fuels | fuel_code %in% coal_fuels) & 
                               total_fuel_consumption_mmbtu > 0, "Yes", NA_character_)) %>%  # if the fuel type is in coal_fuels, set the flag to "Yes", otherwise NA
  ungroup() %>% 
  group_by(plant_id) %>% 
  summarize(coal_flag = max(coal_flag)) %>%  # if any generation comes from coal, flag as a coal plant
  ungroup()
  
plant_file_10 <- 
  plant_file_9 %>% 
  left_join(coal_plants, by = c("plant_id")) 

#### Update coal flag via primary fuel category ----------------

# the coal flag is first assigned through EIA-923 Gen and Fuel data 
# check if plants missing coal flags have a primary_fuel_category == "COAL"
update_coal <- 
  plant_file_10 %>% 
  filter(is.na(coal_flag)) %>% 
  select(plant_id, primary_fuel_category) %>%
  filter(primary_fuel_category == "COAL")

plant_file_11 <- 
  plant_file_9 %>% 
  mutate(coal_flag = if_else(plant_id %in% update_coal$plant_id, "Yes", NA_character_))

### Create combustion flag --------------------

eia_923_combust <- 
  eia_923$generation_and_fuel_combined %>%
  mutate(combustion = if_else(fuel_type %in% fuel_type_categories[["combustion_fuels"]], 1, 0)) %>% 
  group_by(plant_id) %>%
  summarize(sum_combustion = sum(combustion, na.rm = TRUE),
            count_combustion = n()) %>%
  mutate(combust_flag = case_when(sum_combustion == 0 ~ 0,
                                  sum_combustion != count_combustion ~ 0.5,
                                  TRUE ~ 1)) %>% 
  select(-sum_combustion, -count_combustion)

plant_file_12 <- 
  plant_file_11 %>% 
  left_join(eia_923_combust, by = c("plant_id"))


### Create pumped storage flag ----------------------------------------

# identify plants with prime mover PS (pumped)
pumped_storage <- 
  generator_file %>% 
  select(plant_id, plant_state, plant_name, prime_mover) %>%
  filter(prime_mover == "PS") %>% distinct()

plant_file_13 <- 
  plant_file_12 %>% 
  mutate(ps_flag = if_else(plant_id %in% pumped_storage$plant_id, "Yes", NA_character_))

# Biomass adjustment -------------

# Emissions from biomass plants are adjusted to account for the CO2 sequestered during biomass growth equals the amount released during combustion

## Biomass Fuels adjustment (CO2) ----------------

# identify biomass fuels to add adjustments for
biomass_fuel_adj <- fuel_type_categories[["biomass_fuel_adj"]]

# subtract CO2 from biofuels from the unadj_co2_mass to  create co2_mass
eia_923_biomass <- 
  eia_923$generation_and_fuel_combined %>% 
  select(plant_id, "tot_mmbtu" = total_fuel_consumption_mmbtu, fuel_type, starts_with("tot_mmbtu")) %>%
  left_join(ef_co2_ch4_n2o, by = c("fuel_type" = "eia_fuel_code")) %>% 
  filter(fuel_type %in% biomass_fuel_adj[!is.na(biomass_fuel_adj)] & tot_mmbtu > 0) %>%
  mutate(across(.cols = all_of(temporal_res_eia_cols), 
                .fns = ~ co2_ef * .x, 
                .names = "{str_replace(.col, 'tot_mmbtu', 'co2_biomass')}")) %>%
  group_by(plant_id) %>%
  summarize(across(.cols = starts_with("co2_biomass"), 
                   .fns = ~ sum(.x, na.rm = TRUE)), 
            across(.cols = starts_with("tot_mmbtu"), 
                   .fns = ~ sum(.x, na.rm = TRUE))) %>%
  mutate(biomass_adj_flag = "Yes") %>%   # add a flag for this adjustment
  ungroup()
  
plant_file_14 <- 
  plant_file_13 %>% 
  left_join(eia_923_biomass, by = c("plant_id"))  %>%
  mutate(across(.cols = starts_with("co2_biomass"), # define CO2 mass, adjusted for biomass CO2 values
                .fns = ~ get(str_replace(cur_column(), "co2_biomass", "unadj_co2_mass")) - .x, 
                .names = "{str_replace(.col, 'co2_biomass', 'co2_mass')}")) 


## Landfill Gas adjustment (NOX, CH4, N2O, SO2) -----------------

eia_923_lfg <- 
  eia_923$generation_and_fuel_combined %>% 
  rename("tot_mmbtu" = total_fuel_consumption_mmbtu) %>% 
  filter(fuel_type == "LFG" & plant_id != 999999 & tot_mmbtu > 0) %>% 
  left_join(ef_co2_ch4_n2o, by = c("fuel_type" = "eia_fuel_code")) %>%
  group_by(plant_id) %>%
  summarize(heat_input_oz_season = sum(tot_mmbtu_may, tot_mmbtu_june, tot_mmbtu_july, tot_mmbtu_august, tot_mmbtu_september, na.rm = TRUE), 
            across(.cols = all_of(temporal_res_eia_cols), 
                   .fns = ~ sum(.x, na.rm = TRUE)), 
            ch4_ef = paste_concat(ch4_ef, number = TRUE),
            n2o_ef = paste_concat(n2o_ef, number = TRUE)) %>%
  ungroup() %>% 
  mutate(
    across(.cols = all_of(temporal_res_eia_cols), 
           .fns = ~ 0.08 * pmax(.x, 0), 
           .names = "{str_replace(.col, 'tot_mmbtu', 'nox_biomass')}"),
    nox_bio_oz = 0.08 * pmax(heat_input_oz_season, 0),
    across(.cols = all_of(temporal_res_eia_cols), 
           .fns = ~ ch4_ef * .x, 
           .names = "{str_replace(.col, 'tot_mmbtu', 'ch4_biomass')}"),
    across(.cols = all_of(temporal_res_eia_cols), 
           .fns = ~ n2o_ef * .x, 
           .names = "{str_replace(.col, 'tot_mmbtu', 'n2o_biomass')}"),
    across(.cols = all_of(temporal_res_eia_cols), 
           .fns = ~ 0.0115 / 2000 * pmax(.x, 0), 
           .names = "{str_replace(.col, 'tot_mmbtu', 'so2_biomass')}"),
    biomass_adj_flag1 = "Yes") %>%
  select(-heat_input_oz_season, -tot_mmbtu, -ch4_ef, -n2o_ef) 
  
# Assign emission values 
# The adjusted emission mass is the same as the unadjusted emission mass if the plant is not a biomass plant
# Otherwise, the adjusted emission mass subtracts the biomass emission mass
plant_file_15 <- 
  plant_file_14 %>% 
  left_join(eia_923_lfg) %>%
  mutate(#nox_mass = if_else(!is.na(nox_biomass), pmax(unadj_nox_mass - nox_biomass, 0), unadj_nox_mass), # take the max to avoid negative values
         across(.cols = starts_with("nox_biomass"),
                .fns = ~ if_else(!is.na(.x), pmax(get(str_replace(cur_column(), "nox_biomass", "unadj_nox_mass")) - .x, 0), 
                                 get(str_replace(cur_column(), "nox_biomass", "unadj_nox_mass"))), 
                .names = "{str_replace(.col, 'nox_biomass', 'nox_mass')}"),
         nox_oz_mass = if_else(!is.na(nox_bio_oz), pmax(unadj_nox_oz_mass - nox_bio_oz, 0), unadj_nox_oz_mass),
         #so2_mass = if_else(!is.na(so2_biomass), pmax(unadj_so2_mass - so2_biomass, 0), unadj_so2_mass),
         across(.cols = starts_with("so2_biomass"),
                .fns = ~ if_else(!is.na(.x), pmax(get(str_replace(cur_column(), "so2_biomass", "unadj_so2_mass")) - .x, 0), 
                                 get(str_replace(cur_column(), "so2_biomass", "unadj_so2_mass"))), 
                .names = "{str_replace(.col, 'so2_biomass', 'so2_mass')}"),
         #co2_mass = if_else(!is.na(co2_biomass), pmax(unadj_co2_mass - co2_biomass, 0), unadj_co2_mass),
         across(.cols = starts_with("co2_biomass"),
                .fns = ~ if_else(!is.na(.x), pmax(get(str_replace(cur_column(), "co2_biomass", "unadj_co2_mass")) - .x, 0), 
                                 get(str_replace(cur_column(), "co2_biomass", "unadj_co2_mass"))), 
                .names = "{str_replace(.col, 'co2_biomass', 'co2_mass')}"),
         #ch4_mass = if_else(!is.na(ch4_biomass), pmax(unadj_ch4_mass - ch4_biomass, 0), unadj_ch4_mass),
         across(.cols = starts_with("ch4_biomass"),
                .fns = ~ if_else(!is.na(.x), pmax(get(str_replace(cur_column(), "ch4_biomass", "unadj_ch4_mass")) - .x, 0), 
                                 get(str_replace(cur_column(), "ch4_biomass", "unadj_ch4_mass"))), 
                .names = "{str_replace(.col, 'ch4_biomass', 'ch4_mass')}"),
         #n2o_mass = if_else(!is.na(n2o_biomass), pmax(unadj_n2o_mass - n2o_biomass, 0), unadj_n2o_mass),
         across(.cols = starts_with("n2o_biomass"),
                .fns = ~ if_else(!is.na(.x), pmax(get(str_replace(cur_column(), "n2o_biomass", "unadj_n2o_mass")) - .x, 0), 
                                 get(str_replace(cur_column(), "n2o_biomass", "unadj_n2o_mass"))), 
                .names = "{str_replace(.col, 'n2o_biomass', 'n2o_mass')}"),
         #hg_mass = unadj_hg_mass, # no biomass variable for Hg 
         across(.cols = starts_with("unadj_hg_mass"),
                .fns = ~ .x, 
                .names = "{str_replace(.col, 'unadj_hg_mass', 'hg_mass')}"),
         nox_oz_mass = pmin(nox_oz_mass, nox_mass, na.rm = TRUE), # if annual NOx mass is lower than ozone NOx mass, use the annual NOx mass 
         biomass_adj_flag = if_else(biomass_adj_flag == "Yes" | biomass_adj_flag1 == "Yes", "Yes", NA_character_)) %>%
  select(-biomass_adj_flag1) %>% 
  mutate(#nox_biomass = pmin(nox_biomass, unadj_nox_mass), # assign the minimum between biomass mass and unadjusted mass to biomass emissions
         across(.cols = starts_with("nox_biomass"), 
                .fns = ~ pmin(.x, get(str_replace(cur_column(), "nox_biomass", "unadj_nox_mass")))), 
         nox_bio_oz = pmin(nox_bio_oz, unadj_nox_oz_mass),
         #ch4_biomass = pmin(ch4_biomass, unadj_ch4_mass),
         across(.cols = starts_with("ch4_biomass"), 
                .fns = ~ pmin(.x, get(str_replace(cur_column(), "ch4_biomass", "unadj_ch4_mass")))), 
         #n2o_biomass = pmin(n2o_biomass, unadj_n2o_mass),
         across(.cols = starts_with("n2o_biomass"), 
                .fns = ~ pmin(.x, get(str_replace(cur_column(), "n2o_biomass", "unadj_n2o_mass")))), 
         #so2_biomass = pmin(so2_biomass, unadj_so2_mass),
         across(.cols = starts_with("so2_biomass"), 
                .fns = ~ pmin(.x, get(str_replace(cur_column(), "so2_biomass", "unadj_so2_mass")))), 
         #co2_biomass = pmin(co2_biomass, unadj_co2_mass),
         across(.cols = starts_with("co2_biomass"), 
                .fns = ~ pmin(.x, get(str_replace(cur_column(), "co2_biomass", "unadj_co2_mass")))), 
         # calculate CO2e biomass after adjustments to all GHG masses are made
         #co2e_biomass = 
        #   if_else(is.na(co2_biomass), 0, co2_biomass) + 
        #   if_else(is.na(ch4_biomass), 0, gwp$gwp[gwp$gas == "CH4"] * ch4_biomass / 2000) + 
        #   if_else(is.na(n2o_biomass), 0, gwp$gwp[gwp$gas == "N2O"] * n2o_biomass / 2000), # calculate CO2e biomass
         across(.cols = starts_with("co2_biomass"), 
                .fns = ~ if_else(is.na(.x), 0, .x) + 
                         if_else(is.na(get(str_replace(cur_column(), "co2", "ch4"))), 0, 
                                 gwp$gwp[gwp$gas == "CH4"] * get(str_replace(cur_column(), "co2", "ch4")) / 2000) + 
                         if_else(is.na(get(str_replace(cur_column(), "co2", "n2o"))), 0, 
                                 gwp$gwp[gwp$gas == "N2O"] * get(str_replace(cur_column(), "co2", "n2o")) / 2000), 
                .names = "{str_replace(.col, 'co2', 'co2e')}"),
         # if all emission masses are NA, fill CO2e mass with NA
         across(.cols = starts_with("co2e_biomass"), 
                .fns = ~ if_else(is.na(get(str_replace(cur_column(), 'co2e_biomass', 'co2_biomass'))) & 
                                 is.na(get(str_replace(cur_column(), 'co2e_biomass', 'ch4_biomass'))) & 
                                 is.na(get(str_replace(cur_column(), 'co2e_biomass', 'n2o_biomass'))), 
                                 NA_real_, .x)), 
         #co2e_mass = if_else(!is.na(co2e_biomass), pmax(unadj_co2e_mass - co2e_biomass, 0), unadj_co2e_mass),
         across(.cols = starts_with("co2e_biomass"), 
                .fns = ~ if_else(!is.na(.x), pmax(get(str_replace(cur_column(), "co2e_biomass", "unadj_co2e_mass")) - .x, 0), 
                                 get(str_replace(cur_column(), "co2e_biomass", "unadj_co2e_mass"))),
                .names = "{str_replace(.col, 'biomass', 'mass')}"), 
         #co2e_biomass = pmin(co2e_biomass, unadj_co2e_mass))
         across(.cols = starts_with("co2e_biomass"),
                .fns = ~ pmin(.x, get(str_replace(cur_column(), "co2e_biomass", "unadj_co2e_mass")))))


# Sum generation by fuel type and plant ID ------------------

if (params$temporal_res == "annual") { 
  temporal_res_gen_cols <- 
    c("netgen")}
if (params$temporal_res == "monthly") { 
  temporal_res_gen_cols <- 
    c("netgen", 
      paste0("netgen_", tolower(month.name)))}

# use generator file to summarize generation by fuel type 
gen_by_fuel <- 
  eia_923$generation_and_fuel_combined %>% 
  left_join(xwalk_oris_epa %>% filter(!epa_plant_id %in% eia_923$generation_and_fuel_combined$plant_id), 
            by = c("plant_id" = "eia_plant_id")) %>% 
  rename("netgen" = net_generation_megawatthours) %>% 
  mutate(plant_id = if_else(!is.na(epa_plant_id), epa_plant_id, plant_id)) %>% 
  group_by(plant_id, fuel_type) %>% 
  summarize(# generation by plant and fuel type
            across(.cols = all_of(temporal_res_gen_cols), 
                   .fns = ~ if_else(all(is.na(.x)), NA_real_, 
                                    sum(.x, na.rm = TRUE)))) %>% 
  ungroup() %>% 
  group_by(plant_id) %>%
  mutate(# plant total generation
         across(.cols = all_of(temporal_res_gen_cols),
                .fns = ~ if_else(all(is.na(.x)), NA_real_, sum(.x, na.rm = TRUE)), 
                .names = "plant_{.col}"),
         # coal generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type %in% coal_fuels)], na.rm = TRUE)), 
                .names = "coal_{.col}"), 
         # oil generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type %in% oil_fuels)], na.rm = TRUE)), 
                .names = "oil_{.col}"), 
         # gas generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type %in% gas_fuels)], na.rm = TRUE)), 
                .names = "gas_{.col}"), 
         # nuclear generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type == "NUC")], na.rm = TRUE)), 
                .names = "nuclear_{.col}"), 
         # hydro generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type == "WAT")], na.rm = TRUE)), 
                .names = "hydro_{.col}"), 
         # biomass generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type %in% biomass_fuels)], na.rm = TRUE)), 
                .names = "biomass_{.col}"), 
         # wind generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type == "WND")], na.rm = TRUE)), 
                .names = "wind_{.col}"), 
         # solar generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type == "SUN")], na.rm = TRUE)), 
                .names = "solar_{.col}"), 
         # geothermal generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type == "GEO")], na.rm = TRUE)), 
                .names = "geothermal_{.col}"), 
         # other fossil fuel generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type %in% other_ff)], na.rm = TRUE)), 
                .names = "other_ff_{.col}"), 
         # other generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ if_else(is.na(.x), NA_real_, 
                                 sum(.x[which(fuel_type %in% other_fuels)], na.rm = TRUE)), 
                .names = "other_{.col}")) %>% 
  ungroup() %>% 
  select(-fuel_type) %>% 
  distinct() 

## Calculate resource mix generation by fuel type and % resource mix by fuel type ------------

# calculate generation by non-renewables, renewables (and non-hydro renewables), combustion fuels, and non-combustion fuels
gen_by_fuel_2 <- 
  gen_by_fuel %>% 
  mutate(# non-renewable generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ get(paste0("coal_", cur_column())) + 
                         get(paste0("oil_", cur_column())) + 
                         get(paste0("gas_", cur_column())) + 
                         get(paste0("other_ff_", cur_column())) +
                         get(paste0("nuclear_", cur_column())), 
                .names = "nonrenew_{.col}"),
         # renewable generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ get(paste0("biomass_", cur_column())) + 
                         get(paste0("wind_", cur_column())) + 
                         get(paste0("solar_", cur_column())) + 
                         get(paste0("geothermal_", cur_column())) +
                         get(paste0("hydro_", cur_column())), 
                .names = "renew_{.col}"),
         # renewable non-hydro generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ get(paste0("biomass_", cur_column())) + 
                         get(paste0("wind_", cur_column())) + 
                         get(paste0("solar_", cur_column())) + 
                         get(paste0("geothermal_", cur_column())), 
                .names = "renew_nonhydro_{.col}"),
         # non-renewable other generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ get(paste0("other_", cur_column())), 
                .names = "nonrenew_other_{.col}"), 
         # combustion generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ get(paste0("coal_", cur_column())) + 
                         get(paste0("oil_", cur_column())) + 
                         get(paste0("gas_", cur_column())) + 
                         get(paste0("other_ff_", cur_column())) + 
                         get(paste0("biomass_", cur_column())), 
                .names = "combust_{.col}"),
         # non-combustion generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ get(paste0("nuclear_", cur_column())) + 
                  get(paste0("wind_", cur_column())) + 
                  get(paste0("solar_", cur_column())) + 
                  get(paste0("geothermal_", cur_column())) + 
                  get(paste0("hydro_", cur_column())), 
                .names = "noncombust_{.col}"),
         # non-combustion other generation
         across(.cols = all_of(temporal_res_gen_cols), 
                .fns = ~ get(paste0("other_", cur_column())), 
                .names = "noncombust_other_{.col}")) %>% 
  select(-starts_with("netgen")) %>% 
  distinct()

gen_by_fuel_3 <- # calculate resource mix across each fuel type
  gen_by_fuel_2 %>% 
  mutate(# coal percentage of resource mix
         across(.cols = starts_with("coal_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "coal", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "coal", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # oil percentage of resource mix
         across(.cols = starts_with("oil_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "oil", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "oil", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # gas percentage of resource mix
         across(.cols = starts_with("gas_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "gas", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "gas", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # nuclear percentage of resource mix
         across(.cols = starts_with("nuclear_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "nuclear", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "nuclear", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # hydro percentage of resource mix
         across(.cols = starts_with("hydro_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "hydro", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "hydro", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # biomass percentage of resource mix
         across(.cols = starts_with("biomass_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "biomass", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "biomass", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # wind percentage of resource mix
         across(.cols = starts_with("wind_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "wind", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "wind", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # solar percentage of resource mix
         across(.cols = starts_with("solar_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "solar", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "solar", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # geothermal percentage of resource mix
         across(.cols = starts_with("geothermal_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "geothermal", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "geothermal", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # other fossil fuel percentage of resource mix
         across(.cols = starts_with("other_ff_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "other_ff", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "other_ff", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # other percentage of resource mix
         across(.cols = starts_with("other_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "other", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "other", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # non-renewable percentage of resource mix
         across(.cols = starts_with("nonrenew_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "nonrenew", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "nonrenew", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # renewable percentage of resource mix
         across(.cols = starts_with("renew_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "renew", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "renew", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # renewable non-hydro percentage of resource mix
         across(.cols = starts_with("renew_nonhydro_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "renew_nonhydro", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "renew_nonhydro", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # non-renewable other percentage of resource mix
         across(.cols = starts_with("nonrenew_other_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "nonrenew_other", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "nonrenew_other", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # combustion percentage of resource mix
         across(.cols = starts_with("combust_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "combust", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "combust", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # non-combustion percentage of resource mix
         across(.cols = starts_with("noncombust_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "noncombust", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "noncombust", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}"),
         # non-combustion other percentage of resource mix
         across(.cols = starts_with("noncombust_other_netgen"), 
                .fns = ~ if_else(get(str_replace(cur_column(), "noncombust_other", "plant")) != 0, 
                                 .x / get(str_replace(cur_column(), "noncombust_other", "plant")), NA_real_), 
                .names = "{str_replace(.col, 'netgen', 'perc_gen')}")) %>% 
  select(-contains("plant_netgen")) %>% 
  mutate(across(contains("perc"), 
                ~ if_else(.x < 0, 0, .x)))
  

# checks for negative generation values
stopifnot(sum(isTRUE(as.matrix(gen_by_fuel) < 0), na.rm = TRUE) == 0)

plant_file_16 <- 
  plant_file_15 %>% 
  left_join(gen_by_fuel_3, by = c("plant_id")) %>% 
  mutate(primary_fuel_type = if_else(nuclear_perc_gen > 0.50 & !is.na(nuclear_perc_gen), "NUC", primary_fuel_type),
         primary_fuel_category = if_else(nuclear_perc_gen > 0.50 & !is.na(nuclear_perc_gen), "NUCLEAR", primary_fuel_category), 
         across(.cols = contains("netgen"), # fill values with 0 if annual generation is not NA
                .fns = ~ if_else(is.na(.x) & !is.na(generation_ann), 0, .x))) %>% 
  rename("generation" = generation_ann) %>% 
  rowwise() %>% 
  mutate(# recalculate generation if the difference between generation and fuel type generation is > 2
         across(.cols = c(starts_with("generation"), -generation_oz), 
                .fns = ~ if_else(abs(.x - sum(get(str_replace(cur_column(), "generation", "coal_netgen")), 
                                              get(str_replace(cur_column(), "generation", "oil_netgen")),
                                              get(str_replace(cur_column(), "generation", "gas_netgen")),
                                              get(str_replace(cur_column(), "generation", "nuclear_netgen")),
                                              get(str_replace(cur_column(), "generation", "hydro_netgen")), 
                                              get(str_replace(cur_column(), "generation", "biomass_netgen")),
                                              get(str_replace(cur_column(), "generation", "wind_netgen")), 
                                              get(str_replace(cur_column(), "generation", "solar_netgen")), 
                                              get(str_replace(cur_column(), "generation", "geothermal_netgen")), 
                                              get(str_replace(cur_column(), "generation", "other_ff_netgen")), 
                                              get(str_replace(cur_column(), "generation", "other_netgen")), 
                                              na.rm = TRUE)) > 2, 
                                 sum(get(str_replace(cur_column(), "generation", "coal_netgen")), 
                                     get(str_replace(cur_column(), "generation", "oil_netgen")),
                                     get(str_replace(cur_column(), "generation", "gas_netgen")),
                                     get(str_replace(cur_column(), "generation", "nuclear_netgen")),
                                     get(str_replace(cur_column(), "generation", "hydro_netgen")), 
                                     get(str_replace(cur_column(), "generation", "biomass_netgen")),
                                     get(str_replace(cur_column(), "generation", "wind_netgen")), 
                                     get(str_replace(cur_column(), "generation", "solar_netgen")), 
                                     get(str_replace(cur_column(), "generation", "geothermal_netgen")), 
                                     get(str_replace(cur_column(), "generation", "other_ff_netgen")), 
                                     get(str_replace(cur_column(), "generation", "other_netgen")), 
                                     na.rm = TRUE), .x))) %>% ungroup() %>% 
  rename("generation_ann" = generation)
      

# CHP plants ---------------

# Some plants are designated as Combined Heat and Power (CHP) plants
# These plants produce electricity as well as useful thermal energy (such as heat or steam) for industrial, commercial, heating or cooling
# Emissions from eGRID only represent those from electricity generation, 
# so we adjust for emissions created from CHP plants that are used for other purposes than electricity

## Calculate useful thermal output, power heat ratio , and electric allocation factor ---------------------

# Useful thermal output is the fuel consumption in a plant that contributes to non-electricity activities
# Electricity allocation is a ratio of emissions that are attributed to electricity
# Power to heat ratio is is the ratio of heat value of electricity genreation to the facility's useful thermal output

if (params$temporal_res == "annual") { 
  temporal_res_gen_fuel <- 
    c("total_fuel_consumption_mmbtu", 
      "elec_fuel_consumption_mmbtu")}
if (params$temporal_res == "monthly") { 
  temporal_res_gen_fuel <- 
    c("total_fuel_consumption_mmbtu", 
      "elec_fuel_consumption_mmbtu", 
      paste0("tot_mmbtu_", tolower(month.name)), 
      paste0("elec_mmbtu_", tolower(month.name)))}


# sum total fuel consumption and electric fuel consumption to the plant level
eia_923_thermal_output <- 
  eia_923$generation_and_fuel_combined %>% 
  group_by(plant_id) %>%
  summarize(#total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE),
            #elec_fuel_consumption_mmbtu = sum(elec_fuel_consumption_mmbtu, na.rm = TRUE), 
            across(.cols = all_of(temporal_res_gen_fuel), 
                   .fns = ~ sum(.x, na.rm = TRUE))) %>% ungroup()

chp_plants <- # identify CHP plants from EIA-860, EIA-923, EPA CHP database, and the previous eGRID year data
  eia_860$combined %>% 
  select(plant_id, eia_860_chp = associated_with_combined_heat_and_power_system) %>% 
  filter(eia_860_chp == "Y") %>% distinct() %>% 
  full_join(eia_923$generation_and_fuel_combined %>% 
              select(plant_id, eia_923_chp = combined_heat_and_power_plant) %>% 
              filter(eia_923_chp == "Y") %>% 
              distinct()) %>% 
  left_join(chp_database %>% 
              mutate(chp_database = "Y") %>% 
              distinct()) %>% 
  full_join(plant_prev_year) %>% 
  mutate(eia_860_chp = if_else(eia_860_chp == "Y" & !is.na(eia_860_chp), 1, 0), 
         eia_923_chp = if_else(eia_923_chp == "Y" & !is.na(eia_923_chp), 1, 0), 
         prev_egrid_chp = if_else(prev_egrid_chp == "Yes" & !is.na(prev_egrid_chp), 1, 0), 
         chp_database = if_else(chp_database == "Y" & !is.na(chp_database), 1, 0), 
         total = eia_860_chp + eia_923_chp + chp_database + prev_egrid_chp) %>% 
  filter(total > 0)
  
chp <- 
  chp_plants %>% 
  left_join(xwalk_oris_epa %>% filter(!epa_plant_id %in% chp_plants$plant_id), by = c("plant_id" = "eia_plant_id")) %>% 
  left_join(eia_923_thermal_output) %>%
  mutate( # calculate useful thermal output
    across(.cols = c(all_of(temporal_res_gen_fuel), -contains("elec")), 
           .fns = ~ 0.8 * (.x - get(str_replace_all(cur_column(), c("total" = "elec", "tot" = "elec")))), # 0.8 is the assumed efficiency factor from the combustion of the consumed fuel
           .names = "{str_replace_all(.col, c('tot_mmbtu' = 'useful_thermal_output', 
                                              'total_fuel_consumption_mmbtu' = 'useful_thermal_output'))}"), 
    chp_flag = "Yes", 
    plant_id = if_else(!is.na(epa_plant_id), epa_plant_id, plant_id)) %>% # assign CHP flags to all plants in this object
  select(plant_id, contains("useful_thermal_output"), chp_flag) %>% 
  left_join(plant_file_14 %>% select(plant_id, starts_with("generation"), "generation" = generation_ann), by = c("plant_id")) %>% 
  mutate(# calculate power to heat ratio
         across(.cols = starts_with("useful_thermal_output"), 
                .fns = ~ if_else(.x == 0 | is.na(.x), 
                                 NA_real_, 
                                 3.413 * get(str_replace(cur_column(), "useful_thermal_output", "generation"))), # 3.413 converts MWh to MMBtu
                .names = "{str_replace(.col, 'useful_thermal_output', 'power_heat_ratio')}"),
         # calculate electric allocation
         across(.cols = starts_with("useful_thermal_output"), 
                .fns = ~ if_else(.x != 0, 
                                 3.413 *  get(str_replace(cur_column(), "useful_thermal_output", "generation")) / 
                                   (0.75 * .x + 3.413 * get(str_replace(cur_column(), "useful_thermal_output", "generation"))), # 0.75 is an efficiency factor that accounts for once the fuel is combusted, about 75% of useful thermal output can be used for other purposes
                                 1), 
                .names = "{str_replace(.col, 'useful_thermal_output', 'elec_allocation')}"), 
         # check and re-assign electric allocation if it is less than 0 or greater than 1
         across(.cols = starts_with("elec_allocation"), 
                .fns = ~ if_else(.x < 0, 0, .x)), 
         across(.cols = starts_with("elec_allocation"), 
                .fns = ~ if_else(.x > 1, 1, .x))) %>% 
  select(-starts_with("generation"))

plant_file_17 <- 
  plant_file_16 %>% 
  left_join(chp, by = c("plant_id")) 


## CHP adjustment  ------------------

plant_chp <- 
  plant_file_17 %>% 
  filter(chp_flag == "Yes") %>% 
  # rename adjusted emission masses to include biomass adjustments, since we are now applying CHP adjustments
  rename_with(.fn = function(.x){paste0(.x, "_bio_adj")}, 
              c(contains("mass"), -contains("unadj"), -contains("bio"), -contains("hg"))) %>% 
  mutate(elec_allocation_oz = elec_allocation, # assign ozone elec allocation to be the same
         # calculate adjusted values 
         # calculate adjusted heat input 
         across(.cols = c(contains("unadj_heat_input"), -contains("combust"), -contains("source")), 
                .fns = ~ (get(str_replace(cur_column(), "unadj_heat_input", "elec_allocation")) * get(str_replace(cur_column(), "unadj_heat_input", "unadj_combust_heat_input"))) + 
                          (.x - get(str_replace(cur_column(), "unadj_heat_input", "unadj_combust_heat_input"))), 
                .names = "{str_replace(.col, 'unadj_', '')}"), 
         # calculate adjusted combust heat input
         across(.cols = contains("unadj_combust_heat_input"), 
                .fns = ~ get(str_replace(cur_column(), "unadj_combust_heat_input", "elec_allocation")) * .x, 
                .names = "{str_replace(.col, 'unadj_', '')}"), 
         # calculate adjusted emission mass 
         across(.cols = contains("bio_adj"), 
                .fns = ~ .x * get(str_replace_all(cur_column(), c("nox_mass" = "elec_allocation",
                                                                  "nox_oz_mass" = "elec_allocation", 
                                                                  "so2_mass" = "elec_allocation", 
                                                                  "co2e_mass" = "elec_allocation", 
                                                                  "co2_mass" = "elec_allocation", 
                                                                  "n2o_mass" = "elec_allocation", 
                                                                  "ch4_mass" = "elec_allocation", 
                                                                  "_bio_adj" = ""))), 
                .names = "{str_replace(.col, '_bio_adj', '')}"),
         # calculate CHP adjustment values
         # CHP adjustement combust heat input
         across(.cols = c(contains("unadj_combust_heat_input"), -contains("source")), 
                .fns = ~ .x - get(str_replace(cur_column(), "unadj_combust_heat_input", "combust_heat_input")),
                .names = "{str_replace(.col, 'unadj', 'chp')}"), 
         # CHP adjusment emissions mass
         across(.cols = contains("bio_adj"), 
                .fns = ~ .x - get(str_replace(cur_column(), "_bio_adj", "")), 
                .names = "chp_{str_replace(.col, '_bio_adj', '')}"), 
         # check if CHP emission masses are greater than unadjusted values, and assign unadjusted values if TRUE
         across(.cols = c(starts_with("chp"), -contains("heat_input"), -contains("flag")), 
                .fns = ~ if_else(.x > get(str_replace(cur_column(), "chp", "unadj")) | 
                                   .x < 0, get(str_replace(cur_column(), "chp", "unadj")), .x))) %>% 
  select(-contains("bio_adj"))

plant_file_18 <- 
  plant_file_17 %>% 
  filter(is.na(chp_flag)) %>% # filter out CHP flags to easily join in new CHP data
  full_join(plant_chp) %>% 
  mutate(# assign adjusted values to non-CHP plants 
         across(.cols = c(starts_with("heat_input"), starts_with("combust_heat_input")), 
                .fns = ~ if_else(is.na(.x), get(paste0("unadj_", cur_column())), .x)), 
         # calculate nominal heat rate
         across(.cols = starts_with("combust_netgen"), 
                .fns = ~ if_else((combust_flag == 1 | combust_flag == 0.5) & !is.na(.x) & .x != 0, 
                                 get(str_replace(cur_column(), "netgen", "heat_input")) * 1000 / .x, 
                                 NA_real_), 
                .names = "{str_replace(.col, 'combust_netgen', 'nominal_heat_rate')}"))


# Update negative emissions values ----------------------------------------

plant_file_19 <- 
  plant_file_18 %>% 
  mutate(# if emission mass is less than 0, re-assign as 0 
         across(.cols = c(contains("_mass"), -contains("unadj")), 
                .fns = ~ if_else(.x < 0, 0, .x)))


# Calculate emissions rates  --------------------
### Combustion output emissions rates ----------------------------------------

if (params$temporal_res == "annual") { 
  temporal_res_emission_cols <-   
    c("_mass")}
if (params$temporal_res == "monthly") { 
  temporal_res_emission_cols <- 
    c("_mass", 
      paste0("_mass_", tolower(month.name)))}

plant_file_20 <- 
  plant_file_19 %>% 
  mutate(# calculate NOx, SO2, and CO2 combustion output rate
         across(.cols = c(paste0("nox", temporal_res_emission_cols),
                          paste0("so2", temporal_res_emission_cols),
                          paste0("co2", temporal_res_emission_cols)), 
                .fns = ~ if_else(get(paste0("combust_netgen", str_sub(cur_column(), 9))) <= 0 | 
                                   is.na(get(paste0("combust_netgen", str_sub(cur_column(), 9)))), NA_real_, 
                                 2000 * .x / get(paste0("combust_netgen", str_sub(cur_column(), 9)))), 
                .names = "{str_replace(.col, 'mass', 'combust_out_rate')}"), 
         # calculate NOx ozone combustion output rate
         nox_oz_combust_out_rate = if_else(combust_netgen * (generation_oz / generation_ann) <= 0 | is.na(combust_netgen), 
                                                    NA_real_, 2000 * nox_oz_mass / (combust_netgen * (generation_oz / generation_ann))),
         # calculate CH4 and N2O combustion output rate
         across(.cols = c(paste0("ch4", temporal_res_emission_cols),
                          paste0("n2o", temporal_res_emission_cols)), 
                .fns = ~ if_else(get(paste0("combust_netgen", str_sub(cur_column(), 9))) <= 0 | 
                                   is.na(get(paste0("combust_netgen", str_sub(cur_column(), 9)))), NA_real_, 
                                 .x / get(paste0("combust_netgen", str_sub(cur_column(), 9)))), 
                .names = "{str_replace(.col, 'mass', 'combust_out_rate')}"),
         # calculate CO2e combustion output rate
         across(.cols = paste0("co2e", temporal_res_emission_cols), 
                .fns = ~ if_else(get(paste0("combust_netgen", str_sub(cur_column(), 10))) <= 0 | 
                                   is.na(get(paste0("combust_netgen", str_sub(cur_column(), 10)))), NA_real_, 
                                 2000 * .x / get(paste0("combust_netgen", str_sub(cur_column(), 10)))), 
                .names = "{str_replace(.col, 'mass', 'combust_out_rate')}"),
         # assign NAs to Hg combustion output rate
         across(.cols = paste0("hg", temporal_res_emission_cols), 
                .fns = ~ NA_real_, 
                .names = "{str_replace(.col, 'mass', 'combust_out_rate')}"))

### Input emission rates ----------------------------------------
  
plant_file_21 <- 
  plant_file_20 %>% 
  mutate(# calculate NOx, SO2, and CO2 input rate
         across(.cols = c(paste0("nox", temporal_res_emission_cols),
                          paste0("so2", temporal_res_emission_cols),
                          paste0("co2", temporal_res_emission_cols)), 
                .fns = ~ if_else(get(paste0("heat_input", str_sub(cur_column(), 9))) <= 0 | 
                                   is.na(get(paste0("heat_input", str_sub(cur_column(), 9)))), NA_real_, 
                                 2000 * .x / get(paste0("heat_input", str_sub(cur_column(), 9)))), 
                .names = "{str_replace(.col, 'mass', 'input_rate')}"),
         # calculate NOx ozone input rate
         nox_oz_input_rate = if_else(heat_input_oz <= 0 | is.na(heat_input_oz), NA_real_, 
                                           2000 * nox_oz_mass / heat_input_oz),
         # calculate CO2e input rate
         across(.cols = paste0("co2e", temporal_res_emission_cols), 
                .fns = ~ if_else(get(paste0("heat_input", str_sub(cur_column(), 10))) <= 0 | 
                                   is.na(get(paste0("heat_input", str_sub(cur_column(), 10)))), NA_real_, 
                                 2000 * .x / get(paste0("heat_input", str_sub(cur_column(), 10)))), 
                .names = "{str_replace(.col, 'mass', 'input_rate')}"),
         # calculate CH4 and N2O input rate
         across(.cols = c(paste0("ch4", temporal_res_emission_cols),
                          paste0("n2o", temporal_res_emission_cols)), 
                .fns = ~ if_else(get(paste0("heat_input", str_sub(cur_column(), 9))) <= 0 | 
                                   is.na(get(paste0("heat_input", str_sub(cur_column(), 9)))), NA_real_, 
                                 .x / get(paste0("heat_input", str_sub(cur_column(), 9)))), 
                .names = "{str_replace(.col, 'mass', 'input_rate')}"),
         # assign NAs to Hg input rate
         across(.cols = paste0("hg", temporal_res_emission_cols), 
                .fns = ~ NA_real_, 
                .names = "{str_replace(.col, 'mass', 'input_rate')}"))

### Output emission rates ----------------------------------------

plant_file_22 <- 
  plant_file_21 %>% rename("generation" = generation_ann) %>% 
  mutate(# calculate NOx, SO2, and CO2 output rate
         across(.cols = c(paste0("nox", temporal_res_emission_cols),
                          paste0("so2", temporal_res_emission_cols),
                          paste0("co2", temporal_res_emission_cols)), 
                .fns = ~ if_else(get(paste0("generation", str_sub(cur_column(), 9))) <= 0 | 
                                   is.na(get(paste0("generation", str_sub(cur_column(), 9)))), NA_real_, 
                                 2000 * .x / get(paste0("generation", str_sub(cur_column(), 9)))), 
                .names = "{str_replace(.col, 'mass', 'output_rate')}"),
         # calculate NOx ozone output rate
         nox_oz_output_rate = if_else(generation_oz <= 0 | is.na(generation_oz), NA_real_, 
                                           2000 * nox_oz_mass / generation_oz),
         # calculate CO2e output rate
         across(.cols = paste0("co2e", temporal_res_emission_cols), 
                .fns = ~ if_else(get(paste0("generation", str_sub(cur_column(), 10))) <= 0 | 
                                   is.na(get(paste0("generation", str_sub(cur_column(), 10)))), NA_real_, 
                                 2000* .x / get(paste0("generation", str_sub(cur_column(), 10)))), 
                .names = "{str_replace(.col, 'mass', 'output_rate')}"),
         # calculate CH4 and N2O output rate
         across(.cols = c(paste0("ch4", temporal_res_emission_cols),
                          paste0("n2o", temporal_res_emission_cols)), 
                .fns = ~ if_else(get(paste0("generation", str_sub(cur_column(), 9))) <= 0 | 
                                   is.na(get(paste0("generation", str_sub(cur_column(), 9)))), NA_real_, 
                                 .x / get(paste0("generation", str_sub(cur_column(), 9)))), 
                .names = "{str_replace(.col, 'mass', 'output_rate')}"),
         # assign NAs to Hg input rate
         across(.cols = paste0("hg", temporal_res_emission_cols), 
                .fns = ~ NA_real_, 
                .names = "{str_replace(.col, 'mass', 'output_rate')}")) %>% 
  rename("generation_ann" = generation)

# Calculate nonbaseload factor and generation ---------------------------------

# we calculate a plant's nonbaseload factor based on their capacity factors
# some fuel types are excluded from this 
plant_file_23 <- 
  plant_file_22 %>% 
  mutate(across(.cols = starts_with("capfac"), 
                .fns = ~ if_else(!primary_fuel_type %in% c("GEO", "MWH", "NUC", "PUR", "SUN", "WAT", "WND"), 
                                 case_when(.x > 0 & .x < 0.2 ~ 1,
                                           .x > 0.8 ~ 0,
                                           .x == 0 ~ NA_real_,
                                           TRUE ~ (-5/3 * .x) + 4/3),
                               NA_real_), 
                .names = "{str_replace(.col, 'capfac', 'nonbaseload')}"), 
         across(.cols = c(starts_with("generation"), -generation_oz),
                .fns = ~ .x * get(str_replace_all(cur_column(), c("_ann" = "", "generation" = "nonbaseload"))),  # calculate nonbaseload generation for each plant and month
                .names = "{str_replace_all(.col, c('_ann' = '', 'generation' = 'generation_nonbaseload'))}"))

# Update source columns ------------------------------

# list of allowed sources to aid in checking that all sources are updated correctly
check_source_list <- c("EIA", "EPA/CAPD", NA_character_, "EPA/CAPD; EIA")

### Update heat_input_source ----------

unit_source_heat_input <- 
  update_source(x = "heat_input_source", unit_f = unit_file) %>% 
  rename(unadj_heat_input_source = heat_input_source) # rename to match plant file
  
# check if sources updated correctly, stop if not
stopifnot(all(unit_source_heat_input$unadj_heat_input_source %in% check_source_list))


### Update heat_input_oz_source ----------

unit_source_heat_input_oz <- 
  update_source(x = "heat_input_oz_source", unit_f = unit_file) %>% 
  rename(unadj_heat_input_oz_source = heat_input_oz_source) # rename to match plant file

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_heat_input_oz$unadj_heat_input_oz_source %in% check_source_list))


### Update nox_source ---------

unit_source_nox <- update_source(x = "nox_source", unit_f = unit_file) %>% 
  rename(unadj_nox_source = nox_source)

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_nox$unadj_nox_source %in% check_source_list))


### Update nox_oz_source --------

unit_source_nox_oz <- 
  update_source(x = "nox_oz_source", unit_f = unit_file) %>% 
  rename(unadj_nox_oz_source = nox_oz_source) # rename to match plant file

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_nox_oz$unadj_nox_oz_source %in% check_source_list))


### Update co2_source ---------

unit_source_co2 <- 
  update_source(x = "co2_source", unit_f = unit_file) %>% 
  rename(unadj_co2_source = co2_source) # rename to match plant file

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_co2$unadj_co2_source %in% check_source_list))


### Update so2_source ----------

unit_source_so2 <- 
  update_source(x = "so2_source", unit_f = unit_file) %>% 
  rename(unadj_so2_source = so2_source) # rename to match plant file

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_so2$unadj_so2_source %in% check_source_list))

### Update plant file with source updates -------------

plant_file_24 <- 
  plant_file_23 %>% 
  rows_update(unit_source_heat_input, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_heat_input_oz, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_nox, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_nox_oz, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_co2, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_so2, by = c("plant_id"), unmatched = "ignore") %>% 
  mutate(unadj_ch4_source = if_else(!is.na(ch4_mass), "EIA", NA_character_), 
         unadj_n2o_source = if_else(!is.na(n2o_mass), "EIA", NA_character_))

# Final modifications --------------------
### Round columns -------------------------

plant_file_25 <- 
  plant_file_24 %>% 
  mutate(capfac = round(capfac, 5), # capacity factor
         # heat input (unadjusted and adjusted)
         across(contains("combust_heat_input"), ~ round(.x, 3)), 
         across(c(contains("heat_input"), -contains("source")), ~ round(.x, 3)),
         # generation
         across(starts_with("generation"), ~ round(.x, 3)), 
         # emissions mass
         across(contains("_mass"), ~ round(.x, 3)),
         # output emissions rates
         across(contains("output_rate"), ~ round(.x, 3)), 
         # input emissions rates
         across(contains("input_rate"), ~ round(.x, 3)),
         # combustion output emissions rates
         across(contains("combust_out_rate"), ~ round(.x, 3)),
         # nominal heat rate
         nominal_heat_rate = round(nominal_heat_rate, 6),
         # annual generation (by fuel)
         across(contains("netgen"), ~ round(.x, 3)),
         # biomass 
         across(contains("_bio"), ~ round(.x, 3)),
         # CHP emission mass
         across(c(starts_with("chp"), -contains("flag")), ~ round(.x, 3)),
         # power to heat ratio, electricity allocation, and useful thermal output
         across(starts_with("power_heat_ratio"), ~ round(.x, 3)), 
         across(starts_with("elec_allocation"), ~ round(.x, 6)), 
         across(starts_with("useful_thermal_output"), ~ round(.x, 3))
         ) 


# Format plant file --------------

# creating named vector of final variable order and variable name included in plant file
if (params$temporal_res == "annual") {
  final_vars <-
    plant_nonmetric}
if (params$temporal_res == "monthly") { 
  final_vars <- 
    plant_nonmetric_monthly}

plant_formatted <-
  plant_file_25 %>%
  arrange(plant_state, plant_name) %>% 
  mutate(seqplt = row_number(), 
         year = params$eGRID_year) %>% 
  select(as_tibble(final_vars)$value) %>%  # keeping columns with snake_case names until final formatting
  filter(!is.na(plant_id))

# Export plant file -------------

# check if folders exist 
if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
} else {
   dir.create("data/outputs")
}

if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
  print("Folder output already exists.")
} else {
   dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
}

# write RDS file 
print(glue::glue("Saving unit file to folder data/outputs/{params$eGRID_year}"))

write_rds(plant_formatted, glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS"))

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS"))){
  print(glue::glue("File plant_file.RDS successfully written to folder data/outputs/{params$eGRID_year}"))
} else {
   print("File plant_file.RDS failed to write to folder.")
} 
