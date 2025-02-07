## -------------------------------
##
## PM unit file create
## 
## Purpose: 
## 
## This file creates the PM2.5 unit file for eGRID. 
## This file includes PM2.5 emission data, either calculated
## or estimated for the units of the specified eGRID year
## 
## The method of PM2.5 calculations are listed within pm25_source
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

# Load Libraries ---------
library(dplyr)
library(readr)
library(readxl)


# Define eGRID year parameter ----------------

# define parameter year is no one is currently assigned using prompted user input
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

# Load necessary data --------------------
## EIA-923 - for Schedule C Air Emissions Control information
# if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) { 
#   eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))
# } else { 
#   stop("eia_923_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}
eia_923 <- read_csv("data/raw_data/923/2023/eia_923_test.csv", col_types = "ccccccccddddcccdccddcdc") %>%
  janitor::clean_names()

## NEI PM2.5 data
if(file.exists(glue::glue("data/raw_data/nei/{params$eGRID_year}/nei_particulate_matter_emissions_raw.csv"))) { 
  pm_raw_nei <- read_csv(glue::glue("data/raw_data/nei/{params$eGRID_year}/nei_particulate_matter_emissions_raw.csv"), col_types = "cccccccccccccccccdcc") %>%
    janitor::clean_names()
} else { 
  stop("nei_particulate_matter_emissions_raw.csv does not exist.")}
  # access and r both have 355919 observations

## NEI-EIA crosswalk matching NEI and EIA unit ids
nei_eia_xwalk <- read_csv("data/static_tables/xwalk_nei_eia.csv", col_types = "cccccccccccccccccccc") %>%
  janitor::clean_names()

## Particulate matter emission factors from EPA AP-42 dataset
pm_efs <- read_csv("data/static_tables/emission_factors_particulate_matter.csv", col_types = "cccdccc") %>%
  janitor::clean_names()

## eGRID production model data - unit file
unit_file <- read_csv(glue::glue("data/outputs/{params$eGRID_year}/unit_file_2021_access.csv"), col_types = "ccccccccccicddddddddccccccccccc") %>%
  janitor::clean_names()
# unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))


# Calculate PM data -------------
## 1) Direct Match - "NEI/EIA" --------------

# calculate pm2.5 data using direct unit match from EIA to NEI
pm_direct_match <-
  pm_raw_nei %>%
  # match NEI (EIS codes) to eGRID (ORISPL codes) with crosswalk (QAQPS)
  left_join(nei_eia_xwalk, by = join_by(eis_facility_id, eis_unit_id)) %>% 
  select(oris_facility_code = left_side_of_alternate_id_to_become_oris_facility_code, oris_boiler_id = right_side_of_alternatve_id_to_become_oris_boiler_id, 
         pm25 = total_emissions, state = state.y,
         eis_facility_id, eis_unit_id, agency_unit_id) %>%
  group_by(oris_facility_code, oris_boiler_id) %>%
  # calculate the sum of pm emissions for each facility ID and boiler ID combination
  summarise(pm25 = sum(pm25)) %>%
  ungroup()

# create unit file with pm2.5 emission data
unit_pm_emissions <-
  unit_file %>%
  # combine direct match pm2.5 and unit file data
  left_join(pm_direct_match, by = join_by(unitid == oris_boiler_id, orispl == oris_facility_code)) %>% 
  # modify dataset format and add pm2.5 source for those calculated with direct match
  mutate(pm25_source = if_else(is.na(pm25), NA, "EPA/NEI"), eia_pm_control_efficiency = as.numeric(NA), botfirty = if_else(botfirty == "", NA, botfirty))


## 2) Match by fuel type, unit firing type, and prime mover - "NEI avg EF - PM, fuel type, firing type" ----------

# calculate pm2.5 emissions using matching of fuel type, prime mover, and firing type
pm_fuel_pmover_firing <-
  unit_pm_emissions %>%
  # filter to those that have a match with NEI
  filter(pm25_source == "EPA/NEI") %>% 
  # group by prime mover, firing, fuel type
  group_by(prmvr, botfirty, fuelu1, pm25_source) %>% 
  # calculate emission factor
  summarise(sum_heat_input = sum(htian), sum_pm25 = sum(pm25)) %>%
  mutate(emission_factors = sum_pm25 / sum_heat_input) %>% 
  inner_join(unit_pm_emissions, by = join_by(prmvr, botfirty, fuelu1)) %>%
  # multiply individual heat inputs by emission factors to estimate pm2.5
  # define method used under source
  mutate(pm25 = emission_factors * htian, pm25_source = "NEI avg EF - PM, fuel type, firing type") %>% 
  ungroup() %>%
  select(orispl, unitid, prmvr, pm25, pm25_source)


## 3) Match by fuel type and prime mover - "NEI avg EF - PM, fuel type" ----------

# calculate pm2.5 emissions using matching of prime mover and fuel type
pm_fuel_pmover <-
  unit_pm_emissions %>%
  # filter to those that have a match with NEI
  filter(pm25_source == "EPA/NEI") %>% 
  # group by prime mover, fuel type
  group_by(prmvr, fuelu1, pm25_source) %>%
  # calculate emission factor
  summarise(sum_heat_input = sum(htian), sum_pm25 = sum(pm25)) %>%
  mutate(emission_factors = sum_pm25 / sum_heat_input) %>% 
  inner_join(unit_pm_emissions, by = join_by(prmvr, fuelu1)) %>%
  # multiply individual heat inputs by emission factors to estimate pm2.5
  # define method used under source
  mutate(pm25 = emission_factors * htian, pm25_source = "NEI avg EF - PM fuel type") %>%
  ungroup() %>%
  select(orispl, unitid, prmvr, pm25, pm25_source)
  

## 4) Use emissions factors from AP-42 - "Estimated using an emission factor" ---------
  
# calculate pm2.5 emissions based on emission factors in AP-42 report
pm_emission_factors <-
  unit_pm_emissions %>%
  # use emissions factors specific to fuel, firing type, and prime mover to calculate pm2.5
  inner_join(pm_efs, by = join_by(botfirty, fuelu1, prmvr)) %>%
  mutate(pm25 = ef * htian / 2000, pm25_source = "Estimated using an emission factor") %>% 
  filter(!is.na(pm25)) %>%
  select(orispl, unitid, prmvr, pm25, pm25_source)

# if there is a unit match with EIA-923, adjust pm2.5 by control efficiency
pm_removal_efficiencies <-
  eia_923 %>%
  # select plants with removal efficiency rates
  filter(!is.na(pm_removal_efficiency_rate_at_annual_operating_factor)) %>%
  group_by(plant_id) %>%
  # convert efficiency rate to numeric percentage
  summarise(eia_pm_control_efficiency = as.numeric(sub("%", "", max(pm_removal_efficiency_rate_at_annual_operating_factor)))) %>% 
  inner_join(pm_emission_factors, by = join_by(plant_id == orispl)) %>%
  rename(orispl = plant_id) %>%
  # adjust pm2.5 using control efficiency rate
  mutate(pm25 = pm25 * (100 - eia_pm_control_efficiency))


# Add pm2.5 emission estimates to unit data -------------

# update unit file with pm2.5 emission rates from each method - order specific
unit_pm_emissions_updated <-
  unit_pm_emissions %>%
  rows_patch(pm_fuel_pmover_firing, by = c("unitid", "orispl")) %>%
  rows_patch(pm_fuel_pmover, by = c("unitid", "orispl")) %>%
  rows_patch(pm_removal_efficiencies, by = c("unitid", "orispl", "prmvr")) %>%
  rows_patch(pm_emission_factors, by = c("unitid", "orispl", "prmvr"))

# format final version of pm2.5 unit file ------------

#adjust pm2.5 emissions for renewable fuel types and select desired columns
unit_pm_emissions_final <-
  unit_pm_emissions_updated %>%
  # set pm2.5 emissions to NA for renewable fuel types
  mutate(pm25an = if_else(fuelu1 %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA, pm25), 
         # set pm2.5 source type to NA for renewable fuel types
         pm25src2 = if_else(pm25an >= 0, pm25_source, NA), 
         # add data column with adjusted pm2.5 rate
         pm25rt = pm25an * 2000 / htian) %>%
  # select desired variables for final version
  select(pstatabb, pname, orispl, unitid, prmvr, untopst, botfirty, fuelu1, hrsop, htian, pm25an, pm25rt, htiansrc, pm25src2, untyronl)
