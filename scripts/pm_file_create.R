## -------------------------------
##
## Name
## 
## Purpose: 
## 
## Purpose
##
## Additional notes
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load Libraries ---------
library(dplyr)
library(readr)
library(readxl)


# Define eGRID year parameter ----------------
params <- list()
params$eGRID_year <- as.character(2023)
# if (exists("params")) {
#   if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
#     print("eGRID year parameter is already defined.") 
#   } else { # if params() is defined, but eGRID_year is not, define it here 
#     params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
#     params$eGRID_year <- as.character(params$eGRID_year) 
#   }
# } else { # if params() and eGRID_year are not defined, define them here
#   params <- list()
#   params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
#   params$eGRID_year <- as.character(params$eGRID_year)
# }


# Load necessary data --------------------
## EIA-923
# if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) { 
#   eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))
# } else { 
#   stop("eia_923_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}
eia_923 <- read_csv("data/raw_data/923/2023/eia_923_test.csv") %>%
  janitor::clean_names()

## NEI PM data
if(file.exists(glue::glue("data/raw_data/nei/{params$eGRID_year}/nei_particulate_matter_emissions_raw.csv"))) { 
  pm_raw_nei <- read_csv(glue::glue("data/raw_data/nei/{params$eGRID_year}/nei_particulate_matter_emissions_raw.csv")) %>%
    janitor::clean_names()
} else { 
  stop("nei_particulate_matter_emissions_raw.csv does not exist.")}
  # access and r both have 355919 observations

## NEI-EIA crosswalk
nei_eia_xwalk <- read_csv("data/static_tables/xwalk_nei_eia.csv") %>% #, #col_types = "cccccccccccccccccccc") 
  janitor::clean_names()
  # both are 5310 observations

## Particulate matter emission factors
pm_efs <- read_csv("data/static_tables/emission_factors_particulate_matter.csv") %>%
  janitor::clean_names()

## eGRID production model data
unit_file <- read_xlsx(glue::glue("data/outputs/{params$eGRID_year}/unit_file_2021_access.xlsx")) %>%
  janitor::clean_names()
  # both have 24597 observations, 1819 missing htian values

# unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))


# Calculate PM data -------------
## 1) Direct Match - "NEI/EIA" --------------
  ## Queries 000, 00, 01a, 01b, 2
  ## Match NEI (EIS codes) to eGRID (ORISPL codes) with crosswalk (QAQPS)
  ## Calculate the sum of pm emissions for each facility ID and boiler ID combination

pm_direct_match <-
  pm_raw_nei %>%
  left_join(nei_eia_xwalk, by = join_by(eis_facility_id, eis_unit_id)) %>% # match PM NEI data to EPA units
  select(oris_facility_code = left_side_of_alternate_id_to_become_oris_facility_code, oris_boiler_id = right_side_of_alternatve_id_to_become_oris_boiler_id, 
         pm25 = total_emissions, state = state.y,
         eis_facility_id, eis_unit_id, agency_unit_id) %>%
  group_by(oris_facility_code, oris_boiler_id) %>%
  summarise(pm25 = sum(pm25)) %>%  # calculate sum of pm2.5 by unit and plant id combinations
  ungroup()

unit_pm_emissions <- # 3582 matches, now 3582 rows have pm data (same as access)
  unit_file %>%
  left_join(pm_direct_match, by = join_by(unitid == oris_boiler_id, orispl == oris_facility_code)) %>% # match pm2.5 data to unit file
  mutate(pm25_source = if_else(is.na(pm25), NA, "EPA/NEI"), eia_pm_control_efficiency = as.numeric(NA), botfirty = if_else(botfirty == "", NA, botfirty)) # set format for unit file and define pm2.5 source as direct match


## 2) Match by fuel type, unit firing type, and prime mover - "NEI avg EF - PM, fuel type, firing type" ----------
  ## Queries 03a, 03b, 03c
  ## Group EPA/NEI-matched units (3582) by fuel type, firing type, and prime mover (59 combinations)
  ## Calculate the heat input and pm2.5 sums for groups
  ## Calculate emission factors from sum (EF = pm2.5 sum / heat input sum)
  ## Multiply individual heat inputs by emission factors to estimate pm2.5

pm_fuel_pmover_firing <-
  unit_pm_emissions %>%
  filter(pm25_source == "EPA/NEI") %>%
  # filter to those that have a match with NEI
  group_by(prmvr, botfirty, fuelu1, pm25_source) %>% # group by prime mover, firing, fuel type
  summarise(sum_heat_input = sum(htian), sum_pm25 = sum(pm25)) %>%
  mutate(emission_factors = sum_pm25 / sum_heat_input) %>% # calculate emission factor
  inner_join(unit_pm_emissions, by = join_by(prmvr, botfirty, fuelu1)) %>%
  mutate(pm25 = emission_factors * htian, pm25_source = "NEI avg EF - PM, fuel type, firing type") %>%
  ungroup() %>%
  select(orispl, unitid, prmvr, pm25, pm25_source)


## 3) Match by fuel type and prime mover - "NEI avg EF - PM, fuel type" ----------
  ## Queries 04a, 04b, 04c
  ## Group EPA/NEI-matched units (3582) by fuel type and prime mover (25 combinations)
  ## Calculate the heat input and pm2.5 sums for groups
  ## Calculate emission factors from sum (EF = pm2.5 sum / heat input sum)
  ## Multiply individual heat inputs by emission factors to estimate pm2.5

pm_fuel_pmover <-
  unit_pm_emissions %>%
  filter(pm25_source == "EPA/NEI") %>%
  group_by(prmvr, fuelu1, pm25_source) %>%
  summarise(sum_heat_input = sum(htian), sum_pm25 = sum(pm25)) %>%
  mutate(emission_factors = sum_pm25 / sum_heat_input) %>%
  inner_join(unit_pm_emissions, by = join_by(prmvr, fuelu1)) %>%
  mutate(pm25 = emission_factors * htian, pm25_source = "NEI avg EF - PM fuel type") %>%
  ungroup() %>%
  select(orispl, unitid, prmvr, pm25, pm25_source)
  

## 4) Use emissions factors from AP-42 - "Estimated using an emission factor" ---------
  ## Queries 05, 06, 07
  ## Use emissions factors specific to fuel, firing type, and prime mover from AP-42 dataset
  ## calculate the pm2.5 as the emissions factor * heat input / 2000
  ## If there is a unit match with EIA-923, we can adjust pm2.5 by control efficiency

pm_emission_factors <- # query 05
  unit_pm_emissions %>%
  # filter(is.na(pm25_source) & is.na(pm25) | pm25_source == "" & !is.na(htian)) %>%
  inner_join(pm_efs, by = join_by(botfirty, fuelu1, prmvr)) %>%
  mutate(pm25 = ef * htian / 2000, pm25_source = "Estimated using an emission factor") %>%
  filter(!is.na(pm25)) %>%
  select(orispl, unitid, prmvr, pm25, pm25_source)

removal_efficiencies <- # query 06, correct at 488 observations
  eia_923 %>%
  filter(!is.na(pm_removal_efficiency_rate_at_annual_operating_factor)) %>%
  group_by(plant_id) %>%
    summarise(pm_removal = as.numeric(sub("%", "", max(pm_removal_efficiency_rate_at_annual_operating_factor))))
  #plant id, removal efficiencies

## apply pm removal efficiency (query 07)
# we have matches to almost all (1348/1400)
# pm_emission_factors_corected <-
#   pm_emission_factors %>% #filters to estimated using an emission factor
#   left_join(removal_efficiencies, by = join_by(orispl == plant_id)) %>%
#   mutate(pm25 = if_else(!is.na(pm_removal), pm25 * (100 - pm_removal), pm25)) %>%
#   rename(eia_pm_control_efficiency = pm_removal)

pm_removal_efficiencies <- # query 07
  pm_emission_factors %>%
  inner_join(removal_efficiencies, by = join_by(orispl == plant_id)) %>%
  mutate(pm25 = pm25 * (100 - pm_removal)) %>%
  rename(eia_pm_control_efficiency = pm_removal)


# Add pm2.5 emission estimates to unit data -------------
  ## Queries 03d, 04d, 

unit_pm_emissions_updated <-
  unit_pm_emissions %>%
  rows_patch(pm_fuel_pmover_firing, by = c("unitid", "orispl")) %>%
  rows_patch(pm_fuel_pmover, by = c("unitid", "orispl")) %>%
  rows_patch(pm_removal_efficiencies, by = c("unitid", "orispl", "prmvr")) %>%
  rows_patch(pm_emission_factors, by = c("unitid", "orispl", "prmvr"))

# 
# 
# count <-
#   unit_pm_emissions_2 %>%
#   count(pm25_source, is.na(eia_pm_control_efficiency))
# 
# unit_pm_emissions_2 <-
#   unit_pm_emissions %>%
#   rows_patch(pm_fuel_pmover_firing, by = c("unitid", "orispl"))
# 
# count <-
#   unit_pm_emissions_2 %>%
#   count(pm25_source)
# 
# unit_pm_emissions_3 <-
#   unit_pm_emissions_2 %>%
#   rows_patch(pm_fuel_pmover, by = c("unitid", "orispl"))
# 
# count <-
#   unit_pm_emissions_3 %>%
#   count(pm25_source)
# 
# unit_pm_emissions_4 <-
#   unit_pm_emissions_3 %>%
#   rows_patch(pm_emission_factors, by = c("unitid", "orispl"))

## 6) 


