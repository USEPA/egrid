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
if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) { 
  eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))
} else { 
  stop("eia_923_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

## NEI PM data
if(file.exists(glue::glue("data/raw_data/nei/{params$eGRID_year}/emis_sum_unit_25291.csv"))) { 
  pm25_raw_nei <- read_csv(glue::glue("data/raw_data/nei/{params$eGRID_year}/emis_sum_unit_25291.csv")) %>%
    janitor::clean_names()
} else { 
  stop("emis_sum_unit_25291.csv does not exist.")}
  # access and r both have 355919 observations

## NEI-EIA crosswalk
nei_eia_xwalk <- read_csv("data/static_tables/fac_conf_alt_id_25292.csv") %>% #, #col_types = "cccccccccccccccccccc") 
  janitor::clean_names()
  # both are 5310 observations

## eGRID production model data
unit_file <- read_xlsx(glue::glue("data/outputs/{params$eGRID_year}/unit_file_2021_access.xlsx")) %>%
  janitor::clean_names()
  # both have 24597 observations, 1819 missing htian values

#unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))

# Calculate PM data -------------
## 1) Direct Match - "NEI/EIA" --------------
  ## Queries 000, 00, 01a, 01b, 2
  ## Match NEI (EIS codes) to eGRID (ORISPL codes) with crosswalk (QAQPS)
  ## Calculate the sum of pm emissions for each facility ID and boiler ID combination

pm25_direct_match <-
  pm25_raw_nei %>%
  left_join(nei_eia_xwalk, by = join_by(eis_facility_id, eis_unit_id)) %>%
  select(oris_facility_code = left_side_of_alternate_id_to_become_oris_facility_code, oris_boiler_id = right_side_of_alternatve_id_to_become_oris_boiler_id, 
         pm25 = total_emissions, state = state.y,
         eis_facility_id, eis_unit_id, agency_unit_id) %>%
  group_by(oris_facility_code, oris_boiler_id) %>%
  summarise(sum_pm25 = sum(pm25)) %>%
  ungroup() #%>%
  # right_join(unit_file, by = join_by(oris_boiler_id == unitid, oris_facility_code == orispl)) %>%
  # rename(pm25 = sum_pm25, unitid = oris_boiler_id, orispl = oris_facility_code) %>%
  # mutate(pm25_source = if_else(is.na(pm25), NA, "EPA/NEI"), eia_pm_control_efficiency = NA, botfirty = if_else(botfirty == "", NA, botfirty))

# match unit file units to pm25 sums (query 01,01a,01b,2)
  # join by unit id and facility id
  # 3582 matches, now 3582 rows have pm data (same as access)
unit_pm25_emissions <-
  unit_file %>%
  left_join(pm25_direct_match, by = join_by(unitid == oris_boiler_id, orispl == oris_facility_code)) %>%
  rename(pm25 = sum_pm25) %>%
  mutate(pm25_source = if_else(is.na(pm25), NA, "EPA/NEI"), eia_pm_control_efficiency = NA, botfirty = if_else(botfirty == "", NA, botfirty))

## 2) Match by fuel type, unit firing type, and prime mover - "NEI avg EF - PM, fuel type, firing type" ----------
  ## Queries 03a, 03b, 03c
  ## Group EPA/NEI-matched units (3582) by fuel type, firing type, and prime mover (59 combinations)
  ## Calculate the heat input and pm2.5 sums for groups
  ## Calculate emission factors from sum (EF = pm2.5 sum / heat input sum)
  ## Multiply individual heat inputs by emission factors to estimate pm2.5

pm25_fuel_pmover_firing <-
  unit_pm25_emissions %>%
  filter(pm25_source == "EPA/NEI") %>% # filter to those that have a match with NEI
  group_by(prmvr, botfirty, fuelu1, pm25_source) %>% # group by prime mover, firing, fuel type
  summarise(sum_heat_input = sum(htian), sum_pm25 = sum(pm25)) %>%
  mutate(emission_factors = sum_pm25 / sum_heat_input) %>% # calculate emission factor
  inner_join(unit_pm25_emissions, by = join_by(prmvr, botfirty, fuelu1)) %>%
  mutate(pm25 = emission_factors * htian, pm25_source = "NEI avg EF - PM, fuel type, firing type") %>%
  ungroup() %>%
  select(orispl, unitid, prmvr, pm25, pm25_source)

## 3) Match by fuel type and prime mover - "NEI avg EF - PM, fuel type" ----------
  ## Queries 04a, 04b, 04c
  ## Group EPA/NEI-matched units (3582) by fuel type and prime mover (25 combinations)
  ## Calculate the heat input and pm2.5 sums for groups
  ## Calculate emission factors from sum (EF = pm2.5 sum / heat input sum)
  ## Multiply individual heat inputs by emission factors to estimate pm2.5

pm25_fuel_pmover <-
  unit_pm25_emissions %>%
  filter(pm25_source == "EPA/NEI") %>%
  group_by(prmvr, fuelu1, pm25_source) %>%
  summarise(sum_heat_input = sum(htian), sum_pm25 = sum(pm25)) %>%
  mutate(emission_factors = sum_pm25 / sum_heat_input) %>%
  inner_join(unit_pm25_emissions, by = join_by(prmvr, fuelu1)) %>%
  mutate(pm25 = emission_factors * htian, pm25_source = "NEI avg EF - PM fuel type") %>%
  ungroup() %>%
  select(orispl, unitid, prmvr, pm25, pm25_source)


## 4) Use a similar fuel type ---------
  ## pm25src = "Estimated using an emission factor"


## 5) Add pm2.5 emission estimates to unit data -------------
  ## Queries 03d, 04d, 

unit_pm25_emissions_updated <-
  unit_pm25_emissions %>%
  rows_patch(pm25_fuel_pmover_firing, by = c("unitid", "orispl")) %>%
  rows_patch(pm25_fuel_pmover, by = c("unitid", "orispl"))

# unit_pm25_emissions_2 <-
#   unit_pm_emissions %>%
#   rows_patch(pm25_fuel_pmover_firing, by = c("unitid", "orispl"))
# 
# count <-
#   unit_pm25_emissions_2 %>%
#   count(pm25_source)
# 
# unit_pm_emissions_3 <-
#   unit_pm_emissions_2 %>%
#   rows_patch(pm25_fuel_pmover, by = c("unitid", "orispl"))
# 
# count <-
#   unit_pm_emissions_3 %>%
#   count(pm25_source)


###---------- NOTES --------------###

## we have duplicate matches for orispl 2707 and unitid 1-4
  
  # for the values that are still NA after an estimate, should I then replace them if there is an estimate?


## in access the second estimate went from 775 -> 668 (-107)
## the third estimate is 1681 (if we remove the duplicates, we should expect 1574 to be in the third category)
## right now we have 1594 in the third category although we initially have in the inner join 5951 instead of 5947 (an extra 4)

## need to check on the state of this - it seems to override ones that are already listed as the NEI avg from the three categories - need a better system to determine when to rewrite data! 

###-----------------------------------####