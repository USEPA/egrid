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
  pm_data_nei <- read_csv(glue::glue("data/raw_data/nei/{params$eGRID_year}/emis_sum_unit_25291.csv")) %>%
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
  # both have 24597 observations
  # both have 1819 missing htian values
#unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))

# Calculate PM data -------------
## 1) Direct Match - "NEI/EIA" --------------
  # access queries 000-01b
  # match NEI (EIS codes) to eGRID (ORISPL codes)
  # using crosswalk (QAQPS)

# join pm data with matching crosswalk data (query 000)
  # matching by facility and unit IDs
  # left join keeps all pm records and any matching crosswalk records
  # 3918 matching records from crosswalk
pm_unit_match <- # (matching 356369 observations)
  pm_data_nei %>%
  left_join(nei_eia_xwalk, by = join_by(eis_facility_id, eis_unit_id)) %>%
  select(oris_facility_code = left_side_of_alternate_id_to_become_oris_facility_code, oris_boiler_id = right_side_of_alternatve_id_to_become_oris_boiler_id, 
         pm25 = total_emissions, state = state.y,
         eis_facility_id, eis_unit_id, agency_unit_id)

# calculate sum of pm emissions for each facility ID and boiler ID combination (query 00)
 # 3907 observations of summed pm25 (all not NA)
pm_sum_facility_boiler <-
  pm_unit_match %>%
  group_by(oris_facility_code, oris_boiler_id) %>%
  summarise(sum_pm25 = sum(pm25))

# match unit file units to pm25 sums (query 01,01a,01b,2)
  # join by unit id and facility id
  # 3582 matches, now 3582 rows have pm data (same as access)
unit_pm_emissions <-
  unit_file %>%
  left_join(pm_sum_facility_boiler, by = join_by(unitid == oris_boiler_id, orispl == oris_facility_code)) %>%
  rename(pm25 = sum_pm25) %>%
  mutate(pm25_source = if_else(is.na(pm25), NA, "EPA/NEI"), eia_pm_control_efficiency = NA, botfirty = if_else(botfirty == "", NA, botfirty))

## 2) Match by fuel type, unit firing type, and prime mover - "NEI avg EF - PM, fuel type, firing type" ----------
  ## Group units by fuel type, firing type, and prime mover
  ## Calculate the heat input and pm2.5 sums for groups
  ## Calculate emission factors from sum
  ## Multiply individual heat inputs by emission factors to estimate pm2.5
  ## Add estimates to dataset

# calculate emission factors for fuel type, unit firing type, and prime mover combinations (query 03a,03b)
  # select EPA/NEI data (3582)
  # calculate the sum of heat input and sum of pm2.5
  # for all prime mover, firing type, and fuel type combinations (59 combinations)
  # emission factors = pm2.5 / heat input
emission_factors_fuel_pm_firing <-
  unit_pm_emissions %>%
  filter(pm25_source == "EPA/NEI") %>%
  group_by(prmvr, botfirty, fuelu1, pm25_source) %>%
  summarise(sum_heat_input = sum(htian), sum_pm25 = sum(pm25)) %>%
  relocate(pm25_source, .after = sum_pm25) %>%
  mutate(emission_factors = sum_pm25 / sum_heat_input)

# calculate pm2.5 emissions estimate for grouping (query 03c)
  #multiply heat inputs for each unit by estimated emission factors (4353)
pm25_fuel_pm_firing <-
  unit_pm_emissions %>%
  inner_join(emission_factors_fuel_pm_firing, by = join_by(prmvr, botfirty, fuelu1)) %>%
  mutate(pm25_from_nei_avg = emission_factors * htian) %>%
  select(orispl, unitid, pm25_from_nei_avg, pm25_source_estimate = pm25_source.y)

# add pm estimates to unit emissions data (query 03d)
  # identify units that don't already have a direct match estimate
  # 4357 estimates, can fill 775 that don't already have an estimate
unit_pm_emissions_updated <-
  unit_pm_emissions %>%
  left_join(pm25_fuel_pm_firing, by = join_by(unitid, orispl)) %>%
  mutate(pm25_source = if_else(is.na(pm25) & !is.na(pm25_source_estimate), "NEI avg EF - PM, fuel type, firing type", pm25_source)) %>%
  mutate(pm25 = if_else(is.na(pm25), pm25_from_nei_avg, pm25)) %>%
  select(-pm25_source_estimate, -pm25_from_nei_avg) #%>%
  # count(pm25_source)

#************************************************************

# 3) Match by fuel type and prime mover
#4a-4d
#pm25src = "NEI avg EF - PM, fuel type"

# 4) Use a similar fuel type
# 5
#pm25src = "Estimated using an emission factor"

# 5) Use a similar fuel type

