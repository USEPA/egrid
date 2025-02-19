## -------------------------------
##
## Create PM unit data
## 
## Purpose: 
## 
## This function creates the first iteration of the PM2.5 unit
## file that is used to compute plant aggregated values. The 
## output is not the final version used in the unit file and is
## formatted in pm_unit_file_create.
## 
## The method of PM2.5 calculations are listed within pm25_source.
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------


create_pm_unit_data <- function(){
  
  #' create_pm_unit_data
  #' 
  #' Function to create PM unit file data using a sequence of methods
  #' 
  #' @return Dataset with PM2.5 unit data in the format needed for 
  #'         plant file computation
  #'         
  #' @examples 
  #' # Create PM2.5 unit data
  #' pm_unit_data <- create_pm_unit_data()
  
  
  # Require Libraries ---------
  require(dplyr)
  require(readr)
  require(readxl)
  
  
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
  pm_unit_emissions <-
    unit_file %>%
    # combine direct match pm2.5 and unit file data
    left_join(pm_direct_match, by = join_by(orispl == oris_facility_code, unitid == oris_boiler_id)) %>% 
    # modify dataset format and add pm2.5 source for those calculated with direct match
    mutate(pm25_source = if_else(is.na(pm25), NA, "EPA/NEI"), eia_pm_control_efficiency = as.numeric(NA), botfirty = if_else(botfirty == "", NA, botfirty))
  
  
  ## 2) Match by fuel type, unit firing type, and prime mover - "NEI avg EF - PM, fuel type, firing type" ----------
  # calculate pm2.5 emissions using matching of fuel type, prime mover, and firing type
  pm_fuel_pmover_firing <-
    pm_unit_emissions %>%
    # filter to those that have a match with NEI
    filter(pm25_source == "EPA/NEI") %>% 
    # group by prime mover, firing, fuel type
    group_by(prmvr, botfirty, fuelu1, pm25_source) %>% 
    # calculate emission factor
    summarise(sum_heat_input = sum(htian), sum_pm25 = sum(pm25)) %>%
    mutate(emission_factors = sum_pm25 / sum_heat_input) %>% 
    inner_join(pm_unit_emissions, by = join_by(prmvr, botfirty, fuelu1)) %>%
    # multiply individual heat inputs by emission factors to estimate pm2.5
    # define method used under source
    mutate(pm25 = emission_factors * htian, pm25_source = "NEI avg EF - PM, fuel type, firing type") %>% 
    ungroup() %>%
    select(orispl, unitid, prmvr, pm25, pm25_source)
  
  
  ## 3) Match by fuel type and prime mover - "NEI avg EF - PM, fuel type" ----------
  # calculate pm2.5 emissions using matching of prime mover and fuel type
  pm_fuel_pmover <-
    pm_unit_emissions %>%
    # filter to those that have a match with NEI
    filter(pm25_source == "EPA/NEI") %>% 
    # group by prime mover, fuel type
    group_by(prmvr, fuelu1, pm25_source) %>%
    # calculate emission factor
    summarise(sum_heat_input = sum(htian), sum_pm25 = sum(pm25)) %>%
    mutate(emission_factors = sum_pm25 / sum_heat_input) %>% 
    inner_join(pm_unit_emissions, by = join_by(prmvr, fuelu1)) %>%
    # multiply individual heat inputs by emission factors to estimate pm2.5
    # define method used under source
    mutate(pm25 = emission_factors * htian, pm25_source = "NEI avg EF - PM, fuel type") %>%
    ungroup() %>%
    select(orispl, unitid, prmvr, pm25, pm25_source)
  
  
  ## 4) Use emissions factors from AP-42 - "Estimated using an emission factor" ---------
  # calculate pm2.5 emissions based on emission factors in AP-42 report
  pm_emission_factors <-
    pm_unit_emissions %>%
    # use emissions factors specific to fuel, firing type, and prime mover to calculate pm2.5
    inner_join(pm_efs, by = join_by(botfirty, fuelu1, prmvr)) %>%
    mutate(pm25 = ef * htian / 2000, pm25_source = "Estimated using an emission factor") %>% 
    filter(!is.na(pm25)) %>%
    rename(pm25_ef = pm25, pm25_source_ef = pm25_source) %>%
    select(orispl, unitid, prmvr, pm25_ef, pm25_source_ef)
  
  
  # if there is a unit match with EIA-923, adjust pm2.5 by control efficiency
  pm_removal_efficiencies <-
    eia_923 %>%
    # select plants with removal efficiency rates
    filter(!is.na(pm_removal_efficiency_rate_at_annual_operating_factor)) %>%
    group_by(plant_id) %>%
    # convert efficiency rate to numeric percentage
    summarise(eia_pm_control_efficiency = max(as.numeric(sub("%", "", pm_removal_efficiency_rate_at_annual_operating_factor)) / 100)) %>%
    inner_join(pm_emission_factors, by = join_by(plant_id == orispl)) %>%
    rename(orispl = plant_id) %>%
    # adjust pm2.5 using control efficiency rate
    mutate(pm25 = pm25_ef * (1 - eia_pm_control_efficiency), pm25_source = "Estimated using an emission factor") %>%
    rename(pm25_re = pm25, pm25_source_re = pm25_source) %>%
    select(orispl, unitid, prmvr, pm25_re, pm25_source_re)
  
  
  # Add pm2.5 emission estimates to unit data -------------
  # update unit file with pm2.5 emission rates from each method - order specific
  pm_unit_emissions_updated <-
    pm_unit_emissions %>%
    rows_patch(pm_fuel_pmover_firing, by = c("unitid", "orispl")) %>%
    rows_patch(pm_fuel_pmover, by = c("unitid", "orispl")) %>%
    left_join(pm_removal_efficiencies, by = join_by(unitid, orispl, prmvr)) %>%
    mutate(pm25 = if_else(is.na(pm25_source), pm25_re, pm25), pm25_source = if_else(is.na(pm25_source), pm25_source_re, pm25_source)) %>%
    left_join(pm_emission_factors, by = join_by(unitid, orispl, prmvr)) %>%
    mutate(pm25 = if_else(is.na(pm25_source), pm25_ef, pm25), pm25_source = if_else(is.na(pm25_source), pm25_source_ef, pm25_source)) %>%
    select(-pm25_ef, -pm25_re, -pm25_source_ef, -pm25_source_re)
  
  return(pm_unit_emissions_updated)
}