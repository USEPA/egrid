## -------------------------------
##
## Unit data PM NH3 VOC function
## 
## Purpose: 
## 
## This function creates the first iteration of the emission unit
## data for PM2.5, NH3, and VOC that is used to compute plant 
## aggregated values. The output is not the final version used in 
## the unit files and are formatted in unit_file_create_pm_nh3_voc.
## 
## The method of emission calculations are listed within emission_source.
##
## NOTE: Emissions data used in these calculations are from a version of  
## EPA's NEI that is not publicly available.
##
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------


unit_data_pm_nh3_voc <- function(emission_type){
  
  #' unit_data_pm_nh3_voc
  #' 
  #' Function to create pm2.5, nh3, or voc unit file data using a sequence of methods
  #' 
  #' @param emission_type Emission type to be calculated - either
  #'                      "pm25", "nh3", or "voc"
  #' @return Dataset with emission unit data in the format needed for 
  #'         plant file computation
  #'         
  #' @examples 
  #' # Create PM2.5 unit data
  #' pm_unit_data <- unit_data_pm_nh3_voc("pm25")
  
  
  # Require Libraries ---------
  require(dplyr)
  require(readr)
  require(readxl)
  
  # Load necessary data --------------------
  if(params$eGRID_year == "2021") {
    ## EIA-923 - for Schedule C Air Emissions Control information (2021)
    eia_923 <- read_csv(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_9c_airemissions.csv"), col_types = "ccccccccddddcccdccddcdc") %>%
      janitor::clean_names()
  } else {
    ## EIA-923 - for Schedule C Air Emissions Control information (2022+)
    if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) {
      eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))$air_emissions_control_info
    } else {
      stop("eia_923_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}
  }
  
  ## NEI emission data
  if(file.exists(glue::glue("data/raw_data/nei/{params$eGRID_year}/nei_{emission_type}_emissions_raw.csv"))) { 
    raw_nei <- read_csv(glue::glue("data/raw_data/nei/{params$eGRID_year}/nei_{emission_type}_emissions_raw.csv"), col_types = "cccccccccccccccccdcc") %>%
      janitor::clean_names()
  } else { 
    stop(glue::glue("nei_{emission_type}_emissions_raw.csv does not exist."))}
  
  ## NEI-EIA crosswalk matching NEI and EIA unit ids
  nei_eia_xwalk <- read_csv("data/static_tables/xwalk_nei_eia.csv", col_types = "cccccccccccccccccccc") %>%
    janitor::clean_names()
  
  ## Particulate matter emission factors from EPA AP-42 dataset
  efs <- read_csv(glue::glue("data/static_tables/emission_factors_{emission_type}.csv"), col_types = "cccdccc") %>%
    janitor::clean_names()
  
  ## eGRID production model data - unit file
  unit_file_raw <- read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"),
                          sheet = paste0("UNT", substr(params$eGRID_year, 3, 4)),
                          skip = 1,
                          col_names = TRUE) %>%
    rename(CAPDFLAG = CAMDFLAG) # rename CAMD flag to updated name
  
  # replace any "NA" strings with an NA
  unit_file_raw[unit_file_raw == "NA"] <- NA_character_ 
  
  # Prepare unit data for evaluation --------------
  # Load abbreviated name to snake_case matches
  load("data/static_tables/name_matches.Rdata")
  # Select names present in unit file column names
  unit_new_names <- unit_nonmetric[names(unit_nonmetric) %in% colnames(unit_file_raw)]
  
  ## rename data columns to prepare for computation
  unit_file <- 
    unit_file_raw %>%
    # rename columns based on name matches
    rename(!!!setNames(lapply(names(unit_new_names), sym), unit_new_names)) %>%
    # convert year and plant_id data to characters
    mutate(year = as.character(year), plant_id = as.character(plant_id))
  
  # Calculate PM data -------------
  ## 1) Direct Match - "NEI/EIA" --------------
  # calculate emission data using direct unit match from EIA to NEI
  direct_match <-
    raw_nei %>%
    # match NEI (EIS codes) to eGRID (plant_id codes) with crosswalk (QAQPS)
    left_join(nei_eia_xwalk, by = join_by(eis_facility_id, eis_unit_id)) %>% 
    select(oris_facility_code = left_side_of_alternate_id_to_become_oris_facility_code, oris_boiler_id = right_side_of_alternatve_id_to_become_oris_boiler_id, 
           emission = total_emissions, state = state.y,
           eis_facility_id, eis_unit_id, agency_unit_id) %>%
    group_by(oris_facility_code, oris_boiler_id) %>%
    # calculate the sum of pm emissions for each facility ID and boiler ID combination
    summarise(emission = if_else(all(is.na(emission)), NA_real_, sum(emission, na.rm = TRUE))) %>%
    ungroup()
  
  # create unit file with emission emission data
  unit_emissions <-
    unit_file %>%
    # combine direct match emission and unit file data
    left_join(direct_match, by = join_by(plant_id == oris_facility_code, unit_id == oris_boiler_id)) %>% 
    # modify dataset format and add emission source for those calculated with direct match
    mutate(emission_source = if_else(is.na(emission), NA_character_, "EPA/NEI"), eia_control_efficiency = NA_real_, botfirty = if_else(botfirty == "", NA_character_, botfirty))
  
  
  ## 2) Match by fuel type, unit firing type, and prime mover - "NEI avg EF - PM, fuel type, firing type" ----------
  # calculate emission emissions using matching of fuel type, prime mover, and firing type
  fuel_pmover_firing <-
    unit_emissions %>%
    # filter to those with NEI match
    filter(emission_source == "EPA/NEI") %>% 
    # group by prime mover, firing, fuel type
    group_by(prime_mover, botfirty, primary_fuel_type, emission_source) %>% 
    # calculate emissions factor
    summarise(sum_heat_input = if_else(all(is.na(heat_input)), NA_real_, sum(heat_input, na.rm = TRUE)), sum_emission = if_else(all(is.na(emission)), NA_real_, sum(emission, na.rm = TRUE))) %>%
    mutate(emission_factors = if_else(sum_heat_input != 0, sum_emission / sum_heat_input, NA_real_)) %>% 
    inner_join(unit_emissions, by = join_by(prime_mover, botfirty, primary_fuel_type)) %>%
    # multiply individual heat inputs by emission factors to estimate emission
    # define method used under source
    mutate(emission = emission_factors * heat_input, emission_source = "NEI avg EF - PM, fuel type, firing type") %>% 
    ungroup() %>%
    select(plant_id, unit_id, prime_mover, emission, emission_source)
  
  
  ## 3) Match by fuel type and prime mover - "NEI avg EF - PM, fuel type" ----------
  # calculate emission emissions using matching of prime mover and fuel type
  fuel_pmover <-
    unit_emissions %>%
    # filter to those with NEI match
    filter(emission_source == "EPA/NEI") %>% 
    # group by prime mover, fuel type
    group_by(prime_mover, primary_fuel_type, emission_source) %>%
    # calculate emissions factor
    summarise(sum_heat_input = if_else(all(is.na(heat_input)), NA_real_, sum(heat_input, na.rm = TRUE)), sum_emission = if_else(all(is.na(emission)), NA_real_, sum(emission, na.rm = TRUE))) %>%
    mutate(emission_factors = if_else(sum_heat_input != 0, sum_emission / sum_heat_input, NA_real_)) %>%
    inner_join(unit_emissions, by = join_by(prime_mover, primary_fuel_type)) %>%
    # multiply individual heat inputs by emission factors to estimate emission
    # define method used under source
    mutate(emission = emission_factors * heat_input, emission_source = "NEI avg EF - PM, fuel type") %>%
    ungroup() %>%
    select(plant_id, unit_id, prime_mover, emission, emission_source)
  
  
  ## 4) Use emissions factors from AP-42 - "Estimated using an emissions factor" ---------
  # calculate emission emissions based on emission factors in AP-42 report
  emissions_factors <-
    unit_emissions %>%
    # use emissions factors specific to fuel, firing type, and prime mover to calculate emission
    inner_join(efs, by = join_by(botfirty, primary_fuel_type == fuelu1, prime_mover == prmvr)) %>%
    mutate(emission = ef * heat_input / 2000, emission_source = "Estimated using an emissions factor") %>% 
    filter(!is.na(emission)) %>%
    rename(emission_ef = emission, emission_source_ef = emission_source) %>%
    select(plant_id, unit_id, prime_mover, emission_ef, emission_source_ef)
  
  # if there is a unit match with EIA-923, adjust emission by control efficiency (only for PM2.5 data)
  if(emission_type == "pm25") {
    removal_efficiencies <-
      eia_923 %>%
      # select plants with removal efficiency rates
      filter(!is.na(pm_removal_efficiency_rate_at_annual_operating_factor)) %>%
      group_by(plant_id) %>%
      # convert efficiency rate to numeric percentage
      summarise(eia_control_efficiency = max(as.numeric(sub("%", "", pm_removal_efficiency_rate_at_annual_operating_factor)) / 100)) %>%
      filter(eia_control_efficiency <= 1) %>%
      inner_join(emissions_factors, by = join_by(plant_id == plant_id)) %>%
      # adjust emission using control efficiency rate
      mutate(emission = emission_ef * (1 - eia_control_efficiency), emission_source = "Estimated using an emissions factor") %>%
      rename(emission_re = emission, emission_source_re = emission_source) %>%
      select(plant_id, unit_id, prime_mover, emission_re, emission_source_re)
    
    # Add emission estimates to unit data -------------
    # update unit file with emission emission rates from each method - order specific
    
    # includes removal efficiencies for PM2.5
    unit_emissions_updated <-
      unit_emissions %>%
      rows_patch(fuel_pmover_firing, by = c("unit_id", "plant_id", "prime_mover")) %>%
      rows_patch(fuel_pmover, by = c("unit_id", "plant_id", "prime_mover")) %>%
      left_join(removal_efficiencies, by = join_by(unit_id, plant_id, prime_mover)) %>%
      mutate(emission = if_else(is.na(emission_source), emission_re, emission), emission_source = if_else(is.na(emission_source), emission_source_re, emission_source)) %>%
      select(-emission_re, -emission_source_re)
  } else {
    # does not include removal efficiences for other pollutants
    unit_emissions_updated <-
      unit_emissions %>%
      rows_patch(fuel_pmover_firing, by = c("unit_id", "plant_id", "prime_mover")) %>%
      rows_patch(fuel_pmover, by = c("unit_id", "plant_id", "prime_mover"))
  }
  
  # update unit file with remaining emission rates
  unit_emissions_final <-
    unit_emissions_updated %>%
    left_join(emissions_factors, by = join_by(unit_id, plant_id, prime_mover)) %>%
    mutate(emission = if_else(is.na(emission_source), emission_ef, emission), emission_source = if_else(is.na(emission_source), emission_source_ef, emission_source)) %>%
    select(-emission_ef, -emission_source_ef)
  
  return(unit_emissions_final)
}
