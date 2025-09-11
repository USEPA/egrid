## -------------------------------
##
## Unit data PM NH3 VOC functions
## 
## Purpose: 
## 
## This function creates emission unit data for PM2.5, NH3, and VOC 
## 
## The method of emission calculations are listed within emission_source.
##
## NOTE: Emissions data used in these calculations are from a version of  
## EPA's NEI that is not publicly available.
##
##
## Authors:
##      Emma Russell, Abt Global
##
## -------------------------------


unit_data_pm_nh3_voc <- function(emission_type){
  
  #' @name unit_data_pm_nh3_voc
  #' 
  #' Function to create pm2.5, nh3, or voc unit file data using a sequence of methods
  #' 
  #' @param emission_type Emission type to be calculated - either
  #'                      "pm", "nh3", or "voc"
  #' @return Dataset with emission unit data in the format needed for 
  #'         plant file computation
  #'         
  #' @examples 
  #' # Create PM2.5 unit data
  #' pm_unit_data <- unit_data_pm_nh3_voc("pm")
  
  
  # Require libraries ---------
  require(dplyr)
  require(readr)
  require(readxl)
  
  # Load necessary data --------------------
  
  # EIA-923 - for Schedule C Air Emissions Control information
  if(file.exists(glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) {
    eia_923 <- read_rds(glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))$air_emissions_control_info
  } else {
    stop("eia_923_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R in /scripts/1_production_model/ to obtain.")
    }
  
  # NEI emission data
  if(file.exists(glue::glue("data/2a_pm_nh3_voc/inputs/nei/{params$eGRID_year}/nei_emissions_{emission_type}.csv"))) {
    raw_nei <- read_csv(glue::glue("data/2a_pm_nh3_voc/inputs/nei/{params$eGRID_year}/nei_emissions_{emission_type}.csv"), 
                        # set column types to character and double (for total_emissions)
                        col_types = cols(.default = "c",
                                         total_emissions = "d")) %>%
      janitor::clean_names()
  } else { 
    stop(glue::glue("nei_emissions_{emission_type}.csv does not exist."))
    }
  
  # NEI-EIA crosswalk matching NEI and EIA unit ids
  if(file.exists(glue::glue("data/2a_pm_nh3_voc/inputs/nei_eia_crosswalk/{params$eGRID_year}/xwalk_nei_eia.csv"))) { 
    nei_eia_xwalk <- read_csv(glue::glue("data/2a_pm_nh3_voc/inputs/nei_eia_crosswalk/{params$eGRID_year}/xwalk_nei_eia.csv"), 
                              # set column data type all to character
                              col_types = cols(.default = "c")) %>%
      janitor::clean_names()
  } else { 
    stop(glue::glue("data/2a_pm_nh3_voc/inputs/nei_eia_crosswalk/{params$eGRID_year}/xwalk_nei_eia.csv does not exist."))
    }
  
  # Emission factors from EPA AP-42 dataset
  efs <- read_csv(glue::glue("data/2a_pm_nh3_voc/static_tables/emission_factors_{emission_type}.csv"), 
                  # set ef data type to double and all else as charcter
                  col_types = cols(.default = "c",
                                   EF = "d")) %>%
    janitor::clean_names()
  
  # eGRID production model data - unit file (2022)
  if(params$eGRID_year == "2022") {
    unit_file_raw <- read_excel(glue::glue("data/2a_pm_nh3_voc/static_tables/historic_egrid/egrid{params$eGRID_year}_data.xlsx"),
                                sheet = paste0("UNT", substr(params$eGRID_year, 3, 4)),
                                skip = 1,
                                col_names = TRUE) %>%
      rename(CAPDFLAG = CAMDFLAG) %>% # rename CAMD flag to updated name
      rename_with(~ ifelse(. == paste0("SEQUNT", substr(params$eGRID_year, 3, 4)), "SEQUNT", .)) # rename SEQUNT if necessary
    
    # replace any "NA" strings with an NA
    unit_file_raw[unit_file_raw == "NA"] <- NA_character_
    
    # Prepare unit data for evaluation --------------
    # load abbreviated name to snake_case matches
    base::load("data/1_production_model/static_tables/name_matches.Rdata")
    
    # select names present in unit file column names
    unit_new_names <- unit_nonmetric[names(unit_nonmetric) %in% colnames(unit_file_raw)]
    
    # rename data columns to prepare for computation
    unit_file <- 
      unit_file_raw %>%
      # rename columns based on name matches
      rename(!!!setNames(lapply(names(unit_new_names), sym), unit_new_names)) %>%
      # convert years and plant_id data to characters
      mutate(year = as.character(year), 
             plant_id = as.character(plant_id),
             year_online = as.character(year_online))
    
  # eGRID production model data - unit file (2023+)
  } else {
    unit_file <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/unit_file.RDS"))
  }
  
  # Set emission type label for data columns ----
  if (emission_type == "pm") {
    emission_label <- "pm25"
  } else {
    emission_label <- emission_type
  }

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
    # add emission source for those calculated with direct match
    mutate(emission_source = if_else(is.na(emission), NA_character_, "EPA/NEI"),
           eia_control_efficiency = NA_real_, 
           botfirty = if_else(botfirty == "", NA_character_, botfirty))
  
  
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
    ungroup() %>%
    mutate(emission_factors = if_else(sum_heat_input != 0, sum_emission / sum_heat_input, NA_real_)) %>% 
    inner_join(unit_emissions, by = join_by(prime_mover, botfirty, primary_fuel_type)) %>%
    # multiply individual heat inputs by emission factors to estimate emission
    # define method used under source
    mutate(emission = emission_factors * heat_input, 
           emission_source = "NEI avg EF - PM, fuel type, firing type") %>% 
    select(plant_id, unit_id, emission, emission_source)
  
  
  ## 3) Match by fuel type and prime mover - "NEI avg EF - PM, fuel type" ----------
  # calculate emission emissions using matching of prime mover and fuel type
  fuel_pmover <-
    unit_emissions %>%
    # filter to those with NEI match
    filter(emission_source == "EPA/NEI") %>% 
    # group by prime mover, fuel type
    group_by(prime_mover, primary_fuel_type, emission_source) %>%
    # calculate emissions factor
    summarise(sum_heat_input = if_else(all(is.na(heat_input)), NA_real_, sum(heat_input, na.rm = TRUE)), 
              sum_emission = if_else(all(is.na(emission)), NA_real_, sum(emission, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(emission_factors = if_else(sum_heat_input != 0, sum_emission / sum_heat_input, NA_real_)) %>%
    inner_join(unit_emissions, by = join_by(prime_mover, primary_fuel_type)) %>%
    # multiply individual heat inputs by emission factors to estimate emission
    # define method used under source
    mutate(emission = emission_factors * heat_input, emission_source = "NEI avg EF - PM, fuel type") %>%
    select(plant_id, unit_id, emission, emission_source)
  
  
  ## 4) Use emissions factors from AP-42 - "Estimated using an emissions factor" ---------
  #' AP-42 data provided by EPA and is publicly available at: 
  #' https://www.epa.gov/air-emissions-factors-and-quantification/ap-42-compilation-air-emissions-factors-stationary-sources
  #' 
  # calculate emission emissions based on emission factors in AP-42 report
  emissions_factors <-
    unit_emissions %>%
    # calculate emissions with emissions factors specific to fuel, firing type, and prime mover
    inner_join(efs, by = join_by(botfirty, primary_fuel_type == fuelu1, prime_mover == prmvr)) %>%
    mutate(emission = ef * heat_input / 2000, 
           emission_source = "Estimated using an emissions factor") %>% 
    filter(!is.na(emission)) %>%
    rename(emission_ef = emission, emission_source_ef = emission_source) %>%
    select(plant_id, unit_id, prime_mover, emission_ef, emission_source_ef)
  
  # for PM2.5 data, if there is a unit match with EIA-923, adjust emission by control efficiency 
  if(emission_type == "pm") {
    removal_efficiencies <-
      eia_923 %>%
      # group data by plant ID
      group_by(plant_id) %>%
      # remove removal efficiency NA values
      filter(!is.na(pm_removal_efficiency_rate_at_annual_operating_factor)) %>%
      # compute plant-level maximum control efficiency rate
      summarise(eia_control_efficiency = max(pm_removal_efficiency_rate_at_annual_operating_factor, na.rm = TRUE)) %>%
      ungroup() %>%
      # remove any efficiency rates exceeding 100%
      filter(eia_control_efficiency <= 1) %>%
      inner_join(emissions_factors, by = join_by(plant_id == plant_id)) %>%
      # adjust emission using control efficiency rate
      mutate(emission = emission_ef * (1 - eia_control_efficiency), 
             emission_source = "Estimated using an emissions factor") %>%
      rename(emission_re = emission, emission_source_re = emission_source) %>%
      select(plant_id, unit_id, prime_mover, emission_re, emission_source_re)
    
    
    # Add emission estimates to unit data -------------
    # update unit file with emission rates from each method - order specific
    # includes removal efficiencies for PM2.5
    unit_emissions_updated <-
      unit_emissions %>%
      rows_patch(fuel_pmover_firing, by = c("unit_id", "plant_id")) %>%
      rows_patch(fuel_pmover, by = c("unit_id", "plant_id")) %>%
      left_join(removal_efficiencies, by = join_by(unit_id, plant_id, prime_mover)) %>%
      mutate(emission = if_else(is.na(emission_source), emission_re, emission),
             emission_source = if_else(is.na(emission_source), emission_source_re, emission_source)) %>%
      select(-emission_re, -emission_source_re)
    
  } else {
    # does not include removal efficiences for other pollutants
    unit_emissions_updated <-
      unit_emissions %>%
      rows_patch(fuel_pmover_firing, by = c("unit_id", "plant_id")) %>%
      rows_patch(fuel_pmover, by = c("unit_id", "plant_id"))
    }
  
  # update unit file with remaining emission rates
  unit_emissions_total <-
    unit_emissions_updated %>%
    left_join(emissions_factors, by = join_by(unit_id, plant_id, prime_mover)) %>%
    mutate(emission = if_else(is.na(emission_source), emission_ef, emission),
           emission_source = if_else(is.na(emission_source), emission_source_ef, emission_source)) %>%
    select(-emission_ef, -emission_source_ef)
  
  # Format unit data -----
  unit_formatted <-
    unit_emissions_total %>%
    # set annual emissions to NA for renewable fuel types
    mutate(emission_ann = if_else(primary_fuel_type %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA_real_, emission),
  # set emission source type to NA for renewable fuel types
  emission_source = if_else(emission_ann >= 0, emission_source, NA_character_),
  # add data column with adjusted emission rate
  emission_rate = if_else(heat_input != 0, emission_ann * 2000 / heat_input, NA_real_),
  year = params$eGRID_year) %>%
  # select desired variables for final version
  select(year, plant_state, plant_name, plant_id, unit_id, prime_mover, operating_status, botfirty, primary_fuel_type, operating_hours, heat_input, emission, emission_ann, emission_rate, heat_input_source, emission_source, year_online) %>%
  # replace emission with emission label in column names
  rename_with(~gsub("emission", emission_label, .))
  
  return(unit_formatted)

}

