## -------------------------------
## 
## Plant data PM NH3 VOC function
##
## Purpose: 
## 
## This function creates emission plant data for PM2.5, NH3, and VOC 
##
## The method of emission calculations are listed within emission_source
##
## NOTE: Emissions data used in these calculations are from a version of  
## EPA's NEI that is not publicly available.
##
##
## Authors:
##      Emma Russell, Abt Global
##
## -------------------------------

plant_data_pm_nh3_voc <- function(emission_type){
  
  #' @name plant_data_pm_nh3_voc
  #' 
  #' Function to create pm2.5, nh3, or voc plant file data by aggregating unit data
  #' 
  #' @param emission_type Emission type to be calculated - either
  #'                      "pm", "nh3", or "voc" (string)
  #' @return Data frame with PM2.5 plant data in the format needed for 
  #'         regional aggregation
  #'         
  #' @examples 
  #' # Create PM2.5 plant data
  #' pm_plant_data <- plant_data_pm_nh3_voc("pm")
  
  
  # Require libraries ---------
  require(dplyr)
  require(readr)
  require(readxl)
  
  # Set emission type label for data columns ----
  if (emission_type == "pm") {
    emission_label <- "pm25"
  } else {
    emission_label <- emission_type
  }
  
  # Load necessary data --------------------
  ## eGRID production model data - plant file (2022)
  if(params$eGRID_year == "2022") {
    plant_file_raw <- read_excel(glue::glue("data/2a_pm_nh3_voc/static_tables/historic_egrid/egrid{params$eGRID_year}_data.xlsx"),
                                sheet = paste0("PLNT", substr(params$eGRID_year, 3, 4)),
                                skip = 1,
                                col_names = TRUE) %>%
      rename(CAPDFLAG = CAMDFLAG) %>% # rename CAMD flag to updated name
      rename_with(~ ifelse(. == paste0("SEQPLT", substr(params$eGRID_year, 3, 4)), "SEQPLT", .)) # rename SEQPLT if necessary
    
    # Prepare plant data for evaluation --------------
    # Load abbreviated name to snake_case matches
    base::load("data/1_production_model/static_tables/name_matches.Rdata")
    
    # Select names present in unit file column names
    plant_new_names <- plant_nonmetric[names(plant_nonmetric) %in% colnames(plant_file_raw)]
    
    # rename data columns to prepare for computation
    plant_file <- 
      plant_file_raw %>%
      # rename columns based on name matches
      rename(!!!setNames(lapply(names(plant_new_names), sym), plant_new_names)) %>%
      # convert year and plant_id data to characters
      mutate(year = as.character(year), plant_id = as.character(plant_id))
    
    # eGRID production model data - plant file (2023+)
  } else {
    plant_file <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/plant_file.RDS"))
  }
  
  # Load unit file -----
  if(file.exists(glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/unit_file_{emission_type}.RDS"))) {
    unit_data <- read_rds(glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/unit_file_{emission_type}.RDS")) %>%
      # replace emission label with emission for universal computation
      rename_with(~gsub(emission_label, "emission", .))
  } else {
    stop(glue::glue("unit_file_{emission_type}.RDS does not exist. Run unit_file_create_pm_nh3_voc.R to obtain."))
  }

  # Sum emission unit data by plant id ---------
  plant_sum <-
    unit_data %>%
    group_by(plant_id) %>%
    summarise(emission = if_else(all(is.na(unadj_emission)), NA_real_, sum(unadj_emission, na.rm = TRUE))) %>%
    ungroup()
  
  # Add emission data to plant file ---------
  plant_emissions <-
    plant_file %>%
    left_join(plant_sum, by = join_by(plant_id)) %>%
    # multiply emissions by electric allocation if available (not NA) 
    mutate(emission_ann = emission * if_else(is.na(elec_allocation), 1, elec_allocation),
           # calculate total output emission rate
           emission_output_rate = if_else(generation_ann != 0, emission_ann * 2000 / generation_ann, NA_real_),
           # calculate total input emisseion rate
           emission_input_rate = if_else(combust_heat_input != 0, emission_ann * 2000 / combust_heat_input, NA_real_)) %>%
    # rename emissions data to include "unadj_" prefix to distinguish from next iteration of adjustments
    rename_with(~ paste0("unadj_", .x), c(emission, emission_ann, emission_output_rate)) %>%  
    select(plant_state, plant_name, plant_id, egrid_subregion_name, egrid_subregion, primary_fuel_type, nameplate_capacity, elec_allocation, combust_heat_input, generation_ann, unadj_emission_ann, unadj_emission_output_rate, emission_input_rate, unadj_combust_heat_input, unadj_emission)

  ## Adjust emissions for renewable fuel types and select desired columns -----
  plant_adjusted <-
    plant_emissions %>%
    # set annual emissions to NA for renewable fuel types
    mutate(emission_ann = if_else(unadj_emission_ann == 0 & primary_fuel_type %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA_real_, unadj_emission_ann),
           # set output rate to 0 if annual net generation is less than 0
           emission_output_rate = if_else(generation_ann < 0, 0, unadj_emission_output_rate),
           year = params$eGRID_year)
  
  ## Assign emission sources to plant file -------
  plant_sources <- 
    unit_data %>%
    filter(!is.na(unadj_emission_source) | unadj_emission_source == "") %>%
    arrange(unadj_emission_source) %>% # sort by emissions source
    group_by(plant_id) %>%
    # concatenate source strings
    summarize(emission_source_combined = str_c(unique(unadj_emission_source), collapse = "; "), .groups = "drop") %>%
    # replace multiple sources with generalized multiple source assignment
    mutate(emission_source = if_else(grepl(";", emission_source_combined), 
                                                "EPA/NEI; Estimated using an emission source", 
                                                emission_source_combined)) %>%
    ungroup() %>%
    select(plant_id, emission_source)
  
  # update sources in plant file
  plant_formatted <-
    plant_adjusted %>%
    left_join(plant_sources, by = join_by(plant_id)) %>%
    # rename emissions input rate
    # select desired variables for final version
    select(year, plant_state, plant_name, plant_id, egrid_subregion, egrid_subregion_name, primary_fuel_type, nameplate_capacity, elec_allocation, generation_ann, combust_heat_input, unadj_emission_ann, emission_ann, unadj_emission_output_rate, emission_output_rate, emission_input_rate, emission_source, unadj_combust_heat_input, unadj_emission) %>%
    # replace emission with emission type in column names
    rename_with(~gsub("emission", emission_label, .)) %>%
    # order by plant state abbreviation and plant name
    arrange(plant_state, plant_name)
  
  return(plant_formatted)
}