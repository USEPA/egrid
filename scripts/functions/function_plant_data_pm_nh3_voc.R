## -------------------------------
## 
## Plant data PM NH3 VOC function
##
## Purpose: 
## 
## This function creates emission plant data for PM2.5, NH3, and VOC 
## that are used to compute regional aggregated values. 
## The output is not the final version used in  the plant files 
## and are formatted in plant_file_create_pm_nh3_voc.
## 
## The method of emission calculations are listed within emission_source
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

plant_data_pm_nh3_voc <- function(emission_type){
  
  #' plant_data_pm_nh3_voc
  #' 
  #' Function to create pm2.5, nh3, or voc plant file data by aggregating unit data
  #' 
  #' @param emission_type Emission type to be calculated - either
  #'                      "pm25", "nh3", or "voc"
  #' @return Dataset with PM2.5 plant data in the format needed for 
  #'         regional aggregation
  #'         
  #' @examples 
  #' # Create PM2.5 plant data
  #' pm_plant_data <- plant_data_pm_nh3_voc("pm25")
  
  
  # Require libraries ---------
  require(dplyr)
  require(readr)
  require(readxl)

  # Load necessary data --------------------
  ## eGRID production model data - plant file (2021 & 2022)
  if(params$eGRID_year %in% c("2021", "2022")) {
    plant_file_raw <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"),
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

  # Run unit data creation script ---------
  source("scripts/functions/function_unit_data_pm_nh3_voc.R")
  unit_data <- unit_data_pm_nh3_voc(emission_type)
  
  # Sum emission unit data by plant id ---------
  plant_sum <-
    unit_data %>%
    group_by(plant_id) %>%
    summarise(emission_plant = if_else(all(is.na(emission)), NA_real_, sum(emission, na.rm = TRUE))) %>%
    ungroup()
  
  # Add emission data to plant file ---------
  plant_emissions <-
    plant_file %>%
    left_join(plant_sum, by = join_by(plant_id)) %>%
    # multiply emissions by electric allocation if available (not NA) 
    mutate(emission_ann = emission_plant * if_else(is.na(elec_allocation), 1, elec_allocation),
           # calculate total output emission rate
           emission_output_rate = if_else(generation_ann != 0, emission_ann * 2000 / generation_ann, NA_real_),
           # calculate total input emission rate
           emission_input_rate = if_else(combust_heat_input != 0, emission_ann * 2000 / combust_heat_input, NA_real_),
           #  rename unadjusted annual pm2.5 emissions
           unadj_emission = emission_plant) %>%
    select(plant_state, plant_name, plant_id, egrid_subregion_name, egrid_subregion, primary_fuel_type, nameplate_capacity, elec_allocation, combust_heat_input, generation_ann, emission_ann, emission_output_rate, emission_input_rate, unadj_combust_heat_input, unadj_emission)
  
  return(plant_emissions)
}
