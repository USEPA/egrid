## Purpose: 
## 
## This file creates a function to create PM2.5, NH3, and VOC state, US, and subregion 
## files for eGRID. This file includes PM2.5 emission data, either 
## calculated or estimated for the plants of the specified eGRID year
## 
## The method of emission calculations are listed within emission_source
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

pm_nh3_voc_regional_aggregation <- function(emission_type) {
  
  #' pm_nh3_voc_regional_aggregation
  #' 
  #' Function to create pm2.5, nh3, or voc plant file data by aggregating unit data
  #' 
  #' @param emission_type Emission type to be calculated - either
  #'                      "pm25", "nh3", or "voc"
  #' @return Saved regional aggregation files which include
  #'         state, subregion, and US aggregated files
  #'         
  #' @examples 
  #' # Create PM2.5 subregion, state, and US aggregated files
  #' pm_nh3_voc_regional_aggregation("pm25")
  
  # Require libraries --------
  require(dplyr)
  require(readr)
  require(readxl)
  
  # Load plant data --------------------
  if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/{emission_type}_plant_file.RDS"))) {
    plant_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/{emission_type}_plant_file.RDS"))
  } else {
    stop("{emission_type}_plant_file.RDS does not exist. Run pm_nh3_voc_plant_file_create.R to obtain.")}
  
  # Run plant data creation script ---------
  source("scripts/functions/function_pm_nh3_voc_plant_data.R")
  plant_data <- pm_nh3_voc_plant_data(emission_type)
  
  # Sum emission plant data by subregion ---------
  subregion_emissions <-
    plant_data %>%
    rename_with(~gsub("emission", emission_type, .)) %>%
    group_by(egrid_subregion) %>%
    summarise(generation_ann_sum = sum(generation_ann, na.rm = TRUE), "{emission_type}_ann_sum" := sum(get(paste0(emission_type, "_ann")), na.rm = TRUE)) %>%
    mutate(generation_ann = round(generation_ann_sum, 0),
           "{emission_type}_tons" := round(get(paste0(emission_type, "_ann_sum")), 2),
           "{emission_type}_rate" := round(get(paste0(emission_type, "_ann_sum")) * 2000 / generation_ann_sum, 4)) %>%
    select(egrid_subregion, generation_ann, paste0(emission_type, "_tons"), paste0(emission_type, "_rate"))
  
  # Sum emission subregion data to US -------
  us_emissions <-
    subregion_emissions %>%
    summarise(generation_ann = sum(generation_ann, na.rm = TRUE), "{emission_type}_tons" := sum(get(paste0(emission_type, "_tons")), na.rm = TRUE)) %>%
    mutate("{emission_type}_rate" := round(get(paste0(emission_type, "_tons")) * 2000 / generation_ann, 4))
  
  
  # Sum emission plant data by state ---------
  state_emissions <-
    plant_file %>%
    group_by(plant_state) %>%
    summarise(generation_ann = sum(generation_ann, na.rm = TRUE), "{emission_type}_ann" := sum(get(paste0(emission_type, "_ann")), na.rm = TRUE), "{emission_type}_output_rate" := get(paste0(emission_type, "_ann")) * 2000 / generation_ann) %>%
    select(plant_state, generation_ann, paste0(emission_type, "_ann"), paste0(emission_type, "_output_rate"))
  
  
  # Save aggregated data ----------
  source("scripts/functions/function_save_output_data.R")
  save_output_data(subregion_emissions, paste0(emission_type, "_subregion_aggregation.RDS"))
  save_output_data(us_emissions, paste0(emission_type, "_us_aggregation.RDS"))
  save_output_data(state_emissions, paste0(emission_type, "_state_aggregation.RDS"))
}
