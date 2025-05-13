## -------------------------------
##
## Region aggregation PM NH3 VOC function
##
## Purpose: 
## 
## This function creates and saves PM2.5, NH3, and VOC state, US, and subregion 
## files for eGRID.
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

region_aggregation_pm_nh3_voc <- function(emission_type) {
  
  #' region_aggregation_pm_nh3_voc
  #' 
  #' Function to create pm2.5, nh3, or voc state, subregion, and US aggregated
  #' data from the plant data
  #' 
  #' @param emission_type Emission type to be calculated - either
  #'                      "pm25", "nh3", or "voc"
  #' @return Saved regional aggregation files which include
  #'         state, subregion, and US aggregated files
  #'         
  #' @examples 
  #' # Create PM2.5 subregion, state, and US aggregated files
  #' region_aggregation_pm_nh3_voc("pm25")
  
  # Require libraries --------
  require(dplyr)
  require(readr)
  require(readxl)
  
  # Load plant data --------------------
  if(file.exists(glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/plant_file_{emission_type}.RDS"))) {
    plant_file <- read_rds(glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/plant_file_{emission_type}.RDS"))
  } else {
    stop("plant_file_{emission_type}.RDS does not exist. Run plant_file_create_ pm_nh3_voc.R to obtain.")}
  
  # Run plant data creation script ---------
  source("scripts/functions/function_plant_data_pm_nh3_voc.R")
  plant_data <- plant_data_pm_nh3_voc(emission_type)

  # Sum emission plant data by subregion ---------
  subregion_emissions <-
    plant_data %>%
    rename_with(~gsub("emission", emission_type, .)) %>%
    group_by(egrid_subregion, egrid_subregion_name) %>%
    summarise(generation_ann_sum = sum(generation_ann, na.rm = TRUE), 
              "{emission_type}_ann_sum" := sum(get(paste0(emission_type, "_ann")), na.rm = TRUE)) %>%
    mutate(subregion_generation_ann = round(generation_ann_sum, 0),
           "{emission_type}_ann" := round(get(paste0(emission_type, "_ann_sum")), 2),
           "{emission_type}_output_rate" := round(get(paste0(emission_type, "_ann_sum")) * 2000 / generation_ann_sum, 4),
           year = params$eGRID_year) %>%
    ungroup() %>%
    select(year, 
           subregion = egrid_subregion, 
           subregion_name = egrid_subregion_name, 
           subregion_generation_ann, 
           paste0(emission_type, "_ann"), 
           paste0(emission_type, "_output_rate"))
  
  # Sum emission subregion data to US -------
  us_emissions <-
    subregion_emissions %>%
    summarise(generation_ann = sum(subregion_generation_ann, na.rm = TRUE), 
              "{emission_type}_ann" := sum(get(paste0(emission_type, "_ann")), na.rm = TRUE)) %>%
    mutate("{emission_type}_output_rate" := round(get(paste0(emission_type, "_ann")) * 2000 / generation_ann, 4),
           year = params$eGRID_year) %>%
    relocate(year, .before = generation_ann)
  
  # Sum emission plant data by state ---------
  state_emissions <-
    plant_file %>%
    group_by(plant_state) %>%
    summarise(state_generation_ann = sum(generation_ann, na.rm = TRUE), 
              "{emission_type}_ann" := sum(get(paste0(emission_type, "_ann")), na.rm = TRUE),
              "{emission_type}_output_rate" := get(paste0(emission_type, "_ann")) * 2000 / state_generation_ann) %>%
    mutate(year = params$eGRID_year) %>%
    select(year, 
           state = plant_state, 
           state_generation_ann, 
           paste0(emission_type, "_ann"), 
           paste0(emission_type, "_output_rate"))
  
  # Save aggregated data ----------
  source("scripts/functions/function_save_output_data.R")
  output_folder <- "2b_pm_nh3_voc"
  save_output_data(subregion_emissions, output_folder, glue::glue("subregion_aggregation_{emission_type}.RDS"))
  save_output_data(us_emissions, output_folder, glue::glue("us_aggregation_{emission_type}.RDS"))
  save_output_data(state_emissions, output_folder, glue::glue("state_aggregation_{emission_type}.RDS"))
}
