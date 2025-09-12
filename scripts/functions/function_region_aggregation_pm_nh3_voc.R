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
## Authors:
##      Emma Russell, Abt Global
##
## -------------------------------

region_aggregation_pm_nh3_voc <- function(emission_type) {
  
  #' @name region_aggregation_pm_nh3_voc
  #' 
  #' Function to create PM2.5, NH3, or VOC state, subregion, and US aggregated
  #' data from the plant data
  #' 
  #' @param emission_type Emission type to be calculated - either
  #'                      "pm", "nh3", or "voc" (string)
  #' @return Saved regional aggregation files which include
  #'         state, subregion, and US aggregated files
  #'         
  #' @examples 
  #' # Create PM2.5 subregion, state, and US aggregated files
  #' region_aggregation_pm_nh3_voc("pm")
  
  # Require libraries --------
  require(dplyr)
  require(readr)
  require(readxl)
  
  # Set emission type label for data columns ----
  if (emission_type == "pm") {
    emission_label <- "pm25"
  } else {
    emission_label <- emission_type
  }
  
  # Load plant data --------------------
  if(file.exists(glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/plant_file_{emission_type}.RDS"))) {
    plant_data <- read_rds(glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/plant_file_{emission_type}.RDS")) %>%
      # replace emission label with emission for universal computation
      rename_with(~gsub(emission_label, "emission", .))
  } else {
    stop("plant_file_{emission_type}.RDS does not exist. Run plant_file_create_ pm_nh3_voc.R to obtain.")
    }
  
  # Sum emission plant data by subregion ---------
  subregion_emissions_initial <-
    plant_data %>%
    # group data by subregion
    group_by(egrid_subregion, egrid_subregion_name) %>%
    # sum annual generation and annual emissions data by subregion
    summarise(generation_ann_sum = sum(generation_ann, na.rm = TRUE), 
              emission_ann_sum = sum(emission_ann_orig, na.rm = TRUE)) %>%
    ungroup() %>%
    # round data and compute emissions output rate
    mutate(subregion_generation_ann = round(generation_ann_sum, 0),
           emission_ann = round(emission_ann_sum, 2),
           emission_output_rate = round(emission_ann_sum * 2000 / generation_ann_sum, 4),
           year = params$eGRID_year) %>%
    # rename subregion data columns
    rename(subregion = egrid_subregion, subregion_name = egrid_subregion_name) %>%
    # select desired variables for final version
    select(year, subregion, subregion_name, subregion_generation_ann, emission_ann, emission_output_rate)
  
  # Sum emission subregion data to US -------
  us_emissions <-
    subregion_emissions_initial %>%
    # sum annual generation and annual emissions across all subregions
    summarise(generation_ann = sum(subregion_generation_ann, na.rm = TRUE), 
              emission_ann = sum(emission_ann, na.rm = TRUE)) %>%
    # compute output rates from annual emissions and annual generation
    mutate(emission_output_rate = round(emission_ann * 2000 / generation_ann, 4),
           year = params$eGRID_year) %>%
    relocate(year, .before = generation_ann) %>%
    # replace emission with emission type in column names
    rename_with(~gsub("emission", emission_label, .))
  
  # Rename subregion data variables -----
  subregion_emissions <-
    subregion_emissions_initial %>%
    # replace emission with emission type in column names
    rename_with(~gsub("emission", emission_label, .)) %>%
    glimpse()
  
  # Sum emission plant data by state ---------
  state_emissions <-
    plant_data %>%
    group_by(plant_state) %>%
    # sum generation, annual emissions, and output rates by state
    summarise(state_generation_ann = sum(generation_ann, na.rm = TRUE), 
              emission_ann = sum(emission_ann, na.rm = TRUE),
              emission_output_rate = emission_ann * 2000 / state_generation_ann) %>%
    ungroup() %>%
    # replace year data
    mutate(year = params$eGRID_year) %>%
    # rename state variable
    rename(state = plant_state) %>%
    # select variables for final version
    select(year, state, state_generation_ann, emission_ann, emission_output_rate) %>%
    # replace emission with emission type in column names
    rename_with(~gsub("emission", emission_label, .))
  
  # Save aggregated data ----------
  source("scripts/functions/function_save_output_data.R")
  output_folder <- "2a_pm_nh3_voc"
  save_output_data(subregion_emissions, output_folder, glue::glue("subregion_aggregation_{emission_type}.RDS"))
  save_output_data(us_emissions, output_folder, glue::glue("us_aggregation_{emission_type}.RDS"))
  save_output_data(state_emissions, output_folder, glue::glue("state_aggregation_{emission_type}.RDS"))
}
