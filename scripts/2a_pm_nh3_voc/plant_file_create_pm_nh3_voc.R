## -------------------------------
##
## Plant file create PM NH3 VOC
##
## Purpose: 
## 
## This file creates the PM2.5, NH3, and VOC plant files for eGRID 
## using the function plant_data_pm_nh3_voc(). This file includes 
## emission data, either calculated or estimated for the 
## units of the specified eGRID year.
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

# Load libraries ---------
library(dplyr)
library(readr)
library(readxl)
library(stringr)

# Define eGRID year parameter ----------------
# define parameter year if no one is currently assigned using prompted user input
if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.")
  } else { # if params() is defined, but eGRID_year is not, define it here
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year)
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Create function to format plant files ---------------
format_plant <- function(emission_type) {
  
  # load unit file
  if(file.exists(glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/unit_file_{emission_type}.RDS"))) {
    unit_file <- read_rds(glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/unit_file_{emission_type}.RDS"))
  } else {
    stop(glue::glue("unit_file_{emission_type}.RDS does not exist. Run unit_file_create_pm_nh3_voc.R to obtain."))
    }
  
  ## Run plant data creation script ---------
  source("scripts/functions/function_plant_data_pm_nh3_voc.R")
  plant_data <- plant_data_pm_nh3_voc(emission_type)
  
  ## Set emission type label for data columns ----
  if (emission_type == "pm") {
    emission_label <- "pm25"
  } else {
    emission_label <- emission_type
  }
  
  ## Adjust emissions for renewable fuel types and select desired columns -----
  plant_adjusted <-
    plant_data %>%
    # replace emission with emission type in column names
    rename_with(~gsub("emission", emission_label, .)) %>%
    # set annual emissions to NA for renewable fuel types
    mutate("{emission_label}_ann" := if_else(get(paste0(emission_label, "_ann")) == 0 & primary_fuel_type %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA_real_, get(paste0(emission_label, "_ann"))),
           # set output rate to 0 if annual net generation is less than 0
           "{emission_label}_output_rate" := if_else(generation_ann < 0, 0, get(paste0(emission_label, "_output_rate"))),
           year = params$eGRID_year)
  
  ## Assign emission sources to plant file -------
  
  # assign emissions source variable name
  emissions_source_var <- paste0(emission_label, "_source")
  
  # assign emission sources to each plant
  plant_sources <- 
    unit_file %>%
    filter(!is.na(get(emissions_source_var)) | get(emissions_source_var) == "") %>%
    arrange(get(emissions_source_var)) %>% # sort by emissions source
    group_by(plant_id) %>%
    # concatenate source strings
    summarize("{emission_label}_source" := str_c(unique(get(emissions_source_var)), collapse = "; "), .groups = "drop") %>%
    # replace multiple sources with generalized multiple source assignment
    mutate("{emission_label}_source" := if_else(grepl(";", get(emissions_source_var)), 
                                               "EPA/NEI; Estimated using an emission source", 
                                               get(emissions_source_var))) %>%
    ungroup() %>%
    select(-contains("var"))

  # update sources in plant file
  plant_formatted <-
    plant_adjusted %>%
    left_join(plant_sources, by = join_by(plant_id)) %>%
    # select desired variables for final version
    select(year, plant_state, plant_name, plant_id, egrid_subregion, egrid_subregion_name, primary_fuel_type, nameplate_capacity, elec_allocation, generation_ann, combust_heat_input, paste0(emission_label, "_ann"), paste0(emission_label, "_output_rate"), paste0(emission_label, "_input_rate"), paste0(emission_label, "_source"), unadj_combust_heat_input, paste0("unadj_", emission_label)) %>%
    # order by plant state abbreviation and plant name
    arrange(plant_state, plant_name)
  
  return(plant_formatted)
}

# Format final version of PM2.5, NH3, and VOC plant files ------------
pm_plant_formatted <- format_plant("pm")
nh3_plant_formatted <- format_plant("nh3")
voc_plant_formatted <- format_plant("voc")

# Export plant files -----
source("scripts/functions/function_save_output_data.R")
output_folder <- "2a_pm_nh3_voc"
save_output_data(pm_plant_formatted, output_folder, "plant_file_pm.RDS")
save_output_data(nh3_plant_formatted, output_folder, "plant_file_nh3.RDS")
save_output_data(voc_plant_formatted, output_folder, "plant_file_voc.RDS")
