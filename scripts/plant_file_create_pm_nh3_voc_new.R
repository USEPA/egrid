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
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

# Load Libraries ---------
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
  if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/unit_file_{emission_type}.RDS"))) {
    unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file_{emission_type}.RDS")) #%>%
  } else {
    stop(glue::glue("unit_file_{emission_type}.RDS does not exist. Run pm_nh3_voc_unit_file_create.R to obtain."))}
  
  # Run plant data creation script ---------
  source("scripts/functions/function_plant_data_pm_nh3_voc.R")
  plant_data <- plant_data_pm_nh3_voc(emission_type)
  
  ## Assign emission sources to plant file -------
  # define source variable name
  source_var <- paste0(emission_type, "_source")
  sources <- 
    unit_file %>%
    filter(!is.na(get(source_var)) | get(source_var) != "") %>%
    group_by(plant_id) %>%
    arrange(get(source_var)) %>% # sort by PM2.5 source
    summarize("{emission_type}_source" := str_c(unique(get(source_var)), collapse = "; "), .groups = "drop") %>% # concatenate source strings
    ungroup()
  
  # update sources in plant file
  plant_sources <-
    plant_data %>%
    left_join(sources, by = join_by(plant_id))
  
  ## Format final version of plant file ------------
  # adjust emissions for renewable fuel types and select desired columns
  plant_formatted <-
    plant_sources %>%
    # replace emission with emission type in column names
    rename_with(~gsub("emission", emission_type, .)) %>%
    # set annual emissions to NA for renewable fuel types
    mutate("{emission_type}_ann" := if_else(get(paste0(emission_type, "_ann")) == 0 & primary_fuel_type %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA_real_, get(paste0(emission_type, "_ann"))),
           # set output rate to 0 if annual net generation is less than 0
           "{emission_type}_output_rate" := if_else(generation_ann < 0, 0, get(paste0(emission_type, "_output_rate"))),
           year = params$eGRID_year) %>%
    # select desired variables for final version
    select(year, plant_state, plant_name, plant_id, egrid_subregion, egrid_subregion_name, primary_fuel_type, nameplate_capacity, elec_allocation, generation_ann, heat_input, paste0(emission_type, "_ann"), paste0(emission_type, "_output_rate"), paste0(emission_type, "_input_rate"), paste0(emission_type, "_source"), unadj_combust_heat_input, paste0("unadj_", emission_type)) %>%
    # order by plant state abbreviation and plant name
    arrange(plant_state, plant_name)
  
  return(plant_formatted)
}

# Format final version of PM2.5, NH3, and VOC plant files ------------
pm_plant_formatted <- format_plant("pm25")
nh3_plant_formatted <- format_plant("nh3")
voc_plant_formatted <- format_plant("voc")

# Export plant files -----
source("scripts/functions/function_save_output_data.R")
save_output_data(pm_plant_formatted, "plant_file_pm25.RDS")
save_output_data(nh3_plant_formatted, "plant_file_nh3.RDS")
save_output_data(voc_plant_formatted, "plant_file_voc.RDS")
