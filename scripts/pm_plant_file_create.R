## Purpose: 
## 
## This file creates the PM2.5, NH3, and VOC plant files for eGRID 
## using the function create_pm_nh3_voc_plant_data(). This file includes 
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
emission_type <- "pm25"
plant_data <- pm_plant_data
unit_file <- pm_unit_file

format_plant <- function(plant_data, unit_file, emission_type) {
  ## Assign emission sources to plant file -------
  # define source variable name
  source_var <- paste0(emission_type, "_source")
  sources <- 
    unit_file %>%
    filter(!is.na(get(source_var)) | get(source_var) != "") %>%
    group_by(plant_id) %>%
    arrange(get(source_var)) %>% # sort by PM2.5 source
    summarize("{emission_type}_source" := str_c(unique(get(source_var)), collapse = "; "), .groups = "drop") # concatenate source strings
  
  # update sources in plant file
  plant_sources <-
    plant_data %>%
    left_join(sources, by = join_by(plant_id))
  
  ## Format final version of plant file ------------
  # adjust emissions for renewable fuel types and select desired columns
  plant_formatted <-
    plant_sources %>%
    # set annual emissions to NA for renewable fuel types
    mutate("{emission_type}_ann" := if_else(emission_ann == 0 & primary_fuel_type %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA_real_, emission_ann),
           # set output rate to 0 if annual net generation is less than 0
           "{emission_type}_output_rate" := if_else(generation_ann < 0, 0, emission_output_rate)) %>%
    # select desired variables for final version
    select(plant_state, plant_name, plant_id, egrid_subregion, egrid_subregion_name, primary_fuel_type, nameplate_capacity, elec_allocation, generation_ann, heat_input, paste0(emission_type, "_ann"), paste0(emission_type, "_output_rate"), paste0(emission_type, "_input_rate"), paste0(emission_type, "_source"), unadj_combust_heat_input, paste0("unadj_", emission_type)) %>%
    # order by plant state abbreviation and plant name
    arrange(plant_state, plant_name)
  
  return(plant_formatted)
}

# Load necessary data --------------------
## PM2.5 unit file
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/pm_unit_file.RDS"))) {
  pm_unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/pm_unit_file.RDS")) #%>%
    # rename(emission = glue::glue()
} else {
  stop("pm_unit_file.RDS does not exist. Run pm_unit_file_create.R to obtain.")}

# Run plant data creation script ---------
source("scripts/functions/function_create_pm_nh3_voc_plant_data.R")
pm_plant_data <- create_pm_nh3_voc_plant_data("pm25")
nh3_plant_data <- create_pm_nh3_voc_plant_data("nh3")
voc_plant_data <- create_pm_nh3_voc_plant_data("voc")

# Format final version of PM2.5, NH3, and VOC plant files ------------
pm_plant_formatted <- format_plant(pm_plant_data, pm_unit_file, "pm25")
nh3_plant_formatted <- format_plant(nh3_plant_data, nh3_unit_file, "nh3")
voc_plant_formatted <- format_plant(voc_plant_data, voc_unit_file, "voc")

# Export PM2.5 plant file -----
source("scripts/functions/function_save_output_data.R")
save_output_data(pm_plant_formatted, "pm_plant_file.RDS")
save_output_data(nh3_plant_formatted, "nh3_plant_file.RDS")
save_output_data(voc_plant_formatted, "voc_plant_file.RDS")

