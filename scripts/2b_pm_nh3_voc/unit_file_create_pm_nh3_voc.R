## -------------------------------
##
## Unit file create PM NH3 VOC
## 
## Purpose: 
## 
## This file creates the PM2.5, NH3, and VOC unit files for eGRID 
## using the function unit_data_pm_nh3_voc(). This file includes 
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

# Create function to format unit files ---------------
format_unit <- function(unit_emissions, emission_type) {
  unit_formatted <-
    unit_emissions %>%
    # replace emission with emission type in column names
    rename_with(~gsub("emission", emission_type, .)) %>%
    # set  annual emissions to NA for renewable fuel types
    mutate("{emission_type}_ann" := if_else(primary_fuel_type %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA_real_, get(emission_type)),
           # set emission source type to NA for renewable fuel types
           "{emission_type}_source" := if_else(get(paste0(emission_type, "_ann")) >= 0, get(paste0(emission_type, "_source")), NA_character_),
           # add data column with adjusted emission rate
           "{emission_type}_rate" := if_else(heat_input != 0, get(paste0(emission_type, "_ann")) * 2000 / heat_input, NA_real_),
           year = params$eGRID_year) %>%
    # select desired variables for final version
    select(year, plant_state, plant_name, plant_id, unit_id, prime_mover, operating_status, botfirty, primary_fuel_type, operating_hours, heat_input, paste0(emission_type, "_ann"), paste0(emission_type, "_rate"), heat_input_source, paste0(emission_type, "_source"), year_online)
  
  return(unit_formatted)
}

# Produce PM2.5, NH3, and VOC unit data using function ---------

#' Emissions are estimated by the following methods and order:
#'  1) Direct Match - calculate emissions data using direct unit match from EIA to NEI ("NEI/EIA")
#'  2) Match by fuel type, unit firing type, and prime mover - calculate emissions using matching of fuel type, prime mover, and firing type ("NEI avg EF - PM, fuel type, firing type")
#'  3) Match by fuel type and prime mover - calculate emissions using matching of prime mover and fuel type ("NEI avg EF - PM, fuel type")
#'  4) Use emissions factors from AP-42 - calculate pm2.5 emissions based on emission factors in AP-42 report. If there is a unit match with EIA-923, adjust pm2.5 by control efficiency ("Estimated using an emissions factor")

# run script to produce unit data
source("scripts/functions/function_unit_data_pm_nh3_voc.R")
pm_unit_data <- unit_data_pm_nh3_voc("pm25")
nh3_unit_data <- unit_data_pm_nh3_voc("nh3")
voc_unit_data <- unit_data_pm_nh3_voc("voc")

# Format final version of PM2.5, NH3, and VOC unit files ------------
pm_unit_formatted <- format_unit(pm_unit_data, "pm25")
nh3_unit_formatted <- format_unit(nh3_unit_data, "nh3")
voc_unit_formatted <- format_unit(voc_unit_data, "voc")
  
# Export emission unit files ---------
source("scripts/functions/function_save_output_data.R")
output_folder <- "2b_pm_nh3_voc"
save_output_data(pm_unit_formatted, output_folder, "unit_file_pm25.RDS")
save_output_data(nh3_unit_formatted, output_folder, "unit_file_nh3.RDS")
save_output_data(voc_unit_formatted, output_folder, "unit_file_voc.RDS")

