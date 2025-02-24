## -------------------------------
##
## PM unit file create
## 
## Purpose: 
## 
## This file creates the PM2.5 unit file for eGRID using the function
## create_pm_unit_data(). This file includes PM2.5 emission data, either
## calculated or estimated for the units of the specified eGRID year.
## 
## The method of PM2.5 calculations are listed within pm25_source.
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

# Run unit data creation script ---------
## 1) Direct Match - "NEI/EIA" --------------
# calculate pm2.5 data using direct unit match from EIA to NEI
## 2) Match by fuel type, unit firing type, and prime mover - "NEI avg EF - PM, fuel type, firing type" ----------
# calculate pm2.5 emissions using matching of fuel type, prime mover, and firing type
## 3) Match by fuel type and prime mover - "NEI avg EF - PM, fuel type" ----------
# calculate pm2.5 emissions using matching of prime mover and fuel type
## 4) Use emissions factors from AP-42 - "Estimated using an emissions factor" ---------
# calculate pm2.5 emissions based on emission factors in AP-42 report
#if there is a unit match with EIA-923, adjust pm2.5 by control efficiency

source("scripts/functions/function_create_pm_nh3_voc_unit_data.R")
pm_unit_data <- create_pm_nh3_voc_unit_data("pm25")

# Format final version of pm2.5 unit file ------------
#adjust pm2.5 emissions for renewable fuel types and select desired columns
pm_unit_formatted <-
  pm_unit_data %>%
  # set pm2.5 annual emissions to NA for renewable fuel types
  mutate(pm25_ann = if_else(primary_fuel_type %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA_real_, emission),
         # set pm2.5 source type to NA for renewable fuel types
         pm25_source = if_else(pm25_ann >= 0, emission_source, NA_character_),
         # add data column with adjusted pm2.5 rate
         pm25_rate = pm25_ann * 2000 / heat_input) %>%
  # select desired variables for final version
  select(plant_state, plant_name, plant_id, unit_id, prime_mover, operating_status, botfirty, primary_fuel_type, operating_hours, heat_input, pm25_ann, pm25_rate, heat_input_source, pm25_source, year_online)


# Export PM2.5 unit file ---------
source("scripts/functions/function_save_output_data.R")
save_output_data(pm_unit_formatted, "pm_unit_file.RDS")
