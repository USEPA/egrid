## Purpose: 
## 
## This file creates the PM2.5 plant file for eGRID using the function
## create_pm_plant_data(). This file includes PM2.5 emission data, either
## calculated or estimated for the plants of the specified eGRID year.
##  
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


# Load necessary data --------------------
## PM2.5 unit file
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/pm_unit_file.RDS"))) {
  pm_unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/pm_unit_file.RDS")) #%>%
} else {
  stop("pm_unit_file.RDS does not exist. Run pm_unit_file_create.R to obtain.")}


# Run plant data creation script ---------
source("scripts/functions/function_create_pm_plant_data.R")
pm_plant_data <- create_pm_plant_data()

# Assign PM2.5 sources to plant file ---------
# list pm sources to add to plant files
pm_sources <- 
  pm_unit_file %>%
  filter(!is.na(pm25_source) | pm25_source != "") %>%
  group_by(plant_id) %>%
  arrange(pm25_source) %>% # sort by PM2.5 source
  summarize(pm25_source = str_c(unique(pm25_source), collapse = "; "), .groups = "drop") # concatenate source strings

# update sources in plant file
pm_plant_sources <-
  pm_plant_data %>%
  left_join(pm_sources, by = join_by(plant_id))


# Format final version of pm2.5 plant file ------------
# adjust pm2.5 emissions for renewable fuel types and select desired columns
pm_plant_formatted <-
  pm_plant_sources %>%
  # set pm2.5 annual emissions to NA for renewable fuel types
  mutate(pm25_ann = if_else(pm25_ann == 0 & primary_fuel_type %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA, pm25_ann),
         # set pm2.5 output rate to 0 if annual net generation is less than 0
         pm25_output_rate = if_else(generation_ann < 0, 0, pm25_output_rate)) %>%
  # select desired variables for final version
  select(plant_state, plant_name, plant_id, egrid_subregion, egrid_subregion_name, primary_fuel_type, nameplate_capacity, elec_allocation, generation_ann, heat_input, pm25_ann, pm25_output_rate, pm25_input_rate, pm25_source, unadj_combust_heat_input, unadj_pm25) %>%
  # order by plant state abbreviation and plant name
  arrange(plant_state, plant_name)


# Export PM2.5 plant file -----
source("scripts/functions/function_save_output_data.R")
save_output_data(pm_plant_formatted, "pm_plant_file.RDS")
