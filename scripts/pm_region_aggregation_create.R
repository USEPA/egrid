## Purpose: 
## 
## This file creates the PM2.5 state, US, and subregion files for eGRID. 
## This file includes PM2.5 emission data, either calculated
## or estimated for the plants of the specified eGRID year
## 
## The method of PM2.5 calculations are listed within pm25_source
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


# Load necessary data --------------------
## PM2.5 plant file
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/pm_plant_file.RDS"))) {
  pm_plant_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/pm_plant_file.RDS"))
} else {
  stop("pm_plant_file.RDS does not exist. Run pm_plant_file_create.R to obtain.")}


# Run plant data creation script ---------
source("scripts/functions/function_create_pm_plant_data.R")
pm_plant_data <- create_pm_plant_data()


# Sum PM2.5 plant data by subregion ---------
pm_subregion_emissions <-
  pm_plant_data %>%
  group_by(egrid_subregion) %>%
  summarise(generation_ann_sum = sum(generation_ann, na.rm = TRUE), pm25_ann_sum = sum(pm25_ann, na.rm = TRUE)) %>%
  mutate(generation_ann = round(generation_ann_sum, 0),
         pm25_tons = round(pm25_ann_sum, 2),
         pm25_rate = round(pm25_ann_sum * 2000 / generation_ann_sum, 4)) %>%
  select(egrid_subregion, generation_ann, pm25_tons, pm25_rate)


# Sum PM2.5 subregion data to US -------
pm_us_emissions <-
  pm_subregion_emissions %>%
  summarise(generation_ann = sum(generation_ann, na.rm = TRUE), pm25_tons = sum(pm25_tons, na.rm = TRUE)) %>%
  mutate(pm25_rate = round(pm25_tons * 2000 / generation_ann, 4))


# Sum PM2.5 plant data by state ---------
pm_state_emissions <-
  pm_plant_file %>%
  group_by(plant_state) %>%
  summarise(generation_ann = sum(generation_ann, na.rm = TRUE), pm25_ann = sum(pm25_ann, na.rm = TRUE), pm25_output_rate = pm25_ann * 2000 / generation_ann) %>%
  select(plant_state, generation_ann, pm25_ann, pm25_output_rate)


# Save aggregated data ----------
source("scripts/functions/function_save_output_data.R")
save_output_data(pm_subregion_emissions, "pm_subregion_aggregation.RDS")
save_output_data(pm_us_emissions, "pm_us_aggregation.RDS")
save_output_data(pm_state_emissions, "pm_state_aggregation.RDS")
