## -------------------------------
##
## Summary Statistics Emission Source
## 
## Purpose: 
## 
## This file pulls data from unit NH3 and VOC .RDS files to evaluate the differences across emission estimation sources  - used to evaluate the differences across estimation methods and determine the need for adjustment.
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries --------
library(dplyr)
library(janitor)
library(tidyr)
library(readr)

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

# Create save directory for QA outputs -----

if(dir.exists("data/2a_pm_nh3_voc/outputs/qa")) {
  print("Folder qa already exists.")
}else{
  dir.create("data/2a_pm_nh3_voc/outputs/qa")
}

if(dir.exists(glue::glue("data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}"))) {
  print(glue::glue("Folder qa/{params$eGRID_year} already exists."))
}else{
  dir.create(glue::glue("data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}"))
}

# set directory for saving files 
save_dir <- glue::glue("data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}/")


# Create function to summarize data -----
unit_summary <- function(emission_type, year = params$eGRID_year) {
  
  # Set emission type label for data columns
  if (emission_type == "pm") {
    emission_label <- "pm25"
  } else {
    emission_label <- emission_type
  }
  
  # summarize unit emissions data by emission source type
  source_summary <- read_rds(glue::glue("data/2a_pm_nh3_voc/outputs/{year}/unit_file_{emission_type}.RDS")) %>%
    filter(!is.na(get(paste0(emission_label, "_source")))) %>%
    group_by(get(paste0(emission_label, "_source"))) %>%
    summarize(count = n(), 
              emission_avg = mean(get(paste0(emission_label, "_ann")), na.rm = TRUE), 
              rate_avg = mean(get(paste0(emission_label, "_rate")), na.rm = TRUE)) %>%
    rename("{emission_label}_source" := `get(paste0(emission_label, "_source"))`)

  primary_fuel_summary <-  read_rds(glue::glue("data/2a_pm_nh3_voc/outputs/{year}/unit_file_{emission_type}.RDS")) %>%
    filter(!is.na(get(paste0(emission_label, "_source")))) %>%
    group_by(get(paste0(emission_label, "_source")), primary_fuel_type) %>%
    summarize(count = n(), 
              emission_avg = mean(get(paste0(emission_label, "_ann")), na.rm = TRUE), 
              rate_avg = mean(get(paste0(emission_label, "_rate")), na.rm = TRUE)) %>%
    rename("{emission_label}_source" := `get(paste0(emission_label, "_source"))`)
  
  total_summary <- 
    primary_fuel_summary %>%
    bind_rows(source_summary) %>%
    arrange(get(paste0(emission_label, "_source")), desc(is.na(primary_fuel_type))) %>%
    mutate(primary_fuel_type = if_else(is.na(primary_fuel_type), "Total", primary_fuel_type))

  # save output data in QA folder
  write_csv(total_summary, glue::glue("{save_dir}{emission_type}_emissions_source_comparison_{year}.csv"))
  
  print(paste(toupper(emission_type), "UNIT EMISSION TYPE QA COMPLETE"))
}

# Run and save summary statistics for different emission estimation sources -----
unit_summary("nh3")
unit_summary("voc")
