## -------------------------------
##
## Final Formatting PM NH3 VOC
## 
## Purpose: 
## 
## This file pulls data from .RDS files to create final version of saved data in excel sheet
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries --------
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

emission_type <- "pm25"

# Import .RDS data -----

# create a list of files in R directory
data_dir <- glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/")
filename_types <- list("unit_file_",
                         "plant_file_",
                         "state_aggregation_",
                         "subregion_aggregation_")
filenames <- lapply(filename_types, function(filename) paste0(filename, paste0(emission_type, ".RDS")))

# import files in filenames list
for (file in (filenames)){
  assign(str_remove(file,glue::glue("_{emission_type}.RDS")), read_rds(paste0(data_dir, file)))
}

# TABLE 1 - UNIT FILE -----

## Set column titles and subtitles -----

# Load abbreviated name to snake_case matches
load("data/1_production_model/static_tables/name_matches.Rdata")

# add additional column names present in unit data
colnames_emissions <- setNames(c(paste0(emission_type, "_ann"), 
                                 paste0(emission_type, "_rate"), 
                                 paste0(emission_type, "_source")),
                               c(paste0(toupper(emission_type), "AN"), 
                                 paste0(toupper(emission_type), "RT"), 
                                 paste0(toupper(emission_type), "SRC")))

# select name matches present in unit data
colnames_all <- c(unit_nonmetric[unit_nonmetric %in% colnames(unit_file)], colnames_emissions) %>%
  print()

# rename unit column names
unit_file_renamed <-
  unit_file %>%
  rename(!!!setNames(lapply(colnames_all, sym), names(colnames_all)))

# write table data on sheets-----

## Define names for title and subtitles -----

## write in the data

## Format as needed and desired
