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
## Authors:
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

# Load functions -----
source("scripts/functions/function_plant_data_pm_nh3_voc.R")
source("scripts/functions/function_save_output_data.R")

# Produce plant data for PM2.5, NH3, and VOC -----
pm_plant_data <- plant_data_pm_nh3_voc("pm")
nh3_plant_data <- plant_data_pm_nh3_voc("nh3")
voc_plant_data <- plant_data_pm_nh3_voc("voc")

# Export plant files -----
output_folder <- "2a_pm_nh3_voc"
save_output_data(pm_plant_data, output_folder, "plant_file_pm.RDS")
save_output_data(nh3_plant_data, output_folder, "plant_file_nh3.RDS")
save_output_data(voc_plant_data, output_folder, "plant_file_voc.RDS")
