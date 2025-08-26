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
## Authors:
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

# Load functions -----
# function to produce unit emissions data
source("scripts/functions/function_unit_data_pm_nh3_voc.R")
source("scripts/functions/function_save_output_data.R")

# Produce PM2.5, NH3, and VOC unit data using function ---------

#' Emissions are estimated by the following methods and order:
#'  1) Direct Match - calculate emissions data using direct unit match from EIA to NEI ("NEI/EIA")
#'  2) Match by fuel type, unit firing type, and prime mover - calculate emissions using matching of fuel type, prime mover, and firing type ("NEI avg EF - PM, fuel type, firing type")
#'  3) Match by fuel type and prime mover - calculate emissions using matching of prime mover and fuel type ("NEI avg EF - PM, fuel type")
#'  4) Use emissions factors from AP-42 - calculate pm2.5 emissions based on emission factors in AP-42 report. If there is a unit match with EIA-923, adjust PM2.5 by control efficiency ("Estimated using an emissions factor")

# run script to produce unit data
pm_unit_data <- unit_data_pm_nh3_voc("pm")
nh3_unit_data <- unit_data_pm_nh3_voc("nh3")
voc_unit_data <- unit_data_pm_nh3_voc("voc")

# Export emission unit files ---------
output_folder <- "2a_pm_nh3_voc"
save_output_data(pm_unit_data, output_folder, "unit_file_pm.RDS")
save_output_data(nh3_unit_data, output_folder, "unit_file_nh3.RDS")
save_output_data(voc_unit_data, output_folder, "unit_file_voc.RDS")

