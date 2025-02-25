## Purpose: 
## 
## This file creates PM2.5, NH3, and VOC state, US, and subregion 
## files for eGRID using the function pm_nh3_voc_regional_aggregation. 
## This file includes PM2.5 emission data, either 
## calculated or estimated for the plants of the specified eGRID year
## 
## The method of emission calculations are listed within emission_source
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

# Run aggregation data function to produce and save files --------
source("scripts/functions/function_pm_nh3_voc_regional_aggregation.R")
pm_nh3_voc_regional_aggregation("pm25")
pm_nh3_voc_regional_aggregation("nh3")
pm_nh3_voc_regional_aggregation("voc")
