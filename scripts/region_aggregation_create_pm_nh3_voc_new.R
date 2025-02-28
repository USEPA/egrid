## -------------------------------
##
## Region aggregation create PM NH3 VOC
##
## Purpose: 
## 
## This file creates PM2.5, NH3, and VOC state, US, and subregion 
## files for eGRID using the function region_aggregation_pm_nh3_voc. 
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
source("scripts/functions/function_region_aggregation_pm_nh3_voc.R")
region_aggregation_pm_nh3_voc("pm25")
region_aggregation_pm_nh3_voc("nh3")
region_aggregation_pm_nh3_voc("voc")
