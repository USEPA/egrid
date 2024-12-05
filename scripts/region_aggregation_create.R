## -------------------------------
##
## Create region aggregation files. 
## Call function region_aggregation to create region aggregation files. 
## 
## Purpose: 
## 
## This file creates the state, balance authority, NERC, eGRID subregion, and US 
## aggregation files for eGRID. 
##
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries --------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# Check for params() --------

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

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

# Load aggregation function -------------

source("scripts/functions/function_region_aggregation.R")

# Call aggregation function for each region ------------

# state regional aggregation
state_agg <- region_aggregation(region = "state", region_cols = c(state, fips_state_code))

# balance authority regional aggregation
ba_agg <- region_aggregation(region = "ba", region_cols = c(ba_name, ba_code))

# NERC regional aggregation
nerc_agg <- region_aggregation(region = "nerc", region_cols = c(nerc, nerc_name))

# subgregion regional aggregation
subregion_agg <- region_aggregation(region = "subregion", region_cols = c(subregion, subregion_name))

# US aggregation
us_agg <- region_aggregation(region = "us", region_cols = NA)

