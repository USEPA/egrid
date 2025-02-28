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

source("scripts/functions/function_params_check.R")
params <- params_check()

# Specify grouping columns based on temporal_res parameter
temporal_res_cols_all <- 
  list("annual"  = c("year"), 
       "monthly" = c("year", "month"), 
       "daily"   = c("year", "month", "day"), 
       "hourly"  = c("year", "month", "day", "hour"))

temporal_res_cols <- unlist(temporal_res_cols_all[params$temporal_res], use.names = FALSE)

# Load aggregation function -------------

source("scripts/functions/function_region_aggregation.R")

# Load ordered names and abbreviations -------------
load("data/static_tables/name_matches.Rdata")

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

