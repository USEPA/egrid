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

# Load necessary functions -----------------------

source("scripts/functions/function_params_check.R")
source("scripts/functions/function_temporal_res_cols.R")
source("scripts/functions/function_region_aggregation.R")

# Check for params() --------
params <- params_check()

# Create temporal_res_cols --------------------
temporal_res_cols <- create_temporal_res_cols(params$temporal_res)

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

