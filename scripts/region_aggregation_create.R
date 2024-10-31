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

# Load aggregation function -------------

source("scripts/functions/function_region_aggregation.R")

# Call aggregation function for each region ------------

# state regional aggregation
state_agg <- region_aggregation(region = "state", region_cols = c(fips_state_code, state))

# balance authority regional aggregation
ba_agg <- region_aggregation(region = "ba", region_cols = c(ba_code, ba_name))

# NERC regional aggregation
nerc_agg <- region_aggregation(region = "nerc", region_cols = c(nerc))

# subgregion regional aggregation
subregion_agg <- region_aggregation(region = "subregion", region_cols = c(subregion, subregion_name))

# US aggregation
us_agg <- region_aggregation(region = "us", region_cols = NA)

