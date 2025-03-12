## -------------------------------
##
## Create metric versions of files.
## Call function metric_conversion to create metric files.
## 
## Purpose: 
## 
## This file creates new .RDS output files with unit conversions to metric
## when necessary utilizing metric structure excel sheet and conversion rates.
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries ------------------------------------------

library(dplyr)
library(glue)
library(readr)
library(readxl)
library(renv)
detach("package:renv", unload=TRUE)

# Define parameters -------------------------

#check if parameters for eGRID data year need to be defined
#this is only necessary when running the script outside of egrid_master.qmd
#user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.")
  } else { # if params() is defined, but eGRID_year is not, define it here
    params$eGRID_year <- as.character(readline(prompt = "Input eGRID_year: "))
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- as.character(readline(prompt = "Input eGRID_year: "))
}

# Define files to convert ---------------------------------

filenames_orig <- c("unit" = "unit_file",
                   "gen" = "generator_file",
                   "plant" = "plant_file",
                   "state" = "state_aggregation",
                   "ba" = "ba_aggregation",
                   "subregion" = "subregion_aggregation",
                   "nerc" = "nerc_aggregation",
                   "us" = "us_aggregation",
                   "ggl" = "grid_gross_loss")

# Load ordered variable names and conversion factors ---------

# ordered names and abbreviations
load("data/static_tables/name_matches.Rdata")

# conversion factors
convert_rates <- read_csv(file.path("data/static_tables/conversion_factors.csv"),
                          col_names=TRUE, col_types="ccn")

# Run metric conversion function for filenames  ----------------------

source("scripts/functions/function_metric_conversion.R")

for (file in names(filenames_orig)) {
  metric_conversion(file)
  }
