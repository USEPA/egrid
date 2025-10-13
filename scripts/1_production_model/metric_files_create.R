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
# Load necessary functions -----------------------

source("scripts/functions/function_metric_conversion.R")
source("scripts/functions/function_check_params.R")

# Define parameters -------------------------
# check if parameters need to be defined
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")
}

# Define files to convert ---------------------------------

filenames_orig <- c("unit" = "unit_file",
                    "generator" = "generator_file",
                    "plant" = "plant_file",
                    "state" = "state_aggregation",
                    "ba" = "ba_aggregation",
                    "subregion" = "subregion_aggregation",
                    "nerc" = "nerc_aggregation",
                    "us" = "us_aggregation",
                    "ggl" = "grid_gross_loss")

# Load ordered variable names and conversion factors ---------

# ordered names 
source("scripts/1_production_model/name_matching.R")
# conversion factors
convert_rates <- read_csv(file.path("data/1_production_model/static_tables/conversion_factors.csv"),
                          col_names=TRUE, col_types="ccn")

# Run metric conversion function for filenames  ----------------------

for (file in names(filenames_orig)) {
  metric_conversion(file)
}
