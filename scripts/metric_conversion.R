## -------------------------------
##
## Metric conversion file
## 
## Purpose: 
## 
## This file creates new .RDS output files with unit conversions to metric
## when necessary utilizing conversion table with conversion rates
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries and define parameters ----------------------------------------------------------

library(dplyr)
library(glue)
library(readr)
library(readxl)

# Define eGRID param year
#params$eGRIDyear

# Load data for desired output file conversion ----------------------------

# all output files needing conversions
output_file_list <- c('unit_file','generator_file','plant_file','state_aggregation',
                  'ba_aggregation','subregion_aggregation','nerc_aggregation',
                  'us_aggregation','grid_gross_loss')

# which output file for script conversion
output_file <- 'unit_file'

## Load unit dictionary and conversion table from Excel sheets ------------------------------

# dictionary of units and conversions
name_and_units <- read_excel('/media/sdrive/projects/eGrid/production_model/users/russelle/data_dict.xlsx',
                             sheet=which(output_file_list == output_file)) 
                # sheet number is index of output_file in output_file_list

# table of conversion factors
#conversion_factors <-

# .RDS output file
output_file <- read_rds(glue::glue("data/outputs/{output_file}.RDS"))


# Combine output column names with units ----------------------------------

units_colnames <- cbind(name_and_units['Unit'],colnames(output_file))
units_colnames

# call unit for specified column name
test <- units_colnames[0,"colnames(output_file)" == 'heat_input']
test


# Export file -------------

# check if data output folder exists, if not make folder
save_dir <- glue::glue("data/outputs/{params$eGRID_year")
if(dir.exists(save_dir)) {
  print(glue::glue("Folder {save_dir} already exists."))
}else{
  dir.create(save_dir)
}

# save folder to outputs file
print(glue::glue("Saving {filetype} metric file to folder {save_dir}"))
write_rds(units_formatted, glue::glue("{save_dir}/{output_file_type}_file_metric.RDS"))


