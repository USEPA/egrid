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

# Define eGRID paramater year
# may need to be revised for more flexibility
params <- list()
params$eGRID_year <- "2023"

# Load data for desired output file conversion ----------------------------

# all output files needing conversions
orig_data_list <- c('unit_file','generator_file','plant_file','state_aggregation',
                  'ba_aggregation','subregion_aggregation','nerc_aggregation',
                  'us_aggregation','grid_gross_loss')
# which output file for script conversion
orig_data_name <- 'unit_file'

## Load unit dictionary and conversion table from Excel sheets ------------------------------

# dictionary for data tables
table_dir <- '/media/sdrive/projects/eGrid/production_model/users/russelle'
# table of conversion factors
conversion_factors <- read_csv(file.path(table_dir,'conversion_factors.csv'),
                               col_names=TRUE, col_types='ccn')
# .RDS output data
orig_data <-read_rds(glue::glue("data/outputs/{orig_data_name}.RDS")) 
# final structure and units of metric files
metric_structure <- read_excel(file.path(unit_dir,'metric_structure.xlsx'),
                               sheet=which(orig_data_list == orig_data_name)) 

# Combine output column names with units ----------------------------------

# combine data column names with list of metric and imperial units
unit_comparison <- cbind(colnames(orig_data),metric_structure[,3:4])
colnames(unit_comparison) <- c('var','metric','imperial')
# create vector of names with differing variables
units_for_conversion <- unit_comparison %>%
  filter(!is.na(metric) & metric != imperial)

# Testing Mutate ----------------------------------------------------------
metric_data <- output_file %>%
  mutate(across(.cols = any_of(as.vector(unlist(units_for_conversion['var']))),
                .fns = ~ . / 10,
                .names = '{.col}_metric'))
glimpse(metric_data)

# Reordering columns ------------------------------------------------------

# metric_data <- output_file %>%
#   mutate(across(.cols = contains('heat') & !contains('source'),
#                 .fns = ~ . / 10,
#                 .names = '{.col}_metric'))
# %>% need to relocate the columns
# relocate(sort(grep('heat_input_oz_[i-m]*',names(.),value=TRUE)), .after='heat_input') # & !names %in% c('source')),
  # relocate(
  #   output_file %>%
  #     sort(select(contains('heat') & !contains('source')))) # %>% names() %>% sort())
# %>% bind_cols(
#   df %>% 
#     select(
#       df %>% select(starts_with("data")) %>% names() %>% sort()
#relocate(contains('metric'), .after = heat_input)
#select(contains())
#relocate(sort(grep("^data", names(.), value = TRUE)), .before = data_col_1)

# ----------------------------

# Export file -------------

# check if data output folder exists, if not make folder
save_dir <- glue::glue("data/outputs/{params$eGRID_year")
if(dir.exists(save_dir)) {
  print(glue::glue("Folder {save_dir} already exists."))
} else {
  dir.create(save_dir)
  print(glue::glue("Folder {save_dir} created."))
}

# save folder to outputs file
print(glue::glue("Saving {filetype} metric file to folder {save_dir}"))
write_rds(units_formatted, glue::glue("{save_dir}/{output_file_type}_file_metric.RDS"))


