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
orig_data_name <- 'generator_file'

## Load unit dictionary and conversion table from Excel sheets ------------------------------

# .RDS output data
orig_data <-read_rds(glue::glue("data/outputs/{orig_data_name}.RDS")) 
glimpse(orig_data)

# final structure and units of metric files
metric_str <- read_excel('data/static_tables/metric_structure.xlsx',
                               sheet=which(orig_data_list == orig_data_name),
                               skip = 1,
                               col_names = c('descrip','name','metric',
                               'imperial','add_field'))

# table of conversion factors
conversion_rates <- read_csv(file.path('data/static_tables/conversion_factors.csv'),
                               col_names=TRUE, col_types='ccn')

# Combine output column names with units ----------------------------------

# aligning file column names with metric structure names
# alignment requires removal of duplicate columns
metric_orig <- metric_str %>%
  filter(is.na(add_field)) %>%
  mutate(var = colnames(orig_data),.after = name) 
# new variables can then be readded
metric_new <- metric_str %>%
  filter(!is.na(add_field)) %>%
  mutate(var = metric_orig$var[which(metric_orig$name == substr(name,1,nchar(name)-1))], 
         .after = name)
# and combined 
metric_comb <- rbind(metric_orig,metric_new)

# create vector of names with differing variables
units_for_conversion <- metric_comb %>%
  filter(!is.na(metric) & metric != imperial)
units_for_conversion
# Testing Mutate ----------------------------------------------------------
metric_data <- orig_data %>%
  mutate(across(.cols = any_of(as.vector(unlist(units_for_conversion['var']))),
                .fns = ~ . / 10,
                .names = '{.col}_metric'))
glimpse(metric_data)
glimpse(orig_data)
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


