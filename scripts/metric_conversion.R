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

# Create function to convert desired column data to metric --------------

convert_to_metric <- function(data_col,var_name) {
  # identify metric and imperial units for variable
  var_units <- vars_for_conversion %>%
    filter(var == var_name)
  # define conversion factor based on units and stored conversion rates
  convert_factor <- as.numeric(convert_rates %>%
                                    filter(from==var_units$imperial & to==var_units$metric) %>%
                                    select(conversion))
  # multiply column data by conversion factor
  return (data_col * convert_factor)
}

# List original output files and select file for conversion ----------------------------

# output files needing conversions
orig_data_list <- c('unit_file','generator_file','plant_file','state_aggregation',
                  'ba_aggregation','subregion_aggregation','nerc_aggregation',
                  'us_aggregation','grid_gross_loss')

# select file to create metric version
orig_data_name <- 'generator_file'

## Read in original output data, metric structure information, and conversion rates ------------------------------

# original output data in .RDS form
orig_data <-read_rds(glue::glue("data/outputs/{orig_data_name}.RDS")) 

# desired metric file structure with which variables to convert to metric
metric_struct <- read_excel('data/static_tables/metric_structure.xlsx',
                               sheet=which(orig_data_list == orig_data_name),
                               # sheet index is the same index in list of data files
                               skip = 1, # skip first row with names
                               col_names = c('descrip','name','metric',
                               'imperial','new_field'))

# table of conversion factors from imperial to metric
convert_rates <- read_csv(file.path('data/static_tables/conversion_factors.csv'),
                               col_names=TRUE, col_types='ccn')
                               #column types are character, character, numeric

# Align .RDS variable names with metric structure names ----------------------------------

# remove duplicate columns, those being added to metric file
orig_vars <- metric_struct %>%
  filter(is.na(new_field)) %>% # is not listed as a new field
  mutate(var = colnames(orig_data),.after = name)

# define correct variable name for new variables
new_vars <- metric_struct %>%
  filter(!is.na(new_field)) %>% # is listed as a new field
  mutate(var = orig_vars$var[which(orig_vars$name == substr(name,1,nchar(name)-1))], 
         .after = name) # assign same name as the original variable
# recombine data with appropriate variable name assignments
metric_struct_named <- rbind(orig_vars,new_vars)


# Identify variables that need to be converted  -------------------------------------------------------------

# create vector of variable names with differing metric and imperial units
vars_for_conversion <- metric_struct_named %>%
  filter(!is.na(metric) & metric != imperial)

# Testing Mutate ----------------------------------------------------------

metric_data <- orig_data %>%
  # create new metric columns
  mutate(across(.cols = any_of(as.vector(unlist(units_for_conversion['var']))),
                .fns = ~ convert_metric(.,cur_column()) ,
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


