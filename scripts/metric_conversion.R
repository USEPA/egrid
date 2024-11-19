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

# Load libraries and define parameters ------------------------------------------

library(dplyr)
library(glue)
library(readr)
library(readxl)

# Define eGRID paramater year
# may need to be revised for more flexibility
params <- list()
params$eGRID_year <- "2023"

# Define Functions -------------------------------------------------------------
### Function to convert data to metric -----------------------------------------

convert_to_metric <- function(data_col,var_name) {
  # variable row
  var_data <- vars_for_conversion %>%
    filter(var == var_name)
  # conversion rate for units
  convert_factor <- as.numeric(convert_rates %>%
                                    filter(from==var_data$imperial & 
                                             to==var_data$metric) %>%
                                    select(conversion))
  # multiply data by conversion factor
  return (data_col * convert_factor)
}

### Function to name new metric columns ----------------------------------------

naming_metric <- function(var_name) {
  # variable row
  var_data <- vars_for_conversion %>%
    filter(var == var_name)
  # add '_metric' to name if new field
  new_name <- ifelse(is.na(var_data$new_field),
                        var_name,
                        glue::glue('{var_name}_metric'))
  return (new_name)
}

# Load and assign original (nonconverted) data ---------------------------------
### Select file for conversion -------------------------------------------------

# output files needing conversions
filenames_orig <- c('unit' = 'unit_file',
                    'gen' = 'generator_file',
                    'plant' = 'plant_file',
                    'state' = 'state_aggregation',
                    'ba' = 'ba_aggregation',
                    'subregion' = 'subregion_aggregation',
                    'nerc' = 'nerc_aggregation',
                    'us' = 'us_aggregation',
                    'ggl' = 'grid_gross_loss')

# file for metric conversion
which_file <- "plant"
filename_orig <- filenames_orig[which_file]

# load in ordered names with abbreviations
load('data/static_tables/name_matches.Rdata')

# assign ordered name vector for file type
assign('ordered_names',get(glue::glue('{which_file}_metric')))

### Load original, metric structure, and conversion rates data ------------------

# original output data
orig_data <-read_rds(glue::glue("data/outputs/{filename_orig}.RDS")) 

# metric file structure
metric_struct <- read_excel('data/static_tables/metric_structure.xlsx',
                               sheet=which(filenames_orig == filename_orig),
                               # sheet index is same index in list of data files
                               col_names = TRUE) 
                               # keep column names to include NA fields
# rename column names
colnames(metric_struct) <- c('descrip','name','metric','imperial','new_field')

# conversion factors
convert_rates <- read_csv(file.path('data/static_tables/conversion_factors.csv'),
                               col_names=TRUE, col_types='ccn')

# Format data and identify variables needing converting ------------------------
### Assign names to metric structure data --------------------------------------

# Add new column with ordered names
metric_struct_named <- metric_struct %>%
  mutate(var = ordered_names,.after=name)

### Define variables for conversion  -------------------------------------------

vars_for_conversion <- metric_struct_named %>%
  # variables with differing metric and imperial units
  filter(!is.na(metric) & metric != imperial) %>%
  # remove _metric naming to select original data for conversion
  mutate(var = ifelse(!is.na(new_field),stringr::str_remove(var,'_metric'),var))

# Convert data to new metric units ---------------------------------------------
### Create new metric data columns ---------------------------------------------

metric_data <- orig_data %>%
  # create new metric columns using convert_to_metric()
  mutate(across(.cols = any_of(as.vector(unlist(vars_for_conversion['var']))),
                .fns = ~ convert_to_metric(.,cur_column()) ,
                .names = '{naming_metric(.col)}')) %>% # name with naming_metric()
  # select and order based on ordered names 
  select(all_of(ordered_names))
glimpse(metric_data)
# rename to longer variable names after sorting
names(metric_data) <- ordered_names

### Check for accurate conversions ---------------------------------------------

for (val in vars_for_conversion$var) {
  var_units <- vars_for_conversion %>%
    filter(var == val)
  convert_factor <- as.numeric(convert_rates %>%
                                 filter(from==var_units$imperial & 
                                          to==var_units$metric) %>%
                                 select(conversion))
  true_diff <- (mean(unlist(metric_data[,glue::glue('{val}_metric')]) /
                       unlist(metric_data[,val]),
                     na.rm = TRUE))
  print(convert_factor == true_diff)
}

# ----------------------------

# Export file -------------

# # check if data output folder exists, if not make folder
# save_dir <- glue::glue("data/outputs/{params$eGRID_year")
# if(dir.exists(save_dir)) {
#   print(glue::glue("Folder {save_dir} already exists."))
# } else {
#   dir.create(save_dir)
#   print(glue::glue("Folder {save_dir} created."))
# }
# 
# # save folder to outputs file
# print(glue::glue("Saving {filetype} metric file to folder {save_dir}"))
# write_rds(units_formatted, glue::glue("{save_dir}/{output_file_type}_file_metric.RDS"))


