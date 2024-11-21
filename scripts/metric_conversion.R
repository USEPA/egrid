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
  if (is.character(data_col)) {
    #print('Variable is a character. Returning variable.')
    return (data_col)
  } else {
    #print(glue::glue('{var_name} converted from {var_data$imperial} to {var_data$metric} (conversion rate:{convert_factor})'))
    return (data_col * convert_factor)
  }
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
which_file <- "unit"
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
  filter(!is.na(metric) & metric != imperial)
  
vars_conversion_new <- vars_for_conversion %>%
  filter(!is.na(new_field)) %>% # is a new field
  # remove _metric naming to select original data for conversion
  mutate(var = stringr::str_remove(var,'_metric'))

# Convert data to new metric units ---------------------------------------------
### Create new metric data columns ---------------------------------------------

metric_data <- orig_data %>%
  # add duplicate rows for new metric variables
  mutate(across(.cols = any_of(as.vector(unlist(vars_conversion_new['var']))),
                .fns = ~ .,
                .names = '{.col}_metric')) %>%
  # convert units to metric
  mutate(across(.cols = any_of(as.vector(unlist(vars_for_conversion['var']))),
                .fns = ~ convert_to_metric(.,cur_column()))) %>%
  # select and order based on ordered names 
  select(all_of(ordered_names))
print(glue::glue('{nrow(vars_for_conversion)-nrow(vars_conversion_new)} columns altered'))
print(glue::glue('{ncol(metric_data) - ncol(orig_data)} columns added'))
# rename to longer variable names after sorting
names(metric_data) <- ordered_names

### Check for accurate conversions ---------------------------------------------

# for (val in vars_for_conversion$var) {
#   var_units <- vars_for_conversion %>%
#     filter(var == val)
#   convert_factor <- as.numeric(convert_rates %>%
#                                  filter(from==var_units$imperial &
#                                           to==var_units$metric) %>%
#                                  select(conversion))
#   if (is.numeric(unlist(metric_data[,val]))) {
#     true_diff <- (mean(unlist(metric_data[,val]) /
#                        unlist(orig_data[,stringr::str_remove(val,'_metric')]),
#                      na.rm = TRUE))
    #print(glue::glue('{val},{convert_factor == true_diff}'))
    #print(glue::glue('{convert_factor == true_diff},{convert_factor},{true_diff}'))
#  }
#}

# Export file -------------

# check if data output folder exists, if not make folder
save_dir <- glue::glue("data/outputs/{params$eGRID_year}")
if(dir.exists(save_dir)) {
  print(glue::glue("Folder {save_dir} already exists."))
} else {
  dir.create(save_dir)
  print(glue::glue("Folder {save_dir} created."))
}

# save folder to outputs file
print(glue::glue("Saving {filename_orig}_metric.RDS to {save_dir}"))
write_rds(metric_data, glue::glue("{save_dir}/{filename_orig}_metric.RDS"))
