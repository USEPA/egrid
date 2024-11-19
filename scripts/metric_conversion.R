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

# Function to convert data to metric --------------------------------------------

convert_to_metric <- function(data_col,var_name) {
  # variable row
  var_data <- vars_for_conversion %>%
    filter(var == var_name)
  # find conversion rate for units in variable row
  convert_factor <- as.numeric(convert_rates %>%
                                    filter(from==var_data$imperial & to==var_data$metric) %>%
                                    select(conversion))
  # multiply data by conversion factor
  return (data_col * convert_factor)
}

# Function to name new metric columns ----------------------------------------
naming_metric <- function(var_name) {
  # variable row
  var_data <- vars_for_conversion %>%
    filter(var == var_name)
  # add '_metric' to name if it's a new field added to metric file
  new_name <- ifelse(is.na(var_data$new_field),
                        var_name,
                        glue::glue('{var_name}_metric'))
  return (new_name)
}

# Select file for conversion ----------------------------
num <- 1
#filetype
filetype = c('unit','generator','plant','state','ba','subregion',
             'nerc', 'us','grid_gross_loss')

# output files needing conversions
orig_data_list <- c('unit_file','generator_file','plant_file','state_aggregation',
                  'ba_aggregation','subregion_aggregation','nerc_aggregation',
                  'us_aggregation','grid_gross_loss')

# file for metric con version
orig_data_name <- orig_data_list[num]
file <- filetype[num]

# Load original, metric structure, and conversion rates data ------------------------------

# original output data
orig_data <-read_rds(glue::glue("data/outputs/{orig_data_name}.RDS")) 

# metric file structure
metric_struct <- read_excel('data/static_tables/metric_structure.xlsx',
                               sheet=which(orig_data_list == orig_data_name),
                               # sheet index is same index in list of data files
                               col_names = TRUE) # keep column names to include NA fields
# rename column names
colnames(metric_struct) <- c('descrip','name','metric','imperial','new_field')

# conversion factors
convert_rates <- read_csv(file.path('data/static_tables/conversion_factors.csv'),
                               col_names=TRUE, col_types='ccn')

# Assign names to metric structure data ----------------------------------

# load in ordered names with abbreviations
ordered_names <- load('data/static_tables/name_matches.Rdata')

# Add new column with ordered names
metric_struct_named <- metric_struct %>%
  mutate(var = unit_metric,.after=name) #need to make universal
metric_struct_named

# Define variables for conversion  -------------------------------------------------------------

# variables with differing metric and imperial units
vars_for_conversion <- metric_struct_named %>%
  filter(!is.na(metric) & metric != imperial) %>%
  mutate(var = ifelse(!is.na(new_field),stringr::str_remove(var,'_metric'),var))
vars_for_conversion
# Create new metric data columns ----------------------------------------------------------

# works for keeping data 
metric_data <- orig_data %>%
  # create new metric columns
  mutate(across(.cols = any_of(as.vector(unlist(vars_for_conversion['var']))),
                .fns = ~ convert_to_metric(.,cur_column()) ,
                .names = '{naming_metric(.col)}')) %>%
  select(all_of(unit_metric))
glimpse(metric_data)
names(metric_data) <- unit_metric
glimpse(metric_data)


# Check for accurate conversions ------------------------------
for (val in vars_for_conversion$var) {
  var_units <- vars_for_conversion %>%
    filter(var == val)
  # define conversion factor based on units and stored conversion rates
  convert_factor <- as.numeric(convert_rates %>%
                                 filter(from==var_units$imperial & to==var_units$metric) %>%
                                 select(conversion))
  true_diff <- (mean(unlist(metric_data[,glue::glue('{val}_metric')]) / unlist(metric_data[,val]),na.rm=TRUE))
  print(convert_factor == true_diff)
  }

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


