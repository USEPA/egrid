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

# Load libraries -----
library(dplyr)
library(glue)
library(readr)

# Load in conversion table
btu_gj_conversion <- 0.9478

# Define eGRID param year

# Load in unit file .RDS data
output_file_type <- 'plant'
output_file <- read_rds(glue::glue("data/outputs/{output_file_type}_file.RDS")) 
glimpse(output_file)

#'heat' = 'MMBtu' = 'GJ'
#'mass' = 'short_tons' = 'metric_tons'
#'generation' = 'MWh' = 'GJ' # plant has MMBtu to GJ
#'ch4','n2o','hg' = 'lbs' = 'kg'
#'out_emission_rate' : 'lbs/MWh' -> 'kg/MWh' AND 'kg/GJ'
#'in_emission_rate' : lbs/MMBtu -> kg/GJ
#'combust_out_emission_rate' : lbs/MMbtu -> kg/MWh and kg/GJ
#'



# Add new variables with metric conversions
newfilename <- output_file %>%
  mutate(across(.cols = c('heat_input','heat_input_oz') | contains('mass'), 
                .fns = ~ if_else() . / btu_gj_conversion))
  # if name is included, new column is made, else original column is replaced
glimpse(newfilename)
# unit file

# generator file

# plant file

# state file

# balancing authority file

# subregion file

# nerc region file

# US file

# grid gross loss

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


