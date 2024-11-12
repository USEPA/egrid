## -------------------------------
##
## Metric file formatting. 
## Call function format_styles and format_region_m to format the metric file. 
## 
## Purpose: 
## 
## This file formats all of the outputs created for eGRID 
## This includes all operating units for the specified eGRID data year
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------

# Load libraries --------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(openxlsx)

### Load in data ------ 

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}


# load files (will need to be changed to for metric)
unt_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))
gen_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/generator_file.RDS"))
plnt_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS"))
st_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/state_aggregation.RDS"))
ba_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/ba_aggregation.RDS"))
srl_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/subregion_aggregation.RDS"))
nrl_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/nerc_aggregation.RDS"))
us_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/us_aggregation.RDS"))
ggl_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/grid_gross_loss.RDS"))


# extract last two digits of year for universal labeling
year <- as.numeric(params$eGRID_year) %% 1000

# set up output file
contents <- "data/static_tables/formatting/egrid_contents_page.xlsx" # may need adjustments for metric version
wb <- loadWorkbook(contents)

# load helper functions into script
source("scripts/functions/function_format_styles.R") # add once merge final formatting to development
source("scripts/functions/function_format_region_m.R") # create metric version of function

### Create styles ------

# create eGRID output style list using function
# s <- create_format_styles() 


