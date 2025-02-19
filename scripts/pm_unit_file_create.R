## -------------------------------
##
## PM unit file create
## 
## Purpose: 
## 
## This file creates the PM2.5 unit file for eGRID using the function
## create_pm_unit_data(). This file includes PM2.5 emission data, either
## calculated or estimated for the units of the specified eGRID year.
## 
## The method of PM2.5 calculations are listed within pm25_source.
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

# Load Libraries ---------
library(dplyr)
library(readr)
library(readxl)


# Define eGRID year parameter ----------------
# define parameter year if no one is currently assigned using prompted user input
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

# Run unit data creation script ---------
source("scripts/functions/function_create_pm_unit_data.R")
pm_unit_data <- create_pm_unit_data()

# Format final version of pm2.5 unit file ------------
#adjust pm2.5 emissions for renewable fuel types and select desired columns
pm_unit_formatted <-
  pm_unit_data %>%
  # set pm2.5 annual emissions to NA for renewable fuel types
  mutate(pm25an = if_else(fuelu1 %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA, pm25),
         # set pm2.5 source type to NA for renewable fuel types
         pm25src2 = if_else(pm25an >= 0, pm25_source, NA),
         # add data column with adjusted pm2.5 rate
         pm25rt = pm25an * 2000 / htian) %>%
  # select desired variables for final version
  select(pstatabb, pname, orispl, unitid, prmvr, untopst, botfirty, fuelu1, hrsop, htian, pm25an, pm25rt, htiansrc, pm25src2, untyronl)


# Export PM2.5 unit file ---------
# define name of saved file
save_file <- "pm_unit_file.RDS"

# create save directories if they don't exist
if(dir.exists("data/outputs")) {
  print("Folder outputs already exists.")
} else {
  dir.create("data/outputs")
}

if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
  print(glue::glue("Folder outputs/{params$eGRID_year} already exists."))
} else {
  dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
}

print(glue::glue("Saving PM2.5 unit file to folder data/outputs/{params$eGRID_year}"))

# save file
write_rds(pm_unit_formatted, glue::glue("data/outputs/{params$eGRID_year}/{save_file}"))

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/{save_file}"))){
  print(glue::glue("File {save_file} successfully written to folder data/outputs/{params$eGRID_year}"))
} else {
  print(glue::glue("File {save_file} failed to write to folder."))
} 