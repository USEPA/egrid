## Purpose: 
## 
## This file creates the PM2.5 plant file for eGRID using the function
## create_pm_plant_data(). This file includes PM2.5 emission data, either
## calculated or estimated for the plants of the specified eGRID year.
##  
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
library(stringr)


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


# Load necessary data --------------------
## PM2.5 unit file
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/pm_unit_file.RDS"))) {
  pm_unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/pm_unit_file.RDS")) #%>%
} else {
  stop("pm_unit_file.RDS does not exist. Run pm_unit_file_create.R to obtain.")}


# Run plant data creation script ---------
source("scripts/functions/function_create_pm_plant_data.R")
pm_plant_data <- create_pm_plant_data()

# Assign PM2.5 sources to plant file ---------
# list pm sources to add to plant files
pm_sources <- 
  pm_unit_file %>%
  filter(!is.na(pm25src2) | pm25src2 != "") %>%
  group_by(orispl) %>%
  arrange(pm25src2) %>% # sort by PM2.5 source
  summarize(pm25src = str_c(unique(pm25src2), collapse = "; "), .groups = "drop") # concatenate source strings

# update sources in plant file
pm_plant_sources <-
  pm_plant_data %>%
  left_join(pm_sources, by = join_by(orispl))

# Format final version of pm2.5 plant file ------------
# adjust pm2.5 emissions for renewable fuel types and select desired columns
pm_plant_formatted <-
  pm_plant_sources %>%
  # set pm2.5 annual emissions to NA for renewable fuel types
  mutate(plpm25an2 = if_else(plpm25an == 0 & plprmfl %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA, plpm25an),
         # set pm2.5 output rate to 0 is annual net generation is less than 0
         plpm25rta2 = if_else(plngenan < 0, 0, plpm25rta)) %>%
  # select desired variables for final version
  select(pstatabb, pname, orispl, subrgn, srname, plprmfl, namepcap, elcalloc, plngenan, plhtian, plpm25an2, plpm25rta2, plpm25ra, pm25src, unhti, unpm25) %>%
  # order by plant state abbreviation and plant name
  arrange(pstatabb, pname)


# Export PM2.5 plant file ---------
# define name of saved file
save_file <- "pm_plant_file.RDS"

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

print(glue::glue("Saving PM2.5 plant file to folder data/outputs/{params$eGRID_year}"))

# save file
write_rds(pm_plant_formatted, glue::glue("data/outputs/{params$eGRID_year}/{save_file}"))

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/{save_file}"))){
  print(glue::glue("File {save_file} successfully written to folder data/outputs/{params$eGRID_year}"))
} else {
  print(glue::glue("File {save_file} failed to write to folder."))
} 
