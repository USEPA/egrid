## Purpose: 
## 
## This file creates the PM2.5 plant file for eGRID. 
## This file includes PM2.5 emission data, either calculated
## or estimated for the plants of the specified eGRID year
## 
## The method of PM2.5 calculations are listed within pm25_source
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

# Load necessary data --------------------
## PM2.5 unit file
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/pm_unit_file.RDS"))) {
  pm_unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/pm_unit_file.RDS"))
} else {
  stop("pm_unit_file.RDS does not exist. Run pm_unit_file_create.R to obtain.")}

## eGRID production model data - plant file
plant_file <- read_csv(glue::glue("data/outputs/{params$eGRID_year}/plant_file_2021_access.csv"), col_types = "ccccccccccicddddddddccccccccccc") %>%
  janitor::clean_names()


# Sum PM2.5 unit data by plant id ---------
plant_pm <-
  pm_unit_file %>%
  group_by(orispl) %>%
  summarise(pm25 = sum(pm25))

# Add PM2.5 data to plant file ---------
plant_pm_emissions <-
  plant_file %>%
  inner_join(plant_pm, by = join_by(orispl)) %>%
  # set plant electric allocation factors to 1 if NaN 
  mutate(elcalloc = if_else(is.na(elcalloc), 1, elcalloc),
         # calculate annual pm2.5 emissions
         plpm25an = pm25 * elcalloc,
         # calculate total output emission rate
         plpm25rta= plpm25an * 2000 / plngenan,
         # calculate total input emission rate
         plpm25ra = plpm25an * 2000 / plhtian,
         #  rename unadjusted annual pm2.5 emissions
         unpm25 = pm25) %>%
  select(pstatabb, pname, orispl, srname, subrgn, plprmfl, namepcap, elcalloc, plhtian, plngenan, plpm25an, plpm25rta, plpm25ra, unhti, unpm25)


# format final version of pm2.5 plant file ------------

#adjust pm2.5 emissions for renewable fuel types and select desired columns
# plant_pm_emissions_formatted <-
#   plant_pm_emissions %>%
#   # set pm2.5 annual emissions to NA for renewable fuel types
#   mutate(plpm25an2 = if_else(plpm25an == 0 & plprmfl %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA, plpm25an),
#          # set pm2.5 output rate to 0 is annual net generation is less than 0
#          plpm25rta2 = if_else(plngenan < 0, 0, plpm25rta),
#          pm25src = "") %>%
#   # select desired variables for final version
#   select(pstatabb, pname, orispl, subrgn, srname, plprmfl, namepcap, elcalloc, plngenan, plhtian, plpm25an2, plpm25rta2, plpm25ra, pm25src, unhti, unpm25) %>%
#   # order by plant state abbreviation and plant name
#   arrange(pstatabb, pname)


# export PM2.5 plant file ---------

# define name of saved file
save_file <- "pm_plant_file.RDS"

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
write_rds(plant_pm_emissions, glue::glue("data/outputs/{params$eGRID_year}/{save_file}"))

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/{save_file}"))){
  print(glue::glue("File {save_file} successfully written to folder data/outputs/{params$eGRID_year}"))
} else {
  print(glue::glue("File {save_file} failed to write to folder."))
} 