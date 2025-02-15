## Purpose: 
## 
## This file creates the PM2.5 state, US, and subregion files for eGRID. 
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
## PM2.5 plant file
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/pm_plant_file.RDS"))) {
  pm_plant_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/pm_plant_file.RDS")) %>%
    mutate(orispl = as.numeric(orispl))
} else {
  stop("pm_plant_file.RDS does not exist. Run pm_plant_file_create.R to obtain.")}

pm_comparison <- read_csv("data/outputs/2023/access_qa/pm_sum_to_plant.csv") %>%
  janitor::clean_names() %>%
  inner_join(pm_plant_file, by = "orispl") %>%
  mutate(diff = abs(plpm25an.x - plpm25an.y)) %>%
  select(orispl, pm25_access = plpm25an.x, pm25_R = plpm25an.y, diff) %>%
  filter(is.na(diff))


# Sum PM2.5 plant data by subregion ---------
subregion_pm_emissions <-
  pm_plant_file %>%
  group_by(subrgn) %>%
  summarise(srgenan = sum(plngenan, na.rm = TRUE), srpm25an = sum(plpm25an, na.rm = TRUE)) %>%
  mutate(gen = round(srgenan, 0),
         pm25tons = round(srpm25an, 2),
         rate = round(srpm25an * 2000 / srgenan, 4)) %>%
  select(subrgn, gen, pm25tons, rate)

# Sum PM2.5 subregion data to US -------
us_pm_emissions <-
  subregion_pm_emissions %>%
  summarise(gen = sum(gen, na.rm = TRUE), pm25tons = sum(pm25tons, na.rm = TRUE)) %>%
  mutate(rate = round(pm25tons * 2000 / gen, 4))


# format final version of pm2.5 plant file ------------

# adjust pm2.5 emissions for renewable fuel types and select desired columns
plant_pm_emissions_formatted <-
  pm_plant_file %>%
  # set pm2.5 annual emissions to NA for renewable fuel types
  mutate(plpm25an2 = if_else(plpm25an == 0 & plprmfl %in% c("WAT", "SUN", "MWH", "WND", "WH", "PUR", "GEO", "NUC"), NA, plpm25an),
         # set pm2.5 output rate to 0 is annual net generation is less than 0
         plpm25rta2 = if_else(plngenan < 0, 0, plpm25rta),
         pm25src = "") %>%
  # select desired variables for final version
  select(pstatabb, pname, orispl, subrgn, srname, plprmfl, namepcap, elcalloc, plngenan, plhtian, plpm25an2, plpm25rta2, plpm25ra, pm25src, unhti, unpm25) %>%
  # order by plant state abbreviation and plant name
  arrange(pstatabb, pname)


# Sum PM2.5 plant data by state ---------
state_pm_emissions <-
  plant_pm_emissions_formatted %>%
  group_by(pstatabb) %>%
  summarise(stgenan = sum(plngenan, na.rm = TRUE), stpm25an = sum(plpm25an2, na.rm = TRUE), stpm25rta = stpm25an * 2000 / stgenan) %>%
  select(pstatabb, stgenan, stpm25an, stpm25rta)

# # export all aggregated files ---------
# 
# # define name of saved file
# save_file <- "pm_plant_file.RDS"
# 
# # create save directories if they don't exist
# if(dir.exists("data/outputs")) {
#   print("Folder outputs already exists.")
# } else {
#   dir.create("data/outputs")
# }
# 
# if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
#   print(glue::glue("Folder outputs/{params$eGRID_year} already exists."))
# } else {
#   dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
# }
# 
# print(glue::glue("Saving PM2.5 subregion file to folder data/outputs/{params$eGRID_year}"))
# 
# # save file
# write_rds(plant_pm_emissions, glue::glue("data/outputs/{params$eGRID_year}/{save_file}"))
# 
# # check if file is successfully written to folder 
# if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/{save_file}"))){
#   print(glue::glue("File {save_file} successfully written to folder data/outputs/{params$eGRID_year}"))
# } else {
#   print(glue::glue("File {save_file} failed to write to folder."))
# } 
