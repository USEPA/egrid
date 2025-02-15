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


