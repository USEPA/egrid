## -------------------------------
##
## Create Power Profiler
## 
## Purpose: 
## 
## This file creates power profiler data
## 
## Additional notes
##      
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries -----
library(dplyr)
library(readr)
# library(readxl)
# library(stringr)

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

# Load necessary data -----

# previous power profiler data
profiler_prev <- read_csv(glue::glue("data/raw_data/power_profiler_old.csv")) %>%
  janitor::clean_names() %>%
  glimpse()

# utility zip codes 80142
utility_zip <- read_csv(glue::glue("data/raw_data/utility_zipcodes_{params$eGRID_year}.csv")) %>%
  janitor::clean_names() %>%
  glimpse()



# combine these two datasets
#01 : 2889
zips_to_add <-
  profiler_prev %>%
  anti_join(utility_zip, by = "zip") %>%
  glimpse()

# create subregion dataset
#02 : 80142
zip_subregion <-
  utility_zip %>%
  mutate(subregion = NA_character_, predominant_utility = NA_real_, old_zip_code = "no", method = NA_character_) %>%
  # group_by(zip, state, eia_id, utility_name) %>%
  glimpse()

# add zipcodes to add
#03 :
zip_subregion_2 <-
  zip_subregion %>%
  bind_rows(zips_to_add %>% mutate(expr1 = "yes", expr2 = "old power profiler")) %>%
  glimpse()
  
  

  


  

