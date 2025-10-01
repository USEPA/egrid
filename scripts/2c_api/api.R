## -------------------------------
##
## Plumber API set-up 
## 
## Purpose: 
## 
## This file connects the RDS outputs for the plumber API to recognize, and connect to the ETL.
## 
## Authors:  
##      Elisabeth Ashley, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------

library(plumber)
library(this.path)
library(readr)
library(dplyr)

# Identify directory path egrid repository 
file_path <- this.path::this.path()
egrid_dir_path <- dirname(dirname(dirname(file_path)))

# Load name matching to update to shorthand names
base::load(glue::glue("{egrid_dir_path}/data/1_production_model/static_tables/name_matches.RData"))
 
#* @get /<year>/plant
function(year) {
  rds_file <- glue::glue("{egrid_dir_path}/data/1_production_model/outputs/{year}/annual/plant_file_annual.RDS") 
 
  tryCatch({
    plant_data <- read_rds(rds_file) %>% 
      rename(any_of(plant_nonmetric)) %>% janitor::clean_names()
    
    list(success = TRUE, data = plant_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}


#* @get /<year>/balancingauthority
function(year) {
  rds_file <- glue::glue("{egrid_dir_path}/data/1_production_model/outputs/{year}/annual/ba_aggregation_annual.RDS") 
  
  tryCatch({
    ba_data <- read_rds(rds_file) %>% 
      rename(any_of(ba_nonmetric)) %>% janitor::clean_names()
    
    list(success = TRUE, data = ba_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/generator
function(year) {
  rds_file <- glue::glue("{egrid_dir_path}/data/1_production_model/outputs/{year}/annual/generator_file_annual.RDS") 
  
  tryCatch({
    generator_data <- read_rds(rds_file) %>% 
      rename(any_of(gen_nonmetric)) %>% janitor::clean_names()

    list(success = TRUE, data = generator_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/nerc
function(year) {
  rds_file <- glue::glue("{egrid_dir_path}/data/1_production_model/outputs/{year}/annual/nerc_aggregation_annual.RDS") 
  
  tryCatch({
    nerc_data <- read_rds(rds_file) %>% 
      rename(any_of(nerc_nonmetric)) %>% janitor::clean_names()
    
    list(success = TRUE, data = nerc_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/state
function(year) {
  rds_file <- glue::glue("{egrid_dir_path}/data/1_production_model/outputs/{year}/annual/state_aggregation_annual.RDS") 
  
  tryCatch({
    state_data <- read_rds(rds_file) %>% 
      rename(any_of(state_nonmetric)) %>% janitor::clean_names()

    list(success = TRUE, data = state_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/subregion
function(year) {
  rds_file <- glue::glue("{egrid_dir_path}/data/1_production_model/outputs/{year}/annual/subregion_aggregation_annual.RDS") 
  
  tryCatch({
    subregion_data <- read_rds(rds_file) %>% 
      rename(any_of(subregion_nonmetric)) %>% janitor::clean_names()

    list(success = TRUE, data = subregion_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}


#* @get /<year>/unit
function(year) {
  rds_file <- glue::glue("{egrid_dir_path}/data/1_production_model/outputs/{year}/annual/unit_file_annual.RDS")
  
  tryCatch({
    unit_data <- read_rds(rds_file) %>% 
      rename(any_of(unit_nonmetric)) %>% janitor::clean_names()
    
    list(success = TRUE, data = unit_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/us
function(year) {
  rds_file <- glue::glue("{egrid_dir_path}/data/1_production_model/outputs/{year}/annual/us_aggregation_annual.RDS") 
  
  tryCatch({
    us_data <- read_rds(rds_file) %>% 
      rename(any_of(us_nonmetric)) %>% janitor::clean_names()

    list(success = TRUE, data = us_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}


#* @post /process
#* @param input_data:string
function(input_data) {
  processed <- toupper(input_data) # Example: Make input uppercase
  list(processed_data = processed)
}
