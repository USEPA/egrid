library(plumber)
library(readr)
library(dplyr)

base::load("data/1_production_model/static_tables/name_matches.RData")
 
#* @get /<year>/plant
function(year) {
  rds_file <- file.path("data", "1_production_model", "outputs", year, "plant_file.RDS") %>% 
    rename(any_of(plant_nonmetric)) %>% janitor::clean_names()
 
  tryCatch({
    plant_data <- read_rds(rds_file)
    list(success = TRUE, data = plant_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}


#* @get /<year>/balancingauthority
function(year) {
  rds_file <- file.path("data", "1_production_model", "outputs", year, "ba_aggregation.RDS") %>% 
    rename(any_of(ba_nonmetric)) %>% janitor::clean_names()
  
  tryCatch({
    ba_data <- read_rds(rds_file)
    list(success = TRUE, data = ba_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/generator
function(year) {
  rds_file <- file.path("data", "1_production_model", "outputs", year, "generator_file.RDS") %>% 
    rename(any_of(gen_nonmetric)) %>% janitor::clean_names()
  
  tryCatch({
    ba_data <- read_rds(rds_file)
    list(success = TRUE, data = ba_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/nerc
function(year) {
  rds_file <- file.path("data", "1_production_model", "outputs", year, "nerc_aggregation.RDS") %>% 
    rename(any_of(nerc_nonmetric)) %>% janitor::clean_names()
  
  tryCatch({
    ba_data <- read_rds(rds_file)
    list(success = TRUE, data = ba_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/state
function(year) {
  rds_file <- file.path("data", "1_production_model", "outputs", year, "state_aggregation.RDS") %>% 
    rename(any_of(state_nonmetric)) %>% janitor::clean_names()
  
  tryCatch({
    ba_data <- read_rds(rds_file)
    list(success = TRUE, data = ba_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/subregion
function(year) {
  rds_file <- file.path("data", "1_production_model", "outputs", year, "subregion_file.RDS") %>% 
    rename(any_of(subregion_nonmetric)) %>% janitor::clean_names()
  
  tryCatch({
    ba_data <- read_rds(rds_file)
    list(success = TRUE, data = ba_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}


#* @get /<year>/unit
function(year) {
  rds_file <- file.path("data", "1_production_model", "outputs", year, "unit_file.RDS")
  
  tryCatch({
    ba_data <- read_rds(rds_file)
    list(success = TRUE, data = ba_data)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#* @get /<year>/us
function(year) {
  rds_file <- file.path("data", "1_production_model", "outputs", year, "us_aggregation.RDS") %>% 
    rename(any_of(us_nonmetric)) %>% janitor::clean_names()
  
  tryCatch({
    ba_data <- read_rds(rds_file) 
    list(success = TRUE, data = ba_data)
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
