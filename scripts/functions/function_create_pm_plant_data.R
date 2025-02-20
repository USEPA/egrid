## Purpose: 
## 
## This file creates the PM2.5 plant file for eGRID. 
## This file includes PM2.5 emission data, either calculated
## or estimated for the plants of the specified eGRID year.
## 
## The method of PM2.5 calculations are listed within pm25_source
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

create_pm_plant_data <- function(){
  
  #' create_pm_plant_data
  #' 
  #' Function to create PM plant file data by aggregating unit data
  #' 
  #' @return Dataset with PM2.5 plant data in the format needed for 
  #'         regional aggregation
  #'         
  #' @examples 
  #' # Create PM2.5 plant data
  #' pm_plant_data <- create_pm_plant_data()
  
  
  # Require Libraries ---------
  require(dplyr)
  require(readr)
  require(readxl)

  # Load necessary data --------------------
  ## eGRID production model data - plant file
  plant_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS"))


  # Run unit data creation script ---------
  source("scripts/functions/function_create_pm_unit_data.R")
  pm_unit_data <- create_pm_unit_data()

  # Sum PM2.5 unit data by plant id ---------
  pm_plant <-
    pm_unit_data %>%
    group_by(plant_id) %>%
    summarise(pm25 = sum(pm25, na.rm = TRUE))

  # Add PM2.5 data to plant file ---------
  pm_plant_emissions <-
    plant_file %>%
    inner_join(pm_plant, by = join_by(plant_id)) %>%
    # set plant electric allocation factors to 1 if NaN 
    mutate(elec_allocation = if_else(is.na(elec_allocation), 1, elec_allocation),
           # calculate annual pm2.5 emissions
           plpm25an = pm25 * elec_allocation,
           # calculate total output emission rate
           plpm25rta= plpm25an * 2000 / generation_ann,
           # calculate total input emission rate
           plpm25ra = plpm25an * 2000 / heat_input,
           #  rename unadjusted annual pm2.5 emissions
           unpm25 = pm25) %>%
    select(plant_state, plant_name, plant_id, egrid_subregion_name, egrid_subregion, primary_fuel_type, nameplate_capacity, elec_allocation, heat_input, generation_ann, plpm25an, plpm25rta, plpm25ra, unadj_combust_heat_input, unpm25)
  
  return(pm_plant_emissions)
}
