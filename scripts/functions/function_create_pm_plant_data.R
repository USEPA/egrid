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
  plant_file <- read_csv(glue::glue("data/outputs/{params$eGRID_year}/plant_file_2021_access.csv"), col_types = c(YEAR = "c", ORISPL = "c", OPRCODE = "c", UTLSRVID = "c")) %>%
    janitor::clean_names()


  # Run unit data creation script ---------
  source("scripts/functions/function_create_pm_unit_data.R")
  pm_unit_data <- create_pm_unit_data()

  # Sum PM2.5 unit data by plant id ---------
  pm_plant <-
    pm_unit_data %>%
    group_by(orispl) %>%
    summarise(pm25 = sum(pm25), na.rm = TRUE)

  # Add PM2.5 data to plant file ---------
  pm_plant_emissions <-
    plant_file %>%
    inner_join(pm_plant, by = join_by(orispl)) %>%
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
  
  return(pm_plant_emissions)
}
