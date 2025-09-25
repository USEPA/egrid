## -------------------------------
##
## FAQ GWP Table Creation
## 
## Purpose: 
## 
## This file creates the GWP table for the FAQ page using different GWP values. 
##
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries -------------------------------

library(dplyr)
library(readr)

# Check parameters -----------------------

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- (params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Load necessary data -------------------------

gwp <- 
  read_csv("data/1_production_model/static_tables/global_warming_potential.csv") %>% 
  janitor::clean_names()

subregion_table <- 
  read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/subregion_aggregation_annual.RDS")) %>% 
  select(subregion, 
         subregion_generation, 
         subregion_generation_nonbaseload,
         subregion_co2_mass,
         subregion_ch4_mass,
         subregion_n2o_mass)
 
plant_table <- 
  read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/plant_file_annual.RDS")) %>% 
  select(plant_id, 
         subregion = egrid_subregion, 
         nonbaseload, 
         co2_mass, 
         ch4_mass, 
         n2o_mass) 

# Calculate nonbaseload masses for each subregion ------------------------
subregion_nonbaseload_mass <- 
  plant_table %>%
  mutate(co2_nonbaseload = co2_mass * nonbaseload, 
         ch4_nonbaseload = ch4_mass * nonbaseload,
         n2o_nonbaseload = n2o_mass * nonbaseload) %>% 
  group_by(subregion) %>% 
  summarize(co2_nonbaseload = sum(co2_nonbaseload, na.rm = TRUE), 
            ch4_nonbaseload = sum(ch4_nonbaseload, na.rm = TRUE), 
            n2o_nonbaseload = sum(n2o_nonbaseload, na.rm = TRUE))

# Join subregion tables ---------------------------  

subregion_emissions <- 
  subregion_table %>% 
  full_join(subregion_nonbaseload_mass, by = "subregion") %>% 
  rename(co2_mass = subregion_co2_mass, 
         ch4_mass = subregion_ch4_mass, 
         n2o_mass = subregion_n2o_mass)

# Calculate CO2e using multiple GWP values ---------------------

subregion_co2e <- 
  subregion_emissions %>% 
  mutate(# GWP AR4
         co2e_rate_ar4 = 
           (if_else(is.na(co2_mass), 0, co2_mass * 2000) + 
           if_else(is.na(ch4_mass), 0, gwp$ar4[gwp$gas == "CH4"] * ch4_mass) + 
           if_else(is.na(n2o_mass), 0, gwp$ar4[gwp$gas == "N2O"] * n2o_mass)) / 
              subregion_generation, 
         co2e_rate_nonbaseload_ar4 = 
           (if_else(is.na(co2_nonbaseload), 0, co2_nonbaseload * 2000) + 
           if_else(is.na(ch4_nonbaseload), 0, gwp$ar4[gwp$gas == "CH4"] * ch4_nonbaseload) + 
           if_else(is.na(n2o_nonbaseload), 0, gwp$ar4[gwp$gas == "N2O"] * n2o_nonbaseload)) / 
              subregion_generation_nonbaseload,
         
         # GWP AR5 with climate feedbacks
         co2e_ar5_climate_feedbacks = 
           (if_else(is.na(co2_mass), 0, co2_mass * 2000) + 
           if_else(is.na(ch4_mass), 0, gwp$ar5_with_climate_carbon_feedbacks[gwp$gas == "CH4"] * ch4_mass) + 
           if_else(is.na(n2o_mass), 0, gwp$ar5_with_climate_carbon_feedbacks[gwp$gas == "N2O"] * n2o_mass)) / 
              subregion_generation, 
         co2e_nonbaseload_ar5_climate_feedbacks = 
           (if_else(is.na(co2_nonbaseload), 0, co2_nonbaseload * 2000) + 
           if_else(is.na(ch4_nonbaseload), 0, gwp$ar5_with_climate_carbon_feedbacks[gwp$gas == "CH4"] * ch4_nonbaseload) + 
           if_else(is.na(n2o_nonbaseload), 0, gwp$ar5_with_climate_carbon_feedbacks[gwp$gas == "N2O"] * n2o_nonbaseload)) / 
              subregion_generation_nonbaseload,
         
         # GWP AR5 without climate feedbacks
         co2e_ar5_no_climate_feedbacks = 
           (if_else(is.na(co2_mass), 0, co2_mass * 2000) + 
           if_else(is.na(ch4_mass), 0, gwp$ar5_without_climate_carbon_feedbacks[gwp$gas == "CH4"] * ch4_mass) + 
           if_else(is.na(n2o_mass), 0, gwp$ar5_without_climate_carbon_feedbacks[gwp$gas == "N2O"] * n2o_mass)) / 
              subregion_generation,
         co2e_nonbaseload_ar5_no_climate_feedbacks = 
           (if_else(is.na(co2_nonbaseload), 0, co2_nonbaseload * 2000) + 
           if_else(is.na(ch4_nonbaseload), 0, gwp$ar5_without_climate_carbon_feedbacks[gwp$gas == "CH4"] * ch4_nonbaseload) + 
           if_else(is.na(n2o_nonbaseload), 0, gwp$ar5_without_climate_carbon_feedbacks[gwp$gas == "N2O"] * n2o_nonbaseload)) / 
              subregion_generation_nonbaseload,
           
         # GWP AR6
         co2e_ar6 = 
           (if_else(is.na(co2_mass), 0, co2_mass * 2000) + 
           if_else(is.na(ch4_mass), 0, gwp$ar6[gwp$gas == "CH4"] * ch4_mass) + 
           if_else(is.na(n2o_mass), 0, gwp$ar6[gwp$gas == "N2O"] * n2o_mass)) / 
              subregion_generation,
         co2e_ar6 = 
           (if_else(is.na(co2_nonbaseload), 0, co2_nonbaseload * 2000) + 
           if_else(is.na(ch4_nonbaseload), 0, gwp$ar6[gwp$gas == "CH4"] * ch4_nonbaseload) + 
           if_else(is.na(n2o_nonbaseload), 0, gwp$ar6[gwp$gas == "N2O"] * n2o_nonbaseload)) / 
              subregion_generation_nonbaseload) %>% 
  select(subregion, contains("co2e"))

# Export table ----------------------

write_csv(subregion_co2e, glue::glue("data/2b_web_updates/{params$eGRID_year}/faq_gwp_table.csv"))
