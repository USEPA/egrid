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

# Load functions and check parameters -----------------------

source("scripts/functions/function_check_params.R")
source("scripts/functions/function_save_output_data.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")}

# Load necessary data -------------------------

gwp <- 
  read_csv("data/1_production_model/static_tables/global_warming_potential.csv", 
           locale=locale(encoding="latin1")) %>% 
  janitor::clean_names()

subregion_table <- 
  read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/subregion_aggregation.RDS")) %>% 
  select(subregion, 
         subregion_generation_ann, 
         subregion_generation_nonbaseload,
         subregion_co2_mass,
         subregion_ch4_mass,
         subregion_n2o_mass)
 
plant_table <- 
  read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/plant_file.RDS")) %>% 
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
              subregion_generation_ann, 
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
              subregion_generation_ann, 
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
              subregion_generation_ann,
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
              subregion_generation_ann,
         co2e_ar6 = 
           (if_else(is.na(co2_nonbaseload), 0, co2_nonbaseload * 2000) + 
           if_else(is.na(ch4_nonbaseload), 0, gwp$ar6[gwp$gas == "CH4"] * ch4_nonbaseload) + 
           if_else(is.na(n2o_nonbaseload), 0, gwp$ar6[gwp$gas == "N2O"] * n2o_nonbaseload)) / 
              subregion_generation_nonbaseload) %>% 
  select(subregion, contains("co2e"))

# Export table ----------------------

save_output_data(subregion_co2e, "data/2b_web_updates", "faq_gwp_table.csv", file_type = "CSV")
