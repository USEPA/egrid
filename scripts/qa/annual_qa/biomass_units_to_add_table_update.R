## -------------------------------
##
## Biomass units to add table update 
## 
## Purpose: 
## 
## This file identifies any biomass units that need to be added to the unit file.
## This is done by updating biomass_units_to_add_to_unit_file.csv in static_tables. 
##
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries -----------------

library(dplyr)
library(readr)
library(stringr)

# Load functions -------------

source("scripts/functions/function_check_params.R")
source("scripts/functions/function_save_output_data.R")
source("scripts/functions/function_check_file_exists.R")

# Set parameters -----------------------------

# Define paramters if necessary and check for valid params()
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")
}

# Load necessary data ------------------------

# plants with negative CO2 for current eGRID year
plants_negative_co2 <- 
  read_csv("data/1_production_model/static_tables/qa/check_biomass_units.csv") %>% 
  mutate(plant_id = as.character(plant_id))

# EIA data
eia_923 <- check_file_exists(glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))
eia_860 <- check_file_exists(glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS"))

# EPA data
epa <- check_file_exists(glue::glue("data/1_production_model/clean_data/epa/{params$eGRID_year}/epa_clean.RDS"))

# unit file
unit_file <- 
  check_file_exists(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/unit_file_no_bio_added_monthly.RDS")) %>% 
  select(plant_id, unit_id) %>% 
  distinct() %>% 
  mutate(id = paste0(plant_id, "_", unit_id))

# biomass fuel types 
bio_fuels <- 
  read_csv("data/1_production_model/static_tables/fuel_type_categories.csv", 
            col_types = cols_only(biomass_fuel_adj = "c")) %>% 
  tidyr::drop_na()

# EIA Boiler data --------------------------------

eia_923_boiler_bio_plants <- 
  eia_923$boiler %>% 
  inner_join(plants_negative_co2 %>% select(plant_id), by = "plant_id") %>% # only include plants with negative CO2
  group_by(plant_id, boiler_id, fuel_type, prime_mover) %>% 
  summarize(total_fuel_consumption_quantity = sum(total_fuel_consumption_quantity, na.rm = TRUE), 
            heat_input = sum(mmbtu_per_unit, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(plant_id, boiler_id, prime_mover) %>% 
  slice_max(total_fuel_consumption_quantity, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(id = paste0(plant_id, "_", boiler_id), 
         id_pm = paste0(plant_id, "_", boiler_id, "_", prime_mover))

count_923_bio_plants <- nrow(eia_923_boiler_bio_plants %>% select(plant_id) %>% distinct())

check_923_plants_all_biomass <- 
  eia_923_boiler_bio_plants %>% 
  full_join(unit_file, by = c("plant_id", "boiler_id" = "unit_id")) %>% 
  group_by(plant_id) %>% 
  filter(all(fuel_type %in% bio_fuels$biomass_fuel_adj)) %>% 
  ungroup()
  
write_csv(eia_923_boiler_bio_plants, glue::glue("data/1_production_model/static_tables/qa/eia_923_biomass_units_{params$eGRID_year}.csv"))

# EIA 860 Generator data ---------------------------

eia_860_boil_gen_ids <- eia_860$boiler_generator %>% mutate(id = paste0(plant_id, "_", generator_id)) %>% pull(id)

eia_860_gen_bio_plants <- 
  eia_860$operable %>% 
  right_join(plants_negative_co2 %>% select(plant_id), by = "plant_id") %>% # only include plants with negative CO2
  mutate(id = paste0(plant_id, "_", generator_id),
         id_pm = paste0(plant_id, "_", generator_id, "_", prime_mover)) %>% 
  select(plant_id, generator_id, prime_mover, energy_source_1, nameplate_capacity)

count_860_bio_plants <- nrow(eia_860_gen_bio_plants %>% select(plant_id) %>% distinct())

write_csv(eia_860_gen_bio_plants, glue::glue("data/1_production_model/static_tables/qa/eia_860_biomass_units_{params$eGRID_year}.csv"))

# EPA data ------------------------------

epa_bio_plants <- 
  epa %>% 
  right_join(plants_negative_co2 %>% select(plant_id), by = "plant_id") %>% # only include plants with negative CO2
  group_by(plant_id, unit_id, unit_type, primary_fuel_type) %>% 
  summarize(heat_input_mmbtu = sum(heat_input_mmbtu, na.rm = TRUE), 
            co2_mass = sum(co2_mass_short_tons, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(id = paste0(plant_id, "_", unit_id)) %>% 
  tidyr::drop_na(unit_id)

count_epa_bio_plants <- nrow(epa_bio_plants %>% select(plant_id) %>% distinct())

test_epa_plants_all_biomass <- 
  epa_bio_plants %>% 
  group_by(plant_id) %>% 
  filter(all(primary_fuel_type == "Wood"))

write_csv(epa_bio_plants, glue::glue("data/1_production_model/static_tables/qa/epa_biomass_units_{params$eGRID_year}.csv"))

