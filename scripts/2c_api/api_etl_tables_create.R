## -------------------------------
##
## Extract and transform output data for API. 
## 
## Purpose: 
## 
## This file creates tables for eGRID API ETL in CSV format.
##
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries ------------------
library(dplyr)
library(readr)

# Load necessary functions --------------
source("scripts/functions/function_check_dir_exists.R")

# Check for parameters -----------------
params <- list()
params$eGRID_year <- "2023"
#source("scripts/functions/function_params_check.R") # once monthly code is merged we can use this
#params <- params_check()

# Load shorthand names ---------------------
source("scripts/name_matching.R")

# Create output directories ----------------
create_dirs <- c("data/2c_api", 
                 glue::glue("data/2c_api/{params$eGRID_year}"),
                 glue::glue("data/2c_api/{params$eGRID_year}/plant_table"), 
                 glue::glue("data/2c_api/{params$eGRID_year}/state_table"), 
                 glue::glue("data/2c_api/{params$eGRID_year}/ba_table"), 
                 glue::glue("data/2c_api/{params$eGRID_year}/subregion_table"), 
                 glue::glue("data/2c_api/{params$eGRID_year}/nerc_table"), 
                 glue::glue("data/2c_api/{params$eGRID_year}/us_table"))
check_dir_exists(create_dirs)   

# Create datatype list
datatypes <- list()

# Plant file -------------------------

# read in plant file 
plant_file <- 
  read_rds(glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS")) %>% 
  mutate(year = as.integer(year), # convert datatypes to final API datatypes
         plant_id = as.integer(plant_id), 
         utility_id = as.integer(utility_id), 
         system_owner_id = as.integer(system_owner_id))

### Create plant lookup tables ----------------

# system owner lookup table
plant_system_owner <- 
  plant_file %>% 
  select(system_owner_id, system_owner) %>% distinct() 
write_csv(plant_system_owner %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_system_owner_lookup.csv"))

datatypes$plant_system_owner <- sapply(plant_system_owner %>% rename(any_of(plant_nonmetric)), class)

# utility lookup table
plant_utility <- 
  plant_file %>% 
  select(utility_id, utility_name) %>% distinct() 
write_csv(plant_utility %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_utility_lookup.csv"))

datatypes$plant_utility <- sapply(plant_utility %>% rename(any_of(plant_nonmetric)), class)

# balancing authority lookup table
plant_ba <- 
  plant_file %>% 
  select(ba_code, ba_name) %>% distinct() 
write_csv(plant_ba %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_ba_lookup.csv"))

datatypes$plant_ba <- sapply(plant_ba %>% rename(any_of(plant_nonmetric)), class)

# subregion lookup table
plant_subregion <- 
  plant_file %>% 
  select("subregion" = egrid_subregion, "subregion_name" = egrid_subregion_name) %>% distinct() 
write_csv(plant_subregion %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_subregion_lookup.csv"))

datatypes$plant_subregion <- sapply(plant_subregion %>% rename(any_of(plant_nonmetric)), class)

# county lookup table
plant_county <- 
  plant_file %>% 
  select(fips_state_code, fips_county_code, county) %>% distinct() %>% 
  filter(!is.na(fips_county_code)) # exclude NA FIPS county codes
write_csv(plant_county %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_county_lookup.csv"))

plant_county_na_fips <- 
  plant_file %>% 
  select(fips_state_code, fips_county_code, county) %>% distinct() %>% 
  filter(is.na(fips_county_code))

datatypes$plant_county <- sapply(plant_county %>% rename(any_of(plant_nonmetric)), class)

# state lookup table
plant_state <- 
  plant_file %>% 
  select(plant_state, fips_state_code) %>% distinct() 
write_csv(plant_state %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_state_lookup.csv"))

datatypes$plant_state <- sapply(plant_state %>% rename(any_of(plant_nonmetric)), class)

### Create plant subsidiary tables --------------

# plant unadjusted emissions
plant_unadjusted_emissions <- 
  plant_file %>% 
  select(year, 
         plant_state,
         plant_id,
         primary_fuel_type, 
         starts_with("unadj_nox"), 
         unadj_so2_mass, 
         unadj_co2_mass, 
         unadj_ch4_mass,
         unadj_n2o_mass, 
         unadj_co2e_mass, 
         unadj_hg_mass) 
write_csv(plant_unadjusted_emissions %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_unadjusted_emissions.csv"))

datatypes$plant_unadjusted_emissions <- sapply(plant_unadjusted_emissions %>% rename(any_of(plant_nonmetric)), class)


# plant adjusted values 
plant_total_emissions <- 
  plant_file %>% 
  select(year,
         plant_state,
         plant_id,
         primary_fuel_type, 
         starts_with("nox"), -contains("bio"), -contains("rate"), 
         so2_mass, 
         co2_mass, 
         ch4_mass,
         n2o_mass, 
         co2e_mass, 
         hg_mass) 
write_csv(plant_total_emissions %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_total_emissions.csv"))

datatypes$plant_total_emissions <- sapply(plant_total_emissions %>% rename(any_of(plant_nonmetric)), class)

# plant output emission rates 
plant_output_emission_rate <- 
  plant_file %>% 
  select(year, 
         plant_state, 
         plant_id, 
         primary_fuel_type, 
         contains("out_emission_rate"), -contains("combust")) 
write_csv(plant_output_emission_rate %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_output_emission_rate.csv"))

datatypes$plant_output_emission_rate <- sapply(plant_output_emission_rate %>% rename(any_of(plant_nonmetric)), class)

# plant input emission rates 
plant_input_emission_rate <- 
  plant_file %>% 
  select(year, 
         plant_state, 
         plant_id, 
         primary_fuel_type, 
         contains("in_emission_rate")) 
write_csv(plant_input_emission_rate %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_input_emission_rate.csv"))

datatypes$plant_input_emission_rate <- sapply(plant_input_emission_rate %>% rename(any_of(plant_nonmetric)), class)

# plant total generation
plant_generation <- 
  plant_file %>% 
  select(year, 
         plant_state, 
         plant_id, 
         primary_fuel_type,
         generation_ann,
         contains("ann_gen"), # this will become netgen when monthly code is used
         -contains("perc"))
write_csv(plant_generation %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_generation.csv"))

datatypes$plant_generation <- sapply(plant_generation %>% rename(any_of(plant_nonmetric)), class)

# plant nonbaseload total generation
plant_nonbaseload_generation <- 
  plant_file %>% 
  select(year, 
         plant_state, 
         plant_id, 
         primary_fuel_type,
         generation_nonbaseload)
write_csv(plant_nonbaseload_generation %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_nonbaseload_generation.csv"))

datatypes$plant_nonbaseload_generation <- sapply(plant_nonbaseload_generation %>% rename(any_of(plant_nonmetric)), class)


# plant fuel type generation
# plant_fuel_type_gen <- 
#   plant_file %>% 
#   select(plant_id, 
#          contains("ann_gen"), # this will become netgen when monthly code is used
#          -contains("perc")) 
# write_csv(plant_fuel_type_gen %>% rename(any_of(plant_nonmetric)), 
#           glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_fuel_type_gen.csv"))
# 
# datatypes$plant_fuel_type_gen <- sapply(plant_fuel_type_gen %>% rename(any_of(plant_nonmetric)), class)

# plant resource mix 
plant_resource_mix <- 
  plant_file %>% 
  select(plant_id, 
         contains("perc_ann_gen")) 
write_csv(plant_resource_mix %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_resource_mix.csv"))

datatypes$plant_resource_mix <- sapply(plant_resource_mix %>% rename(any_of(plant_nonmetric)), class)

### Create final plant file ---------------------
plant_table <- 
  plant_file %>% 
  select(seqplt, 
         plant_id, 
         fips_state_code, 
         plant_name, 
         system_owner_id, 
         utility_id,
         sector_name, 
         ba_code, 
         nerc,
         fips_county_code, 
         county, # do we want to only keep counties stored here where fips_county_code is NA?
         egrid_subregion,
         lat,
         lon, 
         num_units, 
         num_generators, 
         primary_fuel_type, 
         #secondary_fuel_type, # this needs to be included in plant file, it is not right now
         primary_fuel_category, 
         nameplate_capacity, 
         capfac) 

# State file ------------------------

state_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/state_aggregation.RDS"))

### Create state ID lookup table -----------------

state_id_lookup <- 
  state_file %>% 
  arrange(state) %>% 
  select(fips_state_code) %>% # since fips_state_code is only id left in plant_table, just keep this
  mutate(state_id = row_number())  # do we want to add this ID to the state lookup table, or keep as separate lookup? 
write_csv(state_id_lookup %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_id_lookup.csv"))

datatypes$state_id_lookup <- sapply(state_id_lookup %>% rename(any_of(state_nonmetric)), class)

# update plant table with state ID and remove fips_state_code
plant_table_2 <- 
  plant_table %>% 
  left_join(state_id_lookup, by = c("fips_state_code")) %>% 
  select(-fips_state_code) 

# update state table with state_id
state_table <- 
  state_file %>% 
  left_join(state_id_lookup, by = c("fips_state_code"))

### Create state subsidiary tables -----------

# state total emissions 
state_total_emissions <- 
  state_table %>% 
  select(year,
         state_id,
         fips_state_code,
         starts_with("state_nox"), 
         state_so2_mass, 
         state_co2_mass, 
         state_ch4_mass,
         state_n2o_mass, 
         state_co2e_mass, 
         state_hg_mass, 
         -contains("rate")) 
write_csv(state_total_emissions %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_total_emissions.csv"))

datatypes$state_total_emissions <- sapply(state_total_emissions %>% rename(any_of(state_nonmetric)), class)

# state output emission rates 
state_output_emission_rate <- 
  state_table %>% 
  select(year, 
         state_id, 
         fips_state_code, 
         contains("output_rate"), 
         -contains("nonbaseload")) # under new monthly code we will not need this line
write_csv(state_output_emission_rate %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_output_emission_rate.csv"))

datatypes$state_output_emission_rate <- sapply(state_output_emission_rate %>% rename(any_of(state_nonmetric)), class)

# state input emission rates 
state_input_emission_rate <- 
  state_table %>% 
  select(year, 
         state_id, 
         fips_state_code, 
         contains("input_rate"), 
         -contains("nonbaseload")) # under new monthly code we will not need this line
write_csv(state_input_emission_rate %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_input_emission_rate.csv"))

datatypes$state_output_emission_rate <- sapply(state_output_emission_rate %>% rename(any_of(state_nonmetric)), class)


# state nonbaseload emission rate
state_nonbaseload_emission_rate <- 
  state_table %>% 
  select(state_id,
         fips_state_code, 
         starts_with(c("state_nox", 
                       "state_so2", 
                       "state_co2", 
                       "state_ch4", 
                       "state_n2o", 
                       "state_co2e", 
                       "state_hg")) & 
          contains("rate") & 
          contains("nonbaseload")) 
write_csv(state_nonbaseload_emission_rate %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_nonbaseload_emission_rate.csv"))

datatypes$state_nonbaseload_emission_rate <- sapply(state_nonbaseload_emission_rate %>% rename(any_of(state_nonmetric)), class)

# state fuel type emission rates 
# state_fuel_type_emission_rate <- 
#   state_table %>% 
#   select(state_id,
#          fips_state_code, 
#          starts_with(c("state_nox", 
#                        "state_so2", 
#                        "state_co2", 
#                        "state_ch4", 
#                        "state_n2o", 
#                        "state_co2e", 
#                        "state_hg")) &  
#          contains("rate") & 
#          contains(c("coal", "oil", "gas", "fossil"))) 
# write_csv(state_fuel_type_emission_rate %>% rename(any_of(state_nonmetric)), 
#           glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_fuel_type_emission_rate.csv"))
# 
# datatypes$state_fuel_type_emission_rate <- sapply(state_fuel_type_emission_rate %>% rename(any_of(state_nonmetric)), class)

# state fuel type generation
state_generation <- 
  state_table %>% 
  select(year,
         state_id,
         fips_state_code, 
         state_generation_ann,
         contains("ann_gen"),
         -contains("perc")) 
write_csv(state_generation %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_generation.csv"))

datatypes$state_generation <- sapply(state_generation %>% rename(any_of(state_nonmetric)), class)

# state resource mix 
state_resource_mix <- 
  state_table %>% 
  select(state_id,
         fips_state_code, 
         contains("resource_mix"), -contains("nonbaseload")) 
write_csv(state_resource_mix %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_resource_mix.csv"))

datatypes$state_resource_mix <- sapply(state_resource_mix %>% rename(any_of(state_nonmetric)), class)

# state nonbaseload generation 
state_nonbaseload_generation <- 
  state_table %>% 
  select(state_id, 
         fips_state_code,
         state_generation_nonbaseload,
         contains("nonbaseload_gen_")) 
write_csv(state_nonbaseload_generation %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_nonbaseload_generation.csv"))

datatypes$state_nonbaseload_generation <- sapply(state_nonbaseload_generation %>% rename(any_of(state_nonmetric)), class)
 
# state nonbaseload resource mix
state_nonbaseload_resource_mix <- 
  state_table %>% 
  select(state_id, 
         fips_state_code,
         contains("nonbaseload_resource_mix")) 
write_csv(state_nonbaseload_resource_mix %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_nonbaseload_resource_mix.csv"))

datatypes$state_nonbaseload_resource_mix <- sapply(state_nonbaseload_resource_mix %>% rename(any_of(state_nonmetric)), class)

### Create final state file -----------

state_table_final <- 
  state_table %>% 
  select(state_id,
         state, 
         fips_state_code) 
write_csv(state_table_final %>% rename(any_of(state_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/state_table/state_table.csv"))

datatypes$state_table <- sapply(state_table_final %>% rename(any_of(state_nonmetric)), class)

# Balancing authority file ------------------------

ba_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/ba_aggregation.RDS"))

### Create BA lookup table ------------

ba_id_lookup <- 
  ba_file %>% 
  arrange(ba_code) %>% 
  select(ba_code) %>% 
  mutate(ba_id = row_number())

datatypes$ba_id_lookup <- sapply(ba_id_lookup %>% rename(any_of(ba_nonmetric)), class)

# update plant table with state ID and remove fips_state_code
plant_table_3 <- 
  plant_table_2 %>% 
  left_join(ba_id_lookup, by = c("ba_code")) %>% 
  select(-ba_code) 

# update state table with state_id
ba_table <- 
  ba_file %>% 
  left_join(ba_id_lookup, by = c("ba_code"))

### Create BA subsidiary tables -----------

# ba adjusted values 
ba_adjusted_values <- 
  ba_table %>% 
  select(ba_id,
         ba_code,
         starts_with("ba_generation"), 
         starts_with("ba_nox"), 
         ba_so2_mass, 
         ba_co2_mass, 
         ba_ch4_mass,
         ba_n2o_mass, 
         ba_co2e_mass, 
         ba_hg_mass, 
         -contains("rate")) 
write_csv(ba_adjusted_values %>% rename(any_of(ba_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/ba_table/ba_adjusted_values.csv"))

datatypes$ba_adjusted_values <- sapply(ba_adjusted_values %>% rename(any_of(ba_nonmetric)), class)

# BA emission rates (includes combustion output, input, and output rates)
ba_emission_rate <- 
  ba_table %>% 
  select(ba_id,
         ba_code, 
         starts_with(c("ba_nox", 
                       "ba_so2", 
                       "ba_co2", 
                       "ba_ch4", 
                       "ba_n2o", 
                       "ba_co2e", 
                       "ba_hg")) & 
           contains("rate"), 
         -contains(c("coal", "oil", "gas", "fossil", "nonbaseload"))) # under new monthly code we will not need this line
write_csv(ba_emission_rate %>% rename(any_of(ba_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/ba_table/ba_emission_rate.csv"))

datatypes$ba_emission_rate <- sapply(ba_emission_rate %>% rename(any_of(ba_nonmetric)), class)

# BA nonbaseload emission rate
ba_nonbaseload_emission_rate <- 
  ba_table %>% 
  select(ba_id,
         ba_code, 
         starts_with(c("ba_nox", 
                       "ba_so2", 
                       "ba_co2", 
                       "ba_ch4", 
                       "ba_n2o", 
                       "ba_co2e", 
                       "ba_hg")) & 
           contains("rate") & 
           contains("nonbaseload")) 
write_csv(ba_emission_rate %>% rename(any_of(ba_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/ba_table/ba_nonbaseload_emission_rate.csv"))

datatypes$ba_nonbaseload_emission_rate <- sapply(ba_nonbaseload_emission_rate %>% rename(any_of(ba_nonmetric)), class)

# BA fuel type emission rates 
ba_fuel_type_emission_rate <- 
  ba_table %>% 
  select(ba_id,
         ba_code, 
         starts_with(c("ba_nox", 
                       "ba_so2", 
                       "ba_co2", 
                       "ba_ch4", 
                       "ba_n2o", 
                       "ba_co2e", 
                       "ba_hg")) &  
           contains("rate") & 
           contains(c("coal", "oil", "gas", "fossil")))
write_csv(ba_fuel_type_emission_rate %>% rename(any_of(ba_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/ba_table/ba_fuel_type_emission_rate.csv"))

datatypes$ba_fuel_type_emission_rate <- sapply(ba_fuel_type_emission_rate %>% rename(any_of(ba_nonmetric)), class)

# BA fuel type generation
ba_fuel_type_gen <- 
  ba_table %>% 
  select(ba_id, 
         ba_code, 
         contains("ann_gen"),
         -contains("perc")) 
write_csv(ba_fuel_type_gen %>% rename(any_of(ba_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/ba_table/ba_fuel_type_gen.csv"))

datatypes$ba_fuel_type_gen <- sapply(ba_fuel_type_gen %>% rename(any_of(ba_nonmetric)), class)

# BA resource mix 
ba_resource_mix <- 
  ba_table %>% 
  select(ba_id, 
         ba_code, 
         contains("resource_mix"), -contains("nonbaseload"))
write_csv(ba_resource_mix %>% rename(any_of(ba_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/ba_table/ba_resource_mix.csv"))

datatypes$ba_resource_mix <- sapply(ba_resource_mix %>% rename(any_of(ba_nonmetric)), class)

# BA nonbaseload generation 
ba_nonbaseload_generation <- 
  ba_table %>% 
  select(ba_id, 
         ba_code, 
         contains("nonbaseload_gen_")) 
write_csv(ba_nonbaseload_generation %>% rename(any_of(ba_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/ba_table/ba_nonbaseload_generation.csv"))

datatypes$ba_nonbaseload_generation <- sapply(ba_nonbaseload_generation %>% rename(any_of(ba_nonmetric)), class)

# BA nonbaseload resource mix
ba_nonbaseload_resource_mix <- 
  ba_table %>% 
  select(ba_id, 
         ba_code, 
         contains("nonbaseload_resource_mix")) 
write_csv(ba_nonbaseload_resource_mix %>% rename(any_of(ba_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/ba_table/ba_nonbaseload_resource_mix.csv"))

datatypes$ba_nonbaseload_resource_mix <- sapply(ba_nonbaseload_resource_mix %>% rename(any_of(ba_nonmetric)), class)

### Create final BA file -----------

ba_table_final <- 
  ba_table %>% 
  select(ba_id,
         ba_code, 
         ba_name) 
write_csv(ba_table_final %>% rename(any_of(ba_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/ba_table/ba_table.csv"))

datatypes$ba_table <- sapply(ba_table_final %>% rename(any_of(ba_nonmetric)), class)

# Subregion file ------------------------

subregion_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/subregion_aggregation.RDS"))

### Create subregion ID lookup table ------------

subregion_id_lookup <- 
  subregion_file %>% 
  arrange(subregion) %>% 
  select(subregion) %>% 
  mutate(subregion_id = row_number())  

datatypes$subregion_id_lookup <- sapply(subregion_id_lookup %>% rename(any_of(subregion_nonmetric)), class)

# update plant table with state ID and remove fips_state_code
plant_table_4 <- 
  plant_table_3 %>% 
  left_join(subregion_id_lookup, by = c("egrid_subregion"= "subregion")) %>% 
  select(-egrid_subregion) 

# update state table with state_id
subregion_table <- 
  subregion_file %>% 
  left_join(subregion_id_lookup, by = c("subregion"))

### Create subregion subsidiary tables -----------

# subregion adjusted values 
subregion_adjusted_values <- 
  subregion_table %>% 
  select(subregion_id,
         subregion,
         starts_with("subregion_generation"), 
         starts_with("subregion_nox"), 
         subregion_so2_mass, 
         subregion_co2_mass, 
         subregion_ch4_mass,
         subregion_n2o_mass, 
         subregion_co2e_mass, 
         subregion_hg_mass, 
         -contains("rate")) 
write_csv(subregion_adjusted_values %>% rename(any_of(subregion_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/subregion_table/subregion_adjusted_values.csv"))

datatypes$subregion_adjusted_values <- sapply(subregion_adjusted_values %>% rename(any_of(subregion_nonmetric)), class)

# subregion emission rates (includes combustion output, input, and output rates)
subregion_emission_rate <- 
  subregion_table %>% 
  select(subregion_id, 
         subregion, 
         starts_with(c("subregion_nox", 
                       "subregion_so2", 
                       "subregion_co2", 
                       "subregion_ch4", 
                       "subregion_n2o", 
                       "subregion_co2e", 
                       "subregion_hg")) & 
           contains("rate"), 
         -contains(c("coal", "oil", "gas", "fossil", "nonbaseload"))) # under new monthly code we will not need this line
write_csv(subregion_emission_rate %>% rename(any_of(subregion_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/subregion_table/subregion_emission_rate.csv"))

datatypes$subregion_emission_rate <- sapply(subregion_emission_rate %>% rename(any_of(subregion_nonmetric)), class)

# subregion nonbaseload emission rate
subregion_nonbaseload_emission_rate <- 
  subregion_table %>% 
  select(subregion_id,
         subregion, 
         starts_with(c("subregion_nox", 
                       "subregion_so2", 
                       "subregion_co2", 
                       "subregion_ch4", 
                       "subregion_n2o", 
                       "subregion_co2e", 
                       "subregion_hg")) & 
           contains("rate") & 
           contains("nonbaseload")) 
write_csv(subregion_emission_rate %>% rename(any_of(subregion_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/subregion_table/subregion_nonbaseload_emission_rate.csv"))

datatypes$subregion_nonbaseload_emission_rate <- sapply(subregion_nonbaseload_emission_rate %>% rename(any_of(subregion_nonmetric)), class)

# subregion fuel type emission rates 
subregion_fuel_type_emission_rate <- 
  subregion_table %>% 
  select(subregion_id,
         subregion, 
         starts_with(c("subregion_nox", 
                       "subregion_so2", 
                       "subregion_co2", 
                       "subregion_ch4", 
                       "subregion_n2o", 
                       "subregion_co2e", 
                       "subregion_hg")) &  
           contains("rate") & 
           contains(c("coal", "oil", "gas", "fossil"))) 
write_csv(subregion_fuel_type_emission_rate %>% rename(any_of(subregion_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/subregion_table/subregion_fuel_type_emission_rate.csv"))

datatypes$subregion_fuel_type_emission_rate <- sapply(subregion_fuel_type_emission_rate %>% rename(any_of(subregion_nonmetric)), class)

# subregion fuel type generation
subregion_fuel_type_gen <- 
  subregion_table %>% 
  select(subregion_id,
         subregion, 
         contains("ann_gen"),
         -contains("perc"))  
write_csv(subregion_fuel_type_gen %>% rename(any_of(subregion_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/subregion_table/subregion_fuel_type_gen.csv"))

datatypes$subregion_fuel_type_gen <- sapply(subregion_fuel_type_gen %>% rename(any_of(subregion_nonmetric)), class)

# subregion resource mix 
subregion_resource_mix <- 
  subregion_table %>% 
  select(subregion_id,
         subregion, 
         contains("resource_mix"), -contains("nonbaseload")) 
write_csv(subregion_resource_mix %>%  rename(any_of(subregion_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/subregion_table/subregion_resource_mix.csv"))

datatypes$subregion_resource_mix <- sapply(subregion_resource_mix %>% rename(any_of(subregion_nonmetric)), class)

# subregion nonbaseload generation 
subregion_nonbaseload_generation <- 
  subregion_table %>% 
  select(subregion_id,
         subregion, 
         contains("nonbaseload_gen_"))  
write_csv(subregion_nonbaseload_generation %>% rename(any_of(subregion_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/subregion_table/subregion_nonbaseload_generation.csv"))

datatypes$subregion_nonbaseload_generation <- sapply(subregion_nonbaseload_generation %>% rename(any_of(subregion_nonmetric)), class)

# subregion nonbaseload resource mix 
subregion_nonbaseload_resource_mix <- 
  subregion_table %>% 
  select(subregion_id,
         subregion, 
         contains("nonbaseload_resource_mix"))  
write_csv(subregion_nonbaseload_resource_mix %>% rename(any_of(subregion_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/subregion_table/subregion_nonbaseload_resource_mix.csv"))

datatypes$subregion_nonbaseload_resource_mix <- sapply(subregion_nonbaseload_resource_mix %>% rename(any_of(subregion_nonmetric)), class)


### Create final subregion file -----------

subregion_table_final <- 
  subregion_table %>% 
  select(subregion_id,
         subregion, 
         subregion_name) 
write_csv(subregion_table_final %>% rename(any_of(subregion_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/subregion_table/subregion_table.csv"))

datatypes$subregion_table <- sapply(subregion_table_final %>% rename(any_of(subregion_nonmetric)), class)

# NERC file ------------------------

nerc_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/nerc_aggregation.RDS"))

### Create NERC ID lookup table ------------

nerc_id_lookup <- 
  nerc_file %>% 
  arrange(nerc) %>% 
  select(nerc) %>% 
  mutate(nerc_id = row_number())

datatypes$nerc_id_lookup <- sapply(nerc_id_lookup %>% rename(any_of(nerc_nonmetric)), class)

# update plant table with state ID and remove fips_state_code
plant_table_5 <- 
  plant_table_4 %>% 
  left_join(nerc_id_lookup, by = c("nerc")) %>% 
  select(-nerc) 

# update state table with state_id
nerc_table <- 
  nerc_file %>% 
  left_join(nerc_id_lookup, by = c("nerc"))

### Create NERC subsidiary tables -----------

# NERC adjusted values 
nerc_adjusted_values <- 
  nerc_table %>% 
  select(nerc_id,
         nerc,
         starts_with("nerc_generation"), 
         starts_with("nerc_nox"), 
         nerc_so2_mass, 
         nerc_co2_mass, 
         nerc_ch4_mass,
         nerc_n2o_mass, 
         nerc_co2e_mass, 
         nerc_hg_mass, 
         -contains("rate")) 
write_csv(nerc_adjusted_values %>% rename(any_of(nerc_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/nerc_table/nerc_adjusted_values.csv"))

datatypes$nerc_adjusted_values <- sapply(nerc_adjusted_values %>% rename(any_of(nerc_nonmetric)), class)

# NERC emission rates (includes combustion output, input, and output rates)
nerc_emission_rate <- 
  nerc_table %>% 
  select(nerc_id,
         nerc, 
         starts_with(c("nerc_nox", 
                       "nerc_so2", 
                       "nerc_co2", 
                       "nerc_ch4", 
                       "nerc_n2o", 
                       "nerc_co2e", 
                       "nerc_hg")) & 
           contains("rate"), 
         -contains(c("coal", "oil", "gas", "fossil", "nonbaseload"))) # under new monthly code we will not need this line
write_csv(nerc_emission_rate %>% rename(any_of(nerc_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/nerc_table/nerc_emission_rate.csv"))

datatypes$nerc_emission_rate <- sapply(nerc_emission_rate %>% rename(any_of(nerc_nonmetric)), class)

# NERC nonbaseload emission rate
nerc_nonbaseload_emission_rate <- 
  nerc_table %>% 
  select(nerc_id,
         nerc, 
         starts_with(c("nerc_nox", 
                       "nerc_so2", 
                       "nerc_co2", 
                       "nerc_ch4", 
                       "nerc_n2o", 
                       "nerc_co2e", 
                       "nerc_hg")) & 
           contains("rate") & 
           contains("nonbaseload")) 
write_csv(nerc_emission_rate %>% rename(any_of(nerc_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/nerc_table/nerc_nonbaseload_emission_rate.csv"))

datatypes$nerc_nonbaseload_emission_rate <- sapply(nerc_nonbaseload_emission_rate %>% rename(any_of(nerc_nonmetric)), class)

# NERC fuel type emission rates 
nerc_fuel_type_emission_rate <- 
  nerc_table %>% 
  select(nerc_id,
         nerc,
         starts_with(c("nerc_nox", 
                       "nerc_so2", 
                       "nerc_co2", 
                       "nerc_ch4", 
                       "nerc_n2o", 
                       "nerc_co2e", 
                       "nerc_hg")) &  
           contains("rate") & 
           contains(c("coal", "oil", "gas", "fossil"))) 
write_csv(nerc_fuel_type_emission_rate %>% rename(any_of(nerc_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/nerc_table/nerc_fuel_type_emission_rate.csv"))

datatypes$nerc_fuel_type_emission_rate <- sapply(nerc_fuel_type_emission_rate %>% rename(any_of(nerc_nonmetric)), class)

# NERC fuel type generation
nerc_fuel_type_gen <- 
  nerc_table %>% 
  select(nerc_id,
         nerc,
         contains("ann_gen"),
         -contains("perc"))  
write_csv(nerc_fuel_type_gen %>% rename(any_of(nerc_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/nerc_table/nerc_fuel_type_gen.csv"))

datatypes$nerc_fuel_type_gen <- sapply(nerc_fuel_type_gen %>% rename(any_of(nerc_nonmetric)), class)

# NERC resource mix 
nerc_resource_mix <- 
  nerc_table %>% 
  select(nerc_id,
         nerc,
         contains("resource_mix"), -contains("nonbaseload")) 
write_csv(nerc_resource_mix %>% rename(any_of(nerc_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/nerc_table/nerc_resource_mix.csv"))

datatypes$nerc_resource_mix <- sapply(nerc_resource_mix %>% rename(any_of(nerc_nonmetric)), class)

# NERC nonbaseload generation 
nerc_nonbaseload_generation <- 
  nerc_table %>% 
  select(nerc_id,
         nerc,
         contains("nonbaseload_gen_"))  
write_csv(nerc_nonbaseload_generation %>% rename(any_of(nerc_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/nerc_table/nerc_nonbaseload_generation.csv"))

datatypes$nerc_nonbaseload_generation <- sapply(nerc_nonbaseload_generation %>% rename(any_of(nerc_nonmetric)), class)

# NERC nonbaseload resource mix
nerc_nonbaseload_resource_mix <- 
  nerc_table %>% 
  select(nerc_id,
         nerc,
         contains("nonbaseload_resource"))  
write_csv(nerc_nonbaseload_resource_mix %>% rename(any_of(nerc_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/nerc_table/nerc_nonbaseload_resource_mix.csv"))

datatypes$nerc_nonbaseload_resource_mix <- sapply(nerc_nonbaseload_resource_mix %>% rename(any_of(nerc_nonmetric)), class)

### Create final NERC file -----------

nerc_table_final <- 
  nerc_table %>% 
  select(nerc_id,
         nerc, 
         nerc_name) 
write_csv(nerc_table_final %>% rename(any_of(nerc_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/nerc_table/nerc_table.csv"))

datatypes$nerc_table <- sapply(nerc_table_final %>% rename(any_of(nerc_nonmetric)), class)

# US file ------------------------

us_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/us_aggregation.RDS"))

### Create US subsidiary tables -----------

# US adjusted values 
us_adjusted_values <- 
  us_file %>% 
  select(starts_with("us_generation"), 
         starts_with("us_nox"), 
         us_so2_mass, 
         us_co2_mass, 
         us_ch4_mass,
         us_n2o_mass, 
         us_co2e_mass, 
         us_hg_mass, 
         -contains("rate")) 
write_csv(us_adjusted_values %>% rename(any_of(us_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/us_table/us_adjusted_values.csv"))

datatypes$us_adjusted_values <- sapply(us_adjusted_values %>% rename(any_of(us_nonmetric)), class)

# US emission rates (includes combustion output, input, and output rates)
us_emission_rate <- 
  us_file %>% 
  select(starts_with(c("us_nox", 
                       "us_so2", 
                       "us_co2", 
                       "us_ch4", 
                       "us_n2o", 
                       "us_co2e", 
                       "us_hg")) & 
           contains("rate"), 
         -contains(c("coal", "oil", "gas", "fossil", "nonbaseload"))) # under new monthly code we will not need this line
write_csv(us_emission_rate %>% rename(any_of(us_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/us_table/us_emission_rate.csv"))

datatypes$us_emission_rate <- sapply(us_emission_rate %>% rename(any_of(us_nonmetric)), class)

# US nonbaseload emission rate
us_nonbaseload_emission_rate <- 
  us_file %>% 
  select(starts_with(c("us_nox", 
                       "us_so2", 
                       "us_co2", 
                       "us_ch4", 
                       "us_n2o", 
                       "us_co2e", 
                       "us_hg")) & 
           contains("rate") & 
           contains("nonbaseload"))  
write_csv(us_emission_rate %>% rename(any_of(us_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/us_table/us_nonbaseload_emission_rate.csv"))

datatypes$us_nonbaseload_emission_rate <- sapply(us_nonbaseload_emission_rate %>% rename(any_of(us_nonmetric)), class)

# US fuel type emission rates 
us_fuel_type_emission_rate <- 
  us_file %>% 
  select(starts_with(c("us_nox", 
                       "us_so2", 
                       "us_co2", 
                       "us_ch4", 
                       "us_n2o", 
                       "us_co2e", 
                       "us_hg")) &  
           contains("rate") & 
           contains(c("coal", "oil", "gas", "fossil"))) 
write_csv(us_fuel_type_emission_rate %>% rename(any_of(us_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/us_table/us_fuel_type_emission_rate.csv"))

datatypes$us_fuel_type_emission_rate <- sapply(us_fuel_type_emission_rate %>% rename(any_of(us_nonmetric)), class)

# US fuel type generation
us_fuel_type_gen <- 
  us_file %>% 
  select(contains("ann_gen"),
         -contains("perc"))  
write_csv(us_fuel_type_gen %>% rename(any_of(us_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/us_table/us_fuel_type_gen.csv"))

datatypes$us_fuel_type_gen <- sapply(us_fuel_type_gen %>% rename(any_of(us_nonmetric)), class)

# US resource mix 
us_resource_mix <- 
  us_file %>% 
  select(contains("resource_mix"), -contains("nonbaseload")) 
write_csv(us_resource_mix %>% rename(any_of(us_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/us_table/us_resource_mix.csv"))

datatypes$us_resource_mix <- sapply(us_resource_mix %>% rename(any_of(us_nonmetric)), class)

# US nonbaseload generation 
us_nonbaseload_generation <- 
  us_file %>% 
  select(contains("nonbaseload_gen_")) 
write_csv(us_nonbaseload_generation %>% rename(any_of(us_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/us_table/us_nonbaseload_generation.csv"))

datatypes$us_nonbaseload_generation <- sapply(us_nonbaseload_generation %>% rename(any_of(us_nonmetric)), class)

# US nonbaseload resource mix
us_nonbaseload_resource_mix <- 
  us_file %>% 
  select(contains("nonbaseload_resource_mix")) 
write_csv(us_nonbaseload_resource_mix %>% rename(any_of(us_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/us_table/us_nonbaseload_resource_mix.csv"))

datatypes$us_nonbaseload_resource_mix <- sapply(us_nonbaseload_resource_mix %>% rename(any_of(us_nonmetric)), class)

### Create final US file -----------

us_table_final <- 
  us_file %>% 
  select(us_nameplate_capacity) 
write_csv(us_table_final %>% rename(any_of(us_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/us_table/us_table.csv"))

datatypes$us_table <- sapply(us_table_final %>% rename(any_of(us_nonmetric)), class)

# Export final plant table ----------------------

datatypes$plant_table <- sapply(plant_table_5 %>% rename(any_of(plant_nonmetric)), class)

write_csv(plant_table_5 %>% rename(any_of(plant_nonmetric)), 
          glue::glue("data/2c_api/{params$eGRID_year}/plant_table/plant_table.csv"))

