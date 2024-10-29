## -------------------------------
##
## NERC aggregation file create 
## 
## Purpose: 
## 
## This file creates the NERC aggregation file for eGRID. 
##
## Authors:  
##      Teagan Goforth, Abt Global, teagan.goforth@abtglobal.com
##
## -------------------------------

# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)


# Load and clean necessary data ------

# columns to keep from plant file

columns_to_keep <- 
  c("year", 
    "plant_state",
    "fips_state_code",
    "plant_name",
    "plant_id",
    "nerc",
    "sub_region" = "egrid_subregion",
    "sub_region_name" = "egrid_subregion_name",
    "ba_name",
    "ba_code",
    "primary_fuel_type",
    "primary_fuel_category",
    "nameplate_capacity",
    "capfac",
    "nonbaseload",
    "combust_heat_input",
    "combust_heat_input_oz",
    "heat_input",
    "heat_input_oz",
    "generation_ann",
    "generation_oz",
    "nox_mass",
    "nox_oz_mass",
    "so2_mass",
    "co2_mass",
    "ch4_mass",
    "n2o_mass",
    "co2e_mass",
    "hg_mass",
    "ann_gen_coal",
    "ann_gen_oil",
    "ann_gen_gas",
    "ann_gen_nuclear",
    "ann_gen_hydro",
    "ann_gen_biomass",
    "ann_gen_wind",
    "ann_gen_solar",
    "ann_gen_geothermal",
    "ann_gen_other_ff",
    "ann_gen_other",
    "ann_gen_non_renew",
    "ann_gen_renew",
    "ann_gen_renew_nonhydro",
    "ann_gen_combust",
    "ann_gen_non_combust"
  )


# read in plant file 

plant_file <- 
  read_rds("data/outputs/plant_file.RDS") %>% 
  select(any_of(columns_to_keep))

# factor plant_fuel_category to final eGRID output order
plant_file$primary_fuel_category <- factor(plant_file$primary_fuel_category, 
                                            levels = c("COAL", 
                                                       "OIL", 
                                                       "GAS",
                                                       "NUCLEAR", 
                                                       "HYDRO", 
                                                       "BIOMASS", 
                                                       "WIND", 
                                                       "SOLAR", 
                                                       "GEOTHERMAL", 
                                                       "OFSL", 
                                                       "OTHF"))


# NERC level aggregation ------

# sum capacity, generation, emissions mass to NERC level 

nerc <- 
  plant_file %>% 
  group_by(year, nerc) %>% 
  summarize(across(.cols = c("nameplate_capacity", 
                             contains("heat_input"), 
                             "generation_ann", 
                             "generation_oz", 
                             contains("_mass")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "nerc_{.col}")) %>% 
  mutate(nerc_hg_mass = "--") %>% 
  ungroup()


## Calculate emission rates ------

### Output emission rates (lb/MWh) -----

nerc_output_rates <- 
  nerc %>% 
  mutate(# calculating output emissions rates (lb/MWh)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    nerc_nox_oz_output_rate = 2000 * nerc_nox_oz_mass / nerc_generation_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    nerc_hg_output_rate = "--") %>% 
  relocate(nerc_nox_oz_output_rate, .after = nerc_nox_output_rate) %>%  
  relocate(nerc_co2e_output_rate, .after = nerc_n2o_output_rate) %>% 
  select(nerc, contains("rate"))


### Input emission rates (lb/MMBtu) -----

nerc_input_rates <- 
  nerc %>% 
  mutate(# calculating input emission rates (lb/MMBtu)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    nerc_nox_oz_input_rate = 2000 * nerc_nox_oz_mass / nerc_combust_heat_input_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    nerc_hg_input_rate = "--") %>% 
  relocate(nerc_nox_oz_input_rate, .after = nerc_nox_input_rate) %>% 
  relocate(nerc_co2e_input_rate, .after = nerc_n2o_input_rate) %>% 
  select(nerc, contains("rate"))


### Combustion emission rates (lb/MWh) -----

nerc_combustion_rates <- 
  plant_file %>% 
  group_by(nerc) %>% 
  summarize(nerc_combustion_gen = sum(ann_gen_combust, na.rm = TRUE)) %>% 
  left_join(nerc, by = c("nerc")) %>% 
  mutate(# calculating combustion emissions rates (lb/MWh)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_combustion_gen, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    nerc_nox_oz_combustion_rate = 2000 * nerc_nox_oz_mass / (nerc_combustion_gen * (nerc_generation_oz / nerc_generation_ann)), 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    nerc_hg_combustion_rate = "--") %>% 
  relocate(nerc_nox_oz_combustion_rate, .after = nerc_nox_combustion_rate) %>%  
  relocate(nerc_co2e_combustion_rate, .after = nerc_n2o_combustion_rate) %>% 
  select(nerc, contains("rate"))


### Fuel type output emission rates (lb/MWh) and input emission rates (lb/MMBtu)   -----

# calculate emission rates by fossil fuel types 

fossil_fuels <- c("COAL", 
                  "OIL",
                  "GAS", 
                  "OSFL")

nerc_fuel_rates <-
  plant_file %>% 
  group_by(nerc, primary_fuel_category) %>% 
  filter(primary_fuel_category %in% fossil_fuels, 
         !primary_fuel_category == 'OSFL') %>% # do not include other fossil in individual fuel rate calculations
  summarize(across(.cols = c(contains("heat_input"), 
                             "generation_ann", 
                             "generation_oz", 
                             contains("mass"), 
                             "ann_gen_coal", 
                             "ann_gen_oil", 
                             "ann_gen_gas"), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "nerc_{.col}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    nerc_nox_oz_output_rate = 2000 * nerc_nox_oz_mass / nerc_generation_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    nerc_nox_oz_input_rate = 2000 * nerc_nox_oz_mass / nerc_combust_heat_input_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
  select(nerc, primary_fuel_category, contains("rate")) %>% 
  relocate(nerc_nox_oz_output_rate, .after = nerc_nox_output_rate) %>% 
  relocate(nerc_co2e_output_rate, .after = nerc_n2o_output_rate) %>% 
  relocate(nerc_nox_oz_input_rate, .after = nerc_nox_input_rate) %>% 
  relocate(nerc_co2e_input_rate, .after = nerc_n2o_input_rate) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = contains("rate")) %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0)), 
         nerc_hg_output_rate_coal = "--", 
         nerc_hg_output_rate_fossil = "--", 
         nerc_hg_input_rate_coal = "--", 
         nerc_hg_input_rate_fossil = "--")


# calculate all fossil fuel output and input emission rates 

nerc_fossil_rates <-
  plant_file %>% 
  group_by(nerc) %>% 
  filter(primary_fuel_category %in% fossil_fuels) %>% 
  summarize(across(.cols = c(contains("heat_input"), 
                             "generation_ann", 
                             "generation_oz", 
                             contains("mass"), 
                             "ann_gen_coal", 
                             "ann_gen_oil", 
                             "ann_gen_gas", 
                             "ann_gen_other_ff"), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "nerc_{.col}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
    nerc_nox_oz_output_rate_fossil = 2000 * nerc_nox_oz_mass / nerc_generation_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    nerc_nox_oz_input_rate_fossil = 2000 * nerc_nox_oz_mass / nerc_combust_heat_input_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    across(where(is.numeric), ~ replace_na(., 0))) %>% 
  relocate(nerc_nox_oz_input_rate_fossil, .after = nerc_nox_input_rate_fossil) %>% 
  select(nerc, contains("rate"))  



### Non-baseload output emission rates (lb/MWh) -----

nerc_nonbaseload_rates <- 
  plant_file %>% 
  group_by(nerc) %>% 
  summarize(across(.cols = c("generation_ann", 
                             "generation_oz", 
                             contains("mass")), 
                   .fns = ~ sum(. * nonbaseload, na.rm = TRUE), 
                   .names = "nerc_{.col}")) %>% 
  mutate(across(.cols = c("nerc_nox_mass",  
                          "nerc_so2_mass", 
                          "nerc_co2_mass", 
                          "nerc_co2e_mass"), 
                .fns = ~ 2000 * . / nerc_generation_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
         nerc_nox_oz_output_rate_nonbaseload = 2000 * nerc_nox_oz_mass / nerc_generation_oz, 
         across(.cols = c("nerc_ch4_mass", 
                          "nerc_n2o_mass"),
                .fns = ~ . / nerc_generation_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
         nerc_hg_output_rate_nonbaseload = "--") %>% 
  relocate(nerc_nox_oz_output_rate_nonbaseload, .after = nerc_nox_output_rate_nonbaseload) %>% 
  relocate(nerc_co2e_output_rate_nonbaseload, .after = nerc_n2o_output_rate_nonbaseload) %>% 
  select(nerc, contains("rate"))


## Calculate net generation and resource mix -----

### Generation by fuel category (MWh) -----

nerc_gen <- 
  plant_file %>% 
  group_by(nerc) %>% 
  summarize(across(.cols = c(contains("gen")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "nerc_{.col}")) 

nerc_gen_2 <- 
  nerc_gen %>% 
  select(-nerc_generation_ann, -nerc_generation_oz) # remove duplicate columns for final formatting

### Resource mix by fuel category (%) -----

nerc_resource_mix <- 
  nerc_gen %>% 
  select(-nerc_generation_oz) %>%   
  mutate(across(.cols = -c("nerc", "nerc_generation_ann"), 
                .fns = ~ . / nerc_generation_ann, # convert to percentage 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(nerc, contains("resource_mix"))


### Nonbaseload generation (MWh) -----

nerc_nonbaseload_gen <- 
  plant_file %>% 
  group_by(nerc, primary_fuel_category) %>% 
  summarize(nerc_nonbaseload_gen = sum(generation_ann * nonbaseload, na.rm = TRUE)) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = nerc_nonbaseload_gen, 
              names_prefix = "nerc_nonbaseload_gen_") %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) 


### Nonbaseload resource mix (%) -----

nerc_nonbaseload_resource_mix <- 
  nerc_nonbaseload_gen %>% 
  mutate(nerc_nonbaseload_gen = rowSums(pick(contains("nonbaseload"))), 
         across(.cols = -c("nerc_nonbaseload_gen"), 
                .fns = ~ . / nerc_nonbaseload_gen, 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(nerc, contains("resource_mix"))


# Create final data frame -----

## Join necessary data -----

nerc_merged <- 
  nerc %>% 
  left_join(nerc_output_rates, by = c("nerc")) %>% # output emission rates
  left_join(nerc_input_rates, by = c("nerc")) %>% # input emission rates
  left_join(nerc_combustion_rates, by = c("nerc")) %>% # combustion emission rates
  left_join(nerc_fuel_rates, by = c("nerc")) %>% # output and input emission rates by fuel type
  left_join(nerc_fossil_rates, by = c("nerc")) %>% # output and input emission rates for all fossil fuels
  left_join(nerc_nonbaseload_rates, by = c("nerc")) %>% # output emission rates for nonbaseload generation
  left_join(nerc_gen_2, by = c("nerc")) %>% # generation by fuel category
  left_join(nerc_resource_mix, by = c("nerc")) %>% # resource mix by fuel category
  left_join(nerc_nonbaseload_gen, by = c("nerc")) %>% # nonbaseload generation by fuel category
  left_join(nerc_nonbaseload_resource_mix, by = c("nerc")) %>% 
  mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
         across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
  select(-contains("gen_na"),
         -contains("mix_na")) # remove unnecessary columns


## Round data ----

n2o_ch4_fuel_rates <- c("n2o_input_rate_coal", "n2o_input_rate_oil", "n2o_input_rate_gas", 
                        "n2o_input_rate_fossil", "n2o_output_rate_coal", "n2o_output_rate_oil", 
                        "n2o_output_rate_gas", "n2o_output_rate_fossil", 
                        "ch4_input_rate_coal", "ch4_input_rate_oil", "ch4_input_rate_gas", 
                        "ch4_input_rate_fossil", "ch4_output_rate_coal", "ch4_output_rate_oil", 
                        "ch4_output_rate_gas", "ch4_output_rate_fossil")

nerc_rounded <- 
  nerc_merged %>% 
  mutate(across(c(where(is.numeric), -contains(n2o_ch4_fuel_rates)), 
                \(x) round(x, 3)), # round to three decimals
         across(contains(n2o_ch4_fuel_rates), 
                \(x) round(x, 4))) # round N2O and CH4 fuel specific rates to 4 decimal points


## Format to eGRID output -------

nerc_formatted <- 
  nerc_rounded %>% 
  relocate(nerc_nox_output_rate_fossil, .after = nerc_nox_output_rate_gas) %>% 
  relocate(nerc_nox_oz_output_rate_fossil, .after = nerc_nox_oz_output_rate_gas) %>% 
  relocate(nerc_so2_output_rate_fossil, .after = nerc_so2_output_rate_gas) %>% 
  relocate(nerc_co2_output_rate_fossil, .after = nerc_co2_output_rate_gas) %>% 
  relocate(nerc_co2e_output_rate_fossil, .after = nerc_co2e_output_rate_gas) %>% 
  relocate(nerc_ch4_output_rate_fossil, .after = nerc_ch4_output_rate_gas) %>% 
  relocate(nerc_n2o_output_rate_fossil, .after = nerc_n2o_output_rate_gas) %>% 
  relocate(nerc_hg_output_rate_coal, .after = nerc_co2e_output_rate_fossil) %>% 
  relocate(nerc_hg_output_rate_fossil, .after = nerc_hg_output_rate_coal) %>% 
  relocate(nerc_nox_input_rate_fossil, .after = nerc_nox_input_rate_gas) %>% 
  relocate(nerc_nox_oz_input_rate_fossil, .after = nerc_nox_oz_input_rate_gas) %>% 
  relocate(nerc_so2_input_rate_fossil, .after = nerc_so2_input_rate_gas) %>% 
  relocate(nerc_co2_input_rate_fossil, .after = nerc_co2_input_rate_gas) %>% 
  relocate(nerc_co2e_input_rate_fossil, .after = nerc_co2e_input_rate_gas) %>% 
  relocate(nerc_ch4_input_rate_fossil, .after = nerc_ch4_input_rate_gas) %>% 
  relocate(nerc_n2o_input_rate_fossil, .after = nerc_n2o_input_rate_gas) %>% 
  relocate(nerc_hg_output_rate_coal, .after = nerc_co2e_output_rate_fossil) %>% 
  relocate(nerc_hg_input_rate_fossil, .after = nerc_hg_input_rate_coal) 


# Export NERC aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving NERC aggregation file to folder data/outputs/")

write_rds(nerc_formatted, "data/outputs/nerc_aggregation.RDS")




