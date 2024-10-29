## -------------------------------
##
## Balance authority aggregation file create 
## 
## Purpose: 
## 
## This file creates the balance authority aggregation file for eGRID. 
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


# BA level aggregation ------

# sum capacity, generation, emissions mass to BA level 

ba <- 
  plant_file %>% 
  group_by(year, ba_name, ba_code) %>% 
  summarize(across(.cols = c("nameplate_capacity", 
                             contains("heat_input"), 
                             "generation_ann", 
                             "generation_oz", 
                             contains("_mass")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "ba_{.col}")) %>% 
  mutate(ba_hg_mass = "--") %>% 
  ungroup()


## Calculate emission rates ------

### Output emission rates (lb/MWh) -----

ba_output_rates <- 
  ba %>% 
  mutate(# calculating output emissions rates (lb/MWh)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    ba_nox_oz_output_rate = 2000 * ba_nox_oz_mass / ba_generation_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    ba_hg_output_rate = "--") %>% 
  relocate(ba_nox_oz_output_rate, .after = ba_nox_output_rate) %>%  
  relocate(ba_co2e_output_rate, .after = ba_n2o_output_rate) %>% 
  select(ba_name, ba_code, contains("rate"))


### Input emission rates (lb/MMBtu) -----

ba_input_rates <- 
  ba %>% 
  mutate(# calculating input emission rates (lb/MMBtu)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    ba_nox_oz_input_rate = 2000 * ba_nox_oz_mass / ba_combust_heat_input_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    ba_hg_input_rate = "--") %>% 
  relocate(ba_nox_oz_input_rate, .after = ba_nox_input_rate) %>% 
  relocate(ba_co2e_input_rate, .after = ba_n2o_input_rate) %>% 
  select(ba_name, ba_code, contains("rate"))


### Combustion emission rates (lb/MWh) -----

ba_combustion_rates <- 
  plant_file %>% 
  group_by(ba_name, ba_code) %>% 
  summarize(ba_combustion_gen = sum(ann_gen_combust, na.rm = TRUE)) %>% 
  left_join(ba, by = c("ba_name", "ba_code")) %>% 
  mutate(# calculating combustion emissions rates (lb/MWh)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_combustion_gen, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    ba_nox_oz_combustion_rate = 2000 * ba_nox_oz_mass / (ba_combustion_gen * (ba_generation_oz / ba_generation_ann)), 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    ba_hg_combustion_rate = "--") %>% 
  relocate(ba_nox_oz_combustion_rate, .after = ba_nox_combustion_rate) %>%  
  relocate(ba_co2e_combustion_rate, .after = ba_n2o_combustion_rate) %>% 
  select(ba_name, ba_code, contains("rate"))


### Fuel type output emission rates (lb/MWh) and input emission rates (lb/MMBtu)  -----

# calculate emission rates by fossil fuel types 

fossil_fuels <- c("COAL", 
                  "OIL",
                  "GAS", 
                  "OSFL")

ba_fuel_rates <-
  plant_file %>% 
  group_by(ba_name, ba_code, primary_fuel_category) %>% 
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
                   .names = "ba_{.col}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    ba_nox_oz_output_rate = 2000 * ba_nox_oz_mass / ba_generation_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    ba_nox_oz_input_rate = 2000 * ba_nox_oz_mass / ba_combust_heat_input_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
  select(ba_name, ba_code, primary_fuel_category, contains("rate")) %>% 
  relocate(ba_nox_oz_output_rate, .after = ba_nox_output_rate) %>% 
  relocate(ba_co2e_output_rate, .after = ba_n2o_output_rate) %>% 
  relocate(ba_nox_oz_input_rate, .after = ba_nox_input_rate) %>% 
  relocate(ba_co2e_input_rate, .after = ba_n2o_input_rate) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = contains("rate")) %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0)), 
         ba_hg_output_rate_coal = "--", 
         ba_hg_output_rate_fossil = "--", 
         ba_hg_input_rate_coal = "--", 
         ba_hg_input_rate_fossil = "--")


# calculate all fossil fuel output and input emission rates 

ba_fossil_rates <-
  plant_file %>% 
  group_by(ba_name, ba_code) %>% 
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
                   .names = "ba_{.col}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
    ba_nox_oz_output_rate_fossil = 2000 * ba_nox_oz_mass / ba_generation_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    ba_nox_oz_input_rate_fossil = 2000 * ba_nox_oz_mass / ba_combust_heat_input_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    across(where(is.numeric), ~ replace_na(., 0))) %>% 
  relocate(ba_nox_oz_input_rate_fossil, .after = ba_nox_input_rate_fossil) %>% 
  select(ba_name, ba_code, contains("rate"))  



### Non-baseload output emission rates (lb/MWh) -----

ba_nonbaseload_rates <- 
  plant_file %>% 
  group_by(ba_name, ba_code) %>% 
  summarize(across(.cols = c("generation_ann", 
                             "generation_oz", 
                             contains("mass")), 
                   .fns = ~ sum(. * nonbaseload, na.rm = TRUE), 
                   .names = "ba_{.col}")) %>% 
  mutate(across(.cols = c("ba_nox_mass",  
                          "ba_so2_mass", 
                          "ba_co2_mass", 
                          "ba_co2e_mass"), 
                .fns = ~ 2000 * . / ba_generation_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
         ba_nox_oz_output_rate_nonbaseload = 2000 * ba_nox_oz_mass / ba_generation_oz, 
         across(.cols = c("ba_ch4_mass", 
                          "ba_n2o_mass"),
                .fns = ~ . / ba_generation_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
         ba_hg_output_rate_nonbaseload = "--") %>% 
  relocate(ba_nox_oz_output_rate_nonbaseload, .after = ba_nox_output_rate_nonbaseload) %>% 
  relocate(ba_co2e_output_rate_nonbaseload, .after = ba_n2o_output_rate_nonbaseload) %>% 
  select(ba_name, ba_code, contains("rate"))


## Calculate net generation and resource mix -----

### Generation by fuel category (MWh) -----

ba_gen <- 
  plant_file %>% 
  group_by(ba_name, ba_code) %>% 
  summarize(across(.cols = c(contains("gen")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "ba_{.col}")) %>% 
  ungroup()

ba_gen_2 <- 
  ba_gen %>% 
  select(-ba_generation_ann, -ba_generation_oz) # remove duplicate columns for final formatting

### Resource mix by fuel category (%) -----

ba_resource_mix <- 
  ba_gen %>% 
  select(-ba_generation_oz) %>%  
  mutate(across(.cols = -c("ba_name", "ba_code", "ba_generation_ann"), 
                .fns = ~ . / ba_generation_ann, # convert to percentage 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(ba_name, ba_code, contains("resource_mix"))


### Nonbaseload generation (MWh) -----

ba_nonbaseload_gen <- 
  plant_file %>% 
  group_by(ba_name, ba_code, primary_fuel_category) %>% 
  summarize(ba_nonbaseload_gen = sum(generation_ann * nonbaseload, na.rm = TRUE)) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = ba_nonbaseload_gen, 
              names_prefix = "ba_nonbaseload_gen_") %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) 


### Nonbaseload resource mix (%) -----

ba_nonbaseload_resource_mix <- 
  ba_nonbaseload_gen %>% 
  mutate(ba_nonbaseload_gen = rowSums(pick(contains("nonbaseload"))), 
         across(.cols = -c("ba_nonbaseload_gen"), 
                .fns = ~ . / ba_nonbaseload_gen, 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(ba_name, ba_code, contains("resource_mix"))


# Create final data frame -----

## Join necessary data -----

ba_merged <- 
  ba %>% 
  left_join(ba_output_rates, by = c("ba_name", "ba_code")) %>% # output emission rates
  left_join(ba_input_rates, by = c("ba_name", "ba_code")) %>% # input emission rates
  left_join(ba_combustion_rates, by = c("ba_name", "ba_code")) %>% # combustion emission rates
  left_join(ba_fuel_rates, by = c("ba_name", "ba_code")) %>% # output and input emission rates by fuel type
  left_join(ba_fossil_rates, by = c("ba_name", "ba_code")) %>% # output and input emission rates for all fossil fuels
  left_join(ba_nonbaseload_rates, by = c("ba_name", "ba_code")) %>% # output emission rates for nonbaseload generation
  left_join(ba_gen_2, by = c("ba_name", "ba_code")) %>% # generation by fuel category
  left_join(ba_resource_mix, by = c("ba_name", "ba_code")) %>% # resource mix by fuel category
  left_join(ba_nonbaseload_gen, by = c("ba_name", "ba_code")) %>% # nonbaseload generation by fuel category
  left_join(ba_nonbaseload_resource_mix, by = c("ba_name", "ba_code")) %>% 
  mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
         across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
  select(-contains("gen_na"),
         -contains("mix_na")) # remove unnecessary columns


## Round data ----
ba_rounded <- 
  ba_merged %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals


## Format to eGRID output -------

ba_formatted <- 
  ba_rounded %>% 
  relocate(ba_nox_output_rate_fossil, .after = ba_nox_output_rate_gas) %>% 
  relocate(ba_nox_oz_output_rate_fossil, .after = ba_nox_oz_output_rate_gas) %>% 
  relocate(ba_so2_output_rate_fossil, .after = ba_so2_output_rate_gas) %>% 
  relocate(ba_co2_output_rate_fossil, .after = ba_co2_output_rate_gas) %>% 
  relocate(ba_co2e_output_rate_fossil, .after = ba_co2e_output_rate_gas) %>% 
  relocate(ba_ch4_output_rate_fossil, .after = ba_ch4_output_rate_gas) %>% 
  relocate(ba_n2o_output_rate_fossil, .after = ba_n2o_output_rate_gas) %>% 
  relocate(ba_hg_output_rate_coal, .after = ba_co2e_output_rate_fossil) %>% 
  relocate(ba_hg_output_rate_fossil, .after = ba_hg_output_rate_coal) %>% 
  relocate(ba_nox_input_rate_fossil, .after = ba_nox_input_rate_gas) %>% 
  relocate(ba_nox_oz_input_rate_fossil, .after = ba_nox_oz_input_rate_gas) %>% 
  relocate(ba_so2_input_rate_fossil, .after = ba_so2_input_rate_gas) %>% 
  relocate(ba_co2_input_rate_fossil, .after = ba_co2_input_rate_gas) %>% 
  relocate(ba_co2e_input_rate_fossil, .after = ba_co2e_input_rate_gas) %>% 
  relocate(ba_ch4_input_rate_fossil, .after = ba_ch4_input_rate_gas) %>% 
  relocate(ba_n2o_input_rate_fossil, .after = ba_n2o_input_rate_gas) %>% 
  relocate(ba_hg_output_rate_coal, .after = ba_co2e_output_rate_fossil) %>% 
  relocate(ba_hg_input_rate_fossil, .after = ba_hg_input_rate_coal) 


# Export BA aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
} else {
   dir.create("data/outputs")
}

print("Saving BA aggregation file to folder data/outputs/")

write_rds(ba_formatted, "data/outputs/ba_aggregation.RDS")



