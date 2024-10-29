## -------------------------------
##
## eGRID subregion aggregation file create 
## 
## Purpose: 
## 
## This file creates the eGRID subregion aggregation file for eGRID. 
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
    "ba_id",
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


# eGRID subregion level aggregation ------

# sum capacity, generation, emissions mass to eGRID sub_region level 

subregion <- 
  plant_file %>% 
  group_by(year, sub_region, sub_region_name) %>% 
  summarize(across(.cols = c("nameplate_capacity", 
                             contains("heat_input"), 
                             "generation_ann", 
                             "generation_oz", 
                             contains("_mass")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "subregion_{.col}")) %>% 
  mutate(subregion_hg_mass = "--") %>% 
  ungroup()


## Calculate emission rates ------

### Output emission rates (lb/MWh) -----

subregion_output_rates <- 
  subregion %>% 
  mutate(# calculating output emissions rates (lb/MWh)
    across(.cols = c("subregion_nox_mass",  
                     "subregion_so2_mass", 
                     "subregion_co2_mass", 
                     "subregion_co2e_mass"), 
           .fns = ~ 2000 * . / subregion_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    subregion_nox_oz_output_rate = 2000 * subregion_nox_oz_mass / subregion_generation_oz, 
    across(.cols = c("subregion_ch4_mass", 
                     "subregion_n2o_mass"),
           .fns = ~ . / subregion_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    subregion_hg_output_rate = "--") %>% 
  relocate(subregion_nox_oz_output_rate, .after = subregion_nox_output_rate) %>%  
  relocate(subregion_co2e_output_rate, .after = subregion_n2o_output_rate) %>% 
  select(sub_region, contains("rate"))


### Input emission rates (lb/MMBtu) -----

subregion_input_rates <- 
  subregion %>% 
  mutate(# calculating input emission rates (lb/MMBtu)
    across(.cols = c("subregion_nox_mass",  
                     "subregion_so2_mass", 
                     "subregion_co2_mass", 
                     "subregion_co2e_mass"), 
           .fns = ~ 2000 * . / subregion_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    subregion_nox_oz_input_rate = 2000 * subregion_nox_oz_mass / subregion_combust_heat_input_oz, 
    across(.cols = c("subregion_ch4_mass", 
                     "subregion_n2o_mass"),
           .fns = ~ . / subregion_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    subregion_hg_input_rate = "--") %>% 
  relocate(subregion_nox_oz_input_rate, .after = subregion_nox_input_rate) %>%  
  relocate(subregion_co2e_input_rate, .after = subregion_n2o_input_rate) %>% 
  select(sub_region, contains("rate"))


### Combustion emission rates (lb/MWh) -----

subregion_combustion_rates <- 
  plant_file %>% 
  group_by(sub_region) %>% 
  summarize(subregion_combustion_gen = sum(ann_gen_combust, na.rm = TRUE)) %>% 
  left_join(subregion, by = c("sub_region")) %>% 
  mutate(# calculating combustion emissions rates (lb/MWh)
    across(.cols = c("subregion_nox_mass",  
                     "subregion_so2_mass", 
                     "subregion_co2_mass", 
                     "subregion_co2e_mass"), 
           .fns = ~ 2000 * . / subregion_combustion_gen, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    subregion_nox_oz_combustion_rate = 2000 * subregion_nox_oz_mass / 
                                      (subregion_combustion_gen * (subregion_generation_oz / subregion_generation_ann)), 
    across(.cols = c("subregion_ch4_mass", 
                     "subregion_n2o_mass"),
           .fns = ~ . / subregion_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    subregion_hg_combustion_rate = "--") %>% 
  relocate(subregion_nox_oz_combustion_rate, .after = subregion_nox_combustion_rate) %>%  
  relocate(subregion_co2e_combustion_rate, .after = subregion_n2o_combustion_rate) %>% 
  select(sub_region, contains("rate"))


### Fuel type output emission rates (lb/MWh) and input emission rates (lb/MMBtu)  -----

# calculate emission rates by fossil fuel types 

fossil_fuels <- c("COAL", 
                  "OIL",
                  "GAS", 
                  "OSFL")

subregion_fuel_rates <-
  plant_file %>% 
  group_by(sub_region, primary_fuel_category) %>% 
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
                   .names = "subregion_{.col}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("subregion_nox_mass",  
                     "subregion_so2_mass", 
                     "subregion_co2_mass", 
                     "subregion_co2e_mass"), 
           .fns = ~ 2000 * . / subregion_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    subregion_nox_oz_output_rate = 2000 * subregion_nox_oz_mass / subregion_generation_oz, 
    across(.cols = c("subregion_ch4_mass", 
                     "subregion_n2o_mass"),
           .fns = ~ . / subregion_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("subregion_nox_mass",  
                     "subregion_so2_mass", 
                     "subregion_co2_mass", 
                     "subregion_co2e_mass"), 
           .fns = ~ 2000 * . / subregion_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    subregion_nox_oz_input_rate = 2000 * subregion_nox_oz_mass / subregion_combust_heat_input_oz, 
    across(.cols = c("subregion_ch4_mass", 
                     "subregion_n2o_mass"),
           .fns = ~ . / subregion_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
  select(sub_region, primary_fuel_category, contains("rate")) %>% 
  relocate(subregion_nox_oz_output_rate, .after = subregion_nox_output_rate) %>% 
  relocate(subregion_co2e_output_rate, .after = subregion_n2o_output_rate) %>% 
  relocate(subregion_nox_oz_input_rate, .after = subregion_nox_input_rate) %>% 
  relocate(subregion_co2e_input_rate, .after = subregion_n2o_input_rate) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = contains("rate")) %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0)), 
         subregion_hg_output_rate_coal = "--", 
         subregion_hg_output_rate_fossil = "--", 
         subregion_hg_input_rate_coal = "--", 
         subregion_hg_input_rate_fossil = "--")


# calculate all fossil fuel output and input emission rates 

subregion_fossil_rates <-
  plant_file %>% 
  group_by(sub_region) %>% 
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
                   .names = "subregion_{.col}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("subregion_nox_mass",  
                     "subregion_so2_mass", 
                     "subregion_co2_mass", 
                     "subregion_co2e_mass"), 
           .fns = ~ 2000 * . / subregion_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
    subregion_nox_oz_output_rate_fossil = 2000 * subregion_nox_oz_mass / subregion_generation_oz, 
    across(.cols = c("subregion_ch4_mass", 
                     "subregion_n2o_mass"),
           .fns = ~ . / subregion_generation_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("subregion_nox_mass",  
                     "subregion_so2_mass", 
                     "subregion_co2_mass", 
                     "subregion_co2e_mass"), 
           .fns = ~ 2000 * . / subregion_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    subregion_nox_oz_input_rate_fossil = 2000 * subregion_nox_oz_mass / subregion_combust_heat_input_oz, 
    across(.cols = c("subregion_ch4_mass", 
                     "subregion_n2o_mass"),
           .fns = ~ . / subregion_combust_heat_input, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    across(where(is.numeric), ~ replace_na(., 0))) %>% 
  relocate(subregion_co2e_input_rate_fossil, .after = subregion_n2o_input_rate_fossil) %>%  
  relocate(subregion_nox_oz_input_rate_fossil, .after = subregion_nox_input_rate_fossil) %>% 
  relocate(subregion_nox_oz_output_rate_fossil, .after = subregion_nox_output_rate_fossil) %>% 
  relocate(subregion_co2e_output_rate_fossil, .after = subregion_n2o_output_rate_fossil) %>% 
  select(sub_region, contains("rate"))  



### Non-baseload output emission rates (lb/MWh) -----

subregion_nonbaseload_rates <- 
  plant_file %>% 
  group_by(sub_region) %>% 
  summarize(across(.cols = c("generation_ann", 
                             "generation_oz", 
                             contains("mass")), 
                   .fns = ~ sum(. * nonbaseload, na.rm = TRUE), 
                   .names = "subregion_{.col}")) %>% 
  mutate(across(.cols = c("subregion_nox_mass",  
                          "subregion_so2_mass", 
                          "subregion_co2_mass", 
                          "subregion_co2e_mass"), 
                .fns = ~ 2000 * . / subregion_generation_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
         subregion_nox_oz_output_rate_nonbaseload = 2000 * subregion_nox_oz_mass / subregion_generation_oz, 
         across(.cols = c("subregion_ch4_mass", 
                          "subregion_n2o_mass"),
                .fns = ~ . / subregion_generation_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
         subregion_hg_output_rate_nonbaseload = "--") %>% 
  relocate(subregion_nox_oz_output_rate_nonbaseload, .after = subregion_nox_output_rate_nonbaseload) %>% 
  relocate(subregion_co2e_output_rate_nonbaseload, .after = subregion_n2o_output_rate_nonbaseload) %>%
  select(sub_region, contains("rate"))


## Calculate net generation and resource mix -----

### Generation by fuel category (MWh) -----

subregion_gen <- 
  plant_file %>% 
  group_by(sub_region) %>% 
  summarize(across(.cols = c(contains("gen")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "subregion_{.col}")) 

subregion_gen_2 <- 
  subregion_gen %>% 
  select(-subregion_generation_ann, -subregion_generation_oz) # remove duplicate columns for final formatting

### Resource mix by fuel category (%) -----

subregion_resource_mix <- 
  subregion_gen %>% 
  select(-subregion_generation_oz) %>%   
  mutate(across(.cols = -c("sub_region", "subregion_generation_ann"), 
                .fns = ~ . / subregion_generation_ann, # convert to percentage 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(sub_region, contains("resource_mix"))


### Nonbaseload generation (MWh) -----

subregion_nonbaseload_gen <- 
  plant_file %>% 
  group_by(sub_region, primary_fuel_category) %>% 
  summarize(subregion_nonbaseload_gen = sum(generation_ann * nonbaseload, na.rm = TRUE)) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = subregion_nonbaseload_gen, 
              names_prefix = "subregion_nonbaseload_gen_") %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) 


### Nonbaseload resource mix (%) -----

subregion_nonbaseload_resource_mix <- 
  subregion_nonbaseload_gen %>% 
  mutate(subregion_nonbaseload_gen = rowSums(pick(contains("nonbaseload"))), 
         across(.cols = -c("subregion_nonbaseload_gen"), 
                .fns = ~ . / subregion_nonbaseload_gen, 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(sub_region, contains("resource_mix"))


# Create final data frame -----

## Join necessary data -----

subregion_merged <- 
  subregion %>% 
  left_join(subregion_output_rates, by = c("sub_region")) %>% # output emission rates
  left_join(subregion_input_rates, by = c("sub_region")) %>% # input emission rates
  left_join(subregion_combustion_rates, by = c("sub_region")) %>% # combustion emission rates
  left_join(subregion_fuel_rates, by = c("sub_region")) %>% # output and input emission rates by fuel type
  left_join(subregion_fossil_rates, by = c("sub_region")) %>% # output and input emission rates for all fossil fuels
  left_join(subregion_nonbaseload_rates, by = c("sub_region")) %>% # output emission rates for nonbaseload generation
  left_join(subregion_gen_2, by = c("sub_region")) %>% # generation by fuel category
  left_join(subregion_resource_mix, by = c("sub_region")) %>% # resource mix by fuel category
  left_join(subregion_nonbaseload_gen, by = c("sub_region")) %>% # nonbaseload generation by fuel category
  left_join(subregion_nonbaseload_resource_mix, by = c("sub_region")) %>% 
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

subregion_rounded <- 
  subregion_merged %>% 
  mutate(across(c(where(is.numeric), -contains(n2o_ch4_fuel_rates)), 
                \(x) round(x, 3)), # round to three decimals
         across(contains(n2o_ch4_fuel_rates), 
                \(x) round(x, 4))) # round N2O and CH4 fuel specific rates to 4 decimal points


## Format to subregion output -------

subregion_formatted <- 
  subregion_rounded %>% 
  relocate(subregion_nox_output_rate_fossil, .after = subregion_nox_output_rate_gas) %>% 
  relocate(subregion_nox_oz_output_rate_fossil, .after = subregion_nox_oz_output_rate_gas) %>% 
  relocate(subregion_so2_output_rate_fossil, .after = subregion_so2_output_rate_gas) %>% 
  relocate(subregion_co2_output_rate_fossil, .after = subregion_co2_output_rate_gas) %>% 
  relocate(subregion_co2e_output_rate_fossil, .after = subregion_co2e_output_rate_gas) %>% 
  relocate(subregion_ch4_output_rate_fossil, .after = subregion_ch4_output_rate_gas) %>% 
  relocate(subregion_n2o_output_rate_fossil, .after = subregion_n2o_output_rate_gas) %>% 
  relocate(subregion_hg_output_rate_coal, .after = subregion_co2e_output_rate_fossil) %>% 
  relocate(subregion_hg_output_rate_fossil, .after = subregion_hg_output_rate_coal) %>% 
  relocate(subregion_nox_input_rate_fossil, .after = subregion_nox_input_rate_gas) %>% 
  relocate(subregion_nox_oz_input_rate_fossil, .after = subregion_nox_oz_input_rate_gas) %>% 
  relocate(subregion_so2_input_rate_fossil, .after = subregion_so2_input_rate_gas) %>% 
  relocate(subregion_co2_input_rate_fossil, .after = subregion_co2_input_rate_gas) %>% 
  relocate(subregion_co2e_input_rate_fossil, .after = subregion_co2e_input_rate_gas) %>% 
  relocate(subregion_ch4_input_rate_fossil, .after = subregion_ch4_input_rate_gas) %>% 
  relocate(subregion_n2o_input_rate_fossil, .after = subregion_n2o_input_rate_gas) %>% 
  relocate(subregion_hg_output_rate_coal, .after = subregion_co2e_output_rate_fossil) %>% 
  relocate(subregion_hg_input_rate_fossil, .after = subregion_hg_input_rate_coal) 


# Export eGRID subregion aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
} else {
  dir.create("data/outputs")
}

print("Saving eGRID subregion aggregation file to folder data/outputs/")

write_rds(subregion_formatted, "data/outputs/egrid_subregion_aggregation.RDS")


