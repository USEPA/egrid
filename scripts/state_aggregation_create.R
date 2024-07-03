

# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)


# Load and clean necessary data ------

# rename columns 

columns_to_keep <- 
  c("year", 
    "state" = "pstatabb", 
    "state_fips_code" = "fipsst", 
    "plant_name", 
    "plant_id", 
    "nerc", 
    "sub_region", 
    "sub_region_name" = "srname",
    "balance_authority_name", 
    "balance_authority_code",
    "primary_fuel_type" = "plprmfl", 
    "primary_fuel_category" = "plfuelct", 
    "plant_nameplate_capacity" = "namepcap", 
    "capacity_factor" = "capfac", 
    "nonbaseload_factor" = "nbfactor", 
    "plant_heat_input_comb" = "plhtian", 
    "plant_heat_input_comb_oz" = "plhtioz", 
    "plant_heat_input_ann" = "plhtiant", 
    "plant_heat_input_oz" = "plhtiozt", 
    "plant_gen_ann" = "plngenan", 
    "plant_gen_oz" = "plngenoz", 
    "plant_nox_mass" = "plnoxan", 
    "plant_nox_oz_mass" = "plnoxoz", 
    "plant_so2_mass" = "plso2an", 
    "plant_co2_mass" = "plco2an", 
    "plant_ch4_mass" = "plch4an", 
    "plant_n2o_mass" = "pln2oan", 
    "plant_co2e_mass" = "plco2eqa",
    "plant_hg_mass" = "plhgan", 
    "plant_coal_gen" = "plgenacl", 
    "plant_oil_gen" = "plgenaol", 
    "plant_gas_gen" = "plgenags", 
    "plant_nuclear_gen" = "plgenanc", 
    "plant_hydro_gen" = "plgenahy",
    "plant_biomass_gen" = "plgenabm", 
    "plant_wind_gen" = "plgenawi", 
    "plant_solar_gen" = "plgenaso", 
    "plant_geothermal_gen" = "plgenagt", 
    "plant_other_fossil_gen" = "plgenaof", 
    "plant_other_purchased_gen" = "plgenaop", 
    "plant_nonre_gen" = "plgenatn", 
    "plant_re_gen" = "plgenatr", 
    "plant_re_nonhydro_gen" = "plgenath", 
    "plant_combustion_gen" = "plgenacy", 
    "plant_noncombustion_gen" = "plgenacn"
    )  


# read in plant file 

plant <- 
  read_rds("data/outputs/plant_file_2021.RDS") %>% 
  select(any_of(columns_to_keep))

# factor plant_fuel_category to final eGRID output order
plant$primary_fuel_category <- factor(plant$primary_fuel_category, 
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


# State level aggregation ------

# sum capacity, generation, emissions mass to state level 

state <- 
  plant %>% 
  group_by(year, state, state_fips_code) %>% 
  summarize(across(.cols = c("plant_nameplate_capacity", 
                             contains("heat_input"), 
                             "plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("_mass")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'state_')}")) %>% 
  mutate(state_hg_mass = "--") %>% 
  ungroup()


## Calculate emission rates ------

### Output emission rates (lb/MWh) -----

state_output_rates <- 
  state %>% 
  mutate(# calculating output emissions rates (lb/MWh)
         across(.cols = c("state_nox_mass",  
                          "state_so2_mass", 
                          "state_co2_mass", 
                          "state_co2e_mass"), 
                .fns = ~ 2000 * . / state_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
         state_nox_oz_output_rate = 2000 * state_nox_oz_mass / state_gen_oz, 
         across(.cols = c("state_ch4_mass", 
                          "state_n2o_mass"),
                .fns = ~ . / state_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate"),
         state_hg_output_rate = "--") %>% 
  relocate(state_nox_oz_output_rate, .after = state_nox_output_rate) %>% 
  relocate(state_co2e_output_rate, .after = state_n2o_output_rate) %>% 
  select(state, contains("rate"))
  

### Input emission rates (lb/MMBtu) -----

state_input_rates <- 
  state %>% 
  mutate(# calculating input emission rates (lb/MMBtu)
         across(.cols = c("state_nox_mass",  
                          "state_so2_mass", 
                          "state_co2_mass", 
                          "state_co2e_mass"), 
                .fns = ~ 2000 * . / state_heat_input_comb, 
                .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
         state_nox_oz_input_rate = 2000 * state_nox_oz_mass / state_heat_input_comb_oz, 
         across(.cols = c("state_ch4_mass", 
                          "state_n2o_mass"),
                .fns = ~ . / state_heat_input_comb, 
                .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
         state_hg_input_rate = "--") %>% 
  relocate(state_nox_oz_input_rate, .after = state_nox_input_rate) %>% 
  relocate(state_co2e_input_rate, .after = state_n2o_input_rate) %>% 
  select(state, contains("rate"))


### Combustion emission rates (lb/MWh) -----

state_combustion_rates <- 
  plant %>% 
  group_by(state) %>% 
  summarize(state_combustion_gen = sum(plant_combustion_gen, na.rm = TRUE)) %>% 
  left_join(state, by = c("state")) %>% 
  mutate(# calculating combustion emissions rates (lb/MWh)
         across(.cols = c("state_nox_mass",  
                          "state_so2_mass", 
                          "state_co2_mass", 
                          "state_co2e_mass"), 
                .fns = ~ 2000 * . / state_combustion_gen, 
                .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
         state_nox_oz_combustion_rate = 2000 * state_nox_oz_mass / (state_combustion_gen * (state_gen_oz / state_gen_ann)), 
         across(.cols = c("state_ch4_mass", 
                          "state_n2o_mass"),
                .fns = ~ . / state_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
         state_hg_combustion_rate = "--") %>% 
  relocate(state_nox_oz_combustion_rate, .after = state_nox_combustion_rate) %>%  
  relocate(state_co2e_combustion_rate, .after = state_n2o_combustion_rate) %>% 
  select(state, contains("rate"))


### Fuel type output emission rates (lb/MWh) and input emission rates (lb/MMBtu)  -----

# calculate emission rates by fossil fuel types 

fossil_fuels <- c("COAL", 
                  "OIL",
                  "GAS", 
                  "OSFL")

state_fuel_rates <-
  plant %>% 
  group_by(state, primary_fuel_category) %>% 
  filter(primary_fuel_category %in% fossil_fuels, 
         !primary_fuel_category == 'OSFL') %>% # do not include other fossil in individual fuel rate calculations
  summarize(across(.cols = c(contains("heat_input"), 
                             "plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("mass"), 
                             "plant_coal_gen", 
                             "plant_oil_gen", 
                             "plant_gas_gen", 
                             "plant_other_fossil_gen"), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'state_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
         across(.cols = c("state_nox_mass",  
                          "state_so2_mass", 
                          "state_co2_mass", 
                          "state_co2e_mass"), 
                .fns = ~ 2000 * . / state_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
         state_nox_oz_output_rate = 2000 * state_nox_oz_mass / state_gen_oz, 
         across(.cols = c("state_ch4_mass", 
                         "state_n2o_mass"),
                .fns = ~ . / state_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate"),
         
         # input emission rates (lb/MMBtu)
         across(.cols = c("state_nox_mass",  
                          "state_so2_mass", 
                          "state_co2_mass", 
                          "state_co2e_mass"), 
                .fns = ~ 2000 * . / state_heat_input_comb, 
                .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
         state_nox_oz_input_rate = 2000 * state_nox_oz_mass / state_heat_input_comb_oz, 
         across(.cols = c("state_ch4_mass", 
                          "state_n2o_mass"),
                .fns = ~ . / state_heat_input_comb, 
                .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
  select(state, primary_fuel_category, contains("rate")) %>% 
  relocate(state_nox_oz_output_rate, .after = state_nox_output_rate) %>% 
  relocate(state_co2e_output_rate, .after = state_n2o_output_rate) %>% 
  relocate(state_nox_oz_input_rate, .after = state_nox_input_rate) %>% 
  relocate(state_co2e_input_rate, .after = state_n2o_input_rate) %>%
  pivot_wider(names_from = primary_fuel_category, 
              values_from = contains("rate")) %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0)), 
         state_hg_output_rate_coal = "--", 
         state_hg_output_rate_fossil = "--", 
         state_hg_input_rate_coal = "--", 
         state_hg_input_rate_fossil = "--")


# calculate all fossil fuel output and input emission rates 
         
state_fossil_rates <-
  plant %>% 
  group_by(state) %>% 
  filter(primary_fuel_category %in% fossil_fuels) %>% 
  summarize(across(.cols = c(contains("heat_input"), 
                             "plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("mass"), 
                             "plant_coal_gen", 
                             "plant_oil_gen", 
                             "plant_gas_gen", 
                             "plant_other_fossil_gen"), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'state_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("state_nox_mass",  
                     "state_so2_mass", 
                     "state_co2_mass", 
                     "state_co2e_mass"), 
           .fns = ~ 2000 * . / state_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
    state_nox_oz_output_rate_fossil = 2000 * state_nox_oz_mass / state_gen_oz, 
    across(.cols = c("state_ch4_mass", 
                     "state_n2o_mass"),
           .fns = ~ . / state_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("state_nox_mass",  
                     "state_so2_mass", 
                     "state_co2_mass", 
                     "state_co2e_mass"), 
           .fns = ~ 2000 * . / state_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    state_nox_oz_input_rate_fossil = 2000 * state_nox_oz_mass / state_heat_input_comb_oz, 
    across(.cols = c("state_ch4_mass", 
                     "state_n2o_mass"),
           .fns = ~ . / state_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    across(where(is.numeric), ~ replace_na(., 0))) %>% 
  relocate(state_nox_oz_input_rate_fossil, .after = state_nox_input_rate_fossil) %>% 
  select(state, contains("rate"))  
  


### Non-baseload output emission rates (lb/MWh) -----

state_nonbaseload_rates <- 
  plant %>% 
  group_by(state) %>% 
  summarize(across(.cols = c("plant_gen_ann", 
                          "plant_gen_oz", 
                          contains("mass")), 
                .fns = ~ sum(. * nonbaseload_factor, na.rm = TRUE), 
                .names = "{str_replace(.col, 'plant_', 'state_')}")) %>% 
  mutate(across(.cols = c("state_nox_mass",  
                          "state_so2_mass", 
                          "state_co2_mass", 
                          "state_co2e_mass"), 
                .fns = ~ 2000 * . / state_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
         state_nox_oz_output_rate_nonbaseload = 2000 * state_nox_oz_mass / state_gen_oz, 
         across(.cols = c("state_ch4_mass", 
                          "state_n2o_mass"),
                .fns = ~ . / state_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
         state_hg_output_rate_nonbaseload = "--") %>% 
  relocate(state_nox_oz_output_rate_nonbaseload, .after = state_nox_output_rate_nonbaseload) %>% 
  relocate(state_co2e_output_rate_nonbaseload, .after = state_n2o_output_rate_nonbaseload) %>% 
  select(state, contains("rate"))
  

## Calculate net generation and resource mix -----

### Generation by fuel category (MWh) -----

state_gen <- 
  plant %>% 
  group_by(state) %>% 
  summarize(across(.cols = c(contains("gen")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'state_')}")) 

state_gen_2 <- 
  state_gen %>% 
  select(-state_gen_ann, -state_gen_oz) # remove duplicate columns for final formatting

### Resource mix by fuel category (%) -----

state_resource_mix <- 
  state_gen %>% 
  select(-state_gen_oz) %>%   
  mutate(across(.cols = -c("state", "state_gen_ann"), 
                .fns = ~ . / state_gen_ann,  
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>%
  select(state, contains("resource_mix"))


### Nonbaseload generation (MWh) -----

state_nonbaseload_gen <- 
  plant %>% 
  group_by(state, primary_fuel_category) %>% 
  summarize(state_nonbaseload_gen = sum(plant_gen_ann * nonbaseload_factor, na.rm = TRUE)) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = state_nonbaseload_gen, 
              names_prefix = "state_nonbaseload_gen_") %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) 


### Nonbaseload resource mix (%) -----

state_nonbaseload_resource_mix <- 
  state_nonbaseload_gen %>% 
  mutate(state_nonbaseload_gen = rowSums(pick(contains("nonbaseload"))), 
         across(.cols = -c("state_nonbaseload_gen"), 
                .fns = ~ . / state_nonbaseload_gen, 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(state, contains("resource_mix"))


# Create final data frame -----

## Join necessary data -----

state_merged <- 
  state %>% 
  left_join(state_output_rates, by = c("state")) %>% # output emission rates
  left_join(state_input_rates, by = c("state")) %>% # input emission rates
  left_join(state_combustion_rates, by = c("state")) %>% # combustion emission rates
  left_join(state_fuel_rates, by = c("state")) %>% # output and input emission rates by fuel type
  left_join(state_fossil_rates, by = c("state")) %>% # output and input emission rates for all fossil fuels
  left_join(state_nonbaseload_rates, by = c("state")) %>% # output emission rates for nonbaseload generation
  left_join(state_gen_2, by = c("state")) %>% # generation by fuel category
  left_join(state_resource_mix, by = c("state")) %>% # resource mix by fuel category
  left_join(state_nonbaseload_gen, by = c("state")) %>% # nonbaseload generation by fuel category
  left_join(state_nonbaseload_resource_mix, by = c("state")) %>% 
  mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
         across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
  select(-contains("gen_na"),
         -contains("mix_na")) # remove unnecessary columns


## Round data ----
state_rounded <- 
  state_merged %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals
  

## Format to eGRID output -------

state_formatted <- 
  state_rounded %>% 
  relocate(state_nox_output_rate_fossil, .after = state_nox_output_rate_gas) %>% 
  relocate(state_nox_oz_output_rate_fossil, .after = state_nox_oz_output_rate_gas) %>% 
  relocate(state_so2_output_rate_fossil, .after = state_so2_output_rate_gas) %>% 
  relocate(state_co2_output_rate_fossil, .after = state_co2_output_rate_gas) %>% 
  relocate(state_co2e_output_rate_fossil, .after = state_co2e_output_rate_gas) %>% 
  relocate(state_ch4_output_rate_fossil, .after = state_ch4_output_rate_gas) %>% 
  relocate(state_n2o_output_rate_fossil, .after = state_n2o_output_rate_gas) %>% 
  relocate(state_hg_output_rate_coal, .after = state_co2e_output_rate_fossil) %>% 
  relocate(state_hg_output_rate_fossil, .after = state_hg_output_rate_coal) %>% 
  relocate(state_nox_input_rate_fossil, .after = state_nox_input_rate_gas) %>% 
  relocate(state_nox_oz_input_rate_fossil, .after = state_nox_oz_input_rate_gas) %>% 
  relocate(state_so2_input_rate_fossil, .after = state_so2_input_rate_gas) %>% 
  relocate(state_co2_input_rate_fossil, .after = state_co2_input_rate_gas) %>% 
  relocate(state_co2e_input_rate_fossil, .after = state_co2e_input_rate_gas) %>% 
  relocate(state_ch4_input_rate_fossil, .after = state_ch4_input_rate_gas) %>% 
  relocate(state_n2o_input_rate_fossil, .after = state_n2o_input_rate_gas) %>% 
  relocate(state_hg_output_rate_coal, .after = state_n2o_output_rate_fossil) %>% 
  relocate(state_hg_input_rate_fossil, .after = state_hg_input_rate_coal) 


# Export state aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving state aggregation file to folder data/outputs/")

write_rds(state_formatted, "data/outputs/state_aggregation.RDS")




