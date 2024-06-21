

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


# BA level aggregation ------

# sum capacity, generation, emissions mass to BA level 

ba <- 
  plant %>% 
  group_by(year, balance_authority_name, balance_authority_code) %>% 
  summarize(across(.cols = c("plant_nameplate_capacity", 
                             contains("heat_input"), 
                             "plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("_mass")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'ba_')}")) %>% 
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
           .fns = ~ 2000 * . / ba_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    ba_nox_oz_output_rate = 2000 * ba_nox_oz_mass / ba_gen_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    ba_hg_output_rate = "--") %>% 
  relocate(ba_nox_oz_output_rate, .after = ba_nox_output_rate) %>%  
  relocate(ba_co2e_output_rate, .after = ba_n2o_output_rate) %>% 
  select(balance_authority_name, balance_authority_code, contains("rate"))


### Input emission rates (lb/MMBtu) -----

ba_input_rates <- 
  ba %>% 
  mutate(# calculating input emission rates (lb/MMBtu)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    ba_nox_oz_input_rate = 2000 * ba_nox_oz_mass / ba_heat_input_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    ba_hg_input_rate = "--") %>% 
  relocate(ba_nox_oz_input_rate, .after = ba_nox_input_rate) %>% 
  relocate(ba_co2e_input_rate, .after = ba_n2o_input_rate) %>% 
  select(balance_authority_name, balance_authority_code, contains("rate"))


### Combustion emission rates (lb/MWh) -----

ba_combustion_rates <- 
  plant %>% 
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(ba_combustion_gen = sum(plant_combustion_gen, na.rm = TRUE)) %>% 
  left_join(ba, by = c("balance_authority_name", "balance_authority_code")) %>% 
  mutate(# calculating combustion emissions rates (lb/MWh)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_combustion_gen, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    ba_nox_oz_combustion_rate = 2000 * ba_nox_oz_mass / (ba_combustion_gen * (ba_gen_oz / ba_gen_ann)), 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    ba_hg_combustion_rate = "--") %>% 
  relocate(ba_nox_oz_combustion_rate, .after = ba_nox_combustion_rate) %>%  
  relocate(ba_co2e_combustion_rate, .after = ba_n2o_combustion_rate) %>% 
  select(balance_authority_name, balance_authority_code, contains("rate"))


### Output emission rates (lb/MWh) and input emission rates (lb/MMBtu) by fuel type  -----

# calculate emission rates by fossil fuel types 

fossil_fuels <- c("COAL", 
                  "OIL",
                  "GAS", 
                  "OSFL")

ba_fuel_rates <-
  plant %>% 
  group_by(balance_authority_name, balance_authority_code, primary_fuel_category) %>% 
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
                   .names = "{str_replace(.col, 'plant_', 'ba_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    ba_nox_oz_output_rate = 2000 * ba_nox_oz_mass / ba_gen_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    ba_nox_oz_input_rate = 2000 * ba_nox_oz_mass / ba_heat_input_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
  select(balance_authority_name, balance_authority_code, primary_fuel_category, contains("rate")) %>% 
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
  plant %>% 
  group_by(balance_authority_name, balance_authority_code) %>% 
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
                   .names = "{str_replace(.col, 'plant_', 'ba_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
    ba_nox_oz_output_rate_fossil = 2000 * ba_nox_oz_mass / ba_gen_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("ba_nox_mass",  
                     "ba_so2_mass", 
                     "ba_co2_mass", 
                     "ba_co2e_mass"), 
           .fns = ~ 2000 * . / ba_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    ba_nox_oz_input_rate_fossil = 2000 * ba_nox_oz_mass / ba_heat_input_oz, 
    across(.cols = c("ba_ch4_mass", 
                     "ba_n2o_mass"),
           .fns = ~ . / ba_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    across(where(is.numeric), ~ replace_na(., 0))) %>% 
  relocate(ba_nox_oz_input_rate_fossil, .after = ba_nox_input_rate_fossil) %>% 
  select(balance_authority_name, balance_authority_code, contains("rate"))  



### Non-baseload output emission rates (lb/MWh) -----

ba_nonbaseload_rates <- 
  plant %>% 
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(across(.cols = c("plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("mass")), 
                   .fns = ~ sum(. * nonbaseload_factor, na.rm = TRUE), 
                   .names = "{str_replace(.col, 'plant_', 'ba_')}")) %>% 
  mutate(across(.cols = c("ba_nox_mass",  
                          "ba_so2_mass", 
                          "ba_co2_mass", 
                          "ba_co2e_mass"), 
                .fns = ~ 2000 * . / ba_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
         ba_nox_oz_output_rate_nonbaseload = 2000 * ba_nox_oz_mass / ba_gen_oz, 
         across(.cols = c("ba_ch4_mass", 
                          "ba_n2o_mass"),
                .fns = ~ . / ba_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
         ba_hg_output_rate_nonbaseload = "--") %>% 
  relocate(ba_nox_oz_output_rate_nonbaseload, .after = ba_nox_output_rate_nonbaseload) %>% 
  select(balance_authority_name, balance_authority_code, contains("rate"))


## Calculate net generation and resource mix -----

### Generation by fuel category (MWh) -----

ba_gen <- 
  plant %>% 
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(across(.cols = c(contains("gen")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'ba_')}")) %>% 
  ungroup()

ba_gen_2 <- 
  ba_gen %>% 
  select(-ba_gen_ann, -ba_gen_oz) # remove duplicate columns for final formatting

### Resource mix by fuel category (%) -----

ba_resource_mix <- 
  ba_gen %>% 
  select(-ba_gen_oz) %>%  
  mutate(across(.cols = -c("balance_authority_name", "balance_authority_code", "ba_gen_ann"), 
                .fns = ~ . / ba_gen_ann * 100, # convert to percentage 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(balance_authority_name, balance_authority_code, contains("resource_mix"))


### Nonbaseload generation (MWh) -----

ba_nonbaseload_gen <- 
  plant %>% 
  group_by(balance_authority_name, balance_authority_code, primary_fuel_category) %>% 
  summarize(ba_nonbaseload_gen = sum(plant_gen_ann * nonbaseload_factor, na.rm = TRUE)) %>% 
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
                .fns = ~ . / ba_nonbaseload_gen * 100, 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(balance_authority_name, balance_authority_code, contains("resource_mix"))


# Create final data frame -----

## Join necessary data -----

ba_merged <- 
  ba %>% 
  left_join(ba_output_rates, by = c("balance_authority_name", "balance_authority_code")) %>% # output emission rates
  left_join(ba_input_rates, by = c("balance_authority_name", "balance_authority_code")) %>% # input emission rates
  left_join(ba_combustion_rates, by = c("balance_authority_name", "balance_authority_code")) %>% # combustion emission rates
  left_join(ba_fuel_rates, by = c("balance_authority_name", "balance_authority_code")) %>% # output and input emission rates by fuel type
  left_join(ba_fossil_rates, by = c("balance_authority_name", "balance_authority_code")) %>% # output and input emission rates for all fossil fuels
  left_join(ba_nonbaseload_rates, by = c("balance_authority_name", "balance_authority_code")) %>% # output emission rates for nonbaseload generation
  left_join(ba_gen_2, by = c("balance_authority_name", "balance_authority_code")) %>% # generation by fuel category
  left_join(ba_resource_mix, by = c("balance_authority_name", "balance_authority_code")) %>% # resource mix by fuel category
  left_join(ba_nonbaseload_gen, by = c("balance_authority_name", "balance_authority_code")) %>% # nonbaseload generation by fuel category
  left_join(ba_nonbaseload_resource_mix, by = c("balance_authority_name", "balance_authority_code")) %>% 
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
  relocate(ba_hg_output_rate_coal, .after = ba_n2o_output_rate_fossil) %>% 
  relocate(ba_hg_output_rate_fossil, .after = ba_hg_output_rate_coal) %>% 
  relocate(ba_nox_input_rate_fossil, .after = ba_nox_input_rate_gas) %>% 
  relocate(ba_nox_oz_input_rate_fossil, .after = ba_nox_oz_input_rate_gas) %>% 
  relocate(ba_so2_input_rate_fossil, .after = ba_so2_input_rate_gas) %>% 
  relocate(ba_co2_input_rate_fossil, .after = ba_co2_input_rate_gas) %>% 
  relocate(ba_co2e_input_rate_fossil, .after = ba_co2e_input_rate_gas) %>% 
  relocate(ba_ch4_input_rate_fossil, .after = ba_ch4_input_rate_gas) %>% 
  relocate(ba_n2o_input_rate_fossil, .after = ba_n2o_input_rate_gas) %>% 
  relocate(ba_hg_output_rate_coal, .after = ba_n2o_output_rate_fossil) %>% 
  relocate(ba_hg_input_rate_fossil, .after = ba_hg_input_rate_coal) 


# Export BA aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving BA aggregation file to folder data/outputs/")

write_rds(ba_formatted, "data/outputs/ba_aggregation.RDS")



