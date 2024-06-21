

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


# NERC level aggregation ------

# sum capacity, generation, emissions mass to NERC level 

nerc <- 
  plant %>% 
  group_by(year, nerc) %>% 
  summarize(across(.cols = c("plant_nameplate_capacity", 
                             contains("heat_input"), 
                             "plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("_mass")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'nerc_')}")) %>% 
  mutate(nerc_hg = "--") %>% 
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
           .fns = ~ 2000 * . / nerc_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    nerc_nox_oz_output_rate = 2000 * nerc_nox_oz_mass / nerc_gen_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    nerc_hg_output_rate = "--") %>% 
  relocate(nerc_nox_oz_output_rate, .after = nerc_nox_output_rate) %>% 
  select(nerc, contains("rate"))


### Input emission rates (lb/MMBtu) -----

nerc_input_rates <- 
  nerc %>% 
  mutate(# calculating input emission rates (lb/MMBtu)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    nerc_nox_oz_input_rate = 2000 * nerc_nox_oz_mass / nerc_heat_input_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    nerc_hg_input_rate = "--") %>% 
  relocate(nerc_nox_oz_input_rate, .after = nerc_nox_input_rate) %>% 
  select(nerc, contains("rate"))


### Combustion emission rates (lb/MWh) -----

nerc_combustion_rates <- 
  plant %>% 
  group_by(nerc) %>% 
  summarize(nerc_combustion_gen = sum(plant_combustion_gen, na.rm = TRUE)) %>% 
  left_join(nerc, by = c("nerc")) %>% 
  mutate(# calculating combustion emissions rates (lb/MWh)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_combustion_gen, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    nerc_nox_oz_combustion_rate = 2000 * nerc_nox_oz_mass / (nerc_combustion_gen * (nerc_gen_oz / nerc_gen_ann)), 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate")) %>% 
  relocate(nerc_nox_oz_combustion_rate, .after = nerc_nox_combustion_rate) %>% 
  select(nerc, contains("rate"))


### Output emission rates (lb/MWh) and input emission rates (lb/MMBtu) by fuel type  -----

# calculate emission rates by fossil fuel types 

fossil_fuels <- c("COAL", 
                  "OIL",
                  "GAS", 
                  "OSFL")

nerc_fuel_rates <-
  plant %>% 
  group_by(nerc, primary_fuel_category) %>% 
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
                   .names = "{str_replace(.col, 'plant_', 'nerc_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    nerc_nox_oz_output_rate = 2000 * nerc_nox_oz_mass / nerc_gen_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    nerc_nox_oz_input_rate = 2000 * nerc_nox_oz_mass / nerc_heat_input_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
  select(nerc, primary_fuel_category, contains("rate")) %>% 
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
  plant %>% 
  group_by(nerc) %>% 
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
                   .names = "{str_replace(.col, 'plant_', 'nerc_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
    nerc_nox_oz_output_rate_fossil = 2000 * nerc_nox_oz_mass / nerc_gen_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("nerc_nox_mass",  
                     "nerc_so2_mass", 
                     "nerc_co2_mass", 
                     "nerc_co2e_mass"), 
           .fns = ~ 2000 * . / nerc_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    nerc_nox_oz_input_rate_fossil = 2000 * nerc_nox_oz_mass / nerc_heat_input_oz, 
    across(.cols = c("nerc_ch4_mass", 
                     "nerc_n2o_mass"),
           .fns = ~ . / nerc_heat_input_ann, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    across(where(is.numeric), ~ replace_na(., 0))) %>% 
  relocate(nerc_nox_oz_input_rate_fossil, .after = nerc_nox_input_rate_fossil) %>% 
  select(nerc, contains("rate"))  



### Non-baseload output emission rates (lb/MWh) -----

nerc_nonbaseload_rates <- 
  plant %>% 
  group_by(nerc) %>% 
  summarize(across(.cols = c("plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("mass")), 
                   .fns = ~ sum(. * nonbaseload_factor, na.rm = TRUE), 
                   .names = "{str_replace(.col, 'plant_', 'nerc_')}")) %>% 
  mutate(across(.cols = c("nerc_nox_mass",  
                          "nerc_so2_mass", 
                          "nerc_co2_mass", 
                          "nerc_co2e_mass"), 
                .fns = ~ 2000 * . / nerc_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
         nerc_nox_oz_output_rate_nonbaseload = 2000 * nerc_nox_oz_mass / nerc_gen_oz, 
         across(.cols = c("nerc_ch4_mass", 
                          "nerc_n2o_mass"),
                .fns = ~ . / nerc_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
         nerc_hg_output_rate_nonbaseload = "--") %>% 
  relocate(nerc_nox_oz_output_rate_nonbaseload, .after = nerc_nox_output_rate_nonbaseload) %>% 
  select(nerc, contains("rate"))


## Calculate net generation and resource mix -----

### Generation by fuel category (MWh) -----

nerc_gen <- 
  plant %>% 
  group_by(nerc) %>% 
  summarize(across(.cols = c(contains("gen")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'nerc_')}")) 

nerc_gen_2 <- 
  nerc_gen %>% 
  select(-nerc_gen_ann, -nerc_gen_oz) # remove duplicate columns for final formatting

### Resource mix by fuel category (%) -----

nerc_resource_mix <- 
  nerc_gen %>% 
  select(-nerc_gen_oz) %>%   
  mutate(across(.cols = -c("nerc", "nerc_gen_ann"), 
                .fns = ~ . / nerc_gen_ann * 100, # convert to percentage 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(nerc, contains("resource_mix"))


### Nonbaseload generation (MWh) -----

nerc_nonbaseload_gen <- 
  plant %>% 
  group_by(nerc, primary_fuel_category) %>% 
  summarize(nerc_nonbaseload_gen = sum(plant_gen_ann * nonbaseload_factor, na.rm = TRUE)) %>% 
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
                .fns = ~ . / nerc_nonbaseload_gen * 100, 
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
nerc_rounded <- 
  nerc_merged %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals


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
  relocate(nerc_hg_output_rate_coal, .after = nerc_n2o_output_rate_fossil) %>% 
  relocate(nerc_hg_output_rate_fossil, .after = nerc_hg_output_rate_coal) %>% 
  relocate(nerc_nox_input_rate_fossil, .after = nerc_nox_input_rate_gas) %>% 
  relocate(nerc_nox_oz_input_rate_fossil, .after = nerc_nox_oz_input_rate_gas) %>% 
  relocate(nerc_so2_input_rate_fossil, .after = nerc_so2_input_rate_gas) %>% 
  relocate(nerc_co2_input_rate_fossil, .after = nerc_co2_input_rate_gas) %>% 
  relocate(nerc_co2e_input_rate_fossil, .after = nerc_co2e_input_rate_gas) %>% 
  relocate(nerc_ch4_input_rate_fossil, .after = nerc_ch4_input_rate_gas) %>% 
  relocate(nerc_n2o_input_rate_fossil, .after = nerc_n2o_input_rate_gas) %>% 
  relocate(nerc_hg_output_rate_coal, .after = nerc_n2o_output_rate_fossil) %>% 
  relocate(nerc_hg_input_rate_fossil, .after = nerc_hg_input_rate_coal) 


# Export NERC aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving NERC aggregation file to folder data/outputs/")

write_rds(nerc_formatted, "data/outputs/nerc_aggregation.RDS")




