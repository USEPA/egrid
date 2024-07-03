

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


# US level aggregation ------

# sum capacity, generation, emissions mass to US level 

us <- 
  plant %>% 
  group_by(year) %>% 
  summarize(across(.cols = c("plant_nameplate_capacity", 
                             contains("heat_input"), 
                             "plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("_mass")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'us_')}")) %>% 
  mutate(us_hg_mass = "--") %>% 
  ungroup()


## Calculate emission rates ------

### Output emission rates (lb/MWh) -----

us_output_rates <- 
  us %>% 
  mutate(# calculating output emissions rates (lb/MWh)
    across(.cols = c("us_nox_mass",  
                     "us_so2_mass", 
                     "us_co2_mass", 
                     "us_co2e_mass"), 
           .fns = ~ 2000 * . / us_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    us_nox_oz_output_rate = 2000 * us_nox_oz_mass / us_gen_oz, 
    across(.cols = c("us_ch4_mass", 
                     "us_n2o_mass"),
           .fns = ~ . / us_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    us_hg_output_rate = "--") %>% 
  relocate(us_nox_oz_output_rate, .after = us_nox_output_rate) %>%  
  relocate(us_co2e_output_rate, .after = us_n2o_output_rate) %>% 
  select(year, contains("rate"))


### Input emission rates (lb/MMBtu) -----

us_input_rates <- 
  us %>% 
  mutate(# calculating input emission rates (lb/MMBtu)
    across(.cols = c("us_nox_mass",  
                     "us_so2_mass", 
                     "us_co2_mass", 
                     "us_co2e_mass"), 
           .fns = ~ 2000 * . / us_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    us_nox_oz_input_rate = 2000 * us_nox_oz_mass / us_heat_input_comb_oz, 
    across(.cols = c("us_ch4_mass", 
                     "us_n2o_mass"),
           .fns = ~ . / us_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    us_hg_input_rate = "--") %>% 
  relocate(us_nox_oz_input_rate, .after = us_nox_input_rate) %>% 
  relocate(us_co2e_input_rate, .after = us_n2o_input_rate) %>% 
  select(year, contains("rate"))


### Combustion emission rates (lb/MWh) -----

us_combustion_rates <- 
  plant %>% 
  group_by(year) %>% 
  summarize(us_combustion_gen = sum(plant_combustion_gen, na.rm = TRUE)) %>% 
  left_join(us, by = c("year")) %>% 
  mutate(# calculating combustion emissions rates (lb/MWh)
    across(.cols = c("us_nox_mass",  
                     "us_so2_mass", 
                     "us_co2_mass", 
                     "us_co2e_mass"), 
           .fns = ~ 2000 * . / us_combustion_gen, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    us_nox_oz_combustion_rate = 2000 * us_nox_oz_mass / (us_combustion_gen * (us_gen_oz / us_gen_ann)), 
    across(.cols = c("us_ch4_mass", 
                     "us_n2o_mass"),
           .fns = ~ . / us_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    us_hg_combustion_rate = "--") %>% 
  relocate(us_nox_oz_combustion_rate, .after = us_nox_combustion_rate) %>%  
  relocate(us_co2e_combustion_rate, .after = us_n2o_combustion_rate) %>% 
  select(year, contains("rate"))


### Fuel type output emission rates (lb/MWh) and input emission rates (lb/MMBtu) -----

# calculate emission rates by fossil fuel types 

fossil_fuels <- c("COAL", 
                  "OIL",
                  "GAS", 
                  "OSFL")

us_fuel_rates <-
  plant %>% 
  group_by(year, primary_fuel_category) %>% 
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
                   .names = "{str_replace(.col, 'plant_', 'us_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("us_nox_mass",  
                     "us_so2_mass", 
                     "us_co2_mass", 
                     "us_co2e_mass"), 
           .fns = ~ 2000 * . / us_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    us_nox_oz_output_rate = 2000 * us_nox_oz_mass / us_gen_oz, 
    across(.cols = c("us_ch4_mass", 
                     "us_n2o_mass"),
           .fns = ~ . / us_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("us_nox_mass",  
                     "us_so2_mass", 
                     "us_co2_mass", 
                     "us_co2e_mass"), 
           .fns = ~ 2000 * . / us_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    us_nox_oz_input_rate = 2000 * us_nox_oz_mass / us_heat_input_comb_oz, 
    across(.cols = c("us_ch4_mass", 
                     "us_n2o_mass"),
           .fns = ~ . / us_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
  select(year, primary_fuel_category, contains("rate")) %>% 
  relocate(us_nox_oz_output_rate, .after = us_nox_output_rate) %>% 
  relocate(us_co2e_output_rate, .after = us_n2o_output_rate) %>% 
  relocate(us_nox_oz_input_rate, .after = us_nox_input_rate) %>% 
  relocate(us_co2e_input_rate, .after = us_n2o_input_rate) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = contains("rate")) %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0)), 
         us_hg_output_rate_coal = "--", 
         us_hg_output_rate_fossil = "--", 
         us_hg_input_rate_coal = "--", 
         us_hg_input_rate_fossil = "--")


# calculate all fossil fuel output and input emission rates 

us_fossil_rates <-
  plant %>% 
  group_by(year) %>% 
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
                   .names = "{str_replace(.col, 'plant_', 'us_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("us_nox_mass",  
                     "us_so2_mass", 
                     "us_co2_mass", 
                     "us_co2e_mass"), 
           .fns = ~ 2000 * . / us_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
    us_nox_oz_output_rate_fossil = 2000 * us_nox_oz_mass / us_gen_oz, 
    across(.cols = c("us_ch4_mass", 
                     "us_n2o_mass"),
           .fns = ~ . / us_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("us_nox_mass",  
                     "us_so2_mass", 
                     "us_co2_mass", 
                     "us_co2e_mass"), 
           .fns = ~ 2000 * . / us_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    us_nox_oz_input_rate_fossil = 2000 * us_nox_oz_mass / us_heat_input_comb_oz, 
    across(.cols = c("us_ch4_mass", 
                     "us_n2o_mass"),
           .fns = ~ . / us_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    across(where(is.numeric), ~ replace_na(., 0))) %>% 
  relocate(us_nox_oz_input_rate_fossil, .after = us_nox_input_rate_fossil) %>% 
  select(year, contains("rate"))  



### Non-baseload output emission rates (lb/MWh) -----

us_nonbaseload_rates <- 
  plant %>% 
  group_by(year) %>% 
  summarize(across(.cols = c("plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("mass")), 
                   .fns = ~ sum(. * nonbaseload_factor, na.rm = TRUE), 
                   .names = "{str_replace(.col, 'plant_', 'us_')}")) %>% 
  mutate(across(.cols = c("us_nox_mass",  
                          "us_so2_mass", 
                          "us_co2_mass", 
                          "us_co2e_mass"), 
                .fns = ~ 2000 * . / us_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
         us_nox_oz_output_rate_nonbaseload = 2000 * us_nox_oz_mass / us_gen_oz, 
         across(.cols = c("us_ch4_mass", 
                          "us_n2o_mass"),
                .fns = ~ . / us_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
         us_hg_output_rate_nonbaseload = "--") %>% 
  relocate(us_nox_oz_output_rate_nonbaseload, .after = us_nox_output_rate_nonbaseload) %>% 
  relocate(us_co2e_output_rate_nonbaseload, .after = us_n2o_output_rate_nonbaseload) %>% 
  select(year, contains("rate"))


## Calculate net generation and resource mix -----

### Generation by fuel category (MWh) -----

us_gen <- 
  plant %>% 
  group_by(year) %>% 
  summarize(across(.cols = c(contains("gen")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'us_')}")) 

us_gen_2 <- 
  us_gen %>% 
  select(-us_gen_ann, -us_gen_oz) # remove duplicate columns for final formatting

### Resource mix by fuel category (%) -----

us_resource_mix <- 
  us_gen %>% 
  select(-us_gen_oz) %>%   
  mutate(across(.cols = -c("year", "us_gen_ann"), 
                .fns = ~ . / us_gen_ann, # convert to percentage 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(year, contains("resource_mix"))


### Nonbaseload generation (MWh) -----

us_nonbaseload_gen <- 
  plant %>% 
  group_by(year, primary_fuel_category) %>% 
  summarize(us_nonbaseload_gen = sum(plant_gen_ann * nonbaseload_factor, na.rm = TRUE)) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = us_nonbaseload_gen, 
              names_prefix = "us_nonbaseload_gen_") %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) 


### Nonbaseload resource mix (%) -----

us_nonbaseload_resource_mix <- 
  us_nonbaseload_gen %>% 
  mutate(us_nonbaseload_gen = rowSums(pick(contains("nonbaseload"))), 
         across(.cols = -c("us_nonbaseload_gen"), 
                .fns = ~ . / us_nonbaseload_gen, 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(year, contains("resource_mix"))


# Create final data frame -----

## Join necessary data -----

us_merged <- 
  us %>% 
  left_join(us_output_rates, by = c("year")) %>% # output emission rates
  left_join(us_input_rates, by = c("year")) %>% # input emission rates
  left_join(us_combustion_rates, by = c("year")) %>% # combustion emission rates
  left_join(us_fuel_rates, by = c("year")) %>% # output and input emission rates by fuel type
  left_join(us_fossil_rates, by = c("year")) %>% # output and input emission rates for all fossil fuels
  left_join(us_nonbaseload_rates, by = c("year")) %>% # output emission rates for nonbaseload generation
  left_join(us_gen_2, by = c("year")) %>% # generation by fuel category
  left_join(us_resource_mix, by = c("year")) %>% # resource mix by fuel category
  left_join(us_nonbaseload_gen, by = c("year")) %>% # nonbaseload generation by fuel category
  left_join(us_nonbaseload_resource_mix, by = c("year")) %>% 
  mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
         across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
  select(-contains("gen_na"),
         -contains("mix_na")) # remove unnecessary columns


## Round data ----
us_rounded <- 
  us_merged %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals


## Format to eGRID output -------

us_formatted <- 
  us_rounded %>% 
  relocate(us_nox_output_rate_fossil, .after = us_nox_output_rate_gas) %>% 
  relocate(us_nox_oz_output_rate_fossil, .after = us_nox_oz_output_rate_gas) %>% 
  relocate(us_so2_output_rate_fossil, .after = us_so2_output_rate_gas) %>% 
  relocate(us_co2_output_rate_fossil, .after = us_co2_output_rate_gas) %>% 
  relocate(us_co2e_output_rate_fossil, .after = us_co2e_output_rate_gas) %>% 
  relocate(us_ch4_output_rate_fossil, .after = us_ch4_output_rate_gas) %>% 
  relocate(us_n2o_output_rate_fossil, .after = us_n2o_output_rate_gas) %>% 
  relocate(us_hg_output_rate_coal, .after = us_co2e_output_rate_fossil) %>% 
  relocate(us_hg_output_rate_fossil, .after = us_hg_output_rate_coal) %>% 
  relocate(us_nox_input_rate_fossil, .after = us_nox_input_rate_gas) %>% 
  relocate(us_nox_oz_input_rate_fossil, .after = us_nox_oz_input_rate_gas) %>% 
  relocate(us_so2_input_rate_fossil, .after = us_so2_input_rate_gas) %>% 
  relocate(us_co2_input_rate_fossil, .after = us_co2_input_rate_gas) %>% 
  relocate(us_co2e_input_rate_fossil, .after = us_co2e_input_rate_gas) %>% 
  relocate(us_ch4_input_rate_fossil, .after = us_ch4_input_rate_gas) %>% 
  relocate(us_n2o_input_rate_fossil, .after = us_n2o_input_rate_gas) %>% 
  relocate(us_hg_output_rate_coal, .after = us_co2e_output_rate_fossil) %>% 
  relocate(us_hg_input_rate_fossil, .after = us_hg_input_rate_coal) 


# Export US aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving US aggregation file to folder data/outputs/")

write_rds(us_formatted, "data/outputs/us_aggregation.RDS")




