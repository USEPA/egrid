

# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)


# Load and clean necessary data ------

# rename columns 

columns_to_keep <- 
  # c("year", 
  #   "state" = "pstatabb", 
  #   "state_fips_code" = "fipsst", 
  #   "plant_name", 
  #   "plant_id", 
  #   "nerc", 
  #   "sub_region", 
  #   "sub_region_name" = "srname",
  #   "balance_authority_name", 
  #   "balance_authority_code",
  #   "primary_fuel_type" = "plprmfl", 
  #   "primary_fuel_category" = "plfuelct", 
  #   "plant_nameplate_capacity" = "namepcap", 
  #   "capacity_factor" = "capfac", 
  #   "nonbaseload_factor" = "nbfactor", 
  #   "plant_heat_input_comb" = "plhtian", 
  #   "plant_heat_input_comb_oz" = "plhtioz", 
  #   "plant_heat_input_ann" = "plhtiant", 
  #   "plant_heat_input_oz" = "plhtiozt", 
  #   "plant_gen_ann" = "plngenan", 
  #   "plant_gen_oz" = "plngenoz", 
  #   "plant_nox_mass" = "plnoxan", 
  #   "plant_nox_oz_mass" = "plnoxoz", 
  #   "plant_so2_mass" = "plso2an", 
  #   "plant_co2_mass" = "plco2an", 
  #   "plant_ch4_mass" = "plch4an", 
  #   "plant_n2o_mass" = "pln2oan", 
  #   "plant_co2e_mass" = "plco2eqa",
  #   "plant_hg_mass" = "plhgan", 
  #   "plant_coal_gen" = "plgenacl", 
  #   "plant_oil_gen" = "plgenaol", 
  #   "plant_gas_gen" = "plgenags", 
  #   "plant_nuclear_gen" = "plgenanc", 
  #   "plant_hydro_gen" = "plgenahy",
  #   "plant_biomass_gen" = "plgenabm", 
  #   "plant_wind_gen" = "plgenawi", 
  #   "plant_solar_gen" = "plgenaso", 
  #   "plant_geothermal_gen" = "plgenagt", 
  #   "plant_other_fossil_gen" = "plgenaof", 
  #   "plant_other_purchased_gen" = "plgenaop", 
  #   "plant_nonre_gen" = "plgenatn", 
  #   "plant_re_gen" = "plgenatr", 
  #   "plant_re_nonhydro_gen" = "plgenath", 
  #   "plant_combustion_gen" = "plgenacy", 
  #   "plant_noncombustion_gen" = "plgenacn"
  # )  
  c("year", 
    #"state" = "pstatabb",
    "state" = "plant_state",
    "state_fips_code" = "FIPS_state",
    "plant_name",
    "plant_id",
    "nerc",
    "sub_region" = "egrid_subregion",
    "sub_region_name" = "egrid_subregion_name",
    "ba_name",
    "ba_id",
    "primary_fuel_type" = "primary_fuel_type",
    "primary_fuel_category",
    "plant_nameplate_capacity" = "nameplate_capacity",
    "capacity_factor" = "capfac",
    "nonbaseload_factor" = "nonbaseload",
    "plant_heat_input_comb" = "combust_heat_input",
    "plant_heat_input_comb_oz" = "combust_heat_input_oz",
    "plant_heat_input_ann" = "heat_input",
    "plant_heat_input_oz" = "heat_input_oz",
    "plant_gen_ann" = "generation_ann",
    "plant_gen_oz" = "generation_oz",
    "plant_nox_mass" = "nox_mass",
    "plant_nox_oz_mass" = "nox_oz_mass",
    "plant_so2_mass" = "so2_mass",
    "plant_co2_mass" = "co2_mass",
    "plant_ch4_mass" = "ch4_mass",
    "plant_n2o_mass" = "n2o_mass",
    "plant_co2e_mass" = "co2e_mass",
    "plant_hg_mass" = "hg_mass",
    "plant_coal_gen" = "ann_gen_coal",
    "plant_oil_gen" = "ann_gen_oil",
    "plant_gas_gen" = "ann_gen_gas",
    "plant_nuclear_gen" = "ann_gen_nuclear",
    "plant_hydro_gen" = "ann_gen_hydro",
    "plant_biomass_gen" = "ann_gen_biomass",
    "plant_wind_gen" = "ann_gen_wind",
    "plant_solar_gen" = "ann_gen_solar",
    "plant_geothermal_gen" = "ann_gen_geothermal",
    "plant_other_fossil_gen" = "ann_gen_other_ff",
    "plant_other_purchased_gen" = "ann_gen_other",
    "plant_nonre_gen" = "ann_gen_non_renew",
    "plant_re_gen" = "ann_gen_renew",
    "plant_re_nonhydro_gen" = "ann_gen_renew_nonhydro",
    "plant_combustion_gen" = "ann_gen_combust",
    "plant_noncombustion_gen" = "ann_gen_noncombust"
    )


# read in plant file 

plant <- 
  read_rds("data/outputs/plant_file.RDS") %>% 
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


# eGRID subregion level aggregation ------

# sum capacity, generation, emissions mass to eGRID sub_region level 

egrid <- 
  plant %>% 
  group_by(year, sub_region, sub_region_name) %>% 
  summarize(across(.cols = c("plant_nameplate_capacity", 
                             contains("heat_input"), 
                             "plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("_mass")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'egrid_')}")) %>% 
  mutate(egrid_hg_mass = "--") %>% 
  ungroup()


## Calculate emission rates ------

### Output emission rates (lb/MWh) -----

egrid_output_rates <- 
  egrid %>% 
  mutate(# calculating output emissions rates (lb/MWh)
    across(.cols = c("egrid_nox_mass",  
                     "egrid_so2_mass", 
                     "egrid_co2_mass", 
                     "egrid_co2e_mass"), 
           .fns = ~ 2000 * . / egrid_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    egrid_nox_oz_output_rate = 2000 * egrid_nox_oz_mass / egrid_gen_oz, 
    across(.cols = c("egrid_ch4_mass", 
                     "egrid_n2o_mass"),
           .fns = ~ . / egrid_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    egrid_hg_output_rate = "--") %>% 
  relocate(egrid_nox_oz_output_rate, .after = egrid_nox_output_rate) %>%  
  relocate(egrid_co2e_output_rate, .after = egrid_n2o_output_rate) %>% 
  select(sub_region, contains("rate"))


### Input emission rates (lb/MMBtu) -----

egrid_input_rates <- 
  egrid %>% 
  mutate(# calculating input emission rates (lb/MMBtu)
    across(.cols = c("egrid_nox_mass",  
                     "egrid_so2_mass", 
                     "egrid_co2_mass", 
                     "egrid_co2e_mass"), 
           .fns = ~ 2000 * . / egrid_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    egrid_nox_oz_input_rate = 2000 * egrid_nox_oz_mass / egrid_heat_input_comb_oz, 
    across(.cols = c("egrid_ch4_mass", 
                     "egrid_n2o_mass"),
           .fns = ~ . / egrid_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    egrid_hg_input_rate = "--") %>% 
  relocate(egrid_nox_oz_input_rate, .after = egrid_nox_input_rate) %>%  
  relocate(egrid_co2e_input_rate, .after = egrid_n2o_input_rate) %>% 
  select(sub_region, contains("rate"))


### Combustion emission rates (lb/MWh) -----

egrid_combustion_rates <- 
  plant %>% 
  group_by(sub_region) %>% 
  summarize(egrid_combustion_gen = sum(plant_combustion_gen, na.rm = TRUE)) %>% 
  left_join(egrid, by = c("sub_region")) %>% 
  mutate(# calculating combustion emissions rates (lb/MWh)
    across(.cols = c("egrid_nox_mass",  
                     "egrid_so2_mass", 
                     "egrid_co2_mass", 
                     "egrid_co2e_mass"), 
           .fns = ~ 2000 * . / egrid_combustion_gen, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    egrid_nox_oz_combustion_rate = 2000 * egrid_nox_oz_mass / (egrid_combustion_gen * (egrid_gen_oz / egrid_gen_ann)), 
    across(.cols = c("egrid_ch4_mass", 
                     "egrid_n2o_mass"),
           .fns = ~ . / egrid_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
    egrid_hg_combustion_rate = "--") %>% 
  relocate(egrid_nox_oz_combustion_rate, .after = egrid_nox_combustion_rate) %>%  
  relocate(egrid_co2e_combustion_rate, .after = egrid_n2o_combustion_rate) %>% 
  select(sub_region, contains("rate"))


### Fuel type output emission rates (lb/MWh) and input emission rates (lb/MMBtu)  -----

# calculate emission rates by fossil fuel types 

fossil_fuels <- c("COAL", 
                  "OIL",
                  "GAS", 
                  "OSFL")

egrid_fuel_rates <-
  plant %>% 
  group_by(sub_region, primary_fuel_category) %>% 
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
                   .names = "{str_replace(.col, 'plant_', 'egrid_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("egrid_nox_mass",  
                     "egrid_so2_mass", 
                     "egrid_co2_mass", 
                     "egrid_co2e_mass"), 
           .fns = ~ 2000 * . / egrid_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
    egrid_nox_oz_output_rate = 2000 * egrid_nox_oz_mass / egrid_gen_oz, 
    across(.cols = c("egrid_ch4_mass", 
                     "egrid_n2o_mass"),
           .fns = ~ . / egrid_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("egrid_nox_mass",  
                     "egrid_so2_mass", 
                     "egrid_co2_mass", 
                     "egrid_co2e_mass"), 
           .fns = ~ 2000 * . / egrid_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
    egrid_nox_oz_input_rate = 2000 * egrid_nox_oz_mass / egrid_heat_input_comb_oz, 
    across(.cols = c("egrid_ch4_mass", 
                     "egrid_n2o_mass"),
           .fns = ~ . / egrid_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
  select(sub_region, primary_fuel_category, contains("rate")) %>% 
  relocate(egrid_nox_oz_output_rate, .after = egrid_nox_output_rate) %>% 
  relocate(egrid_co2e_output_rate, .after = egrid_n2o_output_rate) %>% 
  relocate(egrid_nox_oz_input_rate, .after = egrid_nox_input_rate) %>% 
  relocate(egrid_co2e_input_rate, .after = egrid_n2o_input_rate) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = contains("rate")) %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0)), 
         egrid_hg_output_rate_coal = "--", 
         egrid_hg_output_rate_fossil = "--", 
         egrid_hg_input_rate_coal = "--", 
         egrid_hg_input_rate_fossil = "--")


# calculate all fossil fuel output and input emission rates 

egrid_fossil_rates <-
  plant %>% 
  group_by(sub_region) %>% 
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
                   .names = "{str_replace(.col, 'plant_', 'egrid_')}")) %>% 
  mutate(# output emission rates (lb/MWh)
    across(.cols = c("egrid_nox_mass",  
                     "egrid_so2_mass", 
                     "egrid_co2_mass", 
                     "egrid_co2e_mass"), 
           .fns = ~ 2000 * . / egrid_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
    egrid_nox_oz_output_rate_fossil = 2000 * egrid_nox_oz_mass / egrid_gen_oz, 
    across(.cols = c("egrid_ch4_mass", 
                     "egrid_n2o_mass"),
           .fns = ~ . / egrid_gen_ann, 
           .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
    
    # input emission rates (lb/MMBtu)
    across(.cols = c("egrid_nox_mass",  
                     "egrid_so2_mass", 
                     "egrid_co2_mass", 
                     "egrid_co2e_mass"), 
           .fns = ~ 2000 * . / egrid_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    egrid_nox_oz_input_rate_fossil = 2000 * egrid_nox_oz_mass / egrid_heat_input_comb_oz, 
    across(.cols = c("egrid_ch4_mass", 
                     "egrid_n2o_mass"),
           .fns = ~ . / egrid_heat_input_comb, 
           .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
    across(where(is.numeric), ~ replace_na(., 0))) %>% 
  relocate(egrid_co2e_input_rate_fossil, .after = egrid_n2o_input_rate_fossil) %>%  
  relocate(egrid_nox_oz_input_rate_fossil, .after = egrid_nox_input_rate_fossil) %>% 
  relocate(egrid_nox_oz_output_rate_fossil, .after = egrid_nox_output_rate_fossil) %>% 
  relocate(egrid_co2e_output_rate_fossil, .after = egrid_n2o_output_rate_fossil) %>% 
  select(sub_region, contains("rate"))  



### Non-baseload output emission rates (lb/MWh) -----

egrid_nonbaseload_rates <- 
  plant %>% 
  group_by(sub_region) %>% 
  summarize(across(.cols = c("plant_gen_ann", 
                             "plant_gen_oz", 
                             contains("mass")), 
                   .fns = ~ sum(. * nonbaseload_factor, na.rm = TRUE), 
                   .names = "{str_replace(.col, 'plant_', 'egrid_')}")) %>% 
  mutate(across(.cols = c("egrid_nox_mass",  
                          "egrid_so2_mass", 
                          "egrid_co2_mass", 
                          "egrid_co2e_mass"), 
                .fns = ~ 2000 * . / egrid_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
         egrid_nox_oz_output_rate_nonbaseload = 2000 * egrid_nox_oz_mass / egrid_gen_oz, 
         across(.cols = c("egrid_ch4_mass", 
                          "egrid_n2o_mass"),
                .fns = ~ . / egrid_gen_ann, 
                .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
         egrid_hg_output_rate_nonbaseload = "--") %>% 
  relocate(egrid_nox_oz_output_rate_nonbaseload, .after = egrid_nox_output_rate_nonbaseload) %>% 
  relocate(egrid_co2e_output_rate_nonbaseload, .after = egrid_n2o_output_rate_nonbaseload) %>%
  select(sub_region, contains("rate"))


## Calculate net generation and resource mix -----

### Generation by fuel category (MWh) -----

egrid_gen <- 
  plant %>% 
  group_by(sub_region) %>% 
  summarize(across(.cols = c(contains("gen")), 
                   .fns = ~ sum(.x, na.rm = TRUE),
                   .names = "{str_replace(.col, 'plant_', 'egrid_')}")) 

egrid_gen_2 <- 
  egrid_gen %>% 
  select(-egrid_gen_ann, -egrid_gen_oz) # remove duplicate columns for final formatting

### Resource mix by fuel category (%) -----

egrid_resource_mix <- 
  egrid_gen %>% 
  select(-egrid_gen_oz) %>%   
  mutate(across(.cols = -c("sub_region", "egrid_gen_ann"), 
                .fns = ~ . / egrid_gen_ann, # convert to percentage 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(sub_region, contains("resource_mix"))


### Nonbaseload generation (MWh) -----

egrid_nonbaseload_gen <- 
  plant %>% 
  group_by(sub_region, primary_fuel_category) %>% 
  summarize(egrid_nonbaseload_gen = sum(plant_gen_ann * nonbaseload_factor, na.rm = TRUE)) %>% 
  arrange(primary_fuel_category) %>% 
  pivot_wider(names_from = primary_fuel_category, 
              values_from = egrid_nonbaseload_gen, 
              names_prefix = "egrid_nonbaseload_gen_") %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) 


### Nonbaseload resource mix (%) -----

egrid_nonbaseload_resource_mix <- 
  egrid_nonbaseload_gen %>% 
  mutate(egrid_nonbaseload_gen = rowSums(pick(contains("nonbaseload"))), 
         across(.cols = -c("egrid_nonbaseload_gen"), 
                .fns = ~ . / egrid_nonbaseload_gen, 
                .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
  select(sub_region, contains("resource_mix"))


# Create final data frame -----

## Join necessary data -----

egrid_merged <- 
  egrid %>% 
  left_join(egrid_output_rates, by = c("sub_region")) %>% # output emission rates
  left_join(egrid_input_rates, by = c("sub_region")) %>% # input emission rates
  left_join(egrid_combustion_rates, by = c("sub_region")) %>% # combustion emission rates
  left_join(egrid_fuel_rates, by = c("sub_region")) %>% # output and input emission rates by fuel type
  left_join(egrid_fossil_rates, by = c("sub_region")) %>% # output and input emission rates for all fossil fuels
  left_join(egrid_nonbaseload_rates, by = c("sub_region")) %>% # output emission rates for nonbaseload generation
  left_join(egrid_gen_2, by = c("sub_region")) %>% # generation by fuel category
  left_join(egrid_resource_mix, by = c("sub_region")) %>% # resource mix by fuel category
  left_join(egrid_nonbaseload_gen, by = c("sub_region")) %>% # nonbaseload generation by fuel category
  left_join(egrid_nonbaseload_resource_mix, by = c("sub_region")) %>% 
  mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
         across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
  select(-contains("gen_na"),
         -contains("mix_na")) # remove unnecessary columns


## Round data ----
egrid_rounded <- 
  egrid_merged %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals


## Format to eGRID output -------

egrid_formatted <- 
  egrid_rounded %>% 
  relocate(egrid_nox_output_rate_fossil, .after = egrid_nox_output_rate_gas) %>% 
  relocate(egrid_nox_oz_output_rate_fossil, .after = egrid_nox_oz_output_rate_gas) %>% 
  relocate(egrid_so2_output_rate_fossil, .after = egrid_so2_output_rate_gas) %>% 
  relocate(egrid_co2_output_rate_fossil, .after = egrid_co2_output_rate_gas) %>% 
  relocate(egrid_co2e_output_rate_fossil, .after = egrid_co2e_output_rate_gas) %>% 
  relocate(egrid_ch4_output_rate_fossil, .after = egrid_ch4_output_rate_gas) %>% 
  relocate(egrid_n2o_output_rate_fossil, .after = egrid_n2o_output_rate_gas) %>% 
  relocate(egrid_hg_output_rate_coal, .after = egrid_co2e_output_rate_fossil) %>% 
  relocate(egrid_hg_output_rate_fossil, .after = egrid_hg_output_rate_coal) %>% 
  relocate(egrid_nox_input_rate_fossil, .after = egrid_nox_input_rate_gas) %>% 
  relocate(egrid_nox_oz_input_rate_fossil, .after = egrid_nox_oz_input_rate_gas) %>% 
  relocate(egrid_so2_input_rate_fossil, .after = egrid_so2_input_rate_gas) %>% 
  relocate(egrid_co2_input_rate_fossil, .after = egrid_co2_input_rate_gas) %>% 
  relocate(egrid_co2e_input_rate_fossil, .after = egrid_co2e_input_rate_gas) %>% 
  relocate(egrid_ch4_input_rate_fossil, .after = egrid_ch4_input_rate_gas) %>% 
  relocate(egrid_n2o_input_rate_fossil, .after = egrid_n2o_input_rate_gas) %>% 
  relocate(egrid_hg_output_rate_coal, .after = egrid_co2e_output_rate_fossil) %>% 
  relocate(egrid_hg_input_rate_fossil, .after = egrid_hg_input_rate_coal) 


# Export eGRID subregion aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving eGRID subregion aggregation file to folder data/outputs/")

write_rds(egrid_formatted, "data/outputs/egrid_subregion_aggregation.RDS")


