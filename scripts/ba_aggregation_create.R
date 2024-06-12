

# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)


### notes: 
###     overall: will need to coordinate with plant file output once complete
###     CH4, N2O, CO2e emissions to be added 
###     non-baseload % to be added 
###     energy source from plant file to be added
###     need to distinguish between "other fossil" and "other unknown/purchased" groups


# Load and clean necessary data ------

# fuel type / energy source crosswalk 
# this will be unnecessary once plant file is ready

xwalk_energy_source <- read_csv("data/static_tables/xwalk_energy_source.csv")


# unit file 

unit <- read_rds("data/outputs/unit_file_2021.RDS") # need to generalize for any year

# calculate plant level emissions & heat input

plant_emissions_heat_rate <- 
  unit %>% 
  mutate(plant_id = as.character(plant_id)) %>% 
  group_by(plant_name, plant_id, primary_fuel_type) %>% 
  summarize(plant_nox = sum(nox_mass), 
            plant_nox_oz = sum(nox_oz), 
            plant_so2 = sum(so2_mass), 
            plant_co2 = sum(co2_mass),
            plant_hg = sum(hg_mass),
            plant_heat_input = sum(heat_input), 
            plant_heat_input_oz = sum(heat_input_oz))

# generator file 

generator <- read_rds("data/outputs/generator_file.RDS") # need to generalize for any year

# calculate plant level net generation by prime mover and primary fuel type

plant_generation <-
  generator %>% 
  group_by(plant_id) %>% 
  summarize(plant_gen_ann = sum(generation_ann),
            plant_gen_oz = sum(generation_oz),
            plant_nameplate_capacity = sum(nameplate_capacity))

# plant file 

plant <- read_rds("data/outputs/plant_file_2021.RDS") # need to generalize for any year

plant <- 
  plant %>% 
  mutate(plant_id = as.character(plant_id))

# Combine data ------

plant_combined <- 
  plant %>% 
  left_join(plant_emissions_heat_rate, by = c("plant_id", "plant_name")) %>% 
  left_join(plant_generation, by = "plant_id") %>% 
  left_join(xwalk_energy_source, by = "primary_fuel_type") 

# BA level aggregation ------

ba <- 
  plant_combined %>% 
  group_by(year, balance_authority_name, balance_authority_code) %>% 
  summarize(ba_nameplate_capacity = sum(plant_nameplate_capacity, na.rm = TRUE), 
            ba_nox = sum(plant_nox, na.rm = TRUE), 
            ba_nox_oz = sum(plant_nox_oz, na.rm = TRUE), 
            ba_so2 = sum(plant_so2, na.rm = TRUE), 
            ba_co2 = sum(plant_co2, na.rm = TRUE), 
            ba_hg = sum(plant_hg, na.rm = TRUE), 
            ba_gen_ann = sum(plant_gen_ann, na.rm = TRUE), 
            ba_gen_oz = sum(plant_gen_oz, na.rm = TRUE), 
            ba_heat_input = sum(plant_heat_input, na.rm = TRUE), 
            ba_heat_input_oz = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  ungroup()

### Output and input emission rates -----

ba_emission_rates <- 
  ba %>% 
  mutate(ba_output_nox_rate = 2000*ba_nox/ba_gen_ann, # output emissions rates (lb/MWh)
         ba_output_nox_oz_rate = 2000*ba_nox_oz/ba_gen_oz,
         ba_output_so2_rate = 2000*ba_so2/ba_gen_ann,
         ba_output_co2_rate = 2000*ba_co2/ba_gen_ann,
         ba_output_hg_rate = 2000*ba_hg/ba_gen_ann,
         ba_input_nox_rate = 2000*ba_nox/ba_heat_input, # input emissions rate (lb/MMBtu)
         ba_input_nox_oz_rate = 2000*ba_nox/ba_heat_input_oz,
         ba_input_so2_rate = 2000*ba_so2/ba_heat_input,
         ba_input_co2_rate = 2000*ba_co2/ba_heat_input,
         ba_input_hg_rate = 2000*ba_hg/ba_heat_input) %>% 
  select(balance_authority_name, balance_authority_code, contains("rate")) # include necessary data only

### Combustion emission rates -----

# check: once plant file is complete, combustion techs may be identified already

combustion_fuels <- c("coal", 
                      "gas", 
                      "oil", 
                      "biomass")

ba_combustion_rates <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>%  # combustion flag identified in xwalk_energy_source.csv
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(ba_nox_comb = sum(plant_nox, na.rm = TRUE), 
            ba_nox_oz_comb = sum(plant_nox_oz, na.rm = TRUE), 
            ba_so2_comb = sum(plant_so2, na.rm = TRUE), 
            ba_co2_comb = sum(plant_co2, na.rm = TRUE), 
            ba_hg_comb = sum(plant_hg, na.rm = TRUE), 
            ba_gen_ann_comb = sum(plant_gen_ann, na.rm = TRUE), 
            ba_gen_oz_comb = sum(plant_gen_oz, na.rm = TRUE), 
            ba_heat_input_comb = sum(plant_heat_input, na.rm = TRUE), 
            ba_heat_input_oz_comb = sum(plant_heat_input_oz, na.rm = TRUE)) %>%
  mutate(ba_output_nox_rate_comb = 2000*ba_nox_comb/ba_gen_ann_comb, # output emissions rates (lb/MWh)
         ba_output_nox_oz_rate_comb = 2000*ba_nox_oz_comb/ba_gen_oz_comb,
         ba_output_so2_rate_comb = 2000*ba_so2_comb/ba_gen_ann_comb,
         ba_output_co2_rate_comb = 2000*ba_co2_comb/ba_gen_ann_comb,
         ba_output_hg_rate_comb = 2000*ba_hg_comb/ba_gen_ann_comb) %>% 
  select(balance_authority_name, balance_authority_code, contains("output")) # include necessary data only


### Output and input emission rates by fuel type (lb/MWh) -----

# issues/notes here: 
# applied case_when to only calculate rates when generation > 0
# should rate values with gen <= 0 be NA or 0? 

fossil_fuels <- c("coal", 
                  "gas", 
                  "oil")

ba_fuel_type <-
  plant_combined %>% 
  group_by(balance_authority_name, balance_authority_code, energy_source) %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources 
  summarize(ba_nox_fuel = sum(plant_nox, na.rm = TRUE), 
            ba_nox_oz_fuel = sum(plant_nox_oz, na.rm = TRUE), 
            ba_so2_fuel = sum(plant_so2, na.rm = TRUE), 
            ba_co2_fuel = sum(plant_co2, na.rm = TRUE), 
            ba_hg_fuel = sum(plant_hg, na.rm = TRUE), 
            ba_gen_ann_fuel = sum(plant_gen_ann, na.rm = TRUE), 
            ba_gen_oz_fuel = sum(plant_gen_oz, na.rm = TRUE), 
            ba_heat_input_fuel = sum(plant_heat_input, na.rm = TRUE), 
            ba_heat_input_oz_fuel = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
    # output emission rates 
    ba_output_nox_rate_fuel = case_when(    
      ba_gen_ann_fuel > 0 ~ 2000*ba_nox_fuel/ba_gen_ann_fuel, 
      ba_gen_ann_fuel <= 0 ~ 0),
    ba_output_nox_oz_rate_fuel = case_when(
      ba_gen_oz_fuel > 0 ~ 2000*ba_nox_oz_fuel/ba_gen_oz_fuel, 
      ba_gen_oz_fuel <= 0 ~ 0),
    ba_output_so2_rate_fuel = case_when(
      ba_gen_ann_fuel > 0 ~ 2000*ba_so2_fuel/ba_gen_ann_fuel, 
      ba_gen_ann_fuel <= 0 ~ 0),
    ba_output_co2_rate_fuel = case_when(
      ba_gen_ann_fuel > 0 ~ 2000*ba_co2_fuel/ba_gen_ann_fuel, 
      ba_gen_ann_fuel <= 0 ~ 0),
    ba_output_hg_rate_fuel = case_when(
      ba_gen_ann_fuel > 0 ~ 2000*ba_hg_fuel/ba_gen_ann_fuel, 
      ba_gen_ann_fuel <= 0 ~ 0), 
    
    # input emission rates (lb/MMBtu)
    ba_input_nox_rate_fuel = case_when(    
      ba_heat_input_fuel > 0 ~ 2000*ba_nox_fuel/ba_heat_input_fuel, 
      ba_heat_input_fuel <= 0 ~ 0),
    ba_input_nox_oz_rate_fuel = case_when(
      ba_heat_input_oz_fuel > 0 ~ 2000*ba_nox_oz_fuel/ba_heat_input_oz_fuel, 
      ba_heat_input_oz_fuel <= 0 ~ 0),
    ba_input_so2_rate_fuel = case_when(
      ba_heat_input_fuel > 0 ~ 2000*ba_so2_fuel/ba_heat_input_fuel, 
      ba_heat_input_fuel <= 0 ~ 0),
    ba_input_co2_rate_fuel = case_when(
      ba_heat_input_fuel > 0 ~ 2000*ba_co2_fuel/ba_heat_input_fuel, 
      ba_heat_input_fuel <= 0 ~ 0),
    ba_input_hg_rate_fuel = case_when(
      ba_heat_input_fuel > 0 ~ 2000*ba_hg_fuel/ba_heat_input_fuel, 
      ba_heat_input_fuel <= 0 ~ 0)) 

ba_fuel_type_rates <- 
  ba_fuel_type %>% 
  select(balance_authority_name, balance_authority_code, energy_source, contains("rate")) # include only necessary columns

# calculate fossil fuel rate

ba_fossil_rate <-
  plant_combined %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(ba_nox_fossil = sum(plant_nox, na.rm = TRUE), 
            ba_nox_oz_fossil = sum(plant_nox_oz, na.rm = TRUE), 
            ba_so2_fossil = sum(plant_so2, na.rm = TRUE), 
            ba_co2_fossil = sum(plant_co2, na.rm = TRUE), 
            ba_hg_fossil = sum(plant_hg, na.rm = TRUE), 
            ba_gen_ann_fossil = sum(plant_gen_ann, na.rm = TRUE), 
            ba_gen_oz_fossil = sum(plant_gen_oz, na.rm = TRUE), 
            ba_heat_input_fossil = sum(plant_heat_input, na.rm = TRUE), 
            ba_heat_input_oz_fossil = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
    # output emission rates 
    ba_output_nox_rate_fossil = case_when(    
      ba_gen_ann_fossil > 0 ~ 2000*ba_nox_fossil/ba_gen_ann_fossil, 
      ba_gen_ann_fossil <= 0 ~ 0),
    ba_output_nox_oz_rate_fossil = case_when(
      ba_gen_oz_fossil > 0 ~ 2000*ba_nox_oz_fossil/ba_gen_oz_fossil, 
      ba_gen_oz_fossil <= 0 ~ 0),
    ba_output_so2_rate_fossil = case_when(
      ba_gen_ann_fossil > 0 ~ 2000*ba_so2_fossil/ba_gen_ann_fossil, 
      ba_gen_ann_fossil <= 0 ~ 0),
    ba_output_co2_rate_fossil = case_when(
      ba_gen_ann_fossil > 0 ~ 2000*ba_co2_fossil/ba_gen_ann_fossil, 
      ba_gen_ann_fossil <= 0 ~ 0),
    ba_output_hg_rate_fossil = case_when(
      ba_gen_ann_fossil > 0 ~ 2000*ba_hg_fossil/ba_gen_ann_fossil, 
      ba_gen_ann_fossil <= 0 ~ 0), 
    
    # input emission rates (lb/MMBtu)
    ba_input_nox_rate_fossil = case_when(    
      ba_heat_input_fossil > 0 ~ 2000*ba_nox_fossil/ba_heat_input_fossil, 
      ba_heat_input_fossil <= 0 ~ 0),
    ba_input_nox_oz_rate_fossil = case_when(
      ba_heat_input_oz_fossil > 0 ~ 2000*ba_nox_oz_fossil/ba_heat_input_oz_fossil, 
      ba_heat_input_oz_fossil <= 0 ~ 0),
    ba_input_so2_rate_fossil = case_when(
      ba_heat_input_fossil > 0 ~ 2000*ba_so2_fossil/ba_heat_input_fossil, 
      ba_heat_input_fossil <= 0 ~ 0),
    ba_input_co2_rate_fossil = case_when(
      ba_heat_input_fossil > 0 ~ 2000*ba_co2_fossil/ba_heat_input_fossil, 
      ba_heat_input_fossil <= 0 ~ 0),
    ba_input_hg_rate_fossil = case_when(
      ba_heat_input_fossil > 0 ~ 2000*ba_hg_fossil/ba_heat_input_fossil, 
      ba_heat_input_fossil <= 0 ~ 0)) %>% 
  select(balance_authority_name, balance_authority_code, contains("rate")) # include only necessary columns


# format for final data frame 

ba_fuel_type_wider <-
  ba_fuel_type_rates %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = c(ba_output_nox_rate_fuel, 
                    ba_output_nox_oz_rate_fuel,
                    ba_output_so2_rate_fuel,
                    ba_output_co2_rate_fuel,
                    ba_output_hg_rate_fuel, 
                    ba_input_nox_rate_fuel, 
                    ba_input_nox_oz_rate_fuel,
                    ba_input_so2_rate_fuel,
                    ba_input_co2_rate_fuel,
                    ba_input_hg_rate_fuel)
  ) %>% 
  left_join(ba_fossil_rate, by = c("balance_authority_name", "balance_authority_code")) 

### Non-baseload output emission rates (lb/MWh) -----

# missing data from plant file ... 


### Generation by fuel type (MWh) and resource mix (percentage) -----

ba_gen_fuel <- 
  ba_fuel_type %>% 
  left_join(ba, by = c("balance_authority_name", "balance_authority_code")) %>% 
  select(balance_authority_name, balance_authority_code, energy_source, ba_gen_ann_fuel, ba_gen_oz_fuel, 
         ba_gen_ann, ba_gen_oz) %>% 
  group_by(balance_authority_name, balance_authority_code, energy_source) %>% 
  summarize(gen_fuel = sum(ba_gen_ann_fuel), 
            pct_gen_fuel= sum(ba_gen_ann_fuel)/ba_gen_ann) %>% 
  distinct()

# format for final data frame (pivot wider)

ba_gen_fuel_wider <-
  ba_gen_fuel %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = c(gen_fuel, 
                    pct_gen_fuel)
  ) 



### Renewable and non-renewable generation (MWh and percentage) -----

# check: what fuel types are included in RE? 

re_fuels <- c("biomass", 
              "solar", 
              "wind", 
              "geothermal", 
              "hydro")

# RE including hydro

ba_re <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels) %>% 
  left_join(ba, by = c("balance_authority_name", "balance_authority_code")) %>% 
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(gen_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re = sum(plant_gen_ann, na.rm = TRUE)/ba_gen_ann) %>% 
  distinct() 

# RE non hydro

re_fuels_no_hydro <- c("biomass", 
                       "solar", 
                       "wind", 
                       "geothermal")

ba_re_no_hydro <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels_no_hydro) %>% 
  left_join(ba, by = c("balance_authority_name", "balance_authority_code")) %>% 
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE)/ba_gen_ann) %>% 
  distinct()

# non-RE

ba_non_re <- 
  plant_combined %>% 
  filter(! energy_source %in% re_fuels) %>% 
  left_join(ba, by = c("balance_authority_name", "balance_authority_code")) %>% 
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(gen_non_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_re = sum(plant_gen_ann, na.rm = TRUE)/ba_gen_ann) %>% 
  distinct()


### Combustion and non-combustion generation (MWh) and resource mix (percent) -----

# generation from combustion sources

combustion_fuels <- c("coal", 
                      "oil", 
                      "gas", 
                      "biomass")

ba_combustion <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>% 
  left_join(ba, by = c("balance_authority_name", "balance_authority_code")) %>% 
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(gen_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_combustion = sum(plant_gen_ann, na.rm = TRUE)/ba_gen_ann) %>% 
  distinct()

# generation from non-combustion sources

ba_non_combustion <- 
  plant_combined %>% 
  filter(! energy_source %in% combustion_fuels) %>% 
  left_join(ba, by = c("balance_authority_name", "balance_authority_code")) %>% 
  group_by(balance_authority_name, balance_authority_code) %>% 
  summarize(gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE)/ba_gen_ann) %>% 
  distinct()

### Non-baseload generation by fuel type (MWh and percentage) -----



# Create final data frame -----

ba_final <- 
  ba %>% 
  left_join(ba_emission_rates, by = c("balance_authority_name", "balance_authority_code")) %>% # output/input emission rates
  left_join(ba_combustion_rates, by = c("balance_authority_name", "balance_authority_code")) %>% # combustion emission rates 
  left_join(ba_fuel_type_wider, by = c("balance_authority_name", "balance_authority_code")) %>% # fuel specific emission rates
  left_join(ba_gen_fuel_wider, by = c("balance_authority_name", "balance_authority_code")) %>% # generation values and percent by fuel type
  left_join(ba_non_re, by = c("balance_authority_name", "balance_authority_code")) %>% # non-re generation (MWh and %)
  left_join(ba_re, by = c("balance_authority_name", "balance_authority_code")) %>% # re generation (MWh and %)
  left_join(ba_re_no_hydro, by = c("balance_authority_name", "balance_authority_code")) %>%  # re no hydro generation (MWh and %)
  left_join(ba_combustion, by = c("balance_authority_name", "balance_authority_code")) %>%  # combustion generation (MWh and %)
  left_join(ba_non_combustion, by = c("balance_authority_name", "balance_authority_code")) %>%  # non-combustion generation (MWh and %)
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
  select(-contains("fuel_NA"), -ba_input_hg_rate_fuel_gas, 
         -ba_input_hg_rate_fuel_oil) # remove unnecessary columns

ba_rounded <- 
  ba_final %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals


# Export ba aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving BA aggregation file to folder data/outputs/")

write_csv(ba_rounded, "data/outputs/ba_aggregation.csv")




