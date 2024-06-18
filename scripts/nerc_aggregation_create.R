

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

# factor energy sources ordering for final eGRID output
xwalk_energy_source$energy_source <- factor(xwalk_energy_source$energy_source, 
                                            levels = c("coal", 
                                                       "oil", 
                                                       "gas", 
                                                       "nuclear", 
                                                       "hydro", 
                                                       "biomass", 
                                                       "wind", 
                                                       "solar", 
                                                       "geothermal", 
                                                       "other"))


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
            plant_heat_input = sum(heat_input), 
            plant_heat_input_oz = sum(heat_input_oz))

# generator file 

generator <- read_rds("data/outputs/generator_file.RDS") # need to generalize for any year

# calculate plant level net generation by prime mover and primary fuel type

plant_generation <-
  generator %>% 
  mutate(plant_id = as.character(plant_id)) %>% 
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

# NERC level aggregation ------

nerc <- 
  plant_combined %>% 
  group_by(year, nerc) %>% 
  summarize(nerc_nameplate_capacity = sum(plant_nameplate_capacity, na.rm = TRUE),
            nerc_heat_input = sum(plant_heat_input, na.rm = TRUE), 
            nerc_heat_input_oz = sum(plant_heat_input_oz, na.rm = TRUE),
            nerc_gen_ann = sum(plant_gen_ann, na.rm = TRUE), 
            nerc_gen_oz = sum(plant_gen_oz, na.rm = TRUE), 
            nerc_nox = sum(plant_nox, na.rm = TRUE), 
            nerc_nox_oz = sum(plant_nox_oz, na.rm = TRUE), 
            nerc_so2 = sum(plant_so2, na.rm = TRUE), 
            nerc_co2 = sum(plant_co2, na.rm = TRUE)) %>% 
  mutate(nerc_hg = "--") %>% 
  ungroup()


## Calculate emission rates -----

### Output and input emission rates -----

nerc_emission_rates <- 
  nerc %>% 
  mutate(nerc_output_nox_rate = 2000*nerc_nox/nerc_gen_ann, # output emissions rates (lb/MWh)
         nerc_output_nox_oz_rate = 2000*nerc_nox_oz/nerc_gen_oz,
         nerc_output_so2_rate = 2000*nerc_so2/nerc_gen_ann,
         nerc_output_co2_rate = 2000*nerc_co2/nerc_gen_ann,
         nerc_output_hg_rate = "--",
         nerc_input_nox_rate = 2000*nerc_nox/nerc_heat_input, # input emissions rate (lb/MMBtu)
         nerc_input_nox_oz_rate = 2000*nerc_nox/nerc_heat_input_oz,
         nerc_input_so2_rate = 2000*nerc_so2/nerc_heat_input,
         nerc_input_co2_rate = 2000*nerc_co2/nerc_heat_input,
         nerc_input_hg_rate = "--") %>% 
  select(year, nerc, contains("rate")) # include necessary data only

### Combustion emission rates -----

# check: once plant file is complete, combustion techs may be identified already

combustion_fuels <- c("coal", 
                      "oil", 
                      "gas", 
                      "biomass")

nerc_combustion_rates <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>%  # combustion flag identified in xwalk_energy_source.csv
  group_by(year, nerc) %>% 
  summarize(nerc_nox_comb = sum(plant_nox, na.rm = TRUE), 
            nerc_nox_oz_comb = sum(plant_nox_oz, na.rm = TRUE), 
            nerc_so2_comb = sum(plant_so2, na.rm = TRUE), 
            nerc_co2_comb = sum(plant_co2, na.rm = TRUE), 
            nerc_gen_ann_comb = sum(plant_gen_ann, na.rm = TRUE), 
            nerc_gen_oz_comb = sum(plant_gen_oz, na.rm = TRUE), 
            nerc_heat_input_comb = sum(plant_heat_input, na.rm = TRUE), 
            nerc_heat_input_oz_comb = sum(plant_heat_input_oz, na.rm = TRUE)) %>%
  mutate(nerc_output_nox_rate_comb = 2000*nerc_nox_comb/nerc_gen_ann_comb, # output emissions rates (lb/MWh)
         nerc_output_nox_oz_rate_comb = 2000*nerc_nox_oz_comb/nerc_gen_oz_comb,
         nerc_output_so2_rate_comb = 2000*nerc_so2_comb/nerc_gen_ann_comb,
         nerc_output_co2_rate_comb = 2000*nerc_co2_comb/nerc_gen_ann_comb,
         nerc_output_hg_rate_comb = "--") %>% 
  select(year, 
         nerc, 
         nerc_heat_input_comb, 
         nerc_heat_input_oz_comb, 
         contains("output")) # include necessary data only


### Output and input emission rates by fuel type (lb/MWh) -----

# issues/notes here: 
# applied case_when to only calculate rates when generation > 0
# should rate values with gen <= 0 be NA or 0? 

fossil_fuels <- c("coal", 
                  "gas", 
                  "oil")

nerc_fuel_type <-
  plant_combined %>% 
  group_by(year, nerc, energy_source) %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources 
  summarize(nerc_nox_fuel = sum(plant_nox, na.rm = TRUE), 
            nerc_nox_oz_fuel = sum(plant_nox_oz, na.rm = TRUE), 
            nerc_so2_fuel = sum(plant_so2, na.rm = TRUE), 
            nerc_co2_fuel = sum(plant_co2, na.rm = TRUE), 
            nerc_gen_ann_fuel = sum(plant_gen_ann, na.rm = TRUE), 
            nerc_gen_oz_fuel = sum(plant_gen_oz, na.rm = TRUE), 
            nerc_heat_input_fuel = sum(plant_heat_input, na.rm = TRUE), 
            nerc_heat_input_oz_fuel = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
    # output emission rates 
    nerc_output_nox_rate_fuel = case_when(    
      nerc_gen_ann_fuel > 0 ~ 2000*nerc_nox_fuel/nerc_gen_ann_fuel, 
      nerc_gen_ann_fuel <= 0 ~ 0),
    nerc_output_nox_oz_rate_fuel = case_when(
      nerc_gen_oz_fuel > 0 ~ 2000*nerc_nox_oz_fuel/nerc_gen_oz_fuel, 
      nerc_gen_oz_fuel <= 0 ~ 0),
    nerc_output_so2_rate_fuel = case_when(
      nerc_gen_ann_fuel > 0 ~ 2000*nerc_so2_fuel/nerc_gen_ann_fuel, 
      nerc_gen_ann_fuel <= 0 ~ 0),
    nerc_output_co2_rate_fuel = case_when(
      nerc_gen_ann_fuel > 0 ~ 2000*nerc_co2_fuel/nerc_gen_ann_fuel, 
      nerc_gen_ann_fuel <= 0 ~ 0),
    nerc_output_hg_rate_fuel = "--", 
    
    # input emission rates (lb/MMBtu)
    nerc_input_nox_rate_fuel = case_when(    
      nerc_heat_input_fuel > 0 ~ 2000*nerc_nox_fuel/nerc_heat_input_fuel, 
      nerc_heat_input_fuel <= 0 ~ 0),
    nerc_input_nox_oz_rate_fuel = case_when(
      nerc_heat_input_oz_fuel > 0 ~ 2000*nerc_nox_oz_fuel/nerc_heat_input_oz_fuel, 
      nerc_heat_input_oz_fuel <= 0 ~ 0),
    nerc_input_so2_rate_fuel = case_when(
      nerc_heat_input_fuel > 0 ~ 2000*nerc_so2_fuel/nerc_heat_input_fuel, 
      nerc_heat_input_fuel <= 0 ~ 0),
    nerc_input_co2_rate_fuel = case_when(
      nerc_heat_input_fuel > 0 ~ 2000*nerc_co2_fuel/nerc_heat_input_fuel, 
      nerc_heat_input_fuel <= 0 ~ 0),
    nerc_input_hg_rate_fuel = "--") 
  
nerc_fuel_type_rates <- 
  nerc_fuel_type %>% 
  select(year, nerc, energy_source, contains("rate")) # include only necessary columns

# calculate fossil fuel rate

nerc_fossil_rate <-
  plant_combined %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources
  group_by(year, nerc) %>% 
  summarize(nerc_nox_fossil = sum(plant_nox, na.rm = TRUE), 
            nerc_nox_oz_fossil = sum(plant_nox_oz, na.rm = TRUE), 
            nerc_so2_fossil = sum(plant_so2, na.rm = TRUE), 
            nerc_co2_fossil = sum(plant_co2, na.rm = TRUE), 
            nerc_gen_ann_fossil = sum(plant_gen_ann, na.rm = TRUE), 
            nerc_gen_oz_fossil = sum(plant_gen_oz, na.rm = TRUE), 
            nerc_heat_input_fossil = sum(plant_heat_input, na.rm = TRUE), 
            nerc_heat_input_oz_fossil = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
    # output emission rates 
    nerc_output_nox_rate_fossil = case_when(    
      nerc_gen_ann_fossil > 0 ~ 2000*nerc_nox_fossil/nerc_gen_ann_fossil, 
      nerc_gen_ann_fossil <= 0 ~ 0),
    nerc_output_nox_oz_rate_fossil = case_when(
      nerc_gen_oz_fossil > 0 ~ 2000*nerc_nox_oz_fossil/nerc_gen_oz_fossil, 
      nerc_gen_oz_fossil <= 0 ~ 0),
    nerc_output_so2_rate_fossil = case_when(
      nerc_gen_ann_fossil > 0 ~ 2000*nerc_so2_fossil/nerc_gen_ann_fossil, 
      nerc_gen_ann_fossil <= 0 ~ 0),
    nerc_output_co2_rate_fossil = case_when(
      nerc_gen_ann_fossil > 0 ~ 2000*nerc_co2_fossil/nerc_gen_ann_fossil, 
      nerc_gen_ann_fossil <= 0 ~ 0),
    nerc_output_hg_rate_fossil = "--", 
    
    # input emission rates (lb/MMBtu)
    nerc_input_nox_rate_fossil = case_when(    
      nerc_heat_input_fossil > 0 ~ 2000*nerc_nox_fossil/nerc_heat_input_fossil, 
      nerc_heat_input_fossil <= 0 ~ 0),
    nerc_input_nox_oz_rate_fossil = case_when(
      nerc_heat_input_oz_fossil > 0 ~ 2000*nerc_nox_oz_fossil/nerc_heat_input_oz_fossil, 
      nerc_heat_input_oz_fossil <= 0 ~ 0),
    nerc_input_so2_rate_fossil = case_when(
      nerc_heat_input_fossil > 0 ~ 2000*nerc_so2_fossil/nerc_heat_input_fossil, 
      nerc_heat_input_fossil <= 0 ~ 0),
    nerc_input_co2_rate_fossil = case_when(
      nerc_heat_input_fossil > 0 ~ 2000*nerc_co2_fossil/nerc_heat_input_fossil, 
      nerc_heat_input_fossil <= 0 ~ 0),
    nerc_input_hg_rate_fossil = "--") %>% 
  select(year, nerc, contains("rate")) # include only necessary columns


# format for final data frame 

nerc_fuel_type_wider <-
  nerc_fuel_type_rates %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = c(nerc_output_nox_rate_fuel, 
                    nerc_output_nox_oz_rate_fuel,
                    nerc_output_so2_rate_fuel,
                    nerc_output_co2_rate_fuel,
                    nerc_output_hg_rate_fuel, 
                    nerc_input_nox_rate_fuel, 
                    nerc_input_nox_oz_rate_fuel,
                    nerc_input_so2_rate_fuel,
                    nerc_input_co2_rate_fuel,
                    nerc_input_hg_rate_fuel)
  ) %>% 
  left_join(nerc_fossil_rate, by = c("year", "nerc")) 

### Non-baseload output emission rates (lb/MWh) -----

# missing data from plant file ... 


## Calculate net generation and resource mix -----

### Generation by fuel type (MWh) and resource mix (percentage) -----

# net generation by energy_source

nerc_gen <- 
  plant_combined %>% 
  group_by(year, nerc, energy_source) %>% 
  mutate(gen_fuel = sum(plant_gen_ann, na.rm = TRUE)) %>% 
  select(year, nerc, energy_source, gen_fuel) %>% 
  distinct()

# format for final data frame (pivot wider)

nerc_gen_wider <-
  nerc_gen %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = gen_fuel, 
    names_prefix = "nerc_gen_") 


# resource mix (%) by energy_source

nerc_gen_pct <- 
  nerc_gen %>% 
  left_join(nerc, by = c("year", "nerc")) %>% 
  summarize(pct_gen_fuel = sum(gen_fuel, na.rm = TRUE)/nerc_gen_ann) %>% 
  distinct()

# format for final data frame (pivot wider)

nerc_gen_pct_wider <-
  nerc_gen_pct %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = pct_gen_fuel, 
    names_prefix = "nerc_pct_gen_") 



### Renewable and non-renewable generation (MWh and percentage) -----

# check: what fuel types are included in RE? 

re_fuels <- c("biomass", 
              "solar", 
              "wind", 
              "geothermal", 
              "hydro")

# RE including hydro

nerc_re <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels) %>% 
  left_join(nerc, by = c("year", "nerc")) %>% 
  group_by(year, nerc) %>% 
  summarize(gen_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re = sum(plant_gen_ann, na.rm = TRUE)/nerc_gen_ann) %>% 
  distinct()

# format for final egrid output
nerc_re_gen <- 
  nerc_re %>% 
  select(year, nerc, gen_re)

nerc_re_pct <- 
  nerc_re %>% 
  select(year, nerc, pct_gen_re)


# RE non hydro

re_fuels_no_hydro <- c("biomass", 
                       "solar", 
                       "wind", 
                       "geothermal")

nerc_re_no_hydro <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels_no_hydro) %>% 
  left_join(nerc, by = c("year", "nerc")) %>% 
  group_by(year, nerc) %>% 
  summarize(gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE)/nerc_gen_ann) %>% 
  distinct()

# format for final egrid output
nerc_re_no_hydro_gen <- 
  nerc_re_no_hydro %>% 
  select(year, nerc, gen_re_no_hydro)

nerc_re_no_hydro_pct <- 
  nerc_re_no_hydro %>% 
  select(year, nerc, pct_gen_re_no_hydro)


# non-RE

nerc_non_re <- 
  plant_combined %>% 
  filter(! energy_source %in% re_fuels) %>% 
  left_join(nerc, by = c("year", "nerc")) %>% 
  group_by(year, nerc) %>% 
  summarize(gen_non_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_re = sum(plant_gen_ann, na.rm = TRUE)/nerc_gen_ann) %>% 
  distinct()

# format for final egrid output
nerc_non_re_gen <- 
  nerc_non_re %>% 
  select(year, nerc, gen_non_re)

nerc_non_re_pct <- 
  nerc_non_re %>% 
  select(year, nerc, pct_gen_non_re)


### Combustion and non-combustion generation (MWh) and resource mix (percent) -----

# generation from combustion sources

combustion_fuels <- c("coal", 
                      "oil", 
                      "gas", 
                      "biomass")

nerc_combustion <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>% 
  left_join(nerc, by = c("year", "nerc")) %>% 
  group_by(year, nerc) %>% 
  summarize(gen_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_combustion = sum(plant_gen_ann, na.rm = TRUE)/nerc_gen_ann) %>% 
  distinct()

# format for final egrid output
nerc_combustion_gen <- 
  nerc_combustion %>% 
  select(year, nerc, gen_combustion)

nerc_combustion_pct <- 
  nerc_combustion %>% 
  select(year, nerc, pct_gen_combustion)


# generation from non-combustion sources

nerc_non_combustion <- 
  plant_combined %>% 
  filter(! energy_source %in% combustion_fuels) %>% 
  left_join(nerc, by = c("year", "nerc")) %>% 
  group_by(year, nerc) %>% 
  summarize(gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE)/nerc_gen_ann) %>% 
  distinct()

# format for final egrid output
nerc_non_combustion_gen <- 
  nerc_non_combustion %>% 
  select(year, nerc, gen_non_combustion)

nerc_non_combustion_pct <- 
  nerc_non_combustion %>% 
  select(year, nerc, pct_gen_non_combustion)


### Non-baseload generation by fuel type (MWh and percentage) -----



# Create final data frame -----

nerc_merged <- 
  nerc %>% 
  left_join(nerc_emission_rates, by = c("year", "nerc")) %>% # output/input emission rates
  left_join(nerc_combustion_rates, by = c("year", "nerc")) %>% # combustion emission rates 
  left_join(nerc_fuel_type_wider, by = c("year", "nerc")) %>% # fuel specific emission rates
  left_join(nerc_gen_wider, by = c("year", "nerc")) %>% # generation values and percent by fuel type
  left_join(nerc_non_re_gen, by = c("year", "nerc")) %>% # non-re generation (MWh)
  left_join(nerc_re_gen, by = c("year", "nerc")) %>% # re generation (MWh)
  left_join(nerc_re_no_hydro_gen, by = c("year", "nerc")) %>%  # re no hydro generation (MWh)
  left_join(nerc_combustion_gen, by = c("year", "nerc")) %>%  # combustion generation (MWh)
  left_join(nerc_non_combustion_gen, by = c("year", "nerc")) %>%  # non-combustion generation (MWh)
  left_join(nerc_gen_pct_wider, by = c("year", "nerc")) %>% # generation % by energy source)
  left_join(nerc_non_re_pct, by = c("year", "nerc")) %>% # non-re generation (%)
  left_join(nerc_re_pct, by = c("year", "nerc")) %>% # re generation (%)
  left_join(nerc_re_no_hydro_pct, by = c("year", "nerc")) %>%  # re no hydro generation (%)
  left_join(nerc_combustion_pct, by = c("year", "nerc")) %>%  # combustion generation (%)
  left_join(nerc_non_combustion_pct, by = c("year", "nerc")) %>%  # non-combustion generation (%)
  mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
         across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
  select(-contains("fuel_NA"), 
         -contains("gen_NA"), 
         -nerc_input_hg_rate_fuel_gas, 
         -nerc_input_hg_rate_fuel_oil, 
         -nerc_output_hg_rate_fuel_gas, 
         -nerc_output_hg_rate_fuel_oil) # remove unnecessary columns

nerc_rounded <- 
  nerc_merged %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals

# relocate columns to eGRID final output
nerc_formatted <- 
  nerc_rounded %>% 
  relocate(nerc_heat_input_comb, nerc_heat_input_oz_comb, .after = nerc_nameplate_capacity) %>% 
  relocate(nerc_output_nox_rate_fossil, .after = nerc_output_nox_rate_fuel_gas) %>% 
  relocate(nerc_output_nox_oz_rate_fossil, .after = nerc_output_nox_oz_rate_fuel_gas) %>% 
  relocate(nerc_output_so2_rate_fossil, .after = nerc_output_so2_rate_fuel_gas) %>% 
  relocate(nerc_output_co2_rate_fossil, .after = nerc_output_co2_rate_fuel_gas) %>% 
  relocate(nerc_output_hg_rate_fossil, .after = nerc_output_hg_rate_fuel_coal) %>% 
  relocate(nerc_input_nox_rate_fossil, .after = nerc_input_nox_rate_fuel_gas) %>% 
  relocate(nerc_input_nox_oz_rate_fossil, .after = nerc_input_nox_oz_rate_fuel_gas) %>% 
  relocate(nerc_input_so2_rate_fossil, .after = nerc_input_so2_rate_fuel_gas) %>% 
  relocate(nerc_input_co2_rate_fossil, .after = nerc_input_co2_rate_fuel_gas) %>% 
  relocate(nerc_input_hg_rate_fossil, .after = nerc_input_hg_rate_fuel_coal)

# Export NERC aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving NERC aggregation file to folder data/outputs/")

write_csv(nerc_formatted, "data/outputs/NERC_aggregation.csv")




