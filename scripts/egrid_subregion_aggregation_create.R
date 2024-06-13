

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
# this will not be necessary when plant file is ready

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
  group_by(plant_id) %>% 
  summarize(plant_gen_ann = sum(generation_ann),
            plant_gen_oz = sum(generation_oz),
            plant_nameplate_capacity = sum(nameplate_capacity))

# plant file 

plant <- read_rds("data/outputs/plant_file_2021.RDS") # need to generalize for any year


# Combine data ------

plant_combined <- 
  plant %>% 
  mutate(plant_id = as.character(plant_id)) %>% 
  left_join(plant_emissions_heat_rate, by = c("plant_id", "plant_name")) %>% 
  left_join(plant_generation, by = "plant_id") %>% 
  left_join(xwalk_energy_source, by = "primary_fuel_type") 


# eGRID subregion level aggregation ------

egrid <- 
  plant_combined %>% 
  group_by(year, sub_region) %>% 
  summarize(egrid_nameplate_capacity = sum(plant_nameplate_capacity, na.rm = TRUE), 
            egrid_nox = sum(plant_nox, na.rm = TRUE), 
            egrid_nox_oz = sum(plant_nox_oz, na.rm = TRUE), 
            egrid_so2 = sum(plant_so2, na.rm = TRUE), 
            egrid_co2 = sum(plant_co2, na.rm = TRUE), 
            egrid_gen_ann = sum(plant_gen_ann, na.rm = TRUE), 
            egrid_gen_oz = sum(plant_gen_oz, na.rm = TRUE), 
            egrid_heat_input = sum(plant_heat_input, na.rm = TRUE), 
            egrid_heat_input_oz = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  ungroup()

### Output and input emission rates -----

egrid_emission_rates <- 
  egrid %>% 
  mutate(egrid_output_nox_rate = 2000*egrid_nox/egrid_gen_ann, # output emissions rates (lb/MWh)
         egrid_output_nox_oz_rate = 2000*egrid_nox_oz/egrid_gen_oz,
         egrid_output_so2_rate = 2000*egrid_so2/egrid_gen_ann,
         egrid_output_co2_rate = 2000*egrid_co2/egrid_gen_ann,
         egrid_output_hg_rate = "--",
         egrid_input_nox_rate = 2000*egrid_nox/egrid_heat_input, # input emissions rate (lb/MMBtu)
         egrid_input_nox_oz_rate = 2000*egrid_nox/egrid_heat_input_oz,
         egrid_input_so2_rate = 2000*egrid_so2/egrid_heat_input,
         egrid_input_co2_rate = 2000*egrid_co2/egrid_heat_input,
         egrid_input_hg_rate = "--") %>% 
  select(year, sub_region, contains("rate")) # include necessary data only

### Combustion emission rates -----

# check: once plant file is complete, combustion techs may be identified already

combustion_fuels <- c("coal", 
                      "oil", 
                      "gas", 
                      "biomass")

egrid_combustion_rates <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>%  # combustion flag identified in xwalk_energy_source.csv
  group_by(year, sub_region) %>% 
  summarize(egrid_nox_comb = sum(plant_nox, na.rm = TRUE), 
            egrid_nox_oz_comb = sum(plant_nox_oz, na.rm = TRUE), 
            egrid_so2_comb = sum(plant_so2, na.rm = TRUE), 
            egrid_co2_comb = sum(plant_co2, na.rm = TRUE), 
            egrid_gen_ann_comb = sum(plant_gen_ann, na.rm = TRUE), 
            egrid_gen_oz_comb = sum(plant_gen_oz, na.rm = TRUE), 
            egrid_heat_input_comb = sum(plant_heat_input, na.rm = TRUE), 
            egrid_heat_input_oz_comb = sum(plant_heat_input_oz, na.rm = TRUE)) %>%
  mutate(egrid_output_nox_rate_comb = 2000*egrid_nox_comb/egrid_gen_ann_comb, # output emissions rates (lb/MWh)
         egrid_output_nox_oz_rate_comb = 2000*egrid_nox_oz_comb/egrid_gen_oz_comb,
         egrid_output_so2_rate_comb = 2000*egrid_so2_comb/egrid_gen_ann_comb,
         egrid_output_co2_rate_comb = 2000*egrid_co2_comb/egrid_gen_ann_comb,
         egrid_output_hg_rate_comb = "--") %>% 
  select(year, sub_region, contains("output")) # include necessary data only


### Output and input emission rates by fuel type (lb/MWh) -----

# issues/notes here: 
# applied case_when to only calculate rates when generation > 0
# should rate values with gen <= 0 be NA or 0? 

fossil_fuels <- c("coal", 
                  "gas", 
                  "oil")

egrid_fuel_type <-
  plant_combined %>% 
  group_by(year, sub_region, energy_source) %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources 
  summarize(egrid_nox_fuel = sum(plant_nox, na.rm = TRUE), 
            egrid_nox_oz_fuel = sum(plant_nox_oz, na.rm = TRUE), 
            egrid_so2_fuel = sum(plant_so2, na.rm = TRUE), 
            egrid_co2_fuel = sum(plant_co2, na.rm = TRUE), 
            egrid_gen_ann_fuel = sum(plant_gen_ann, na.rm = TRUE), 
            egrid_gen_oz_fuel = sum(plant_gen_oz, na.rm = TRUE), 
            egrid_heat_input_fuel = sum(plant_heat_input, na.rm = TRUE), 
            egrid_heat_input_oz_fuel = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
    # output emission rates 
    egrid_output_nox_rate_fuel = case_when(    
      egrid_gen_ann_fuel > 0 ~ 2000*egrid_nox_fuel/egrid_gen_ann_fuel, 
      egrid_gen_ann_fuel <= 0 ~ 0),
    egrid_output_nox_oz_rate_fuel = case_when(
      egrid_gen_oz_fuel > 0 ~ 2000*egrid_nox_oz_fuel/egrid_gen_oz_fuel, 
      egrid_gen_oz_fuel <= 0 ~ 0),
    egrid_output_so2_rate_fuel = case_when(
      egrid_gen_ann_fuel > 0 ~ 2000*egrid_so2_fuel/egrid_gen_ann_fuel, 
      egrid_gen_ann_fuel <= 0 ~ 0),
    egrid_output_co2_rate_fuel = case_when(
      egrid_gen_ann_fuel > 0 ~ 2000*egrid_co2_fuel/egrid_gen_ann_fuel, 
      egrid_gen_ann_fuel <= 0 ~ 0),
    egrid_output_hg_rate_fuel = "--", 
    
    # input emission rates (lb/MMBtu)
    egrid_input_nox_rate_fuel = case_when(    
      egrid_heat_input_fuel > 0 ~ 2000*egrid_nox_fuel/egrid_heat_input_fuel, 
      egrid_heat_input_fuel <= 0 ~ 0),
    egrid_input_nox_oz_rate_fuel = case_when(
      egrid_heat_input_oz_fuel > 0 ~ 2000*egrid_nox_oz_fuel/egrid_heat_input_oz_fuel, 
      egrid_heat_input_oz_fuel <= 0 ~ 0),
    egrid_input_so2_rate_fuel = case_when(
      egrid_heat_input_fuel > 0 ~ 2000*egrid_so2_fuel/egrid_heat_input_fuel, 
      egrid_heat_input_fuel <= 0 ~ 0),
    egrid_input_co2_rate_fuel = case_when(
      egrid_heat_input_fuel > 0 ~ 2000*egrid_co2_fuel/egrid_heat_input_fuel, 
      egrid_heat_input_fuel <= 0 ~ 0),
    egrid_input_hg_rate_fuel = "--") 

egrid_fuel_type_rates <-
  egrid_fuel_type %>% 
  select(year, sub_region, energy_source, contains("rate")) # include only necessary columns

# calculate fossil fuel rate

egrid_fossil_rate <-
  plant_combined %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources
  group_by(sub_region) %>% 
  summarize(egrid_nox_fossil = sum(plant_nox, na.rm = TRUE), 
            egrid_nox_oz_fossil = sum(plant_nox_oz, na.rm = TRUE), 
            egrid_so2_fossil = sum(plant_so2, na.rm = TRUE), 
            egrid_co2_fossil = sum(plant_co2, na.rm = TRUE), 
            egrid_gen_ann_fossil = sum(plant_gen_ann, na.rm = TRUE), 
            egrid_gen_oz_fossil = sum(plant_gen_oz, na.rm = TRUE), 
            egrid_heat_input_fossil = sum(plant_heat_input, na.rm = TRUE), 
            egrid_heat_input_oz_fossil = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
    # output emission rates 
    egrid_output_nox_rate_fossil = case_when(    
      egrid_gen_ann_fossil > 0 ~ 2000*egrid_nox_fossil/egrid_gen_ann_fossil, 
      egrid_gen_ann_fossil <= 0 ~ 0),
    egrid_output_nox_oz_rate_fossil = case_when(
      egrid_gen_oz_fossil > 0 ~ 2000*egrid_nox_oz_fossil/egrid_gen_oz_fossil, 
      egrid_gen_oz_fossil <= 0 ~ 0),
    egrid_output_so2_rate_fossil = case_when(
      egrid_gen_ann_fossil > 0 ~ 2000*egrid_so2_fossil/egrid_gen_ann_fossil, 
      egrid_gen_ann_fossil <= 0 ~ 0),
    egrid_output_co2_rate_fossil = case_when(
      egrid_gen_ann_fossil > 0 ~ 2000*egrid_co2_fossil/egrid_gen_ann_fossil, 
      egrid_gen_ann_fossil <= 0 ~ 0),
    egrid_output_hg_rate_fossil = "--", 
    
    # input emission rates (lb/MMBtu)
    egrid_input_nox_rate_fossil = case_when(    
      egrid_heat_input_fossil > 0 ~ 2000*egrid_nox_fossil/egrid_heat_input_fossil, 
      egrid_heat_input_fossil <= 0 ~ 0),
    egrid_input_nox_oz_rate_fossil = case_when(
      egrid_heat_input_oz_fossil > 0 ~ 2000*egrid_nox_oz_fossil/egrid_heat_input_oz_fossil, 
      egrid_heat_input_oz_fossil <= 0 ~ 0),
    egrid_input_so2_rate_fossil = case_when(
      egrid_heat_input_fossil > 0 ~ 2000*egrid_so2_fossil/egrid_heat_input_fossil, 
      egrid_heat_input_fossil <= 0 ~ 0),
    egrid_input_co2_rate_fossil = case_when(
      egrid_heat_input_fossil > 0 ~ 2000*egrid_co2_fossil/egrid_heat_input_fossil, 
      egrid_heat_input_fossil <= 0 ~ 0),
    egrid_input_hg_rate_fossil = "--") %>% 
  select(year, sub_region, contains("rate")) # include only necessary columns


# format for final data frame 

egrid_fuel_type_rates_wider <-
  egrid_fuel_type_rates %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = c(egrid_output_nox_rate_fuel, 
                    egrid_output_nox_oz_rate_fuel,
                    egrid_output_so2_rate_fuel,
                    egrid_output_co2_rate_fuel,
                    egrid_output_hg_rate_fuel, 
                    egrid_input_nox_rate_fuel, 
                    egrid_input_nox_oz_rate_fuel,
                    egrid_input_so2_rate_fuel,
                    egrid_input_co2_rate_fuel,
                    egrid_input_hg_rate_fuel)
  ) %>% 
  left_join(egrid_fossil_rate, by = c("year", "sub_region")) 

### Non-baseload output emission rates (lb/MWh) -----

# missing data from plant file ... 


### Generation by fuel type (MWh) and resource mix (percentage) -----

# net generation by energy_source

egrid_gen <- 
  plant_combined %>% 
  group_by(year, sub_region, energy_source) %>% 
  mutate(gen_fuel = sum(plant_gen_ann, na.rm = TRUE)) %>% 
  select(year, sub_region, energy_source, gen_fuel) %>% 
  distinct()

# format for final data frame (pivot wider)

egrid_gen_wider <-
  egrid_gen %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = gen_fuel, 
    names_prefix = "gen_") 


# resource mix (%) by energy_source

egrid_gen_pct <- 
  egrid_gen %>% 
  left_join(egrid, by = c("year", "sub_region")) %>% 
  summarize(pct_gen_fuel = sum(gen_fuel, na.rm = TRUE)/state_gen_ann) %>% 
  distinct()

# format for final data frame (pivot wider)

egrid_gen_pct_wider <-
  egrid_gen_pct %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = pct_gen_fuel, 
    names_prefix = "pct_gen_") 

### Renewable and non-renewable generation (MWh and percentage) -----

# check: what fuel types are included in RE? 

re_fuels <- c("biomass", 
              "solar", 
              "wind", 
              "geothermal", 
              "hydro")

# RE including hydro

egrid_re <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels) %>% 
  left_join(egrid, by = c("year", "sub_region")) %>% 
  group_by(year, sub_region) %>% 
  summarize(gen_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re = sum(plant_gen_ann, na.rm = TRUE)/egrid_gen_ann) %>% 
  distinct()

# RE non hydro

re_fuels_no_hydro <- c("biomass", 
                       "solar", 
                       "wind", 
                       "geothermal")

egrid_re_no_hydro <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels_no_hydro) %>% 
  left_join(egrid, by = c("year", "sub_region")) %>% 
  group_by(year, sub_region) %>% 
  summarize(gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE)/egrid_gen_ann) %>% 
  distinct()

# non-RE

egrid_non_re <- 
  plant_combined %>% 
  filter(! primary_fuel_type %in% re_fuels) %>% 
  left_join(egrid, by = c("year", "sub_region")) %>% 
  group_by(year, sub_region) %>% 
  summarize(gen_non_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_re = sum(plant_gen_ann, na.rm = TRUE)/egrid_gen_ann) %>% 
  distinct()


### Combustion and non-combustion generation (MWh) and resource mix (percent) -----

# generation from combustion sources

combustion_fuels <- c("coal", 
                      "oil", 
                      "gas", 
                      "biomass")

egrid_combustion <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>% 
  left_join(egrid, by = c("year", "sub_region")) %>% 
  group_by(year, sub_region) %>% 
  summarize(gen_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_combustion = sum(plant_gen_ann, na.rm = TRUE)/egrid_gen_ann) %>% 
  distinct()

# generation from non-combustion sources

egrid_non_combustion <- 
  plant_combined %>% 
  filter(! energy_source %in% combustion_fuels) %>% 
  left_join(egrid, by = c("year", "sub_region")) %>% 
  group_by(year, sub_region) %>% 
  summarize(gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE)/egrid_gen_ann) %>% 
  distinct()

### Non-baseload generation by fuel type (MWh and percentage) -----



# Create final data frame -----

egrid_final <- 
  egrid %>% 
  left_join(egrid_emission_rates, by = c("year", "sub_region")) %>% # output/input emission rates
  left_join(egrid_combustion_rates, by = c("year", "sub_region")) %>% # combustion emission rates 
  left_join(egrid_fuel_type_rates_wider, by = c("year", "sub_region")) %>% # fuel specific emission rates
  left_join(egrid_gen_wider, by = c("year", "sub_region")) %>% # generation values and percent by fuel type
  left_join(egrid_non_re, by = c("year", "sub_region")) %>% # non-re generation (MWh and %)
  left_join(egrid_re, by = c("year", "sub_region")) %>% # re generation (MWh and %)
  left_join(egrid_re_no_hydro, by = c("year", "sub_region")) %>%  # re no hydro generation (MWh and %)
  left_join(egrid_combustion, by = c("year", "sub_region")) %>%  # combustion generation (MWh and %)
  left_join(egrid_non_combustion, by = c("year", "sub_region")) %>%  # non-combustion generation (MWh and %)
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
  select(-contains("fuel_NA"), 
         -egrid_input_hg_rate_fuel_gas, 
         -egrid_input_hg_rate_fuel_oil, 
         -egrid_output_hg_rate_fuel_gas, 
         -egrid_output_hg_rate_fuel_oil) # remove unnecessary columns

egrid_rounded <- 
  egrid_final %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals


# Export eGRID aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving eGRID subregion aggregation file to folder data/outputs/")

write_csv(egrid_rounded, "data/outputs/egrid_subregion_aggregation.csv")




