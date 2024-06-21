

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
###     check: if flags (combustion, RE, etc.) are created in plant file
###     check: energy source column created in plant file 
###     check: which fuels included in combustion, fossil fuel, RE etc. 
###     issue: overestimating nameplate capacity at state level
###     issue: overestimating generation, emissions, heat input --> underestimation of rates
###     check: overestimating issues may be addressed in plant file through adjustments
###     check: reading in unit and generator files not necessary once plant file is ready
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
  group_by(plant_state, plant_name, plant_id, primary_fuel_type) %>% 
  summarize(plant_heat_input = sum(heat_input), 
            plant_heat_input_oz = sum(heat_input_oz),
            plant_nox = sum(nox_mass), 
            plant_nox_oz = sum(nox_oz), 
            plant_so2 = sum(so2_mass), 
            plant_co2 = sum(co2_mass))

# generator file 

generator <- read_rds("data/outputs/generator_file.RDS") # need to generalize for any year


# calculate plant level net generation by prime mover and primary fuel type
  
# check: assuming generation is split by NP capacity across generators (by looking at file)
# I summed this to get total plant generation, check if correct? 

# check: check if fuel type and fuel code need to be matched - maybe done in plant file beforehand

plant_generation <-
  generator %>% 
  # important to match fuel type to fuel codes? ignoring for now - there were many-to-many merge issues
  #left_join(xwalk_fuel_type, by = c("plant_id", "generator_id", "prime_mover", "fuel_code")) 
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

# State level aggregation ------

state <- 
  plant_combined %>% 
  group_by(year, plant_state) %>% 
  summarize(state_nameplate_capacity = sum(plant_nameplate_capacity, na.rm = TRUE), 
            state_heat_input = sum(plant_heat_input, na.rm = TRUE), 
            state_heat_input_oz = sum(plant_heat_input_oz, na.rm = TRUE),
            state_gen_ann = sum(plant_gen_ann, na.rm = TRUE), 
            state_gen_oz = sum(plant_gen_oz, na.rm = TRUE),
            state_nox = sum(plant_nox, na.rm = TRUE), 
            state_nox_oz = sum(plant_nox_oz, na.rm = TRUE), 
            state_so2 = sum(plant_so2, na.rm = TRUE), 
            state_co2 = sum(plant_co2, na.rm = TRUE)) %>% 
  mutate(state_hg = "--") %>% 
  ungroup()


## Calculate emission rates ------

### Output and input emission rates -----

state_emission_rates <- 
  state %>% 
  mutate(state_output_nox_rate = 2000*state_nox/state_gen_ann, # output emissions rates (lb/MWh)
            state_output_nox_oz_rate = 2000*state_nox_oz/state_gen_oz,
            state_output_so2_rate = 2000*state_so2/state_gen_ann,
            state_output_co2_rate = 2000*state_co2/state_gen_ann,
            state_output_hg_rate = "--",
            state_input_nox_rate = 2000*state_nox/state_heat_input, # input emissions rate (lb/MMBtu)
            state_input_nox_oz_rate = 2000*state_nox/state_heat_input_oz,
            state_input_so2_rate = 2000*state_so2/state_heat_input,
            state_input_co2_rate = 2000*state_co2/state_heat_input,
            state_input_hg_rate = "--") %>% 
  select(year, plant_state, contains("rate")) # include necessary data only

### Combustion emission rates -----

# check: once plant file is complete, combustion techs may be identified already
# check: which fuels are included in combustion

combustion_fuels <- c("coal", 
                      "gas", 
                      "oil", 
                      "biomass")

state_combustion_rates <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>%  # combustion flag identified in xwalk_energy_source.csv
  group_by(year, plant_state) %>% 
  summarize(state_nox_comb = sum(plant_nox, na.rm = TRUE), 
            state_nox_oz_comb = sum(plant_nox_oz, na.rm = TRUE), 
            state_so2_comb = sum(plant_so2, na.rm = TRUE), 
            state_co2_comb = sum(plant_co2, na.rm = TRUE), 
            state_gen_ann_comb = sum(plant_gen_ann, na.rm = TRUE), 
            state_gen_oz_comb = sum(plant_gen_oz, na.rm = TRUE), 
            state_heat_input_comb = sum(plant_heat_input, na.rm = TRUE), 
            state_heat_input_oz_comb = sum(plant_heat_input_oz, na.rm = TRUE)) %>%
  mutate(state_output_nox_rate_comb = 2000*state_nox_comb/state_gen_ann_comb, # output emissions rates (lb/MWh)
         state_output_nox_oz_rate_comb = 2000*state_nox_oz_comb/state_gen_oz_comb,
         state_output_so2_rate_comb = 2000*state_so2_comb/state_gen_ann_comb,
         state_output_co2_rate_comb = 2000*state_co2_comb/state_gen_ann_comb,
         state_output_hg_rate_comb = "--") %>% 
  select(year, 
         plant_state, 
         state_heat_input_comb, 
         state_heat_input_oz_comb,
         contains("output")) # include necessary data only
  

### Output and input emission rates by fuel type (lb/MWh) -----

# issues/notes here: 
# applied case_when to only calculate rates when generation > 0
# should rate values with gen <= 0 be NA or 0? 

fossil_fuels <- c("coal", 
                  "oil",
                  "gas")

state_fuel_type_fossil <-
  plant_combined %>% 
  group_by(year, plant_state, energy_source) %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources 
  summarize(state_nox_fuel = sum(plant_nox, na.rm = TRUE), 
            state_nox_oz_fuel = sum(plant_nox_oz, na.rm = TRUE), 
            state_so2_fuel = sum(plant_so2, na.rm = TRUE), 
            state_co2_fuel = sum(plant_co2, na.rm = TRUE), 
            state_gen_ann_fuel = sum(plant_gen_ann, na.rm = TRUE), 
            state_gen_oz_fuel = sum(plant_gen_oz, na.rm = TRUE), 
            state_heat_input_fuel = sum(plant_heat_input, na.rm = TRUE), 
            state_heat_input_oz_fuel = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
         # output emission rates 
         state_output_nox_rate_fuel = case_when(    
          state_gen_ann_fuel > 0 ~ 2000*state_nox_fuel/state_gen_ann_fuel, 
          state_gen_ann_fuel <= 0 ~ 0),
         state_output_nox_oz_rate_fuel = case_when(
           state_gen_oz_fuel > 0 ~ 2000*state_nox_oz_fuel/state_gen_oz_fuel, 
           state_gen_oz_fuel <= 0 ~ 0),
         state_output_so2_rate_fuel = case_when(
           state_gen_ann_fuel > 0 ~ 2000*state_so2_fuel/state_gen_ann_fuel, 
           state_gen_ann_fuel <= 0 ~ 0),
         state_output_co2_rate_fuel = case_when(
           state_gen_ann_fuel > 0 ~ 2000*state_co2_fuel/state_gen_ann_fuel, 
           state_gen_ann_fuel <= 0 ~ 0),
         state_output_hg_rate_fuel = "--", 
         
         # input emission rates (lb/MMBtu)
         state_input_nox_rate_fuel = case_when(    
           state_heat_input_fuel > 0 ~ 2000*state_nox_fuel/state_heat_input_fuel, 
           state_heat_input_fuel <= 0 ~ 0),
         state_input_nox_oz_rate_fuel = case_when(
           state_heat_input_oz_fuel > 0 ~ 2000*state_nox_oz_fuel/state_heat_input_oz_fuel, 
           state_heat_input_oz_fuel <= 0 ~ 0),
         state_input_so2_rate_fuel = case_when(
           state_heat_input_fuel > 0 ~ 2000*state_so2_fuel/state_heat_input_fuel, 
           state_heat_input_fuel <= 0 ~ 0),
         state_input_co2_rate_fuel = case_when(
           state_heat_input_fuel > 0 ~ 2000*state_co2_fuel/state_heat_input_fuel, 
           state_heat_input_fuel <= 0 ~ 0),
         state_input_hg_rate_fuel = "--") 

state_fuel_type_rates <- 
  state_fuel_type_fossil %>% 
  select(year, plant_state, energy_source, contains("rate")) # include only necessary columns

# calculate fossil fuel rate

state_fossil_rate <-
  plant_combined %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources
  group_by(year, plant_state) %>% 
  summarize(state_nox_fossil = sum(plant_nox, na.rm = TRUE), 
            state_nox_oz_fossil = sum(plant_nox_oz, na.rm = TRUE), 
            state_so2_fossil = sum(plant_so2, na.rm = TRUE), 
            state_co2_fossil = sum(plant_co2, na.rm = TRUE), 
            state_gen_ann_fossil = sum(plant_gen_ann, na.rm = TRUE), 
            state_gen_oz_fossil = sum(plant_gen_oz, na.rm = TRUE), 
            state_heat_input_fossil = sum(plant_heat_input, na.rm = TRUE), 
            state_heat_input_oz_fossil = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
    # output emission rates 
    state_output_nox_rate_fossil = case_when(    
      state_gen_ann_fossil > 0 ~ 2000*state_nox_fossil/state_gen_ann_fossil, 
      state_gen_ann_fossil <= 0 ~ 0),
    state_output_nox_oz_rate_fossil = case_when(
      state_gen_oz_fossil > 0 ~ 2000*state_nox_oz_fossil/state_gen_oz_fossil, 
      state_gen_oz_fossil <= 0 ~ 0),
    state_output_so2_rate_fossil = case_when(
      state_gen_ann_fossil > 0 ~ 2000*state_so2_fossil/state_gen_ann_fossil, 
      state_gen_ann_fossil <= 0 ~ 0),
    state_output_co2_rate_fossil = case_when(
      state_gen_ann_fossil > 0 ~ 2000*state_co2_fossil/state_gen_ann_fossil, 
      state_gen_ann_fossil <= 0 ~ 0),
    state_output_hg_rate_fossil = "--", 
    
    # input emission rates (lb/MMBtu)
    state_input_nox_rate_fossil = case_when(    
      state_heat_input_fossil > 0 ~ 2000*state_nox_fossil/state_heat_input_fossil, 
      state_heat_input_fossil <= 0 ~ 0),
    state_input_nox_oz_rate_fossil = case_when(
      state_heat_input_oz_fossil > 0 ~ 2000*state_nox_oz_fossil/state_heat_input_oz_fossil, 
      state_heat_input_oz_fossil <= 0 ~ 0),
    state_input_so2_rate_fossil = case_when(
      state_heat_input_fossil > 0 ~ 2000*state_so2_fossil/state_heat_input_fossil, 
      state_heat_input_fossil <= 0 ~ 0),
    state_input_co2_rate_fossil = case_when(
      state_heat_input_fossil > 0 ~ 2000*state_co2_fossil/state_heat_input_fossil, 
      state_heat_input_fossil <= 0 ~ 0),
    state_input_hg_rate_fossil = "--") %>% 
  select(year, plant_state, contains("rate")) # include only necessary columns


# format for final data frame 

state_fuel_type_wider <-
  state_fuel_type_rates %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = c(state_output_nox_rate_fuel, 
                    state_output_nox_oz_rate_fuel,
                    state_output_so2_rate_fuel,
                    state_output_co2_rate_fuel,
                    state_output_hg_rate_fuel, 
                    state_input_nox_rate_fuel, 
                    state_input_nox_oz_rate_fuel,
                    state_input_so2_rate_fuel,
                    state_input_co2_rate_fuel,
                    state_input_hg_rate_fuel)
  ) %>% 
  left_join(state_fossil_rate, by = c("year", "plant_state")) 


### Non-baseload output emission rates (lb/MWh) -----

# missing data from plant file ... 


## Calculate net generation and resource mix -----

### Generation by fuel type (MWh) and resource mix (percentage) -----

# net generation by energy_source

state_gen <- 
  plant_combined %>% 
  group_by(year, plant_state, energy_source) %>% 
  mutate(gen_fuel = sum(plant_gen_ann, na.rm = TRUE)) %>% 
  select(year, plant_state, energy_source, gen_fuel) %>% 
  distinct()

# format for final data frame (pivot wider)

state_gen_wider <-
  state_gen %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = gen_fuel, 
    names_prefix = "state_gen_") 


# resource mix (%) by energy_source

state_gen_pct <- 
  state_gen %>% 
  left_join(state, by = c("year", "plant_state")) %>% 
  summarize(pct_gen_fuel = sum(gen_fuel, na.rm = TRUE)/state_gen_ann) %>% 
  distinct()

# format for final data frame (pivot wider)

state_gen_pct_wider <-
  state_gen_pct %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = pct_gen_fuel, 
    names_prefix = "state_pct_gen_") 

  
### Renewable and non-renewable generation (MWh and percentage) -----

# check: what fuel types are included in RE? 

re_fuels <- c("biomass", 
              "solar", 
              "wind", 
              "geothermal", 
              "hydro")

# RE including hydro

state_re <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels) %>% 
  left_join(state, by = c("year", "plant_state")) %>% 
  group_by(year, plant_state) %>% 
  summarize(gen_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re = sum(plant_gen_ann, na.rm = TRUE)/state_gen_ann) %>% 
  distinct()

# format for final egrid output
state_re_gen <- 
  state_re %>% 
  select(year, plant_state, gen_re)

state_re_pct <- 
  state_re %>% 
  select(year, plant_state, pct_gen_re)

# RE non hydro

re_fuels_no_hydro <- c("biomass", 
                       "solar", 
                       "wind", 
                       "geothermal")

state_re_no_hydro <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels_no_hydro) %>% 
  left_join(state, by = c("year", "plant_state")) %>% 
  group_by(year, plant_state) %>% 
  summarize(gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE)/state_gen_ann) %>% 
  distinct()

# format for final egrid output
state_re_no_hydro_gen <- 
  state_re_no_hydro %>% 
  select(year, plant_state, gen_re_no_hydro)

state_re_no_hydro_pct <- 
  state_re_no_hydro %>% 
  select(year, plant_state, pct_gen_re_no_hydro)


# non-RE

state_non_re <- 
  plant_combined %>% 
  filter(! energy_source %in% re_fuels) %>% 
  left_join(state, by = c("year", "plant_state")) %>% 
  group_by(year, plant_state) %>% 
  summarize(gen_non_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_re = sum(plant_gen_ann, na.rm = TRUE)/state_gen_ann) %>% 
  distinct()

# format for final egrid output
state_non_re_gen <- 
  state_non_re %>% 
  select(year, plant_state, gen_non_re)

state_non_re_pct <- 
  state_non_re %>% 
  select(year, plant_state, pct_gen_non_re)


### Combustion and non-combustion generation (MWh) and resource mix (percent) -----

# generation from combustion sources

combustion_fuels <- c("coal", 
                      "oil", 
                      "gas", 
                      "biomass")

state_combustion <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>% 
  left_join(state, by = c("year", "plant_state")) %>% 
  group_by(year, plant_state) %>% 
  summarize(gen_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_combustion = sum(plant_gen_ann, na.rm = TRUE)/state_gen_ann) %>% 
  distinct()

# format for final egrid output
state_combustion_gen <- 
  state_combustion %>% 
  select(year, plant_state, gen_combustion)

state_combustion_pct <- 
  state_combustion %>% 
  select(year, plant_state, pct_gen_combustion)


# generation from non-combustion sources

state_non_combustion <- 
  plant_combined %>% 
  filter(! energy_source %in% combustion_fuels) %>% 
  left_join(state, by = c("year", "plant_state")) %>% 
  group_by(year, plant_state) %>% 
  summarize(gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE)/state_gen_ann) %>% 
  distinct()

# format for final egrid output
state_non_combustion_gen <- 
  state_non_combustion %>% 
  select(year, plant_state, gen_non_combustion)

state_non_combustion_pct <- 
  state_non_combustion %>% 
  select(year, plant_state, pct_gen_non_combustion)


### Non-baseload generation by fuel type (MWh and percentage) -----



# Create final data frame -----

state_merged <- 
  state %>% 
  left_join(state_emission_rates, by = c("year", "plant_state")) %>% # output/input emission rates
  left_join(state_combustion_rates, by = c("year", "plant_state")) %>% # combustion emission rates 
  left_join(state_fuel_type_wider, by = c("year", "plant_state")) %>% # fuel specific emission rates
  left_join(state_gen_wider, by = c("year", "plant_state")) %>% # generation values by energy source
  left_join(state_non_re_gen, by = c("year", "plant_state")) %>% # non-re generation (MWh)
  left_join(state_re_gen, by = c("year", "plant_state")) %>% # re generation (MWh)
  left_join(state_re_no_hydro_gen, by = c("year", "plant_state")) %>%  # re no hydro generation (MWh)
  left_join(state_combustion_gen, by = c("year", "plant_state")) %>%  # combustion generation (MWh)
  left_join(state_non_combustion_gen, by = c("year", "plant_state")) %>%  # non-combustion generation (MWh)
  left_join(state_gen_pct_wider, by = c("year", "plant_state")) %>% # generation % by energy source)
  left_join(state_non_re_pct, by = c("year", "plant_state")) %>% # non-re generation (%)
  left_join(state_re_pct, by = c("year", "plant_state")) %>% # re generation (%)
  left_join(state_re_no_hydro_pct, by = c("year", "plant_state")) %>%  # re no hydro generation (%)
  left_join(state_combustion_pct, by = c("year", "plant_state")) %>%  # combustion generation (%)
  left_join(state_non_combustion_pct, by = c("year", "plant_state")) %>%  # non-combustion generation (%)
  mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
         across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
  select(-contains("fuel_NA"), 
         -contains("gen_NA"), 
         -state_input_hg_rate_fuel_gas, 
         -state_input_hg_rate_fuel_oil, 
         -state_output_hg_rate_fuel_gas, 
         -state_output_hg_rate_fuel_oil) # remove unnecessary columns
  
state_rounded <- 
  state_merged %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals
  

# format to eGRID output 

state_formatted <- 
  state_rounded %>% 
  relocate(state_heat_input_comb, state_heat_input_oz_comb, .after = state_nameplate_capacity) %>% 
  relocate(state_output_nox_rate_fossil, .after = state_output_nox_rate_fuel_gas) %>% 
  relocate(state_output_nox_oz_rate_fossil, .after = state_output_nox_oz_rate_fuel_gas) %>% 
  relocate(state_output_so2_rate_fossil, .after = state_output_so2_rate_fuel_gas) %>% 
  relocate(state_output_co2_rate_fossil, .after = state_output_co2_rate_fuel_gas) %>% 
  relocate(state_output_hg_rate_fossil, .after = state_output_hg_rate_fuel_coal) %>% 
  relocate(state_input_nox_rate_fossil, .after = state_input_nox_rate_fuel_gas) %>% 
  relocate(state_input_nox_oz_rate_fossil, .after = state_input_nox_oz_rate_fuel_gas) %>% 
  relocate(state_input_so2_rate_fossil, .after = state_input_so2_rate_fuel_gas) %>% 
  relocate(state_input_co2_rate_fossil, .after = state_input_co2_rate_fuel_gas) %>% 
  relocate(state_input_hg_rate_fossil, .after = state_input_hg_rate_fuel_coal)


# Export state aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving state aggregation file to folder data/outputs/")

write_rds(state_formatted, "data/outputs/state_aggregation.RDS")




