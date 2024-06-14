

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
###     check: which fuels included in combustion, fossil fuel, RE etc. 
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
                                                       "other")) # other fossil and unknown need to be included here


# unit file 
# this will not be necessary when plant file is ready

unit <- read_rds("data/outputs/unit_file_2021.RDS") # need to generalize for any year

# calculate plant level emissions & heat input

plant_emissions_heat_rate <- 
  unit %>% 
  mutate(plant_id = as.character(plant_id)) %>%
  group_by(plant_state, plant_name, plant_id, primary_fuel_type) %>% 
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

# State level aggregation ------

us <- 
  plant_combined %>% 
  group_by(year) %>% 
  summarize(us_nameplate_capacity = sum(plant_nameplate_capacity, na.rm = TRUE), 
            us_heat_input = sum(plant_heat_input, na.rm = TRUE), 
            us_heat_input_oz = sum(plant_heat_input_oz, na.rm = TRUE), 
            us_gen_ann = sum(plant_gen_ann, na.rm = TRUE), 
            us_gen_oz = sum(plant_gen_oz, na.rm = TRUE), 
            us_nox = sum(plant_nox, na.rm = TRUE), 
            us_nox_oz = sum(plant_nox_oz, na.rm = TRUE), 
            us_so2 = sum(plant_so2, na.rm = TRUE), 
            us_co2 = sum(plant_co2, na.rm = TRUE)) %>% 
  mutate(us_hg = "--") %>% 
  ungroup()

### Output and input emission rates -----

us_emission_rates <- 
  us %>% 
  mutate(us_output_nox_rate = 2000*us_nox/us_gen_ann, # output emissions rates (lb/MWh)
         us_output_nox_oz_rate = 2000*us_nox_oz/us_gen_oz,
         us_output_so2_rate = 2000*us_so2/us_gen_ann,
         us_output_co2_rate = 2000*us_co2/us_gen_ann,
         us_output_hg_rate = "--",
         us_input_nox_rate = 2000*us_nox/us_heat_input, # input emissions rate (lb/MMBtu)
         us_input_nox_oz_rate = 2000*us_nox/us_heat_input_oz,
         us_input_so2_rate = 2000*us_so2/us_heat_input,
         us_input_co2_rate = 2000*us_co2/us_heat_input,
         us_input_hg_rate = "--") %>% 
  select(year, contains("rate")) # include necessary data only

### Combustion emission rates -----

# check: once plant file is complete, combustion techs may be identified already
# check: which fuels are included in combustion

combustion_fuels <- c("coal", 
                      "gas", 
                      "oil", 
                      "biomass")

us_combustion_rates <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>%  
  group_by(year) %>% 
  summarize(us_nox_comb = sum(plant_nox, na.rm = TRUE), 
            us_nox_oz_comb = sum(plant_nox_oz, na.rm = TRUE), 
            us_so2_comb = sum(plant_so2, na.rm = TRUE), 
            us_co2_comb = sum(plant_co2, na.rm = TRUE), 
            us_gen_ann_comb = sum(plant_gen_ann, na.rm = TRUE), 
            us_gen_oz_comb = sum(plant_gen_oz, na.rm = TRUE), 
            us_heat_input_comb = sum(plant_heat_input, na.rm = TRUE), 
            us_heat_input_oz_comb = sum(plant_heat_input_oz, na.rm = TRUE)) %>%
  mutate(us_output_nox_rate_comb = 2000*us_nox_comb/us_gen_ann_comb, # output emissions rates (lb/MWh)
         us_output_nox_oz_rate_comb = 2000*us_nox_oz_comb/us_gen_oz_comb,
         us_output_so2_rate_comb = 2000*us_so2_comb/us_gen_ann_comb,
         us_output_co2_rate_comb = 2000*us_co2_comb/us_gen_ann_comb,
         us_output_hg_rate_comb = "--") %>% 
  select(year, 
         us_heat_input_comb, 
         us_heat_input_oz_comb, 
         contains("output")) # include necessary data only


### Output and input emission rates by fuel type (lb/MWh) -----

# issues/notes here: 
# applied case_when to only calculate rates when generation > 0
# should rate values with gen <= 0 be NA or 0? 

fossil_fuels <- c("coal", 
                  "gas", 
                  "oil")

us_fuel_type <-
  plant_combined %>% 
  group_by(year, energy_source) %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources 
  summarize(us_nox_fuel = sum(plant_nox, na.rm = TRUE), 
            us_nox_oz_fuel = sum(plant_nox_oz, na.rm = TRUE), 
            us_so2_fuel = sum(plant_so2, na.rm = TRUE), 
            us_co2_fuel = sum(plant_co2, na.rm = TRUE), 
            us_gen_ann_fuel = sum(plant_gen_ann, na.rm = TRUE), 
            us_gen_oz_fuel = sum(plant_gen_oz, na.rm = TRUE), 
            us_heat_input_fuel = sum(plant_heat_input, na.rm = TRUE), 
            us_heat_input_oz_fuel = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
    # output emission rates 
    us_output_nox_rate_fuel = case_when(    
      us_gen_ann_fuel > 0 ~ 2000*us_nox_fuel/us_gen_ann_fuel, 
      us_gen_ann_fuel <= 0 ~ 0),
    us_output_nox_oz_rate_fuel = case_when(
      us_gen_oz_fuel > 0 ~ 2000*us_nox_oz_fuel/us_gen_oz_fuel, 
      us_gen_oz_fuel <= 0 ~ 0),
    us_output_so2_rate_fuel = case_when(
      us_gen_ann_fuel > 0 ~ 2000*us_so2_fuel/us_gen_ann_fuel, 
      us_gen_ann_fuel <= 0 ~ 0),
    us_output_co2_rate_fuel = case_when(
      us_gen_ann_fuel > 0 ~ 2000*us_co2_fuel/us_gen_ann_fuel, 
      us_gen_ann_fuel <= 0 ~ 0),
    us_output_hg_rate_fuel = "--", 
    
    # input emission rates (lb/MMBtu)
    us_input_nox_rate_fuel = case_when(    
      us_heat_input_fuel > 0 ~ 2000*us_nox_fuel/us_heat_input_fuel, 
      us_heat_input_fuel <= 0 ~ 0),
    us_input_nox_oz_rate_fuel = case_when(
      us_heat_input_oz_fuel > 0 ~ 2000*us_nox_oz_fuel/us_heat_input_oz_fuel, 
      us_heat_input_oz_fuel <= 0 ~ 0),
    us_input_so2_rate_fuel = case_when(
      us_heat_input_fuel > 0 ~ 2000*us_so2_fuel/us_heat_input_fuel, 
      us_heat_input_fuel <= 0 ~ 0),
    us_input_co2_rate_fuel = case_when(
      us_heat_input_fuel > 0 ~ 2000*us_co2_fuel/us_heat_input_fuel, 
      us_heat_input_fuel <= 0 ~ 0),
    us_input_hg_rate_fuel = "--") 

us_fuel_type_rates <- 
  us_fuel_type %>% 
  select(year, energy_source, contains("rate")) # include only necessary columns

# calculate fossil fuel rate

us_fossil_rate <-
  plant_combined %>% 
  filter(energy_source %in% fossil_fuels) %>% # only calculate these energy sources
  group_by(year) %>% 
  summarize(us_nox_fossil = sum(plant_nox, na.rm = TRUE), 
            us_nox_oz_fossil = sum(plant_nox_oz, na.rm = TRUE), 
            us_so2_fossil = sum(plant_so2, na.rm = TRUE), 
            us_co2_fossil = sum(plant_co2, na.rm = TRUE), 
            us_gen_ann_fossil = sum(plant_gen_ann, na.rm = TRUE), 
            us_gen_oz_fossil = sum(plant_gen_oz, na.rm = TRUE), 
            us_heat_input_fossil = sum(plant_heat_input, na.rm = TRUE), 
            us_heat_input_oz_fossil = sum(plant_heat_input_oz, na.rm = TRUE)) %>% 
  mutate(
    # output emission rates 
    us_output_nox_rate_fossil = case_when(    
      us_gen_ann_fossil > 0 ~ 2000*us_nox_fossil/us_gen_ann_fossil, 
      us_gen_ann_fossil <= 0 ~ 0),
    us_output_nox_oz_rate_fossil = case_when(
      us_gen_oz_fossil > 0 ~ 2000*us_nox_oz_fossil/us_gen_oz_fossil, 
      us_gen_oz_fossil <= 0 ~ 0),
    us_output_so2_rate_fossil = case_when(
      us_gen_ann_fossil > 0 ~ 2000*us_so2_fossil/us_gen_ann_fossil, 
      us_gen_ann_fossil <= 0 ~ 0),
    us_output_co2_rate_fossil = case_when(
      us_gen_ann_fossil > 0 ~ 2000*us_co2_fossil/us_gen_ann_fossil, 
      us_gen_ann_fossil <= 0 ~ 0),
    us_output_hg_rate_fossil = "--", 
    
    # input emission rates (lb/MMBtu)
    us_input_nox_rate_fossil = case_when(    
      us_heat_input_fossil > 0 ~ 2000*us_nox_fossil/us_heat_input_fossil, 
      us_heat_input_fossil <= 0 ~ 0),
    us_input_nox_oz_rate_fossil = case_when(
      us_heat_input_oz_fossil > 0 ~ 2000*us_nox_oz_fossil/us_heat_input_oz_fossil, 
      us_heat_input_oz_fossil <= 0 ~ 0),
    us_input_so2_rate_fossil = case_when(
      us_heat_input_fossil > 0 ~ 2000*us_so2_fossil/us_heat_input_fossil, 
      us_heat_input_fossil <= 0 ~ 0),
    us_input_co2_rate_fossil = case_when(
      us_heat_input_fossil > 0 ~ 2000*us_co2_fossil/us_heat_input_fossil, 
      us_heat_input_fossil <= 0 ~ 0),
    us_input_hg_rate_fossil = "--") %>% 
  select(year, contains("rate")) # include only necessary columns


# format for final data frame 

us_fuel_type_wider <-
  us_fuel_type_rates %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = c(us_output_nox_rate_fuel, 
                    us_output_nox_oz_rate_fuel,
                    us_output_so2_rate_fuel,
                    us_output_co2_rate_fuel,
                    us_output_hg_rate_fuel, 
                    us_input_nox_rate_fuel, 
                    us_input_nox_oz_rate_fuel,
                    us_input_so2_rate_fuel,
                    us_input_co2_rate_fuel,
                    us_input_hg_rate_fuel)
  ) %>% 
  left_join(us_fossil_rate, by = c("year")) 

### Non-baseload output emission rates (lb/MWh) -----

# missing data from plant file ... 


### Generation by fuel type (MWh) and resource mix (percentage) -----

# net generation by energy_source

us_gen <- 
  plant_combined %>% 
  group_by(year, energy_source) %>% 
  mutate(gen_fuel = sum(plant_gen_ann, na.rm = TRUE)) %>% 
  select(year, energy_source, gen_fuel) %>% 
  arrange(energy_source) %>% 
  distinct()

# format for final data frame (pivot wider)

us_gen_wider <-
  us_gen %>% 
  pivot_wider(
    names_from = energy_source, 
    values_from = gen_fuel, 
    names_prefix = "gen_") 


# resource mix (%) by energy_source

us_gen_pct <- 
  us_gen %>% 
  left_join(us, by = c("year")) %>% 
  summarize(pct_gen_fuel = sum(gen_fuel, na.rm = TRUE)/us_gen_ann) %>% 
  distinct()

# format for final data frame (pivot wider)

us_gen_pct_wider <-
  us_gen_pct %>% 
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

us_re <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels) %>% 
  left_join(us, by = c("year")) %>% 
  group_by(year) %>% 
  summarize(gen_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re = sum(plant_gen_ann, na.rm = TRUE)/us_gen_ann) %>% 
  distinct()


# separate generation and pct values for final formatting
us_re_gen <- 
  us_re %>% 
  select(year, gen_re)

us_re_pct <- 
  us_re %>% 
  select(year, pct_gen_re)


# RE non hydro

re_fuels_no_hydro <- c("biomass", 
                       "solar", 
                       "wind", 
                       "geothermal")

us_re_no_hydro <- 
  plant_combined %>% 
  filter(energy_source %in% re_fuels_no_hydro) %>% 
  left_join(us, by = c("year")) %>% 
  group_by(year) %>% 
  summarize(gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_re_no_hydro = sum(plant_gen_ann, na.rm = TRUE)/us_gen_ann) %>% 
  distinct()


# separate generation and pct values for final formatting
us_re_no_hydro_gen <- 
  us_re_no_hydro %>% 
  select(year, gen_re_no_hydro)

us_re_no_hydro_pct <- 
  us_re_no_hydro %>% 
  select(year, pct_gen_re_no_hydro)


# non-RE

us_non_re <- 
  plant_combined %>% 
  filter(! energy_source %in% re_fuels) %>% 
  left_join(us, by = c("year")) %>% 
  group_by(year) %>% 
  summarize(gen_non_re = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_re = sum(plant_gen_ann, na.rm = TRUE)/us_gen_ann) %>% 
  distinct()


# separate generation and pct values for final formatting
us_non_re_gen <- 
  us_non_re %>% 
  select(year, gen_non_re)

us_non_re_pct <- 
  us_non_re %>% 
  select(year, pct_gen_non_re)


### Combustion and non-combustion generation (MWh) and resource mix (percent) -----

# generation from combustion sources

combustion_fuels <- c("coal", 
                      "oil", 
                      "gas", 
                      "biomass")

us_combustion <- 
  plant_combined %>% 
  filter(energy_source %in% combustion_fuels) %>% 
  left_join(us, by = c("year")) %>% 
  group_by(year) %>% 
  summarize(gen_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_combustion = sum(plant_gen_ann, na.rm = TRUE)/us_gen_ann) %>% 
  distinct()

# separate generation and pct values for final formatting
us_combustion_gen <- 
  us_combustion %>% 
  select(year, gen_combustion)

us_combustion_pct <- 
  us_combustion %>% 
  select(year, pct_gen_combustion)


# generation from non-combustion sources

us_non_combustion <- 
  plant_combined %>% 
  filter(! energy_source %in% combustion_fuels) %>% 
  left_join(us, by = c("year")) %>% 
  group_by(year) %>% 
  summarize(gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE), 
            pct_gen_non_combustion = sum(plant_gen_ann, na.rm = TRUE)/us_gen_ann) %>% 
  distinct()

# separate generation and pct values for final formatting
us_non_combustion_gen <- 
  us_non_combustion %>% 
  select(year, gen_non_combustion)

us_non_combustion_pct <- 
  us_non_combustion %>% 
  select(year, pct_gen_non_combustion)

### Non-baseload generation by fuel type (MWh and percentage) -----



# Create final data frame -----

us_final <- 
  us %>% 
  left_join(us_emission_rates, by = c("year")) %>% # output/input emission rates
  left_join(us_combustion_rates, by = c("year")) %>% # combustion emission rates 
  left_join(us_fuel_type_wider, by = c("year")) %>% # fuel specific emission rates
  left_join(us_gen_wider, by = c("year")) %>% # generation values 
  left_join(us_non_re_gen, by = c("year")) %>% # non-re generation (MWh)
  left_join(us_re_gen, by = c("year")) %>% # re generation (MWh)
  left_join(us_re_no_hydro_gen, by = c("year")) %>%  # re no hydro generation (MWh)
  left_join(us_combustion_gen, by = c("year")) %>%  # combustion generation (MWh)
  left_join(us_non_combustion_gen, by = c("year")) %>%  # non-combustion generation (MWh)
  left_join(us_gen_pct_wider, by = c("year")) %>% # resource mix by energy source (%)
  left_join(us_non_re_pct, by = c("year")) %>% # non-re generation (%)
  left_join(us_re_pct, by = c("year")) %>% # re generation (%)
  left_join(us_re_no_hydro_pct, by = c("year")) %>%  # re no hydro generation (%)
  left_join(us_combustion_pct, by = c("year")) %>%  # combustion generation (%)
  left_join(us_non_combustion_pct, by = c("year")) %>%  # non-combustion generation (%)
  mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
         across(where(is.numeric), ~replace_na(., 0))) %>% # fill all other NAs with 0
  select(-contains("fuel_NA"), 
         -contains("gen_NA"), 
         -us_input_hg_rate_fuel_gas, 
         -us_input_hg_rate_fuel_oil,
         -us_output_hg_rate_fuel_gas, 
         -us_output_hg_rate_fuel_oil) # remove unnecessary columns

us_rounded <- 
  us_final %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3))) # round to three decimals


# format to eGRID output 

us_formatted <- 
  us_rounded %>% 
  relocate(us_heat_input_comb, us_heat_input_oz_comb, .after = us_nameplate_capacity) %>% 
  relocate(us_output_nox_rate_fossil, .after = us_output_nox_rate_fuel_gas) %>% 
  relocate(us_output_nox_oz_rate_fossil, .after = us_output_nox_oz_rate_fuel_gas) %>% 
  relocate(us_output_so2_rate_fossil, .after = us_output_so2_rate_fuel_gas) %>% 
  relocate(us_output_co2_rate_fossil, .after = us_output_co2_rate_fuel_gas) %>% 
  relocate(us_output_hg_rate_fossil, .after = us_output_hg_rate_fuel_coal) %>% 
  relocate(us_input_nox_rate_fossil, .after = us_input_nox_rate_fuel_gas) %>% 
  relocate(us_input_nox_oz_rate_fossil, .after = us_input_nox_oz_rate_fuel_gas) %>% 
  relocate(us_input_so2_rate_fossil, .after = us_input_so2_rate_fuel_gas) %>% 
  relocate(us_input_co2_rate_fossil, .after = us_input_co2_rate_fuel_gas) %>% 
  relocate(us_input_hg_rate_fossil, .after = us_input_hg_rate_fuel_coal)

# Export state aggregation file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving US aggregation file to folder data/outputs/")

# check: output file type 

write_csv(us_formatted, "data/outputs/US_aggregation.csv")


