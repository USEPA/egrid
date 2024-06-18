
# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)


# Load and data -----

# Rename necessary columns to snake_case 

column_names <- c(
  "year" = "YEAR", 
  "sub_region" = "SUBRGN",
  "sub_region_name" = "SRNAME", 
  "egrid_nameplate_capacity" = "SRNAMEPCAP", 
  "egrid_co2" = "SRCO2AN", 
  "egrid_gen_fuel_coal" = "SRGENACL", 
  "egrid_gen_fuel_oil" = "SRGENAOL", 
  "egrid_gen_fuel_gas" = "SRGENAGS", 
  "egrid_gen_fuel_nuclear" = "SRGENANC", 
  "egrid_gen_fuel_hydro" = "SRGENAHY", 
  "egrid_gen_fuel_biomass" = "SRGENABM",
  "egrid_gen_fuel_wind" = "SRGENAWI", 
  "egrid_gen_fuel_solar" = "SRGENASO",
  "egrid_gen_fuel_geothermal" = "SRGENAGT", 
  "egrid_gen_fuel_otherfossil" = "SRGENAOF")

egrid_2019 <- 
  read_excel("archive/egrid2019_data.xlsx", 
                        sheet = "SRL19", 
                        skip = 1) %>% 
  rename(all_of(column_names)) %>% 
  select(sub_region, sub_region_name, contains("egrid")) %>% 
  rename_with(~ paste0(., "_2019"), starts_with("egrid")) # append year suffix for relevant data
  
egrid_2020 <- 
  read_excel("archive/egrid2020_data.xlsx", 
                        sheet = "SRL20",
                        skip = 1) %>% 
  rename(all_of(column_names)) %>% 
  select(sub_region, sub_region_name, contains("egrid")) %>% 
  rename_with(~ paste0(., "_2020"), starts_with("egrid"))

egrid_2021 <- 
  read_excel("archive/egrid2021_data.xlsx", 
                        sheet = "SRL21",
                        skip = 1) %>% 
  rename(all_of(column_names)) %>% 
  select(sub_region, sub_region_name, contains("egrid")) %>% 
  rename_with(~ paste0(., "_2021"), starts_with("egrid"))

egrid_2022 <- 
  read_excel("archive/egrid2022_data.xlsx", 
                        sheet = "SRL22",
                        skip = 1) %>% 
  rename(all_of(column_names)) %>% 
  select(sub_region, sub_region_name, contains("egrid")) %>% 
  rename_with(~ paste0(., "_2022"), starts_with("egrid"))


# CO2 comparison across eGRID subregions -----

egrid_comparison <- 
  egrid_2019 %>% 
  left_join(egrid_2020, by = c("sub_region", "sub_region_name")) %>% 
  left_join(egrid_2021, by = c("sub_region", "sub_region_name")) %>% 
  left_join(egrid_2022, by = c("sub_region", "sub_region_name")) %>% 
  mutate(egrid_co2_2019 = egrid_co2_2019 / 1000000, 
         egrid_co2_2020 = egrid_co2_2020 / 1000000, 
         egrid_co2_2021 = egrid_co2_2021 / 1000000, 
         egrid_co2_2022 = egrid_co2_2022 / 1000000, 
         pct_co2_diff = round((egrid_co2_2022 - egrid_co2_2021) / egrid_co2_2021 * 100, 0), 
         pct_coal_diff = case_when(
           (egrid_gen_fuel_coal_2021 == 0 & egrid_gen_fuel_coal_2022 == 0) ~ 0, 
           egrid_gen_fuel_coal_2021 > 0 ~ round((egrid_gen_fuel_coal_2022 - egrid_gen_fuel_coal_2021) / egrid_gen_fuel_coal_2021 * 100, 1)), 
         pct_oil_diff = case_when(
           (egrid_gen_fuel_oil_2021 == 0 & egrid_gen_fuel_oil_2022 == 0) ~ 0, 
           egrid_gen_fuel_oil_2021 > 0 ~ round((egrid_gen_fuel_oil_2022 - egrid_gen_fuel_oil_2021) / egrid_gen_fuel_oil_2021 * 100, 1)),
         pct_gas_diff = case_when(
           (egrid_gen_fuel_gas_2021 == 0 & egrid_gen_fuel_gas_2022 == 0) ~ 0, 
           egrid_gen_fuel_gas_2021 > 0 ~ round((egrid_gen_fuel_gas_2022 - egrid_gen_fuel_gas_2021) / egrid_gen_fuel_gas_2021 * 100, 1)),
         pct_other_fossil_diff = case_when(
           (egrid_gen_fuel_otherfossil_2021 == 0 & egrid_gen_fuel_otherfossil_2022 == 0) ~ 0, 
           egrid_gen_fuel_otherfossil_2021 > 0 ~ round((egrid_gen_fuel_otherfossil_2022 - egrid_gen_fuel_otherfossil_2021) / egrid_gen_fuel_otherfossil_2021 * 100, 1)), 
         pct_nuclear_diff = case_when(
           (egrid_gen_fuel_nuclear_2021 == 0 & egrid_gen_fuel_nuclear_2022 == 0) ~ 0, 
           egrid_gen_fuel_nuclear_2021 > 0 ~ round((egrid_gen_fuel_nuclear_2022 - egrid_gen_fuel_nuclear_2021) / egrid_gen_fuel_nuclear_2021 * 100, 1)),
         pct_hydro_diff = case_when(
           (egrid_gen_fuel_hydro_2021 == 0 & egrid_gen_fuel_hydro_2022 == 0) ~ 0, 
           egrid_gen_fuel_hydro_2021 > 0 ~ round((egrid_gen_fuel_hydro_2022 - egrid_gen_fuel_hydro_2021) / egrid_gen_fuel_hydro_2021 * 100, 1)),
         pct_biomass_diff = case_when(
           (egrid_gen_fuel_biomass_2021 == 0 & egrid_gen_fuel_biomass_2022 == 0) ~ 0, 
           egrid_gen_fuel_biomass_2021 > 0 ~ round((egrid_gen_fuel_biomass_2022 - egrid_gen_fuel_biomass_2021) / egrid_gen_fuel_biomass_2021 * 100, 1)),
         pct_wind_diff = case_when(
           (egrid_gen_fuel_wind_2021 == 0 & egrid_gen_fuel_wind_2022 == 0) ~ 0, 
           egrid_gen_fuel_wind_2021 > 0 ~ round((egrid_gen_fuel_wind_2022 - egrid_gen_fuel_wind_2021) / egrid_gen_fuel_wind_2021 * 100, 1)),
         pct_solar_diff = case_when(
           (egrid_gen_fuel_solar_2021 == 0 & egrid_gen_fuel_solar_2022 == 0) ~ 0, 
           egrid_gen_fuel_solar_2021 > 0 ~ round((egrid_gen_fuel_solar_2022 - egrid_gen_fuel_solar_2021) / egrid_gen_fuel_solar_2021 * 100, 1)),
         pct_geothermal_diff = case_when(
           (egrid_gen_fuel_geothermal_2021 == 0 & egrid_gen_fuel_geothermal_2022 == 0) ~ 0, 
           egrid_gen_fuel_geothermal_2021 > 0 ~ round((egrid_gen_fuel_geothermal_2022 - egrid_gen_fuel_geothermal_2021) / egrid_gen_fuel_geothermal_2021 * 100, 1)))



# Compare US resource mix -----

us_resource_mix <- 
  egrid_comparison %>% 
  select(sub_region, sub_region_name, contains("gen")) %>% 
  pivot_longer(cols = contains("gen"), 
               names_to = "energy_source", 
               values_to = "generation") %>% 
  separate_wider_delim("energy_source", 
                       delim = "_", 
                       names = c("1", "2", "3", "energy_source", "year"), #1, 2, and 3 are unnecessary cols and will be deleted
                       too_many = "merge") %>% 
  group_by(year, energy_source) %>% 
  summarize(energy_source_generation = sum(generation)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = year,
              names_from = energy_source, 
              values_from = energy_source_generation) %>% 
  mutate(net_gen = rowSums(across(where(is.numeric)))) %>% 
  relocate(net_gen, .after = year)

us_comparison <- 
  us_resource_mix %>% 
  filter(year %in% c("2021", "2022")) %>% 
  arrange(year) %>% 
  mutate(percent_change = round((net_gen - lag(net_gen)) / lag(net_gen) * 100, 1))











