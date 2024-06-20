
# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)


# Load and clean data -----

# Rename necessary columns to snake_case 

column_names <- c(
  "year" = "YEAR", 
  "sub_region" = "SUBRGN",
  "sub_region_name" = "SRNAME", 
  "egrid_nameplate_capacity" = "SRNAMEPCAP", 
  "egrid_co2" = "SRCO2AN", 
  "coal_gen" = "SRGENACL", 
  "oil_gen" = "SRGENAOL", 
  "gas_gen" = "SRGENAGS", 
  "nuclear_gen" = "SRGENANC", 
  "hydro_gen" = "SRGENAHY", 
  "biomass_gen" = "SRGENABM",
  "wind_gen" = "SRGENAWI", 
  "solar_gen" = "SRGENASO",
  "geothermal_gen" = "SRGENAGT", 
  "otherfossil_gen" = "SRGENAOF",
  "net_gen" = "SRNGENAN")

egrid_2019 <- 
  read_excel("archive/egrid2019_data.xlsx", 
                        sheet = "SRL19", 
                        skip = 1) %>% 
  rename(all_of(column_names)) %>% 
  select(year, sub_region, sub_region_name, contains("egrid"), 
         contains("_gen")) 
  
egrid_2020 <- 
  read_excel("archive/egrid2020_data.xlsx", 
                        sheet = "SRL20",
                        skip = 1) %>% 
  rename(all_of(column_names)) %>% 
  select(year, sub_region, sub_region_name, contains("egrid"), 
         contains("_gen")) 

egrid_2021 <- 
  read_excel("archive/egrid2021_data.xlsx", 
                        sheet = "SRL21",
                        skip = 1) %>% 
  rename(all_of(column_names)) %>% 
  select(year, sub_region, sub_region_name, contains("egrid"), 
         contains("_gen")) 

egrid_2022 <- 
  read_excel("archive/egrid2022_data.xlsx", 
                        sheet = "SRL22",
                        skip = 1) %>% 
  rename(all_of(column_names)) %>% 
  select(year, sub_region, sub_region_name, contains("egrid"), 
         contains("_gen")) 


# CO2 comparison across eGRID subregions -----

# combine all egrid years
egrid_comparison <- 
  egrid_2019 %>% 
  bind_rows(egrid_2020) %>% 
  bind_rows(egrid_2021) %>% 
  bind_rows(egrid_2022) %>% 
  mutate(egrid_co2 = egrid_co2 / 1000000, 
         year = as.character(year)) 

# calculate co2 percent change 
egrid_co2_comparison <- 
  egrid_comparison %>% 
  select(year, sub_region, sub_region_name, egrid_co2) %>% 
  pivot_wider(names_from = year, 
              values_from = egrid_co2, 
              names_prefix = "co2_") %>% 
  mutate(co2_pct_diff = (co2_2022 - co2_2021) / co2_2021 * 100)
  

# Compare eGRID region and US resource mix -----

# calculate generation percent change
egrid_gen_comparison <- 
  egrid_comparison %>% 
  select(year, sub_region, sub_region_name, contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("gen")) %>% 
  mutate(
    coal = case_when(
      (coal_gen_2021 == 0 & coal_gen_2022 == 0) ~ 0, 
      coal_gen_2021 > 0 ~ round((coal_gen_2022 - coal_gen_2021) / coal_gen_2021 * 100, 1)), 
    oil = case_when(
      (oil_gen_2021 == 0 & oil_gen_2022 == 0) ~ 0, 
      oil_gen_2021 > 0 ~ round((oil_gen_2022 - oil_gen_2021) / oil_gen_2021 * 100, 1)),
    gas = case_when(
      (gas_gen_2021 == 0 & gas_gen_2022 == 0) ~ 0, 
      gas_gen_2021 > 0 ~ round((gas_gen_2022 - gas_gen_2021) / gas_gen_2021 * 100, 1)),
    other_fossil = case_when(
      (otherfossil_gen_2021 == 0 & otherfossil_gen_2022 == 0) ~ 0, 
      otherfossil_gen_2021 > 0 ~ round((otherfossil_gen_2022 - otherfossil_gen_2021) / otherfossil_gen_2021 * 100, 1)), 
    nuclear = case_when(
      (nuclear_gen_2021 == 0 & nuclear_gen_2022 == 0) ~ 0, 
      nuclear_gen_2021 > 0 ~ round((nuclear_gen_2022 - nuclear_gen_2021) / nuclear_gen_2021 * 100, 1)),
    hydro = case_when(
      (hydro_gen_2021 == 0 & hydro_gen_2022 == 0) ~ 0, 
      hydro_gen_2021 > 0 ~ round((hydro_gen_2022 - hydro_gen_2021) / hydro_gen_2021 * 100, 1)),
    biomass = case_when(
      (biomass_gen_2021 == 0 & biomass_gen_2022 == 0) ~ 0, 
      biomass_gen_2021 > 0 ~ round((biomass_gen_2022 - biomass_gen_2021) / biomass_gen_2021 * 100, 1)),
    wind = case_when(
      (wind_gen_2021 == 0 & wind_gen_2022 == 0) ~ 0, 
      wind_gen_2021 > 0 ~ round((wind_gen_2022 - wind_gen_2021) / wind_gen_2021 * 100, 1)),
    solar = case_when(
      (solar_gen_2021 == 0 & solar_gen_2022 == 0) ~ 0, 
      solar_gen_2021 > 0 ~ round((solar_gen_2022 - solar_gen_2021) / solar_gen_2021 * 100, 1)),
    geothermal = case_when(
      (geothermal_gen_2021 == 0 & geothermal_gen_2022 == 0) ~ 0, 
      geothermal_gen_2021 > 0 ~ round((geothermal_gen_2022 - geothermal_gen_2021) / geothermal_gen_2021 * 100, 1))) %>% 
  select(-contains("gen")) %>% 
  pivot_longer(cols = -any_of(c("sub_region", "sub_region_name")), 
               names_to = "energy_source", 
               values_to = "pct_change")

# format generation mix and merge in percent change data
egrid_resource_mix <- 
  egrid_comparison %>% 
  select(year, sub_region, sub_region_name, contains("gen"), -net_gen) %>% 
  pivot_longer(cols = contains("gen"), 
               names_to = "energy_source", 
               values_to = "generation") %>% 
  separate_wider_delim("energy_source", 
                       delim = "_", 
                       names = c("energy_source", "gen"), #1, 2, and 3 are unnecessary cols and will be deleted
                       too_many = "merge") %>% 
  select(-gen) 

egrid_resource_mix_wider <- 
  egrid_resource_mix %>% 
  pivot_wider(names_from = year, 
              values_from = generation) %>% 
  left_join(egrid_gen_comparison, by = c("sub_region", "sub_region_name", "energy_source"))

# summarize nameplate capacity and net gen 
egrid_cap_gen <- 
  egrid_comparison %>% 
  select(year, sub_region, sub_region_name, egrid_nameplate_capacity, net_gen)


# calculate us resource mix
us_resource_mix <-
  egrid_resource_mix %>%
  group_by(year, energy_source) %>%
  summarize(energy_source_generation = sum(generation)) %>%
  ungroup()

us_resource_mix_formatted <-
  us_resource_mix %>%
  pivot_wider(id_cols = year,
              names_from = energy_source,
              values_from = energy_source_generation) %>%
  mutate(net_gen = rowSums(across(where(is.numeric)))) %>%
  relocate(net_gen, .after = year)

us_comparison <-
  us_resource_mix %>%
  pivot_wider(names_from = year,
              values_from = energy_source_generation,
              names_prefix = "gen_") %>%
  mutate(pct_change_gen = round((gen_2022 - gen_2021) / gen_2021 * 100, 1))

us_comparison_totals <-
  us_comparison %>%
  summarize(net_gen_2019 = sum(gen_2019),
            net_gen_2020 = sum(gen_2020),
            net_gen_2021 = sum(gen_2021),
            net_gen_2022 = sum(gen_2022)) %>%
  mutate(pct_change_gen = (net_gen_2022 - net_gen_2021) / net_gen_2021 * 100)

# eGRID subregion resource mix -----








