
### Notes ###
# do we want to 1) generalize this script for most recent 4 years or 
#               2) add each year as it is created? 

# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# Load and clean data -----

year <- params$eGRID_year

## Load and combine GRID region data -----

# Rename necessary columns to snake_case 

egrid_column_names <- c(
  "year" = "YEAR", 
  "sub_region" = "SUBRGN",
  "sub_region_name" = "SRNAME", 
  "egrid_nameplate_capacity" = "SRNAMEPCAP", 
  "egrid_co2_rate" = "SRCO2RTA", 
  "coal_gen" = "SRGENACL", 
  "oil_gen" = "SRGENAOL", 
  "gas_gen" = "SRGENAGS", 
  "nuclear_gen" = "SRGENANC", 
  "hydro_gen" = "SRGENAHY", 
  "biomass_gen" = "SRGENABM",
  "wind_gen" = "SRGENAWI", 
  "solar_gen" = "SRGENASO",
  "geothermal_gen" = "SRGENAGT", 
  "other_fossil_gen" = "SRGENAOF",
  "net_gen" = "SRNGENAN")

egrid_2019 <- 
  read_excel("archive/egrid2019_data.xlsx", 
                        sheet = "SRL19", 
                        skip = 1) %>% 
  rename(all_of(egrid_column_names)) %>% 
  select(year, sub_region, sub_region_name, contains("egrid"), 
         contains("_gen")) 
  
egrid_2020 <- 
  read_excel("archive/egrid2020_data.xlsx", 
                        sheet = "SRL20",
                        skip = 1) %>% 
  rename(all_of(egrid_column_names)) %>% 
  select(year, sub_region, sub_region_name, contains("egrid"), 
         contains("_gen")) 

egrid_2021 <- 
  read_excel("archive/egrid2021_data.xlsx", 
                        sheet = "SRL21",
                        skip = 1) %>% 
  rename(all_of(egrid_column_names)) %>% 
  select(year, sub_region, sub_region_name, contains("egrid"), 
         contains("_gen")) 

egrid_2022 <- 
  read_excel("archive/egrid2022_data.xlsx", 
                        sheet = "SRL22",
                        skip = 1) %>% 
  rename(all_of(egrid_column_names)) %>% 
  select(year, sub_region, sub_region_name, contains("egrid"), 
         contains("_gen")) 

# combine all egrid years
egrid_comparison <- 
  egrid_2019 %>% 
  bind_rows(egrid_2020) %>% 
  bind_rows(egrid_2021) %>% 
  bind_rows(egrid_2022) %>% 
  mutate(year = as.character(year))


## Load state region data -----

# Rename necessary columns to snake_case 

state_column_names <- c(
  "year" = "YEAR", 
  "state" = "PSTATABB",
  "state_nameplate_capacity" = "STNAMEPCAP", 
  "state_co2_rate" = "STCO2RTA", 
  "coal_gen" = "STGENACL", 
  "oil_gen" = "STGENAOL", 
  "gas_gen" = "STGENAGS", 
  "nuclear_gen" = "STGENANC", 
  "hydro_gen" = "STGENAHY", 
  "biomass_gen" = "STGENABM",
  "wind_gen" = "STGENAWI", 
  "solar_gen" = "STGENASO",
  "geothermal_gen" = "STGENAGT", 
  "other_fossil_gen" = "STGENAOF",
  "net_gen" = "STNGENAN")

state_2019 <- 
  read_excel("archive/egrid2019_data.xlsx", 
             sheet = "ST19", 
             skip = 1) %>% 
  rename(all_of(state_column_names)) %>% 
  select(year, contains("state"), 
         contains("_gen")) 

state_2020 <- 
  read_excel("archive/egrid2020_data.xlsx", 
             sheet = "ST20",
             skip = 1) %>% 
  rename(all_of(state_column_names)) %>% 
  select(year, contains("state"), 
         contains("_gen")) 

state_2021 <- 
  read_excel("archive/egrid2021_data.xlsx", 
             sheet = "ST21",
             skip = 1) %>% 
  rename(all_of(state_column_names)) %>% 
  select(year, contains("state"), 
         contains("_gen")) 

state_2022 <- 
  read_excel("archive/egrid2022_data.xlsx", 
             sheet = "ST22",
             skip = 1) %>% 
  rename(all_of(state_column_names)) %>% 
  select(year, contains("state"), 
         contains("_gen")) 

# combine all years
state_comparison <- 
  state_2019 %>% 
  bind_rows(state_2020) %>% 
  bind_rows(state_2021) %>% 
  bind_rows(state_2022) %>% 
  mutate(year = as.character(year))


## Load plant level data -----

plant_column_names <- c(
  "year" = "YEAR", 
  "state" = "PSTATABB",
  "plant_name" = "PNAME", 
  "plant_id" = "ORISPL",
  "plant_co2" = "PLCO2AN")

plant_2019 <- 
  read_excel("archive/egrid2019_data.xlsx", 
             sheet = "PLNT19", 
             skip = 1) %>% 
  rename(all_of(plant_column_names)) %>% 
  select(year, state, contains("plant")) 

plant_2020 <- 
  read_excel("archive/egrid2020_data.xlsx", 
             sheet = "PLNT20",
             skip = 1) %>% 
  rename(all_of(plant_column_names)) %>% 
  select(year, state, contains("plant")) 

plant_2021 <- 
  read_excel("archive/egrid2021_data.xlsx", 
             sheet = "PLNT21",
             skip = 1) %>% 
  rename(all_of(plant_column_names)) %>% 
  select(year, state, contains("plant")) 

plant_2022 <- 
  read_excel("archive/egrid2022_data.xlsx", 
             sheet = "PLNT22",
             skip = 1) %>% 
  rename(all_of(plant_column_names)) %>% 
  select(year, state, contains("plant")) 

# combine all years
plant_comparison <- 
  plant_2019 %>% 
  bind_rows(plant_2020) %>% 
  bind_rows(plant_2021) %>% 
  bind_rows(plant_2022) %>% 
  mutate(year = as.character(year))


# CO2 comparison across eGRID subregions -----

# calculate co2 percent change 
egrid_co2_comparison <- 
  egrid_comparison %>% 
  select(year, sub_region, sub_region_name, egrid_co2_rate) %>% 
  pivot_wider(names_from = year, 
              values_from = egrid_co2_rate, 
              names_prefix = "co2_rate_") %>% 
  mutate(co2_rate_pct_diff = (co2_rate_2022 - co2_rate_2021) / co2_rate_2021 * 100)
  

# eGRID region and US resource mix -----

# calculate generation percent change
egrid_gen_comparison <- 
  egrid_comparison %>% 
  select(year, sub_region, sub_region_name, contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("gen")) %>% 
  mutate(across(
         .cols = contains(cbind("gen_", year)), 
         .fns = ~ case_when(
                  (get(str_replace(cur_column(), year, "2021")) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), year, "2021")) != 0) ~ round((. - get(str_replace(cur_column(), year, "2021"))) / get(str_replace(cur_column(), year, "2021")) * 100, 1), 
                  (get(str_replace(cur_column(), year, "2021")) == 0 & . > 0) ~ 100), 
         .names = "{str_replace(.col, paste0('_gen_', year), '')}")) %>% 
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
  mutate(energy_source = str_replace(energy_source, "_gen", "")) 

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


# State resource mix ----- 

# calculate generation percent change
state_gen_comparison <- 
  state_comparison %>% 
  select(year, state, contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("gen")) %>% 
  mutate(across(
    .cols = contains(cbind("gen_", params$eGRID_year)), 
    .fns = ~ case_when(
      (get(str_replace(cur_column(), params$eGRID_year, "2021")) == 0 & . == 0) ~ 0, 
      (get(str_replace(cur_column(), params$eGRID_year, "2021")) != 0) ~ round((. - get(str_replace(cur_column(), params$eGRID_year, "2021"))) / get(str_replace(cur_column(), params$eGRID_year, "2021")) * 100, 1), 
      (get(str_replace(cur_column(), params$eGRID_year, "2021")) == 0 & . > 0) ~ 100), 
    .names = "{str_replace(.col, '_gen_2022', '')}")) %>% 
  select(-contains("gen")) %>% 
  pivot_longer(cols = -any_of(c("state")), 
               names_to = "energy_source", 
               values_to = "pct_change")

# format generation mix and merge in percent change data
state_resource_mix <- 
  state_comparison %>% 
  select(year, state, contains("gen"), -net_gen) %>% 
  pivot_longer(cols = contains("gen"), 
               names_to = "energy_source", 
               values_to = "generation") %>% 
  separate_wider_delim("energy_source", 
                       delim = "_", 
                       names = c("energy_source", "gen"), #1, 2, and 3 are unnecessary cols and will be deleted
                       too_many = "merge") %>% 
  select(-gen) 

state_resource_mix_wider <- 
  state_resource_mix %>% 
  pivot_wider(names_from = year, 
              values_from = generation) %>% 
  left_join(state_gen_comparison, by = c("state", "energy_source"))

# summarize nameplate capacity and net gen 
state_cap_gen <- 
  state_comparison %>% 
  select(year, state, state_nameplate_capacity, net_gen)







