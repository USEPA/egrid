## -------------------------------
##
## QA eGRID Annual Comparison 
## 
## Purpose: 
## 
## This file creates the QA to compare across eGRID years.
## 
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# Load necessary functions and name matches
source("scripts/functions/function_check_params.R")
source("scripts/1_production_model/name_matching.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year, temporal resolution, and version parameters are already defined.")
}
# Set years to evaluate ---------

cur_year <- as.numeric(params$eGRID_year)
prev_yr1 <- as.character(cur_year - 1)
prev_yr2 <- as.character(cur_year - 2)
prev_yr3 <- as.character(cur_year - 3)
cur_year <- as.character(cur_year)

# Load and clean data -----

## Download historical eGRID data
### Note: check each year if these URLs have changed 

# 2019 data
path_2019 <- "data/1_production_model/static_tables/historical_egrid/egrid2019_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/sites/default/files/2021-02/egrid2019_data.xlsx", 
                destfile = path_2019, 
                mode = "wb")
} else {
  print("Stopping. File egrid2019_data.xlsx already downloaded.")
}

# 2020 data
path_2020 <- "data/1_production_model/static_tables/historical_egrid/egrid2020_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/system/files/documents/2022-09/eGRID2020_Data_v2.xlsx", 
                destfile = path_2020, 
                mode = "wb")
} else {
  print("Stopping. File egrid2020_data.xlsx already downloaded.")
}

# 2021 data
path_2021 <- "data/1_production_model/static_tables/historical_egrid/egrid2021_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/system/files/documents/2023-01/eGRID2021_data.xlsx", 
                destfile = path_2021, 
                mode = "wb")
} else {
  print("Stopping. File egrid2021_data.xlsx already downloaded.")
}

# 2022 data
path_2022 <- "data/1_production_model/static_tables/historical_egrid/egrid2022_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx", 
                destfile = path_2021, 
                mode = "wb")
} else {
  print("Stopping. File egrid2022_data.xlsx already downloaded.")
}


## Load and combine eGRID subregion data -----

# rename necessary columns to snake_case 
subregion_nonmetric_annual <- setNames(names(subregion_nonmetric_annual), subregion_nonmetric_annual)

# read in subregion data for each data year to compare here
subregion_prev_yr3 <- 
  read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{prev_yr3}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(prev_yr3) %% 1000}"), 
                        skip = 1) %>% 
  select(any_of(subregion_nonmetric_annual), -contains("hg"))
  
subregion_prev_yr2 <- 
  read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{prev_yr2}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(prev_yr2) %% 1000}"),
                        skip = 1) %>% 
  select(any_of(subregion_nonmetric_annual), -contains("hg"))

subregion_prev_yr1 <- 
  read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{prev_yr1}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(prev_yr1) %% 1000}"),
                        skip = 1) %>% 
  select(any_of(subregion_nonmetric_annual), -contains("hg"))

subregion_cur_yr <- 
  read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(cur_year) %% 1000}"),
                        skip = 1) %>% 
  select(any_of(subregion_nonmetric_annual), -contains("hg"))

# combine all subregion years
subregion_comparison <- 
  subregion_prev_yr3 %>% 
  bind_rows(subregion_prev_yr2) %>% 
  bind_rows(subregion_prev_yr1) %>% 
  bind_rows(subregion_cur_yr) %>% 
  mutate(year = as.character(year))

names(subregion_comparison) <- sub("^subregion_", "", names(subregion_comparison))

## Load state region data -----

# rename necessary columns to snake_case 
state_nonmetric_annual <- setNames(names(state_nonmetric_annual), state_nonmetric_annual)

# read in state data for each data year to compare here
state_prev_yr3 <- 
  read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{prev_yr3}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(prev_yr3) %% 1000}"), 
             skip = 1) %>% 
  select(any_of(state_nonmetric_annual), -contains("hg"))

state_prev_yr2 <- 
  read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{prev_yr2}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(prev_yr2) %% 1000}"),
             skip = 1) %>% 
  select(any_of(state_nonmetric_annual), -contains("hg"))

state_prev_yr1 <- 
  read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{prev_yr1}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(prev_yr1) %% 1000}"),
             skip = 1) %>% 
  select(any_of(state_nonmetric_annual), -contains("hg"))

state_cur_yr <- 
  read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(cur_year) %% 1000}"),
             skip = 1) %>% 
  select(any_of(state_nonmetric_annual), -contains("hg"))

# combine all years
state_comparison <- 
  state_prev_yr3 %>% 
  bind_rows(state_prev_yr2) %>% 
  bind_rows(state_prev_yr1) %>% 
  bind_rows(state_cur_yr) %>% 
  mutate(year = as.character(year)) 

names(state_comparison) <- sub("^state_", "", names(state_comparison))

## Load US level data -------------

us_nonmetric_annual <- setNames(names(us_nonmetric_annual), us_nonmetric_annual)

us_prev_yr3 <- 
  read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{prev_yr3}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(prev_yr3) %% 1000}"), 
             skip = 1) %>% 
  select(any_of(us_nonmetric_annual), contains("rate"), contains("netgen"), -contains("hg"))

us_prev_yr2 <- 
  read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{prev_yr2}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(prev_yr2) %% 1000}"),
             skip = 1) %>% 
  select(any_of(us_nonmetric_annual), contains("rate"), contains("netgen"), -contains("hg"))

us_prev_yr1 <- 
  read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{prev_yr1}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(prev_yr1) %% 1000}"),
             skip = 1) %>% 
  select(any_of(us_nonmetric_annual), contains("rate"), contains("netgen"), -contains("hg"))

us_cur_yr <- 
  read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(cur_year) %% 1000}"),
             skip = 1) %>% 
  select(any_of(us_nonmetric_annual), contains("rate"), contains("netgen"), -contains("hg"))


# combine all years
us_comparison <- 
  us_prev_yr3 %>% 
  bind_rows(us_prev_yr2) %>% 
  bind_rows(us_prev_yr1) %>% 
  bind_rows(us_cur_yr) %>% 
  mutate(year = as.character(year), 
         subregion = "US") 

names(us_comparison) <- sub("^us_", "", names(us_comparison))

## Combine US and subregion data 
subregion_us_comparison <- 
  us_comparison %>% 
  bind_rows(subregion_comparison) 

# Emission rate comparisons -------------
## Emission rate comparison across eGRID subregions -------

# calculate emission rate percent change 

subregion_rate_comparison <- 
  subregion_us_comparison %>% 
  select(year, subregion, name, contains("rate"), contains("netgen"), -contains("hg")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("rate") | contains("netgen")) %>% 
  mutate(across(.cols = contains(glue::glue("rate_{cur_year}")), 
                .fns = ~ (.x - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                                      / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100,
                .names = "{sub(glue::glue('{cur_year}'), '', .col)}pct"), 
         across(.cols = contains(glue::glue("netgen_{cur_year}")), 
                .fns = ~ case_when(
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                                ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                                                / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
                .names = "{sub('_netgen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  mutate(generation_notes = paste(sprintf("Coal: %+.1f%%,", coal_pct), # add summary of net generation changes
                                  sprintf("Oil: %+.1f%%,", oil_pct), 
                                  sprintf("Gas: %+.1f%%,", gas_pct), 
                                  sprintf("Other fossil: %+.1f%%,", other_ff_pct), 
                                  sprintf("Nuclear: %+.1f%%,", nuclear_pct), 
                                  sprintf("Hydro: %+.1f%%,", hydro_pct), 
                                  sprintf("Biomass: %+.1f%%,", biomass_pct), 
                                  sprintf("Wind: %+.1f%%,", wind_pct),
                                  sprintf("Solar: %+.1f%%,", solar_pct), 
                                  sprintf("Geothermal: %+.1f%%,", geothermal_pct)))
  

## Emission rate comparison across eGRID states -------

state_rate_comparison <- 
  state_comparison %>%
  select(year, state, contains("rate"), contains("netgen"), -contains("hg")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("rate") | contains("netgen")) %>% 
  mutate(across(.cols = contains(c(glue::glue("rate_{cur_year}"))),
                .fns = ~ (. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100,
                .names = "{sub(glue::glue('{cur_year}'), '', .col)}pct"), , 
         across(.cols = contains(glue::glue("netgen_{cur_year}")), 
                .fns = ~ case_when(
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                  ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                          / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
                .names = "{sub('_netgen.*', '', .col)}_pct")) %>% 
  select(-contains("netgen")) %>% 
  mutate(generation_notes = paste(sprintf("Coal: %+.1f%%,", coal_pct), # add summary of net generation changes
                                  sprintf("Oil: %+.1f%%,", oil_pct), 
                                  sprintf("Gas: %+.1f%%,", gas_pct), 
                                  sprintf("Other fossil: %+.1f%%,", other_ff_pct), 
                                  sprintf("Nuclear: %+.1f%%,", nuclear_pct), 
                                  sprintf("Hydro: %+.1f%%,", hydro_pct), 
                                  sprintf("Biomass: %+.1f%%,", biomass_pct), 
                                  sprintf("Wind: %+.1f%%,", wind_pct),
                                  sprintf("Solar: %+.1f%%,", solar_pct), 
                                  sprintf("Geothermal: %+.1f%%,", geothermal_pct)))

# eGRID subregion and US resource mix -----

# calculate generation percent change

subregion_gen_comparison <- 
  subregion_us_comparison %>% 
  select(year, subregion, name, contains("netgen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("netgen")) %>% 
  mutate(across(
         .cols = contains(glue::glue("netgen_{cur_year}")), 
         .fns = ~ case_when(
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                          ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                                            / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
         .names = "{sub('_netgen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  pivot_longer(cols = -c("subregion", "name"), 
               names_to = "energy_source", 
               values_to = "pct_change")

# format generation mix and merge in percent change data
subregion_resource_mix <- 
  subregion_us_comparison %>% 
  select(year, subregion, name, contains("netgen")) %>% 
  pivot_longer(cols = contains("netgen"), 
               names_to = "energy_source", 
               values_to = "generation") %>% 
  mutate(energy_source = str_replace(energy_source, "_netgen", "")) 

subregion_resource_mix$energy_source <- 
  factor(subregion_resource_mix$energy_source, 
          levels = c("coal", 
                     "oil", 
                     "gas",
                     "other_ff", 
                     "nuclear", 
                     "hydro", 
                     "biomass", 
                     "wind", 
                     "solar", 
                     "geothermal", 
                     "other_purchased"))


subregion_resource_mix_wider <- 
  subregion_resource_mix %>% 
  pivot_wider(names_from = year, 
              values_from = generation) %>% 
  left_join(subregion_gen_comparison, by = c("subregion", "name", "energy_source")) %>% 
  select(-name)

# summarize nameplate capacity and net gen 
subregion_cap_gen <- 
  subregion_us_comparison %>% 
  select(year, subregion, name, nameplate_capacity, generation)


# calculate us resource mix
us_resource_mix <-
  subregion_resource_mix %>%
  group_by(year, energy_source) %>%
  summarize(energy_source_generation = sum(generation, na.rm = TRUE)) %>%
  ungroup() 

cur_year_gen <- as.symbol(glue::glue("generation_{cur_year}"))
prev_year_gen <- as.symbol(glue::glue("generation_{prev_yr1}"))

us_resource_mix_formatted <-
  us_resource_mix %>%
  pivot_wider(names_from = energy_source,
              values_from = energy_source_generation) %>%
  mutate(net_gen = rowSums(across(where(is.numeric)))) %>%
  relocate(net_gen, .after = year) %>% 
  pivot_longer(cols = -c(year),
               names_to = "energy_source", 
               values_to = "generation") %>% 
  pivot_wider(names_from = year, 
              values_from = generation, 
              names_prefix = "generation_") %>% 
  mutate(percent_change = ({{cur_year_gen}} - {{prev_year_gen}}) 
                                          / {{prev_year_gen}} * 100)


# State resource mix ----- 

# calculate generation percent change
state_gen_comparison <- 
  state_comparison %>% 
  select(year, state, contains("netgen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("netgen")) %>% 
  mutate(across(
    .cols = contains(glue::glue("netgen_{(cur_year)}")), 
    .fns = ~ case_when(
      (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & .x == 0) ~ 0, 
      (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                ~ round((.x - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                        / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
      (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
    .names = "{sub('_netgen.*', '', .col)}_pct")) %>% 
  select(-contains("netgen")) %>% 
  pivot_longer(cols = -c("state"), 
               names_to = "energy_source", 
               values_to = "pct_change")

# format generation mix and merge in percent change data
state_resource_mix <- 
  state_comparison %>% 
  select(year, state, contains("netgen")) %>% 
  pivot_longer(cols = contains("netgen"), 
               names_to = "energy_source", 
               values_to = "generation") %>% 
  mutate(energy_source = str_replace(energy_source, "_netgen", ""))
  

state_resource_mix_wider <- 
  state_resource_mix %>% 
  pivot_wider(names_from = year, 
              values_from = generation) %>% 
  left_join(state_gen_comparison, by = c("state", "energy_source"))

# summarize nameplate capacity and net gen 
state_cap_gen <- 
  state_comparison %>% 
  select(year, state, nameplate_capacity, generation)

