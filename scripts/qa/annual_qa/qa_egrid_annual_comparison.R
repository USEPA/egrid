

# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# Check for params() --------

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- (params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
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
path_2019 <- "data/static_tables/qa/egrid2019_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/sites/default/files/2021-02/egrid2019_data.xlsx", 
                destfile = path_2019, 
                mode = "wb")
} else {
  print("Stopping. File egrid2019_data.xlsx already downloaded.")
}

# 2020 data
path_2020 <- "data/static_tables/qa/egrid2020_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/system/files/documents/2022-09/eGRID2020_Data_v2.xlsx", 
                destfile = path_2020, 
                mode = "wb")
} else {
  print("Stopping. File egrid2020_data.xlsx already downloaded.")
}

# 2021 data
path_2021 <- "data/static_tables/qa/egrid2021_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/system/files/documents/2023-01/eGRID2021_data.xlsx", 
                destfile = path_2021, 
                mode = "wb")
} else {
  print("Stopping. File egrid2021_data.xlsx already downloaded.")
}

# 2022 data
path_2022 <- "data/static_tables/qa/egrid2022_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx", 
                destfile = path_2021, 
                mode = "wb")
} else {
  print("Stopping. File egrid2022_data.xlsx already downloaded.")
}


## Load and combine eGRID subregion data -----

# Rename necessary columns to snake_case 

  subregion_column_names <- c(
  "year" = "YEAR", 
  "subregion" = "SUBRGN",
  "subregion_name" = "SRNAME", 
  "nameplate_capacity" = "SRNAMEPCAP", 
  "nox_output_rate" = "SRNOXRTA", 
  "nox_oz_output_rate" = "SRNOXRTO", 
  "so2_output_rate" = "SRSO2RTA",
  "co2_output_rate" = "SRCO2RTA", 
  "ch4_output_rate" = "SRCH4RTA", 
  "n2o_output_rate" = "SRN2ORTA", 
  "co2e_output_rate" = "SRC2ERTA",
  "nox_input_rate" = "SRNOXRA", 
  "nox_oz_input_rate" = "SRNOXRO", 
  "so2_input_rate" = "SRSO2RA",
  "co2_input_rate" = "SRCO2RA", 
  "ch4_input_rate" = "SRCH4RA", 
  "n2o_input_rate" = "SRN2ORA", 
  "co2e_input_rate" = "SRC2ERA",
  "nox_combustion_rate" = "SRNOXCRT", 
  "nox_oz_combustion_rate" = "SRNOXCRO", 
  "so2_combustion_rate" = "SRSO2CRT",
  "co2_combustion_rate" = "SRCO2CRT", 
  "ch4_combustion_rate" = "SRCH4CRT", 
  "n2o_combustion_rate" = "SRN2OCRT", 
  "co2e_combustion_rate" = "SRC2ECRT",
  "nox_nonbaseload_rate" = "SRNBNOX",
  "nox_oz_nonbaseload_rate" = "SRNBNXO",
  "so2_nonbaseload_rate" = "SRNBSO2",
  "co2_nonbaseload_rate" = "SRNBCO2",
  "ch4_nonbaseload_rate" =  "SRNBCH4",
  "n2o_nonbaseload_rate" =  "SRNBN2O",
  "co2e_nonbaseload_rate" = "SRNBC2E",
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
  "other_purchased_gen" = "SRGENAOP", 
  "net_gen" = "SRNGENAN")

# read in subregion data for each data year to compare here
subregion_prev_yr3 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr3}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(prev_yr3) %% 1000}"), 
                        skip = 1) %>% 
  select(any_of(subregion_column_names))
  
subregion_prev_yr2 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr2}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(prev_yr2) %% 1000}"),
                        skip = 1) %>% 
  select(any_of(subregion_column_names))

subregion_prev_yr1 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr1}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(prev_yr1) %% 1000}"),
                        skip = 1) %>% 
  select(any_of(subregion_column_names))

subregion_cur_yr <- 
  read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(cur_year) %% 1000}"),
                        skip = 1) %>% 
  select(any_of(subregion_column_names))

# combine all subregion years
subregion_comparison <- 
  subregion_prev_yr3 %>% 
  bind_rows(subregion_prev_yr2) %>% 
  bind_rows(subregion_prev_yr1) %>% 
  bind_rows(subregion_cur_yr) %>% 
  mutate(year = as.character(year))

## Load state region data -----

# Rename necessary columns to snake_case 

state_column_names <- c(
  "year" = "YEAR", 
  "state" = "PSTATABB",
  "state_nameplate_capacity" = "STNAMEPCAP", 
  "nox_output_rate" = "STNOXRTA", 
  "nox_oz_output_rate" = "STNOXRTO", 
  "so2_output_rate" = "STSO2RTA",
  "co2_output_rate" = "STCO2RTA", 
  "ch4_output_rate" = "STCH4RTA", 
  "n2o_output_rate" = "STN2ORTA", 
  "co2e_output_rate" = "STC2ERTA",
  "nox_input_rate" = "STNOXRA", 
  "nox_oz_input_rate" = "STNOXRO", 
  "so2_input_rate" = "STSO2RA",
  "co2_input_rate" = "STCO2RA", 
  "ch4_input_rate" = "STCH4RA", 
  "n2o_input_rate" = "STN2ORA", 
  "co2e_input_rate" = "STC2ERA",
  "nox_combustion_rate" = "STNOXCRT", 
  "nox_oz_combustion_rate" = "STNOXCRO", 
  "so2_combustion_rate" = "STSO2CRT",
  "co2_combustion_rate" = "STCO2CRT", 
  "ch4_combustion_rate" = "STCH4CRT", 
  "n2o_combustion_rate" = "STN2OCRT", 
  "co2e_combustion_rate" = "STC2ECRT",
  "nox_nonbaseload_rate" = "STNBNOX",
  "nox_oz_nonbaseload_rate" = "STNBNXO",
  "so2_nonbaseload_rate" = "STNBSO2",
  "co2_nonbaseload_rate" = "STNBCO2",
  "ch4_nonbaseload_rate" =  "STNBCH4",
  "n2o_nonbaseload_rate" =  "STNBN2O",
  "co2e_nonbaseload_rate" = "STNBC2E",
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

# read in state data for each data year to compare here
state_prev_yr3 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr3}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(prev_yr3) %% 1000}"), 
             skip = 1) %>% 
  select(any_of(state_column_names))

state_prev_yr2 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr2}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(prev_yr2) %% 1000}"),
             skip = 1) %>% 
  select(any_of(state_column_names))

state_prev_yr1 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr1}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(prev_yr1) %% 1000}"),
             skip = 1) %>% 
  select(any_of(state_column_names))

state_cur_yr <- 
  read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(cur_year) %% 1000}"),
             skip = 1) %>% 
  select(any_of(state_column_names))

# combine all years
state_comparison <- 
  state_prev_yr3 %>% 
  bind_rows(state_prev_yr2) %>% 
  bind_rows(state_prev_yr1) %>% 
  bind_rows(state_cur_yr) %>% 
  mutate(year = as.character(year))

## Load US level data -------------

us_column_names <- c(
  "year" = "YEAR", 
  "state" = "PSTATABB",
  "nameplate_capacity" = "USNAMEPCAP", 
  "nox_output_rate" = "USNOXRTA", 
  "nox_oz_output_rate" = "USNOXRTO", 
  "so2_output_rate" = "USSO2RTA",
  "co2_output_rate" = "USCO2RTA", 
  "ch4_output_rate" = "USCH4RTA", 
  "n2o_output_rate" = "USN2ORTA", 
  "co2e_output_rate" = "USC2ERTA",
  "nox_input_rate" = "USNOXRA", 
  "nox_oz_input_rate" = "USNOXRO", 
  "so2_input_rate" = "USSO2RA",
  "co2_input_rate" = "USCO2RA", 
  "ch4_input_rate" = "USCH4RA", 
  "n2o_input_rate" = "USN2ORA", 
  "co2e_input_rate" = "USC2ERA",
  "nox_combustion_rate" = "USNOXCRT", 
  "nox_oz_combustion_rate" = "USNOXCRO", 
  "so2_combustion_rate" = "USSO2CRT",
  "co2_combustion_rate" = "USCO2CRT", 
  "ch4_combustion_rate" = "USCH4CRT", 
  "n2o_combustion_rate" = "USN2OCRT", 
  "co2e_combustion_rate" = "USC2ECRT",
  "coal_gen" = "USGENACL", 
  "oil_gen" = "USGENAOL", 
  "gas_gen" = "USGENAGS", 
  "nuclear_gen" = "USGENANC", 
  "hydro_gen" = "USGENAHY", 
  "biomass_gen" = "USGENABM",
  "wind_gen" = "USGENAWI", 
  "solar_gen" = "USGENASO",
  "geothermal_gen" = "USGENAGT", 
  "other_fossil_gen" = "USGENAOF",
  "other_purchased_gen" = "USGENAOP", 
  "net_gen" = "USNGENAN")

us_prev_yr3 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr3}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(prev_yr3) %% 1000}"), 
             skip = 1) %>% 
  select(any_of(us_column_names))

us_prev_yr2 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr2}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(prev_yr2) %% 1000}"),
             skip = 1) %>% 
  select(any_of(us_column_names))

us_prev_yr1 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr1}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(prev_yr1) %% 1000}"),
             skip = 1) %>% 
  select(any_of(us_column_names))

us_cur_yr <- 
  read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(cur_year) %% 1000}"),
             skip = 1) %>% 
  select(any_of(us_column_names))


# combine all years
us_comparison <- 
  us_prev_yr3 %>% 
  bind_rows(us_prev_yr2) %>% 
  bind_rows(us_prev_yr1) %>% 
  bind_rows(us_cur_yr) %>% 
  mutate(year = as.character(year), 
         subregion = "US")

## Combine US and subregion data 
subregion_us_comparison <- 
  us_comparison %>% 
  bind_rows(subregion_comparison)

# Emission rate comparisons -------------
## Emission rate comparison across eGRID subregions -------

# calculate emission rate percent change 

subregion_rate_comparison <- 
  subregion_us_comparison %>% 
  select(year, subregion, subregion_name, contains("rate"), contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("rate") | contains("gen")) %>% 
  mutate(across(.cols = contains(glue::glue("rate_{cur_year}")), 
                .fns = ~ (. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                                      / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100,
                .names = "{sub('_rate.*', '', .col)}_pct"), 
         across(.cols = contains(glue::glue("gen_{cur_year}")), 
                .fns = ~ case_when(
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                                ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                                                / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
                .names = "{sub('_gen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  mutate(generation_notes = paste(sprintf("Coal: %+.1f%%,", coal_pct), # add summary of net generation changes
                                  sprintf("Oil: %+.1f%%,", oil_pct), 
                                  sprintf("Gas: %+.1f%%,", gas_pct), sprintf("Other fossil: %+.1f%%,", other_fossil_pct), 
                                  sprintf("Nuclear: %+.1f%%,", nuclear_pct), sprintf("Hydro: %+.1f%%,", hydro_pct), 
                                  sprintf("Biomass: %+.1f%%,", biomass_pct), sprintf("Wind: %+.1f%%,", wind_pct),
                                  sprintf("Solar: %+.1f%%,", solar_pct), sprintf("Geothermal: %+.1f%%,", geothermal_pct)))
  
## Emission rate comparison across eGRID states -------

state_rate_comparison <- 
  state_comparison %>% 
  select(year, state, contains("rate"), contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("rate") | contains("gen")) %>% 
  mutate(across(.cols = contains(glue::glue("rate_{cur_year}")), 
                .fns = ~ (. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100,
                .names = "{sub('_rate.*', '', .col)}_pct"), 
         across(.cols = contains(glue::glue("gen_{cur_year}")), 
                .fns = ~ case_when(
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                  ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                          / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
                .names = "{sub('_gen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  mutate(generation_notes = paste(sprintf("Coal: %+.1f%%,", coal_pct), # add summary of net generation changes
                                  sprintf("Oil: %+.1f%%,", oil_pct), 
                                  sprintf("Gas: %+.1f%%,", gas_pct), sprintf("Other fossil: %+.1f%%,", other_fossil_pct), 
                                  sprintf("Nuclear: %+.1f%%,", nuclear_pct), sprintf("Hydro: %+.1f%%,", hydro_pct), 
                                  sprintf("Biomass: %+.1f%%,", biomass_pct), sprintf("Wind: %+.1f%%,", wind_pct),
                                  sprintf("Solar: %+.1f%%,", solar_pct), sprintf("Geothermal: %+.1f%%,", geothermal_pct)))

# eGRID subregion and US resource mix -----

# calculate generation percent change

subregion_gen_comparison <- 
  subregion_us_comparison %>% 
  select(year, subregion, subregion_name, contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("gen")) %>% 
  mutate(across(
         .cols = contains(glue::glue("gen_{cur_year}")), 
         .fns = ~ case_when(
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                          ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                                            / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
         .names = "{sub('_gen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  pivot_longer(cols = -c("subregion", "subregion_name"), 
               names_to = "energy_source", 
               values_to = "pct_change")

# format generation mix and merge in percent change data
subregion_resource_mix <- 
  subregion_us_comparison %>% 
  select(year, subregion, subregion_name, contains("gen"), -net_gen) %>% 
  pivot_longer(cols = contains("gen"), 
               names_to = "energy_source", 
               values_to = "generation") %>% 
  mutate(energy_source = str_replace(energy_source, "_gen", "")) 

subregion_resource_mix$energy_source <- 
  factor(subregion_resource_mix$energy_source, 
          levels = c("coal", 
                     "oil", 
                     "gas",
                     "other_fossil", 
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
  left_join(subregion_gen_comparison, by = c("subregion", "subregion_name", "energy_source")) %>% 
  select(-subregion_name)

# summarize nameplate capacity and net gen 
subregion_cap_gen <- 
  subregion_us_comparison %>% 
  select(year, subregion, subregion_name, nameplate_capacity, net_gen)


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
  select(year, state, contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("gen")) %>% 
  mutate(across(
    .cols = contains(glue::glue("gen_{(cur_year)}")), 
    .fns = ~ case_when(
      (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
      (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                        / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
      (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
    .names = "{sub('_gen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  pivot_longer(cols = -c("state"), 
               names_to = "energy_source", 
               values_to = "pct_change")

# format generation mix and merge in percent change data
state_resource_mix <- 
  state_comparison %>% 
  select(year, state, contains("gen"), -net_gen) %>% 
  pivot_longer(cols = contains("gen"), 
               names_to = "energy_source", 
               values_to = "generation") %>% 
  mutate(energy_source = str_replace(energy_source, "_gen", ""))
  

state_resource_mix_wider <- 
  state_resource_mix %>% 
  pivot_wider(names_from = year, 
              values_from = generation) %>% 
  left_join(state_gen_comparison, by = c("state", "energy_source"))

# summarize nameplate capacity and net gen 
state_cap_gen <- 
  state_comparison %>% 
  select(year, state, state_nameplate_capacity, net_gen)

