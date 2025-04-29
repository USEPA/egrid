## -------------------------------
##
## Generator file create 
## 
## Purpose: 
## 
## This file creates the generator file for eGRID. 
## This includes all operating generators for the specified eGRID data year
## 
## Authors:  
##      Sean Bock, Abt Global
##      Caroline Watson, Abt Global
##      Teagan Goforth, Abt Global
##      Madeline Zhang, Abt Global
##
## -------------------------------


# Load libraries ----------

library(dplyr)
library(readr)
library(stringr)
library(glue)
library(readxl)
library(tidyverse)

# Load necessary functions
source("scripts/functions/function_check_params.R")
source("scripts/functions/function_temporal_res_cols.R")
source("scripts/functions/function_coalesce_join_vars.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year, temporal resolution, and version parameters are already defined.")
}

# Set up temporal_res_cols
temporal_res_cols <- create_temporal_res_cols("monthly") # keep as monthly for both annual and monthly versions

# Create temporal dataframe for better and more accurate joins
temporal_cols_to_add <- cols_to_add("monthly") # keep as monthly

# Load in necessary 923 and 860 files ----------

if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) { # if file does not exist, stop code and print error
  eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS")) # read in all 923 files
} else { 
   stop("eia_923_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS"))) { # if file does not exist, stop code and print error
  eia_860 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS")) # read in all 860 files
} else { 
   stop("eia_860_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

eia_923_gen <- eia_923$generator_data
eia_923_gen_fuel <- eia_923$generation_and_fuel_combined
eia_860_boiler <- eia_860$boiler_generator
eia_860_combined <- eia_860$combined %>% 
                    select(plant_id, # keeping only necessary files for to streamline joins
                           plant_name, 
                           plant_state, 
                           generator_id, 
                           prime_mover, 
                           status, 
                           energy_source_1, 
                           nameplate_capacity,
                           planned_retirement_year,
                           retirement_year,
                           operating_year)

# Load crosswalks and static tables ------------

xwalk_fuel_codes <- # xwalk for specific changes made to certain generator fuel types
  read_csv("data/static_tables/og_oth_units_to_change_fuel_type.csv", 
           col_types = "cccccccc") %>% 
  select(plant_id, fuel_code) %>% distinct()

xwalk_eia_epa <- # xwalk for updating certain plants to EPA plant names and ids
  read_csv("data/static_tables/xwalk_oris_epa.csv", 
           col_types = "cccc") # all fields are characters

epa_plants_to_delete <- 
  read_csv("data/static_tables/epa_plants_to_delete.csv", 
           col_types = cols_only(`ORIS Code` = "c")) %>% 
  janitor::clean_names() %>% 
  rename("plant_id" = oris_code)

manual_corrections <- # manual corrections needed for generator file
  read_xlsx("data/static_tables/manual_corrections.xlsx", 
            sheet = "generator_file", 
            col_types = c("text", "text", "text", "text", "text"))

# Load EPA data to update plant names to EPA versions
if(file.exists(glue::glue("data/clean_data/epa/{params$eGRID_year}/epa_clean.RDS"))) { # if file does not exist, stop code and print error
  epa <- read_rds(glue::glue("data/clean_data/epa/{params$eGRID_year}/epa_clean.RDS")) %>% 
    select(plant_id, plant_name) %>% distinct()
} else { 
  stop("epa_clean.RDS does not exist. Run data_load_epa.R and data_clean_epa.R to obtain.")}

# load in name matches for shorthand to snake_case
if(file.exists("data/static_tables/name_matches.RData")) {
  base::load("data/static_tables/name_matches.RData")
} else { 
  source("scripts/name_matching.R")
  base::load("data/static_scripts/name_matches.RData")
}

# Create lookup table for generator IDs with leading zeroes ------------
# some IDs in EIA-923 do not have leading zeroes, but should match to generators in EIA-860 that have leading zeroes
# we do this to match more generators between EIA-923 and EIA-860
# however, we want to maintain the generator IDs with leading zeroes once they are matched

plant_keep_leading_zeroes <- 
  manual_corrections %>% 
  filter(column_to_update == "keep_leading_zeroes") %>% # some plant IDs need to keep leading zeroes to avoid duplicates
  pull(plant_id)
  
eia_923_leading_zeroes <- 
  eia_923_gen %>% 
  filter(str_detect(generator_id, "^0+") & !(plant_id %in% plant_keep_leading_zeroes)) %>% 
  select(plant_id, generator_id) %>% 
  mutate(gen_id_clean = str_remove(generator_id, "^0+"), 
         id = paste0(plant_id, "_", gen_id_clean)) 

lookup_923_leading_zeroes <- with(eia_923_leading_zeroes, setNames(generator_id, id))
print(glue::glue("{length(lookup_923_leading_zeroes)} generator IDs have leading zeroes in EIA-923 Generator file. 
                 The leading zeroes are removed for matching purposes and replaced at the end of the script."))

eia_860_leading_zeroes <- 
  eia_860_combined %>% 
  filter(str_detect(generator_id, "^0+") & !(plant_id %in% plant_keep_leading_zeroes)) %>%  
  select(plant_id, generator_id) %>% 
  mutate(gen_id_clean = str_remove(generator_id, "^0+"),
         id = paste0(plant_id, "_", gen_id_clean))

lookup_860_leading_zeroes <- with(eia_860_leading_zeroes, setNames(generator_id, id))
print(glue::glue("{length(lookup_860_leading_zeroes)} generator IDs have leading zeroes in EIA-860 Combined file. 
                 The leading zeroes are removed for matching purposes and replaced at the end of the script."))

# Create modified dfs that will be used to calculate generation values ---------

gen_id_manual_corrections <- # update generator IDs
  manual_corrections %>% 
  filter(column_to_update == "generator_id" & is.na(prime_mover)) %>% 
  select(plant_id, generator_id, update)

gen_id_pm_corrections <- # update generator IDs for plant with duplicate prime movers
  manual_corrections %>% 
  filter(column_to_update == "generator_id" & !is.na(prime_mover)) %>% 
  select(plant_id, generator_id, prime_mover, update)

eia_923_gen_r <- 
  eia_923_gen %>% 
  left_join(gen_id_manual_corrections, by = c("plant_id", "generator_id")) %>% 
  left_join(gen_id_pm_corrections, by = c("plant_id", "generator_id", "prime_mover")) %>% 
  mutate(
    generator_id = if_else(!(plant_id %in% plant_keep_leading_zeroes), str_remove(generator_id, "^0+"), generator_id), # remove leading zeroes from generator IDs
    generator_id = if_else(!is.na(update.x), update.x, generator_id), 
    generator_id = if_else(!is.na(update.y), update.y, generator_id)) %>% # update generator IDs from manual_corrections)
  group_by(pick(all_of(temporal_res_cols)), # group by month and year
           plant_id,
           generator_id,
           combined_heat_and_power_plant,
           respondent_frequency
           ) %>% 
  summarize(net_generation = sum(net_generation, na.rm = TRUE),
            net_generation_year_to_date = sum(unique(net_generation_year_to_date), na.rm = TRUE)) %>% # sum generation for plants with duplicate prime movers) # keep respondent_frequency
  ungroup() # %>%
  # select(year,  # select necessary columns
  #        month, 
  #        plant_id,
  #        generator_id, 
  #        combined_heat_and_power_plant, 
  #        net_generation, 
  #        net_generation_year_to_date,
  #        respondent_frequency,
  #        prime_mover)

eia_923_gen_dups <- # check for duplicates in EIA-923 Generator File
  eia_923_gen_r %>% 
  group_by(year, plant_id, generator_id) %>% 
  mutate(n = n(),
         id = paste0(plant_id, "_", generator_id)) %>% 
  filter(n > 12, # set to greater than 12 for month duplicates
         combined_heat_and_power_plant == "Y") %>%  # default to generators with "Y" CHP plant flag
  ungroup() %>%
  left_join(eia_923_gen_r) # add back in generation data
  
eia_923_gen_r_2 <- 
  eia_923_gen_r %>% 
  filter(!(paste0(plant_id, "_", generator_id) %in% eia_923_gen_dups$id)) %>% # filter out duplicate plants 
  rbind(eia_923_gen_dups %>% select(-id, -n)) # add back in generators that were duplicated with correct non-duplicated row

eia_860_combined_r <- 
  eia_860_combined %>% 
  left_join(gen_id_manual_corrections, by = c("plant_id", "generator_id")) %>% 
  mutate(
    generator_id = if_else(!(plant_id %in% plant_keep_leading_zeroes), str_remove(generator_id, "^0+"), generator_id), # remove leading zeroes from generator IDs
    generator_id = if_else(!is.na(update), update, generator_id), # update generator IDs from manual_corrections
    retirement_year = if_else(is.na(retirement_year), planned_retirement_year, retirement_year)) %>% 
  select(-update)
    
eia_860_boiler_count <- # creating count of boilers for each generator
  eia_860_boiler %>% 
  group_by(plant_id, 
           generator_id) %>% 
  summarize(n_boilers = n()) %>% 
  ungroup()

# Determine generation ------------

## Generation from EIA-923 Generator file ------

eia_gen_generation <- eia_860_combined_r %>% 
                      cross_join(temporal_cols_to_add) %>% # join in missing temporal_cols
                      left_join(eia_923_gen_r_2 %>% # join EIA-923 Generator Data
                                  select(all_of(temporal_res_cols),
                                         plant_id, 
                                         generator_id, 
                                         combined_heat_and_power_plant, 
                                         net_generation,
                                         net_generation_year_to_date
                                         ),
                                by = c(temporal_res_cols, "plant_id", "generator_id")) %>%
                      group_by(pick(all_of(temporal_res_cols)), plant_id, generator_id, combined_heat_and_power_plant) %>% # group by month (to keep necessary data for December gen and ozone calculations)
                      mutate(generation = sum(net_generation, na.rm = TRUE), # sum generation to month
                             gen_data_source = if_else(is.na(net_generation), # label data source 
                                                       NA_character_,
                                                       "EIA-923 Generator File")) %>%
                      ungroup() %>%
                      select(-net_generation) # remove columns to prevent duplication

# check how many generators are missing generation values
missing_gen_data <- 
  eia_gen_generation %>% 
  filter(is.na(gen_data_source)) %>% 
  select(year, 
         plant_id, 
         prime_mover, 
         generator_id, 
         gen_data_source) %>% 
  distinct()

print(glue::glue("{nrow(eia_gen_generation %>% 
                          select(plant_id, generator_id) %>% 
                          distinct()) - nrow(missing_gen_data)} generators updated with generation values from direct matches to EIA-923 Generator File data.
                 {nrow(missing_gen_data)} generators without generation values remain."))

## Distribute generation to plants not in EIA-923 Generator file -------

### We create a distributional proportion based on nameplate capacity for plant/prime movers that are not in the 
### EIA-923 Generator file and distribute the generation with proportion

### Generation from EIA-923 Generation and Fuel file at the plant/prime mover level ---------
# 1. EIA-923 Generation and Fuel: Calculate generation at plant and prime mover level 
eia_gen_fuel_generation_sum <-
  eia_923_gen_fuel %>% 
  group_by(pick(all_of(temporal_res_cols)), # create generation totals per month 
           plant_id, 
           prime_mover) %>% 
  summarize(tot_generation_fuel = sum(netgen, na.rm = TRUE)) %>% # label summed generation with "fuel" for Generation and Fuel file
  ungroup()

# 2. EIA-923 Generator Data: Calculate generation at plant and prime mover level 
eia_gen_generation_sum <-
  eia_gen_generation %>%
  group_by(pick(all_of(temporal_res_cols)), # create generation totals per month
           plant_id, 
           prime_mover) %>% 
  summarize(tot_generation_gen = sum(generation, na.rm = TRUE)) %>% # label summed generation with "gen" for Generator Data file
  ungroup()

# 3. Calculate differences between EIA-923 Generator Data and EIA-923 Generation and Fuel
# Subtract the two to prevent NAs from populating data 
eia_gen_genfuel_diff <-
  eia_gen_generation_sum %>%
  left_join(eia_gen_fuel_generation_sum) %>%
  mutate(generation_diff = tot_generation_fuel - tot_generation_gen) %>%
  select(-contains("tot"))

# 4. Create proportion dataframe using nameplate capacity 
gen_distributed_props_diff <-
  eia_gen_generation %>%
  select(plant_id, 
         prime_mover,
         generator_id,
         gen_data_source,
         nameplate_capacity) %>%
  distinct() %>% # keep only distinct plants and generators
  filter(is.na(gen_data_source)) %>% 
  group_by(plant_id, prime_mover) %>%
  mutate(tot_nameplate_capacity = sum(nameplate_capacity),
         prop = if_else(tot_nameplate_capacity != 0, # the proportion of the individual generator nameplate capacity to total nameplate capacity at plant/PM level
                        nameplate_capacity / tot_nameplate_capacity,
                        NA_real_)) %>%
  ungroup() %>%
  select(-contains("nameplate_capacity"), -gen_data_source)
  
# 5. Distribute generation where there is no data using calculated proportions
gen_distributed <- 
  eia_gen_generation %>% 
  left_join(gen_distributed_props_diff) %>%
  left_join(eia_gen_genfuel_diff) %>%
  mutate(generation = generation_diff * prop,
         gen_data_source = if_else(!is.na(generation), 
                                   "Distributed from EIA-923 Generation and Fuel", 
                                   NA_character_)) # if no calculated generation, leave source as NA 

generation_df <- # flag: changed variable name, generation_df contains all plants, generators, and related data
  gen_distributed %>%
  select(-c(prop, contains("diff"))) %>% 
  bind_rows(eia_gen_generation %>% 
              filter(!is.na(gen_data_source))) # adding back 923 Generation source rows

# check how many generators are missing generation values
missing_gen_data_2 <- 
  generation_df %>% 
  filter(is.na(gen_data_source)) %>%
  select(year, 
         plant_id, 
         prime_mover, 
         generator_id) %>% 
  distinct()

print(glue::glue("{nrow(gen_distributed %>% 
                          select(plant_id, generator_id) %>% 
                          distinct())} generators updated with generation values by distributing generation by plant and prime mover from EIA-923 Generation and Fuel data.
                 {nrow(missing_gen_data_2)} generators without generation values remain."))

## December generation ------
# find plants in the EIA-923 Generator file that are using the same net generation amount in December and redistribute using GenFuel file 

# select out columns for respondent_frequency
respondent_frequency_df <-
  eia_923_gen_r_2 %>% 
  select(plant_id, 
         generator_id, 
         respondent_frequency) %>%
  unique()

# select out columns for netgen
netgen_df <-
  eia_923_gen_fuel %>%
  group_by(pick(all_of(temporal_res_cols)), plant_id, prime_mover) %>%
  summarize(netgen = sum(netgen, na.rm = TRUE),
            net_generation_megawatthours = sum(net_generation_megawatthours, na.rm = TRUE)) %>%
  ungroup()

# find generator data where December generation is equal to net_generation_year_to_date
december_gen_ids <- 
  generation_df %>%
  left_join(respondent_frequency_df, by = c("plant_id", "generator_id")) %>% # join in respondent_frequency information
  filter(respondent_frequency %in% c("A", "AM")) %>% # only calculate if annual reporter 
  group_by(year, 
           plant_id, 
           prime_mover) %>% # group by to prevent December generators from being distributed
  filter(month == 12) %>% # filter for only December months
  mutate(december_gen_flag = if_else(generation != 0 & generation == net_generation_year_to_date, # identifying cases where annual generation = December generation
                                     "yes", "no"),
         december_gen_flag = if_else(any(december_gen_flag == "yes"), # flag where any of December generators so that they are not incorrectly distributed later on
                                     "yes", "no")) %>% 
  ungroup() %>%
  select(plant_id, 
         prime_mover, 
         generator_id,
         december_gen_flag,
         respondent_frequency) %>%
  unique() %>%
  filter(december_gen_flag == "yes")

# create proportion to distribute generation for December generators
december_gen_props <-
  generation_df %>%
  right_join(december_gen_ids) %>%
  left_join(eia_gen_fuel_generation_sum) %>%
  left_join(netgen_df) %>%
  mutate(prop_netgen = if_else(net_generation_megawatthours == 0, 0,
                               netgen / net_generation_megawatthours)) %>%
  select(all_of(temporal_res_cols),
         plant_id, 
         prime_mover, 
         generator_id, 
         tot_generation_fuel, 
         prop_netgen)

december_gen <-
  generation_df %>%
  right_join(december_gen_ids) %>%
  left_join(december_gen_props) %>%
  mutate(
    # generation = tot_generation_fuel * prop, # distribute using same method of distribution instead of dividing by 12
    generation = net_generation_year_to_date * prop_netgen,
    # generation = net_generation_year_to_date / 12, # divide generation by 12 months
    gen_data_source = "Distributed from EIA-923 Generation and Fuel File") %>% # flag: created new generation data source - i.e. distributed through EIA-923
  # gen_data_source = "EIA-923 Generator File") %>% # - i.e. distributed through EIA-923
  select(plant_id,
         prime_mover,
         generator_id,
         year,
         month,
         gen_data_source,
         generation) %>%  # keeping only necessary columns
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) # creating unique idea to identify duplicates

print(glue::glue("{nrow(december_gen_ids)} generators have generation data where generation data equals December generation."))

### Determine differences between EIA-923 Generator File and EIA-923 Generation and Fuel file, and identify and distribute large cases ---------

# 1. Calculate generation total post-distribution (to plant/PM level)
generation_sum <- 
  generation_df %>%
  group_by(pick(all_of(temporal_res_cols)), 
           plant_id, 
           prime_mover) %>%
  summarize(tot_generation = sum(generation, na.rm = TRUE)) %>%
  ungroup()

# 2. Calculate generation differences and flag "overwrite" on significant plants
generation_diff <-
  generation_sum %>%
  left_join(eia_gen_fuel_generation_sum) %>%
  mutate(abs_diff_generation = abs(tot_generation_fuel - tot_generation),
         perc_diff_generation = if_else(abs_diff_generation == 0, 0, 
                                        abs_diff_generation / tot_generation_fuel), 
         overwrite = if_else(perc_diff_generation > 0.001, "overwrite", "EIA-923 Generator File"))

## Where overwrite == overwrite, we distribute the the generation figures in the EIA-923 Gen and Fuel file and 
## create a DF of generators that have large differences between EIA-923 Generator file and EIA-923 Generation and Fuel file 
## and distribute the difference with EIA-923 Generation and Fuel File generation values.
## Note that generators incorrectly end up in overwrite if they share a plant prime mover id with another generator
## that is indeed in generator file.
  
gen_overwrite <-
  generation_df %>%
  left_join(generation_diff) %>%
  #left_join(gen_distributed_props) %>%
  left_join(eia_860_combined_r %>% 
              select(plant_id, generator_id, prime_mover, nameplate_capacity)) %>% 
  group_by(plant_id, prime_mover) %>% 
  mutate(tot_nameplate_capacity = sum(nameplate_capacity),
         prop = if_else(tot_nameplate_capacity != 0, # creating proportion based on nameplate_capacity used to distribute generation across generators
                        nameplate_capacity / tot_nameplate_capacity, 
                        NA_real_)) %>% 
  ungroup() %>% 
  # anti_join(december_gen_ids) %>%
  group_by(year, plant_id, prime_mover) %>%
  filter(any(overwrite == "overwrite")) %>% # prevents data deletion of non-overwrite months in the same generator
  ungroup() %>%
  mutate(
         generation = case_when(overwrite == "overwrite" & tot_generation_fuel < tot_generation ~ generation,
                                overwrite == "overwrite" & tot_generation_fuel > tot_generation ~ tot_generation_fuel * prop,
                                TRUE ~ generation),
         gen_data_source = if_else(overwrite != "overwrite" | tot_generation_fuel < tot_generation,
                                   # "EIA-923 Generator File",
                                   gen_data_source,
                                   "Data from EIA-923 Generator File overwritten with distributed data from EIA-923 Generation and Fuel")) %>%
 select(all_of(temporal_res_cols),
        plant_id, 
        prime_mover, 
        generator_id, 
        gen_data_source, 
        generation) %>%  # reducing columns for clarity and to facilitate QA
 mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) # creating unique id to identify duplicates

print(glue::glue("{length(unique(gen_overwrite$id_pm))} generators have generation data overwritten from EIA-923 Generator file with distributed data from EIA-923 Generation and Fuel due to percent difference >0.1% between data sources."))

# Form generator file structure ------------
# check_dup_ids <-
#   december_gen %>%
#   select(id_pm) %>%
#   filter(id_pm %in% gen_overwrite$id_pm) %>%
#   distinct() %>%
#   pull(id_pm)
check_dup_ids <-
  gen_overwrite %>%
  select(id_pm) %>%
  filter(id_pm %in% december_gen$id_pm) %>%
  distinct() %>%
  pull(id_pm)

# combine set of special cases
december_and_overwritten <- 
  # bind_rows(
  #   december_gen %>% filter(!(id_pm %in% check_dup_ids)), # if generator is in both december_gen and gen_overwrite, default to gen_overwrite
  #   gen_overwrite) %>%
  bind_rows(
    gen_overwrite %>% filter(!(id_pm %in% check_dup_ids)) %>% distinct(), # if generator is in both december_gen and gen_overwrite, default to december_gen
    december_gen %>% distinct()) %>% # prevents errors in final dataframe
  left_join(eia_gen_generation %>% # merging all columns back in
            select(-c(contains("generation"), gen_data_source)),
            by = c(temporal_res_cols, "plant_id", "generator_id", "prime_mover"))

print(glue::glue("{length(check_dup_ids)} generators are in both december_gen and gen_overwrite. We default to gen_overwrite.")) # flag to change to december_gen if make change
print(glue::glue("{length(unique(december_and_overwritten$id_pm))} generator generation data are either overwritten from EIA-923 Generator and Fuel or from December generation."))

# now combining all generators 
generators_combined <- # flag: potentially change to generation_df2
  generation_df %>% 
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) %>%
  filter(!(id_pm %in% december_and_overwritten$id_pm)) %>%  #filtering out observations that are in modified df
  bind_rows(december_and_overwritten)

# check if the number of rows when combining generation_df and december_and_overwritten is correct
gen_dist_no_dec_overwritten <-
  generation_df %>%
  select(-net_generation_year_to_date) %>%
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) %>%
  filter(!(id_pm %in% december_and_overwritten$id_pm))

# flag: I believe gen_dist_no_dec_overwritten + december_and_overwritten will always equal generators_combined since it's using the same methodology (MZ 4/7/2025)
if(nrow(generators_combined) > nrow(generation_df)) { # check if there are any units with duplicate entries 
  print(glue::glue("There are {nrow(gen_dist_no_dec_overwritten)} generators that are not overwritten or use December generation data. 
                   There are {nrow(december_and_overwritten)} generators. The dataframe with all generators contains {nrow(generators_combined)} generators."))
  dup_ids <- 
    generators_combined %>%
    count(plant_id, generator_id, sort =  TRUE) %>% 
    filter(n > 12) %>% # set to 12, since monthly
    mutate(plant_gen = glue::glue("Plant:{plant_id}, Generator:{generator_id}")) %>% 
    pull(plant_gen) %>% 
    str_c(., collapse = "\n")
  
  stop(glue::glue("There are more rows than there should be in the generators_combined dataframe. There are multiple rows for the following units: {\n dup_ids}.\n Check for possible sources of duplicate generator_ids."))
} else{
  print("The number of rows in generators_combined matches the sum of generators that are overwritten, generators that use December generation, and all other generators.")
}

if (params$temporal_res == "annual") {
  ozone_months <- c(5:9)
  
  generators_combined <-
    generators_combined %>%
    group_by(pick(-c(month, generation, gen_data_source))) %>%  # group by everything except month and generation
    mutate(generation_oz = sum(generation[month %in% ozone_months], na.rm = TRUE),
           generation = sum(generation, na.rm = TRUE),
           gen_data_source = paste(unique(gen_data_source), collapse = ", ")) %>%
    ungroup() %>%
    select(-month) %>%
    distinct(plant_id, generator_id, prime_mover, .keep_all = TRUE) %>%
    mutate(gen_data_source = if_else(gen_data_source == "NA", NA_character_, gen_data_source))
}

# Update capacity factor  -----------------------------------------------

hours <- capfac_hours(params$temporal_res, params$eGRID_year)

temporal_res_cols_new <- create_temporal_res_cols(params$temporal_res) # reset temporal_res_cols to correctly calculate capfact

generators_combined2 <-
  generators_combined %>%
  left_join(hours, by = temporal_res_cols_new) %>%
  mutate(capfact = if_else(nameplate_capacity != 0, 
                           generation / (nameplate_capacity * hours), 
                           0)) %>%
  select(-hours)

# Final modifications to generator file -----------

lookup_eia_id_epa_id <- with(xwalk_eia_epa, setNames(epa_plant_id, eia_plant_id))
lookup_epa_id_name <- with(xwalk_eia_epa, setNames(epa_plant_name, epa_plant_id))

generators_edits <- 
  generators_combined2 %>% 
  left_join(xwalk_fuel_codes %>% rename(fuel_code_update = fuel_code), by = c("plant_id")) %>% 
  mutate(id = paste0(plant_id, "_", generator_id), 
         fuel_code = if_else(plant_id %in% xwalk_fuel_codes$plant_id & energy_source_1 %in% c("OG", "OTH"), fuel_code_update, energy_source_1),
         generator_id = recode(id, !!!lookup_860_leading_zeroes, .default = generator_id), # updating generator ID to add back in leading zeroes
         generator_id = recode(id, !!!lookup_923_leading_zeroes, .default = generator_id), # updating generator ID to add back in leading zeroes
         plant_id = recode(plant_id, !!!lookup_eia_id_epa_id), # updating plant_id to corresponding EPA IDs with lookup table
         plant_name = recode(plant_id, !!!lookup_epa_id_name, .default = plant_name), # updating plant_name for specific plant_ids with lookup table
         gen_data_source = if_else(is.na(generation), NA_character_, gen_data_source), # updating generation source to missing if annual generation is missing
         year = params$eGRID_year) %>%
  left_join(eia_860_boiler_count) %>% 
  rows_update(epa, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_delete(epa_plants_to_delete, by = c("plant_id"), unmatched = "ignore")

# creating named vector of final variable order and variable name included in generator file
if(params$temporal_res == "annual") {
  final_vars <- generator_nonmetric_annual}
if(params$temporal_res == "monthly") {
  final_vars <- generator_nonmetric_monthly}

generators_formatted <-
  generators_edits %>%
  arrange(plant_state, plant_name, fuel_code) %>%
  mutate(seqgen = row_number()) %>%
  select(as_tibble(final_vars)$value) %>% # keeping columns with tidy names since the rename is done in the final formatting script
  drop_na(plant_id, generator_id) %>%
  mutate(across(c(starts_with("capfac"), starts_with("generation")), ~ round(.x, 3)))

# Export generator file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
} else {
   dir.create("data/outputs")
}
 
if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
  print(glue::glue("Folder output/{params$eGRID_year} already exists."))
} else {
   dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
}

print(glue::glue("Saving generator file to folder data/outputs/{params$eGRID_year}"))

write_rds(generators_formatted, glue::glue("data/outputs/{params$eGRID_year}/generator_file_{params$temporal_res}.RDS"))

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/generator_file_{params$temporal_res}.RDS"))){
  print(glue::glue("File generator_file_{params$temporal_res}.RDS successfully written to folder data/outputs/{params$eGRID_year}"))
} else {
   print(glue::glue("File generator_file_{params$temporal_res}.RDS failed to write to folder."))
}  

  