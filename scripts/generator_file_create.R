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
source("scripts/functions/function_params_check.R")
source("scripts/functions/function_temporal_res_cols.R")
source("scripts/functions/function_coalesce_join_vars.R")

# Create and check parameters 
params <- params_check()

# Set up temporal_res_cols
temporal_res_cols <- temporal_res_cols(params$temporal_res)

# Create dataframe for better and more accurate joins
# temporal_cols_to_add <- cols_to_add(params$temporal_res) 
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
  group_by(plant_id, generator_id, combined_heat_and_power_plant, year, month) %>% # added group_by month, keep month 2.20.25
  mutate(across(contains("generation"), 
                   ~ sum(., na.rm = TRUE))) %>%# sum generation for plants with duplicate prime movers
  ungroup() %>%
  select(year,  # select necessary columns
         month, 
         plant_id,
         generator_id, 
         combined_heat_and_power_plant, 
         net_generation, 
         net_generation_year_to_date,
         respondent_frequency)

eia_923_gen_dups <- # check for duplicates in EIA-923 Generator File
  eia_923_gen_r %>% 
  group_by(plant_id, generator_id) %>% 
  mutate(n = n(),
         id = paste0(plant_id, "_", generator_id)) %>% 
  filter(n > 1, 
         combined_heat_and_power_plant == "Y") %>%  # default to generators with "Y" CHP plant flag
  ungroup()
  
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

ozone_months <- c(5:9) # creating vector for ozone month generation

eia_gen_generation <- eia_860_combined_r %>% 
                      left_join(eia_923_gen_r_2 %>% # join eia_860 and eia_923 data
                                  select(year, 
                                         month, 
                                         plant_id, 
                                         generator_id, 
                                         combined_heat_and_power_plant, 
                                         net_generation, 
                                         net_generation_year_to_date),
                                  # select(plant_id, generator_id, starts_with("net"), combined_heat_and_power_plant), # keeping only necessary columns
                                by = c("plant_id", "generator_id")) 
                      # mutate(generation_oz = rowSums(pick(all_of(ozone_months_gen)), na.rm = TRUE),
                      #        gen_data_source = if_else(is.na(net_generation_year_to_date), NA_character_, "EIA-923 Generator File"),
                      #        generation_ann = net_generation_year_to_date)

eia_gen_generation_nas <- eia_gen_generation %>% # where there are NAs for temporal_res_cols, add filler data values
                          # select(-c(contains("generation"), gen_data_source)) %>%
                          filter(is.na(year)) %>%
                          # select(-all_of(temporal_res_cols)) %>% 
                          select(-c(year, month)) %>% # removing both month and year
                          crossing(temporal_cols_to_add)

eia_gen_generation <- eia_gen_generation %>% 
                      filter(!is.na(year)) %>%
                      bind_rows(eia_gen_generation_nas)


# if temporal_res is annual or monthly, calculate generation ozone (come back to edit this 2.27.25)
# if (params$temporal_res == "annual" | params$temporal_res == "monthly") { # sum generation_oz for all? 
  
  # eia_gen_generation <-
  #   eia_gen_generation %>%
  #   group_by(year, plant_id, generator_id, combined_heat_and_power_plant) %>%
  #   mutate(generation_oz = sum(net_generation[month %in% ozone_months], na.rm = TRUE)) %>% # calculate ozone over the year
  #   ungroup() # %>%
    # mutate(year = if_else(is.na(year), params$eGRID_year, year))# adding in year values to groupby
    
# }

# calculate generation by temporal_res
eia_gen_generation <-
  eia_gen_generation %>%
  group_by(year, month, plant_id, generator_id, combined_heat_and_power_plant) %>% # group_by temporal_res_cols
  mutate(generation = sum(net_generation, na.rm = TRUE), # keep summing to month 
  # group_by(pick(all_of(temporal_res_cols)), plant_id, generator_id, combined_heat_and_power_plant) %>% # group_by temporal_res_cols
  # mutate(generation = if_else(params$temporal_res == "annual", # if annual, use net_generation_year_to_date (ask about this)
  #                             unique(net_generation_year_to_date),
  #                             sum(net_generation, na.rm = TRUE)),
         gen_data_source = if_else(is.na(net_generation) | is.na(net_generation_year_to_date), # label data source 
                                                                                               # might need to remove or condition 2.20.25
                                   NA_character_,
                                   "EIA-923 Generator File")) %>%
  ungroup() %>%
  select(-net_generation) # 2.28.25 remove columns that are causing NAs and duplication
    # mutate(year = if_else(is.na(year), params$eGRID_year, year)) # adding in year values to groupby

# check how many generators are missing generation values
missing_gen_data <- 
  eia_gen_generation %>% 
  filter(is.na(gen_data_source)) %>% # might need to update this since now have monthly to account for, groupby generator_id?
  group_by(year, month, plant_id, prime_mover, generator_id, gen_data_source) %>% 
  summarize(generation = sum(generation, na.rm = TRUE)) %>%
  ungroup()

filled_gen_data <-
  eia_gen_generation %>%
  filter(gen_data_source == "EIA-923 Generator File") %>%
  group_by(year, month, plant_id, prime_mover, generator_id, gen_data_source) %>%
  summarize(generation = sum(generation, na.rm = TRUE)) %>%
  ungroup()

# print(glue::glue("{nrow(eia_gen_generation) - nrow(missing_gen_data)} generators updated with generation values from direct matches to EIA-923 Generator File data.
#                  {nrow(missing_gen_data)} generators without generation values remain."))

print(glue::glue("{nrow(filled_gen_data)} generators updated with generation values from direct matches to EIA-923 Generator File data.
                 {nrow(missing_gen_data)} generators without generation values remain."))

## Distribute generation to plants not in EIA-923 Generator file -------

### We create a distributional proportion based on nameplate capacity for plant/prime movers that are not in the 
### EIA-923 Generator file and distribute the generation with proportion

# first calculate generation at plant/pm level for gen_fuel file 
### Generation from EIA-923 Generation and Fuel file at the plant/prime mover level ---------

# figure out ozone months later
# eia_gen_fuel_generation_sum <- 
#   eia_923_gen_fuel %>% 
#   # mutate(generation_oz = rowSums(pick(all_of(ozone_months_gen_fuel)), na.rm = TRUE)) %>% # summing generation across ozone months
#   # group_by(year, month, plant_id, prime_mover) %>% # added groupby year and month 2.20.25, can potentially do temporal res cols here
#   group_by(year, 
#            plant_id, 
#            prime_mover, 
#            combined_heat_and_power_plant, 
#            fuel_type) %>% # added fuel_type groupby here 2.21.25
#   mutate(generation_oz = sum(netgen[month %in% ozone_months], na.rm = TRUE)) %>%
#   ungroup()

# if (params$temporal_res == "annual") {
# eia_gen_fuel_generation_sum <- 
#   # eia_gen_fuel_generation_sum %>%
#   eia_923_gen_fuel %>%
#   # mutate(generation_oz = rowSums(pick(all_of(ozone_months_gen_fuel)), na.rm = TRUE)) %>% # summing generation across ozone months
#   # group_by(year, month, plant_id, prime_mover) %>% # added groupby year and month 2.20.25, can potentially do temporal res cols here
#   # group_by(year, 
#   #          plant_id, 
#   #          prime_mover, 
#   #          combined_heat_and_power_plant, 
#   #          fuel_type) %>% # added fuel_type groupby here 2.21.25
#   # mutate(generation_oz = sum(netgen[month %in% ozone_months], na.rm = TRUE)) %>%
#   # ungroup() %>%
#   select(-c(month,
#             netgen,
#             contains("consumption"),
#             quantity,
#             elec_quantity,
#             mmbtuper_unit,
#             tot_mmbtu,
#             elec_mmbtu)) %>%
#   distinct() %>% # removes doubling of totals
#   group_by(pick(all_of(temporal_res_cols)), 
#            plant_id, 
#            prime_mover) %>% # group_by temporal_res_cols
#   summarize(
#             #tot_generation_oz_fuel = sum(generation_oz, na.rm = TRUE), # ozone months total 
#             tot_generation_fuel = sum(net_generation_megawatthours, na.rm = TRUE)) %>% # net_generation_megawatthours for annual data
#                                                                                        # netgen for monthly data
#   ungroup()
#   
# } else {
  #2.21.25 need to fix this to be good for monthly
  eia_gen_fuel_generation_sum <-
   #  eia_gen_fuel_generation_sum %>% 
    eia_923_gen_fuel %>% # replace while ozone month is temp not in use
    select(-c(contains("consumption"), 
              quantity,
              elec_quantity,
              mmbtuper_unit,
              tot_mmbtu,
              elec_mmbtu)) %>%
    group_by(year, # group_by temporal_res_cols
             plant_id, 
             prime_mover,
             # combined_heat_and_power_plant,
             respondent_frequency # keep respondent_frequency for calculating generation, actually might get dropped later and not necessary
             ) %>% 
    mutate(
              #tot_generation_oz_fuel = sum(generation_oz, na.rm = TRUE), # ozone months total 
              tot_generation_fuel = sum(netgen, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-combined_heat_and_power_plant) # might be giving double/duplication
  
# }



# if (params$temporal_res == "annual") {
#   eia_gen_generation <-
#     eia_gen_generation %>%
#     select(-month) %>% # if annual, remove month to avoid duplicates 2.20.25
#     distinct() 
# }

# create proportion dataframe using nameplate capacity 
gen_distributed_props <-
  eia_gen_generation %>%
  group_by(year, plant_id, prime_mover) %>% # adding year to group_by
  # groupby distinct year
  # group_by(plant_id, prime_mover) %>%
  # mutate(tot_generation = sum(generation, na.rm = TRUE), # summing generation at plant/pm level (1)
  #           tot_generation_oz = sum(generation_oz, na.rm = TRUE)) %>% # change this to only do generation_oz for monthly/annual (1)
  summarize(tot_generation = sum(generation, na.rm = TRUE) # , # summing generation at plant/pm level (2)
            # tot_generation_oz = sum(generation_oz, na.rm = TRUE)
  ) %>% # change this to only do generation_oz for monthly/annual (2)
  ungroup() %>%
  left_join(eia_gen_fuel_generation_sum) %>% # joining with gen fuel file to compare totals
  mutate(generation_diff = tot_generation_fuel - tot_generation #, # calculating difference between gen and gen fuel file
         # generation_oz_diff = tot_generation_oz_fuel - tot_generation_oz
  ) %>% 
  select(plant_id, prime_mover, generation_diff, tot_generation, tot_generation_fuel
         # generation_oz_diff
  ) %>% # (annual ver.) (2)
  right_join(eia_gen_generation) %>% # joining back in other columns (2)
  filter(is.na(gen_data_source)) %>% # filtering to only generators with missing source
  group_by(year, plant_id, prime_mover, generator_id) %>%  # added generator_id here 2.28.25, extra group_by to avoid duplicated
  mutate(tot_nameplate_capacity = unique(nameplate_capacity)) %>%
  ungroup() %>%
  group_by(year, plant_id, prime_mover) %>%
  distinct(generator_id, .keep_all = TRUE) %>%
  mutate(tot_nameplate_capacity = sum(tot_nameplate_capacity),
         prop = if_else(tot_nameplate_capacity != 0, # creating proportion based on nameplate_capacity used to distribute generation across generators
                        nameplate_capacity / tot_nameplate_capacity,
                        NA_real_)) %>%
  ungroup() %>%
  select(plant_id, 
         prime_mover, 
         generator_id, 
         # nameplate_capacity, 
         # tot_nameplate_capacity, 
         # generation, 
         generation_diff, 
         prop)
  
# distribute generation using calculated proportions
gen_distributed <- 
  eia_gen_generation %>% 
  # # mutate(year = if_else(is.na(year), params$eGRID_year, year)) %>% # adding in year values to groupby 
  # # group_by(pick(all_of(temporal_res_cols)), plant_id, prime_mover) %>% # group_by temporal_res_cols 
  # group_by(year, plant_id, prime_mover) %>% # adding year to group_by
  # # groupby distinct year
  # # group_by(plant_id, prime_mover) %>%
  # # mutate(tot_generation = sum(generation, na.rm = TRUE), # summing generation at plant/pm level (1)
  # #           tot_generation_oz = sum(generation_oz, na.rm = TRUE)) %>% # change this to only do generation_oz for monthly/annual (1)
  # summarize(tot_generation = sum(generation, na.rm = TRUE) # , # summing generation at plant/pm level (2)
  #           # tot_generation_oz = sum(generation_oz, na.rm = TRUE)
  #           ) %>% # change this to only do generation_oz for monthly/annual (2)
  # ungroup() %>%
  # left_join(eia_gen_fuel_generation_sum) %>% # joining with gen fuel file to compare totals
  # mutate(generation_diff = tot_generation_fuel - tot_generation #, # calculating difference between gen and gen fuel file
  #       # generation_oz_diff = tot_generation_oz_fuel - tot_generation_oz
  #        ) %>% 
  # select(plant_id, prime_mover, generation_diff #, 
  #        # generation_oz_diff
  #        ) %>% # (annual ver.) (2)
  # right_join(eia_gen_generation) %>% # joining back in other columns (2)
  # filter(is.na(gen_data_source)) %>% # filtering to only generators with missing source
  # group_by(year, plant_id, prime_mover, generator_id) %>%  # added generator_id here 2.28.25, extra group_by to avoid duplicated
  # mutate(tot_nameplate_capacity = unique(nameplate_capacity)) %>%
  # ungroup() %>%
  # group_by(year, plant_id, prime_mover) %>%
  # mutate(tot_nameplate_capacity = sum(tot_nameplate_capacity),
  #         prop = if_else(tot_nameplate_capacity != 0, # creating proportion based on nameplate_capacity used to distribute generation across generators
  #                       nameplate_capacity / tot_nameplate_capacity,
  #                       NA_real_)) %>%
  # ungroup() %>%
  right_join(gen_distributed_props, by = c("plant_id", "generator_id", "prime_mover")) %>%
  # mutate(generation = if (params$temporal_res == "annual") # might need to do case_when for daily/hourly
  #                             generation_diff * prop else 
  #                             generation_diff * (prop / 12), # multiplying differences by proportion value
  mutate(generation = generation_diff * (prop / 12),
         # generation_oz = generation_oz_diff * prop,
         gen_data_source = if_else(!is.na(generation), "Distributed from EIA-923 Generation and Fuel", NA)) %>% # if no calculated generation, leave source as NA 
  # (changed from !is.na(generation_oz) to !is.na(generation) since daily has no generation_oz)
        #  gen_data_source = "Distributed from EIA-923 Generation and Fuel") %>%
  bind_rows(eia_gen_generation %>% 
              filter(!is.na(gen_data_source))) # adding back 923 Generation source rows


# check how many generators are missing generation values
missing_gen_data_2 <- 
  gen_distributed %>% 
  filter(is.na(gen_data_source)) %>%
  group_by(year, month, plant_id, prime_mover, generator_id) %>%
  summarize(generation = sum(generation, na.rm = TRUE)) %>%
  ungroup()

filled_gen_data_2 <-
  gen_distributed %>%
  filter(gen_data_source == "Distributed from EIA-923 Generation and Fuel") %>%
  group_by(year, month, plant_id, prime_mover, generator_id, gen_data_source) %>%
  summarize(generation = sum(generation, na.rm = TRUE)) %>%
  ungroup()

# print(glue::glue("{nrow(gen_distributed) - (nrow(eia_gen_generation) - nrow(missing_gen_data))} generators updated with generation values by distributing generation by plant and prime mover from EIA-923 Generation and Fuel data.
#                  {nrow(missing_gen_data_2)} generators without generation values remain."))
print(glue::glue("{nrow(filled_gen_data_2)} generators updated with generation values by distributing generation by plant and prime mover from EIA-923 Generation and Fuel data.
                 {nrow(missing_gen_data_2)} generators without generation values remain."))


### Determine differences between EIA-923 Generator File and EIA-923 Generation and Fuel file, and identify and distribute large cases --------- 

cols_to_keep <- # could remove this, only used for one life of code 2.25.25
    c("plant_id",
      "prime_mover",
      "overwrite",
      temporal_res_cols)

eia_gen_genfuel_diff <- 
  gen_distributed %>% 
  # group_by(plant_id, prime_mover) %>% # (1)
  group_by(year, plant_id, prime_mover) %>% # (2) added groupby temporal_res_cols
  summarize(tot_generation_gen = sum(generation, na.rm = TRUE) #, # summing generation to plant/pm level to compare to gen_fuel file
            # tot_generation_oz_gen = sum(generation_oz, na.rm = TRUE)
            ) %>%
  ungroup() %>%
  left_join(eia_gen_fuel_generation_sum, by = c("plant_id", "prime_mover", "year")) %>% # joining with gen_fuel file (2.28.25 added temporal res cols)
  mutate(abs_diff_generation = abs(tot_generation_fuel - tot_generation_gen), # calculating absolute differences between generation values
         # abs_diff_generation_oz = abs(tot_generation_oz_fuel - tot_generation_oz_gen), # calculating the percentage of the difference over the fuel levels in gen_fuel file
         perc_diff_generation = if_else(abs_diff_generation == 0, 0, 
                                        abs_diff_generation / tot_generation_fuel), 
         # perc_diff_generation_oz = if_else(abs_diff_generation_oz == 0, 0,
         #                                   abs_diff_generation_oz / tot_generation_oz_fuel), # calculating percent differences for monthly generation and generation ozone
         overwrite = if_else(perc_diff_generation > 0.001, "overwrite", "EIA-923 Generator File")) %>% # , # (1) flag to overwrite for generation if difference is greater than .1% 
         # overwrite = if_else(perc_diff_generation_oz > 0.001, "overwrite", "EIA-923 Generator File")) %>% # (2) flag to overwrite for generation_oz 
  filter(tot_generation_fuel != 0) %>% 
  select(all_of(cols_to_keep), starts_with("tot"), starts_with("abs"), starts_with("perc"))

## Where overwrite == overwrite, we distribute the the generation figures in the EIA-923 Gen and Fuel file and 
## create a DF of generators that have large differences between EIA-923 Generator file and EIA-923 Generation and Fuel file 
## and distribute the difference with EIA-923 Generation and Fuel File generation values.
## Note that generators incorrectly end up in overwrite if they share a plant prime mover id with another generator
## that is indeed in generator file.

# creating vector of column names that are essentials for modified dataframes. 
# These are used to reduce clutter in dfs before they are combined in final structure below. 

# if (params$temporal_res == "annual") {
  key_columns <-
    c("plant_id",
      "prime_mover",
      temporal_res_cols,
      "generator_id",
     # "generation_ann",
      "generation",
      # "generation_oz", # remove for temporary
      "gen_data_source")
# }
  
gen_overwrite <- 
  eia_gen_genfuel_diff %>% 
  select(-temporal_res_cols) %>% # deselect temporal res cols that will interfere with joining
  left_join(eia_860_combined %>% # join with EIA-860 data, only keeping necessary columns
            select(plant_id, generator_id, prime_mover, nameplate_capacity)) %>% 
  distinct() %>%
  group_by(plant_id, prime_mover, generator_id) %>% 
  mutate(tot_nameplate_capacity = unique(nameplate_capacity)) %>%
  ungroup() %>%
  group_by(plant_id, prime_mover) %>%
  mutate(tot_nameplate_capacity = sum(nameplate_capacity),
         prop = if_else(tot_nameplate_capacity != 0, # creating proportion based on nameplate_capacity used to distribute generation across generators
                        nameplate_capacity / tot_nameplate_capacity, 
                        NA_real_)) %>% 
  ungroup() %>% 
  right_join(eia_gen_genfuel_diff) %>% # join back other columns
  filter(overwrite == "overwrite") %>% 
  mutate(generation = tot_generation_fuel * (prop / 12),
         # generation_oz = tot_generation_oz_fuel * prop,
         # across(paste0("tot_netgen_", tolower(month.name)),
         #        .fns = ~ . * prop,
         #        .names = "{gsub('tot_', '', col)}"),
         gen_data_source = "Data from EIA-923 Generator File overwritten with distributed data from EIA-923 Generation and Fuel") %>% 
  select(any_of(key_columns), overwrite) %>%  # reducing columns for clarity and to facilitate QA
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) # creating unique id to identify duplicates
 
# gen_overwrite_test <-
#   gen_overwrite %>%
#   group_by(year, plant_id, prime_mover, generator_id) %>%
#   summarize(generation = sum(unique(generation), na.rm = TRUE))
  
print(glue::glue("{length(unique(gen_overwrite$id_pm))} generators have generation data overwritten from EIA-923 Generator file with distributed data from EIA-923 Generation and Fuel due to percent difference >0.1% between data sources."))

## December generation ------
# find plants in the EIA-923 Generator file that are using the same net generation amount in December and redistribute using GenFuel file 
# these plants incorrectly have the total generation in the column net_generation_december

# if (params$temporal_res == "monthly") {
#   key_columns <- 
#     c(key_columns,
#       paste0("tot_netgen_", tolower(month.name)),
#       "prop")
# }
respondent_frequency <- # select out columns for respondent_frequency
  eia_923_gen_r_2 %>% 
  select(plant_id, generator_id, respondent_frequency) %>%
  unique()

december_gen_ids <- 
  gen_distributed %>%
  left_join(respondent_frequency, by = c("plant_id", "generator_id")) %>% # ideally after this join - either filter by A, M, AM or use case when in mutate code
  group_by(year, plant_id, prime_mover) %>% # add groupby here 
  filter(month == 12) %>% # filter for only December months
  # proposed change: generation_ann_dec_equal to december_gen_flag?
  mutate(generation_ann_dec_equal = if_else(generation != 0 & generation == net_generation_year_to_date, # (1) identifying cases where annual generation = december generation
                                            "yes", "no")) %>% 
  ungroup() %>%
  # filter(generation_ann_dec_equal == "yes") %>%
  select(plant_id, 
         prime_mover, 
         generator_id,
         generation_ann_dec_equal,
         respondent_frequency) %>%
  unique()

# need to handle this for both monthly and annual, maybe make ozone calculations after? ? ? not sure 
december_gen <-
  gen_distributed %>%
  # mutate(generation_ann_dec_equal = if_else(net_generation_december != 0 & # (2) identifying cases where annual generation = december generation
  #                                           net_generation_december == net_generation_year_to_date,
  #                                           "yes", "no")) %>% 
  left_join(december_gen_ids) %>%
  left_join(eia_gen_fuel_generation_sum,  # pulling in EIA gen fuel data
            by = c("plant_id", "prime_mover", "year", "month")) %>%
  # coalesce_join_vars() %>%
  group_by(plant_id, prime_mover) %>%
  mutate(tot_nameplate_capacity = sum(nameplate_capacity, na.rm = TRUE),
         prop = if_else(tot_nameplate_capacity != 0, # creating proportion based on nameplate_capacity used to distribute generation across generators
                        nameplate_capacity / tot_nameplate_capacity,
                        NA_real_)) %>%
  ungroup() %>%
  filter(generation_ann_dec_equal == "yes" & grepl("A", respondent_frequency)) %>%
  # filter for December generators and where respondent_Frequency is an annual reporter
  
  mutate(
    # generation = if (params$temporal_res == "annual") # might need to do case_when for daily/hourly
    #              net_generation_year_to_date  else 
    #              tot_generation_fuel * (prop / 12), 
     generation = tot_generation_fuel * (prop / 12), # distribute using same method instead of dividing by 12?
   # generation = net_generation_year_to_date / 12, # divide generation by 12 months
    # generation_oz = tot_generation_oz_fuel * prop,
    gen_data_source = "EIA-923 Generator File") %>% # create a new gen data source? - i.e. distributed through EIA-923
 #  mutate(generation = net_generation_year_to_date / 12)
  select(any_of(key_columns), 
         all_of(temporal_res_cols),
         generation_ann_dec_equal, 
         net_generation_year_to_date) %>%  # keeping only necessary columns
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id))# creating unique idea to identify duplicates

# test_dec_netgen <- december_gen %>% # small testing chunk to see if calculated monthly generation sums up to 
#                    group_by(plant_id, prime_mover, year) %>%
#                    mutate(generation = sum(generation, na.rm = FALSE))


print(glue::glue("{length(unique(december_gen$id_pm))} generators have generation data where generation data equals December generation."))

# create if all monthly generation values are 0, but annual net generation > 0, replace 0 with NA


# Form generator file structure ------------
check_dup_ids <-  
  december_gen %>% 
  filter(id_pm %in% gen_overwrite$id_pm) %>% 
  pull(id_pm)

# combine set of special cases
december_and_overwritten <- 
  bind_rows(
    december_gen %>% filter(!(id_pm %in% check_dup_ids)), # if generator is in both december_netgen and gen_overwrite, default to gen_overwrite
    gen_overwrite) %>% 
  left_join(eia_gen_generation %>% # merging all columns back in
            select(-c(contains("generation"), gen_data_source)),
            by = c("plant_id", "generator_id", "prime_mover", temporal_res_cols)) # %>%
 # distinct(id_pm, .keep_all = TRUE)

# test <- gen_overwrite %>% # view outputs to compare with development version, remove if not needed anymore 
#         distinct(id_pm, .keep_all = TRUE)


print(glue::glue("{length(check_dup_ids)} generators are in both december_netgen and gen_overwrite. We default to gen_overwrite."))
print(glue::glue("{nrow(december_and_overwritten)} generator generation data are either overwritten from EIA-923 Generator and Fuel or from December generation."))

# now combining all generators 
generators_combined <- 
  gen_distributed %>% 
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) %>%
  filter(!(id_pm %in% december_and_overwritten$id_pm)) %>%  #filtering out observations that are in modified df
  bind_rows(december_and_overwritten)

# check if the number of rows when combining gen_distributed and december_and_overwritten is correct
gen_dist_no_dec_overwritten <- 
  gen_distributed %>% 
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) %>%
  filter(!(id_pm %in% december_and_overwritten$id_pm))

if(nrow(generators_combined) > (nrow(gen_dist_no_dec_overwritten) + nrow(december_and_overwritten))) { # check if there are any units with duplicate entries 
  print(glue::glue("There are {nrow(gen_dist_no_dec_overwritten)} generators that are not overwritten or use December generation data. 
                   There are {nrow(december_and_overwritten)} generators. The dataframe with all generators {nrow(generators_combined)} generators."))
  dup_ids <- 
    generators_combined %>%
    count(plant_id, generator_id, sort =  TRUE) %>% 
    filter(n > 1) %>% 
    mutate(plant_gen = glue::glue("Plant :{plant_id}, Generator: {generator_id}")) %>% 
    pull(plant_gen) %>% 
    str_c(., collapse = "\n")
  
  stop(glue::glue("There are more rows than there should be in the generators_combined dataframe. There are multiple rows for the following units: {\n dup_ids}.\n Check for possible sources of duplicate generator_ids."))
} else{
  print("The number of rows in generators_combined matches the sum of generators that are overwritten, generators that use December generation, and all other generators.")
}

if (params$temporal_res == "annual") {
  generators_combined <-
    generators_combined %>%
    # mutate(test = 1) %>%
    group_by(year, plant_id, generator_id, prime_mover) %>%
    mutate(generation = sum(generation, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-month) %>%
    distinct() #??? 
}

# Update capacity factor  -----------------------------------------------
hours <- capfac_hours(params$temporal_res, params$eGRID_year)

generators_combined2 <-
  generators_combined %>%
  left_join(hours, by = temporal_res_cols) %>%
  mutate(capfac = if_else(nameplate_capacity != 0, 
                          generation / (nameplate_capacity * hours), 
                          0)) %>%
  select(-hours)


# Final modifications to generator file -----------

# creating lookup tables based on xwalk to use with recode() 
#lookup_fuel_codes <- with(xwalk_fuel_codes, setNames(fuel_code, id))

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
         year = params$eGRID_year) %>%#,
         #capfact = if_else(nameplate_capacity != 0, generation_ann / (nameplate_capacity * 8760), 0)) %>%  # calculating capacity factor
  left_join(eia_860_boiler_count) %>% 
  rows_update(epa, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_delete(epa_plants_to_delete, by = c("plant_id"), unmatched = "ignore")


# creating named vector of final variable order and variable name included in generator file
final_vars <-
    c("SEQGEN" = "seqgen",
      "YEAR" = "year",
      "PSTATABB" = "plant_state",
      "PNAME" = "plant_name",
      "ORISPL" = "plant_id",
      "GENID" = "generator_id",
      "NUMBLR" = "n_boilers",
      "GENSTAT" = "status",
      "PRMVR" =  "prime_mover",
      "FUELG1" = "fuel_code",
      "NAMEPCAP" = "nameplate_capacity",
      "CFACT" = "capfac", 
      "GENNTAN" = "generation", # rename GENNTAN? 
      # "GENNTOZ" = "generation_oz",
      "GENERSRC" = "gen_data_source",
      "GENYRONL" = "operating_year",
      "GENYRRET" = "retirement_year")

if (params$temporal_res == "monthly") { # add monthly column
  final_vars <-
    c(final_vars, 
    "MONTH" = "month") # added for monthly data
}


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
  print(glue::glue("File generator_file.RDS successfully written to folder data/outputs/{params$eGRID_year}"))
} else {
   print("File generator_file.RDS failed to write to folder.")
}  

  