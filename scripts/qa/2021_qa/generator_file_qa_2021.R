## -------------------------------
##
## Generator file QA 
## 
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the generator file creation in 2021. 
##
## The checks performed will output a CSV file with any differences 
## between Access and R generator files. 
## 
## Author: Teagan Goforth
## 
## Date created: 7/29/2024 
##
## -------------------------------


# load libraries and data ------
library(dplyr)
library(readr)
library(stringr)
library(glue)
library(readxl)

# create and set directory for saving files 

if(dir.exists("data/outputs/qa")) {
  print("Folder qa already exists.")
}else{
  dir.create("data/outputs/qa")
}

if(dir.exists("data/outputs/qa/generator_file_differences/2023")) {
  print("Folder generator_file_differences already exists.")
}else{
  dir.create("data/outputs/qa/generator_file_differences/2023")
}

save_dir <- "data/outputs/qa/generator_file_differences/2023/"

# load R dataset
generator_r <- read_rds("data/outputs/generator_file.RDS") 

# add "_r" after each variable to easily identify dataset 
colnames(generator_r) <- paste0(colnames(generator_r), "_r")

generator_r <- 
  generator_r %>% 
  rename("plant_id" = "plant_id_r", 
         "generator_id" = "generator_id_r") %>% select(-seqgen_r)

# load access dataset (from published 2021 eGRID excel sheet)  
generator_access <- read_excel("data/raw_data/eGRID_Data2023.xlsx", sheet = "GEN23", 
                               skip = 1, 
                               guess_max = 4000) %>% janitor::clean_names() %>% 
  select(-seqgen23) %>% 
  rename("year_access" = "year", 
         "plant_id" = "orispl",
         "plant_name_access" = "pname", 
         "plant_state_access" = "pstatabb", 
         "generator_id" = "genid", 
         "n_boilers_access" = "numblr", 
         "status_access" = "genstat", 
         "prime_mover_access" = "prmvr", 
         "fuel_code_access" = "fuelg1", 
         "nameplate_capacity_access" = "namepcap", 
         "capfact_access" = "cfact", 
         "generation_ann_access" = "genntan", 
         "generation_oz_access" = "genntoz", 
         "gen_data_source_access" = "genersrc", 
         "operating_year_access" = "genyronl", 
         "retirement_year_access" = "genyrret") %>% 
  mutate(plant_id = as.character(plant_id), 
         gen_data_source_access = if_else(gen_data_source_access == "Distributed from 923 Generation And Fuel", 
                                          "Distributed from EIA-923 Generation and Fuel", # updating capitalization difference in generation source
                                          gen_data_source_access), 
         n_boilers_access = as.integer(n_boilers_access), 
         operating_year_access = as.character(operating_year_access), 
         retirement_year_access = as.character(retirement_year_access)) 

# merge generator files from Access and R -------
gen_comparison <- 
  generator_r %>% 
  full_join(generator_access, by = c("plant_id", 
                                     "generator_id")) 

# check if any plants in R that are NOT in Access data set ---------
check_diff_plant_r <- 
  generator_r %>% 
  anti_join(generator_access, by = c("plant_id", "generator_id")) %>% 
  filter(!is.na(plant_id))

if(nrow(check_diff_plant_r) > 0) {
  write_csv(check_diff_plant_r, paste0(save_dir, "check_diff_plant_r.csv")) }

# check if any plants or in Access that are NOT in R data set ----------
check_diff_plant_access <- 
  generator_access %>% 
  anti_join(generator_r, by = c("plant_id", "generator_id")) %>% 
  filter(!is.na(plant_id))

if(nrow(check_diff_plant_access) > 0) {
  write_csv(check_diff_plant_access, paste0(save_dir, "check_diff_plant_access.csv")) }

# check if plant names match --------
check_plant_names <- 
  gen_comparison %>% 
  filter(mapply(identical, plant_name_r, plant_name_access) == FALSE) %>% 
  select(plant_id, plant_name_r, plant_name_access) %>% distinct()

if(nrow(check_plant_names) > 0) {
  write_csv(check_plant_names, paste0(save_dir, "check_plant_names.csv")) }
 
# check if plant states match --------------
check_plant_state <- 
  gen_comparison %>% 
  filter(mapply(identical, plant_state_r, plant_state_access) == FALSE) %>% 
  select(plant_id, plant_state_r, plant_state_access) %>% distinct()

if(nrow(check_plant_state) > 0) {
  write_csv(check_plant_state, paste0(save_dir, "check_plant_state.csv")) }

# check if number of boilers match --------------
check_n_boilers <- 
  gen_comparison %>% 
  filter(mapply(identical, n_boilers_r, n_boilers_access) == FALSE) %>% 
  select(plant_id, generator_id, n_boilers_r, n_boilers_access)

if(nrow(check_n_boilers) > 0) {
  write_csv(check_n_boilers, paste0(save_dir, "check_n_boilers.csv")) }

# check if operating status matches ---------------
check_status <- 
  gen_comparison %>% 
  filter(mapply(identical, status_r, status_access) == FALSE) %>% 
  select(plant_id, generator_id, status_r, status_access)

if(nrow(check_status) > 0) {
  write_csv(check_status, paste0(save_dir, "check_status.csv")) }

# check if prime mover matches -----------
check_prime_mover <- 
  gen_comparison %>% 
  filter(mapply(identical, prime_mover_r, prime_mover_access) == FALSE) %>% 
  select(plant_id, generator_id, prime_mover_r, prime_mover_access)

if(nrow(check_prime_mover) > 0) {
  write_csv(check_prime_mover, paste0(save_dir, "check_prime_mover.csv")) }

# check if fuel types match --------------
check_fuel_type <- 
  gen_comparison %>% 
  filter(mapply(identical, fuel_code_r, fuel_code_access) == FALSE) %>% 
  select(plant_id, generator_id, fuel_code_r, fuel_code_access)

if(nrow(check_fuel_type) > 0) {
  write_csv(check_fuel_type, paste0(save_dir, "check_fuel_type.csv")) }

# check if nameplate capacity matches --------------
check_nameplate_capacity <- 
  gen_comparison %>% 
  filter(mapply(identical, nameplate_capacity_r, nameplate_capacity_access) == FALSE) %>% 
  select(plant_id, generator_id, nameplate_capacity_r, nameplate_capacity_access)

if(nrow(check_nameplate_capacity) > 0) {
  write_csv(check_nameplate_capacity, paste0(save_dir, "check_nameplate_capacity.csv")) }

# check if capacity factor matches ----------
check_capacity_factor <- 
  gen_comparison %>% 
  filter(mapply(identical, capfact_r, capfact_access) == FALSE) %>% 
  select(plant_id, generator_id, fuel_code_r, nameplate_capacity_r, generation_ann_r, capfact_r, 
         fuel_code_access, nameplate_capacity_access, generation_ann_access, capfact_access)

if(nrow(check_capacity_factor) > 0) {
  write_csv(check_capacity_factor, paste0(save_dir, "check_capacity_factor.csv")) }

# check if year online matches ------------
check_year_online <- 
  gen_comparison %>% 
  filter(mapply(identical, operating_year_r, operating_year_access) == FALSE) %>% 
  select(plant_id, generator_id, operating_year_r, operating_year_access)

if(nrow(check_year_online) > 0) {
  write_csv(check_year_online, paste0(save_dir, "check_year_online.csv")) }

# check if year retired matches ------------
check_year_retired <- 
  gen_comparison %>% 
  filter(mapply(identical, retirement_year_r, retirement_year_access) == FALSE) %>% 
  select(plant_id, generator_id, retirement_year_r, retirement_year_access)

if(nrow(check_year_retired) > 0) {
  write_csv(check_year_retired, paste0(save_dir, "check_year_retired.csv")) }

# check the sum of total and ozone generation ------------
check_generation <- 
  gen_comparison %>% 
  summarize(total_gen_ann_r = sum(generation_ann_r, na.rm = TRUE), 
            total_gen_ann_access = sum(generation_ann_access, na.rm = TRUE), 
            total_gen_oz_r = sum(generation_oz_r, na.rm = TRUE), 
            total_gen_oz_access = sum(generation_oz_access, na.rm = TRUE), 
            gen_ann_diff = total_gen_ann_r - total_gen_ann_access, 
            gen_oz_diff = total_gen_oz_r - total_gen_oz_access)

if(!(check_generation$gen_ann_diff == 0) | !(check_generation$gen_oz_diff == 0)) {
  write_csv(check_generation, paste0(save_dir, "check_generation.csv")) }

# check annual generation by generator -------------
check_annual_generation_by_generator <- 
  gen_comparison %>% 
  mutate(generation_ann_r = round(generation_ann_r, 0), 
         generation_ann_access = round(generation_ann_access, 0)) %>% 
  filter(mapply(identical, generation_ann_r, generation_ann_access) == FALSE) %>% 
  mutate(diff_ann = generation_ann_r - generation_ann_access) %>% 
  filter(diff_ann > 1 | diff_ann < -1 | is.na(diff_ann)) %>% 
  select(plant_id, generator_id, generation_ann_r, generation_ann_access, diff_ann, 
         gen_data_source_r, gen_data_source_access)

if(nrow(check_annual_generation_by_generator) > 0) {
  write_csv(check_annual_generation_by_generator, paste0(save_dir, "check_annual_generation_by_generator.csv")) }

# check ozone generation by generator -------------
check_ozone_generation_by_generator <- 
  gen_comparison %>% 
  mutate(generation_oz_r = round(generation_oz_r, 0), 
         generation_oz_access = round(generation_oz_access, 0)) %>% 
  filter(mapply(identical, generation_oz_r, generation_oz_access) == FALSE) %>% 
  mutate(diff_oz = generation_oz_r - generation_oz_access) %>% 
  filter(diff_oz > 1 | diff_oz < -1 | is.na(diff_oz)) %>% 
  select(plant_id, generator_id, 
         generation_oz_r, generation_oz_access, diff_oz, 
         gen_data_source_r, gen_data_source_access)

if(nrow(check_ozone_generation_by_generator) > 0) {
  write_csv(check_ozone_generation_by_generator, paste0(save_dir, "check_ozone_generation_by_generator.csv")) }

# check if generation source matches -------------
check_gen_source <- 
  gen_comparison %>% 
  filter(mapply(identical, gen_data_source_r, gen_data_source_access) == FALSE) %>% 
  select(plant_id, generator_id, gen_data_source_access, gen_data_source_r)

if(nrow(check_gen_source) > 0) {
  write_csv(check_gen_source, paste0(save_dir, "check_generation_source.csv"))}

# Identify all unique plant and unit IDs that have differences ------------

files <- grep("check", dir("data/outputs/qa/generator_file_differences"), value = TRUE)

plant_gen_diffs <- 
  purrr::map_df(paste0("data/outputs/qa/generator_file_differences/", files), 
                read.csv) %>% 
  select(plant_id, generator_id) %>% 
  distinct() %>% 
  mutate(source_diff = "gen_file")

write_csv(plant_gen_diffs, "data/outputs/qa/generator_file_differences/plant_gen_difference_ids.csv")

