## -------------------------------
##
## Unit file QA 
## 
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the unit file creation in 2021. 
##
## The checks performed will output a CSV file with any differences 
## between Access and R unit files. 
## 
## Author: Teagan Goforth, teagan.goforth@abtglobal.com 
## 
## Date created: 8/5/2024
##
## -------------------------------


# load libraries
library(dplyr)
library(readr)
library(readxl)

# set directory for saving files 
save_dir <- "data/outputs/qa/unit_file_differences/"

# load unit file R
unit_r <- read_rds("data/outputs/unit_file.RDS")

# add "_r" after each variable to easily identify dataset 
colnames(unit_r) <- paste0(colnames(unit_r), "_r")

unit_r <- unit_r %>% rename("plant_id" = "plant_id_r", 
                            "unit_id" = "unit_id_r")

unit_access <- read_excel("archive/egrid2021_data.xlsx", sheet = "UNT21", 
                               skip = 1, 
                               guess_max = 4000) %>% janitor::clean_names() %>% 
  rename("sequnt_access" = "sequnt",
         "year_access" = "year", 
         "plant_id" = "orispl",
         "plant_name_access" = "pname", 
         "plant_state_access" = "pstatabb", 
         "unit_id" = "unitid", 
         "prime_mover_access" = "prmvr", 
         "operating_status_access" = "untopst", 
         "camd_flag_access" = "camdflag", 
         "program_code_access" = "prgcode", 
         "botfirty_access" = "botfirty", 
         "num_generators_access" = "numgen", 
         "primary_fuel_type_access" = "fuelu1", 
         "operating_hours_access" = "hrsop", 
         "heat_input_access" = "htian", 
         "heat_input_oz_access" = "htioz", 
         "nox_mass_access" = "noxan", 
         "nox_mass_oz_access" = "noxoz", 
         "so2_mass_access" = "so2an", 
         "co2_mass_access" = "co2an", 
         "hg_mass_access" = "hgan", 
         "heat_input_source_access" = "htiansrc", 
         "heat_input_oz_source_access" = "htiozsrc", 
         "nox_source_access" = "noxansrc", 
         "nox_oz_source_access" = "noxozsrc", 
         "so2_source_access" = "so2src", 
         "co2_source_access" = "co2src", 
         "hg_controls_access" = "hgsrc", 
         "so2_controls_access" = "so2ctldv", 
         "nox_controls_access" = "noxctldv", 
         "hg_controls_flag_access" = "hgctldv",
         "year_online_access" = "untyronl") %>% 
  mutate(plant_id = as.character(plant_id))

# combine the two datasets
unit_comparison <- 
  unit_r %>% 
  full_join(unit_access, by = c("plant_id", 
                                "unit_id")) 

# identify if there are any plants in R that are NOT in Access dataset
# anti_join() pulls out differences
check_diff_plant_r <- 
  unit_r %>% 
  anti_join(unit_access, by = c("plant_id", "unit_id")) %>% 
  filter(!is.na(plant_id))

if(nrow(check_diff_plant_r) > 0) {
  write_csv(check_diff_plant_r, paste0(save_dir, "check_diff_plant_r.csv")) }

# check if there are any plants in Access that are NOT in R dataset
check_diff_plant_access <- 
  unit_access %>% 
  anti_join(unit_r, by = c("plant_id", "unit_id")) %>% 
  filter(!is.na(plant_id))

if(nrow(check_diff_plant_access) > 0) {
  write_csv(check_diff_plant_access, paste0(save_dir, "check_diff_plant_access.csv")) }

# check if plant names match 
check_plant_names <- 
  unit_comparison %>% 
  filter(!(plant_name_r == plant_name_access)) %>% 
  select(plant_id, plant_name_r, plant_name_access) %>% distinct()

if(nrow(check_plant_names) > 0) {
  write_csv(check_plant_names, paste0(save_dir, "check_plant_names.csv")) }

# check if plant states match 
check_plant_state <- 
  unit_comparison %>% 
  filter(!(plant_state_r == plant_state_access)) %>% 
  select(plant_id, plant_state_r, plant_state_access) %>% distinct()

if(nrow(check_plant_state) > 0) {
  write_csv(check_plant_state, paste0(save_dir, "check_plant_state.csv")) }

# check if unit prime movers match
check_prime_mover <- 
  unit_comparison %>% 
  filter(!(prime_mover_r == prime_mover_access)) %>% 
  select(plant_id, unit_id, prime_mover_r, prime_mover_access)

if(nrow(check_prime_mover) > 0) {
  write_csv(check_prime_mover, paste0(save_dir, "check_prime_mover.csv")) }

# check operating status
check_operating_status <- 
  unit_comparison %>% 
  filter(!(operating_status_r == operating_status_access)) %>% 
  select(plant_id, unit_id, operating_status_r, operating_status_access)

if(nrow(check_operating_status) > 0) {
  write_csv(check_operating_status, paste0(save_dir, "check_operating_status.csv")) }

# check CAMD flag
check_camd_flag <- 
  unit_comparison %>% 
  filter(!(camd_flag_r == camd_flag_access)) %>% 
  select(plant_id, unit_id, camd_flag_r, camd_flag_access)

if(nrow(check_camd_flag) > 0) {
  write_csv(check_camd_flag, paste0(save_dir, "check_camd_flag.csv")) }

# check program code
check_program_code <- 
  unit_comparison %>% 
  filter(!(program_code_r == program_code_access)) %>% 
  select(plant_id, unit_id, program_code_r, program_code_access)

if(nrow(check_program_code) > 0) {
  write_csv(check_program_code, paste0(save_dir, "check_program_code.csv")) }

# check boiler firing type
check_botfirty <- 
  unit_comparison %>% 
  filter(!(botfirty_r == botfirty_access)) %>% 
  select(plant_id, unit_id, botfirty_r, botfirty_access)

if(nrow(check_botfirty) > 0) {
  write_csv(check_botfirty, paste0(save_dir, "check_botfirty.csv")) }

# check number of generators
check_num_gens <- 
  unit_comparison %>% 
  filter(!(num_generators_r == num_generators_access)) %>% 
  select(plant_id, unit_id, num_generators_r, num_generators_access)

if(nrow(check_num_gens) > 0) {
  write_csv(check_num_gens, paste0(save_dir, "check_num_gens.csv")) }

# check primary fuel type
check_fuel_type <- 
  unit_comparison %>% 
  filter(!(num_generators_r == num_generators_access)) %>% 
  select(plant_id, unit_id, num_generators_r, num_generators_access)

if(nrow(check_fuel_type) > 0) {
  write_csv(check_fuel_type, paste0(save_dir, "check_fuel_type.csv")) }

# check operating hours
check_operating_hours <- 
  unit_comparison %>% 
  filter(!(operating_hours_r == operating_hours_access)) %>% 
  select(plant_id, unit_id, operating_hours_r, operating_hours_access)

if(nrow(check_operating_hours) > 0) {
  write_csv(check_operating_hours, paste0(save_dir, "check_operating_hours.csv")) }

# check so2 controls


