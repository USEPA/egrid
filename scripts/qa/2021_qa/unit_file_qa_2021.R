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
library(stringr)


# create save directory 

if(dir.exists("data/outputs/qa")) {
  print("Folder qa already exists.")
}else{
  dir.create("data/outputs/qa")
}

if(dir.exists("data/outputs/qa/unit_file_differences")) {
  print("Folder unit_file_differences already exists.")
}else{
  dir.create("data/outputs/qa/unit_file_differences")
}

# set directory for saving files 
save_dir <- "data/outputs/qa/unit_file_differences/"

# load unit file R
unit_r <- read_rds("data/outputs/unit_file.RDS")

# add "_r" after each variable to easily identify dataset 
colnames(unit_r) <- paste0(colnames(unit_r), "_r")

unit_r <- unit_r %>% rename("plant_id" = "plant_id_r", 
                            "unit_id" = "unit_id_r")

unit_access <- read_excel("archive/access_unit_file_8_26_24.xlsx", sheet = "Unit_File", 
                               #skip = 1, 
                               guess_max = 4000) %>% janitor::clean_names() %>% 
  rename("sequnt_access" = "sequnt",
         #"year_access" = "year", 
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
         "nox_oz_mass_access" = "noxoz", 
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
  mutate(plant_id = as.character(plant_id), 
         program_code_access = gsub(",([[:alpha:]])", ", \\1", program_code_access)) # adding space between listed program codes

# combine the two datasets
unit_comparison <- 
  unit_r %>% 
  full_join(unit_access, by = c("plant_id", 
                                "unit_id")) 

# Column by column checks -------------
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
  separate_wider_delim(program_code_access, delim = ",", names = c("program_1_access", 
                                                                   "program_2_access", 
                                                                   "program_3_access", 
                                                                   "program_4_access", 
                                                                   "program_5_access", 
                                                                   "program_6_access"), 
                       too_few = "align_start", too_many = "error") %>% # fix delimiter to match R 
  unite("program_code_access", c(program_1_access, program_2_access, program_3_access, 
                                 program_4_access, program_5_access, program_6_access), 
        sep = ",", na.rm = TRUE) %>% 
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
  filter(!(primary_fuel_type_r == primary_fuel_type_access)) %>% 
  select(plant_id, unit_id, prime_mover_r, prime_mover_access, primary_fuel_type_r, primary_fuel_type_access)

if(nrow(check_fuel_type) > 0) {
  write_csv(check_fuel_type, paste0(save_dir, "check_fuel_type.csv")) }

# check operating hours
check_operating_hours <- 
  unit_comparison %>% 
  filter(!(operating_hours_r == operating_hours_access)) %>% 
  select(plant_id, unit_id, operating_hours_r, operating_hours_access)

if(nrow(check_operating_hours) > 0) {
  write_csv(check_operating_hours, paste0(save_dir, "check_operating_hours.csv")) }

# check SO2 controls
check_so2_controls <- 
  unit_comparison %>% 
  filter(!(so2_controls_r == so2_controls_access)) %>% 
  select(plant_id, unit_id, so2_controls_r, so2_controls_access)

if(nrow(check_so2_controls) > 0) {
  write_csv(check_so2_controls, paste0(save_dir, "check_so2_controls.csv")) }

# check NOx controls
check_nox_controls <- 
  unit_comparison %>% 
  filter(!(nox_controls_r == nox_controls_access)) %>% 
  select(plant_id, unit_id, nox_controls_r, nox_controls_access)

if(nrow(check_nox_controls) > 0) {
  write_csv(check_nox_controls, paste0(save_dir, "check_nox_controls.csv")) }

# check Hg controls flag
check_hg_flag <- 
  unit_comparison %>% 
  filter(!(hg_controls_flag_r == hg_controls_flag_access)) %>% 
  select(plant_id, unit_id, hg_controls_flag_r, hg_controls_flag_access)

if(nrow(check_hg_flag) > 0) {
  write_csv(check_hg_flag, paste0(save_dir, "check_hg_controls_flag.csv")) }

# check year online
check_year_online <- 
  unit_comparison %>% 
  filter(!(year_online_r == year_online_access)) %>% 
  select(plant_id, unit_id, year_online_r, year_online_access)

if(nrow(check_year_online) > 0) {
  write_csv(check_year_online, paste0(save_dir, "check_year_online.csv")) }

# Heat input checks ---------
# compare sums of heat input annual and ozone season
check_total_heat_input <- 
  unit_comparison %>% 
  summarize(sum_heat_input_r = sum(heat_input_r, na.rm = TRUE), 
            sum_heat_input_access = sum(heat_input_access, na.rm = TRUE), 
            sum_heat_input_oz_r = sum(heat_input_oz_r, na.rm = TRUE), 
            sum_heat_input_oz_access = sum(heat_input_oz_access, na.rm = TRUE)) %>% 
  select(contains("sum")) %>% 
  mutate(diff_heat_input = sum_heat_input_r - sum_heat_input_access, 
         diff_heat_input_oz = sum_heat_input_oz_r - sum_heat_input_oz_access)
  
if(!(check_total_heat_input$diff_heat_input == 0) | !(check_total_heat_input$diff_heat_input_oz == 0)) {
  write_csv(check_total_heat_input %>% select(sum_heat_input_r, sum_heat_input_access, diff_heat_input, 
                                        sum_heat_input_oz_r, sum_heat_input_oz_access, diff_heat_input_oz), 
            paste0(save_dir, "check_total_heat_input.csv")) }

# check heat input at unit level 
# annual heat input check 
check_heat_input_ann_unit <- 
  unit_comparison %>% 
  filter(!(heat_input_r == heat_input_access)) %>% 
  mutate(diff_heat_input = heat_input_r - heat_input_access) %>% 
  filter(diff_heat_input > 1 | diff_heat_input < -1) %>% 
  select(plant_id, unit_id, heat_input_r, heat_input_access, diff_heat_input, 
         heat_input_source_r, heat_input_source_access)

if(nrow(check_heat_input_ann_unit) > 0) {
  write_csv(check_heat_input_ann_unit, paste0(save_dir, "check_heat_input_ann_unit.csv")) }

# ozone season heat input check 
check_heat_input_oz_unit <- 
  unit_comparison %>% 
  filter(!(heat_input_oz_r == heat_input_oz_access)) %>% 
  mutate(diff_heat_input_oz = heat_input_oz_r - heat_input_oz_access) %>% 
  filter(diff_heat_input_oz > 1 | diff_heat_input_oz < -1) %>% 
  select(plant_id, unit_id, heat_input_oz_r, heat_input_oz_access, diff_heat_input_oz, 
         heat_input_oz_source_r, heat_input_oz_source_access)

if(nrow(check_heat_input_oz_unit) > 0) {
  write_csv(check_heat_input_oz_unit, paste0(save_dir, "check_heat_input_oz_unit.csv")) }

# check heat input sources
# annual heat input source
check_heat_input_ann_source <- 
  unit_comparison %>% 
  filter(!(heat_input_source_r == heat_input_source_access)) %>% 
  select(plant_id, unit_id, heat_input_r, heat_input_access, heat_input_source_r, heat_input_source_access)

if(nrow(check_heat_input_ann_source) > 0) {
  write_csv(check_heat_input_ann_source, paste0(save_dir, "check_heat_input_ann_source.csv")) }

# ozone heat input source 
check_heat_input_oz_source <- 
  unit_comparison %>% 
  filter(!(heat_input_oz_source_r == heat_input_oz_source_access)) %>% 
  select(plant_id, unit_id, heat_input_oz_r, heat_input_oz_access, heat_input_oz_source_r, heat_input_oz_source_access)

if(nrow(check_heat_input_oz_source) > 0) {
  write_csv(check_heat_input_oz_source, paste0(save_dir, "check_heat_input_oz_source.csv")) }

# Check emissions ------------
## Check NOx emissions -------------
check_total_nox_mass <- 
  unit_comparison %>% 
  summarize(sum_nox_mass_r = sum(nox_mass_r, na.rm = TRUE), 
            sum_nox_mass_access = sum(nox_mass_access, na.rm = TRUE), 
            sum_nox_oz_mass_r = sum(nox_oz_mass_r, na.rm = TRUE), 
            sum_nox_oz_mass_access = sum(nox_oz_mass_access, na.rm = TRUE)) %>% 
  select(contains("sum")) %>% 
  mutate(diff_nox_ann = sum_nox_mass_r - sum_nox_mass_access,
         diff_nox_oz = sum_nox_oz_mass_r - sum_nox_oz_mass_access)

if(!(check_total_nox_mass$diff_nox_ann == 0) | !(check_total_nox_mass$diff_nox_oz == 0)) {
  write_csv(check_total_nox_mass %>% select(sum_nox_mass_r, sum_nox_mass_access, diff_nox_ann, 
                                        sum_nox_oz_mass_r, sum_nox_oz_mass_access, diff_nox_oz), 
            paste0(save_dir, "check_total_nox_mass.csv")) }

# unit level NOx emissions
# annual NOx
check_nox_ann_unit <- 
  unit_comparison %>% 
  filter(!(nox_mass_r == nox_mass_access)) %>% 
  mutate(diff_nox_mass = nox_mass_r - nox_mass_access) %>% 
  filter(diff_nox_mass > 1 | diff_nox_mass < -1) %>% 
  select(plant_id, unit_id, primary_fuel_type_r, primary_fuel_type_access, 
         nox_mass_r, nox_mass_access, nox_source_r, nox_source_access)

if(nrow(check_nox_ann_unit) > 0) {
  write_csv(check_nox_ann_unit, paste0(save_dir, "check_nox_ann_unit.csv")) }

# ozone season NOx
check_nox_oz_unit <- 
  unit_comparison %>% 
  filter(!(nox_oz_mass_r == nox_oz_mass_access)) %>% 
  mutate(diff_nox_oz_mass = nox_oz_mass_r - nox_oz_mass_access) %>% 
  filter(diff_nox_oz_mass > 1 | diff_nox_oz_mass < -1) %>% 
  select(plant_id, unit_id, primary_fuel_type_r, primary_fuel_type_access, 
         nox_oz_mass_r, nox_oz_mass_access, nox_oz_source_r, nox_oz_source_access)

if(nrow(check_nox_oz_unit) > 0) {
  write_csv(check_nox_oz_unit, paste0(save_dir, "check_nox_oz_unit.csv")) }

# check NOx sources 
# annual NOx source
check_nox_ann_source <- 
  unit_comparison %>% 
  filter(!(nox_source_r == nox_source_access)) %>% 
  select(plant_id, unit_id, primary_fuel_type_r, primary_fuel_type_access, 
         nox_mass_r, nox_mass_access, nox_source_r, nox_source_access)

if(nrow(check_nox_ann_source) > 0) {
  write_csv(check_nox_ann_source, paste0(save_dir, "check_nox_ann_source.csv")) }

# ozone NOx source 
check_nox_oz_source <- 
  unit_comparison %>% 
  filter(!(nox_oz_source_r == nox_oz_source_access)) %>% 
  select(plant_id, unit_id, primary_fuel_type_r, primary_fuel_type_access, 
         nox_oz_mass_r, nox_oz_mass_access, nox_oz_source_r, nox_oz_source_access)

if(nrow(check_nox_oz_source) > 0) {
  write_csv(check_nox_oz_source, paste0(save_dir, "check_nox_oz_source.csv")) }

## Check SO2 emissions -------------
# SO2 emissions mass 
check_total_so2_mass <- 
  unit_comparison %>% 
  summarize(sum_so2_mass_r = sum(so2_mass_r, na.rm = TRUE), 
            sum_so2_mass_access = sum(so2_mass_access, na.rm = TRUE)) %>% 
  select(contains("sum")) %>% 
  mutate(diff_so2 = sum_so2_mass_r - sum_so2_mass_access)

if(!(check_total_so2_mass$diff_so2 == 0) | !(check_total_so2_mass$diff_so2 == 0)) {
  write_csv(check_total_so2_mass %>% select(sum_so2_mass_r, sum_so2_mass_access, diff_so2), 
            paste0(save_dir, "check_total_so2_mass.csv")) }

# unit level SO2 emissions
check_so2_unit <- 
  unit_comparison %>% 
  filter(!(so2_mass_r == so2_mass_access)) %>% 
  mutate(diff_so2_mass = so2_mass_r - so2_mass_access) %>% 
  filter(diff_so2_mass > 1 | diff_so2_mass < -1) %>% 
  select(plant_id, unit_id, primary_fuel_type_r, primary_fuel_type_access, 
         so2_mass_r, so2_mass_access, diff_so2_mass, 
         so2_source_r, so2_source_access)

if(nrow(check_so2_unit) > 0) {
  write_csv(check_so2_unit, paste0(save_dir, "check_so2_unit.csv")) }

# check SO2 source
check_so2_source <- 
  unit_comparison %>% 
  filter(!(so2_source_r == so2_source_access)) %>% 
  select(plant_id, unit_id, primary_fuel_type_r, primary_fuel_type_access, 
         so2_mass_r, so2_mass_access, so2_source_r, so2_source_access)

if(nrow(check_so2_source) > 0) {
  write_csv(check_so2_source, paste0(save_dir, "check_so2_source.csv")) }

## Check CO2 emissions ----------
# CO2 emissions mass 
check_total_co2_mass <- 
  unit_comparison %>% 
  summarize(sum_co2_mass_r = sum(co2_mass_r, na.rm = TRUE), 
            sum_co2_mass_access = sum(co2_mass_access, na.rm = TRUE)) %>% 
  select(contains("sum")) %>% 
  mutate(diff_co2 = sum_co2_mass_r - sum_co2_mass_access)

if(!(check_total_co2_mass$diff_co2 == 0) | !(check_total_co2_mass$diff_co2 == 0)) {
  write_csv(check_total_co2_mass %>% select(sum_co2_mass_r, sum_co2_mass_access, diff_co2), 
            paste0(save_dir, "check_total_co2_mass.csv")) }

# unit level CO2 emissions
check_co2_unit <- 
  unit_comparison %>% 
  filter(!(co2_mass_r == co2_mass_access)) %>% 
  mutate(diff_co2_mass = co2_mass_r - co2_mass_access) %>% 
  filter(diff_co2_mass > 1 | diff_co2_mass < -1) %>% 
  select(plant_id, unit_id, primary_fuel_type_r, primary_fuel_type_access, 
         co2_mass_r, co2_mass_access, diff_co2_mass, 
         co2_source_r, co2_source_access)

if(nrow(check_co2_unit) > 0) {
  write_csv(check_co2_unit, paste0(save_dir, "check_co2_unit.csv")) }

# check CO2 source
check_co2_source <- 
  unit_comparison %>% 
  filter(!(co2_source_r == co2_source_access)) %>% 
  select(plant_id, unit_id, primary_fuel_type_r, primary_fuel_type_access, 
         co2_mass_r, co2_mass_access, co2_source_r, co2_source_access)

if(nrow(check_co2_source) > 0) {
  write_csv(check_co2_source, paste0(save_dir, "check_co2_source.csv")) }
