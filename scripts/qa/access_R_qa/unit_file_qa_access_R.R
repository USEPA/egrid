## -------------------------------
##
## Note: this file is for internal QA only. 
##
## Unit file QA 
## 
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the unit file creation in a given year. 
##
## The checks performed will output a CSV file with any differences 
## between Access and R unit files. 
## 
## Author: Teagan Goforth
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

if(dir.exists(glue::glue("data/outputs/qa/unit_file_differences/{params$eGRID_year}"))) {
  print(glue::glue("Folder unit_file_differences/{params$eGRID_year} already exists."))
}else{
  dir.create(glue::glue("data/outputs/qa/unit_file_differences/{params$eGRID_year}"))
}

# set directory for saving files 
save_dir <- glue::glue("data/outputs/qa/unit_file_differences/{params$eGRID_year}/")

# load unit file R
unit_r <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))

# add "_r" after each variable to easily identify dataset 
colnames(unit_r) <- paste0(colnames(unit_r), "_r")

unit_r <- unit_r %>% 
  rename("plant_id" = "plant_id_r", 
         "unit_id" = "unit_id_r",
         "prime_mover" = "prime_mover_r") %>% 
  mutate(num_generators_r = as.numeric(num_generators_r), 
         year_online_r = as.character(year_online_r))

unit_access <- 
  read_excel("data/raw_data/unit file 12_18_24.xlsx",
             sheet = "Sheet2", 
             skip = 1, 
             guess_max = 4000) %>% janitor::clean_names() %>% 
  #read_excel(glue::glue("data/raw_data/eGRID_Data{params$eGRID_year}.xlsx"), 
  #                        sheet = glue::glue("UNT{as.numeric(params$eGRID_year) %% 1000}"), 
  #                        skip = 1, 
  #                        guess_max = 4000) %>% janitor::clean_names() %>% 
  rename(#"sequnt_access" = glue::glue("sequnt{as.numeric(params$eGRID_year) %% 1000}"),
         #"year_access" = "year", 
         "plant_id" = "orispl",
         "plant_name_access" = "pname", 
         "plant_state_access" = "pstatabb", 
         "unit_id" = "unitid", 
         "prime_mover" = "prmvr", 
         "operating_status_access" = "untopst", 
         "capd_flag_access" = "camdflag", 
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
         program_code_access = gsub(",([[:alpha:]])", ", \\1", program_code_access), # adding space between listed program codes 
         year_online_access = as.character(year_online_access)) 

unit_access[unit_access == "NA"] <- NA_character_ # replace any "NA" strings with an NA 
         
# combine the two datasets
unit_comparison <- 
  unit_r %>% 
  full_join(unit_access, by = c("plant_id", 
                                "unit_id",
                                "prime_mover")) 

# Column by column checks -------------
### Identify if there are any plants in R that are NOT in Access dataset --------
# anti_join() pulls out differences
check_diff_plant_r <- 
  unit_r %>% 
  anti_join(unit_access, by = c("plant_id", "unit_id", "prime_mover")) %>% 
  filter(!is.na(plant_id))

if(nrow(check_diff_plant_r) > 0) {
  write_csv(check_diff_plant_r, paste0(save_dir, "check_diff_plant_r.csv")) }

### Identify if there are any plants in Access that are NOT in R dataset --------
check_diff_plant_access <- 
  unit_access %>% 
  anti_join(unit_r, by = c("plant_id", "unit_id", "prime_mover")) %>% 
  filter(!is.na(plant_id))

if(nrow(check_diff_plant_access) > 0) {
  write_csv(check_diff_plant_access, paste0(save_dir, "check_diff_plant_access.csv")) }


### Check plant names ------- 
check_plant_name <- 
  unit_comparison %>% 
  filter(mapply(identical, plant_name_r, plant_name_access) == FALSE) %>% 
  select(plant_id, plant_name_r, plant_name_access) %>% distinct()

if(nrow(check_plant_name) > 0) {
  write_csv(check_plant_name, paste0(save_dir, "check_plant_names.csv")) }

### Check plant states ------ 
check_plant_state <- 
  unit_comparison %>% 
  filter(mapply(identical, plant_state_r, plant_state_access) == FALSE) %>% 
  select(plant_id, plant_state_r, plant_state_access) %>% distinct()

if(nrow(check_plant_state) > 0) {
  write_csv(check_plant_state, paste0(save_dir, "check_plant_state.csv")) }

### Check operating status ------
check_operating_status <- 
  unit_comparison %>% 
  filter(mapply(identical, operating_status_r, operating_status_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, operating_status_r, operating_status_access)

if(nrow(check_operating_status) > 0) {
  write_csv(check_operating_status, paste0(save_dir, "check_operating_status.csv")) }

### Check CAPD flag --------
check_capd_flag <- 
  unit_comparison %>% 
  filter(mapply(identical, capd_flag_r, capd_flag_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, capd_flag_r, capd_flag_access)

if(nrow(check_capd_flag) > 0) {
  write_csv(check_capd_flag, paste0(save_dir, "check_capd_flag.csv")) }

### Check program code ------
check_program_code <- 
  unit_comparison %>% 
  filter(mapply(identical, program_code_r, program_code_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, program_code_r, program_code_access)


if(nrow(check_program_code) > 0) {
  write_csv(check_program_code, paste0(save_dir, "check_program_code.csv")) }

### Check boiler firing type --------
check_botfirty <- 
  unit_comparison %>% 
  filter(mapply(identical, botfirty_r, botfirty_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, botfirty_r, botfirty_access)

if(nrow(check_botfirty) > 0) {
  write_csv(check_botfirty, paste0(save_dir, "check_botfirty.csv")) }

### Check number of generators ------
check_num_gens <- 
  unit_comparison %>% 
  filter(mapply(identical, num_generators_r, num_generators_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, num_generators_r, num_generators_access)

if(nrow(check_num_gens) > 0) {
  write_csv(check_num_gens, paste0(save_dir, "check_num_gens.csv")) }

### Check primary fuel type -------
check_fuel_type <- 
  unit_comparison %>% 
  filter(mapply(identical, primary_fuel_type_r, primary_fuel_type_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, prime_mover, primary_fuel_type_r, primary_fuel_type_access)

if(nrow(check_fuel_type) > 0) {
  write_csv(check_fuel_type, paste0(save_dir, "check_fuel_type.csv")) }

### Check operating hours ------
check_operating_hours <- 
  unit_comparison %>% 
  filter(mapply(identical, operating_hours_r, operating_hours_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, operating_hours_r, operating_hours_access)

if(nrow(check_operating_hours) > 0) {
  write_csv(check_operating_hours, paste0(save_dir, "check_operating_hours.csv")) }

### Check SO2 controls --------
check_so2_controls <- 
  unit_comparison %>% 
  filter(mapply(identical, so2_controls_r, so2_controls_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, so2_controls_r, so2_controls_access)

if(nrow(check_so2_controls) > 0) {
  write_csv(check_so2_controls, paste0(save_dir, "check_so2_controls.csv")) }

### Check NOx controls -------
check_nox_controls <- 
  unit_comparison %>% 
  filter(mapply(identical, nox_controls_r, nox_controls_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, nox_controls_r, nox_controls_access)

if(nrow(check_nox_controls) > 0) {
  write_csv(check_nox_controls, paste0(save_dir, "check_nox_controls.csv")) }

### Check Hg controls flag -------
check_hg_flag <- 
  unit_comparison %>% 
  filter(mapply(identical, hg_controls_flag_r, hg_controls_flag_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, hg_controls_flag_r, hg_controls_flag_access)

if(nrow(check_hg_flag) > 0) {
  write_csv(check_hg_flag, paste0(save_dir, "check_hg_controls_flag.csv")) }

### Check year online ------
check_year_online <- 
  unit_comparison %>% 
  filter(mapply(identical, year_online_r, year_online_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, year_online_r, year_online_access)

if(nrow(check_year_online) > 0) {
  write_csv(check_year_online, paste0(save_dir, "check_year_online.csv")) }

# Check heat input ---------
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
  filter(mapply(identical, heat_input_r, heat_input_access) == FALSE) %>% 
  mutate(diff_heat_input = heat_input_r - heat_input_access) %>% 
  filter(diff_heat_input > 1 | diff_heat_input < -1 | is.na(diff_heat_input)) %>% 
  select(plant_id, unit_id, prime_mover, heat_input_r, heat_input_access, diff_heat_input, 
         heat_input_source_r, heat_input_source_access)

if(nrow(check_heat_input_ann_unit) > 0) {
  write_csv(check_heat_input_ann_unit, paste0(save_dir, "check_heat_input_ann_unit.csv")) }

# ozone season heat input check 
check_heat_input_oz_unit <- 
  unit_comparison %>% 
  filter(mapply(identical, heat_input_oz_r, heat_input_oz_access) == FALSE) %>% 
  mutate(diff_heat_input_oz = heat_input_oz_r - heat_input_oz_access) %>% 
  filter(diff_heat_input_oz > 1 | diff_heat_input_oz < -1) %>% 
  select(plant_id, unit_id, prime_mover, heat_input_oz_r, heat_input_oz_access, diff_heat_input_oz, 
         heat_input_oz_source_r, heat_input_oz_source_access)

if(nrow(check_heat_input_oz_unit) > 0) {
  write_csv(check_heat_input_oz_unit, paste0(save_dir, "check_heat_input_oz_unit.csv")) }

# check heat input sources
# annual heat input source
check_heat_input_ann_source <- 
  unit_comparison %>% 
  filter(mapply(identical, heat_input_source_r, heat_input_source_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, heat_input_r, heat_input_access, heat_input_source_r, heat_input_source_access)

if(nrow(check_heat_input_ann_source) > 0) {
  write_csv(check_heat_input_ann_source, paste0(save_dir, "check_heat_input_ann_source.csv")) }

# ozone heat input source 
check_heat_input_oz_source <- 
  unit_comparison %>% 
  filter(mapply(identical, heat_input_oz_source_r, heat_input_oz_source_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, heat_input_oz_r, heat_input_oz_access, heat_input_oz_source_r, heat_input_oz_source_access)

if(nrow(check_heat_input_oz_source) > 0) {
  write_csv(check_heat_input_oz_source, paste0(save_dir, "check_heat_input_oz_source.csv")) }

# Check emissions ------------
### Check NOx emissions -------------
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
  filter(mapply(identical, nox_mass_r, nox_mass_access) == FALSE) %>% 
  mutate(diff_nox_mass = nox_mass_r - nox_mass_access) %>% 
  select(plant_id, unit_id, primary_fuel_type_r, primary_fuel_type_access, 
         prime_mover, heat_input_r, heat_input_access,
         nox_mass_r, nox_mass_access, diff_nox_mass, nox_source_r, nox_source_access)

if(nrow(check_nox_ann_unit) > 0) {
  write_csv(check_nox_ann_unit, paste0(save_dir, "check_nox_ann_unit.csv")) }

# ozone season NOx
check_nox_oz_unit <- 
  unit_comparison %>% 
  filter(mapply(identical, nox_oz_mass_r, nox_oz_mass_access) == FALSE) %>% 
  mutate(diff_nox_oz_mass = nox_oz_mass_r - nox_oz_mass_access) %>% 
  select(plant_id, unit_id, prime_mover, primary_fuel_type_r, primary_fuel_type_access, 
         heat_input_oz_r, heat_input_oz_access,
         nox_oz_mass_r, nox_oz_mass_access, diff_nox_oz_mass, nox_oz_source_r, nox_oz_source_access)

if(nrow(check_nox_oz_unit) > 0) {
  write_csv(check_nox_oz_unit, paste0(save_dir, "check_nox_oz_unit.csv")) }

# check NOx sources 
# annual NOx source
check_nox_ann_source <- 
  unit_comparison %>% 
  filter(mapply(identical, nox_source_r, nox_source_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, primary_fuel_type_r, primary_fuel_type_access,
         nox_mass_r, nox_mass_access, nox_source_r, nox_source_access)

if(nrow(check_nox_ann_source) > 0) {
  write_csv(check_nox_ann_source, paste0(save_dir, "check_nox_ann_source.csv")) }

# ozone NOx source 
check_nox_oz_source <- 
  unit_comparison %>% 
  filter(mapply(identical, nox_oz_source_r, nox_oz_source_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, 
         primary_fuel_type_r, primary_fuel_type_access, 
         nox_oz_mass_r, nox_oz_mass_access, nox_oz_source_r, nox_oz_source_access)

if(nrow(check_nox_oz_source) > 0) {
  write_csv(check_nox_oz_source, paste0(save_dir, "check_nox_oz_source.csv")) }

### Check SO2 emissions -------------
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
  filter(mapply(identical, so2_mass_r, so2_mass_access) == FALSE) %>% 
  mutate(diff_so2_mass = so2_mass_r - so2_mass_access) %>% 
  select(plant_id, unit_id, prime_mover,
         primary_fuel_type_r, primary_fuel_type_access, 
         heat_input_r, heat_input_access,
         so2_mass_r, so2_mass_access, diff_so2_mass, 
         so2_source_r, so2_source_access)

if(nrow(check_so2_unit) > 0) {
  write_csv(check_so2_unit, paste0(save_dir, "check_so2_unit.csv")) }

# check SO2 source
check_so2_source <- 
  unit_comparison %>% 
  filter(mapply(identical, so2_source_r, so2_source_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, 
         primary_fuel_type_r, primary_fuel_type_access, 
         so2_mass_r, so2_mass_access, so2_source_r, so2_source_access)

if(nrow(check_so2_source) > 0) {
  write_csv(check_so2_source, paste0(save_dir, "check_so2_source.csv")) }

### Check CO2 emissions ----------
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
  filter(mapply(identical, co2_mass_r, co2_mass_access) == FALSE) %>% 
  mutate(diff_co2_mass = co2_mass_r - co2_mass_access) %>% 
  filter(diff_co2_mass > 1 | diff_co2_mass < -1 | is.na(diff_co2_mass)) %>% 
  select(plant_id, unit_id, prime_mover, primary_fuel_type_r, primary_fuel_type_access, 
         heat_input_r, heat_input_access,
         co2_mass_r, co2_mass_access, diff_co2_mass, 
         co2_source_r, co2_source_access)

if(nrow(check_co2_unit) > 0) {
  write_csv(check_co2_unit, paste0(save_dir, "check_co2_unit.csv")) }

# check CO2 source
check_co2_source <- 
  unit_comparison %>% 
  filter(mapply(identical, co2_source_r, co2_source_access) == FALSE) %>% 
  select(plant_id, unit_id, prime_mover, 
         primary_fuel_type_r, primary_fuel_type_access, 
         co2_mass_r, co2_mass_access, co2_source_r, co2_source_access)

if(nrow(check_co2_source) > 0) {
  write_csv(check_co2_source, paste0(save_dir, "check_co2_source.csv")) }

# Identify all unique plant and unit IDs that have differences ------------

files <- grep("check", dir(glue::glue("data/outputs/qa/unit_file_differences/{params$eGRID_year}")), value = TRUE)

plant_unit_diffs <- 
  purrr::map_df(paste0(glue::glue("data/outputs/qa/unit_file_differences/{params$eGRID_year}/"), files), 
                ~read_csv(.x, col_types = "ccccccccccccccccccccccccccccccccccccc")) %>% 
  select(plant_id, unit_id, prime_mover) %>% 
  distinct() %>% 
  mutate(source_diff = "unit_file")

write_csv(plant_unit_diffs, glue::glue("data/outputs/qa/unit_file_differences/{params$eGRID_year}/plant_unit_difference_ids.csv"))

