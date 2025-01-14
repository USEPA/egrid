## -------------------------------
##
## Data clean EPA
## 
## Purpose: 
## 
## This file cleans EPA datasets for the production of eGRID. 
## 
## Authors:  
##      Sean Bock, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries ---------

library(dplyr)
library(readxl)
library(stringr)
library(janitor)
library(purrr)
library(readr)

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Read raw EPA files -------

epa_raw <- read_rds(glue::glue("data/raw_data/epa/{params$eGRID_year}/epa_raw.RDS"))

# standardizing variables names to match eia data and removing retired and inactive plants

rename_cols <- # standardizing variable names across files
  c("plant_state" = "state", 
    "plant_name" = "facility_name",
    "plant_id" = "facility_id",
    "primary_fuel_type" = "primary_fuel_info",
    "secondary_fuel_type" = "secondary_fuel_info")

unit_abbs <- # abbreviation crosswalk for unit types 
  c(
    "Arch-fired boiler" = "AF",
    "Bubbling fluidized bed boiler" = "BFB",
    "Cyclone boiler" = "C",
    "Cell burner boiler" = "CB",
    "Combined cycle" = "CC",
    "Circulating fluidized bed boiler" = "CFB",
    "Combustion turbine" = "CT",
    "Dry bottom wall-fired boiler" = "DB",
    "Dry bottom turbo-fired boiler" = "DTF",
    "Dry bottom vertically-fired boiler" = "DVF",
    "Internal combustion engine" = "ICE",
    "Integrated gasification combined cycle" = "IGC",
    "Cement Kiln" = "KLN",
    "Other boiler" = "OB",
    "Other turbine" = "OT",
    "Pressurized fluidized bed boiler" = "PFB",
    "Process Heater" = "PRH",
    "Stoker" = "S",
    "Tangentially-fired" = "T",
    "Wet bottom wall-fired boiler" = "WBF",
    "Wet bottom turbo-fired boiler" = "WBT",
    "Wet bottom vertically-fired boiler" = "WVF"
  )

# Clean raw data -------

xwalk_so2_control_abrvs <- 
  read_csv("data/static_tables/xwalk_so2_control_abbreviations.csv") %>% 
  janitor::clean_names()

epa_nox_controls <- 
  read_csv("data/static_tables/epa_nox_controls.csv") 

# load manual corrections to data
manual_corrections <- 
  read_xlsx("data/static_tables/manual_corrections.xlsx", 
            sheet = "epa_clean", 
            col_types = c("text", "text", "text"))

plant_id_corrections <- # plant ID corrections
  manual_corrections %>% 
  filter(column_to_update == "plant_id") %>% 
  select(-column_to_update) %>% 
  mutate(plant_id = as.numeric(plant_id), 
         update = as.numeric(update))

op_status_corrections <- # operating status corrections
  manual_corrections %>% 
  filter(column_to_update == "operating_status") %>% 
  select(plant_id, operating_status = update)

epa_r <- 
  epa_raw %>% 
  rename(any_of(rename_cols)) %>%
  filter((!operating_status %in% c("Future", "Retired", "Long-term Cold Storage") | plant_id %in% manual_corrections$plant_id), # removing plants that are listed as future, retired, or long-term cold storage
         (plant_id < 880000 | plant_id %in% manual_corrections$plant_id)) %>% # removing plant with plant ids above 880,000 unless they are in Puerto Rico
  mutate(
    plant_id = case_when(
      plant_id %in% plant_id_corrections$plant_id ~ plant_id_corrections$update, 
      TRUE ~ plant_id), 
    heat_input_source = if_else(is.na(heat_input_mmbtu), NA_character_, "EPA/CAPD"), # creating source variables based on emissions data
    heat_input_oz_source = if_else(is.na(heat_input_mmbtu_ozone), NA_character_, "EPA/CAPD"),
    nox_source = if_else(is.na(nox_mass_short_tons), NA_character_, "EPA/CAPD"),
    nox_oz_source = if_else(is.na(nox_mass_short_tons_ozone), NA_character_, "EPA/CAPD"),
    so2_source = if_else(is.na(so2_mass_short_tons), NA_character_, "EPA/CAPD"),
    co2_source = if_else(is.na(co2_mass_short_tons), NA_character_, "EPA/CAPD"),
    hg_source = if_else(is.na(hg_mass_lbs), NA_character_, "EPA/CAPD"), # Mercury mass field needs to come from separate bulk api (SB 3/28/2024)
    year = params$eGRID_year,
    epa = if_else(nox_source == "EPA/CAPD" , "Yes", NA_character_),
    operating_status = case_when(
      operating_status == "Operating" ~ "OP",
      startsWith(operating_status, "Operating") ~ "OP", # Units that started operating in current year have "Operating" plus the date of operation.
      operating_status == "Retired" ~ "RE",
      plant_id %in% op_status_corrections$plant_id ~ op_status_corrections$operating_status,
      TRUE ~ operating_status),
    unit_type = str_replace(unit_type, "\\(.*?\\)", "") %>% str_trim(), # removing notes about start dates and getting rid of extra white space
    unit_type_abb = recode(unit_type, !!!unit_abbs), ## Recoding values based on lookup table. need to looking into cases with multipe types (SB 3/28/2024)
    year_online = lubridate::year(commercial_operation_date)
    ) %>% 
  left_join(xwalk_so2_control_abrvs, by = c("so2_controls")) %>% 
  mutate(so2_controls = if_else(!is.na(so2_control_abbreviation), so2_control_abbreviation, so2_controls)) %>% 
  select(-so2_control_abbreviation) %>% 
  rows_update(epa_nox_controls, by = c("plant_id", "unit_id"), unmatched = "ignore")
 
print(glue::glue("{nrow(epa_raw) - nrow(epa_r)} rows removed because units have status of future, retired, long-term cold storage, or the plant ID is > 80,000."))

# Remove unnecessary columns and rename as needed ------------

epa_final <- # removing unnecessary columns and final renames
  epa_r %>% 
  select(starts_with("plant"),
         unit_id,
         year,
         latitude,
         longitude,
         associated_stacks,
         program_code,
         ends_with("_region"),
         nameplate_capacity,
         operating_status,
         associated_generators,
         ends_with("_type"),
         unit_type_abb,
         reporting_frequency,
         starts_with(c("heat","so2", "co2", "nox", "hg")),
         contains("operating_time"),
         -contains("rate"),
         year_online) %>% 
  mutate(across(ends_with("id"), ~ as.character(.x)))

# Save clean EPA file ------------

# creating folder if not already present

if(!dir.exists("data/clean_data/epa")){
  dir.create("data/clean_data/epa")
} else{
  print("Folder data/clean_data/epa already exists.")
}

if(!dir.exists(glue::glue("data/clean_data/epa/{params$eGRID_year}"))){
  dir.create(glue::glue("data/clean_data/epa/{params$eGRID_year}"))
} else{
  print(glue::glue("Folder data/clean_data/epa/{params$eGRID_year} already exists."))
}

write_rds(epa_final, glue::glue("data/clean_data/epa/{params$eGRID_year}/epa_clean.RDS"))

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/clean_data/epa/{params$eGRID_year}/epa_clean.RDS"))){
  print(glue::glue("File epa_clean.RDS successfully written to folder data/clean_data/epa/{params$eGRID_year}"))
} else {
  print("File epa_clean.RDS failed to write to folder.")
}
