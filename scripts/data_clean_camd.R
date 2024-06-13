
library(dplyr)
library(readxl)
library(stringr)
library(janitor)
library(purrr)

egrid_year <- params$eGRID_year


# Reading raw camd files

camd_raw <- readr::read_rds("data/raw_data/camd/camd_raw.RDS")


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


camd_r <- 
  camd_raw %>% 
  rename(any_of(rename_cols)) %>%
  filter(!operating_status %in% c("Future", "Retired", "Long-term Cold Storage"), # removing plants that are listed as future, retired, or long-term cold storage
         plant_id < 80000) %>% # removing plant with plant ids above 80,000
  mutate(
    heat_input_source = if_else(is.na(heat_input_mmbtu), NA_character_, "EPA/CAMD"), # creating source variables based on emissions data
    heat_input_oz_source = if_else(is.na(heat_input_mmbtu_ozone), NA_character_, "EPA/CAMD"),
    nox_source = if_else(is.na(nox_mass_short_tons), NA_character_, "EPA/CAMD"),
    nox_oz_source = if_else(is.na(nox_mass_short_tons_ozone), NA_character_, "EPA/CAMD"),
    so2_source = if_else(is.na(so2_mass_short_tons), NA_character_, "EPA/CAMD"),
    co2_source = if_else(is.na(co2_mass_short_tons), NA_character_, "EPA/CAMD"),
    hg_source = if_else(is.na(hg_mass_lbs), NA_character_, "EPA/CAMD"), # Mercury mass field needs to come from separate bulk api (SB 3/28/2024)
    year = egrid_year,
    camd = if_else(nox_source == "EPA/CAMD" , "Yes", NA_character_),
    operating_status = case_when(
      operating_status == "Operating" ~ "OP",
      startsWith(operating_status, "Operating") ~ "OP", # Units that started operating in current year have "Operating" plus the date of operation.
      operating_status == "Retired" ~ "RE",
      TRUE ~ NA_character_),
    unit_type = str_replace(unit_type, "\\(.*?\\)", "") %>% str_trim(), # removing notes about start dates and getting rid of extra white space
    unit_type_abb = recode(unit_type, !!!unit_abbs), ## Recoding values based on lookup table. need to looking into cases with multipe types (SB 3/28/2024)
    year_online = lubridate::year(commercial_operation_date)
    )
 
print(glue::glue("{nrow(camd_raw) - nrow(camd_r)} rows removed because units have status of future, retired, long-term cold storage, or the plant ID is > 80,000."))


camd_final <- # removing unnecessary columns and final renames
  camd_r %>% 
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

# saving clean camd file

# creating folder if not already present



if(!dir.exists("data/clean_data/camd")){
  dir.create("data/clean_data/camd")
} else{
  print("Folder data/clean_data/camd already exists.")
}



readr::write_rds(camd_final, "data/clean_data/camd/camd_clean.RDS")

if(file.exists("data/clean_data/camd/camd_clean.RDS")){
  print("File camd_clean.RDS successfully written to folder data/clean_data/camd")
} else {
  print("File camd_clean.RDS failed to write to folder.")
}
