
library(dplyr)
library(readxl)
library(glue)
library(stringr)
library(janitor)
library(purrr)

egrid_year <- Sys.getenv("eGRID_year")


# Reading raw camd files

camd_facilities <- readr::read_rds("data/raw_data/camd/camd_facilities.RDS")
camd_emissions <- readr::read_rds("data/raw_data/camd/camd_emissions.RDS")
camd_emissions_ozone <- readr::read_rds("data/raw_data/camd/camd_emissions_ozone.RDS") %>% # adding "ozone" prefix to non-id cols
  rename_with(.cols = -c("stateCode", "facilityName", "facilityId","unitId", "associatedStacks", "year"),
              ~ paste0(.x, "_", "oz"))
  

# Combining files and standardizing variables names to match eia data


camd_combined <- 
  camd_facilities %>% 
  left_join(camd_emissions) %>%
  left_join(camd_emissions_ozone) %>%
  janitor::clean_names() %>% 
  rename_with(~ stringr::str_replace(.x, "o2", "o2_")) %>% # clean_names() doesn't add underscore after numeric values. Adding in to keep snake case.
  rename("plant_state" = state_code, # renaming to matching eia
         "plant_name" = facility_name,
         "plant_id" = facility_id,
         "primary_fuel_type" = primary_fuel_info,
         "secondary_fuel_type" = secondary_fuel_info) 

camd_combined_r <-
  camd_combined %>% 
  filter(!operating_status %in% c("Future", "Retired", "Long-term Cold Storage"), # removing plants that are listed as future, retired, or long-term cold storage
         plant_id < 80000) %>% # removing plant with plant ids above 80,000
  mutate(
    heat_input_source = if_else(is.na(heat_input), NA_character_, "EPA/CAMD"), # creating source variables based on emissions data
    heat_input_oz_source = if_else(is.na(heat_input_oz), NA_character_, "EPA/CAMD"),
    nox_source = if_else(is.na(nox_mass), NA_character_, "EPA/CAMD"),
    nox_oz_source = if_else(is.na(nox_mass_oz), NA_character_, "EPA/CAMD"),
    so2_source = if_else(is.na(so2_mass), NA_character_, "EPA/CAMD"),
    co2_source = if_else(is.na(co2_mass), NA_character_, "EPA/CAMD"),
    #hg_source = if_else(is.na(hg_mass), NA_character_, "EPA/CAMD"), #* Need to find mercury mass field
    year = egrid_year,
    camd = if_else(nox_source == "EPA/CAMD" , "Yes", NA_character_)
  ) %>%
  mutate(operating_status = case_when(
    operating_status == "Operating" ~ "OP",
    startsWith(operating_status, "Operating") ~ "OP", # Units that started operating in current year have "Operating" plus the date of operattion.
    operating_status == "Retired" ~ "RE",
    TRUE ~ NA_character_
  )) 
  # left_join(camd_eia_xwalk,
  #           by = c("Unit Type" = "CAMD Unit Type"))  ## Need to double check why this is needed and where abb. unit type comes from

print(glue::glue("{nrow(camd_combined) - nrow(camd_combined_r)} rows removed because units have status of future, retired, long-term cold storage, or the plant ID is > 80,000."))


# saving clean camd file

# creating folder if not already present
if(!dir.exists("data/clean_data/camd")){
  dir.create("data/clean_data/camd")
} else{
  print("Folder data/clean_data/camd already exists.")
}



readr::write_rds(camd_combined_r, "data/clean_data/camd/camd_clean.RDS")

if(file.exists("data/clean_data/camd/camd_clean.RDS")){
  print("File camd_clean.RDS successfully written to folder data/clean_data/camd")
} else {
  print("File camd_clean.RDS failed to write to folder.")
}
