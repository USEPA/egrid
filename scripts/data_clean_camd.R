
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
              ~ paste0("ozone","_",.x))
  

# Combining files and standardizing variables names to match eia data


camd_combined <- 
  camd_facilities %>% 
  left_join(camd_emissions) %>%
  left_join(camd_emissions_ozone) %>%
  janitor::clean_names() %>%
  rename("plant_state" = state_code,
         "plant_name" = facility_name,
         "plant_id" = facility_id,
         "primary_fuel_type" = primary_fuel_info,
         "secondary_fuel_type" = secondary_fuel_info) %>%
  arrange(plant_id, unit_id)


# saving clean camd file

# creating folder if not already present
if(!dir.exists("data/clean_data/camd")){
  dir.create("data/clean_data/camd")
} else{
  print("Folder data/clean_data/camd already exists.")
}



readr::write_rds(camd_combined, "data/clean_data/camd/camd_clean.RDS")

if(file.exists("data/clean_data/camd/camd_clean.RDS")){
  print("File camd_clean.RDS successfully written to folder data/clean_data/camd")
} else {
  print("File camd_clean.RDS failed to write to folder.")
}
