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
library(readr)

# Load functions ---------

source("scripts/functions/function_check_params.R")
source("scripts/functions/function_temporal_res_cols.R")
source("scripts/functions/function_save_output_data.R")

# check if parameters for eGRID data year need to be defined
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")
}

# Specify grouping columns based on temporal_res parameter
# annual version will use monthly version of EPA data 
temporal_res_cols <- create_temporal_res_cols("monthly")

# Read raw EPA files -------

# annual version will use monthly version of EPA data 
epa_raw <- read_rds(glue::glue("data/1_production_model/raw_data/epa/{params$eGRID_year}/epa_raw.RDS"))

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

# load manual corrections to data
manual_corrections <- 
  read_xlsx("data/1_production_model/static_tables/manual_corrections.xlsx", 
            sheet = "epa_clean", 
            col_types = c("numeric", "text", "text", "text")) %>% 
  filter(year >= as.numeric(params$eGRID_year))

plant_id_corrections <- # plant ID corrections
  manual_corrections %>% 
  filter(column_to_update == "plant_id") %>% 
  select(plant_id, plant_id_update = update) 

op_status_corrections <- # operating status corrections
  manual_corrections %>% 
  filter(column_to_update == "operating_status") %>% 
  select(plant_id, operating_status_update = update)

epa_r <- 
  epa_raw %>% 
  rename(any_of(rename_cols)) %>%
  filter((!operating_status %in% c("Future", "Retired", "Long-term Cold Storage") | plant_id %in% manual_corrections$plant_id), # removing plants that are listed as future, retired, or long-term cold storage
         (plant_id < 880000 | plant_id %in% manual_corrections$plant_id)) %>% # removing plant with plant ids above 880000
  mutate(plant_id = as.character(plant_id)) %>% 
  left_join(op_status_corrections, by = "plant_id") %>% 
  left_join(plant_id_corrections, by = "plant_id") %>% 
  mutate(
    plant_id = case_when(
      !is.na(plant_id_update) ~ plant_id_update, 
      TRUE ~ plant_id), 
    heat_input_source = if_else(is.na(heat_input_mmbtu), NA_character_, "EPA/CAPD"), # creating source variables based on emissions data
    nox_source = if_else(is.na(nox_mass_short_tons), NA_character_, "EPA/CAPD"),
    so2_source = if_else(is.na(so2_mass_short_tons), NA_character_, "EPA/CAPD"),
    co2_source = if_else(is.na(co2_mass_short_tons), NA_character_, "EPA/CAPD"),
    hg_source = if_else(is.na(hg_mass_lbs), NA_character_, "EPA/CAPD"), # Mercury mass field needs to come from separate bulk api (SB 3/28/2024)
    year = params$eGRID_year,
    epa = if_else(nox_source == "EPA/CAPD" , "Yes", NA_character_),
    operating_status = case_when(
      operating_status == "Operating" ~ "OP",
      startsWith(operating_status, "Operating") ~ "OP", # Units that started operating in current year have "Operating" plus the date of operation.
      operating_status == "Retired" ~ "RE",
      startsWith(operating_status, "Retired") ~ "RE",
      !is.na(operating_status_update) ~ operating_status_update,
      TRUE ~ operating_status),
    #unit_type = str_replace(unit_type, "\\(.*?\\)", "") %>% str_trim(), 
    unit_type = sub("\\(.*", "", unit_type) %>% str_trim(), # removing notes about start dates and getting rid of extra white space
    unit_type_abb = recode(unit_type, !!!unit_abbs), ## Recoding values based on lookup table. need to looking into cases with multiple types (SB 3/28/2024)
    year_online = lubridate::year(commercial_operation_date)
    ) 

## Abbreviate control technologies ---------------------

### SO2 control technologies ---------------------

epa_abbv_so2_controls <- # assign abbreviations to SO2 controls
  epa_r %>% 
  select(plant_id, unit_id, so2_controls) %>% distinct() %>% # remove duplicates that will arise during monthly version 
  mutate(so2_controls = str_replace_all(so2_controls, "\\|", ","), 
         so2_controls = gsub(", \\b[0-9]{4}\\b", "\\b[0-9]{4}\\b", so2_controls)) %>% # removing comma from year retired or installed to avoid issues using separate_longer_delim()
  tidyr::separate_longer_delim(so2_controls, ",") %>% 
  mutate(
    so2_controls = 
        gsub("^Activated (C|c)arbon (I|i)njection.*", "ACI", 
        gsub("^Circulating (D|d)ry (S|s)crubber.*", "CD",
        gsub("^Dual (A|a)lkali.*", "DA",
        gsub("^Dry (L|l)ime FGD.*", "DL",
        gsub("^Dry (S|s)orbent (I|i)njection.*", "DSI",
        gsub("^Electrostatic (P|p)recipitator.*", "EK",
        gsub("^Fluidized (B|b)ed (L|l)imestone (I|i)njection.*", "FBL",
        gsub("^Other.*", "O",
        gsub("^Sodium (B|b)ased.*", "SB",
        gsub("^Wet (L|l)ime FGD.*", "WL",
        gsub("^Wet (L|l)imestone.*", "WLS", so2_controls)))))))))))
  ) %>% 
  group_by(plant_id, unit_id) %>% 
  mutate(so2_controls = paste(so2_controls, collapse = ","), # collapse SO2 controls into one row
         so2_controls = if_else(so2_controls == "NA", NA_character_, so2_controls)) %>% 
  ungroup() %>% distinct()

### NOx control technologies --------------------

epa_abbv_nox_controls <- # assign abbreviations to NOx controls
  epa_r %>% 
  select(plant_id, unit_id, nox_controls) %>% distinct() %>% # remove duplicates that will arise during monthly version 
  mutate(nox_controls = str_replace_all(nox_controls, "\\|", ","), 
         nox_controls = gsub(", \\b[0-9]{4}\\b", "\\b[0-9]{4}\\b", nox_controls)) %>% # removing comma from year retired or installed to avoid issues using separate_longer_delim()
  tidyr::separate_longer_delim(nox_controls, ",") %>% 
  mutate(
    nox_controls = 
      gsub("^Combustion (M|m)odification/(F|f)uel (R|r)eburning.*", "CM", 
      gsub("^Dry (L|l)ow NOx (P|p)remixed (T|t)echnology.*", "DLNB",
      gsub("^Electrostatic (P|p)recipitator, (H|h)ot side, without (F|f)lue (G|g)as (C|c)onditioning.*", "EW",
      gsub("^Water (I|i)njection.*", "H2O",
      gsub("^Low NOx (B|b)urner (T|t)echnology \\(Dry (B|b)ottom (O|o)nly\\).*", "LNB",
      gsub("^Dry (L|l)ow NOx (B|b)urners.*", "LNB",
      gsub("^Low NOx (B|b)urner (T|t)echnology (with|w/) ((O|o)verfire (A|a)ir|OFA).*", "LNBO",
      gsub("^Low NOx (B|b)urner (T|t)echnology (with|w/) (C|c)losed-coupled ((O|o)verfire (A|a)ir|OFA).*", "LNC1",
      gsub("^Low NOx (B|b)urner (T|t)echnology (with|w/) (S|s)eparated ((O|o)verfire (A|a)ir|OFA).*", "LNC2",
      gsub("^Low NOx (B|b)urner (T|t)echnology (with|w/) (C|c)losed-coupled/(S|s)eparated ((O|o)verfire (A|a)ir|OFA).*", "LNC3",
      gsub("^Low NOx (C|c)ell (B|b)urner.*", "LNCB",
      gsub("^Ammonia (I|i)njection.*", "NH3",
      gsub("^Other.*", "O",
      gsub("^Overfire (A|a)ir.*", "OFA",
      gsub("^Selective (C|c)atalytic (R|r)eduction.*", "SCR",
      gsub("^Selective (N|n)o(n|n-)catalytic (R|r)eduction.*", "SNCR",
      gsub("^Steam (I|i)njection.*", "STM", nox_controls)))))))))))))))))
  ) %>% 
  group_by(plant_id, unit_id) %>% 
  mutate(nox_controls = paste(nox_controls, collapse = ","), # collapse NOx controls into one row
         nox_controls = if_else(nox_controls == "NA", NA_character_, nox_controls)) %>% 
  ungroup() %>% distinct()

epa_r_2 <- 
  epa_r %>% 
  rows_update(epa_abbv_so2_controls, by = c("plant_id", "unit_id")) %>% 
  rows_update(epa_abbv_nox_controls, by = c("plant_id", "unit_id"))
 
print(glue::glue("{nrow(epa_raw) - nrow(epa_r)} rows removed because units have status of future, retired, long-term cold storage, or the plant ID is > 880,000."))

# Remove unnecessary columns and rename as needed ------------

epa_final <- # removing unnecessary columns and final renames
  epa_r_2 %>%
  select(starts_with("plant"),
         unit_id,
         all_of(temporal_res_cols),
         latitude,
         longitude,
         associated_stacks,
         program_code,
         ends_with("_region"),
         nameplate_capacity,
         operating_status,
         associated_generators,
         max_hourly_hi_rate_mmbtu_hr, 
         ends_with("_type"),
         unit_type_abb,
         reporting_frequency,
         starts_with(c("heat","so2", "co2", "nox", "hg")),
         contains("operating_time"),
         -contains("so2_rate"),
         -contains("nox_rate"), 
         -contains("co2_rate"),
         year_online) %>%
  mutate(across(ends_with("id"), ~ as.character(.x)))

# Save clean EPA file ------------

file <- "epa_clean.RDS"

save_output_data(epa_final, "data/1_production_model/clean_data/epa", file)
