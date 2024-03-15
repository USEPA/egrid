
# Note: Need to update with additional raw data sources. Also need to revisit choices aobut variables types across files (SB: 3/15/24)



library(dplyr)
library(readxl)
library(glue)
library(stringr)
library(janitor)

egrid_year <- Sys.getenv("eGRID_year") # define year


# 923 files ----------------

rename_cols_923 <- c("prime_mover" = "reported_prime_mover", # creating character vector to rename selected columns in 923 files
                      "fuel_type" = "reported_fuel_type_code")

# getting sheet names to use as sheet filter 
sheet_names_923 <- excel_sheets(glue("data/raw_data/923/EIA923_Schedules_2_3_4_5_M_12_{egrid_year}_Final.xlsx")) # get vector of sheet names


eia_923_gen <- 
  read_excel(glue("data/raw_data/923/EIA923_Schedules_2_3_4_5_M_12_{egrid_year}_Final.xlsx"),
             sheet = sheet_names_923[str_detect(sheet_names_923, "Generator Data")], # names change year to year, so can't rely on name or position
             skip = 5, # Removing intro header rows
             na = ".", # Convert periods to missing
             guess_max = 4000) %>% # increased # of rows to guess type
  janitor::clean_names() %>% # remove white space, formatting characters, and convert to snake case
  rename(any_of(rename_cols_923)) # renaming columns if present

eia_923_gen_fuel <- 
  read_excel(glue("data/raw_data/923/EIA923_Schedules_2_3_4_5_M_12_{egrid_year}_Final.xlsx"),
             sheet = sheet_names_923[str_detect(sheet_names_923, "Generation and Fuel")], 
             skip = 5, 
             na = ".",
             guess_max = 4000) %>% 
  janitor::clean_names() %>% 
  rename(any_of(rename_cols_923))


eia_923_gen_fuel_PR <- 
  read_excel(glue("data/raw_data/923/EIA923_Schedules_2_3_4_5_M_12_{egrid_year}_Final.xlsx"), 
             sheet =  sheet_names_923[!str_detect(sheet_names_923, "Plant Frame") &
                                        str_detect(sheet_names_923, "Puerto Rico")],
             na = ".", 
             guess_max = 4000,
             skip = 6) %>% 
  janitor::clean_names() %>%  
  rename("reserved" = "reserved_10", # fixing issue of two "Reserved" columns. Need to figure out better way in case they're not 10 and 17
         "balancing_authority_code" = "reserved_17",
         any_of(rename_cols_923))

## 923 combined files ----------

eia_923_gen_fuel_combined <-
  eia_923_gen_fuel %>% 
  bind_rows(eia_923_gen_fuel_PR)

# 860 files -----------------

rename_cols_860 <- c("plant_id" = "plant_code",
                     "plant_state" = "state",
                     "nameplate_capacity" = "nameplate_capacity_mw")

sheet_names_860 <- excel_sheets(glue("data/raw_data/860/3_1_Generator_Y{egrid_year}.xlsx")) # get vector of sheet names


eia_860_operable <-
  read_excel(glue("data/raw_data/860/3_1_Generator_Y{egrid_year}.xlsx"), 
             sheet =  "Operable",
             na = c(".","X"), # Xs used as missing, for 860 files
             guess_max = 4000,
             skip = 1) %>% 
  janitor::clean_names() %>% 
  filter(!if_all(everything(), is.na)) %>% # this table includes a note, which turns into complete row of NAs. Removing where all columns are missing. 
  rename(any_of(rename_cols_860)) %>% 
  mutate(across(contains("capacity"), ~ as.numeric(.x))) # transforming any capacity variables to numeric




eia_860_retired <-
  read_excel(glue("data/raw_data/860/3_1_Generator_Y{egrid_year}.xlsx"), 
             sheet =  "Retired and Canceled",
             na = c(".","X"), 
             guess_max = 4000,
             skip = 1) %>% 
  janitor::clean_names() %>% 
  rename(any_of(rename_cols_860)) %>% 
  mutate(across(c("operating_month", "operating_year", "utility_id"), ~ as.numeric(.x)),
         across(contains("capacity"), ~ as.numeric(.x))) %>% # changing types to matched eia_860_operable
  filter(retirement_year == egrid_year) # only plants retiring in current year are added to 860 combined file

eia_860_proposed <-
  read_excel(glue("data/raw_data/860/3_1_Generator_Y{egrid_year}.xlsx"), 
             sheet =  "Proposed",
             guess_max = 4000,
             na = c(".","X"), 
             skip = 1) %>% 
  janitor::clean_names() %>%  
  rename(any_of(rename_cols_860)) %>% 
  mutate(across(c("utility_id"), ~ as.numeric(.x)),
         across(contains("capacity"), ~ as.numeric(.x))) # changing types to matched eia_860_operable
  

# eia combined includes op, retired, and proposed, but filters out some plants based on camd. Need to check this logic.  
# There also two files loaded for Puerto Rico from the monthly December data? I don't see this documented anywhere, so need to check.

eia_860_combined <-
  eia_860_operable %>% 
  bind_rows(eia_860_retired) %>% 
  bind_rows(eia_860_proposed)  # There are some conditions that need to be added to this file.            

eia_860_boil_gen <-
  read_excel(glue("data/raw_data/860/6_1_EnviroAssoc_Y{egrid_year}.xlsx"), 
             sheet =  "Boiler Generator",
             guess_max = 4000,
             na = c(".","X"), 
             skip = 1) %>% 
  janitor::clean_names() %>% 
  filter(!if_all(everything(), is.na)) %>% 
  rename(any_of(rename_cols_860))


# Writing cleaned files


if(!dir.exists("data/clean_data/eia")){
  dir.create("data/clean_data/eia")
} else{
  print("Folder data/clean_data/eia already exists.")
}
  

readr::write_rds(eia_923_gen, file = "data/clean_data/eia/eia_923_gen_clean.RDS")
readr::write_rds(eia_923_gen_fuel_combined, file = "data/clean_data/eia/eia_923_gen_fuel_clean.RDS")
readr::write_rds(eia_860_combined, file = "data/clean_data/eia/eia_860_combined_clean.RDS")
readr::write_rds(eia_860_boil_gen, file = "data/clean_data/eia/eia_860_boil_gen_clean.RDS")

clean_files <- list.files("data/clean_data/eia")

print(glue::glue("Files {glue::glue_collapse(clean_files, sep = ', ', last = ', and ')} written to folder data/clean_data/eia."))
