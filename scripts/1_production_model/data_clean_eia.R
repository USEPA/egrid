## -------------------------------
##
## Data clean EIA
## 
## Purpose: 
## 
## This file cleans the EIA datasets for eGRID: EIA-860, EIA-861, and EIA-923 
## 
## Authors:  
##      Sean Bock, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries ---------

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)

# Load necessary functions -----------------------

source("scripts/functions/function_check_params.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")
}

# Load manual corrections ----------

manual_corrections <- 
  read_xlsx("data/1_production_model/static_tables/manual_corrections.xlsx", 
            sheet = "eia_clean", 
            col_types = c("text", "text", "text"))

# Create month mapping from month name to number -----------------
month_name_map <- # creating map to recode numeric monthly names to values
  c(1:12) %>% 
  purrr::set_names(tolower(month.name))

# List file in EIA raw data folders ------------

# file names are not always consistent from the EIA website
# for example, some may have a "_Revision" addition if the data has been revised by EIA after initial release
# to address this issue, we list file names that are extracted from the EIA website and identify file names based on consistent strings across years

eia_923_files <- list.files(glue::glue("data/1_production_model/raw_data/923/{params$eGRID_year}"))
eia_860_files <- list.files(glue::glue("data/1_production_model/raw_data/860/{params$eGRID_year}"))
eia_860m_files <- list.files(glue::glue("data/1_production_model/raw_data/860m/{params$eGRID_year}"))
eia_861_files <- list.files(glue::glue("data/1_production_model/raw_data/861/{params$eGRID_year}"))


# EIA-923 ------------

## Annual 923 data -----------------------------

if (length(grep("M_12", eia_923_files)) > 0) { # check if the annual files are available. If not, we use the available monthly data. 
  
  ### 923 Schedules_2_3_4_5_M_12 ----------------
  
  sheets_923_1 <- c("Page 1 Generation and Fuel Data", # defining list of sheets to iterate over and extract from excel file
                    "Page 1 Puerto Rico",
                    "Page 3 Boiler Fuel Data",
                    "Page 4 Generator Data")
  
  file_name_schedule_2_3_4_5_m_12 <- grep("2_3_4_5_M", eia_923_files, value = TRUE)
  
  sched_2_3_4_5_m_12_dfs <- 
    purrr::map2(sheets_923_1, # .x, defining sheets to iterate over
                c(5,6,5,5),   # .y, adding second argument to define the number of rows to skip (differs between files)
               ~ read_excel(paste0(glue::glue("data/1_production_model/raw_data/923/{params$eGRID_year}/"), file_name_schedule_2_3_4_5_m_12), 
                            sheet = .x,
                            skip = .y,
                            na = ".", # converting "." to NAs
                            guess_max = 4000)) %>% # expanding length of rows for R to check to guess data type
    purrr::map(., ~ .x %>% 
                 rename_with(tolower) %>% 
                 janitor::clean_names()) %>% # this lower cases and converts to snake_case
    purrr::map(., ~ .x %>% 
                 pivot_longer(cols = contains(tolower(month.name)),
                              names_to = c(".value", "month"),
                              names_pattern = "^(.*)_(.*)$")) %>% 
    purrr::map(., ~ .x %>% 
                 mutate(month = as.numeric(recode(month, !!!month_name_map)))) %>% 
    setNames(., janitor::make_clean_names(str_replace_all(sheets_923_1, "Page \\d+ ", ""))) %>% # This assigns cleaned sheets names name values for list of dataframes. Storing df names without Page #s
    purrr::map_at("puerto_rico", # modifying puerto_rico tab only
                  ~ .x %>% 
                    rename("reserved" = "reserved_10", # fixing issue of two "Reserved" columns. Need to figure out better way in case they're not 10 and 17
                           "balancing_authority_code" = "reserved_17")) %>% 
    purrr::map(., ~ .x %>% 
                 relocate(month) %>% 
                 relocate(year))
  
  #### Adding Puerto Rico data to EIA-923 Generation and Fuel ---------------
  
  gen_fuel_combined <-
    bind_rows(sched_2_3_4_5_m_12_dfs$generation_and_fuel_data,
              sched_2_3_4_5_m_12_dfs$puerto_rico)
  
  
  #### EIA923_Schedule_8_Annual_Environmental_Information -------------------------
  
  file_name_schedule_8 <- grep("Schedule_8", eia_923_files, value = TRUE)
  
  air_emissions_control_info <- 
   read_excel(paste0(glue::glue("data/1_production_model/raw_data/923/{params$eGRID_year}/"), file_name_schedule_8), 
                             sheet = "8C Air Emissions Control Info",
                             skip = 4,
                             na = ".",
                             guess_max = 4000) %>%
    rename_with(tolower) %>%
    janitor::clean_names() 
  
  
  ### Combining 923 files -------------------
  
  rename_cols_923 <- c("prime_mover" = "reported_prime_mover", # creating character vector to rename selected columns in 923 files
                       "fuel_type" = "reported_fuel_type_code")
  
  dfs_923 <- c(sched_2_3_4_5_m_12_dfs,
               "air_emissions_control_info" = list(air_emissions_control_info),
               "generation_and_fuel_combined" = list(gen_fuel_combined)) %>% 
               purrr:::map(., ~ .x %>%  # standardizing column types across all dfs
                  rename(any_of(rename_cols_923)) %>% # standardizing col names to match other files
                  mutate(across(ends_with("id"), ~ as.character(.x)),
                         across(contains(c("capacity", "generation", "netgen")), ~ as.numeric(.x)),
                         across(starts_with(c("year")), ~ as.character(.x))) %>% 
                  filter(!if_all(everything(), is.na)))
  
## Monthly 923 data ----------------------
} else { 
  ### 923 Schedules_2_3_4_5_M_12 --------
  
  sheets_923_1 <- c("Page 1 Generation and Fuel Data", # defining list of sheets to iterate over and extract from excel file
                    "Page 1 Puerto Rico",
                    "Page 3 Boiler Fuel Data",
                    "Page 4 Generator Data")
  
  file_name_schedule_2_3_4_5_m <- grep("2_3_4_5_M", eia_923_files, value = TRUE)
  
  sched_2_3_4_5_m_12_dfs <- 
    purrr::map2(sheets_923_1, # .x, defining sheets to iterate over
                c(5,5,4,4),   # .y, adding second argument to define the number of rows to skip (differs between files)
                ~ read_excel(paste0(glue::glue("data/1_production_model/raw_data/923/{params$eGRID_year}/"), file_name_schedule_2_3_4_5_m), 
                             sheet = .x,
                             skip = .y,
                             na = ".", # converting "." to NAs
                             guess_max = 4000)) %>% # expanding length of rows for R to check to guess data type
    purrr::map(., ~ .x %>% 
                 rename_with(tolower) %>% 
                 janitor::clean_names()) %>% # this lower cases and converts to snake_case
    purrr::map(., ~ .x %>% 
                 pivot_longer(cols = contains(tolower(month.name)),
                              names_to = c(".value", "month"),
                              names_pattern = "^(.*)_(.*)$")) %>% 
    purrr::map(., ~ .x %>% 
                 mutate(month = as.numeric(recode(month, !!!month_name_map)))) %>% 
    setNames(., janitor::make_clean_names(str_replace_all(sheets_923_1, "Page \\d+ ", ""))) %>% # This assigns cleaned sheets names name values for list of dataframes. Storing df names without Page #s
    purrr::map_at("puerto_rico", # modifying puerto_rico tab only
                  ~ .x %>% 
                    rename("reserved" = "reserved_10", # fixing issue of two "Reserved" columns. Need to figure out better way in case they're not 10 and 17
                           "balancing_authority_code" = "reserved_17")) %>% 
    purrr::map(., ~ .x %>% 
                 relocate(month) %>% 
                 relocate(year))
  
  #### Adding Puerto Rico data to EIA-923 Generation and Fuel --------
  
  gen_fuel_combined <-
    bind_rows(sched_2_3_4_5_m_12_dfs$generation_and_fuel_data,
              sched_2_3_4_5_m_12_dfs$puerto_rico)
  
  ### Combining 923 files -------------------
  
  rename_cols_923 <- c("prime_mover" = "reported_prime_mover", # creating character vector to rename selected columns in 923 files
                       "fuel_type" = "reported_fuel_type_code")
  
  dfs_923 <- c(sched_2_3_4_5_m_12_dfs,
               "generation_and_fuel_combined" = list(gen_fuel_combined)) %>% 
    purrr:::map(., ~ .x %>%  # standardizing column types across all dfs
                  rename(any_of(rename_cols_923)) %>% # standardizing col names to match other files
                  mutate(across(ends_with("id"), ~ as.character(.x)),
                         across(contains(c("capacity", "generation", "netgen")), ~ as.numeric(.x)),
                         across(starts_with(c("year")), ~ as.character(.x)), 
                         respondent_frequency = "M") %>% 
                  filter(!if_all(everything(), is.na)))
  }

## Saving 923 Files --------

# create directory
if(!dir.exists(glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}"))){
  dir.create(glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}"), recursive = TRUE)
} else{
  print(glue::glue("Folder data/1_production_model/clean_data/eia/{params$eGRID_year} already exists."))
}

write_rds(dfs_923, glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))

# printing confirmation message
print(glue::glue("File eia_923_clean.RDS, containing dataframes {glue::glue_collapse(names(dfs_923), sep = ', ', last = ', and ')}, written to folder data/clean_data/eia/{params$eGRID_year}."))


# EIA-860 ----------------


## Annual 860 data ----------------------

if (length(list.files(eia_860_files)) > 0) { 

  ### 860 3_1_Generator -------
  
  generator_sheets <- c("Operable",
                        "Proposed",
                        "Retired and Canceled")
  
  file_name_generator <- grep("Generator_Y", eia_860_files, value = TRUE)
  
  generator_dfs <- 
    purrr::map(generator_sheets, 
               ~ read_excel(paste0(glue::glue("data/1_production_model/raw_data/860/{params$eGRID_year}/"), file_name_generator),
                            sheet = .x,
                            skip = 1,
                            na = c(".", "X"),
                            guess_max = 4000)) %>%
    purrr::map(., ~ .x %>% 
                 rename_with(tolower) %>% # lowercasing before applying clean_names() to avoid splitting on capital letters
                 janitor::clean_names()) %>%
    setNames(., janitor::make_clean_names(generator_sheets)) 
  
  #### Modifying 860 generator files ----------------
  
  epa_clean <- # need EPA plants to filter 860 proposed file
    readr::read_rds(glue::glue("data/1_production_model/clean_data/epa/{params$eGRID_year}/epa_clean_monthly.RDS")) %>% 
    select(plant_id) %>% distinct()
  
  generator_dfs_mod <-
    generator_dfs %>% 
    purrr::map_at("proposed", # only keeping proposed plants in EPA and removing if already in operable
                  ~ .x %>% 
                    filter(plant_code %in% epa_clean$plant_id,
                           !plant_code %in% generator_dfs$operable$plant_code, 
                           plant_code %in% manual_corrections$plant_id)) %>% 
    purrr::map_at("retired_and_canceled", # retaining only plants retired in eGRID year
                  ~ .x %>% 
                    filter(retirement_year == {params$eGRID_year}))
  
  
  ### 860 6_1_EnviroAssoc files ----------------
  enviro_assoc_sheets <- c("Boiler Generator",
                           "Boiler NOx",
                           "Boiler SO2",
                           "Boiler Mercury",
                           "Boiler Particulate Matter",
                           "Boiler Stack Flue",
                           "Emissions Control Equipment")
  
  file_name_enviro_assoc <- grep("EnviroAssoc_Y", eia_860_files, value = TRUE)
  
  enviro_assoc_dfs <- 
      purrr::map(enviro_assoc_sheets, 
        ~ read_excel(paste0(glue::glue("data/1_production_model/raw_data/860/{params$eGRID_year}/"), file_name_enviro_assoc),
          sheet = .x,
          skip = 1,
          na = ".",
          guess_max = 4000)) %>%
      purrr::map(., ~ .x %>% 
                   rename_with(tolower) %>% 
                   janitor::clean_names()) %>%
      setNames(., janitor::make_clean_names(tolower(enviro_assoc_sheets)))
  
  ### 860 6_2_EnviroEquip ---------------
  
  enviro_equip_sheets <- c("Emission Standards & Strategies",
                          "Boiler Info & Design Parameters",
                          "FGD", 
                          "Stack Flue")
  
  file_name_enviro_equip <- grep("EnviroEquip_Y", eia_860_files, value = TRUE)
  
  enviro_equip_dfs <- 
    purrr::map(enviro_equip_sheets, 
             ~ read_excel(paste0(glue::glue("data/1_production_model/raw_data/860/{params$eGRID_year}/"), file_name_enviro_equip),
                          sheet = .x,
                          skip = 1,
                          na = ".",
                          guess_max = 4000)) %>%
    purrr::map(., ~ .x %>% 
                 rename_with(tolower) %>%
                 janitor::clean_names()) %>%
    setNames(., janitor::make_clean_names(enviro_equip_sheets))
  
  
  ### 860 2__Plant_ --------------------
  
  file_name_plant <- grep("Plant_Y", eia_860_files, value = TRUE)
  
  plant_df <- 
    read_excel(paste0(glue::glue("data/1_production_model/raw_data/860/{params$eGRID_year}/"), file_name_plant),
                          sheet = "Plant",
                          skip = 1,
                          na = ".",
                          guess_max = 4000) %>% 
    rename_with(tolower) %>%
    janitor::clean_names()
  
  
  ### Puerto Rico 860m -----------------------
  
  pr_sheets <- c("Operating_PR",
                 "Retired_PR")
  
  names_860_PR_op <- 
    c("Utility ID" = "Entity ID",
      "Utility Name" = "Entity Name",
      "Plant Code" = "Plant ID",
      "Plant Name" = "Plant Name",
      "Sector Name" = "Sector",
      "State" = "Plant State",
      "Generator ID" = "Generator ID",
      "Unit Code" = "Unit Code",
      "Nameplate Capacity (MW)" = "Nameplate Capacity (MW)",
      "Summer Capacity (MW)" = "Net Summer Capacity (MW)",
      "Winter Capacity (MW)" = "Net Winter Capacity (MW)",
      "Technology" = "Technology",
      "Energy Source 1" = "Energy Source Code",
      "Prime Mover" = "Prime Mover Code",
      "Operating Month" = "Operating Month",
      "Operating Year" = "Operating Year",
      "Planned Retirement Month" = "Planned Retirement Month",
      "Planned Retirement Year" = "Planned Retirement Year",
      "Status" = "Status",
      "Planned Derate Year" = "Planned Derate Year",
      "Planned Derate Month" = "Planned Derate Month",
      "Planned Net Summer Capacity Derate (MW)" = "Planned Derate of Summer Capacity (MW)",
      "Planned Uprate Year" = "Planned Uprate Year",
      "Planned Uprate Month" = "Planned Uprate Month",
      "Planned Net Summer Capacity Uprate (MW)" = "Planned Uprate of Summer Capacity (MW)",
      "County" = "County")
  
  names_860_PR_ret <-
    c("Utility ID" = "Entity ID",
      "Utility Name" = "Entity Name",
      "Plant Code" = "Plant ID",
      "Plant Name" = "Plant Name",
      "Sector Name" = "Sector",
      "State" = "Plant State",
      "Generator ID" = "Generator ID",
      "Unit Code" = "Unit Code",
      "Nameplate Capacity (MW)" = "Nameplate Capacity (MW)",
      "Summer Capacity (MW)" = "Net Summer Capacity (MW)",
      "Winter Capacity (MW)" = "Net Winter Capacity (MW)",
      "Technology" = "Technology",
      "Energy Source 1" = "Energy Source Code",
      "Prime Mover" = "Prime Mover Code",
      "Retirement Month" = "Retirement Month",
      "Retirement Year" = "Retirement Year",
      "Operating Month" = "Operating Month",
      "Operating Year" = "Operating Year",
      "County" = "County",
      "Status" = "Status")
  
  
  puerto_rico_dfs <- 
    purrr::map(pr_sheets, 
               ~ read_excel(glue::glue("data/1_production_model/raw_data/860m/{params$eGRID_year}/eia_860m_december_generator{params$eGRID_year}.xlsx"),
                            sheet = .x,
                            skip = 2,
                            na = c(".", "X"),
                            guess_max = 4000)) %>%
    setNames(., janitor::make_clean_names(pr_sheets))
  
  
  #### 860m modifications -------------------
  
  puerto_rico_dfs_mod <- 
    puerto_rico_dfs %>%
    purrr::map_at("operating_pr", # modifying operating pr df
                  ~ .x %>% 
                    mutate(Status = str_extract(Status, "(?<=\\().*?(?=\\))")) %>% # updating status variable to match other files (extracting abb. inside of parantheses).
                    rename(any_of(names_860_PR_op))) %>% # renaming columns to match other forms based on lookup table
    purrr::map_at("retired_pr", # modifying retired pr data
                  ~ .x %>% 
                    filter("Retirement Year" == {params$eGRID_year}) %>% # filtering to only plants retired in eGRID year
                    rename(any_of(names_860_PR_ret))) %>% # renaming columns to match other 860 files.
    purrr::map(., ~ .x %>% 
                 rename_with(tolower) %>% 
                 janitor::clean_names()) # converting column names in both files to snake_case and lower
  
  
  ### Create 860 combined file ----------------
  
  rename_cols_860 <- c("plant_id" = "plant_code",
                       "plant_state" = "state",
                       "nameplate_capacity" = "nameplate_capacity_mw")
  
  
  dfs_860 <-
    c(generator_dfs_mod,
      enviro_assoc_dfs,
      enviro_equip_dfs,
      "plant" = list(plant_df),
      puerto_rico_dfs_mod) %>% 
    purrr:::map(., ~ .x %>%  # standardizing column types across all dfs
                  mutate(across(ends_with(c("id","code")), ~ as.character(.x)),
                         across(contains("capacity"), ~ as.numeric(.x)),
                         across(contains(c("month", "year")), ~ as.character(.x))) %>% 
                  rename(any_of(rename_cols_860)) %>% # standardizing col names to match other files
                  filter(!if_all(everything(), is.na))) # removing empty rows
  
  
  eia_860_combined <-  
    bind_rows(dfs_860$operable,
              dfs_860$proposed, 
              dfs_860$retired_and_canceled,
              dfs_860$operating_pr,
              dfs_860$retired_pr) %>% 
    select(any_of(names(dfs_860$operable)), retirement_year) # keeping only columns in operable file and retirement year from retired and canceled
  
  
  dfs_860_final <-
    c(dfs_860,
      "combined" = list(eia_860_combined))

## Monthly 860 data ------------------------
  
} else { # if EIA-860 Annual is not available, default to monthly data 

  monthly_860_sheets <- 
    c("Operating", 
      "Planned", 
      "Retired",
      "Operating_PR", 
      "Retired_PR")
  
  names_860m_op <- 
    c("Utility ID" = "Entity ID",
      "Utility Name" = "Entity Name",
      "Plant Code" = "Plant ID",
      "Plant Name" = "Plant Name",
      "Sector Name" = "Sector",
      "State" = "Plant State",
      "Generator ID" = "Generator ID",
      "Unit Code" = "Unit Code",
      "Nameplate Capacity (MW)" = "Nameplate Capacity (MW)",
      "Summer Capacity (MW)" = "Net Summer Capacity (MW)",
      "Winter Capacity (MW)" = "Net Winter Capacity (MW)",
      "Technology" = "Technology",
      "Energy Source 1" = "Energy Source Code",
      "Prime Mover" = "Prime Mover Code",
      "Operating Month" = "Operating Month",
      "Operating Year" = "Operating Year",
      "Planned Retirement Month" = "Planned Retirement Month",
      "Planned Retirement Year" = "Planned Retirement Year",
      "Status" = "Status",
      "Planned Derate Year" = "Planned Derate Year",
      "Planned Derate Month" = "Planned Derate Month",
      "Planned Net Summer Capacity Derate (MW)" = "Planned Derate of Summer Capacity (MW)",
      "Planned Uprate Year" = "Planned Uprate Year",
      "Planned Uprate Month" = "Planned Uprate Month",
      "Planned Net Summer Capacity Uprate (MW)" = "Planned Uprate of Summer Capacity (MW)",
      "County" = "County")
  
  names_860m_planned <- 
    c("Utility ID" = "Entity ID",
      "Utility Name" = "Entity Name",
      "Plant Code" = "Plant ID",
      "Plant Name" = "Plant Name",
      "Sector Name" = "Sector",
      "State" = "Plant State",
      "Generator ID" = "Generator ID",
      "Unit Code" = "Unit Code",
      "Nameplate Capacity (MW)" = "Nameplate Capacity (MW)",
      "Summer Capacity (MW)" = "Net Summer Capacity (MW)",
      "Winter Capacity (MW)" = "Net Winter Capacity (MW)",
      "Technology" = "Technology",
      "Energy Source 1" = "Energy Source Code",
      "Prime Mover" = "Prime Mover Code",
      "Planned Operation Month" = "Planned Operation Month",
      "Planned Operation Year" = "Planned Operation Year",
      "Status" = "Status",
      "County" = "County")
  
  names_860m_ret <-
    c("Utility ID" = "Entity ID",
      "Utility Name" = "Entity Name",
      "Plant Code" = "Plant ID",
      "Plant Name" = "Plant Name",
      "Sector Name" = "Sector",
      "State" = "Plant State",
      "Generator ID" = "Generator ID",
      "Unit Code" = "Unit Code",
      "Nameplate Capacity (MW)" = "Nameplate Capacity (MW)",
      "Summer Capacity (MW)" = "Net Summer Capacity (MW)",
      "Winter Capacity (MW)" = "Net Winter Capacity (MW)",
      "Technology" = "Technology",
      "Energy Source 1" = "Energy Source Code",
      "Prime Mover" = "Prime Mover Code",
      "Retirement Month" = "Retirement Month",
      "Retirement Year" = "Retirement Year",
      "Operating Month" = "Operating Month",
      "Operating Year" = "Operating Year",
      "County" = "County",
      "Status" = "Status")
  
  sheet_col_types <- list( # specify column types for each sheet
    "Operating" = c(
      "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", 
      "text", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", 
      "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
    ), 
    "Planned" = c(
      "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", 
      "text", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric",
      "text", "numeric", "numeric"
    ), 
    "Retired" = c(
      "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", 
      "text", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
    ),
    "Operating_PR" = c(
      "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", 
      "text", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", 
      "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
    ), 
    "Retired_PR" = c(
      "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", 
      "text", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
    ))
  
  ### 860m January ---------------------------
  # start with January and add any generators from other months if they do not exist in January data
  january_860m_dfs <- 
    purrr::map(monthly_860_sheets, 
               ~ read_excel(paste0(glue::glue("data/1_production_model/raw_data/860m/{params$eGRID_year}/eia_860m_january_generator{params$eGRID_year}.xlsx")), 
                            sheet = .x, 
                            skip = 2, 
                            na = c(".", "X", "", " "), 
                            col_types = sheet_col_types$.x)) %>%
    setNames(., janitor::make_clean_names(monthly_860_sheets))
  
  epa_clean <- # need EPA plants to filter 860 proposed file
    readr::read_rds(glue::glue("data/1_production_model/clean_data/epa/{params$eGRID_year}/epa_clean_monthly.RDS")) %>% 
    select(plant_id) %>% distinct() 
  
  eia_860m_mod <- 
    january_860m_dfs %>% 
    purrr::map_at("operating", 
                  ~ .x %>% 
                    mutate(Status = str_extract(Status, "(?<=\\().*?(?=\\))")) %>% 
                    rename(any_of(names_860m_op))) %>% # extract abbreviated status 
    purrr::map_at("planned", 
                  ~ .x %>% 
                    filter(`Plant ID` %in% epa_clean$plant_id,
                           !`Plant ID` %in% january_860m_dfs$operating$`Plant ID`, 
                           `Plant ID` %in% manual_corrections$plant_id) %>% 
                    mutate(Status = str_extract(Status, "(?<=\\().*?(?=\\))")) %>% 
                    rename(any_of(names_860m_planned))) %>% 
    purrr::map_at("retired", 
                  ~ .x %>% 
                    filter(`Retirement Year` == {params$eGRID_year}) %>% 
                    rename(any_of(names_860m_ret))) %>% 
    purrr::map_at("operating_pr", 
                  ~ .x %>% 
                    mutate(Status = str_extract(Status, "(?<=\\().*?(?=\\))")) %>% 
                    rename(any_of(names_860m_op))) %>% 
    purrr::map_at("retired_pr", 
                  ~ .x %>% 
                    rename(any_of(names_860m_ret))) %>% 
    purrr::map(., ~ .x %>% 
                 rename_with(tolower) %>% # lowercasing before applying clean_names() to avoid splitting on capital letters
                 janitor::clean_names() %>% # convert entity_id to character for consistency across data
                 filter(nchar(utility_id) <= 5) %>% # excluding any "NOTES" section that populates in utility_id
                 mutate(utility_id = as.character(utility_id),
                        unit_code = as.character(unit_code))) 
  
  eia_860m_combined <- 
    bind_rows(eia_860m_mod$operating, 
              eia_860m_mod$retired, 
              eia_860m_mod$operating_pr, 
              eia_860m_mod$retired_pr)
  
  dfs_860 <- 
    eia_860m_mod
  
  ## Add other available 860m months ----------------
  for (month in tolower(month.name[2:12])) { 
    file_name_860m = glue::glue("eia_860m_{month}_generator{params$eGRID_year}.xlsx")
    
    if (file.exists(glue::glue("data/1_production_model/raw_data/860m/{params$eGRID_year}/{file_name_860m}"))) { 
      monthly_860_dfs <- 
        purrr::map(monthly_860_sheets, 
                   ~ read_excel(glue::glue("data/1_production_model/raw_data/860m/{params$eGRID_year}/{file_name_860m}"), 
                                sheet = .x, 
                                skip = 2, 
                                na = c(".", "X", "", " "), 
                                col_types = sheet_col_types$.x)) %>%
        setNames(., janitor::make_clean_names(monthly_860_sheets))
      
      monthly_860_mod <- 
        monthly_860_dfs %>% 
        purrr::map_at("operating", 
                      ~ .x %>% 
                        mutate(Status = str_extract(Status, "(?<=\\().*?(?=\\))")) %>% 
                        rename(any_of(names_860m_op))) %>% # extract abbreviated status 
        purrr::map_at("planned", 
                      ~ .x %>% 
                        filter(`Plant ID` %in% epa_clean$plant_id,
                               !`Plant ID` %in% monthly_860_dfs$operating$`Plant ID`, 
                               `Plant ID` %in% manual_corrections$plant_id) %>% 
                        mutate(Status = str_extract(Status, "(?<=\\().*?(?=\\))")) %>% 
                        rename(any_of(names_860m_planned))) %>% 
        purrr::map_at("retired", 
                      ~ .x %>% 
                        filter(`Retirement Year` == {params$eGRID_year}) %>% 
                        rename(any_of(names_860m_ret))) %>% 
        purrr::map_at("operating_pr", 
                      ~ .x %>% 
                        mutate(Status = str_extract(Status, "(?<=\\().*?(?=\\))")) %>% 
                        rename(any_of(names_860m_op))) %>% 
        purrr::map_at("retired_pr", 
                      ~ .x %>% 
                        rename(any_of(names_860m_ret))) %>% 
        purrr::map(., ~ .x %>% 
                     rename_with(tolower) %>% # lowercasing before applying clean_names() to avoid splitting on capital letters
                     janitor::clean_names() %>% # convert entity_id to character for consistency across data
                     filter(nchar(utility_id) <= 5) %>% # excluding any "NOTES" section that populates in utility_id
                     mutate(utility_id = as.character(utility_id),
                            unit_code = as.character(unit_code))) 
      
      eia_860m_combined <- 
        bind_rows(eia_860m_combined, # only add generators that are not already in eia_860m_combined
                  monthly_860_mod$operating %>% filter(!paste0(plant_code, "_", generator_id) %in% paste0(eia_860m_combined$plant_code, "_", eia_860m_combined$generator_id)),
                  monthly_860_mod$planned %>% filter(!paste0(plant_code, "_", generator_id) %in% paste0(eia_860m_combined$plant_code, "_", eia_860m_combined$generator_id)), 
                  monthly_860_mod$retired %>% filter(!paste0(plant_code, "_", generator_id) %in% paste0(eia_860m_combined$plant_code, "_", eia_860m_combined$generator_id)),
                  monthly_860_mod$operating_pr %>% filter(!paste0(plant_code, "_", generator_id) %in% paste0(eia_860m_combined$plant_code, "_", eia_860m_combined$generator_id)),
                  monthly_860_mod$retired_pr %>% filter(!paste0(plant_code, "_", generator_id) %in% paste0(eia_860m_combined$plant_code, "_", eia_860m_combined$generator_id))) %>% 
        rows_update(monthly_860_mod$retired %>% # update retirement month and year if a generator retired in the eGRID_year
                      select(plant_code, generator_id, retirement_month, retirement_year) %>% 
                      mutate(status = "RE"), 
                    by = c("plant_code", "generator_id")) %>% 
        rows_update(monthly_860_mod$retired_pr %>% # update retirement month and year if a generator retired in the eGRID_year
                      select(plant_code, generator_id, retirement_month, retirement_year) %>% 
                      mutate(status = "RE"),
                    by = c("plant_code", "generator_id")) 
    
      dfs_860 <- 
        dfs_860 %>% 
        purrr::map_at("operating", 
                      ~ bind_rows(.x, 
                                  monthly_860_mod$operating %>% filter(!plant_code %in% .x$plant_code))) %>% 
        purrr::map_at("planned", 
                      ~ bind_rows(.x, 
                                  monthly_860_mod$planned %>% filter(!plant_code %in% .x$plant_code))) %>% 
        purrr::map_at("retired", 
                      ~ bind_rows(.x, 
                                  monthly_860_mod$planned %>% filter(!plant_code %in% .x$plant_code))) %>% 
        purrr::map_at("operating_pr", 
                      ~ bind_rows(.x, 
                                  monthly_860_mod$operating_pr %>% filter(!plant_code %in% .x$plant_code))) %>% 
        purrr::map_at("retired_pr", 
                      ~ bind_rows(.x, 
                                  monthly_860_mod$retired_pr %>% filter(!plant_code %in% .x$plant_code)))
      
    }} 
  
  eia_860m_combined_clean <- 
    eia_860m_combined %>% 
    select(any_of(names(dfs_860$operating)), retirement_year) %>%
    distinct()

  ### Create 860 combined file ----------------
  
  rename_cols_860 <- c("plant_id" = "plant_code",
                       "plant_state" = "state",
                       "nameplate_capacity" = "nameplate_capacity_mw")
  
  dfs_860_2 <-
    c(dfs_860,
      "combined" = list(eia_860m_combined_clean)) %>% 
    purrr:::map(., ~ .x %>%  # standardizing column types across all dfs
                  mutate(across(ends_with(c("id","code")), ~ as.character(.x)),
                         across(contains("capacity"), ~ as.numeric(.x)),
                         across(contains(c("month", "year")), ~ as.character(.x))) %>% 
                  rename(any_of(rename_cols_860)) %>% # standardizing col names to match other files
                  filter(!if_all(everything(), is.na))) # removing empty rows
  
  # remove generators that have been retired or started operating within the year
  dfs_860_final <- 
    dfs_860_2 %>% 
    purrr::map_at("operating", 
                  ~ .x %>% 
                    filter(!paste0(plant_id, "_", generator_id) %in% paste0(dfs_860_2$retired$plant_id, "_", dfs_860_2$retired$generator_id))) %>% 
    purrr::map_at("planned", 
                  ~ .x %>% 
                    filter(!paste0(plant_id, "_", generator_id) %in% paste0(dfs_860_2$operating$plant_id, "_", dfs_860_2$operating$generator_id), 
                           !paste0(plant_id, "_", generator_id) %in% paste0(dfs_860_2$retired$plant_id, "_", dfs_860_2$retired$generator_id)))
  
  names_860 <- # rename sheets to be consistent with annual 860 version
    c("operable", "proposed", "retired_and_canceled", "operating_pr", "retired_pr", "combined")
  names(dfs_860_final) <- names_860
  
}

## Saving 860 files ----------

write_rds(dfs_860_final, glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS"))

# printing confirmation message
print(glue::glue("File eia_860_clean.RDS, containing dataframes {glue::glue_collapse(names(dfs_860_final), sep = ', ', last = ', and ')}, written to folder data/clean_data/eia/{params$eGRID_year}."))


# EIA-861 -------------

if (length(list.files(eia_861_files)) > 1) { # check if 861 for the data year exists. If it does not, skip. 
  
  ## 861 Balancing authority  ----------
  
  rename_cols_861 <- # creating list of variable name mappings
    c("year" = "data_year")
  
  file_name_ba <- grep("Balancing_Authority", eia_861_files, value = TRUE)
  
  balancing_authority <-  
    read_excel(paste0(glue::glue("data/1_production_model/raw_data/861/{params$eGRID_year}/"), file_name_ba), 
               sheet = "Balancing Authority",
               guess_max = 4000,
               na = "."
               ) %>% 
    rename_with(tolower) %>%
    janitor::clean_names() 
  
  ## 861 Sales Ult Cust --------
  
  file_name_sales_ult_cust <- grep("Sales_Ult_Cust", eia_861_files, value = TRUE)[1]
  
  sales_ult_cust <-
    read_excel(paste0(glue::glue("data/1_production_model/raw_data/861/{params$eGRID_year}/"), file_name_sales_ult_cust),
               sheet = "States",
               skip = 2,
               guess_max = 4000,
               na = "."
               ) %>% 
    rename_with(tolower) %>%
    janitor::clean_names() %>% 
    select(1:ba_code) %>% # removing all columns after ba_code
    rename("data_type" = contains("data_type")) # renaming long col name that contains "data_type" to just "data_type"
  
  
    
  ## 861 Utility Data ---------
  
  file_name_utility <- grep("Utility_Data", eia_861_files, value = TRUE)
  
  utility_data <-
    read_excel(glue::glue("data/1_production_model/raw_data/861/{params$eGRID_year}/Utility_Data_{params$eGRID_year}.xlsx"),
               sheet = "States",
               skip = 1,
               guess_max = 4000,
               na = "."
    ) %>% 
    rename_with(tolower) %>%
    janitor::clean_names() %>% 
    mutate(across(caiso:other, ~ if_else(is.na(.x), 0, 1)), # creating count of ISORTO territories
           isorto_total = rowSums(pick(caiso:other)),
           isorto_count = case_when(
             isorto_total == 0 ~ "0",
             isorto_total == 1 ~ "1",
             isorto_total == 2 ~ "2",
             TRUE ~ NA_character_
           )) %>% 
    select(data_year:nerc_region, isorto_count)  # keeping necessary columns
  
  
  ## Combining 861 data ----------
  
  dfs_861 <-
    c("balancing_authority" = list(balancing_authority),
      "sales_ult_cust" = list(sales_ult_cust),
      "utility_data" = list(utility_data)) %>% 
    purrr::map(., ~ .x %>% rename(any_of(rename_cols_861)))
  
  ## Saving 861 Files -----------
  
  write_rds(dfs_861, glue::glue("data/1_production_model/clean_data/eia/{params$eGRID_year}/eia_861_clean.RDS"))
  
  # printing confirmation message
  print(glue::glue("File eia_861_clean.RDS, containing dataframes {glue::glue_collapse(names(dfs_861), sep = ', ', last = ', and ')}, written to folder data/clean_data/eia/{params$eGRID_year}."))
} else { 
  print(glue::glue("EIA-861 Files do not exist. Either run data_load_eia.R to obtain or files are not yet available for {params$eGRID_year}."))}
