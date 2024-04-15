library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(glue)



# EIA-923 ------------

## 923 Schedules_2_3_4_5_M_12 --------

sheets_923_1 <- c("Page 1 Generation and Fuel Data",
                  "Page 1 Puerto Rico",
                  "Page 3 Boiler Fuel Data",
                  "Page 4 Generator Data")


sched_2_3_4_5_m_12_dfs <- 
  purrr::map2(sheets_923_1, 
              c(5,6,5,5),
             ~ read_excel(glue::glue("data/raw_data/923/EIA923_Schedules_2_3_4_5_M_12_{Sys.getenv('eGRID_year')}_Final_Revision.xlsx"),
                          sheet = .x,
                          skip = .y,
                          na = ".",
                          guess_max = 4000)) %>%
  purrr::map(., ~ janitor::clean_names(.x)) %>%
  setNames(., janitor::make_clean_names(str_replace_all(sheets_923_1, "Page \\d+ ", ""))) %>% # storing df names without Page #s
  purrr::map_at("puerto_rico",
                ~ .x %>% 
                  rename("reserved" = "reserved_10", # fixing issue of two "Reserved" columns. Need to figure out better way in case they're not 10 and 17
                         "balancing_authority_code" = "reserved_17"))

### Adding Puerto Rico data to Gen and fuel ----

gen_fuel_combined <-
  bind_rows(sched_2_3_4_5_m_12_dfs$generation_and_fuel_data,
            sched_2_3_4_5_m_12_dfs$puerto_rico)


## EIA923_Schedule_8_Annual_Environmental_Information ------


air_emissions_control_info <- 
 read_excel(glue::glue("data/raw_data/923/EIA923_Schedule_8_Annual_Environmental_Information_{Sys.getenv('eGRID_year')}_Final_Revision.xlsx"),
                           sheet = "8C Air Emissions Control Info",
                           skip = 4,
                           guess_max = 4000) %>%
  janitor::clean_names() 


## Combining 923 files 

rename_cols_923 <- c("prime_mover" = "reported_prime_mover", # creating character vector to rename selected columns in 923 files
                     "fuel_type" = "reported_fuel_type_code")

dfs_923 <- c(sched_2_3_4_5_m_12_dfs,
             "air_emissions_control_info" = list(air_emissions_control_info),
             "generation_and_fuel_combined" = list(gen_fuel_combined)) %>% 
  purrr:::map(., ~ .x %>%  # standardizing column types across all dfs
                mutate(across(ends_with("id"), ~ as.character(.x)),
                       across(contains("capacity"), ~ as.numeric(.x)),
                       across(contains(c("month", "year")), ~ as.character(.x))) %>% 
                rename(any_of(rename_cols_923)) %>% # standardizing col names to match other files
                filter(!if_all(everything(), is.na)))



## Saving 923 Files --------

write_rds(dfs_923, "data/clean_data/eia_923_clean.RDS")

# printing confirmation message
print(glue::glue("File eia_923_clean.RDS, containing dataframes {glue::glue_collapse(names(dfs_923), sep = ', ', last = ', and ')}, written to folder data/clean_data."))


# EIA-860 ----------------

## 860 3_1_Generator -------

generator_sheets <- c("Operable",
                      "Proposed",
                      "Retired and Canceled")

generator_dfs <- 
  purrr::map(generator_sheets, 
             ~ read_excel(glue::glue("data/raw_data/860/3_1_Generator_Y{Sys.getenv('eGRID_year')}.xlsx"),
                          sheet = .x,
                          skip = 1,
                          na = c(".", "X"),
                          guess_max = 4000)) %>%
  purrr::map(., ~ janitor::clean_names(.x)) %>%
  setNames(., janitor::make_clean_names(generator_sheets)) 

### Modifying 860 generator files ---------

camd_clean <- readr::read_rds("data/clean_data/camd/camd_clean.RDS") # need camd plants to filter 860 proposed file

generator_dfs_mod <-
  generator_dfs %>% 
  purrr::map_at("proposed", # only keeping proposed plants in camd and removing if already in operable
                ~ .x %>% 
                  filter(plant_code %in% camd_clean$plant_id,
                         !plant_code %in% generator_dfs$operable$plant_code)) %>% 
  purrr::map_at("retired and canceled", # retaining only plants retired in eGRID year
                ~ .x %>% 
                  filter(retirement_year == Sys.getenv("eGRID_year")))




## 860 6_1_EnviroAssoc files ----
enviro_assoc_sheets <- c("Boiler Generator",
                         "Boiler NOx",
                         "Boiler SO2",
                         "Boiler Mercury",
                         "Boiler Particulate Matter",
                         "Emissions Control Equipment")

enviro_assoc_dfs <- 
    purrr::map(enviro_assoc_sheets, 
      ~ read_excel(glue::glue("data/raw_data/860/6_1_EnviroAssoc_Y{Sys.getenv('eGRID_year')}.xlsx"),
        sheet = .x,
        skip = 1,
        na = ".",
        guess_max = 4000)) %>%
    purrr::map(., ~ janitor::clean_names(.x)) %>%
    setNames(., janitor::make_clean_names(enviro_assoc_sheets))

## 860 6_2_EnviroEquip --------

enviro_equip_sheets <- c("Emission Standards & Strategies",
                        "Boiler Info & Design Parameters",
                        "FGD")

enviro_equip_dfs <- 
  purrr::map(enviro_equip_sheets, 
           ~ read_excel(glue::glue("data/raw_data/860/6_2_EnviroEquip_Y{Sys.getenv('eGRID_year')}.xlsx"),
                        sheet = .x,
                        skip = 1,
                        na = ".",
                        guess_max = 4000)) %>%
  purrr::map(., ~ janitor::clean_names(.x)) %>%
  setNames(., janitor::make_clean_names(enviro_equip_sheets))


## 860 2__Plant_ -------

plant_df <- 
  read_excel(glue::glue("data/raw_data/860/2___Plant_Y{Sys.getenv('eGRID_year')}.xlsx"),
                        sheet = "Plant",
                        skip = 1,
                        na = ".",
                        guess_max = 4000) %>% 
  clean_names()


## 860m -------------

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
  c(
    "Utility ID" = "Entity ID",
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
             ~ read_excel(glue::glue("data/raw_data/860m.xlsx"),
                          sheet = .x,
                          skip = 2,
                          na = c(".", "X"),
                          guess_max = 4000)) %>%
  #purrr::map(., ~ janitor::clean_names(.x)) %>%
  setNames(., janitor::make_clean_names(pr_sheets))


### 860m modifications ------------

puerto_rico_dfs_mod <- 
  puerto_rico_dfs %>%
  purrr::map_at("operating_pr", # modifying optaring pr df
                ~ .x %>% 
                  mutate(Status = str_extract(Status, "(?<=\\().*?(?=\\))")) %>% # updating status variable to match other files (extracting abb. inside of parantheses).
                  rename(any_of(names_860_PR_op))) %>% # renaming columns to match other forms based on lookup table
  purrr::map_at("retired_pr", # modifying retired pr data
                ~ .x %>% 
                  filter("Retirement Year" == Sys.getenv("eGRID_year")) %>% # filtering to only plants retired in eGRID year
                  rename(any_of(names_860_PR_ret))) %>% # renaming columns to match other 860 files.
  purrr::map(., ~ clean_names(.x)) # converting column names in both files to snake_case and lower


## create 860 combined file -------------

  
  
## Combining 860 files ---------

rename_cols_860 <- c("plant_id" = "plant_code",
                     "plant_state" = "state",
                     "nameplate_capacity" = "nameplate_capacity_mw")


dfs_860 <-
  c(generator_dfs,
    enviro_assoc_dfs,
    enviro_equip_dfs,
    "plant" = list(plant),
    puerto_rico_dfs_mod) %>% 
  purrr:::map(., ~ .x %>%  # standardizing column types across all dfs
                mutate(across(ends_with("id"), ~ as.character(.x)),
                       across(contains("capacity"), ~ as.numeric(.x)),
                       across(contains(c("month", "year")), ~ as.character(.x))) %>% 
                rename(any_of(rename_cols_860)) %>% # standardizing col names to match other files
                filter(!if_all(everything(), is.na))) # removing empty rows



### creating 860 combined file -----------

eia_860_combined <- 
  bind_rows(dfs_860$operable,
            dfs_860$retired_and_canceled,
            dfs_860$operating_pr,
            dfs_860$retired_pr) %>% 
  select(any_of(names(dfs_860$operable))) # keeping only columns in operable file


dfs_860_final <-
  c(dfs_860,
    "combined" = list(eia_860_combined))

## Saving 860 files ----------

write_rds(dfs_860_final, "data/clean_data/eia_860_clean.RDS")

# printing confirmation message
print(glue::glue("File eia_860_clean.RDS, containing dataframes {glue::glue_collapse(names(dfs_860_final), sep = ', ', last = ', and ')}, written to folder data/clean_data."))



# EIA-861 -------------

## 861 Balancing authority  ----------

rename_cols_861 <- # creating list of variable name mappings
  c("year" = "data_year")

balancing_authority <-  
  read_excel(glue::glue("data/raw_data/861/Balancing_Authority_{Sys.getenv('eGRID_year')}.xlsx"), 
             sheet = "Balancing Authority",
             guess_max = 4000,
             na = "."
             ) %>% 
  clean_names() 

## 861 Sales Ult Cust --------

sales_ult_cust <-
  read_excel(glue::glue("data/raw_data/861/Sales_Ult_Cust_{Sys.getenv('eGRID_year')}.xlsx"),
             sheet = "States",
             skip = 2,
             guess_max = 4000,
             na = "."
             ) %>% 
  clean_names() %>% 
  select(1:ba_code) %>% # removing all columns after ba_code
  rename("data_type" = contains("data_type")) # renaming long col name that contains "data_type" to just "data_type"


  
## 861 Utility Data ---------

utility_data <-
  read_excel(glue::glue("data/raw_data/861/Utility_Data_{Sys.getenv('eGRID_year')}.xlsx"),
             sheet = "States",
             skip = 1,
             guess_max = 4000,
             na = "."
  ) %>% 
  clean_names() %>% 
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

write_rds(dfs_861, "data/clean_data/eia_861_clean.RDS")

# printing confirmation message
print(glue::glue("File eia_861_clean.RDS, containing dataframes {glue::glue_collapse(names(dfs_861), sep = ', ', last = ', and ')}, written to folder data/clean_data."))
