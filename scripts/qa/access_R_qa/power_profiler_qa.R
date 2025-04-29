## -------------------------------
##
## Note: this file is for internal QA only.
##
## Power Profiler
## 
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the power profiler files in a given year. 
##
## The checks performed will output a CSV file with any differences 
## between Access and R plant files. 
## 
## Additional notes
##      
##      Teagan Goforth, Abt Global
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries -----
library(dplyr)
library(readr)
library(readxl)
library(stringr)


# Define eGRID year parameter ----------------
# define parameter year if no one is currently assigned using prompted user input
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

# Create save directory for QA outputs -----

if(dir.exists("data/2a_power_profiler/outputs/qa")) {
  print("Folder qa already exists.")
}else{
  dir.create("data/2a_power_profiler/outputs/qa")
}

if(dir.exists(glue::glue("data/2a_power_profiler/outputs/qa/{params$eGRID_year}"))) {
  print(glue::glue("Folder qa/{params$eGRID_year} already exists."))
}else{
  dir.create(glue::glue("data/2a_power_profiler/outputs/qa/{params$eGRID_year}"))
}

if(dir.exists(glue::glue("data/2a_power_profiler/outputs/qa/{params$eGRID_year}/power_profiler_differences"))) {
  print(glue::glue("Folder qa/{params$eGRID_year}/power_profiler_differences already exists."))
}else{
  dir.create(glue::glue("data/2a_power_profiler/outputs/qa/{params$eGRID_year}/power_profiler_differences"))
}

# set directory for saving files 
save_dir <- glue::glue("data/2a_power_profiler/outputs/qa/{params$eGRID_year}/power_profiler_differences/")

## Create function to save file differences -----
save_diffs <- function(datacheck) {
  if(nrow(datacheck) > 0) {
    write_csv(datacheck, paste0(save_dir, deparse(substitute(datacheck)), ".csv")) }
}

print("POWER PROFILER QA IN PROGRESS")

# ZIPCODE UTILITY SUBREGION DATA -----
## Import Access data ------
access_zip_utility <- read_excel(glue::glue("data/2a_power_profiler/static_tables/qa/{params$eGRID_year}/ZipSubregion{params$eGRID_year}.xlsx"), 
                         sheet = "ZipSubregion for Website",
                         col_names = TRUE,
                         col_types = c("text", "text", "text", "text", "text", "numeric")) %>%
  janitor::clean_names() %>%
  glimpse()

# add "_access" after each variable to easily identify dataset 
colnames(access_zip_utility) <- paste0(colnames(access_zip_utility), "_access")

# replace any "NA" strings with an NA
access_zip_utility[access_zip_utility == "NA"] <- NA_character_


## Import R data ------
r_zip_utility <- read_rds(glue::glue("data/2a_power_profiler/outputs/{params$eGRID_year}/zip_utility_subregion.RDS")) %>%
  glimpse()

# add "_r" after each variable to easily identify dataset 
colnames(r_zip_utility) <- paste0(colnames(r_zip_utility), "_r")

## Combine two datasets for comparison -----

zip_utility_comparison <-
  r_zip_utility %>%
  full_join(access_zip_utility, by = c("zip_r" = "zip_access", "eiaid_r" = "eiaid_access")) %>%
  glimpse()

## Difference checks ------
### Plant presence ------

# Records in Access not in R
check_zip_utility_records_missing_from_r <- #61
  access_zip_utility %>%
  anti_join(r_zip_utility, by = c("zip_access" = "zip_r", "eiaid_access" = "eiaid_r")) %>%
  filter(!is.na(eiaid_access)) %>%
  glimpse()
save_diffs(check_zip_utility_records_missing_from_r)

# Records in R not in Access
check_zip_utility_records_missing_from_access <- #35
  r_zip_utility %>%
  anti_join(access_zip_utility, by = c("zip_r" = "zip_access", "eiaid_r" = "eiaid_access")) %>%
  filter(!is.na(eiaid_r)) %>%
  glimpse()
save_diffs(check_zip_utility_records_missing_from_access)

### Utility states -----
check_zip_utility_state <-
  zip_utility_comparison %>%
  filter(state_r != state_access) %>%
  select(eiaid_r, state_r, state_access) %>% distinct() %>%
  glimpse()
save_diffs(check_zip_utility_state)

### Utility name ------
check_zip_utility_utility_name <-
  zip_utility_comparison %>%
  filter(utility_name_r != utility_name_access) %>%
  select(eiaid_r, utility_name_r, utility_name_access) %>% distinct() %>%
  glimpse()
save_diffs(check_zip_utility_utility_name)

### Utility subregion ------
check_zip_utility_subregion <-
  zip_utility_comparison %>%
  filter(subregion_r != subregion_access) %>%
  select(eiaid_r, subregion_r, subregion_access) %>% distinct() %>%
  glimpse()
save_diffs(check_zip_utility_subregion)

### Utility predominant utility assignment -----
check_zip_utility_predominant_utility <-
  zip_utility_comparison %>%
  filter(predominant_utility_r != predominant_utility_access) %>%
  select(eiaid_r, subregion_r, subregion_access) %>% distinct() %>%
  glimpse()
save_diffs(check_zip_utility_predominant_utility)

### Identify all unique plant IDs that have differences ------------

# grab check files in QA folder
check_files <- grep("check", dir(save_dir), value = TRUE)
# exclude those potentially saved from subregion assignments
check_files_2 <- grep("subregion_assign", check_files, invert = TRUE, value = TRUE)

#### Regular checks -----
# do not include missing values
non_access_files <- grep("missing_from_r", check_files_2, invert = TRUE, value = TRUE)

if(length(non_access_files) != 0) {
  # combine checked files
  zip_utility_diffs <-
    purrr::map_df(paste0(save_dir, non_access_files),
                  ~read_csv(.x, col_types = cols(.default = col_character()))) %>%
    select(eiaid_r) %>%
    distinct() %>%
    mutate(source_diff = "zip_utility_file")

  write_csv(zip_utility_diffs, paste0(save_dir, "zip_utility_difference_ids.csv"))
}

#### Missing from R values ------
missing_r_files <- grep("missing_from_r", check_files_2, value = TRUE)
if(length(missing_r_files) != 0) {
  # combine checked files
  zip_missing_diffs <-
    purrr::map_df(paste0(save_dir, missing_r_files),
                  ~read_csv(.x, col_types = cols(.default = col_character()))) %>%
    select(eiaid_access) %>%
    distinct() %>%
    mutate(source_diff = "zip_utility_file")
  
  write_csv(zip_missing_diffs, paste0(save_dir, "zip_utility_missing_r_ids.csv"))
}

# ZIPCODE SUBREGION ASSIGNMENTS -----
## Import Access data ------
access_subregion_assign <- read_excel(glue::glue("data/2a_power_profiler/static_tables/qa/{params$eGRID_year}/ZipSubregion{params$eGRID_year}.xlsx"), 
                              sheet = "ZipSubregion for Excel Tool",
                              col_names = TRUE,
                              col_types = c("text", "text", "text", "text", "text", "text")) %>%
  janitor::clean_names() %>%
  rename(zip = zip_character,
         subregion = e_grid_subregion_number_1,
         subregion_secondary = e_grid_subregion_number_2,
         subregion_tertiary = e_grid_subregion_number_3) %>%
  glimpse()

# add "_access" after each variable to easily identify dataset 
colnames(access_subregion_assign) <- paste0(colnames(access_subregion_assign), "_access")

# replace any "NA" strings with an NA
access_subregion_assign[access_subregion_assign == "NA"] <- NA_character_

## Import R data ------
r_subregion_assign <- read_rds(glue::glue("data/2a_power_profiler/outputs/{params$eGRID_year}/zip_subregion_assignments.RDS")) %>%
  glimpse()

# add "_r" after each variable to easily identify dataset 
colnames(r_subregion_assign) <- paste0(colnames(r_subregion_assign), "_r")

## Combine two datasets for comparison -----
subregion_assign_comparison <-
  r_subregion_assign %>%
  full_join(access_subregion_assign, by = c("zip_r" = "zip_access", "subregion_r" = "subregion_access")) %>%
  glimpse()

## Difference checks ------
### Plant presence ------

# Records in Access not in R
check_subregion_assign_records_missing_from_r <-
  access_subregion_assign %>%
  anti_join(r_subregion_assign, by = c("zip_access" = "zip_r")) %>%
  filter(!is.na(zip_access)) %>%
  glimpse()
save_diffs(check_subregion_assign_records_missing_from_r)

# Records in R not in Access
check_subregion_assign_records_missing_from_access <-
  r_subregion_assign %>%
  anti_join(access_subregion_assign, by = c("zip_r" = "zip_access")) %>%
  filter(!is.na(zip_r)) %>%
  glimpse()
save_diffs(check_subregion_assign_records_missing_from_access)

### Subregion states -----
check_subregion_assign_state <-
  subregion_assign_comparison %>%
  filter(state_r != state_access) %>%
  select(zip_r, state_r, state_access) %>%
  print()
save_diffs(check_subregion_assign_state)

### Subregion secondary assignment -----
check_subregion_assign_secondary_subregion <-
  subregion_assign_comparison %>%
  filter(subregion_secondary_r != subregion_secondary_access) %>%
  select(zip_r, subregion_secondary_r, subregion_secondary_access) %>%
  print()
save_diffs(check_subregion_assign_secondary_subregion)

### Subregion tertiary assignment -----
check_subregion_assign_tertiary_subregion <-
  subregion_assign_comparison %>%
  filter(subregion_tertiary_r != subregion_tertiary_access) %>%
  select(zip_r, subregion_tertiary_r, subregion_tertiary_access) %>%
  print()
save_diffs(check_subregion_assign_tertiary_subregion)

# Identify all unique plant IDs that have differences ------------

### Identify all unique plant IDs that have differences ------------

# grab check files in QA folder
check_files <- grep("check", dir(save_dir), value = TRUE)
# exclude those saved from zip utility
check_files_2 <- grep("zip_utility", check_files, invert = TRUE, value = TRUE)

#### Regular checks -----
# do not include missing values
non_access_files <- grep("missing_from_r", check_files, invert = TRUE, value = TRUE)

if(length(non_access_files) != 0) {
  # combine checked files
  zip_utility_diffs <-
    purrr::map_df(paste0(save_dir, non_access_files),
                  ~read_csv(.x, col_types = cols(.default = col_character()))) %>%
    select(zip_r) %>%
    distinct() %>%
    mutate(source_diff = "zip_subregion_assignment_file")
  
  write_csv(zip_utility_diffs, paste0(save_dir, "subregion_assignment_difference_ids.csv"))
}

#### Missing from R values ------
missing_r_files <- grep("missing_from_r", check_files_2, value = TRUE)
if(length(missing_r_files) != 0) {
  # combine checked files
  zip_missing_diffs <-
    purrr::map_df(paste0(save_dir, missing_r_files),
                  ~read_csv(.x, col_types = cols(.default = col_character()))) %>%
    select(zip_access) %>%
    distinct() %>%
    mutate(source_diff = "zip_subregion_assignment_file")
  
  write_csv(zip_missing_diffs, paste0(save_dir, "subregion_assignment_difference_ids_r.csv"))
}

print("POWER PROFILER QA COMPLETE")