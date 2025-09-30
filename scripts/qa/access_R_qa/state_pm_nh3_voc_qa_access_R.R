## -------------------------------
##
## Note: this file is for internal QA only.
##
## PM, NH3, VOC State file QA 
## 
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the PM, NH3, and VOC state file creation in a given year. 
##
## The checks performed will output a CSV file with any differences 
## between Access and R state files. 
## 
## Authors:  
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

# Create QA function -----
state_qa <- function(emission_type) {
  print(paste(toupper(emission_type), "STATE QA IN PROGRESS"))
  
  # Create save directory for QA outputs -----
  
  if(dir.exists("data/2a_pm_nh3_voc/outputs/qa")) {
    print("Folder qa already exists.")
  }else{
    dir.create("data/2a_pm_nh3_voc/outputs/qa")
  }
  
  if(dir.exists(glue::glue("data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}"))) {
    print(glue::glue("Folder qa/{params$eGRID_year} already exists."))
  }else{
    dir.create(glue::glue("data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}"))
  }
  
  if(dir.exists(glue::glue("data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}/state_file_{emission_type}_differences"))) {
    print(glue::glue("Folder qa/{params$eGRID_year}/state_file_{emission_type}_differences already exists."))
  }else{
    dir.create(glue::glue("data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}/state_file_{emission_type}_differences"))
  }
  
  # set directory for saving files 
  save_dir <- glue::glue("data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}/state_file_{emission_type}_differences/")
  
  
  ## Create function to save file differences -----
  save_diffs <- function(datacheck) {
    if(nrow(datacheck) > 0) {
      write_csv(datacheck, paste0(save_dir, deparse(substitute(datacheck)), ".csv")) }
  }
  
  # Import Access state data and match formatting of R ------
  ## Load state data -------
  if(emission_type == "pm") {
    emission_label <- "pm25"
  } else {
    emission_label <- emission_type
  }
  
  # check for file presence and load if file exists
  if(file.exists(glue::glue("data/2a_pm_nh3_voc/static_tables/qa/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_type}emissions_state.xlsx"))) {
    state_access_raw <- read_excel(glue::glue("data/2a_pm_nh3_voc/static_tables/qa/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_type}emissions_state.xlsx"), 
                                   col_names = TRUE) %>%
      rename(STNGENAN = SumOfPLNGENAN, STPM25AN = SumOfPLPM25AN2)
  } else {
    stop(glue::glue("Access output 'eGRID{params$eGRID_year}_{emission_type}emissions_state.xlsx' does not exist and is required for QA. \n Skipping {toupper(emission_type)} QA."))
  }
  
  ## Define updated column names ---------
  # Load abbreviated name to snake_case matches
  load("data/1_production_model/static_tables/name_matches.Rdata")
  
  # add additional column names present in state data
  additional_names <- setNames(c(paste0(emission_label, "_tons"), paste0(emission_label, "_rate")), 
                               c(paste0("ST", toupper(emission_label), "AN"), paste0("ST", toupper(emission_label), "RTA")))
  
  # select name matches present in state data
  state_new_names <- c(state_nonmetric[names(state_nonmetric) %in% colnames(state_access_raw)], additional_names)
  
  # update state column names
  state_access <-
    state_access_raw %>%
    rename(!!!setNames(lapply(names(state_new_names), sym), state_new_names))
  
  # add "_access" after each variable to easily identify dataset 
  colnames(state_access) <- paste0(colnames(state_access), "_access")
  
  # replace any "NA" strings with an NA
  state_access[state_access == "NA"] <- NA_character_ 
  
  # Import R state data ---------
  state_r <- read_rds(glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/state_aggregation_{emission_type}.RDS")) %>%
    rename(pm25_tons = pm25_ann, pm25_rate = pm25_output_rate)
  
  # add "_r" after each variable to easily identify dataset 
  colnames(state_r) <- paste0(colnames(state_r), "_r")
  
  # Combine two datasets for comparison -----
  state_comparison <-
    state_r %>%
    full_join(state_access, by = join_by(state_r == state_access))
  
  # Difference checks ------
  ## State presence --------
  # States in R not in Access
  check_diff_state_r <- 
    state_r %>% 
    anti_join(state_access, by = join_by(state_r == state_access)) %>% 
    filter(!is.na(state_r))
  save_diffs(check_diff_state_r)
  
  # States in Access not in R
  check_diff_state_access <- 
    state_access %>% 
    anti_join(state_r, by = join_by(state_access == state_r)) %>% 
    filter(!is.na(state_access))
  save_diffs(check_diff_state_access)
 
   ## Annual generation -----
  check_generation_ann <- 
    state_comparison %>% 
    filter(mapply(identical, state_generation_ann_r, state_generation_ann_access) == FALSE) %>% 
    mutate("diff_generation_ann" = abs(state_generation_ann_r - state_generation_ann_access)) %>% 
    filter(diff_generation_ann > 1E-3) %>%
    select(state_r, state_generation_ann_r, state_generation_ann_access)
  save_diffs(check_generation_ann)
 
   ## Annual emissions -----
  check_emissions_tons <- 
    state_comparison %>% 
    filter(mapply(identical, get(paste0(emission_label, "_tons_r")), get(paste0(emission_label, "_tons_access"))) == FALSE) %>% 
    mutate("diff_{emission_label}_tons" := abs(get(paste0(emission_label, "_tons_r")) - get(paste0(emission_label, "_tons_access")))) %>% 
    filter(get(paste0("diff_", emission_label, "_tons")) > 1E-5 | is.na(get(paste0(emission_label, "_tons_r"))) & !is.na(get(paste0(emission_label, "_tons_access"))) | 
             !is.na(get(paste0(emission_label, "_tons_r"))) & is.na(get(paste0(emission_label, "_tons_access")))) %>%
    select(state_r,
           paste0(emission_label, "_tons_r"), paste0(emission_label, "_tons_access"), paste0("diff_", emission_label, "_tons"))
  save_diffs(check_emissions_tons)
  
  check_total_emissions_tons <- 
    state_comparison %>% 
    summarize("sum_{emission_label}_tons_r" := sum(get(paste0(emission_label, "_tons_r")), na.rm = TRUE), 
              "sum_{emission_label}_tons_access" := sum(get(paste0(emission_label, "_tons_access")), na.rm = TRUE)) %>% 
    mutate("diff_{emission_label}_tons" := abs(get(paste0("sum_", emission_label, "_tons_r")) - get(paste0("sum_", emission_label, "_tons_access")))) %>%
    filter(get(paste0("diff_", emission_label, "_tons")) > 0)
  save_diffs(check_total_emissions_tons)
  
  ## Emissions output rate ------
  check_emissions_rate <- 
    state_comparison %>% 
    filter(mapply(identical, get(paste0(emission_label, "_rate_r")), get(paste0(emission_label, "_rate_access"))) == FALSE) %>% 
    mutate("diff_{emission_label}_rate" := abs(get(paste0(emission_label, "_rate_r")) - get(paste0(emission_label, "_rate_access")))) %>% 
    filter(get(paste0("diff_", emission_label, "_rate")) > 1E-5 | is.na(get(paste0(emission_label, "_rate_r"))) & !is.na(get(paste0(emission_label, "_rate_access"))) | 
             !is.na(get(paste0(emission_label, "_rate_r"))) & is.na(get(paste0(emission_label, "_rate_access")))) %>%
    select(state_r,
           paste0(emission_label, "_rate_r"), paste0(emission_label, "_rate_access"), paste0("diff_", emission_label, "_rate"))
  save_diffs(check_emissions_rate)
  
  check_total_emissions_rate <- 
    state_comparison %>% 
    summarize("sum_{emission_label}_rate_r" := sum(get(paste0(emission_label, "_rate_r")), na.rm = TRUE), 
              "sum_{emission_label}_rate_access" := sum(get(paste0(emission_label, "_rate_access")), na.rm = TRUE)) %>% 
    mutate("diff_{emission_label}_rate" := abs(get(paste0("sum_", emission_label, "_rate_r")) - get(paste0("sum_", emission_label, "_rate_access")))) %>%
    filter(get(paste0("diff_", emission_label, "_rate")) > 0)
  save_diffs(check_total_emissions_rate)
  
  # Identify all unique states that have differences ------------
  
# grab check files in QA filder
check_files <- grep("check", dir(save_dir), value = TRUE)
# ignore datasets with total value differences
files <- grep("total", check_files, invert = TRUE, value = TRUE)

if(length(files) > 0) {
  # combine checked files
  state_unit_diffs <-
    purrr::map_df(paste0(save_dir, files),
                  ~read_csv(.x, col_types = cols(.default = col_character()))) %>%
    select(state_r) %>%
    distinct() %>%
    mutate(source_diff = "state_file")
  
  write_csv(state_unit_diffs, paste0(save_dir, "state_difference_ids.csv"))
}

print(paste(toupper(emission_type), "STATE QA COMPLETE"))
}

# Create a function to safely call QA function and handle errors -----
state_qa_safe_call <- function(emission_type) {
  result <- tryCatch({
    state_qa(emission_type)
  }, error = function(e) {
    message("Caught an error: \n", e$message)
  })
}

# Run function for emission types -----
state_qa_safe_call("pm")
state_qa_safe_call("nh3")
state_qa_safe_call("voc")
