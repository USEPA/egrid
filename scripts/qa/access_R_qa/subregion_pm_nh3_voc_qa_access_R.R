## -------------------------------
##
## Note: this file is for internal QA only.
##
## PM, NH3, VOC Subregion file QA 
## 
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the PM, NH3, and VOC subregion file creation in a given year. 
##
## The checks performed will output a CSV file with any differences 
## between Access and R subregion files. 
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
emission_type = "pm25"
# Create QA function -----
subregion_qa <- function(emission_type) {
  print(paste(toupper(emission_type), "SUBREGION QA IN PROGRESS"))
  
  # Create save directory for QA outputs -----
  
  if(dir.exists("data/2b_pm_nh3_voc/outputs/qa")) {
    print("Folder qa already exists.")
  }else{
    dir.create("data/2b_pm_nh3_voc/outputs/qa")
  }
  
  if(dir.exists(glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}"))) {
    print(glue::glue("Folder qa/{params$eGRID_year} already exists."))
  }else{
    dir.create(glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}"))
  }
  
  if(dir.exists(glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}/subregion_file_{emission_type}_differences"))) {
    print(glue::glue("Folder qa/{params$eGRID_year}/subregion_file_{emission_type}_differences already exists."))
  }else{
    dir.create(glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}/subregion_file_{emission_type}_differences"))
  }
  
  # set directory for saving files 
  save_dir <- glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}/subregion_file_{emission_type}_differences/")
  
  
  ## Create function to save file differences -----
  save_diffs <- function(datacheck) {
    if(nrow(datacheck) > 0) {
      write_csv(datacheck, paste0(save_dir, deparse(substitute(datacheck)), ".csv")) }
  }
  
  # Import Access subregion data and match formatting of R ------
  ## Load subregion data -------
  if(emission_type == "pm25") {
    emission_abbrev <- substr(emission_type, 1, 2)
  } else {
    emission_abbrev <- emission_type
  }
  if(params$eGRID_year == "2021") {
  subregion_access_raw <- read_excel(glue::glue("data/2b_pm_nh3_voc/static_tables/qa/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_abbrev}emissions.xlsx"),
                                sheet = paste(params$eGRID_year, toupper(emission_abbrev), "Subregion-level Data"),
                                skip = 1,
                                col_names = TRUE) %>%
    filter(SUBRGN != "U.S.")
  } else {
      if(emission_type == "pm25") {
        subregion_access_raw <- read_excel(glue::glue("data/2b_pm_nh3_voc/static_tables/qa/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_abbrev}emissions_subregion.xlsx"), 
                                      col_names = TRUE) %>%
          rename(SRNGENAN = Gen, SRPM25AN = PM25tons, SRPM25RTA = Rate)
      } else {
          subregion_access_raw <- read_excel(glue::glue("data/2b_pm_nh3_voc/static_tables/qa/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_abbrev}emissions_subregion.xlsx"), 
                                             col_names = TRUE) %>%
            rename(SRNGENAN = Gen, SRPM25AN = PM25tons, SRPM25RTA = Rate)
      }
  }
  
  ## Define updated column names ---------
  # Load abbreviated name to snake_case matches
  load("data/1_production_model/static_tables/name_matches.Rdata")
  
  # add additional column names present in subregion data
  additional_names <- setNames(c(paste0(emission_type, "_tons"), paste0(emission_type, "_rate")), 
                               c(paste0("SR", toupper(emission_type), "AN"), paste0("SR", toupper(emission_type), "RTA")))
  
  # select name matches present in subregion data
  subregion_new_names <- c(subregion_nonmetric[names(subregion_nonmetric) %in% colnames(subregion_access_raw)], additional_names)
  
  # update subregion column names
  # subregion_access <-
  #   subregion_access_raw %>%
  #   rename(!!!setNames(lapply(names(subregion_new_names), sym), subregion_new_names)) %>%
  #   mutate(year = as.character(year))
  subregion_access <-
    subregion_access_raw %>%
    rename(!!!setNames(lapply(names(subregion_new_names), sym), subregion_new_names))
  
  # add "_access" after each variable to easily identify dataset 
  colnames(subregion_access) <- paste0(colnames(subregion_access), "_access")
  
  # replace any "NA" strings with an NA
  subregion_access[subregion_access == "NA"] <- NA_character_ 
  
  # Import R subregion data ---------
  subregion_r <- read_rds(glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/subregion_aggregation_{emission_type}.RDS"))
  
  # add "_r" after each variable to easily identify dataset 
  colnames(subregion_r) <- paste0(colnames(subregion_r), "_r")
  
  # Combine two datasets for comparison -----
  subregion_comparison <-
    subregion_r %>%
    full_join(subregion_access, by = join_by(subregion_r == subregion_access))
  
  # Difference checks ------
  ## Subregion presence --------
  # Subregions in R not in Access
  check_diff_subregion_r <- 
    subregion_r %>% 
    anti_join(subregion_access, by = join_by(subregion_r == subregion_access)) %>% 
    filter(!is.na(subregion_r))
  save_diffs(check_diff_subregion_r)
  
  # Subregions in Access not in R
  check_diff_subregion_access <- 
    subregion_access %>% 
    anti_join(subregion_r, by = join_by(subregion_access == subregion_r)) %>% 
    filter(!is.na(subregion_access))
  save_diffs(check_diff_subregion_access)
  
  ## Subregion names -----
  # check_subregion_name <- 
  #   subregion_comparison %>% 
  #   filter(mapply(identical, subregion_name_r, subregion_name_access) == FALSE) %>% 
  #   select(subregion_r, subregion_name_r, subregion_name_access) %>% distinct()
  # save_diffs(check_subregion_name)
 
   ## Annual generation -----
  check_generation_ann <- 
    subregion_comparison %>% 
    filter(mapply(identical, subregion_generation_ann_r, subregion_generation_ann_access) == FALSE) %>% 
    select(subregion_r, subregion_generation_ann_r, subregion_generation_ann_access)
  save_diffs(check_generation_ann)
 
   ## Annual emissions -----
  # check_emissions_tons <- 
  #   subregion_comparison %>% 
  #   filter(mapply(identical, get(paste0(emission_type, "_tons_r")), get(paste0(emission_type, "_tons_access"))) == FALSE) %>% 
  #   mutate("{emission_type}_tons_r" := round(get(paste0(emission_type, "_tons_r")), 0),
  #     "diff_{emission_type}_tons" := abs(get(paste0(emission_type, "_tons_r")) - get(paste0(emission_type, "_tons_access")))) %>% 
  #   filter(get(paste0("diff_", emission_type, "_tons")) > 1E-5 | is.na(get(paste0(emission_type, "_tons_r"))) & !is.na(get(paste0(emission_type, "_tons_access"))) | 
  #            !is.na(get(paste0(emission_type, "_tons_r"))) & is.na(get(paste0(emission_type, "_tons_access")))) %>%
  #   select(subregion_r,
  #          paste0(emission_type, "_tons_r"), paste0(emission_type, "_tons_access"), paste0("diff_", emission_type, "_tons"))
  # save_diffs(check_emissions_tons)
  check_emissions_tons <- 
    subregion_comparison %>% 
    filter(mapply(identical, get(paste0(emission_type, "_tons_r")), get(paste0(emission_type, "_tons_access"))) == FALSE) %>% 
    mutate("{emission_type}_tons_r" := round(get(paste0(emission_type, "_tons_r")), 2),
           "diff_{emission_type}_tons" := abs(get(paste0(emission_type, "_tons_r")) - get(paste0(emission_type, "_tons_access")))) %>% 
    filter(get(paste0("diff_", emission_type, "_tons")) > 1E-5 | is.na(get(paste0(emission_type, "_tons_r"))) & !is.na(get(paste0(emission_type, "_tons_access"))) | 
             !is.na(get(paste0(emission_type, "_tons_r"))) & is.na(get(paste0(emission_type, "_tons_access")))) %>%
    select(subregion_r,
           paste0(emission_type, "_tons_r"), paste0(emission_type, "_tons_access"), paste0("diff_", emission_type, "_tons"))
  save_diffs(check_emissions_tons)
  
  check_total_emissions_tons <- 
    subregion_comparison %>% 
    summarize("sum_{emission_type}_tons_r" := sum(get(paste0(emission_type, "_tons_r")), na.rm = TRUE), 
              "sum_{emission_type}_tons_access" := sum(get(paste0(emission_type, "_tons_access")), na.rm = TRUE)) %>% 
    mutate("diff_{emission_type}_tons" := abs(get(paste0("sum_", emission_type, "_tons_r")) - get(paste0("sum_", emission_type, "_tons_access")))) %>%
    filter(get(paste0("diff_", emission_type, "_tons")) > 0)
  save_diffs(check_total_emissions_tons)
  
  ## Emissions output rate ------
  check_emissions_rate <- 
    subregion_comparison %>% 
    filter(mapply(identical, get(paste0(emission_type, "_rate_r")), get(paste0(emission_type, "_rate_access"))) == FALSE) %>% 
    mutate("diff_{emission_type}_rate" := abs(get(paste0(emission_type, "_rate_r")) - get(paste0(emission_type, "_rate_access")))) %>% 
    filter(get(paste0("diff_", emission_type, "_rate")) > 1E-5 | is.na(get(paste0(emission_type, "_rate_r"))) & !is.na(get(paste0(emission_type, "_rate_access"))) | 
             !is.na(get(paste0(emission_type, "_rate_r"))) & is.na(get(paste0(emission_type, "_rate_access")))) %>%
    select(subregion_r,
           paste0(emission_type, "_rate_r"), paste0(emission_type, "_rate_access"), paste0("diff_", emission_type, "_rate"))
  save_diffs(check_emissions_rate)
  
  check_total_emissions_rate <- 
    subregion_comparison %>% 
    summarize("sum_{emission_type}_rate_r" := sum(get(paste0(emission_type, "_rate_r")), na.rm = TRUE), 
              "sum_{emission_type}_rate_access" := sum(get(paste0(emission_type, "_rate_access")), na.rm = TRUE)) %>% 
    mutate("diff_{emission_type}_rate" := abs(get(paste0("sum_", emission_type, "_rate_r")) - get(paste0("sum_", emission_type, "_rate_access")))) %>%
    filter(get(paste0("diff_", emission_type, "_rate")) > 0)
  save_diffs(check_total_emissions_rate)
  
  # Identify all unique subregions that have differences ------------
  
# grab check files in QA filder
check_files <- grep("check", dir(save_dir), value = TRUE)
# ignore datasets with total value differences
files <- grep("total", check_files, invert = TRUE, value = TRUE)

# combine checked files
subregion_unit_diffs <-
  purrr::map_df(paste0(save_dir, files),
                ~read_csv(.x, col_types = cols(.default = col_character()))) %>%
  select(subregion_r) %>%
  distinct() %>%
  mutate(source_diff = "subregion_file")

write_csv(subregion_unit_diffs, paste0(save_dir, "subregion_difference_ids.csv"))

print(paste(toupper(emission_type), "SUBREGION QA COMPLETE"))
}

# Run function for emission types -----
subregion_qa("pm25")
#subregion_qa("nh3")
#subregion_qa("voc")
