## -------------------------------
##
## Note: this file is for internal QA only.
##
## PM, NH3, VOC Unit file QA 
## 
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the PM, NH3, and VOC unit file creation in a given year. 
##
## The checks performed will output a CSV file with any differences 
## between Access and R unit files. 
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
#emission_type <- "pm25"
# Create QA function -----
unit_qa <- function(emission_type) {
  print(paste(toupper(emission_type), "UNIT QA IN PROGRESS"))
  
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
  
  if(dir.exists(glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}/unit_file_{emission_type}_differences"))) {
    print(glue::glue("Folder qa/{params$eGRID_year}/unit_file_{emission_type}_differences already exists."))
  }else{
    dir.create(glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}/unit_file_{emission_type}_differences"))
  }
  
  # set directory for saving files 
  save_dir <- glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}/unit_file_{emission_type}_differences/")
  
  ## Create function to save file differences -----
  save_diffs <- function(datacheck) {
    if(nrow(datacheck) > 0) {
      write_csv(datacheck, paste0(save_dir, deparse(substitute(datacheck)), ".csv")) }
  }
  
  # Import Access unit data and match formatting of R ------
  ## Load unit data -------
  if(emission_type == "pm25") {
    emission_abbrev <- "pm"
  } else {
    emission_abbrev <- emission_type
  }
  if(params$eGRID_year == "2021") {
    unit_access_raw <- read_excel(glue::glue("data/2b_pm_nh3_voc/static_tables/qa/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_abbrev}emissions_unit.xlsx"), 
                                    
                                    col_names = TRUE) %>%
        rename(PM25SRC = PM25SRC2)
   } else if(params$eGRID_year == "2022") {
     unit_access_raw <- read_excel(glue::glue("data/2b_pm_nh3_voc/static_tables/qa/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_abbrev}emissions_unit.xlsx"), 
                                   sheet = "UNT_A___latest_NEI_yr_w_PM_com1",
                                   col_names = TRUE) %>%
       rename(PM25SRC = PM25SRC2) 
  }
  
  ## Define updated column names ---------
  # Load abbreviated name to snake_case matches
  load("data/1_production_model/static_tables/name_matches.Rdata")

  # add additional column names present in unit data
  additional_names <- setNames(c(paste0(emission_type, "_ann"), paste0(emission_type, "_rate"), paste0(emission_type, "_source")),
                               c(paste0(toupper(emission_type), "AN"), paste0(toupper(emission_type), "RT"), paste0(toupper(emission_type), "SRC")))
  
  # select name matches present in unit data
  unit_new_names <- c(unit_nonmetric[names(unit_nonmetric) %in% colnames(unit_access_raw)], additional_names)
  
  # define numeric column names
  numeric_cols <- c("operating_hours", "heat_input", paste0(emission_type, "_ann"), paste0(emission_type, "_rate"), "year_online")
  # update unit column names
  unit_access_renamed <-
    unit_access_raw %>%
    rename(!!!setNames(lapply(names(unit_new_names), sym), unit_new_names)) 
  
  unit_access <-
    unit_access_renamed %>%
    mutate(across(numeric_cols[sapply(unit_access_renamed[numeric_cols], is.character)], ~ parse_number(.)),
           across(!any_of(numeric_cols), ~ as.character(.)),
           !!paste0(emission_type, "_source") := if_else(get(paste0(emission_type, "_source")) == "Estimated using an emission factor", "Estimated using an emissions factor", get(paste0(emission_type, "_source"))))
  
  # add "_access" after each variable to easily identify dataset 
  colnames(unit_access) <- paste0(colnames(unit_access), "_access")

  # replace any "NA" strings with an NA
  unit_access[unit_access == "NA"] <- NA_character_ 
  
  # Import R unit data ---------
  unit_r <- read_rds(glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/unit_file_{emission_type}.RDS"))
  
  # add "_r" after each variable to easily identify dataset 
  colnames(unit_r) <- paste0(colnames(unit_r), "_r")
  
  # Combine two datasets for comparison -----
  unit_comparison <-
    unit_r %>%
    full_join(unit_access, by = join_by(plant_id_r == plant_id_access, unit_id_r == unit_id_access, prime_mover_r == prime_mover_access))
  
  # Difference checks ------
  ## Plant presence --------
  # Plants in R not in Access
  check_diff_plant_r <- 
    unit_r %>% 
    anti_join(unit_access, by = join_by(plant_id_r == plant_id_access, unit_id_r == unit_id_access, prime_mover_r == prime_mover_access)) %>% 
    filter(!is.na(plant_id_r))
  save_diffs(check_diff_plant_r)
  
  # Plants in Access not in R
  check_diff_plant_access <- 
    unit_access %>% 
    anti_join(unit_r, by = join_by(plant_id_access == plant_id_r, unit_id_access == unit_id_r, prime_mover_access == prime_mover_r)) %>% 
    filter(!is.na(plant_id_access))
  save_diffs(check_diff_plant_access)
  
  ## Plant states -----
  check_plant_state <- 
    unit_comparison %>% 
    filter(mapply(identical, plant_state_r, plant_state_access) == FALSE) %>% 
    select(plant_id_r, plant_state_r, plant_state_access) %>% distinct()
  save_diffs(check_plant_state)
  
  ## Plant names -----
  check_plant_name <- 
    unit_comparison %>% 
    filter(mapply(identical, plant_name_r, plant_name_access) == FALSE) %>% 
    select(plant_id_r, plant_name_r, plant_name_access) %>% distinct()
  save_diffs(check_plant_name)
  
  ## Operating status -----
  check_operating_status <- 
    unit_comparison %>% 
    filter(mapply(identical, operating_status_r, operating_status_access) == FALSE) %>% 
    select(plant_id_r, unit_id_r, prime_mover_r, operating_status_r, operating_status_access)
  save_diffs(check_operating_status)
  
  ## Boiler firing type -----
  check_botfirty <- 
    unit_comparison %>% 
    filter(mapply(identical, botfirty_r, botfirty_access) == FALSE) %>% 
    select(plant_id_r, unit_id_r, prime_mover_r, botfirty_r, botfirty_access)
  save_diffs(check_botfirty)
  
  ## Primary fuel type ------
  check_fuel_type <- 
    unit_comparison %>% 
    filter(mapply(identical, primary_fuel_type_r, primary_fuel_type_access) == FALSE) %>% 
    select(plant_id_r, unit_id_r, prime_mover_r, primary_fuel_type_r, primary_fuel_type_access)
  save_diffs(check_fuel_type)
  
  ## Operating hours -----
  check_operating_hours <- 
    unit_comparison %>% 
    filter(mapply(identical, operating_hours_r, operating_hours_access) == FALSE) %>% 
    select(plant_id_r, unit_id_r, prime_mover_r, operating_hours_r, operating_hours_access)
  save_diffs(check_operating_hours)
  
  ## Heat input -----
  check_heat_input <- 
    unit_comparison %>% 
    filter(mapply(identical, heat_input_r, heat_input_access) == FALSE) %>% 
    mutate(diff_heat_input = heat_input_r - heat_input_access) %>% 
    filter(abs(diff_heat_input) > 1 | is.na(diff_heat_input)) %>% 
    select(plant_id_r, unit_id_r, prime_mover_r, heat_input_r, heat_input_access, diff_heat_input, heat_input_source_r, heat_input_source_access)
  save_diffs(check_heat_input)
  
  check_total_heat_input <- 
    unit_comparison %>% 
    summarize(sum_heat_input_r = sum(heat_input_r, na.rm = TRUE), 
              sum_heat_input_access = sum(heat_input_access, na.rm = TRUE)) %>% 
    mutate(diff_heat_input = abs(sum_heat_input_r - sum_heat_input_access)) %>%
    filter(diff_heat_input > 0)
  save_diffs(check_total_heat_input)
  
  ## Annual emissions -----
  
  # count differences in NA annual emission values
  check_emissions_na <-
    unit_comparison %>%
    summarize(na_count_r = sum(is.na(get(paste0(emission_type, "_ann_r")))), na_count_access = sum(is.na((get(paste0(emission_type, "_ann_access")))))) %>%
    mutate(diff_na_count = na_count_r - na_count_access) %>%
    filter(abs(diff_na_count) > 1) %>%
    print()

  # look at values of those NA discrepancies
  check_emissions_na_values <-
    unit_comparison %>%
    filter(is.na(get(paste0(emission_type, "_ann_r"))) & !is.na(get(paste0(emission_type, "_ann_access"))) |
             !is.na(get(paste0(emission_type, "_ann_r"))) & is.na(get(paste0(emission_type, "_ann_access"))))
  
  check_na_sources <-
    check_emissions_na_values %>%
    count(get(paste0(emission_type, "_source_r"))) %>%
    print()
    
  # calculate difference in emissions and rates for those with NA in one dataset
  check_emissions_na_sum <-
    check_emissions_na_values %>%
    summarize(na_ann_sum = sum(get(paste0(emission_type, "_ann_r")), na.rm = TRUE),
              na_rate_sum = sum(get(paste0(emission_type, "_rate_r")), na.rm = TRUE)) %>%
    print()
  
  # compare annual emission values
  check_emissions_ann <- 
    unit_comparison %>% 
    filter(mapply(identical, get(paste0(emission_type, "_ann_r")), get(paste0(emission_type, "_ann_access"))) == FALSE) %>% 
    mutate("diff_{emission_type}_ann" := abs(get(paste0(emission_type, "_ann_r")) - get(paste0(emission_type, "_ann_access")))) %>% 
    filter(get(paste0("diff_", emission_type, "_ann")) > 1E-5 | is.na(get(paste0(emission_type, "_ann_r"))) & !is.na(get(paste0(emission_type, "_ann_access"))) | 
             !is.na(get(paste0(emission_type, "_ann_r"))) & is.na(get(paste0(emission_type, "_ann_access")))) %>%
    select(plant_id_r, unit_id_r, primary_fuel_type_r, primary_fuel_type_access, 
           prime_mover_r, heat_input_r, heat_input_access,
           paste0(emission_type, "_ann_r"), paste0(emission_type, "_ann_access"), paste0("diff_", emission_type, "_ann"), paste0(emission_type, "_source_r"), paste0(emission_type, "_source_access"))
  save_diffs(check_emissions_ann)
  
  # calculate sum of annual emissions differences
  check_total_emissions_ann <- 
    unit_comparison %>% 
    summarize("sum_{emission_type}_ann_r" := sum(get(paste0(emission_type, "_ann_r")), na.rm = TRUE), 
              "sum_{emission_type}_ann_access" := sum(get(paste0(emission_type, "_ann_access")), na.rm = TRUE)) %>% 
    mutate("diff_{emission_type}_ann" := abs(get(paste0("sum_", emission_type, "_ann_r")) - get(paste0("sum_", emission_type, "_ann_access")))) %>%
    filter(get(paste0("diff_", emission_type, "_ann")) > 0) 
  save_diffs(check_total_emissions_ann)
  
  # compare total emission difference to sum of NA differences
  # if equal, the discrepancy are due to these differences
  print(c(check_total_emissions_ann[[paste0("diff_", emission_type, "_ann")]],
        check_emissions_na_sum$na_ann_sum,
        abs(check_total_emissions_ann[[paste0("diff_", emission_type, "_ann")]] - check_emissions_na_sum$na_ann_sum)))
  
  ## Emissions Rate ------
  
  # compare emission rates
  check_emissions_rate <- 
    unit_comparison %>% 
    filter(mapply(identical, get(paste0(emission_type, "_rate_r")), get(paste0(emission_type, "_rate_access"))) == FALSE) %>% 
    mutate("diff_{emission_type}_rate" := abs(get(paste0(emission_type, "_rate_r")) - get(paste0(emission_type, "_rate_access")))) %>% 
    filter(get(paste0("diff_", emission_type, "_rate")) > 1E-5 | is.na(get(paste0(emission_type, "_rate_r"))) & !is.na(get(paste0(emission_type, "_rate_access"))) | 
             !is.na(get(paste0(emission_type, "_rate_r"))) & is.na(get(paste0(emission_type, "_rate_access")))) %>%
    select(plant_id_r, unit_id_r, primary_fuel_type_r, primary_fuel_type_access, 
           prime_mover_r, heat_input_r, heat_input_access,
           paste0(emission_type, "_rate_r"), paste0(emission_type, "_rate_access"), paste0("diff_", emission_type, "_rate"), paste0(emission_type, "_source_r"), paste0(emission_type, "_source_access"))
  save_diffs(check_emissions_rate)
  
  # compare sum of emissions rates
  check_total_emissions_rate <- 
    unit_comparison %>% 
    summarize("sum_{emission_type}_rate_r" := sum(get(paste0(emission_type, "_rate_r")), na.rm = TRUE), 
              "sum_{emission_type}_rate_access" := sum(get(paste0(emission_type, "_rate_access")), na.rm = TRUE)) %>% 
    mutate("diff_{emission_type}_rate" := abs(get(paste0("sum_", emission_type, "_rate_r")) - get(paste0("sum_", emission_type, "_rate_access")))) %>%
    filter(get(paste0("diff_", emission_type, "_rate")) > 0)
  save_diffs(check_total_emissions_rate)
  
  # compare total emission difference to sum of NA differences
  # if equal, the discrepancy are due to these differences
  print(c(check_total_emissions_rate[[paste0("diff_", emission_type, "_rate")]],
          check_emissions_na_sum$na_rate_sum,
          abs(check_total_emissions_rate[[paste0("diff_", emission_type, "_rate")]] - check_emissions_na_sum$na_rate_sum)))
  
  ## Heat input source ------
  check_heat_input_source <- 
    unit_comparison %>% 
    filter(mapply(identical, heat_input_source_r, heat_input_source_access) == FALSE) %>% 
    select(plant_id_r, unit_id_r, prime_mover_r, heat_input_r, heat_input_access, heat_input_source_r, heat_input_source_access)
  save_diffs(check_heat_input_source)
  
  ## Emissions source -------
  check_emissions_source <- 
    unit_comparison %>% 
    filter(mapply(identical, get(paste0(emission_type, "_source_r")), get(paste0(emission_type, "_source_access"))) == FALSE) %>%
    select(plant_id_r, unit_id_r, prime_mover_r, paste0(emission_type, "_ann_r"), paste0(emission_type, "_ann_access"), paste0(emission_type, "_source_r"), paste0(emission_type, "_source_access"))
  save_diffs(check_emissions_source)
  
  ## Year online -----
  check_year_online <- 
    unit_comparison %>% 
    filter(mapply(identical, year_online_r, year_online_access) == FALSE) %>% 
    select(plant_id_r, unit_id_r, prime_mover_r, year_online_r, year_online_access)
  save_diffs(check_year_online)
  
  # Identify all unique plant and unit IDs that have differences ------------
  
  # grab check files in QA filder
  check_files <- grep("check", dir(save_dir), value = TRUE)
  # ignore datasets with total value differences
  files <- grep("total", check_files, invert = TRUE, value = TRUE)
  
  # combine checked files
  plant_unit_diffs <- 
    purrr::map_df(paste0(save_dir, files), 
                  ~read_csv(.x, col_types = cols(.default = col_character()))) %>% 
    select(plant_id_r, unit_id_r, prime_mover_r) %>% 
    distinct() %>% 
    mutate(source_diff = "unit_file")
  
  write_csv(plant_unit_diffs, paste0(save_dir, "plant_unit_difference_ids.csv"))
  
  print(paste(toupper(emission_type), "UNIT QA COMPLETE"))
}

# Run function for emission types -----
unit_qa("pm25")
unit_qa("nh3")
unit_qa("voc")
