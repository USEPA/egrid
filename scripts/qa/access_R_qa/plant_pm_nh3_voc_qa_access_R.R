## -------------------------------
##
## Note: this file is for internal QA only.
##
## PM, NH3, VOC Plant file QA 
## 
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the PM, NH3, and VOC plant file creation in a given year. 
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

# Create QA function -----
plant_qa <- function(emission_type) {
  print(paste(toupper(emission_type), "PLANT QA IN PROGRESS"))
  
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
  
  if(dir.exists(glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}/plant_file_{emission_type}_differences"))) {
    print(glue::glue("Folder qa/{params$eGRID_year}/plant_file_{emission_type}_differences already exists."))
  }else{
    dir.create(glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}/plant_file_{emission_type}_differences"))
  }
  
  # set directory for saving files 
  save_dir <- glue::glue("data/2b_pm_nh3_voc/outputs/qa/{params$eGRID_year}/plant_file_{emission_type}_differences/")
  
  ## Create function to save file differences -----
  save_diffs <- function(datacheck) {
    if(nrow(datacheck) > 0) {
      write_csv(datacheck, paste0(save_dir, deparse(substitute(datacheck)), ".csv")) }
  }
  
  # Import Access plant data and match formatting of R ------
  ## Load plant data -------
  if(emission_type == "pm25") {
    emission_abbrev <- substr(emission_type, 1, 2)
  } else {
    emission_abbrev <- emission_type
  }
  
  # check for file presence and load if file exists
  if(file.exists(glue::glue("data/2b_pm_nh3_voc/static_tables/qa/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_abbrev}emissions_plant.xlsx"))) {
    plant_access_raw <- read_excel(glue::glue("data/2b_pm_nh3_voc/static_tables/qa/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_abbrev}emissions_plant.xlsx"), 
                                   col_names = TRUE) %>%
      rename(PLPM25AN = PLPM25AN2, PLPM25RTA = PLPM25RTA2)
  } else {
    stop(glue::glue("Access output 'eGRID{params$eGRID_year}_{emission_abbrev}emissions_plant.xlsx' does not exist and is required for QA. \n Skipping {toupper(emission_abbrev)} QA."))
  }
  
  ## Define updated column names ---------
  # Load abbreviated name to snake_case matches
  load("data/1_production_model/static_tables/name_matches.Rdata")
  
  # add additional column names present in plant data
  additional_names <- setNames(c(paste0(emission_type, "_ann"), paste0(emission_type, "_output_rate"), paste0(emission_type, "_input_rate"), paste0(emission_type, "_source"), paste0("unadj_", emission_type)),
                               c(paste0("PL", toupper(emission_type), "AN"), paste0("PL", toupper(emission_type), "RTA"), paste0("PL", toupper(emission_type), "RA"), paste0(toupper(emission_type), "SRC"), paste0("UN", toupper(emission_type))))
  
  # select name matches present in plant data
  plant_new_names <- c(plant_nonmetric[names(plant_nonmetric) %in% colnames(plant_access_raw)], additional_names)
  
  # update plant column names
  plant_access <-
    plant_access_raw %>%
    rename(!!!setNames(lapply(names(plant_new_names), sym), plant_new_names)) %>%
    mutate(plant_id = as.character(plant_id), 
           !!paste0(emission_type, "_source") := if_else(get(paste0(emission_type, "_source")) == "Estimated using an emission factor", "Estimated using an emissions factor", get(paste0(emission_type, "_source"))))
  
  # add "_access" after each variable to easily identify dataset 
  colnames(plant_access) <- paste0(colnames(plant_access), "_access")
  
  # replace any "NA" strings with an NA
  plant_access[plant_access == "NA"] <- NA_character_ 
  
  # Import R plant data ---------
  plant_r <- read_rds(glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/plant_file_{emission_type}.RDS"))
  
  # add "_r" after each variable to easily identify dataset 
  colnames(plant_r) <- paste0(colnames(plant_r), "_r")
  
  # Combine two datasets for comparison -----
  plant_comparison <-
    plant_r %>%
    full_join(plant_access, by = join_by(plant_id_r == plant_id_access))
  
  # Difference checks ------
  ## Plant presence --------
  # Plants in R not in Access
  check_diff_plant_r <- 
    plant_r %>% 
    anti_join(plant_access, by = join_by(plant_id_r == plant_id_access)) %>% 
    filter(!is.na(plant_id_r))
  save_diffs(check_diff_plant_r)
  
  # Plants in Access not in R
  check_diff_plant_access <- 
    plant_access %>% 
    anti_join(plant_r, by = join_by(plant_id_access == plant_id_r)) %>% 
    filter(!is.na(plant_id_access))
  save_diffs(check_diff_plant_access)
  
  ## Plant states -----
  check_plant_state <- 
    plant_comparison %>% 
    filter(mapply(identical, plant_state_r, plant_state_access) == FALSE) %>% 
    select(plant_id_r, plant_state_r, plant_state_access) %>% distinct()
  save_diffs(check_plant_state)
  
  ## Plant names -----
  check_plant_name <- 
    plant_comparison %>% 
    filter(mapply(identical, plant_name_r, plant_name_access) == FALSE) %>% 
    select(plant_id_r, plant_name_r, plant_name_access) %>% distinct()
  save_diffs(check_plant_name)
  
  ## Plant subregion -----
  check_subregion <-
    plant_comparison %>%
    filter(mapply(identical, egrid_subregion_r, egrid_subregion_access) == FALSE) %>%
    select(plant_id_r, plant_name_r, egrid_subregion_r, egrid_subregion_access) %>% distinct()
  save_diffs(check_subregion)
  
  ## Primary fuel type ------
  check_fuel_type <- 
    plant_comparison %>% 
    filter(mapply(identical, primary_fuel_type_r, primary_fuel_type_access) == FALSE) %>% 
    select(plant_id_r, primary_fuel_type_r, primary_fuel_type_access)
  save_diffs(check_fuel_type)
  
  ## Nameplate capacity -----
  check_nameplate_capacity <- 
    plant_comparison %>% 
    filter(mapply(identical, nameplate_capacity_r, nameplate_capacity_access) == FALSE) %>% 
    select(plant_id_r, nameplate_capacity_r, nameplate_capacity_access)
  save_diffs(check_nameplate_capacity)
  
  ## Electric allocation -----
  check_elec_allocation <- 
    plant_comparison %>% 
    filter(mapply(identical, elec_allocation_r, elec_allocation_access) == FALSE) %>% 
    select(plant_id_r, elec_allocation_r, elec_allocation_access)
  save_diffs(check_elec_allocation)
  
  ## Annual generation -----
  check_generation_ann <- 
    plant_comparison %>% 
    filter(mapply(identical, generation_ann_r, generation_ann_access) == FALSE) %>% 
    select(plant_id_r, generation_ann_r, generation_ann_access)
  save_diffs(check_generation_ann)
  
  ## Heat input -----
  check_heat_input <- 
    plant_comparison %>% 
    filter(mapply(identical, combust_heat_input_r, combust_heat_input_access) == FALSE) %>% 
    mutate(diff_heat_input = combust_heat_input_r - combust_heat_input_access) %>% 
    filter(abs(diff_heat_input) > 1 | is.na(diff_heat_input)) %>% 
    select(plant_id_r, combust_heat_input_r, combust_heat_input_access, diff_heat_input)
  save_diffs(check_heat_input)
  
  check_total_heat_input <- 
    plant_comparison %>% 
    summarize(sum_heat_input_r = sum(combust_heat_input_r, na.rm = TRUE), 
              sum_heat_input_access = sum(combust_heat_input_access, na.rm = TRUE)) %>% 
    mutate(diff_heat_input = abs(sum_heat_input_r - sum_heat_input_access)) %>%
    filter(diff_heat_input > 0)
  save_diffs(check_total_heat_input)
  
  ## Annual emissions -----
  check_emissions_ann <- 
    plant_comparison %>% 
    filter(mapply(identical, get(paste0(emission_type, "_ann_r")), get(paste0(emission_type, "_ann_access"))) == FALSE) %>% 
    mutate("diff_{emission_type}_ann" := abs(get(paste0(emission_type, "_ann_r")) - get(paste0(emission_type, "_ann_access")))) %>% 
    filter(get(paste0("diff_", emission_type, "_ann")) > 1E-5 | is.na(get(paste0(emission_type, "_ann_r"))) & !is.na(get(paste0(emission_type, "_ann_access"))) | 
             !is.na(get(paste0(emission_type, "_ann_r"))) & is.na(get(paste0(emission_type, "_ann_access")))) %>%
    select(plant_id_r, primary_fuel_type_r, primary_fuel_type_access, 
          combust_heat_input_r, combust_heat_input_access,
           paste0(emission_type, "_ann_r"), paste0(emission_type, "_ann_access"), paste0("diff_", emission_type, "_ann"), paste0(emission_type, "_source_r"), paste0(emission_type, "_source_access"))
  save_diffs(check_emissions_ann)
  
  check_total_emissions_ann <- 
    plant_comparison %>% 
    summarize("sum_{emission_type}_ann_r" := sum(get(paste0(emission_type, "_ann_r")), na.rm = TRUE), 
              "sum_{emission_type}_ann_access" := sum(get(paste0(emission_type, "_ann_access")), na.rm = TRUE)) %>% 
    mutate("diff_{emission_type}_ann" := abs(get(paste0("sum_", emission_type, "_ann_r")) - get(paste0("sum_", emission_type, "_ann_access")))) %>%
    filter(get(paste0("diff_", emission_type, "_ann")) > 0)
  save_diffs(check_total_emissions_ann)
  
  ## Emissions output rate ------
  check_emissions_output_rate <- 
    plant_comparison %>% 
    filter(mapply(identical, get(paste0(emission_type, "_output_rate_r")), get(paste0(emission_type, "_output_rate_access"))) == FALSE) %>% 
    mutate("diff_{emission_type}_output_rate" := abs(get(paste0(emission_type, "_output_rate_r")) - get(paste0(emission_type, "_output_rate_access")))) %>% 
    filter(get(paste0("diff_", emission_type, "_output_rate")) > 1E-5 | is.na(get(paste0(emission_type, "_ann_r"))) & !is.na(get(paste0(emission_type, "_ann_access"))) | 
             !is.na(get(paste0(emission_type, "_ann_r"))) & is.na(get(paste0(emission_type, "_ann_access")))) %>%
    select(plant_id_r, primary_fuel_type_r, primary_fuel_type_access, 
           combust_heat_input_r, combust_heat_input_access,
           paste0(emission_type, "_output_rate_r"), paste0(emission_type, "_output_rate_access"), paste0("diff_", emission_type, "_output_rate"), paste0(emission_type, "_source_r"), paste0(emission_type, "_source_access"))
  save_diffs(check_emissions_output_rate)
  
  check_total_emissions_output_rate <- 
    plant_comparison %>% 
    summarize("sum_{emission_type}_output_rate_r" := sum(get(paste0(emission_type, "_output_rate_r")), na.rm = TRUE), 
              "sum_{emission_type}_output_rate_access" := sum(get(paste0(emission_type, "_output_rate_access")), na.rm = TRUE)) %>% 
    mutate("diff_{emission_type}_output_rate" := abs(get(paste0("sum_", emission_type, "_output_rate_r")) - get(paste0("sum_", emission_type, "_output_rate_access")))) %>%
    filter(get(paste0("diff_", emission_type, "_output_rate")) > 0)
  save_diffs(check_total_emissions_output_rate)
 
   ## Emissions input rate -------
  check_emissions_input_rate <- 
    plant_comparison %>% 
    filter(mapply(identical, get(paste0(emission_type, "_input_rate_r")), get(paste0(emission_type, "_input_rate_access"))) == FALSE) %>% 
    mutate("diff_{emission_type}_input_rate" := abs(get(paste0(emission_type, "_input_rate_r")) - get(paste0(emission_type, "_input_rate_access")))) %>% 
    filter(get(paste0("diff_", emission_type, "_input_rate")) > 1E-5 | is.na(get(paste0(emission_type, "_ann_r"))) & !is.na(get(paste0(emission_type, "_ann_access"))) | 
             !is.na(get(paste0(emission_type, "_ann_r"))) & is.na(get(paste0(emission_type, "_ann_access")))) %>%
    select(plant_id_r, primary_fuel_type_r, primary_fuel_type_access, 
           combust_heat_input_r, combust_heat_input_access,
           paste0(emission_type, "_input_rate_r"), paste0(emission_type, "_input_rate_access"), paste0("diff_", emission_type, "_input_rate"), paste0(emission_type, "_source_r"), paste0(emission_type, "_source_access"))
  save_diffs(check_emissions_input_rate)
  
  check_total_emissions_input_rate <- 
    plant_comparison %>% 
    summarize("sum_{emission_type}_input_rate_r" := sum(get(paste0(emission_type, "_input_rate_r")), na.rm = TRUE), 
              "sum_{emission_type}_input_rate_access" := sum(get(paste0(emission_type, "_input_rate_access")), na.rm = TRUE)) %>% 
    mutate("diff_{emission_type}_input_rate" := abs(get(paste0("sum_", emission_type, "_input_rate_r")) - get(paste0("sum_", emission_type, "_input_rate_access")))) %>%
    filter(get(paste0("diff_", emission_type, "_input_rate")) > 0)
  save_diffs(check_total_emissions_input_rate)
  
  ## Emissions source -------
  check_emissions_source <- 
    plant_comparison %>% 
    filter(mapply(identical, get(paste0(emission_type, "_source_r")), get(paste0(emission_type, "_source_access"))) == FALSE) %>%
    filter(!str_detect(get(paste0(emission_type, "_source_r")), ";") & get(paste0(emission_type, "_source_access")) != "EPA/NEI; Estimated using an emission source") %>%
    select(plant_id_r, paste0(emission_type, "_ann_r"), paste0(emission_type, "_ann_access"), paste0(emission_type, "_source_r"), paste0(emission_type, "_source_access"))
  save_diffs(check_emissions_source)
  
  ## Unadjusted combustion heat input -----
  check_unadj_heat_input <- 
    plant_comparison %>% 
    filter(mapply(identical, unadj_combust_heat_input_r, unadj_combust_heat_input_access) == FALSE) %>% 
    mutate(diff_heat_input = unadj_combust_heat_input_r - unadj_combust_heat_input_access) %>% 
    filter(abs(diff_heat_input) > 1 | is.na(diff_heat_input)) %>% 
    select(plant_id_r, unadj_combust_heat_input_r, unadj_combust_heat_input_access, diff_heat_input)
  save_diffs(check_unadj_heat_input)
  
  check_total_unadj_heat_input <- 
    plant_comparison %>% 
    summarize(sum_heat_input_r = sum(unadj_combust_heat_input_r, na.rm = TRUE), 
              sum_heat_input_access = sum(unadj_combust_heat_input_access, na.rm = TRUE)) %>% 
    mutate(diff_heat_input = abs(sum_heat_input_r - sum_heat_input_access)) %>%
    filter(diff_heat_input > 0)
  save_diffs(check_total_unadj_heat_input)
  
  ## Unadjusted emissions -----
  check_emissions_unadj <- 
    plant_comparison %>% 
    filter(mapply(identical, get(paste0("unadj_", emission_type, "_r")), get(paste0("unadj_", emission_type, "_access"))) == FALSE) %>% 
    mutate("diff_unadj_{emission_type}" := abs(get(paste0("unadj_", emission_type, "_r")) - get(paste0("unadj_", emission_type, "_access")))) %>% 
    filter(get(paste0("diff_unadj_", emission_type)) > 1E-5| is.na(get(paste0("unadj_", emission_type, "_r"))) & !is.na(get(paste0("unadj_", emission_type, "_access"))) | !is.na(get(paste0("unadj_", emission_type, "_r"))) & is.na(get(paste0("unadj_", emission_type, "_access")))) %>%
    select(plant_id_r, primary_fuel_type_r, primary_fuel_type_access, 
           combust_heat_input_r, combust_heat_input_access,
           paste0("unadj_", emission_type, "_r"), paste0("unadj_", emission_type, "_access"), paste0("diff_unadj_", emission_type), paste0(emission_type, "_source_r"), paste0(emission_type, "_source_access"))
  save_diffs(check_emissions_unadj)
  
  check_total_emissions_unadj <- 
    plant_comparison %>% 
    summarize("sum_unadj_{emission_type}_r" := sum(get(paste0("unadj_", emission_type, "_r")), na.rm = TRUE), 
              "sum_unadj_{emission_type}_access" := sum(get(paste0("unadj_", emission_type, "_access")), na.rm = TRUE)) %>% 
    mutate("diff_unadj_{emission_type}" := abs(get(paste0("sum_unadj_", emission_type, "_r")) - get(paste0("sum_unadj_", emission_type, "_access")))) %>%
    filter(get(paste0("diff_unadj_", emission_type)) > 0)
  save_diffs(check_total_emissions_ann)

  # Identify all unique plant and unit IDs that have differences ------------

# grab check files in QA folder
check_files <- grep("check", dir(save_dir), value = TRUE)
# ignore datasets with total value differences
files <- grep("total", check_files, invert = TRUE, value = TRUE)

if(length(files) > 0) {
  # combine checked files
  plant_unit_diffs <-
    purrr::map_df(paste0(save_dir, files),
                  ~read_csv(.x, col_types = cols(.default = col_character()))) %>%
    select(plant_id_r) %>%
    distinct() %>%
    mutate(source_diff = "plant_file")
  
  write_csv(plant_unit_diffs, paste0(save_dir, "plant_difference_ids.csv"))
}
  
print(paste(toupper(emission_type), "PLANT QA COMPLETE"))
}

# Create a function to safely call QA function and handle errors -----
plant_qa_safe_call <- function(emission_type) {
  result <- tryCatch({
    plant_qa(emission_type)
  }, error = function(e) {
    message("Caught an error: \n", e$message)
  })
}

# Run function for emission types -----
plant_qa_safe_call("pm25")
plant_qa_safe_call("nh3")
plant_qa_safe_call("voc")
