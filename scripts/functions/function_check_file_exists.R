## -------------------------------
##
## Check file exists
## 
## Purpose: 
## 
## This function checks if the file exists before attempting to 
## load in file data. 
##
## Additional notes
##
##      Madeline Zhang, Abt Global
##
## -------------------------------

check_file_exists <- function(file_path){
  
  #' check_file_exists
  #' 
  #' Function to check if file exists and then loads in data
  #' 
  #' @param file_path String name of file path to check and load
  #' 
  #' @return Data contained at file path
  #'         
  #' @examples 
  #' # Save PM2.5 plant file
  #' save_output_data(pm_plant_formatted, "data/outputs/1_production_model", "pm_plant_file.RDS")
  
  # get file name from file path
  file <- str_split(file_path, "/") %>%
          sapply(tail, 1)
  
  # produce dynamic output depending which file is attempted 
  run_options <- c("data_load_eia.R and data_clean_eia.R",
                   "data_load_epa.R and data_clean_epa.R",
                   "generator_file_create.R",
                   "unit_file_create.R",
                   "plant_file_create.R",
                   "grid_gross_loss_create.R",
                   "region_aggregation_create.R")
  
  if (grepl("eia", file)) {
    file_run_options <- run_options[1]
  } else if (grepl("epa", file)) {
    file_run_options <- run_options[2]
  } else if (grepl("generator", file)) {
    file_run_options <- run_options[3]
  } else if (grepl("plant", file)) {
    file_run_options <- run_options[4]
  } else if (grepl("unit", file)) {
    file_run_options <- run_options[5]
  } else if (grepl("grid_gross_loss", file)) {
    file_run_options <- run_options[6]
  } else if (grepl("aggregation", file)) {
    file_run_options <- run_options[7]
  } else {
    file_run_options <- "unknown ?"
  }
  
  # if the file exists, load in RDS file
  if(file.exists(file_path)) {
    
    data <- read_rds(file_path)
    
    return (data)
    
  # otherwise, stop and produce error message
  } else { 
    
    stop(glue::glue("{file} does not exist. Run {file_run_options} to obtain."))}
  

}

check_name_matches <- function() {
  
  # load in name matches for shorthand to snake_case
  if(file.exists("data/1_production_model/static_tables/name_matches.RData")) {
    base::load("data/1_production_model/static_tables/name_matches.RData")
  } else { 
    source("scripts/1_production_model/name_matching.R")
    base::load("data/1_production_model/static_scripts/name_matches.RData")
  }
  
}
