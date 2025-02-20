## -------------------------------
##
## Function for temporal resolution params check
## 
## Purpose: 
## 
## This function checks for correct inputs in params and 
## creates a dataframe of all temporal_res combinations to avoid NAs in date and time columns
## 
## Authors:  
##      Teagan Goforth, Abt Global
##      Madeline Zhang, Abt Global
##
## -------------------------------

params_check <- function() {
  
  #' params_check
  #' 
  #' Create list params that contains eGRID_year and temporal_res
  #' and check that the input variables match acceptable responses.
  #' 
  #' @return list params
  #' Example: params <- params_check()
  
  # check if parameters for eGRID data year need to be defined
  # this is only necessary when running the script outside of egrid_master.qmd
  # user will be prompted to input parameters in the console if params does not exist
  
  if (exists("params")) {
    
    if ("eGRID_year" %in% names(params) & "temporal_res" %in% names(params)) { # if params(), params$eGRID_year, and params$temporal_res exist, do not re-define
      print("eGRID year and temporal resolution parameters are already defined.") 
    } else if (!("eGRID_year" %in% names(params))) {  # if params() is defined, but eGRID_year is not, define it here 
      params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
      params$eGRID_year <- as.character(params$eGRID_year) 
    } else if (!("temporal_res" %in% names(params))) {  # if params() is defined, but temporal_res is not, define it here 
      params$temporal_res <- readline(prompt = "Input temporal resolution (annual/monthly/daily/hourly): ")
      params$temporal_res <- as.character(params$temporal_res) 
    }
  } else { # if params(), eGRID_year, temporal_res are not defined, define them here
    params <- list()
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year)
    params$temporal_res <- readline(prompt = "Input temporal resolution (annual/monthly/daily/hourly): ")
    params$temporal_res <- as.character(params$temporal_res) 
  }
  
  # valid eGRID years (update this every eGRID year to include latest year)
  eGRID_year_inputs <- c(1996:2023)
  
  if(!(params$eGRID_year %in% eGRID_year_inputs)) {
    print("The input for params$eGRID_year is not one of the valid responses. Please input a year within the range of 1996-2023.")
    return(params_check()) # restart function for new inputs
  }
  
  # valid temporal_res inputs
  temporal_res_inputs <- c("annual", "monthly", "daily", "hourly")
  
  if (!(params$temporal_res %in% temporal_res_inputs)) {
    print("The input for params$temporal_res is not one of the valid responses. Please input either annual, monthly, daily, or hourly.")
    return(params_check()) # restart function for new inputs
  }
  
  return(params)
}

