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
  
  # Specify grouping columns based on temporal_res parameter
  temporal_res_cols_all <- 
    list("annual"  = c("year"), 
         "monthly" = c("year", "month"), 
         "daily"   = c("year", "month", "day"), 
         "hourly"  = c("year", "month", "day", "hour"))
  
  # Save column names 
  params$temporal_res_cols <- unlist(temporal_res_cols_all[params$temporal_res], use.names = FALSE)
  
  
  # Create dataframe that contains all date and time combinations. 
  if (params$temporal_res == "annual") { 
    df <- data.frame(year = params$eGRID_year)}
  if (params$temporal_res == "monthly") { 
    df <- data.frame(year = params$eGRID_year, 
                     month = c(1:12))}
  if (params$temporal_res == "daily") {
    if ((as.numeric(params$eGRID_year) %% 4 == 0 & as.numeric(params$eGRID_year) %% 100 != 0) | 
        (as.numeric(params$eGRID_year) %% 400 == 0)) { 
      feb_days <- 29
    } else {feb_days <- 28}
    df <- as.data.frame(cbind(year = params$eGRID_year, 
                              month = c(rep(1, each = 31), 
                                        rep(2, each = feb_days), 
                                        rep(3, each = 31), 
                                        rep(4, each = 30), 
                                        rep(5, each = 31), 
                                        rep(6, each = 30), 
                                        rep(7, each = 31), 
                                        rep(8, each = 31), 
                                        rep(9, each = 30), 
                                        rep(10, each = 31), 
                                        rep(11, each = 30), 
                                        rep(12, each = 31)), 
                              day = c(1:31, 1:feb_days, 1:31, 1:30, 1:31, 1:30, 1:31, 1:31, 1:30, 1:31, 1:30, 1:31)))}
  if (params$temporal_res == "hourly") { 
    if ((as.numeric(params$eGRID_year) %% 4 == 0 & as.numeric(params$eGRID_year) %% 100 != 0) | 
        (as.numeric(params$eGRID_year) %% 400 == 0)) { 
      feb_days <- 29
      days <- 366
    } else {feb_days <- 28
    days <- 365}
    df <- as.data.frame(cbind(year = params$eGRID_year, 
                              month = c(rep(1, each = 31), 
                                        rep(2, each = feb_days), 
                                        rep(3, each = 31), 
                                        rep(4, each = 30), 
                                        rep(5, each = 31), 
                                        rep(6, each = 30), 
                                        rep(7, each = 31), 
                                        rep(8, each = 31), 
                                        rep(9, each = 30), 
                                        rep(10, each = 31), 
                                        rep(11, each = 30), 
                                        rep(12, each = 31)), 
                              day = c(1:31, 1:feb_days, 1:31, 1:30, 1:31, 1:30, 1:31, 1:31, 1:30, 1:31, 1:30, 1:31),
                              hour = rep(1:24, each = days)))}
  
  # Save temporal columns to add in params
  params$temporal_res_cols_to_add <- df
  
  return(params)
}

