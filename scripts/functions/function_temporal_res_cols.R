## -------------------------------
##
## Function for temporal resolution columns
## 
## Purpose: 
## 
## This function creates a dataframe of all temporal_res combinations to avoid NAs in date and time columns
## 
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------


create_temporal_res_cols <- function(temporal_res) {
  
  #' create_temporal_res_cols
  #' 
  #' Create vector that contains all of the temporal_res columns
  #' needed for groupby 
  #' 
  #' @param temporal_res Temporal resolution of data run, likely will be the same as params$temporal_res
  #' @return Dataframe of all date and time combinations 
  
  # Specify grouping columns based on temporal_res parameter
  temporal_res_cols_all <- 
    list("annual"  = c("year"), 
         "monthly" = c("year", "month"), 
         "daily"   = c("year", "month", "day"), 
         "hourly"  = c("year", "month", "day", "hour"))
  
  # Save column names 
  temporal_res_cols <- unlist(temporal_res_cols_all[temporal_res], use.names = FALSE)
  
  return(temporal_res_cols)
}


cols_to_add <- function(temporal_res) {
  
  #' cols_to_add
  #' 
  #' Create dataframe that contains all date and time combinations. 
  #' 
  #' @param temporal_res Temporal resolution of data run, likely will be the same as params$temporal_res
  #' @return Dataframe of all date and time combinations 
  
  if (temporal_res == "annual") { 
    df <- data.frame(year = params$eGRID_year)}
  if (temporal_res == "monthly") { 
    df <- data.frame(year = params$eGRID_year, 
                 month = c(1:12))}
  if (temporal_res == "daily") {
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
  if (temporal_res == "hourly") { 
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
  return(df)
}


capfac_hours <- function(temporal_res, eGRID_year) {
  
  #' capfac_hours
  #' 
  #' Create dataframe that contains the amount of hours total necessary 
  #' to calculate capacity factor or "capfac"
  #' 
  #' @param temporal_res Temporal resolution of data run, likely will be the same as params$temporal_res
  #' @param eGRID_year Year of data being used/calculated, likely will be the same as params$eGRID_year
  #' @return Dataframe of all date and time combinations 
  
  # create dataframe of number of hours in each year or month
  if (temporal_res == "annual") { 
    hours <- data.frame(year = eGRID_year,
                        hours = 8760)}
  if (temporal_res == "monthly") { 
    hours <- 
      data.frame(year = eGRID_year,
                 month = c(1:12), 
                 hours = c(744, 672, 744, 720, 744, 720, 744, 744, 720, 744, 720, 744))
    if ((as.numeric(eGRID_year) %% 4 == 0 & as.numeric(eGRID_year) %% 100 != 0) | 
        (as.numeric(eGRID_year) %% 400 == 0)) { 
      hours <- hours %>% mutate(hours = case_when(month == 2 ~ 696))}} # if it is a leap year, assign hours of 29 days to february
  
  return(hours)
}
