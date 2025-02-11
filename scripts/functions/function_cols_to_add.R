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
