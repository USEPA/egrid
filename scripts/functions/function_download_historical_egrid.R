## -------------------------------
##
## Download eGRID Historical Data
## 
## Purpose: 
## 
## This function downloads historical eGRID for the use in all 
## eGRID processes. 
##
## Authors:
##      Emma Russell, Abt Global
##
## -------------------------------

download_historical_egrid <- function(year, egrid_dir) {
  
  #' @name download_historical_egrid
  #' 
  #' Function to check for presence of historical eGRID data and download if 
  #' it doesn't exist
  #' 
  #' @param year eGRID data year to download
  #' @param file_path eGRID data URL where online data is available
  #' @return Downloaded eGRID data file path to be temporarily stored in repository and
  #'         is available to load in scripts
  #'         
  #' @examples 
  #' # Download 2022 eGRID historical data
  #' egrid_historical_path <- download_historical_egrid(2022, "data/1_production_model/static_tables/historical_egrid/")
  
  source("scripts/functions/function_check_valid_url.R")
  
  ### Note: check for updates or changes each data year ###
  # add previous eGRID data year every year
  urls <- c("2018" = "https://www.epa.gov/sites/default/files/2020-03/egrid2018_data_v2.xlsx",
            "2019" = "https://www.epa.gov/sites/default/files/2021-02/egrid2019_data.xlsx",
            "2020" = "https://www.epa.gov/system/files/documents/2022-09/eGRID2020_Data_v2.xlsx",
            "2021" = "https://www.epa.gov/system/files/documents/2023-01/eGRID2021_data.xlsx",
            "2022" = "https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx",
            "2023" = "https://www.epa.gov/system/files/documents/2025-06/egrid2023_data_rev2.xlsx")
  
  # save URLs as a callable variable
  assign("egrid_urls", urls, envir = .GlobalEnv)
  
  # set name of previous year's file
  file_path <- glue::glue("{egrid_dir}egrid{year}_data.xlsx")
  
  # check if historical data file does not exist
  if (!file.exists(file_path)) {
    print(glue::glue("eGRID {year} historical data does not exist. Downloading..."))
    
    # check for presence of output directory and create if doesn't exist
    if (!dir.exists(egrid_dir)) {
      dir.create(egrid_dir, recursive = TRUE)
    }
    
    # check if year is in URLs
    if (any(grepl(year, names(urls), ignore.case = TRUE))) {
      
      # index URL based on year
      egrid_url <- urls[as.character(year)]
      
      # check if URL is valid 
      if (check_valid_url(egrid_url)) {
        
        # download file
        download.file(url = egrid_url, 
                      destfile = file_path, 
                      mode = "wb")
        
        print(glue::glue("eGRID {year} historical data downloaded and stored in {file_path}"))
      } else {
        # if not, stop to check script or manually add previous data
        stop(glue::glue("Check script or add data for {year} historical eGRID data."))
      }
      
    } else {
      # if not, stop to check script or manually add previous data
      stop(glue::glue("Check script or add data for {year} historical eGRID data."))
    }
    
  } else {
    # skip download if file exists already
    print(glue::glue("eGRID {year} historical data already exists in {file_path}"))
  }
 
  return(file_path)
}
  


 
