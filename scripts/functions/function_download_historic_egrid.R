## -------------------------------
##
## Download eGRID Historic PM NH3 VOC functions
## 
## Purpose: 
## 
## This function downloads historic eGRID for the use in PM, NH3, and VOC
### calculations when the year is 2022 and raw production model data is 
## not available.
##
##
## Authors:
##      Emma Russell, Abt Global
##
## -------------------------------


download_egrid_historic <- function(year, egrid_url) {
  
  #' @name download_egrid_historic
  #' 
  #' Function to check for presence of historic eGRID data and download if 
  #' it doesn't exist
  #' 
  #' @param year eGRID data year to download, in this case used for 2022 in
  #'             PM, NH3, and VOC calculations
  #' @param egrid_url eGRID data URL where online data is available
  #' @return Downloaded eGRID data to be temporarily stored in repository and
  #'         is available to load in scripts
  #'         Character of file path
  #'         
  #' @examples 
  #' # Download 2022 eGRID historical data
  #' egrid_historical_path <- download_egrid_historic(2022, "https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx")

  # set directory to store historical egrid data
  egrid_dir <- glue::glue("data/2a_pm_nh3_voc/inputs/egrid_historic/{year}/")
  # set name of previous year's file
  egrid_name <- glue::glue("{egrid_dir}eGRID{year}_data.xlsx")
  
  # download historical eGRID data if file doesn't exist
  if (!file.exists(egrid_name)) {
    print(glue::glue("eGRID {year} Historical Data does not exist. Downloading..."))
    # check for presence of directories and create if doesn't exist
    if (!dir.exists(egrid_dir)) {
      dir.create(egrid_dir, recursive = TRUE)
    }
    # download production model data and save in desired folder
    download.file(url = egrid_url, destfile = egrid_name, mode = "wb")
    print(glue::glue("eGRID {year} Historical Data downloaded and stored in {egrid_name}"))
  }
  else {
    print(glue::glue("eGRID {year} Historical Data already exists in {egrid_name}"))
  }
  return(egrid_name)
}

download_historic_egrid <- function(year, egrid_dir) {
  
  source("scripts/functions/function_check_valid_url.R")
  
  ### Note: check for updates or changes each data year ###
  # add previous eGRID data year every year
  urls <- c("2018" = "https://www.epa.gov/sites/default/files/2020-03/egrid2018_data_v2.xlsx",
            "2019" = "https://www.epa.gov/sites/default/files/2021-02/egrid2019_data.xlsx",
            "2020" = "https://www.epa.gov/system/files/documents/2022-09/eGRID2020_Data_v2.xlsx",
            "2021" = "https://www.epa.gov/system/files/documents/2023-01/eGRID2021_data.xlsx",
            "2022" = "https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx")
  
  # save URLs as a callable variable
  assign("egrid_urls", urls, envir = .GlobalEnv)
  
  # set name of previous year's file
  file_path <- glue::glue("{egrid_dir}egrid{year}_data.xlsx")
  
  egrid_url <- urls[as.character(year)]
  
  # check if historical data file does not exist
  if (!file.exists(file_path)) {
    print(glue::glue("eGRID {year} Historical Data does not exist. Downloading..."))
    
    # check for presence of output directory and create if doesn't exist
    if (!dir.exists(egrid_dir)) {
      dir.create(egrid_dir, recursive = TRUE)
    }
    
    # check if year is in URLs
    if (any(grepl(year, names(url), ignore.case = TRUE))) {
      
      # check if URL is valid 
      if (check_valid_url(egrid_url)) {
        
        # download file
        download.file(url = egrid_url, 
                      destfile = file_path, 
                      mode = "wb")
        
        print(glue::glue("eGRID {year} Historical Data downloaded and stored in {file_path}"))
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
    print(glue::glue("eGRID {year} Historical Data already exists in {file_path}"))
  }
 
  return(file_path)
}
  


 
