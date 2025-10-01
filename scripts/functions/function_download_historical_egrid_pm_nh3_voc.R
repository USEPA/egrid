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
