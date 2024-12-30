## -------------------------------
##
## Download EIA files
## 
## Purpose: 
## 
## This file creates a function to download EIA data from their website. 
##
## Authors:  
##      Sean Bock, Abt Global
##
## -------------------------------

download_eia_files <- function(form, year) {
  
  #' download_eia_files
  #' 
  #' Function to download data from EIA website (https://www.eia.gov/electricity/data/) for forms 923, 860, or 861. Each form's data is stored in a zipped Excel file on the EIA website. This function extracts a given file and unzips into data folder.
  
  #' @param form EIA form to download. Possible options include: "923", "860", and "861"
  #' @param year Year of EIA data to download
  #' @return Unzipped Excel file
  #' @examples
  #' download_eia_files("923", params$eGRID_year) # Download a single form's data for params$eGRID_year
  #' purrr::map(c("923","860","861"), ~ download_eia_files(.x, params$eGRID_year)) # Download and unzip forms 923, 860, and 861 for params$eGRID_year
  
  url <- dplyr::case_when(
    form == "923" ~ glue::glue("https://www.eia.gov/electricity/data/eia923/archive/xls/f923_{year}.zip"),
    form == "860" ~ glue::glue("https://www.eia.gov/electricity/data/eia860/archive/xls/eia860{year}.zip"),
    form == "861" ~ glue::glue("https://www.eia.gov/electricity/data/eia861/archive/zip/f861{year}.zip")
  )
  
  new_folder <- glue::glue("data/raw_data/{form}/{params$eGRID_year}")
  
  if (!dir.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
  }
  
  
  dest_file <- glue::glue("{new_folder}/{form}.zip")
  
  download_and_unzip <- function(url, dest_file, new_folder) {
    
    # Check if there are other files in the folder
    existing_files <- list.files(new_folder)
    
    if (length(existing_files) > 1) {
      print("Files already exist in folder. Stopping")
      return(TRUE)
    }
    
    
    download.file(url, dest_file, mode = "wb")
    print(paste("Unzipping to:", new_folder))
    unzip_result <- tryCatch({
      unzip(dest_file, exdir = new_folder)
      TRUE
    }, warning = function(w) {
      FALSE
    }, error = function(e) {
      FALSE
    }, finally = {
      FALSE
    })
    
    # Remove the .zip file after successful unzip
    if (unzip_result) {
      file.remove(dest_file)
    }
    
    return(unzip_result)
  }
  
  # These URLs are different depending whether or not they're the most recent data. If not most recent year, "archive" is in the url
  # We check whether "/archive" exists in the URL and detect if URL needs to be updated. 
  
  if (!download_and_unzip(url, dest_file, new_folder) && stringr::str_detect(url, "/archive")) {
    url <- stringr::str_replace(url, "/archive", "")
    if (!download_and_unzip(url, dest_file, new_folder)) {
      print(paste("Failed to download or unzip:", dest_file))
    }
  }
}

