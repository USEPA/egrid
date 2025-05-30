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
##      Teagan Goforth, Abt Global
##
## -------------------------------

check_valid_url <- function(url) { 
  #' check_valid_url
  #' 
  #' Function to check if the URL is valid
  
  #' @param url URL to check
  #' @return Boolean if URL is valid (TRUE) or invalid (FALSE)
  
  status <- httr::HEAD(url)$all_headers[[1]]$status == "200"

  return(status)
}

download_eia_files <- function(form, year) {
  
  #' download_eia_files
  #' 
  #' Function to download data f  rom EIA website (https://www.eia.gov/electricity/data/) for forms 923, 860, or 861. Each form's data is stored in a zipped Excel file on the EIA website. This function extracts a given file and unzips into data folder.
  
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
  
  new_folder <- glue::glue("data/1_production_model/raw_data/{form}/{params$eGRID_year}")
  
  if (!dir.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
  }
  
  dest_file <- glue::glue("{new_folder}/{form}.zip")
  
  download_and_unzip <- function(url, dest_file, new_folder) {
    
    # Check if there are other files in the folder
    existing_files <- list.files(new_folder)
    
    # remove existing files and re-download data
    if (length(existing_files) > 0) { 
      file.remove(paste0(new_folder, "/", existing_files))}
    
    if (!check_valid_url(url)) { # if URL is not valid - skip 
      print(glue::glue("{url} is not valid. Stopping download."))
      return(FALSE)
    } else { # if URL is valid, download zip file
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
    }
    
    
    # Remove the .zip file after successful unzip
    if (unzip_result) {
      file.remove(dest_file)
    }
    
    return(unzip_result)
  }
  
  
  # These URLs are different depending whether or not they're the most recent data. If not most recent year, "archive" is in the url
  # We check whether "/archive" exists in the URL and detect if URL needs to be updated. 
  
  if (!download_and_unzip(url, dest_file, new_folder) & stringr::str_detect(url, "/archive")) {
    url <- stringr::str_replace(url, "/archive", "")
    if (!download_and_unzip(url, dest_file, new_folder)) {
      print(paste("Failed to download or unzip:", dest_file))
    }
  }
  
  
  # If annual 860 data is not available, download monthly 860 data (Form 860m)
  
  if (form == "860" & !check_valid_url(url)) { # download monthly 860 data if annual version is not available and form 
    
    print("Downloading monthly 860 data (Form 860m) since 860 annual data is not available.")
    
    new_folder <- glue::glue("data/1_production_model/raw_data/860m/{params$eGRID_year}")
    
    if (!dir.exists(new_folder)) {
      dir.create(new_folder, recursive = TRUE)
    }
    
    for (month in tolower(month.name)) { 
      
      url = glue::glue("https://www.eia.gov/electricity/data/eia860m/archive/xls/{month}_generator{params$eGRID_year}.xlsx")
      path = glue::glue("{new_folder}/eia_860m_{month}_generator{params$eGRID_year}.xlsx")
      
      if (file.exists(path)) { 
        print(glue::glue("eia_860m_{month}_generator{params$eGRID_year}.xlsx already downloaded."))
      } else {
        if (check_valid_url(url)) {
          download.file(url, path, mode = "wb")
        } else {
          url <- stringr::str_replace(url, "/archive", "") # if URL is invalid, try removing archive from the URL
          if (check_valid_url(url)) { 
            download.file(url, path, mode = "wb")
          } else { 
            print(glue::glue("Stopping 860m download at {toupper(month)} because future months are not available or URL is invalid."))
            break}
        }
      }
    }
  }
}

