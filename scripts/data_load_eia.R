
## These urls are different depending whether or not they're the most recent data. If not most recent year, "archive" is in the url


download_files <- function(form, year) {
  
  url <- dplyr::case_when(
    form == "923" ~ glue::glue("https://www.eia.gov/electricity/data/eia923/archive/xls/f923_{year}.zip"),
    form == "860" ~ glue::glue("https://www.eia.gov/electricity/data/eia860/archive/xls/eia860{year}.zip"),
    form == "861" ~ glue::glue("https://www.eia.gov/electricity/data/eia861/archive/zip/f861{year}.zip")
  )
  
  new_folder <- glue::glue("data/raw_data/{form}")
  
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
  

  
    
  if (!download_and_unzip(url, dest_file, new_folder) && stringr::str_detect(url, "/archive")) {
    url <- stringr::str_replace(url, "/archive", "")
    if (!download_and_unzip(url, dest_file, new_folder)) {
      print(paste("Failed to download or unzip:", dest_file))
    }
  }
}


# Now iterating over each file, downloading, and unzipping

invisible(purrr::map(c("860", "861", "923"), ~ download_files(form = .x, year = Sys.getenv("eGRID_year"))))

# Downloading Puerto Rico .xls file separately since it isn't zipped

url_860m  <- glue::glue("https://www.eia.gov/electricity/data/eia860m/archive/xls/december_generator{Sys.getenv('eGRID_year')}.xlsx")

path_860m <- glue::glue("data/raw_data/860m.xlsx")

if(!file.exists(path_860m)){
    download.file(url = url_860m,
                  destfile = path_860m)
}else{
  print("Stopping. File 860m.xlsx already downloaded.")
}

