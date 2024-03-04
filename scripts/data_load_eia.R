
## These urls are different depending whether or not they're the most recent data. If not most recent year, "archive" is in the url


download_files <- function(form, year) {
  
  url <- dplyr::case_when(
    form == "923" ~ glue::glue("https://www.eia.gov/electricity/data/eia923/archive/xls/f923_{year}.zip"),
    form == "860" ~ glue::glue("https://www.eia.gov/electricity/data/eia860/archive/xls/eia860{year}.zip"),
    form == "861" ~ glue::glue("https://www.eia.gov/electricity/data/eia861/archive/zip/f861{year}.zip")
  )
  
  new_folder <- glue::glue("archive/data_archive/download_test/{form}")
  
  if (!dir.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
  }
  
  dest_file <- glue::glue("{new_folder}/{form}.zip")
  
  download_and_unzip <- function(url, dest_file, new_folder) {
    print(paste("Downloading from:", url))
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
    return(unzip_result)
  }
  
  if (!download_and_unzip(url, dest_file, new_folder) && str_detect(url, "/archive")) {
    url <- str_replace(url, "/archive", "")
    if (!download_and_unzip(url, dest_file, new_folder)) {
      print(paste("Failed to download or unzip:", dest_file))
    }
  }
}



map(c("860", "861", "923"), ~ download_files(form = .x, year = "2021"))




