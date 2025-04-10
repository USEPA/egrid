## -------------------------------
##
## Data load EIA
## 
## Purpose: 
## 
## This file downloads the necessary data sets from the EIA website.  
## 
## Authors:  
##      Sean Bock, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load necessary functions
source("scripts/functions/function_download_eia_files.R")
source("scripts/functions/function_check_params.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")
}

# Download each EIA file from the EIA website ------------ 
# Iterating over each file, downloading, and unzipping

invisible(purrr::map(c("860", "861", "923"), ~ download_eia_files(form = .x, year = params$eGRID_year)))

# Download Puerto Rico files -----------
# Downloading Puerto Rico .xls file separately since it isn't zipped

url_860m  <- glue::glue("https://www.eia.gov/electricity/data/eia860m/archive/xls/december_generator{params$eGRID_year}.xlsx")

path_860m <- glue::glue("data/raw_data/860/{params$eGRID_year}/eia_pr_860m.xlsx")

if(!file.exists(path_860m)){
    download.file(url = url_860m,
                  destfile = path_860m, 
                  mode = "wb") 
} else {
   print("File eia_pr_860m.xlsx already exists in folder. Stopping.")
}

