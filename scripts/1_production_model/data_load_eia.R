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

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Load necessary functions
source("scripts/functions/function_download_eia_files.R")

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

