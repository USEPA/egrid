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
##      Teagan Goforth, Abt Global, teagan.goforth@abtglobal.com
##
## -------------------------------


# Iterating over each file, downloading, and unzipping

invisible(purrr::map(c("860", "861", "923"), ~ download_eia_files(form = .x, year = params$eGRID_year)))

# Downloading Puerto Rico .xls file separately since it isn't zipped

url_860m  <- glue::glue("https://www.eia.gov/electricity/data/eia860m/archive/xls/december_generator{params$eGRID_year}.xlsx")

path_860m <- glue::glue("data/raw_data/860m.xlsx")

if(!file.exists(path_860m)){
    download.file(url = url_860m,
                  destfile = path_860m)
}else{
  print("Stopping. File 860m.xlsx already downloaded.")
}

