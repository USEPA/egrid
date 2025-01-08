## -------------------------------
##
## Get sheets in EIA Excel files
## 
## Purpose: 
## 
## This file creates a function that returns the names of sheets in EIA Excel files.
##
## Authors:  
##      Sean Bock, Abt Global
##
## -------------------------------


get_sheets <- function(form){
  
  #' get_sheets
  #' 
  #' Return names of sheets in EIA Excel files
  #' @param form Name of EIA form (e.g., "923")
  #' @return A character vector with the names of all Excel sheets within a given EIA form.
  #' @examples
  #' get_sheets("923")
  
  
  path <- glue::glue("data/raw_data/{form}/{params$eGRID_year}")
  
  if(form != "860m") {
    
    files <- list.files(path)
    
    # Filter only Excel files
    excel_files <- stringr::str_subset(files, ".xls|.xlsx")
    
    # read each sheet and store them in a list
    sheet_data <- purrr::map(excel_files, ~ readxl::excel_sheets(glue::glue("{path}/{.x}")))
    
    # set names for each element based on file name
    sheet_data_named <- setNames(sheet_data, excel_files)
    
    return(sheet_data_named)
    
  } else{
    
    sheet_data <- readxl::excel_sheets(glue::glue("data/raw_data/860/{params$eGRID_year}/eia_pr_860m.xlsx"))
    
    return(sheet_data)
    
  }
}