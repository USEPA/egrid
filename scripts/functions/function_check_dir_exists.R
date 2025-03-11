## -------------------------------
##
## Check if directory exists and create if not
## 
## Purpose: 
## 
## This file creates a function that creates a directory if it does not already exist
##
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------

check_dir_exists <- function(folder_paths) { 
  
  #' @param folder_path path to the folder to check if it exists in vector format
  #' @return A print statement that the folder already exists or create the folder 
  
  for (path in folder_paths) { 
    if(dir.exists(path)) { 
      print(glue::glue("{path} already exists!"))
    } else { 
      dir.create(path)
      print(glue::glue("{path} created."))}}}