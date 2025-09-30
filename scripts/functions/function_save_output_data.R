## -------------------------------
##
## Save output data
## 
## Purpose: 
## 
## This function saves RDS output datasets in the output 
## folder by first checking if directories exist and creating 
## them where necessary.
##
## Authors:
##      Emma Russell, Abt Global
##
## -------------------------------

save_output_data <- function(data, output_folder_path, file_name, file_type="RDS"){
  
  #' save_output_data
  #' 
  #' Function to save RDS data in the output file and 
  #' create directories when necessary
  #' 
  #' @param data Dataset variable name to save
  #' @param output_folder String name of output folder to save to
  #' @param file_name String name of new file being saved
  #' 
  #' @return Saves the RDS dataset in {output_folder_path}/{params$eGRID_year}
  #'         directory
  #'         
  #' @examples 
  #' # Save PM2.5 plant file
  #' save_output_data(pm_plant_formatted, "data/outputs/1_production_model", "pm_plant_file.RDS")
  
  require(stringr)
  
  # annual files
  if (str_detect(file_name, "annual") & !str_detect(file_name, "epa")) {
    
    # create save directories if they don't exist
    if(!dir.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/annual"))) {
      dir.create(glue::glue("{output_folder_path}/{params$eGRID_year}/annual"), recursive = TRUE)
    }
    
    print(glue::glue("Saving {file_name} to folder {output_folder_path}/{params$eGRID_year}/annual"))
    
    # save file
    if(file_type == "RDS"){
      write_rds(data, glue::glue("{output_folder_path}/{params$eGRID_year}/annual/{file_name}"))
    } else if(file_type == "CSV") { 
      write.csv(data, glue::glue("{output_folder_path}/{params$eGRID_year}/annual/{file_name}"), na="", row.names = FALSE)
    }
    
    # check if file is successfully written to folder
    if(file.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/annual/{file_name}"))){
      print(glue::glue("File {file_name} successfully written to folder {output_folder_path}/{params$eGRID_year}/annual"))
    } else {
      print(glue::glue("File {file_name} failed to write to folder."))
    }
    
    # monthly files
  } else if (str_detect(file_name, "monthly") & !str_detect(file_name, "epa")) {
    
    # create save directories if they don't exist
    if(!dir.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/monthly"))) {
      dir.create(glue::glue("{output_folder_path}/{params$eGRID_year}/monthly"), recursive = TRUE)
    }
    
    print(glue::glue("Saving {file_name} to folder {output_folder_path}/{params$eGRID_year}/monthly"))
    
    # save file
    if(file_type == "RDS"){
      write_rds(data, glue::glue("{output_folder_path}/{params$eGRID_year}/monthly/{file_name}"))
    } else if(file_type == "CSV") { 
      write.csv(data, glue::glue("{output_folder_path}/{params$eGRID_year}/monthly/{file_name}"), na="", row.names = FALSE)
    }

    # check if file is successfully written to folder
    if(file.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/monthly/{file_name}"))){
      print(glue::glue("File {file_name} successfully written to folder {output_folder_path}/{params$eGRID_year}/monthly"))
    } else {
      print(glue::glue("File {file_name} failed to write to folder."))
    }
    
    # non-temporal-res files
  } else {
    
    # create save directories if they don't exist
    if(!dir.exists(glue::glue("{output_folder_path}/{params$eGRID_year}"))) {
      dir.create(glue::glue("{output_folder_path}/{params$eGRID_year}"), recursive = TRUE)
    }
    
    print(glue::glue("Saving {file_name} to folder {output_folder_path}/{params$eGRID_year}"))
    
    # save file
    if(file_type == "RDS"){
      write_rds(data, glue::glue("{output_folder_path}/{params$eGRID_year}/{file_name}"))
    } else if(file_type == "CSV") { 
      write.csv(data, glue::glue("{output_folder_path}/{params$eGRID_year}/{file_name}"), na="", row.names = FALSE)
    }

    # check if file is successfully written to folder
    if(file.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/{file_name}"))){
      print(glue::glue("File {file_name} successfully written to folder {output_folder_path}/{params$eGRID_year}"))
    } else {
      print(glue::glue("File {file_name} failed to write to folder."))
    }
    
  }
}