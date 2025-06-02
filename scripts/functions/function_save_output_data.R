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
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

save_output_data <- function(data, output_folder_path, filename){
  
  #' save_output_data
  #' 
  #' Function to save RDS data in the output file and 
  #' create directories when necessary
  #' 
  #' @param data Dataset variable name to save
  #' @param output_folder String name of output folder to save to
  #' @param filename String name of new file being saved
  #' 
  #' @return Saves the RDS dataset in {output_folder_path}/{crosswalk_year}
  #'         directory
  #'         
  #' @examples 
  #' # Save PM2.5 plant file
  #' save_output_data(pm_plant_formatted, "1_production_model", "pm_plant_file.RDS")
  
  # annual files
  if (str_detect(filename, "annual")) {
    # create save directories if they don't exist
    # if(dir.exists(glue::glue("{output_folder_path}"))) {
    #   print(glue::glue("Folder {output_folder_path} already exists."))
    # } else {
    #   dir.create(glue::glue("{output_folder_path}"))
    # }
    # 
    # # create folder for eGRID year
    # if(dir.exists(glue::glue("{output_folder_path}/{params$eGRID_year}"))) {
    #   print(glue::glue("Folder {output_folder_path}/{params$eGRID_year} already exists."))
    # } else {
    #   dir.create(glue::glue("{output_folder_path}/{params$eGRID_year}"))
    # }
    
    # create folder for annual or monthly
    if(dir.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/annual"))) {
      print(glue::glue("Folder {output_folder_path}/{params$eGRID_year}/annual already exists."))
    } else {
      dir.create(glue::glue("{output_folder_path}/{params$eGRID_year}/annual"))
    }
    
    print(glue::glue("Saving {filename} to folder {output_folder_path}/{params$crosswalk_year}/annual"))
    
    # save file
    write_rds(data, glue::glue("{output_folder_path}/{params$eGRID_year}/annual/{filename}"))
    
    # check if file is successfully written to folder
    if(file.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/annual/{filename}"))){
      print(glue::glue("File {filename} successfully written to folder {output_folder_path}/{params$eGRID_year}/annual"))
    } else {
      print(glue::glue("File {filename} failed to write to folder."))
    }
    
    # monthly files
  } else if (str_detect(filename, "monthly")) {
    # # create save directories if they don't exist
    # if(dir.exists(glue::glue("{output_folder_path}"))) {
    #   print(glue::glue("Folder {output_folder_path} already exists."))
    # } else {
    #   dir.create(glue::glue("{output_folder_path}"))
    # }
    # 
    # # create folder for eGRID year
    # if(dir.exists(glue::glue("{output_folder_path}/{params$eGRID_year}"))) {
    #   print(glue::glue("Folder {output_folder_path}/{params$eGRID_year} already exists."))
    # } else {
    #   dir.create(glue::glue("{output_folder_path}/{params$eGRID_year}"))
    # }
    # 
    # create folder for annual or monthly
    if(dir.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/monthly"))) {
      print(glue::glue("Folder {output_folder_path}/{params$eGRID_year} already exists."))
    } else {
      dir.create(glue::glue("{output_folder_path}/{params$eGRID_year}/monthly"))
    }
    
    print(glue::glue("Saving {filename} to folder {output_folder_path}/{params$eGRID_year}/monthly"))
    
    # save file
    write_rds(data, glue::glue("{output_folder_path}/{params$eGRID_year}/monthly/{filename}"))
    
    # check if file is successfully written to folder
    if(file.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/monthly/{filename}"))){
      print(glue::glue("File {filename} successfully written to folder {output_folder_path}/monthly/{params$eGRID_year}"))
    } else {
      print(glue::glue("File {filename} failed to write to folder."))
    }
    
    # non-temporal-res files
  } else {
    # create save directories if they don't exist
    # if(dir.exists(glue::glue("{output_folder_path}"))) {
    #   print(glue::glue("Folder {output_folder_path} already exists."))
    # } else {
    #   dir.create(glue::glue("{output_folder_path}"))
    # }
    
    if(dir.exists(glue::glue("{output_folder_path}/{params$eGRID_year}"))) {
      print(glue::glue("Folder {output_folder_path}/{params$eGRID_year} already exists."))
    } else {
      dir.create(glue::glue("{output_folder_path}/{params$eGRID_year}"))
    }
    
    print(glue::glue("Saving {filename} to folder {output_folder_path}/{params$eGRID_year}"))
    
    # save file
    write_rds(data, glue::glue("{output_folder_path}/{params$eGRID_year}/{filename}"))
    
    # check if file is successfully written to folder
    if(file.exists(glue::glue("{output_folder_path}/{params$eGRID_year}/{filename}"))){
      print(glue::glue("File {filename} successfully written to folder {output_folder_path}/{params$eGRID_year}"))
    } else {
      print(glue::glue("File {filename} failed to write to folder."))
    }
    
  }
  
  
}