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

save_output_data <- function(data, filename){
  
  #' save_output_data
  #' 
  #' Function to save RDS data in the output file and 
  #' create directories when necessary
  #' 
  #' @param data Dataset variable name to save
  #' @param filename String name of new file being saved
  #' 
  #' @return Saves the RDS dataset in outputs/{params$eGRID_year}
  #'         directory
  #'         
  #' @examples 
  #' # Save PM2.5 plant file
  #' save_output_data(pm_plant_formatted, "pm_plant_file.RDS")

  
  # create save directories if they don't exist
  if(dir.exists("data/outputs")) {
    print("Folder outputs already exists.")
  } else {
    dir.create("data/outputs")
  }
  
  if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
    print(glue::glue("Folder outputs/{params$eGRID_year} already exists."))
  } else {
    dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
  }
  
  print(glue::glue("Saving {filename} to folder data/outputs/{params$eGRID_year}"))
  
  # save file
  write_rds(data, glue::glue("data/outputs/{params$eGRID_year}/{filename}"))
  
  # check if file is successfully written to folder
  if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/{filename}"))){
    print(glue::glue("File {filename} successfully written to folder data/outputs/{params$eGRID_year}"))
  } else {
    print(glue::glue("File {filename} failed to write to folder."))
  }
}