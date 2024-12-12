## -------------------------------
##
## Metric conversion function
## 
## Purpose: 
## 
## This function creates new .RDS output files with unit conversions to metric
## when necessary utilizing conversion table with conversion rates
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

metric_conversion <- function(which_file) {
  
  #' metric_conversion
  #' 
  #' Function to create metric versions of eGRID files
  #' 
  #' @param which_file The shorthand name of original file 
  #'                   used for creation of metric counterpart -
  #'                   Limited to ("unit","gen","plant","state",
  #'                   "ba","subregion","nerc","us","ggl")
  #' 
  #' @return A new .RDS file with metric structure
  #' 
  #' @examples 
  #' # Create unit metric file
  #' metric_conversion("unit")
  #' # Create balancing authority aggregation metric file
  #' metric_conversion("ba")
  
  print(glue::glue("CREATING {toupper(which_file)} METRIC FILE"))
  
  # List required packages  ------------------------------------------
  
  require(dplyr)
  require(glue)
  require(readr)
  require(readxl)
  
  # Function to convert data to metric -----------------------------------------
  
  convert_to_metric <- function(data_col, var_name) {
    
    #' convert_to_metric
    #'
    #' Function to convert data columns from imperial to metric units
    #' Formatted to be used within an across() function
    #' 
    #' @param data_col Column data to convert 
    #' @param var_name Name of variable to convert
    #' 
    #' @return New data column converted to new units
    #' 
    #' @examples 
    #' # Converting hg_mass column in plant file to metric
    #' convert_to_metric(orig_data$hg_mass, "hg_mass")
    #' # Creating generation_ann_metric column in plant file
    #' convert_to_metric(orig_data$generation_ann_metric,"generation_ann_metric")
    
    # variable row
    var_data <- 
      vars_to_convert %>%
      filter(var == var_name)
    # conversion rate for units
    convert_factor <- 
      as.numeric(convert_rates %>%
                   filter(from == var_data$imperial & 
                            to == var_data$metric) %>%
                   select(conversion))
    # skip vars with empty characters, otherwise multiply data by conversion factor
    ifelse(is.character(data_col), return (data_col), return (data_col * convert_factor))
  }
  
  # Select file for conversion -------------------------------------------------
  
  filename <- filenames_orig[which_file]
  
  # assign ordered name vector for file type
  assign("ordered_names", get(glue::glue("{which_file}_metric")))
  
  # Load original data and metric structure ------------------
  
  # original output data
  orig_data <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/{filename}.RDS")) 
  
  # metric file structure
  metric_struct <- read_excel("data/static_tables/metric_structure.xlsx",
                                 # sheet index is same index in list of data files
                                 sheet = which(filenames_orig == filename),
                                 # keep column names to include NA fields
                                 col_names = TRUE) 
                  
  # rename column names
  colnames(metric_struct) <- c("descrip", "name", "metric", "imperial", "new_field")
  
  # Add new column with ordered names ----------------------
  
  metric_struct_named <- 
    metric_struct %>%
    mutate(var = ordered_names, .after=name)
  
  # Define variables to convert  -------------------------------------------
  
  # all variables to change units
  vars_to_convert <- 
    metric_struct_named %>%
    # variables with differing metric and imperial units
    filter(!is.na(metric) & metric != imperial)
    
  # new columns in metric structure
  vars_convert_new <- 
    vars_to_convert %>%
    filter(!is.na(new_field)) %>% # is a new field
    # remove _metric naming to select original data for conversion
    mutate(var = stringr::str_remove(var, "_metric"))
  
  # Convert data to new metric units ---------------------------------------------

  metric_data <- 
    orig_data %>%
    # add duplicate rows for new metric variables
    mutate(across(.cols = any_of(as.vector(unlist(vars_convert_new["var"]))),
                  .fns = ~ .,
                  .names = "{.col}_metric")) %>%
    # convert units to metric
    mutate(across(.cols = any_of(as.vector(unlist(vars_to_convert["var"]))),
                  .fns = ~ convert_to_metric(., cur_column()))) %>%
    # select and order based on ordered names 
    select(all_of(ordered_names))
  
  # list updates to file
  print(glue::glue("{nrow(vars_to_convert) - nrow(vars_convert_new)} columns altered"))
  print(glue::glue("{ncol(metric_data) - ncol(orig_data)} columns added"))
  
  # rename to longer variable names after sorting
  names(metric_data) <- ordered_names
  
  # Export file -------------
  
  # check if data output folder exists, if not make folder
  save_dir <- glue::glue("data/outputs/{params$eGRID_year}")
  if(!dir.exists(save_dir)) {
    dir.create(save_dir)
    print(glue::glue("Folder {save_dir} created."))
  }
  
  # save folder to outputs file
  print(glue::glue("Saving {filename}_metric.RDS to {save_dir}"))
  write_rds(metric_data, glue::glue("{save_dir}/{filename}_metric.RDS"))
}