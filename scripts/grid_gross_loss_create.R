## -------------------------------
##
## Grid gross loss create 
## 
## Purpose: 
## 
## This file creates the grid gross loss calculation for eGRID. 
## This includes all operating units for the specified eGRID data year
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------

# Load libraries ------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)

## Read in EIA files ------

eia_860 <- read_rds("data/clean_data/eia/eia_860_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia/eia_923_clean.RDS")

# define params for eGRID data year 
# this is only necessary when running the script outside of egrid_master.qmd

params <- list()
params$eGRID_year <- "2021"


## viewing data tables (remove later)
eia_operable <- eia_860$operable
eia_860_combined <- eia_860$combined
eia_923_generation_and_fuel <- eia_923$generation_and_fuel_data

## Function to download EIA files from website (adapted from function: "download_eia_files" )-------
## These urls are different depending whether or not they're the most recent data. If not most recent year, "archive" is in the url



download_eia_ggl <- function() {
  
  #' download_eia_ggl
  #' 
  #' Function to download data from EIA website (https://www.eia.gov/electricity/) for all states. Each states' data is stored in a Excel file on the EIA website. This function extracts all of the tables and combines it into one and creates a summary table.
  
  #' @return Unzipped Excel file
  #' @examples
  #' download_eia_files("923") # Download a single form's data
  #' purrr::map(c("923","860","861"), ~ download_eia_files(.x)) # Download and unzip forms 923, 860, and 861
  
  state <- gsub(" ", "", tolower(datasets::state.name))
  state_abbr <- tolower(datasets::state.abb)

  
  # create a eia_ggl folder if one does not already exists
  new_folder <- "data/raw_data/eia_ggl"
  new_folder2 <- "data/clean_data/eia_ggl"

  if (!dir.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
  }
  
  
  if (!dir.exists(new_folder2)) {
    dir.create(new_folder2, recursive = TRUE)
  }
  
  # function that extracts files from EIA website
  download_file <- function(url, dest_file, new_folder) {
    
    download.file(url, dest_file, mode = "wb")
    end <- paste("Downloading to:", new_folder)
    
    return(end)
  }
  
  
  # Check if there are other files in the folder
  existing_files <- list.files(new_folder)
  
  if (length(existing_files) > 1) {
    print(glue::glue("Files already exist in folder:{new_folder}. Stopping"))
    return(TRUE)
  }
  
  existing_files2 <- list.files(new_folder2)
  
  if (length(existing_files2) > 1) {
    print(glue:glue("Files already exist in folder:{new_folder2}. Stopping"))
    return(TRUE)
  }
  
  # initialize list of tables and workbook
  ggl_data <- list()
  ggl_wb <- createWorkbook()
  ggl_summary_data <- list()
  
  
  # loop for all of the states and files
  for (i in 1:length(state_abbr)){
    
    # initialize variables for i
    name <- state[i]
    abbr <- state_abbr[i]
    
    # download file for corresponding state i from EIA
    url <- glue::glue("https://www.eia.gov/electricity/state/{name}/xls/{abbr}.xlsx")
    dest_file <- glue::glue("{new_folder}/{abbr}.xlsx")
    
    download_file(url,dest_file,new_folder) 
    
    # select table needed for GGL calculation (Table 10: Supply and disposition of energy)
    select_table <- read_excel(dest_file, sheet = 11)
    ggl_data[[i]] <- select_table
    
    # add table to workbook as a sheet
    addWorksheet(ggl_wb, sheetName = toupper(abbr))
    writeData(ggl_wb, sheet = toupper(abbr), ggl_data[[i]])
    
  }
    
  
  ggl_file <- glue::glue("{new_folder2}/R_ggl.xlsx")
  saveWorkbook(ggl_wb, ggl_file, overwrite = TRUE)
  
  # if (!download_and_unzip(url, dest_file, new_folder) && stringr::str_detect(url, "/archive")) {
  #   url <- stringr::str_replace(url, "/archive", "")
  #   if (!download_and_unzip(url, dest_file, new_folder)) {
  #     print(paste("Failed to download or unzip:", dest_file))
  #  }
  #}
  
  
}

download_eia_ggl()

## area to develop new parts of the function :-3
# new_folder <- glue::glue("data/raw_data/eia_ggl")
# dest_file1 <- glue::glue("{new_folder}/al.xlsx")
# select_table1 <- read_excel(dest_file1, sheet = 11)
# 
# ggl_data <- list()
# ggl_data[[1]] <- select_table1
# 
# wb <- createWorkbook()
# addWorksheet(wb, sheetName = 'AL')
# writeData(wb, sheet = 'AL', ggl_data[[1]])
# 
# ggl_file <- glue::glue("{new_folder}/R_ggl.xlsx")
# saveWorkbook(wb, ggl_file, overwrite = TRUE)


## 01: Create GGL table at state level for non duplicate states

# select - state and interconnection, state postal code, state 

