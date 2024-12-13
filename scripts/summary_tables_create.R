## -------------------------------
##
## Create summary tables of finalized eGRID data.
## 
## Purpose: 
## 
## This file pulls data from .RDS files to create summary tables
## saved in an excel sheet.
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries ------------------------------------------

library(dplyr)
library(glue)
library(readr)
library(readxl)
#library(xlsx)
library(stringr)

# Define eGRID year ------------------

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.")
  } else { # if params() is defined, but eGRID_year is not, define it here
    params$eGRID_year <- as.character(readline(prompt = "Input eGRID_year: "))
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- as.character(readline(prompt = "Input eGRID_year: "))
}

# Import .RDS data --------------------------------

# create a list of files in R directory
rdata_dir <- glue::glue("data/outputs/{params$eGRID_year}/")
r_files <- list.files(path = rdata_dir, pattern = "*[^c].RDS")

# import files in list
for (file in (r_files)){
  assign(str_remove(file, ".RDS"), read_rds(paste0(rdata_dir, file)))
}

# Pull desired data

# Format to excel sheet
