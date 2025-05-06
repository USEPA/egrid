## -------------------------------
##
## Final Formatting PM NH3 VOC
## 
## Purpose: 
## 
## This file pulls data from .RDS files to create final version of saved data in excel sheet
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries --------
library(dplyr)
library(openxlsx)
library(readr)
library(readxl)
library(stringr)

params <- list()
params$eGRID_year <- "2022"

# Define eGRID year parameter ----------------
# define parameter year if no one is currently assigned using prompted user input
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

emission_type <- "pm25"
if(emission_type == "pm25") {
  emission_abbrev <- "pm"
} else {
  emission_abbrev <- emission_type
}

# Import .RDS data -----

# create a list of files in R directory
data_dir <- glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/")
filename_types <- list("unit_file_",
                         "plant_file_",
                         "state_aggregation_",
                         "subregion_aggregation_")
filenames <- lapply(filename_types, function(filename) paste0(filename, paste0(emission_type, ".RDS")))

# import files in filenames list
for (file in (filenames)){
  if(grepl("_aggregation", file)) {
    assign(str_replace(file, glue::glue("aggregation_{emission_type}.RDS"), "file"), read_rds(paste0(data_dir, file)))
  } else {
  assign(str_remove(file,glue::glue("_{emission_type}.RDS")), read_rds(paste0(data_dir, file)))
  }
}

# Load abbreviated name to snake_case matches -----
load("data/1_production_model/static_tables/name_matches.Rdata")

# Format emissions data for tables  -----
level_abbrev <-c("unit" = "",
                 "plant" = "PL",
                 "state" = "ST",
                 "subregion" = "SR")

for(emission_level in c("unit", "plant", "state", "subregion")) {
  name_matches <- paste0(emission_level, "_nonmetric")
  emission_data <- paste0(emission_level, "_file")
  
  # add additional column names present in unit data
  colnames_emissions <- setNames(c(paste0(emission_type, "_ann"),
                                   paste0(emission_type, "_output_rate"),
                                   paste0(emission_type, "_input_rate"),
                                   paste0(emission_type, "_rate"), 
                                   paste0(emission_type, "_source"),
                                   paste0("unadj_", emission_type)),
                                 c(paste0(level_abbrev[[emission_level]], toupper(emission_type), "AN"), 
                                   paste0(level_abbrev[[emission_level]], toupper(emission_type), "RTA"),
                                   paste0(level_abbrev[[emission_level]], toupper(emission_type), "RA"),
                                   paste0(level_abbrev[[emission_level]], toupper(emission_type), "RT"), 
                                   paste0(toupper(emission_type), "SRC"),
                                   paste0("UN", toupper(emission_type))))

  # select name matches present in data
  colnames <- c(get(name_matches)[get(name_matches) %in% colnames(get(emission_data))],
                colnames_emissions[colnames_emissions %in% colnames(get(emission_data))])
  
  # rename emission data column names
  emission_data_renamed <-
    get(emission_data) %>%
    rename(!!!setNames(lapply(colnames, sym), names(colnames)))
  
  # rename dataset variable
  assign(paste0(emission_level, "_formatted"), emission_data_renamed) %>%
    glimpse()
}

# Write Table Data ----
wb <- createWorkbook()

for(emission_level in c("unit", "plant", "state", "subregion")) {
  
  # create new worksheet
  current_worksheet <- glue::glue("{params$eGRID_year} {toupper(emission_abbrev)} {str_to_title(emission_level)}-level Data")
  addWorksheet(wb, sheetName = current_worksheet)
  
  # write in data including titles
  writeData(wb, sheet = current_worksheet, x = get(paste0(emission_level, "_formatted")), startCol = 1, startRow = 2)
}
  
saveWorkbook(wb, glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/{emission_type}_final.xlsx"), 
             overwrite = TRUE)

# Format Table

## Headers
## Subtitles
## Colors
## Font - size and BF
## Cell sizes

