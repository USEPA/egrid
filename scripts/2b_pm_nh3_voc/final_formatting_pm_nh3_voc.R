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

# Ultimately loop through emission types -----
emission_type <- "pm25"
if(emission_type == "pm25") {
  emission_abbrev <- "pm"
} else {
  emission_abbrev <- emission_type
}

# Define selection of style types -----
headers_long <- createStyle(
  fontName = "Arial",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  fgFill = "#F2F2F2",
  border = "TopBottomLeftRight",
  borderColour = "black",
  borderStyle = "thin")

headers_abbrev <- createStyle(
  fontName = "Arial",
  textDecoration = "bold",
  halign = "left",
  valign = "bottom",
  fgFill = "#F2F2F2",
  border = "TopBottomLeftRight",
  borderColour = "black",
  borderStyle = "thin")

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

# define level abbreviations to assist in renaming
level_abbrev <-c("unit" = "",
                 "plant" = "PL",
                 "state" = "ST",
                 "subregion" = "SR")

# Create workbook -----
wb <- createWorkbook()

# set base font
modifyBaseFont(wb, fontName = "Arial", fontSize = 8.5)

# Loop through emission levels and write and format data
for(emission_level in c("unit", "plant", "state", "subregion")) {
  
  ## Rename and format level data -----
  
  # define names of name match and emissions data variables
  name_matches <- paste0(emission_level, "_nonmetric")
  emission_data <- paste0(emission_level, "_file")
  
  # match additional column names present in emissions data not in name_matches
  colnames_new <- setNames(c(paste0(emission_type, "_ann"),
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

  # select name matches and new column names present in emissions data
  colnames <- c(get(name_matches)[get(name_matches) %in% colnames(get(emission_data))],
                colnames_new[colnames_new %in% colnames(get(emission_data))])
  
  # rename emission data column names
  emission_data_formatted <-
    get(emission_data) %>%
    rename(!!!setNames(lapply(colnames, sym), names(colnames)))

  ## Create new worksheet for emissions level -----

  current_worksheet <- glue::glue("{params$eGRID_year} {toupper(emission_abbrev)} {str_to_title(emission_level)}-level Data")
  addWorksheet(wb, sheetName = current_worksheet)
  
  ## Write data to new worksheet -----
  
  # full description headers
  
  # emissions data
  writeData(wb, sheet = current_worksheet, x = emission_data_formatted, startCol = 1, startRow = 2)

  ## Format worksheet -----
  
  ### Headers -----
  
  # assign column type for color-coding based on variable naming
  if(emission_level == "unit") {
    annual_cols <- c()
    unadj_annual_cols <- which(grepl("AN$|RT$|SRC$", colnames(emission_data_formatted)))
  } else {
    annual_cols <- which(grepl("AN$", colnames(emission_data_formatted)))
    unadj_annual_cols <- which(grepl("^UN", colnames(emission_data_formatted)))
  }
  output_rate_cols <- which(grepl("RTA$", colnames(emission_data_formatted)))
  input_rate_cols <- which(grepl("RA$", colnames(emission_data_formatted)))
  
  # grey headers
  addStyle(wb, current_worksheet, headers_long, rows = 1, cols = 1:sheetWidth, 
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, headers_abbrev, rows = 2, cols = 1:sheetWidth, 
           stack = TRUE, gridExpand = TRUE)
  
  # color-coded headers
  addStyle(wb, current_worksheet, createStyle(fgFill = "#F2DCDB"), rows = 1:2, 
           cols = annual_cols, stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, createStyle(fgFill = "#E6B8B7"), rows = 1:2, 
           cols = unadj_annual_cols, stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, createStyle(fgFill = "#EBF1DE"), rows = 1:2, 
           cols = output_rate_cols, stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, createStyle(fgFill = "#C4D79B"), rows = 1:2, 
           cols = input_rate_cols, stack = TRUE, gridExpand = TRUE)
  
  ### Cell Sizes ------
  setRowHeights(wb, current_worksheet, rows = 1, heights = 54)
  setRowHeights(wb, current_worksheet, rows = 2:sheetLength, heights = 10.5)
  setColWidths(wb, current_worksheet, cols = c(1:sheetWidth), widths = 12.7)

  ### Data Types -----
  
}

saveWorkbook(wb, glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/{emission_type}_final.xlsx"), 
             overwrite = TRUE)

