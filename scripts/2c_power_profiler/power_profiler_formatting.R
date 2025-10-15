## -------------------------------
##
## Power Profiler Formatting
## 
## Purpose: 
## 
## This file formats the power profiler output data
## as an Excel file with two sheets:
## 1) ZipSubregion for Website (zipcode subregion and predominant utility assignments)
## 2) ZipSubregion for Excel Tool (zipcode primary, secondary, and tertiary assignments)
##
## Authors:
##      
##      Madeline Zhang, Abt Global
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries -----
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(openxlsx)

# Define eGRID year parameter ----------------

# Load necessary functions
source("scripts/functions/function_check_params.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and temporal resolution parameters are already defined.")
}

# Load in power profiler data ----
zip_utility_subregion <- read_rds(glue::glue("data/2c_power_profiler/outputs/{params$eGRID_year}/zip_utility_subregion.RDS")) %>%
          rename("Zip code" = "zip",
                 "State" = "state",
                 "Utility ID " = "eiaid",
                 "Utility name" = "utility_name",
                 "Subregion" = "subregion",
                 "Predominant utility" = "predominant_utility") 

zip_subregion_assignment <- read_rds(glue::glue("data/2c_power_profiler/outputs/{params$eGRID_year}/zip_subregion_assignments.RDS")) %>%
          rename("Zip code" = "zip",
                 "State" = "state",
                 "Subregion 1" = "subregion_1",
                 "Subregion 2" = "subregion_2",
                 "Subregion 3" = "subregion_3")

# Create format styles
header_style <- createStyle(fgFill = "#BFBFBF", 
                            wrapText = TRUE,
                            halign = "center",
                            border = "TopBottomLeftRight",
                            borderStyle = "thin")

border_style <- createStyle(border = "TopBottomLeftRight",
                          borderStyle = "thin",
                          borderColour = "gray")

# Create workbook
wb <- createWorkbook()
addWorksheet(wb, "ZipSubregion for Website")
addWorksheet(wb, "ZipSubregion for Excel Tool")

# Write data
writeData(wb, 
          sheet = 1, 
          x = zip_utility_subregion, 
          startRow = 1)

writeData(wb, 
          sheet = 2, 
          x = zip_subregion_assignment, 
          startRow = 1)

# Add header styles
addStyle(wb, sheet = 1, style = header_style,  rows = 1, cols = 1:6, gridExpand = TRUE)
addStyle(wb, sheet = 2, style = header_style,  rows = 1, cols = 1:5, gridExpand = TRUE)

# Add border styles
addStyle(wb, sheet = 1, style = border_style,  rows = 2:nrow(zip_utility_subregion), cols = 1:6, gridExpand = TRUE)
addStyle(wb, sheet = 2, style = border_style,  rows = 2:nrow(zip_subregion_assignment), cols = 1:5, gridExpand = TRUE)

# Set column widths
setColWidths(wb, sheet = 1, cols = 4, widths = 59.43)
setColWidths(wb, sheet = 1, cols = 6, widths = 20)
setColWidths(wb, sheet = 2, cols = 1, widths = 13)
setColWidths(wb, sheet = 2, cols = 3:5, widths = 20)

# Save output
output_path <- glue::glue("data/2c_power_profiler/outputs/{params$eGRID_year}/ZipSubregion{params$eGRID_year}.xlsx")
saveWorkbook(wb, output_path, overwrite = TRUE)