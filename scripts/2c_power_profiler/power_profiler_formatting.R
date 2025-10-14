## -------------------------------
##
## Power Profiler Formatting
## 
## Purpose: 
## 
## This file 
##
## Additional notes
##      
##      Madeline Zhang, Abt Global
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

# Load in data
sheet1 <- read_rds(glue::glue("data/2c_power_profiler/outputs/{params$eGRID_year}/zip_utility_subregion.RDS")) %>%
          rename("Zip code" = "zip",
                 "State" = "state",
                 "Utility ID " = "eiaid",
                 "Utility name" = "utility_name",
                 "Subregion" = "subregion",
                 "Predominant utility" = "predominant_utility") 

sheet2 <- read_rds(glue::glue("data/2c_power_profiler/outputs/{params$eGRID_year}/zip_subregion_assignments.RDS")) %>%
          rename("Zip code (character)" = "zip",
                 "State" = "state",
                 "Subregion 1" = "subregion_1",
                 "Subregion 2" = "subregion_2",
                 "Subregion 3" = "subregion_3") %>%
          select(-zip_numeric)

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
addWorksheet(wb, "ZipRegion for Website")
addWorksheet(wb, "ZipRegion for Excel Tool")

# Write data
writeData(wb, 
          sheet = 1, 
          sheet1, 
          startRow = 1)

writeData(wb, 
          sheet = 2, 
          sheet2, 
          startRow = 1)

# Add header styles
addStyle(wb, sheet = 1, style = header_style,  rows = 1, cols = 1:6, gridExpand = TRUE)
addStyle(wb, sheet = 2, style = header_style,  rows = 1, cols = 1:5, gridExpand = TRUE)

# Add border styles
addStyle(wb, sheet = 1, style = border_style,  rows = 2:nrow(sheet1), cols = 1:6, gridExpand = TRUE)
addStyle(wb, sheet = 2, style = border_style,  rows = 2:nrow(sheet2), cols = 1:5, gridExpand = TRUE)

# Set column widths
setColWidths(wb, sheet = 1, cols = 4, widths = 59.43)
setColWidths(wb, sheet = 1, cols = 6, widths = 20)
setColWidths(wb, sheet = 2, cols = 1, widths = 13)
setColWidths(wb, sheet = 2, cols = 3:5, widths = 20)

# Save output
output <- glue::glue("data/2c_power_profiler/outputs/{params$eGRID_year}/ZipSubregion2023.xlsx")
saveWorkbook(wb, output, overwrite = TRUE)