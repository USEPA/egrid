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

# load in data
sheet1 <- read_rds(glue::glue("data/2b_power_profiler/outputs/{params$eGRID_year}/zip_utility_subregion.RDS")) %>%
          rename("Zip" = "zip",
                 "State" = "state",
                 "EIA_ID" = "eiaid",
                 "Utility_Name" = "utility_name",
                 "Subregion" = "subregion",
                 "Predominant_Utility" = "predominant_utility") 
sheet2 <- read_rds(glue::glue("data/2b_power_profiler/outputs/{params$eGRID_year}/zip_subregion_assignments.RDS")) %>%
          rename("ZIP_Character" = "zip",
                 "ZIP_Numeric" = "zip_numeric",
                 "State" = "state",
                 "eGRID_Subregion_1" = "subregion_1",
                 "eGRID_Subregion_2" = "subregion_2",
                 "eGRID_Subregion_3" = "subregion_3")

# create format styles
header_style <- createStyle(fgFill = "#BFBFBF", 
                            wrapText = TRUE,
                            halign = "center",
                            border = "TopBottomLeftRight",
                            borderStyle = "thin")

wb <- createWorkbook()
addWorksheet(wb, "ZipRegion for Website")
addWorksheet(wb, "ZipRegion for Excel Tool")

# write data
writeData(wb, 
          sheet = 1, 
          sheet1, 
          startRow = 1)

writeData(wb, 
          sheet = 2, 
          sheet2, 
          startRow = 1)

addStyle(wb, sheet = 1, style = header_style,  rows = 1, cols = 1:6, gridExpand = TRUE)
addStyle(wb, sheet = 2, style = header_style,  rows = 1, cols = 1:6, gridExpand = TRUE)
setColWidths(wb, sheet = 1, cols = 4, widths = 59.43)


output <- glue::glue("data/2b_power_profiler/outputs/{params$eGRID_year}/ZipSubregion2023.xlsx")
saveWorkbook(wb, output, overwrite = TRUE)