## -------------------------------
##
## Format headers PM, NH3, VOC function
## 
## Purpose: 
## 
## These functions:
##    1) create header description data for the specified
##        emission level (unit, plant, state, subregion) to format the final
##        .xlsx file for PM, NH3, and VOC emissions. 
##    2) updates formatting of .xlsx file imported to match the current standard
##        prior to adding the new year's data
##
##
## Authors:
##      Emma Russell, Abt Global
##
## -------------------------------

format_headers_pm_nh3_voc <- function(emission_level) {
  
  #' @name format_headers_pm_nh3_voc
  #' 
  #' Function to create header description data for final formatting script of 
  #' pm2.5, nh3, or voc emissions. Headers are the same across emission types
  #' 
  #' @param emission_level Emission level to produce header data - must be the following:
  #'                       "unit", "plant", "state", "subregion" (string)
  #'                       
  #' @return Vector of header descriptors
  #'         
  #' @examples 
  #' # Create unit-level header descriptor data (same for PM2.5, NH3, and VOC)
  #' pm_unit_headers <- format_headers_pm_nh3_voc("unit")

  if(emission_level == "unit") {
    headers <- c(
      "Data Year",
      "Plant state abbreviation",
      "Plant name",
      "DOE/EIA ORIS plant or facility code",
      "Unit ID",
      "Prime Mover",
      "Unit Operational Status",
      "Unit bottom and firing type",
      "Unit primary fuel",
      "Unit operating hours",
      "Unit unadjusted annual heat input (MMBtu)",
      paste("Unit unadjusted annual", emission_header, "emissions (tons)"),
      paste("Unit annual", emission_header, "emission rate (lb/MMBtu)"),
      "Unit unadjusted annual heat input source",
      paste("Unit unadjusted annual", emission_header, "emissions source"),
      "Unit year on-line")
    
  } else if(emission_level == "plant") {
    headers <- c(
      "Data Year",
      "Plant state abbreviation",
      "Plant name",
      "DOE/EIA ORIS plant or facility code",
      "eGRID subregion acronym",
      "eGRID subregion name",
      "Plant primary fuel",
      "Plant nameplate capacity (MW)",
      "CHP plant electric allocation factor",
      "Plant annual net generation (MWh)",
      "Plant total annual heat input (MMBtu)",
      paste("Plant annual", emission_header, "emissions (tons)"),
      paste("Plant annual", emission_header, "total output emission rate (lb/MWh)"),
      paste("Plant annual", emission_header, "total input emission rate (lb/MMBtu)"),
      paste(emission_header, "Source"),
      "Plant unadjusted total annual heat input (MMBtu)",
      paste("Plant unadjusted annual", emission_header, "emissions (tons)"))
    
  } else if(emission_level == "state") {
    headers <- c(
      "Data Year",
      "Plant state abbreviation",
      "State annual net generation (MWh)",
      paste("State annual", emission_header, "emissions (tons)"),
      paste("State annual", emission_header, "total output emission rate (lb/MWh)"))
    
  } else if(emission_level == "subregion") {
    headers <- c(
      "Data Year",
      "eGRID subregion acronym",
      "eGRID subregion name",
      "Subregion annual net generation (MWh)",
      paste(emission_header, "Emissions (tons)"),
      paste(emission_header, "Total Output Emission rate (lb/MWh)"))
  }
  
  return(headers)
}

update_wb_formatting_pm_nh3_voc <- function(wb, emission_type) {
  
  #' @name update_wb_formatting_pm_nh3_voc
  #' 
  #' Function to update the formatting of the .xlsx of previous emissions data
  #' downloaded from EPA to match the current standard. Function is specific to 
  #' the current formatting discrepancies.
  #' 
  #' @param wb Workbook object from .xlsx file to revise 
  #' @param emission_type Emission type of .xlsx file - either
  #'                      "pm", "nh3", or "voc" (string)
  #'                       
  #' @return Revised workbook object with the appropriate formatting ready to be
  #'         used for the duration of the final formatting script
  #'         
  #' @examples 
  #' # Update workbook to fix formatting discrepancies and match standard for 
  #'   PM2.5 .xlsx document downloaded from EPA
  #' pm_unit_headers <- update_wb_formatting_pm_nh3_voc(wb, "pm")
  
  # rename subregion tabs to fix discrepancy
  if (emission_type == "pm") {
    # get sheet names
    sheet_names <- names(wb)
    
    # filter to subregion names
    subregion_sheets <- sheet_names[grepl("Subregion Rates", sheet_names)]
    
    # rename worksheets
    for (sheet in subregion_sheets) {
      renameWorksheet(wb, sheet, str_replace(sheet, "Subregion Rates", glue::glue("{toupper(emission_type)} Subregion-level Data")))
    }
    
  } else if (emission_type == "nh3") {
    # rename emissions column abbreviations from PM naming to NH3 naming
    # get sheet names
    sheet_names <- names(wb)
    
    # filter to subregion names
    emission_level_sheets <- sheet_names[grepl(str_to_sentence(emission_level), sheet_names)]
    
    for (sheet in emission_level_sheets) {
      # update header descriptions and shortforms
      writeData(wb, sheet, headers_longform, startCol = 1, startRow = 1, colNames = FALSE)
      writeData(wb, sheet, headers_shortform, startCol = 1, startRow = 2, colNames = FALSE)
      
      # add freeze pane
      if (emission_level == "unit") {
        freezePane(wb, sheet, firstActiveCol = 6, firstActiveRow = 3)
      } else if (emission_level == "plant") {
        freezePane(wb, sheet, firstActiveCol = 5, firstActiveRow = 3)
      } else {
        freezePane(wb, sheet, firstActiveRow = 3)
      }
    }
  }
  return(wb)
}

