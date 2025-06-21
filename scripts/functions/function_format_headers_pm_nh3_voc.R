## -------------------------------
##
## Format headers PM, NH3, VOC function
## 
## Purpose: 
## 
## This function creates header description data for the specified
## emission level (unit, plant, state, subregion) to format the final
## .xlsx file for PM, NH3, and VOC emissions. 
##
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

format_headers_pm_nh3_voc <- function(emission_level) {
  
  #' format_headers_pm_nh3_voc
  #' 
  #' Function to create header description data for final formatting script of 
  #' pm2.5, nh3, or voc emissions. Headers are the same across emission types
  #' 
  #' @param emission_level Emission level to produce header data - must be the following:
  #'                       "unit", "plant", "state", "subregion"
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
