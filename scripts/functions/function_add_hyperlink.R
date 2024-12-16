## -------------------------------
##
## Function add hyperlink
## 
## Purpose: 
## 
## This file add hyperlinks for final_formatting.R
##
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------

add_hyperlink <- function(sheet_name, row_link, col_link, loc, text_to_show) { 
  
  #' add_hyperlink
  #' 
  #' Helper function to format region aggregation files. 
  #' Needs function header style create 
  #' 
  #' @param sheet_name Name of the sheet to hyperlink to 
  #' @param row_link Row location to hyperlink to
  #' @param col_link Column location to hyperlink to
  #' @param loc Location of hyperlink in sheet
  #' @param text_to_show Text to display for hyperlink
  #' @return Hyperlinked text in sheet 
  
  require(openxlsx)
  
  writeFormula(
    wb, sheet = "Contents", 
    x = makeHyperlinkString(sheet = sheet_name, row = row_link, col = col_link, text = text_to_show), 
    xy = loc)
  
}
