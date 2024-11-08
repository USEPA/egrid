## -------------------------------
##
## Function format region files
## 
## Purpose: 
## 
## This file formats the regional aggregation files for final_formatting.R
## This includes all operating units for the specified eGRID data year
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------


format_region_files <- function(region, region_rows) {
  
  #' region_formatting
  #' 
  #' Helper function to format region aggregation files. 
  #' Needs function header style create 
  #' 
  #' @region Region for data to be aggregated to, as a string
  #' @return Formatted dataframe with style
  #' @examples
  #' region_aggregation("state", c(fips_state_code, state)) # Aggregates the plant file to the state regional level
  
  require(openxlsx)
  
  # if region is not = 'us' then use this style

  # add style for first row only
  addStyle(wb, sheet = region, style = desc_style, rows = 1, cols = 1:4, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color1_desc, rows = 1, cols = 5:18, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color4_desc, rows = 1, cols = 19:26, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color5_desc, rows = 1, cols = 27:34, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color6_desc, rows = 1, cols = 35:42, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color13_desc, rows = 1, cols = 43:72, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color14_desc, rows = 1, cols = 73:102, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color15_desc, rows = 1, cols = 103:110, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color7_desc, rows = 1, cols = 111:121, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color8_desc, rows = 1, cols = 122:123, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color9_desc, rows = 1, cols = 124, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color9v2_desc, rows = 1, cols = 125:126, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color10_desc, rows = 1, cols = 127:137, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color11_desc, rows = 1, cols = 138:139, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color12_desc, rows = 1, cols = 140, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color12v2_desc, rows = 1, cols = 141:142, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color16_desc, rows = 1, cols = 143:153, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color17_desc, rows = 1, cols = 154:164, gridExpand = TRUE)
  
  ## add header style
  addStyle(wb, sheet = region, style = header_style, rows = 2, cols = 1:4, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color1_header, rows = 2, cols = 5:18, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color4_header, rows = 2, cols = 19:26, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color5_header, rows = 2, cols = 27:34, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color6_header, rows = 2, cols = 35:42, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color13_header, rows = 2, cols = 43:72, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color14_header, rows = 2, cols = 73:102, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color15_header, rows = 2, cols = 103:110, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color7_header, rows = 2, cols = 111:121, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color8_header, rows = 2, cols = 122:123, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color9_header, rows = 2, cols = 124, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color9v2_header, rows = 2, cols = 125:126, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color10_header, rows = 2, cols = 127:137, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color11_header, rows = 2, cols = 138:139, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color12_header, rows = 2, cols = 140, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color12v2_header, rows = 2, cols = 141:142, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color16_header, rows = 2, cols = 143:153, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = color17_header, rows = 2, cols = 154:164, gridExpand = TRUE)
  
  ## set row heights
  
  setRowHeights(wb, sheet = region, row = 1, heights = 67.5)
  
  ## add number styles
  
  addStyle(wb, sheet = region, style = integer2, rows = 3:st_rows, cols = 4:18, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = decimal1, rows = 3:st_rows, cols = 19:58, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = decimal4, rows = 3:st_rows, cols = 59:66, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = decimal1, rows = 3:st_rows, cols = 67:88, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = decimal4, rows = 3:st_rows, cols = 89:96, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = decimal1, rows = 3:st_rows, cols = 97:110, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = integer2, rows = 3:st_rows, cols = 111:126, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = percent, rows = 3:st_rows, cols = 127:142, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = integer2, rows = 3:st_rows, cols = 143:153, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = percent, rows = 3:st_rows, cols = 154:164, gridExpand = TRUE)
  
  ## add text styles
  
  addStyle(wb, sheet = region, style = basic, rows = 3:st_rows, cols = 1:3, gridExpand = TRUE)
  
  # freeze panes
  freezePane(wb, sheet = region, firstActiveCol = 4)
  
  # else (for us) 
  
}
