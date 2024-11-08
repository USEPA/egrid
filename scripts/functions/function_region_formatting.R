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
  addStyle(wb, sheet = srl, style = s[['desc_style']], rows = 1, cols = 1:4, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color1_desc']], rows = 1, cols = 5:18, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color4_desc']], rows = 1, cols = 19:26, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color5_desc']], rows = 1, cols = 27:34, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color6_desc']], rows = 1, cols = 35:42, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color13_desc']], rows = 1, cols = 43:72, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color14_desc']], rows = 1, cols = 73:102, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color15_desc']], rows = 1, cols = 103:110, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color7_desc']], rows = 1, cols = 111:121, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color8_desc']], rows = 1, cols = 122:123, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color9_desc']], rows = 1, cols = 124, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color9v2_desc']], rows = 1, cols = 125:126, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color10_desc']], rows = 1, cols = 127:137, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color11_desc']], rows = 1, cols = 138:139, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color12_desc']], rows = 1, cols = 140, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color12v2_desc']], rows = 1, cols = 141:142, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color16_desc']], rows = 1, cols = 143:153, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color17_desc']], rows = 1, cols = 154:164, gridExpand = TRUE)
  
  
  # write data to sheet
  writeData(wb, 
            sheet = srl, 
            srl_file,
            startRow = 2)
  
  ## add header style
  addStyle(wb, sheet = srl, style = s[['header_style']], rows = 2, cols = 1:4, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color1_header']], rows = 2, cols = 5:18, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color4_header']], rows = 2, cols = 19:26, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color5_header']], rows = 2, cols = 27:34, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color6_header']], rows = 2, cols = 35:42, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color13_header']], rows = 2, cols = 43:72, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color14_header']], rows = 2, cols = 73:102, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color15_header']], rows = 2, cols = 103:110, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color7_header']], rows = 2, cols = 111:121, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color8_header']], rows = 2, cols = 122:123, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color9_header']], rows = 2, cols = 124, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color9v2_header']], rows = 2, cols = 125:126, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color10_header']], rows = 2, cols = 127:137, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color11_header']], rows = 2, cols = 138:139, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color12_header']], rows = 2, cols = 140, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color12v2_header']], rows = 2, cols = 141:142, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color16_header']], rows = 2, cols = 143:153, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['color17_header']], rows = 2, cols = 154:164, gridExpand = TRUE)
  
  ## set column widths
  
  setColWidths(wb, sheet = srl, cols = 1, widths = 12)
  setColWidths(wb, sheet = srl, cols = 2, widths = 14 )
  setColWidths(wb, sheet = srl, cols = 3, widths = 18.43)
  setColWidths(wb, sheet = srl, cols = 4:152, widths = 14)
  setColWidths(wb, sheet = srl, cols = 153, widths = 15.11)
  setColWidths(wb, sheet = srl, cols = 154:163, widths = 14)
  setColWidths(wb, sheet = srl, cols = 164, widths = 15.11)
  
  ## set row heights
  
  setRowHeights(wb, sheet = srl, row = 1, heights = 67.5)
  
  ## add number styles
  
  addStyle(wb, sheet = srl, style = s[['integer2']], rows = 3:srl_rows, cols = 4:18, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['decimal1']], rows = 3:srl_rows, cols = 19:58, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['decimal4']], rows = 3:srl_rows, cols = 59:66, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['decimal1']], rows = 3:srl_rows, cols = 67:88, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['decimal4']], rows = 3:srl_rows, cols = 89:96, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['decimal1']], rows = 3:srl_rows, cols = 97:110, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['integer2']], rows = 3:srl_rows, cols = 111:126, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['percent']], rows = 3:srl_rows, cols = 127:142, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['integer2']], rows = 3:srl_rows, cols = 143:153, gridExpand = TRUE)
  addStyle(wb, sheet = srl, style = s[['percent']], rows = 3:srl_rows, cols = 154:164, gridExpand = TRUE)
  
  ## add text styles
  
  addStyle(wb, sheet = srl, style = s[['basic']], rows = 3:srl_rows, cols = 1:3, gridExpand = TRUE)
  
  
  # freeze pane
  freezePane(wb, sheet = srl, firstActiveCol = 4)

  
  # else (for us) 
  
  
  # add style for first row only
  addStyle(wb, sheet = us, style = s[['desc_style']], rows = 1, cols = 1:2, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color1_desc']], rows = 1, cols = 3:16, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color4_desc']], rows = 1, cols = 17:24, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color5_desc']], rows = 1, cols = 25:32, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color6_desc']], rows = 1, cols = 33:40, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color13_desc']], rows = 1, cols = 41:70, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color14_desc']], rows = 1, cols = 71:100, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color15_desc']], rows = 1, cols = 101:108, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color7_desc']], rows = 1, cols = 109:119, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color8_desc']], rows = 1, cols = 120:121, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color9_desc']], rows = 1, cols = 122, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color9v2_desc']], rows = 1, cols = 123:124, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color10_desc']], rows = 1, cols = 125:135, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color11_desc']], rows = 1, cols = 136:137, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color12_desc']], rows = 1, cols = 138, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color12v2_desc']], rows = 1, cols = 139:140, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color16_desc']], rows = 1, cols = 141:151, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color17_desc']], rows = 1, cols = 152:162, gridExpand = TRUE)
  
  
  ## add header style
  addStyle(wb, sheet = us, style = s[['header_style']], rows = 2, cols = 1:2, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color1_header']], rows = 2, cols = 3:16, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color4_header']], rows = 2, cols = 17:24, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color5_header']], rows = 2, cols = 25:32, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color6_header']], rows = 2, cols = 33:40, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color13_header']], rows = 2, cols = 41:70, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color14_header']], rows = 2, cols = 71:100, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color15_header']], rows = 2, cols = 101:108, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color7_header']], rows = 2, cols = 109:119, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color8_header']], rows = 2, cols = 120:121, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color9_header']], rows = 2, cols = 122, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color9v2_header']], rows = 2, cols = 123:124, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color10_header']], rows = 2, cols = 125:135, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color11_header']], rows = 2, cols = 136:137, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color12_header']], rows = 2, cols = 138, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color12v2_header']], rows = 2, cols = 139:140, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color16_header']], rows = 2, cols = 141:151, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['color17_header']], rows = 2, cols = 152:162, gridExpand = TRUE)
  
  
  ## set row heights
  
  setRowHeights(wb, sheet = us, row = 1, heights = 67.5)
  
  ## add number styles
  
  addStyle(wb, sheet = us, style = s[['integer2']], rows = 3:us_rows, cols = 2:16, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['decimal1']], rows = 3:us_rows, cols = 17:56, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['decimal4']], rows = 3:us_rows, cols = 57:64, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['decimal1']], rows = 3:us_rows, cols = 65:86, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['decimal4']], rows = 3:us_rows, cols = 87:94, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['decimal1']], rows = 3:us_rows, cols = 95:108, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['integer2']], rows = 3:us_rows, cols = 109:124, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['percent']], rows = 3:us_rows, cols = 125:140, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['integer2']], rows = 3:us_rows, cols = 141:151, gridExpand = TRUE)
  addStyle(wb, sheet = us, style = s[['percent']], rows = 3:us_rows, cols = 152:162, gridExpand = TRUE)
  
  ## add text styles
  
  addStyle(wb, sheet = us, style = s[['basic']], rows = 3:us_rows, cols = 1, gridExpand = TRUE)
  
  
}
