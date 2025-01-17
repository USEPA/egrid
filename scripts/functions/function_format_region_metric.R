## -------------------------------
##
## Function format region files (m = metric) 
## 
## Purpose: 
## 
## This file formats the regional aggregation files for metric_file_format.R
## This includes all operating units for the specified eGRID data year
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------


format_region_metric <- function(region, reg_rows) {
  
  #' format_region_m
  #' 
  #' Helper function to format region aggregation files (m = metric file)
  #' Needs function header style create 
  #' 
  #' @region Region for data to be aggregated to,
  #' @reg_rows Count of rows for the region dataframe
  #' @return Formatted dataframe with style
  #' 
  #' @examples format_region(st, st_rows) # Where st is the sheet name and st_rows = number of rows in the final sheet
  
  require(openxlsx)
  
  # if region is not for US, then region uses this style
  
  if(as.character(region) != glue::glue("US{year}")) {
    
    ## add style for first row (description)
    addStyle(wb, sheet = region, style = s[['desc_style']],       rows = 1, cols = 1:4,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color1_desc']],      rows = 1, cols = 5:22,     gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color4_desc']],      rows = 1, cols = 23:38,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color5_desc']],      rows = 1, cols = 39:46,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color6_desc']],      rows = 1, cols = 47:62,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color13_desc']],     rows = 1, cols = 63:122,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color14_desc']],     rows = 1, cols = 123:152,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color15_desc']],     rows = 1, cols = 153:168,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color7_desc']],      rows = 1, cols = 169:190,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color8_desc']],      rows = 1, cols = 191:196,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9_desc']],      rows = 1, cols = 197:198,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9v2_desc']],    rows = 1, cols = 199:204,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color10_desc']],     rows = 1, cols = 205:215,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color11_desc']],     rows = 1, cols = 216:218,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12_desc']],     rows = 1, cols = 219,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12v2_desc']],   rows = 1, cols = 220:222,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color16_desc']],     rows = 1, cols = 223:244,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color17_desc']],     rows = 1, cols = 245:255,  gridExpand = TRUE)
    
    ## add header style
    addStyle(wb, sheet = region, style = s[['header_style']],     rows = 2, cols = 1:4,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color1_header']],    rows = 2, cols = 5:22,     gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color4_header']],    rows = 2, cols = 23:38,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color5_header']],    rows = 2, cols = 39:46,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color6_header']],    rows = 2, cols = 47:62,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color13_header']],   rows = 2, cols = 63:122,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color14_header']],   rows = 2, cols = 123:152,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color15_header']],   rows = 2, cols = 153:168,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color7_header']],    rows = 2, cols = 169:190,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color8_header']],    rows = 2, cols = 191:196,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9_header']],    rows = 2, cols = 197:198,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9v2_header']],  rows = 2, cols = 199:204,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color10_header']],   rows = 2, cols = 205:215,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color11_header']],   rows = 2, cols = 216:218,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12_header']],   rows = 2, cols = 219,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12v2_header']], rows = 2, cols = 220:222,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color16_header']],   rows = 2, cols = 223:244,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color17_header']],   rows = 2, cols = 245:255,  gridExpand = TRUE)
    
    ## set column widths
    setColWidths(wb, sheet = region, cols = 1,       widths = 12)
    setColWidths(wb, sheet = region, cols = 2,       widths = 14)
    setColWidths(wb, sheet = region, cols = 3,       widths = 18.43)
    setColWidths(wb, sheet = region, cols = 4:255,   widths = 15)
    
    ## set row heights
    setRowHeights(wb, sheet = region, row = 1, heights = 67.5)
    
    ## add number styles
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 4:22,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 23:94,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 95:110,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 111:139, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 140:149, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 150:168, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 169:204, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 205:222, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 223:244, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 245:255, gridExpand = TRUE)
    
    ## add text styles
    addStyle(wb, sheet = region, style = s[['basic']],    rows = 3:reg_rows, cols = 1:3,     gridExpand = TRUE)
    
    ## freeze pane
    freezePane(wb, sheet = region, firstActiveCol = 4, firstActiveRow = 3)
    
  } else {
    
    # else (for US) 
    
    ## add style for first row (description)
    addStyle(wb, sheet = region, style = s[['desc_style']],       rows = 1, cols = 1:2,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color1_desc']],      rows = 1, cols = 3:20,     gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color4_desc']],      rows = 1, cols = 21:36,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color5_desc']],      rows = 1, cols = 37:44,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color6_desc']],      rows = 1, cols = 45:60,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color13_desc']],     rows = 1, cols = 61:120,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color14_desc']],     rows = 1, cols = 121:150,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color15_desc']],     rows = 1, cols = 151:166,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color7_desc']],      rows = 1, cols = 167:188,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color8_desc']],      rows = 1, cols = 189:194,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9_desc']],      rows = 1, cols = 195:196,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9v2_desc']],    rows = 1, cols = 197:202,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color10_desc']],     rows = 1, cols = 203:213,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color11_desc']],     rows = 1, cols = 214:216,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12_desc']],     rows = 1, cols = 217,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12v2_desc']],   rows = 1, cols = 218:220,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color16_desc']],     rows = 1, cols = 221:242,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color17_desc']],     rows = 1, cols = 243:253,  gridExpand = TRUE)
    
    ## add header style
    addStyle(wb, sheet = region, style = s[['header_style']],     rows = 2, cols = 1:2,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color1_header']],    rows = 2, cols = 3:20,     gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color4_header']],    rows = 2, cols = 21:36,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color5_header']],    rows = 2, cols = 37:44,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color6_header']],    rows = 2, cols = 45:60,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color13_header']],   rows = 2, cols = 61:120,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color14_header']],   rows = 2, cols = 121:150,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color15_header']],   rows = 2, cols = 151:166,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color7_header']],    rows = 2, cols = 167:188,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color8_header']],    rows = 2, cols = 189:194,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9_header']],    rows = 2, cols = 195:196,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9v2_header']],  rows = 2, cols = 197:202,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color10_header']],   rows = 2, cols = 203:213,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color11_header']],   rows = 2, cols = 214:216,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12_header']],   rows = 2, cols = 217,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12v2_header']], rows = 2, cols = 218:220,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color16_header']],   rows = 2, cols = 221:242,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color17_header']],   rows = 2, cols = 243:253,  gridExpand = TRUE)
    
    ## set column widths
    setColWidths(wb, sheet = region, cols = 1:4,   widths = 14.14)
    setColWidths(wb, sheet = region, cols = 5:6,   widths = 14.43)
    setColWidths(wb, sheet = region, cols = 7:253, widths = 15)
    
    ## set row heights
    setRowHeights(wb, sheet = region, row = 1, heights = 67.5)
    
    ## add number styles
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 2:20,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 21:92,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 93:108,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 109:137, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 138:147, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 148:166, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 167:202, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 203:220, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 221:242, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 243:253, gridExpand = TRUE)
    
    ## add text styles
    addStyle(wb, sheet = region, style = s[['basic']],    rows = 3:reg_rows, cols = 1,       gridExpand = TRUE)
    
  }
}
