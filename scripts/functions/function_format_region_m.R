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


format_region_m <- function(region, reg_rows) {
  
  #' format_region_m
  #' 
  #' Helper function to format region aggregation files (m = metric file)
  #' Needs function header style create 
  #' 
  #' @region Region for data to be aggregated to,
  #' @reg_rows Count of rows for the region dataframe
  #' @return Formatted dataframe with style
  #' @examples format_region(st, st_rows) # Where st is the sheet name and st_rows = number of rows in the final sheet
  
  require(openxlsx)
  
  # if region is not for US, then region uses this style
  
  if(as.character(region) != glue::glue("US{year}")) {
    
    ## add style for first row (description)
    addStyle(wb, sheet = region, style = s[['desc_style']],       rows = 1, cols = 1:4,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color1_desc']],      rows = 1, cols = 5:20,     gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color4_desc']],      rows = 1, cols = 21:36,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color5_desc']],      rows = 1, cols = 37:44,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color6_desc']],      rows = 1, cols = 45:60,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color13_desc']],     rows = 1, cols = 61:120,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color14_desc']],     rows = 1, cols = 121:150,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color15_desc']],     rows = 1, cols = 151:166,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color7_desc']],      rows = 1, cols = 167:188,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color8_desc']],      rows = 1, cols = 189:192,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9_desc']],      rows = 1, cols = 193:194,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9v2_desc']],    rows = 1, cols = 195:198,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color10_desc']],     rows = 1, cols = 199:209,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color11_desc']],     rows = 1, cols = 210:211,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12_desc']],     rows = 1, cols = 212,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12v2_desc']],   rows = 1, cols = 213:214,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color16_desc']],     rows = 1, cols = 215:236,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color17_desc']],     rows = 1, cols = 237:248,  gridExpand = TRUE)
    
    ## add header style
    addStyle(wb, sheet = region, style = s[['header_style']],     rows = 2, cols = 1:4,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color1_header']],    rows = 2, cols = 5:20,     gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color4_header']],    rows = 2, cols = 21:36,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color5_header']],    rows = 2, cols = 37:44,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color6_header']],    rows = 2, cols = 45:60,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color13_header']],   rows = 2, cols = 61:120,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color14_header']],   rows = 2, cols = 121:150,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color15_header']],   rows = 2, cols = 151:166,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color7_header']],    rows = 2, cols = 167:188,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color8_header']],    rows = 2, cols = 189:192,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9_header']],    rows = 2, cols = 193:194,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9v2_header']],  rows = 2, cols = 195:198,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color10_header']],   rows = 2, cols = 199:209,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color11_header']],   rows = 2, cols = 210:211,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12_header']],   rows = 2, cols = 212,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12v2_header']], rows = 2, cols = 213:214,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color16_header']],   rows = 2, cols = 215:236,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color17_header']],   rows = 2, cols = 237:248,  gridExpand = TRUE)
    
    ## set column widths
    setColWidths(wb, sheet = region, cols = 1,       widths = 12)
    setColWidths(wb, sheet = region, cols = 2,       widths = 14)
    setColWidths(wb, sheet = region, cols = 3,       widths = 18.43)
    setColWidths(wb, sheet = region, cols = 4:152,   widths = 14)
    setColWidths(wb, sheet = region, cols = 153,     widths = 15.11)
    setColWidths(wb, sheet = region, cols = 154:163, widths = 14)
    setColWidths(wb, sheet = region, cols = 164,     widths = 15.11)
    
    ## set row heights
    setRowHeights(wb, sheet = region, row = 1, heights = 67.5)
    
    ## add number styles
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 4:18,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 19:58,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 59:66,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 67:88,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 89:96,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 97:110,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 111:126, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 127:142, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 143:153, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 154:164, gridExpand = TRUE)
    
    ## add text styles
    addStyle(wb, sheet = region, style = s[['basic']],    rows = 3:reg_rows, cols = 1:3,     gridExpand = TRUE)
    
    
    ## freeze pane
    freezePane(wb, sheet = region, firstActiveCol = 4)
    
  } else {
    
    # else (for US) 
    
    ## add style for first row (description)
    addStyle(wb, sheet = region, style = s[['desc_style']],       rows = 1, cols = 1:2,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color1_desc']],      rows = 1, cols = 3:18,     gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color4_desc']],      rows = 1, cols = 19:34,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color5_desc']],      rows = 1, cols = 35:42,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color6_desc']],      rows = 1, cols = 43:58,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color13_desc']],     rows = 1, cols = 59:118,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color14_desc']],     rows = 1, cols = 119:148,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color15_desc']],     rows = 1, cols = 149:164,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color7_desc']],      rows = 1, cols = 165:186,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color8_desc']],      rows = 1, cols = 187:190,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9_desc']],      rows = 1, cols = 191:192,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9v2_desc']],    rows = 1, cols = 193:196,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color10_desc']],     rows = 1, cols = 197:207,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color11_desc']],     rows = 1, cols = 208:209,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12_desc']],     rows = 1, cols = 210,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12v2_desc']],   rows = 1, cols = 211:212,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color16_desc']],     rows = 1, cols = 213:234,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color17_desc']],     rows = 1, cols = 235:246,  gridExpand = TRUE)
    
    ## add header style
    addStyle(wb, sheet = region, style = s[['header_style']],     rows = 2, cols = 1:2,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color1_header']],    rows = 2, cols = 3:18,     gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color4_header']],    rows = 2, cols = 19:34,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color5_header']],    rows = 2, cols = 35:42,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color6_header']],    rows = 2, cols = 43:58,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color13_header']],   rows = 2, cols = 59:118,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color14_header']],   rows = 2, cols = 119:148,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color15_header']],   rows = 2, cols = 149:164,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color7_header']],    rows = 2, cols = 165:186,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color8_header']],    rows = 2, cols = 187:190,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9_header']],    rows = 2, cols = 191:192,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color9v2_header']],  rows = 2, cols = 193:196,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color10_header']],   rows = 2, cols = 197:207,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color11_header']],   rows = 2, cols = 208:209,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12_header']],   rows = 2, cols = 210,      gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color12v2_header']], rows = 2, cols = 211:212,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color16_header']],   rows = 2, cols = 213:234,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['color17_header']],   rows = 2, cols = 235:246,  gridExpand = TRUE)
    
    ## set column widths
    setColWidths(wb, sheet = region, cols = 1:4,   widths = 14.14)
    setColWidths(wb, sheet = region, cols = 5:6,   widths = 14.43)
    setColWidths(wb, sheet = region, cols = 7:162, widths = 14.14)
    
    ## set row heights
    setRowHeights(wb, sheet = region, row = 1, heights = 67.5)
    
    ## add number styles
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 2:16,    gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 17:56,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 57:64,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 65:86,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 87:94,   gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 95:108,  gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 109:124, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 125:140, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 141:151, gridExpand = TRUE)
    addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 152:162, gridExpand = TRUE)
    
    ## add text styles
    addStyle(wb, sheet = region, style = s[['basic']],    rows = 3:reg_rows, cols = 1,       gridExpand = TRUE)
    
  }
}
