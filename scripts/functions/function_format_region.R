## -------------------------------
##
## Function format region files
## 
## Purpose: 
## 
## This file formats the regional aggregation files for final_formatting.R
## This includes all operating units for the specified eGRID data year
##
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------


format_region <- function(region, reg_rows) {
  
  #' format_region
  #' 
  #' Helper function to format region aggregation files. 
  #' Needs function header style create 
  #' 
  #' @param region Region for data to be aggregated to,
  #' @param reg_rows Count of rows for the region dataframe
  #' @return Formatted dataframe with style
  #' @examples 
  #' format_region(st, st_rows) # Where st is the sheet name and st_rows = number of rows in the final sheet
  
  require(openxlsx)
  
  # if region is not for US, then region uses this style
  
  if(as.character(region) != glue::glue("US{year}")) {
    
  ## add style for first row (description)
  addStyle(wb, sheet = region, style = s[['base_desc']],        rows = 1, cols = 1:4,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color1_desc']],      rows = 1, cols = 5:19,     gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color4_desc']],      rows = 1, cols = 20:27,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color5_desc']],      rows = 1, cols = 28:35,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color6_desc']],      rows = 1, cols = 36:43,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color13_desc']],     rows = 1, cols = 44:73,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color14_desc']],     rows = 1, cols = 74:103,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color15_desc']],     rows = 1, cols = 104:111,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color7_desc']],      rows = 1, cols = 112:122,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color8_desc']],      rows = 1, cols = 123:125,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color9_desc']],      rows = 1, cols = 126,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color9v2_desc']],    rows = 1, cols = 127:129,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color10_desc']],     rows = 1, cols = 130:140,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color11_desc']],     rows = 1, cols = 141:143,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color12_desc']],     rows = 1, cols = 144,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color12v2_desc']],   rows = 1, cols = 145:147,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color16_desc']],     rows = 1, cols = 148:158,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color17_desc']],     rows = 1, cols = 159:169,  gridExpand = TRUE)
  
  ## add header style
  addStyle(wb, sheet = region, style = s[['base_header']],      rows = 2, cols = 1:4,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color1_header']],    rows = 2, cols = 5:19,     gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color4_header']],    rows = 2, cols = 20:27,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color5_header']],    rows = 2, cols = 28:35,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color6_header']],    rows = 2, cols = 36:43,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color13_header']],   rows = 2, cols = 44:73,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color14_header']],   rows = 2, cols = 74:103,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color15_header']],   rows = 2, cols = 104:111,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color7_header']],    rows = 2, cols = 112:122,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color8_header']],    rows = 2, cols = 123:125,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color9_header']],    rows = 2, cols = 126,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color9v2_header']],  rows = 2, cols = 127:129,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color10_header']],   rows = 2, cols = 130:140,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color11_header']],   rows = 2, cols = 141:143,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color12_header']],   rows = 2, cols = 144,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color12v2_header']], rows = 2, cols = 145:147,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color16_header']],   rows = 2, cols = 148:158,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color17_header']],   rows = 2, cols = 159:169,  gridExpand = TRUE)
  
  ## set column widths
  setColWidths(wb, sheet = region, cols = 1,       widths = 12)
  setColWidths(wb, sheet = region, cols = 2,       widths = 14)
  setColWidths(wb, sheet = region, cols = 3,       widths = 18.43)
  setColWidths(wb, sheet = region, cols = 4:124,   widths = 14)
  setColWidths(wb, sheet = region, cols = 125,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 126:128, widths = 14)
  setColWidths(wb, sheet = region, cols = 129,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 130:142, widths = 14)
  setColWidths(wb, sheet = region, cols = 143,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 144:146, widths = 14)
  setColWidths(wb, sheet = region, cols = 147,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 149:152, widths = 14)
  setColWidths(wb, sheet = region, cols = 153,     widths = 15.11)
  setColWidths(wb, sheet = region, cols = 154:163, widths = 14)
  setColWidths(wb, sheet = region, cols = 164,     widths = 15.11)
  setColWidths(wb, sheet = region, cols = 165:169, widths = 14)

  
  ## set row heights
  setRowHeights(wb, sheet = region, row = 1, heights = 67.5)
  
  ## add number styles
  addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 4:19,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 20:59,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 60:67,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 68:89,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 90:97,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 98:111,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 112:129, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 130:147, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 148:158, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 159:169, gridExpand = TRUE)
  
  ## add text styles
  addStyle(wb, sheet = region, style = s[['basic']],    rows = 3:reg_rows, cols = 1:3,     gridExpand = TRUE)
  
  ## freeze pane
  freezePane(wb, sheet = region, firstActiveCol = 4, firstActiveRow = 3)
  
  } else {
  
  # else (for US) 
  
  ## add style for first row (description)
  addStyle(wb, sheet = region, style = s[['desc_style']],       rows = 1, cols = 1:2,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color1_desc']],      rows = 1, cols = 3:17,     gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color4_desc']],      rows = 1, cols = 18:25,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color5_desc']],      rows = 1, cols = 26:33,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color6_desc']],      rows = 1, cols = 34:41,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color13_desc']],     rows = 1, cols = 42:71,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color14_desc']],     rows = 1, cols = 72:101,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color15_desc']],     rows = 1, cols = 102:109,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color7_desc']],      rows = 1, cols = 110:120,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color8_desc']],      rows = 1, cols = 121:123,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color9_desc']],      rows = 1, cols = 124,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color9v2_desc']],    rows = 1, cols = 125:127,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color10_desc']],     rows = 1, cols = 128:138,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color11_desc']],     rows = 1, cols = 139:141,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color12_desc']],     rows = 1, cols = 142,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color12v2_desc']],   rows = 1, cols = 143:145,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color16_desc']],     rows = 1, cols = 146:156,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color17_desc']],     rows = 1, cols = 157:167,  gridExpand = TRUE)
  
  ## add header style
  addStyle(wb, sheet = region, style = s[['header_style']],     rows = 2, cols = 1:2,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color1_header']],    rows = 2, cols = 3:17,     gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color4_header']],    rows = 2, cols = 18:25,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color5_header']],    rows = 2, cols = 26:33,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color6_header']],    rows = 2, cols = 34:41,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color13_header']],   rows = 2, cols = 42:71,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color14_header']],   rows = 2, cols = 72:101,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color15_header']],   rows = 2, cols = 102:109,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color7_header']],    rows = 2, cols = 110:120,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color8_header']],    rows = 2, cols = 121:123,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color9_header']],    rows = 2, cols = 124,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color9v2_header']],  rows = 2, cols = 125:127,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color10_header']],   rows = 2, cols = 128:138,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color11_header']],   rows = 2, cols = 139:141,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color12_header']],   rows = 2, cols = 142,      gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color12v2_header']], rows = 2, cols = 143:145,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color16_header']],   rows = 2, cols = 146:156,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['color17_header']],   rows = 2, cols = 157:167,  gridExpand = TRUE)
  
  ## set column widths
  setColWidths(wb, sheet = region, cols = 1:4,     widths = 14.14)
  setColWidths(wb, sheet = region, cols = 5:6,     widths = 14.43)
  setColWidths(wb, sheet = region, cols = 7:124,   widths = 14.14)
  setColWidths(wb, sheet = region, cols = 123,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 124:126, widths = 14.14)
  setColWidths(wb, sheet = region, cols = 127,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 128:140, widths = 14.14)
  setColWidths(wb, sheet = region, cols = 141,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 142:144, widths = 14.14)
  setColWidths(wb, sheet = region, cols = 145,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 146:165, widths = 14.14)
  
  ## set row heights
  setRowHeights(wb, sheet = region, row = 1, heights = 67.5)
  
  ## add number styles
  addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 2:16,    gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 18:57,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 58:65,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 66:87,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal4']], rows = 3:reg_rows, cols = 88:95,   gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['decimal1']], rows = 3:reg_rows, cols = 96:109,  gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 110:127, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 128:145, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['integer2']], rows = 3:reg_rows, cols = 146:156, gridExpand = TRUE)
  addStyle(wb, sheet = region, style = s[['percent']],  rows = 3:reg_rows, cols = 157:167, gridExpand = TRUE)
  
  ## add text styles
  addStyle(wb, sheet = region, style = s[['basic']],    rows = 3:reg_rows, cols = 1,       gridExpand = TRUE)
  
  }
}



format_cols <- function(df, sheet, style_map, text_style_map, start_col = 0) {
  
  # text_style_map <- c("base"    = "basic",
  #                     "color1"  = "integer2",
  #                     "color4"  = "decimal1",
  #                     "color5"  = "decimal1",
  #                     "color15" = "decimal1")
  
  # format names 
  for (colname in names(df)) {
    if (colname %in% names(style_map)) {
      col_index <- which(names(df) == colname) + start_col
      
      color_index <- style_map[[colname]]
      
      #  add header style
      style_header_name <- paste0(color_index,"_header")
      # print(style_header_name)
      addStyle(
        wb, 
        sheet = sheet, 
        style = s[[style_header_name]], 
        cols = col_index, 
        rows = 2, 
        gridExpand = TRUE
      )
      
      # add description style
      style_desc_name <- paste0(color_index,"_desc")
      # print(style_header_name)
      addStyle(
        wb, 
        sheet = sheet, 
        style = s[[style_desc_name]], 
        cols = col_index, 
        rows = 1, 
        gridExpand = TRUE
      )
      
      # add text style
      text_style_name <- text_style_map[[colname]]
      addStyle(
        wb, 
        sheet = sheet, 
        style = s[[text_style_name]], 
        cols = col_index, 
        rows = 3:(nrow(df)+2), 
        gridExpand = TRUE
      )
      
      col_width <- if_else(grepl("ANNUAL$", colname), 16.5, 13.14)
      
      # set row heights
      setRowHeights(wb, 
                    sheet = sheet, 
                    row = 1, 
                    heights = 67.5)
      
      # set column widths
      setColWidths(wb, 
                   sheet = sheet, 
                   cols = col_index,     
                   widths = col_width)
      
    }
  }
}

# function to modify style_maps in order to match monthly 
modify_style_name <- function(name_map, old_name, new_name) {
  names(name_map)[names(name_map) == old_name] <- new_name
  return(name_map)
}


# function for formatting a sheet
format_sheet <- function(df_month = "", df_ann, file_name, temporal_res, default_style_map, text_style_map = c("base"    = "basic",
                                                                                                              "color1"  = "integer2",
                                                                                                              "color4"  = "decimal1",
                                                                                                              "color5"  = "decimal1",
                                                                                                              "color15" = "decimal1")) {
  
  # get sheet name from file_name
  if (file_name %in% c("SR", "NR")) {
    sheet <- paste0(file_name,"L",year)
  } else {
    sheet <- paste0(file_name, year)
  }
  
  # style map for all base columns
  base_style_map <- c("YEAR" = "base",
                      "PSTATABB" = "base",
                      "FIPSST" = "base",
                      "BANAME" = "base",
                      "BACODE" = "base",
                      "SUBRGN" = "base",
                      "SRNAME" = "base",
                      "NERC" = "base",
                      "NERCNAME" = "base",
                      "PSTATABB" = "base",
                      "FIPSST" = "base")

  ### Monthly Formatting ###
  if (temporal_res == "monthly") {
    
    # replacement vector for annual columns
    header_ann_replace <- c("^HTIT$" = "HTIANT",
                            "^NGEN$" = "NGENAN",
                            "^NOX$"  = "NOXAN",
                            "^SO2$"  = "SO2AN",
                            "^CO2$"  = "CO2AN",
                            "^CH4$"  = "CH4AN",
                            "^N2O$"  = "N2OAN",
                            "^HG$"   = "HGAN",
                            "^NOXCRTA$" = "NOXCRT")
    
    # uppercase month abbreviations
    month_abbr_upper <- toupper(month.abb)
    
    # format monthly columns
    for (month in month_abbr_upper){
      style_map <- default_style_map # set up style map
      
      for (i in 1:length(style_map)) { 
        old_name <- names(default_style_map)[i]
        new_name <- paste0(file_name, old_name, "_", month)
        style_map <- modify_style_name(style_map, old_name, new_name) # update names from base style map to monthly ver
      }
      
      format_cols(df_month, sheet, style_map, text_style_map) # format
    }
    
    # format annual columns
    if (!missing(df_ann)) {
      style_map_ann <- default_style_map
      for (i in 1:length(style_map)){
        old_name <- names(default_style_map)[i]
        
        # name cleaning 
        new_name1 <- gsub("RT", "RTA", old_name)
        new_name2 <- ifelse(endsWith(new_name1, "R"), gsub("R", "RA", new_name1), new_name1)
        new_name3 <- str_replace_all(new_name2, header_ann_replace)
        new_name_final <- paste0(file_name, new_name3, "_", "ANNUAL")
        
        style_map_ann <- modify_style_name(style_map_ann, old_name, new_name_final)
      }
      
      format_cols(df_ann, sheet, style_map_ann, text_style_map, start_col = length(df_month))
    }

    # format base identifier columns
    format_cols(df_month, sheet, base_style_map, text_style_map)
    

  } else if (temporal_res == "annual") {
    format_cols(df_ann, sheet, default_style_map, text_style_map)
    
    style_map <- default_style_map # set up style map
    
    # region formatting
    if (file_name %in% c("ST","BA","SR","NR","US")) {
      for (i in 1:length(style_map)) { 
        old_name <- names(default_style_map)[i]
        new_name <- paste0(file_name, old_name)
        style_map <- modify_style_name(style_map, old_name, new_name) # update names from base style map to monthly ver
      }
      
      format_cols(df_ann, sheet, style_map, text_style_map)
      # format base identifier columns
      format_cols(df_ann, sheet, base_style_map, text_style_map)
    
    # all other sheets formatting
    } else {
      format_cols(df_ann, sheet, default_style_map, text_style_map)
    }
    
  
  }
  
}
