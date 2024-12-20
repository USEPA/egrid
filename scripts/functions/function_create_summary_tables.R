## -------------------------------
##
## Create tables for eGRID summary tables.
## 
## Purpose: 
## 
## This file formats summary data as four tables each within its own sheet in 
## final summary tables excel sheet. 
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

create_summary_tables <- function(contents = table_data) {
  
  #' create_summary_tables
  #' 
  #' Function to create and format summary tables for final excel file.
  #' 
  #' @param contents Vector of the datasets containing the data for 
  #'                 the summary tables - includes four formatted datasets.
  #' 
  #' @return Four new excel sheets added to the preexisting workbook each with 
  #'         one of the formatted summary tables
  #' 
  #' @examples 
  #' # Create the four sheets containing the summary tables
  #' create_summary_tables()
  
  # Define select styles -------

  toptitle <- createStyle(
    fontSize = 12,           
    halign = "center", 
    valign = "center", 
    wrapText = TRUE, 
    textDecoration = "bold", 
    border = "TopLeftRight", 
    borderColour = "black", 
    borderStyle = "thick")
  
  titles_gen <- createStyle(
    halign = "center",
    valign = "center",
    wrapText = TRUE, 
    textDecoration = "bold")
  
  all_borders <- createStyle(
    border = "TopBottomLeftRight", 
    borderColour = "black",
    borderStyle = "thin")
  
  right_borders <- createStyle(
    border = "Right", 
    borderColour = "black",
    borderStyle = "thin")  
  
  top_borders <- createStyle(
    border = "Top", 
    borderColour = "black",
    borderStyle = "thin")
  
  grey <- createStyle(fgFill = "#F2F2F2")
  green <- createStyle(fgFill = "#ebf1de")
  purple <- createStyle(fgFill = "#e4dfec")
  red <- createStyle(fgFill = "#f2dcdb")
  orange <- createStyle(fgFill = "#fde9d9")
  
  us_boundary <- createStyle(
    textDecoration = "bold",
    border = "Top",
    borderStyle = "thick",
    borderColour = "black")
  
  # Set sheet dimensions and orientations --------------
  
  startcol <- 2 # column to start data write
  colname_rows <- c(4, 3, 4, 3) # number of rows for titles
  sheet_orientation <- c("landscape", "landscape", "portrait", "portrait") # print orientation
  
  for (sheet in 2:5) {
    
    # Add new worksheet data --------------

    addWorksheet(wb, sheetName = paste0("Table ",sheet))
    
    # define size of data for formatting
    sheetWidth <- as.numeric(ncol(contents[[sheet - 1]])) 
    sheetLength <- nrow(contents[[sheet - 1]]) + colname_rows[sheet - 1] 
    
    # assign printing preferences
    pageSetup(wb, sheet, orientation = sheet_orientation[sheet - 1], fitToWidth = TRUE,
              fitToHeight =  TRUE, left = 0.5, right = 0.5, top = 0.5, bottom = 0.5)
    
    ## Write in sheet data --------
    
    if (sheet %in% c(3, 5)) {
      # add percentages statement for resource mix tables
      writeData(wb, sheet, "*percentages may not sum to 100 due to rounding", 
                startCol = startcol, startRow = sheetLength + 1)
      addStyle(wb, sheet, top_borders, cols = startcol:(sheetWidth + 1), rows = sheetLength + 1)
      # write creation data
      creationRow <- sheetLength + 1
      writeData(wb, sheet, x = matrix(c("Created:", format(Sys.Date(), "%m/%d/%Y")), 
                                      ncol = 2), startCol = sheetWidth, startRow = creationRow)
      # remove matrix titles
      deleteData(wb, sheet, cols = sheetWidth:(sheetWidth + 1), rows = creationRow, gridExpand = TRUE)
    } else {
      # write creation data
      creationRow <- sheetLength
      writeData(wb, sheet, x = matrix(c("Created:", format(Sys.Date(), "%m/%d/%Y")), 
                                      ncol = 2), startCol = sheetWidth, startRow = creationRow)
    }
    
    # add creation data styling
    addStyle(wb, sheet, createStyle(fontSize = 8, halign = "right"), cols = sheetWidth,
             rows = creationRow + 1)
    addStyle(wb, sheet, createStyle(fontSize = 8, halign = "center"), cols = sheetWidth + 1,
             rows = creationRow + 1)
    
    # write eGRID data onto sheet
    writeData(wb, sheet, contents[[sheet - 1]], startCol = startcol, startRow = colname_rows[sheet - 1] + 1, 
              colNames = FALSE)
    
    # Table 1 formatting ----------------------------------
    
    if (sheet == 2) {
      
      # column names in sheet
      colnames_lower <- matrix(c(
        "eGRID subregion acronym",
        "eGRID subregion name",
        paste0("CO", "\u2082"),
        paste0("CH", "\u2084"),
        paste0("N", "\u2082", "O"),
        paste0("CO", "\u2082", "e"),
        paste0("Annual NO", "\u2093"),
        paste0("Ozone Season NO", "\u2093"),
        paste0("SO", "\u2082"),
        paste0("CO", "\u2082"),
        paste0("CH", "\u2084"),
        paste0("N", "\u2082", "O"),
        paste0("CO", "\u2082", "e"),
        paste0("Annual NO", "\u2093"),
        paste0("Ozone Season NO", "\u2093"),
        paste0("SO", "\u2082"),
        "Grid Gross Loss (%)"), ncol = sheetWidth)
      
      # column larger categories
      colnames_upper <- colnames_lower
      colnames_upper[3] <- "Total output emission rates"
      colnames_upper[10] <-  "Non-baseload output emission rates"
      
      # units for larger categories
      colnames_units <- rbind(colnames_upper, 
                              matrix(c("", "", "lb/MWh", "", "", "", "", "", "",
                                       "lb/MWh", "", "", "", "", "", "", ""), 
                                     ncol = sheetWidth))
      
      ## Column title formatting ---------------------------
      
      # write column titles
      writeData(wb, sheet, eval(colnames_lower), startCol = startcol, startRow = colname_rows[sheet - 1] - 1) # column titles
      writeData(wb, sheet, colnames_units, startCol = startcol, startRow = 1) # column units and larger categories
      
      # add borders
      addStyle(wb, sheet, all_borders, rows = c(1, 4:sheetLength), cols = startcol:(sheetWidth + 1),
               stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, right_borders, rows = 2:4, cols = startcol:(sheetWidth + 1), 
               stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, top_borders, rows = 2, cols = startcol:(sheetWidth +1), 
               stack = TRUE, gridExpand = TRUE)
      
      # add shading
      addStyle(wb, sheet, grey, rows = 2:4, cols = c(2:3, 18), stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, green, rows = 2:4, cols = 4:10, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, purple, rows = 2:4, cols = 11:17, stack = TRUE, gridExpand = TRUE)
      
      # merge column title cells
      mergeCells(wb, sheet, cols = 2:18, rows = 1)
      for (colnum in c(2, 3, 18)) {
        mergeCells(wb, sheet, cols = colnum, rows = 2:4)
      }
      
      for (colnum in c(4, 11)) {
        for (rownum in c(2, 3)) {
          mergeCells(wb, sheet, cols = c(colnum, colnum + 6), rows = rownum)
        }
      }
      
      ## Data formatting ------------------------------
      
      # add number formats
      addStyle(wb, sheet, createStyle(numFmt = "#,##0.0"), rows = 5:sheetLength, 
               cols = c(4, 7:9, 11, 14:16), stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, createStyle(numFmt = "#,##0.000"), rows = 5:sheetLength, 
               cols = c(5:6, 10, 12:13, 17), stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, createStyle(numFmt = "#,##0.0%"), rows = 5:sheetLength, 
               cols = 18, stack = TRUE, gridExpand = TRUE)
      
      # merge US cells in resource mix
      mergeCells(wb, sheet, cols = 2:3, rows = sheetLength)
      
      ## General formatting ---------------------------
      
      # set cell sizes
      setRowHeights(wb, sheet, rows = 1:4, heights = c(22, 14.4, 14.4, 35))
      setRowHeights(wb, sheet, rows = 5:(sheetLength + 1), heights = 15)
      setColWidths(wb, sheet, cols = c(1:3, 18), widths = c(1, 10, 22, 8.75))
      setColWidths(wb, sheet, cols = 4:17, widths = 7)
      
      # Table 2 formatting ----------------------
      
    } else if (sheet == 3) {
      
      # column names in sheet
      colnames_lower <- matrix(c(
        "eGRID subregion acronym",
        "eGRID subregion name",
        "Nameplate Capacity (MW)",
        "Net Generation (MWh)",
        "Coal",
        "Oil",
        "Gas",
        "Other Fossil",
        "Nuclear",
        "Hydro",
        "Biomass",
        "Wind",
        "Solar",
        "Geo- thermal",
        "Other unknown/ purchased fuel"),
        ncol = sheetWidth)
      
      # column larger categories
      colnames_upper <- colnames_lower
      colnames_upper[5] <- "Generation Resource Mix (percent)*"
      
      ## Column title formatting ---------------------------
      
      # write column titles
      writeData(wb, sheet, colnames_lower, startCol = startcol, startRow = 2)
      writeData(wb, sheet, colnames_upper, startCol = startcol, startRow = 1)
      
      # add borders
      addStyle(wb, sheet, all_borders, rows = 1:sheetLength , cols = startcol:(sheetWidth + 1),
               stack = TRUE, gridExpand = TRUE)
      
      # add shading
      addStyle(wb, sheet, grey, rows = 2:3, cols = 2:4, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, red, rows = 2:3, cols = 5, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, orange, rows = 2:3, cols = 6:16, stack = TRUE, gridExpand = TRUE)
      
      # merge column title cells
      mergeCells(wb, sheet, cols = 2:16, rows = 1)
      mergeCells(wb, sheet, cols = 6:16, rows = 2)
      for (colnum in 2:5) {
        mergeCells(wb, sheet, cols = colnum, rows = 2:3)
      }
      
      ## Data formatting ------------------
      
      # add number formats
      addStyle(wb, sheet, createStyle(numFmt = "#,##0"), rows = 4:sheetLength, 
               cols = 4:5, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, createStyle(numFmt = "#,##0.0%"), rows = 4:sheetLength, 
               cols = 6:16, stack = TRUE, gridExpand = TRUE)
      
      # merge US cells in subregion sheets
      mergeCells(wb, sheet, cols = 2:3, rows = sheetLength)
      
      ## General formatting -----------------
      
      # set cell sizes
      setRowHeights(wb, sheet, rows = 1:3, heights = c(22, 15, 45))
      setRowHeights(wb, sheet, rows = 4:(sheetLength + 2), heights = 15)
      setColWidths(wb, sheet, cols = c(1:5, 12, 16), 
                   widths = c(1, 10, 22, 10, 12, 8.2, 10))
      setColWidths(wb, sheet, cols = c(6:11, 13:15), widths = 8)
      
      # Table 3 formatting -----------
      
    } else if (sheet == 4) {
      
      # column names in sheet
      colnames_lower <- matrix(c(
        "State",
        paste0("CO", "\u2082"),
        paste0("CH", "\u2084"),
        paste0("N", "\u2082", "O"),
        paste0("CO", "\u2082", "e"),
        paste0("Annual NO", "\u2093"),
        paste0("Ozone Season NO", "\u2093"),
        paste0("SO", "\u2082")),
        ncol = sheetWidth)
      
      #column larger categories
      colnames_upper <- colnames_lower
      colnames_upper[2] <- "Total output emission rates"
      
      # units for larger categories
      colnames_units <- rbind(colnames_upper, 
                              matrix(c("", "lb/MWh", "", "", "", "", "", ""), 
                                     ncol = sheetWidth))
      
      ## Column title formatting -----------------
      
      # write column titles
      writeData(wb, sheet, colnames_lower, startCol = startcol, startRow = 3)
      writeData(wb, sheet, colnames_units, startCol = startcol, startRow = 1)
      
      # add borders
      addStyle(wb, sheet, all_borders, rows = c(1, 4:sheetLength), cols = 2:(sheetWidth + 1), 
               stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, right_borders, rows = 2:4, cols = 2:(sheetWidth + 1), 
               stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, top_borders, rows = 2, cols = 2:(sheetWidth + 1), 
               stack = TRUE, gridExpand = TRUE)
      
      # add shading
      addStyle(wb, sheet, grey, rows = 2:4, cols = 2, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, green, rows = 2:4, cols = 3:9, stack = TRUE, gridExpand = TRUE)
      
      # merge column title cells
      mergeCells(wb, sheet, cols = 2:9, rows = 1)
      mergeCells(wb, sheet, cols = 2, rows = 2:4)
      for (rownum in c(2,3)) {
        mergeCells(wb, sheet, cols = 3:9, rows = rownum)
      }
      
      ## Data formatting ----------------
      
      # add number formats
      addStyle(wb, sheet, createStyle(numFmt = "#,##0.0"), rows = 5:sheetLength, cols = c(3, 6:8),
               stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, createStyle(numFmt = "#,##0.000"), rows = 5:sheetLength, cols = c(4:5, 9),
               stack = TRUE, gridExpand = TRUE)
      
      ## General formatting ----------------
      
      # set cell sizes
      setRowHeights(wb, sheet, rows = 1:4, heights = c(22, 11.25, 11.25, 22))
      setRowHeights(wb, sheet, rows = 5:(sheetLength + 1), heights = 15)
      setColWidths(wb, sheet, cols = 1:2, width = c(1, 10))
      setColWidths(wb, sheet, cols = 3:9, widths = 15)
      
      ##Table 4 formatting ------------------
      
    } else if (sheet == 5) {
      
      # column names in sheet
      colnames_lower <- matrix(c(
        "State",
        "Nameplate Capacity (MW)",
        "Net Generation (MWh)",
        "Coal",
        "Oil",
        "Gas",
        "Other Fossil",
        "Nuclear",
        "Hydro",
        "Biomass",
        "Wind",
        "Solar",
        "Geo- thermal",
        "Other unknown/ purchased fuel"),
        ncol = sheetWidth)
      
      #larger column categories
      colnames_upper <- colnames_lower
      colnames_upper[4] <- "Generation Resource Mix (percent)*"
      
      ## Column title and formatting -----------------
      
      # write column titles
      writeData(wb, sheet, colnames_lower, startCol = startcol, startRow = 2)
      writeData(wb, sheet, colnames_upper, startCol = startcol, startRow = 1)
      
      #add borders
      addStyle(wb, sheet, all_borders, rows = 1:sheetLength, cols = 2:(sheetWidth + 1),
               stack = TRUE, gridExpand = TRUE)
      
      # add shading
      addStyle(wb, sheet, grey, rows = 2:3, cols = 2:3, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, red, rows = 2:3, cols = 4, stack = TRUE,  gridExpand = TRUE)
      addStyle(wb, sheet, orange, rows = 2:3, cols = 5:15, stack = TRUE, gridExpand = TRUE)
      
      # merge column title cells
      mergeCells(wb, sheet, cols = 2:15, rows = 1)
      mergeCells(wb, sheet, cols = 5:15, rows = 2)
      for (colnum in 2:4) {
        mergeCells(wb, sheet, cols = colnum, rows = 2:3)
      }
      
      ## Data formatting --------------
      
      # add number formats
      addStyle(wb, sheet, createStyle(numFmt = "#,##0"), rows = 4:sheetLength,
               cols = 3:4, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, sheet, createStyle(numFmt = "#,##0.0%"), rows = 4:sheetLength, 
               cols = 5:15, stack = TRUE, gridExpand = TRUE)
      
      ## General formatting --------------------
      # set cell sizes
      setRowHeights(wb, sheet, rows = 1:3, heights = c(22, 15, 45))
      setRowHeights(wb, sheet, rows = 4:(sheetLength + 2), heights = 15)
      setColWidths(wb, sheet, cols = c(1:4, 9, 11, 14:15), 
                   width = c(1, 7.5, 10, 12, 8, 9, 8.5, 11))
      setColWidths(wb, sheet, cols = c(5:8, 10, 12:13), widths = 7)
    }  
    
    # All tables formatting ------
    
    # add main titles and format
    writeData(wb, sheet, names(contents[sheet - 1]), startCol = startcol, startRow = 1)
    addStyle(wb, sheet, toptitle, rows = 1, cols = 1:sheetWidth + 1, stack = TRUE, gridExpand = TRUE)
    
    # add column titles formatting
    addStyle(wb, sheet, titles_gen, rows = 2:colname_rows[sheet - 1], cols = 2:(sheetWidth + 1), stack = TRUE, gridExpand = TRUE)
    
    # format US row with thick outline and bold font
    addStyle(wb, sheet, us_boundary, rows = sheetLength, cols = 2:(sheetWidth + 1), stack = TRUE, gridExpand = TRUE)
    
    # add exterior thick borders
    addStyle(wb, sheet, createStyle(border = "Right", borderColour = "black", borderStyle = "thick"),
             rows = 1:creationRow, cols = sheetWidth + 1, stack = TRUE, gridExpand = TRUE)
    addStyle(wb, sheet, createStyle(border = "Left", borderColour = "black", borderStyle = "thick"),
             rows = 1:creationRow, cols = startcol, stack = TRUE, gridExpand = TRUE)
    addStyle(wb, sheet, createStyle(border = "Bottom", borderColour = "black", borderStyle = "thick"),
             rows = creationRow, cols = 2:(sheetWidth + 1), stack = TRUE, gridExpand = TRUE)
  }
}