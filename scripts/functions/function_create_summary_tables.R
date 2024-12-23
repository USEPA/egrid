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
  #'         one of the formatted summary tables.
  #' 
  #' @examples 
  #' # Create the four sheets containing the summary tables from the datasets
  #' # listed in table_data.
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
  
  for (table_num in 1:4) {
    
    # Add new worksheet data --------------
    
    current_worksheet <- paste0("Table ",table_num)
    addWorksheet(wb, sheetName = current_worksheet)
    
    # define size of data for formatting
    sheetWidth <- as.numeric(ncol(contents[[table_num]])) 
    sheetLength <- nrow(contents[[table_num]]) + colname_rows[table_num] 
    
    # assign font size for data regions
    addStyle(wb, current_worksheet, createStyle(fontSize = 8.5), 
             rows = 2:(sheetLength + 2), cols = startcol:(sheetWidth + 1), stack = TRUE, gridExpand = TRUE)
    
    # assign printing preferences
    pageSetup(wb, current_worksheet, orientation = sheet_orientation[table_num], 
              fitToWidth = TRUE, fitToHeight =  TRUE, left = 0.5, right = 0.5, 
              top = 0.5, bottom = 0.5)
    
    ## Write in sheet data --------
    
    if (table_num %in% c(2, 4)) {
      # add percentages statement for resource mix tables
      writeData(wb, current_worksheet, "*percentages may not sum to 100 due to rounding", 
                startCol = startcol, startRow = sheetLength + 1)
      addStyle(wb, current_worksheet, top_borders, cols = startcol:(sheetWidth + 1), 
               rows = sheetLength + 1, stack = TRUE)
      # write created datestamp
      creationRow <- sheetLength + 1
      writeData(wb, current_worksheet, x = matrix(c("Created:", format(Sys.Date(), "%m/%d/%Y")), 
                                      ncol = 2), startCol = sheetWidth, startRow = creationRow)
      # remove matrix titles
      deleteData(wb, current_worksheet, cols = sheetWidth:(sheetWidth + 1), rows = creationRow, gridExpand = TRUE)
    } else {
      # write created datestamp
      creationRow <- sheetLength
      writeData(wb, current_worksheet, x = matrix(c("Created:", format(Sys.Date(), "%m/%d/%Y")), 
                                      ncol = 2), startCol = sheetWidth, startRow = creationRow)
    }
    
    # add creation data styling
    addStyle(wb, current_worksheet, createStyle(fontSize = 8, halign = "right"), 
             cols = sheetWidth, rows = creationRow + 1)
    addStyle(wb, current_worksheet, createStyle(fontSize = 8, halign = "center"), 
             cols = sheetWidth + 1, rows = creationRow + 1)
    
    # write eGRID data onto sheet
    writeData(wb, current_worksheet, contents[[table_num]], startCol = startcol, 
              startRow = colname_rows[table_num] + 1, colNames = FALSE)
    
    # Table 1 formatting ----------------------------------
    
    if (table_num == 1) {
      
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
      writeData(wb, current_worksheet, eval(colnames_lower), startCol = startcol, 
                startRow = colname_rows[table_num] - 1) # column titles
      writeData(wb, current_worksheet, colnames_units, startCol = startcol, startRow = 1) # column units and larger categories
      
      # add borders
      addStyle(wb, current_worksheet, all_borders, rows = c(1, 4:sheetLength), 
               cols = startcol:(sheetWidth + 1), stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, right_borders, rows = 2:4, cols = startcol:(sheetWidth + 1), 
               stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, top_borders, rows = 2, cols = startcol:(sheetWidth +1), 
               stack = TRUE, gridExpand = TRUE)
      
      # add shading
      addStyle(wb, current_worksheet, grey, rows = 2:4, cols = c(2:3, 18), stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, green, rows = 2:4, cols = 4:10, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, purple, rows = 2:4, cols = 11:17, stack = TRUE, gridExpand = TRUE)
      
      # merge column title cells
      mergeCells(wb, current_worksheet, cols = 2:18, rows = 1)
      for (colnum in c(2, 3, 18)) {
        mergeCells(wb, current_worksheet, cols = colnum, rows = 2:4)
      }
      
      for (colnum in c(4, 11)) {
        for (rownum in c(2, 3)) {
          mergeCells(wb, current_worksheet, cols = c(colnum, colnum + 6), rows = rownum)
        }
      }
      
      ## Data formatting ------------------------------
      
      # add number formats
      addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.0"), rows = 5:sheetLength, 
               cols = c(4, 7:9, 11, 14:16), stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.000"), rows = 5:sheetLength, 
               cols = c(5:6, 10, 12:13, 17), stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.0%"), rows = 5:sheetLength, 
               cols = 18, stack = TRUE, gridExpand = TRUE)
      
      # merge US cells in resource mix
      mergeCells(wb, current_worksheet, cols = 2:3, rows = sheetLength)
      
      ## General formatting ---------------------------
      
      # set cell sizes
      setRowHeights(wb, current_worksheet, rows = 1:4, heights = c(21.75, 14.4, 14.4, 34))
      setRowHeights(wb, current_worksheet, rows = 5:(sheetLength + 1), heights = 15)
      setColWidths(wb, current_worksheet, cols = c(1:3, 18), widths = c(1, 7.75, 16.2, 6.3))
      setColWidths(wb, current_worksheet, cols = 4:17, widths = 5.5)
      
      # Table 2 formatting ----------------------
      
    } else if (table_num == 2) {
      
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
      writeData(wb, current_worksheet, colnames_lower, startCol = startcol, startRow = 2)
      writeData(wb, current_worksheet, colnames_upper, startCol = startcol, startRow = 1)
      
      # add borders
      addStyle(wb, current_worksheet, all_borders, rows = 1:sheetLength , cols = startcol:(sheetWidth + 1),
               stack = TRUE, gridExpand = TRUE)
      
      # add shading
      addStyle(wb, current_worksheet, grey, rows = 2:3, cols = 2:4, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, red, rows = 2:3, cols = 5, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, orange, rows = 2:3, cols = 6:16, stack = TRUE, gridExpand = TRUE)
      
      # merge column title cells
      mergeCells(wb, current_worksheet, cols = 2:16, rows = 1)
      mergeCells(wb, current_worksheet, cols = 6:16, rows = 2)
      for (colnum in 2:5) {
        mergeCells(wb, current_worksheet, cols = colnum, rows = 2:3)
      }
      
      ## Data formatting ------------------
      
      # add number formats
      addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0"), rows = 4:sheetLength, 
               cols = 4:5, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.0%"), rows = 4:sheetLength, 
               cols = 6:16, stack = TRUE, gridExpand = TRUE)
      
      # merge US cells in subregion sheets
      mergeCells(wb, current_worksheet, cols = 2:3, rows = sheetLength)
      
      ## General formatting -----------------
      
      # set cell sizes
      setRowHeights(wb, current_worksheet, rows = 1:3, heights = c(21.75, 15, 45))
      setRowHeights(wb, current_worksheet, rows = 4:(sheetLength + 2), heights = 15)
      setColWidths(wb, current_worksheet, cols = c(1:5, 12, 15, 16), 
                   widths = c(1, 7.75, 16.2, 8.5, 9, 6.4, 6.4, 8))
      setColWidths(wb, current_worksheet, cols = c(6:11, 13:14), widths = 5.9)
      
      # Table 3 formatting -----------
      
    } else if (table_num == 3) {
      
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
      writeData(wb, current_worksheet, colnames_lower, startCol = startcol, startRow = 3)
      writeData(wb, current_worksheet, colnames_units, startCol = startcol, startRow = 1)
      
      # add borders
      addStyle(wb, current_worksheet, all_borders, rows = c(1, 4:sheetLength), cols = 2:(sheetWidth + 1), 
               stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, right_borders, rows = 2:4, cols = 2:(sheetWidth + 1), 
               stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, top_borders, rows = 2, cols = 2:(sheetWidth + 1), 
               stack = TRUE, gridExpand = TRUE)
      
      # add shading
      addStyle(wb, current_worksheet, grey, rows = 2:4, cols = 2, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, green, rows = 2:4, cols = 3:9, stack = TRUE, gridExpand = TRUE)
      
      # merge column title cells
      mergeCells(wb, current_worksheet, cols = 2:9, rows = 1)
      mergeCells(wb, current_worksheet, cols = 2, rows = 2:4)
      for (rownum in c(2,3)) {
        mergeCells(wb, current_worksheet, cols = 3:9, rows = rownum)
      }
      
      ## Data formatting ----------------
      
      # add number formats
      addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.0"), rows = 5:sheetLength, 
               cols = c(3, 6:8), stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.000"), rows = 5:sheetLength, 
               cols = c(4:5, 9), stack = TRUE, gridExpand = TRUE)
      
      ## General formatting ----------------
      
      # set cell sizes
      setRowHeights(wb, current_worksheet, rows = 1:4, heights = c(21.75, 11.25, 11.25, 21.75))
      setRowHeights(wb, current_worksheet, rows = 5:(sheetLength + 1), heights = 13.5)
      setColWidths(wb, current_worksheet, cols = 1:2, width = c(1, 7.5))
      setColWidths(wb, current_worksheet, cols = 3:9, widths = 10)
      
      ##Table 4 formatting ------------------
      
    } else if (table_num == 4) {
      
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
      writeData(wb, current_worksheet, colnames_lower, startCol = startcol, startRow = 2)
      writeData(wb, current_worksheet, colnames_upper, startCol = startcol, startRow = 1)
      
      #add borders
      addStyle(wb, current_worksheet, all_borders, rows = 1:sheetLength, cols = 2:(sheetWidth + 1),
               stack = TRUE, gridExpand = TRUE)
      
      # add shading
      addStyle(wb, current_worksheet, grey, rows = 2:3, cols = 2:3, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, red, rows = 2:3, cols = 4, stack = TRUE,  gridExpand = TRUE)
      addStyle(wb, current_worksheet, orange, rows = 2:3, cols = 5:15, stack = TRUE, gridExpand = TRUE)
      
      # merge column title cells
      mergeCells(wb, current_worksheet, cols = 2:15, rows = 1)
      mergeCells(wb, current_worksheet, cols = 5:15, rows = 2)
      for (colnum in 2:4) {
        mergeCells(wb, current_worksheet, cols = colnum, rows = 2:3)
      }
      
      ## Data formatting --------------
      
      # add number formats
      addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0"), rows = 4:sheetLength,
               cols = 3:4, stack = TRUE, gridExpand = TRUE)
      addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.0%"), rows = 4:sheetLength, 
               cols = 5:15, stack = TRUE, gridExpand = TRUE)
      
      ## General formatting --------------------
      # set cell sizes
      setRowHeights(wb, current_worksheet, rows = 1:3, heights = c(21.75, 11.25, 45))
      setRowHeights(wb, current_worksheet, rows = 4:(sheetLength + 2), heights = 13.5)
      setColWidths(wb, current_worksheet, cols = c(1:4, 9, 11, 14:16), 
                   width = c(1, 5, 7.75, 9, 5.75, 5.9, 5.75, 7.5))
      setColWidths(wb, current_worksheet, cols = c(9, 12:13), widths = 5.4)
      setColWidths(wb, current_worksheet, cols = c(5:8, 10, 12:13), widths = 5.4)
    }  
    
    # All tables formatting ------
    
    # add main titles and format
    writeData(wb, current_worksheet, names(contents[table_num]), startCol = startcol, startRow = 1)
    addStyle(wb, current_worksheet, toptitle, rows = 1, cols = 1:sheetWidth + 1, 
             stack = TRUE, gridExpand = TRUE)
    
    # add column titles formatting
    addStyle(wb, current_worksheet, titles_gen, rows = 2:colname_rows[table_num], 
             cols = 2:(sheetWidth + 1), stack = TRUE, gridExpand = TRUE)
    
    # format US row with thick outline and bold font
    addStyle(wb, current_worksheet, us_boundary, rows = sheetLength, cols = 2:(sheetWidth + 1), 
             stack = TRUE, gridExpand = TRUE)
    
    # add exterior thick borders
    addStyle(wb, current_worksheet, createStyle(border = "Right", borderColour = "black", borderStyle = "thick"),
             rows = 1:creationRow, cols = sheetWidth + 1, stack = TRUE, gridExpand = TRUE)
    addStyle(wb, current_worksheet, createStyle(border = "Left", borderColour = "black", borderStyle = "thick"),
             rows = 1:creationRow, cols = startcol, stack = TRUE, gridExpand = TRUE)
    addStyle(wb, current_worksheet, createStyle(border = "Bottom", borderColour = "black", borderStyle = "thick"),
             rows = creationRow, cols = 2:(sheetWidth + 1), stack = TRUE, gridExpand = TRUE)
  }
}