## -------------------------------
##
## Create contents sheet for eGRID summary tables.
## 
## Purpose: 
## 
## This file creates the contents sheet used in the eGRID summary tables.
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

create_contents_summary_tables <- function(contents = table_data) {
  
  #' create_summary_tables
  #' 
  #' Function to create and format summary tables for final excel file.
  #' 
  #' @param contents Vector of the datasets containing the data for 
  #'                 the summary tables - includes four formatted datasets.
  #' 
  #' @return One new excel sheet added to the preexisting workbook with a title,
  #'         introduction, table of contents, image of eGRID subregions, and 
  #'         feedback section. Includes external hyperlinks.
  #' 
  #' @examples 
  #' # Create the contents sheet for the datasets within table_data.
  #' create_contents_summarytables()
  
  # Define select styles ------------
  
  title_main <- createStyle(
    fontSize = 18,
    textDecoration = "bold",
    halign = "center",
    valign = "center", 
    fgFill = "#D9D9D9")
  
  title_reg <- createStyle(
    fontSize = 14,
    textDecoration = "bold",
    halign = "center",
    valign = "center", 
    fgFill = "#D9D9D9")
  
  contents <- createStyle(
    wrapText = TRUE,
    valign = "center",
    fgFill = "#F2F2F2")
  
  toc_labels <- createStyle(
    textDecoration = "underline",
    halign = "center")
  
  top_borders <- createStyle(
    border = "Top", 
    borderColour = "black",
    borderStyle = "thin")
  
  bottom_borders <- createStyle(
    border = "Bottom", 
    borderColour = "black",
    borderStyle = "thin")
  
  right_borders <- createStyle(
    border = "Right", 
    borderColour = "black",
    borderStyle = "thin")  
  
  left_borders <- createStyle(
    border = "Left", 
    borderColour = "black",
    borderStyle = "thin") 
  
  # Write data for sections -----------

  # format table of contents data using worksheet contents
  worksheet_contents <- names(table_data)
  remove_strings <- c("\\s*\\([^\\)]+\\)", "[1-4]. ")
  for (pattern in remove_strings) {
    worksheet_contents <- lapply(worksheet_contents, str_remove, pattern)
  }
  table_of_contents <- matrix(rbind(c("Table", as.character(1:4)), c("Description", worksheet_contents)), 
                              ncol = 2, byrow = TRUE)
  
  # write introduction data 
  introduction <- glue::glue("This document provides eGRID{params$eGRID_year} data summary tables. The tables include subregion and state-level emission rates and resource mix as well as grid gross loss values. Please note that the tables presented here only show a subset of the eGRID{params$eGRID_year} data. The entire dataset is in the eGRID{params$eGRID_year} Excel file available on the eGRID website.")
  
  # write feedback data
  feedback_data <- c("https://www.abtsurvey.com/wix/23/p1172452.aspx", 
                     "https://www.epa.gov/egrid/forms/contact-us-about-egrid")
  names(feedback_data) <- c("Customer Satisfaction Survey",
                            "Contact EPA")
  class(feedback_data) <- "hyperlink"
  
  # Classify workheet parameters ---------------
  
  # section names
  titles <- c(
    paste("eGRID Summary Tables"),
    "Introduction",
    "Table of Contents",
    "Feedback")
  
  # section locations
  start_rows <- c(2, 4, 7, 42)
  end_rows <- c(
    start_rows[1],
    start_rows[2] + 1,
    start_rows[3] + 6,
    start_rows[4] + 2)
  start_cols <- 3
  sheetWidth <- 12
  
  # Create new worksheet ----------------
  
  current_worksheet <- "Contents"
  addWorksheet(wb, sheetName = current_worksheet)
  modifyBaseFont(wb, fontSize = 12, fontName = "Arial")
  
  ## Add created datestamp
  writeData(wb, current_worksheet, x = matrix(c("Created:", format(Sys.Date(), "%m/%d/%Y")), 
                                  ncol = 2), startCol = sheetWidth - 1, startRow = end_rows[4])
  # add creation data styling
  addStyle(wb, current_worksheet, createStyle(fontSize = 8, halign = "right"), cols = sheetWidth - 1,
           rows = end_rows[4] + 1)
  addStyle(wb, current_worksheet, createStyle(fontSize = 8, halign = "center"), cols = sheetWidth,
           rows = end_rows[4] + 1)
  ## Add images -------------
  
  insertImage(wb, current_worksheet, file = "data/static_tables/eGRID_subregions.png", 
              width = 7.24, height = 5.41, startRow = 15, startCol = start_cols)
  insertImage(wb, current_worksheet, file = "data/static_tables/eGRID_logo.png",
              width = 2.06, height = 1.16, startRow = 5, startCol = 4)
  
  ## Add data ----------------
  
  # write table of contents twice to account for cell merge
  writeData(wb, current_worksheet, table_of_contents, startCol = start_cols + 1, startRow = start_rows[3])
  writeData(wb, current_worksheet, table_of_contents, startCol = start_cols, startRow = start_rows[3])
  writeData(wb, current_worksheet, introduction, startCol = start_cols + 4, startRow = start_rows[2])
  writeData(wb, current_worksheet, feedback_data, startCol = start_cols, startRow = start_rows[4] + 1)
  
  ## Add styling and merges ---------------------
  
  # content merges and styling
  for (row in c((start_rows[2]) + 1, (start_rows[3] + 1):end_rows[3], 
                (start_rows[4] + 1):end_rows[4])) {
    # introduction
    if (row == start_rows[2] + 1) {
      mergecol <- 7
      # table of contents
    } else if (row < start_rows[4] + 1) {
      mergecol <- 5
      addStyle(wb, current_worksheet, toc_labels, rows = row, cols = start_cols, stack = TRUE, gridExpand = TRUE)
      mergeCells(wb, current_worksheet, cols = 3:4, rows = row)
      if (row == start_rows[3] + 1) {
        addStyle(wb, current_worksheet, createStyle(textDecoration = "underline"), rows = row, 
                 cols = start_cols:5, stack = TRUE, gridExpand = TRUE)
      }
      # feedback
    } else {
      mergecol <- 3
      addStyle(wb, current_worksheet, createStyle(textDecoration = "underline"), rows = row, 
               cols = start_cols, stack = TRUE, gridExpand = TRUE)
    }
    # format for all
    addStyle(wb, current_worksheet, contents, rows = row, cols = start_cols:sheetWidth, stack = TRUE, gridExpand = TRUE)
    mergeCells(wb, current_worksheet, rows = row, cols = mergecol:sheetWidth)
  }
  
  ## Add cell sizes -------------
  
  setRowHeights(wb, current_worksheet, rows = c(1:2, 4:5, 7:8, 13:14, 41:42), 
                heights = c(8.3, 42.8, 17.4, 90.8, 17.4, 15.8, 6.8, 6, 6.5, 17.4))
  setColWidths(wb, current_worksheet, cols = 1:12, widths = 8.33)
  setColWidths(wb, current_worksheet, cols = c(1:6, 12), widths = c(1, 3.78, 0.5, 12, 4.5, 4.4, 7.5))
  
  ## Add section titles ---------------
  
  for (i in 1:length(titles)) {
    writeData(wb, current_worksheet, titles[i], startCol = start_cols, startRow = start_rows[i])
    if ( i == 1) {
      addStyle(wb, current_worksheet, title_main, rows = start_rows[i], cols = start_cols)
    } else {
      addStyle(wb, current_worksheet, title_reg, rows = start_rows[i], cols = start_cols)
    }
    mergeCells(wb, current_worksheet, rows = start_rows[i], cols = start_cols:sheetWidth)
  }
  
  ## Add borders --------
  
  addStyle(wb, current_worksheet, right_borders, rows = c(end_rows[1], start_rows[2]:end_rows[2], 
                                              start_rows[3]:end_rows[3], start_rows[4]:end_rows[4]), cols = sheetWidth, 
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, left_borders, rows = c(end_rows[1], start_rows[2]:end_rows[2], 
                                             start_rows[3]:end_rows[3], start_rows[4]:end_rows[4]), cols = start_cols, 
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, top_borders, rows = start_rows, cols = start_cols:sheetWidth,
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, bottom_borders, rows = end_rows, cols = start_cols:sheetWidth,
           stack = TRUE, gridExpand = TRUE)
  
  pageSetup(wb, current_worksheet, orientation = "portrait", fitToWidth = TRUE,
            fitToHeight =  TRUE, left = 0.5, right = 0.5, top = 0.5, bottom = 0.5)
  
}