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
  #' Function to create and format contents sheet for summary tables file
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
  #' create_contents_summary_tables()
  
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
  
  created_stamp <- createStyle(
    fontSize = 8,
    valign = "center",
    halign = "center"
    )
  
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
  table_of_contents <- matrix(c("Description", worksheet_contents), nrow = 5, byrow = TRUE)
  
  # write introduction data 
  introduction <- glue::glue("This document provides eGRID{params$eGRID_year} data summary tables. The tables include subregion and state-level emission rates and resource mix as well as grid gross loss values. Please note that the tables presented here only show a subset of the eGRID{params$eGRID_year} data. The entire dataset is in the eGRID{params$eGRID_year} Excel file available on the eGRID website.")
  
  # write feedback data
  feedback_data <- c("https://www.abtsurvey.com/wix/23/p1172452.aspx", 
                     "https://www.epa.gov/egrid/forms/contact-us-about-egrid")
  names(feedback_data) <- c("Customer Satisfaction Survey",
                            "Contact EPA")
  class(feedback_data) <- "hyperlink"
  
  # write production link
  production_link <- c("https://github.com/USEPA/egrid")
  names(production_link) <- c(glue::glue("eGRID R production model {params$version}."))
  class(production_link) <- "hyperlink"
  
  # Classify worksheet parameters ---------------
  
  # section names
  titles <- c(
    paste("eGRID Summary Tables", params$eGRID_year),
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
  start_cols <- 2
  sheetWidth <- start_cols + 9
  
  # Create new worksheet ----------------
  
  current_worksheet <- "Contents"
  addWorksheet(wb, sheetName = current_worksheet)
  
  # set font size
  addStyle(wb, current_worksheet, createStyle(fontName = "Arial", fontSize = "12"), 
           cols = 1:sheetWidth, rows = 1:50, gridExpand = TRUE, stack = TRUE)

  ## Add created datestamp ----------------
  
  addStyle(wb, current_worksheet, created_stamp, cols = start_cols:sheetWidth,
           rows = end_rows[4] + 1)
  
  writeData(wb, current_worksheet, glue::glue("Produced on {format(Sys.Date(), '%m/%d/%Y')} with"), 
            startCol = sheetWidth - 3, startRow = end_rows[4])
  writeData(wb, current_worksheet, production_link, startCol = sheetWidth - 1, startRow = end_rows[4] + 1)
  
  # add creation data styling
  mergeCells(wb, current_worksheet, cols = (sheetWidth - 3):(sheetWidth - 2),
             rows = end_rows[4] + 1)
  mergeCells(wb, current_worksheet, cols = (sheetWidth - 1):sheetWidth,
             rows = end_rows[4] + 1)
  addStyle(wb, current_worksheet, createStyle(halign = "right"),
           cols = sheetWidth - 3, rows = end_rows[4] + 1, stack = TRUE)

  ## Add images -------------
  
  insertImage(wb, current_worksheet, file = "data/static_tables/formatting/eGRID_subregions.png",
              width = 7.24, height = 5.41, startRow = end_rows[3] + 2, startCol = start_cols)
  insertImage(wb, current_worksheet, file = glue::glue("data/static_tables/formatting/eGRIDLogoYear{params$eGRID_year}.png"),
              width = 2.06, height = 1.16, startRow = start_rows[2] + 1, startCol = start_cols + 1, 
              address = "https://www.epa.gov/egrid")
   
  ## Add data ----------------
  
  writeData(wb, current_worksheet, table_of_contents, startCol = start_cols + 2, startRow = start_rows[3])
  writeData(wb, current_worksheet, "Table", startCol = start_cols, startRow = start_rows[3] + 1)
  writeData(wb, current_worksheet, introduction, startCol = start_cols + 4, startRow = start_rows[2])
  writeData(wb, current_worksheet, feedback_data, startCol = start_cols, startRow = start_rows[4] + 1)
  
  # add hyperlinks to sheets
  for (i in 1:4) {
    link <- makeHyperlinkString(sheet = paste("Table",i), text = i)
    writeFormula(wb, "Contents", link, xy = c(start_cols, start_rows[3] + 1 + i))                  
  }
  
  ## Add styling and merges ---------------------
  
  # content merges and styling
  for (row in c((start_rows[2]) + 1, (start_rows[3] + 1):end_rows[3], 
                (start_rows[4] + 1):end_rows[4])) {
    # introduction
    if (row == start_rows[2] + 1) {
      mergecol <- start_cols + 4
      # table of contents
    } else if (row < start_rows[4] + 1) {
      mergecol <- start_cols + 2
      addStyle(wb, current_worksheet, toc_labels, rows = row, cols = start_cols, 
               stack = TRUE, gridExpand = TRUE)
      mergeCells(wb, current_worksheet, cols = start_cols:(start_cols + 1), rows = row)
      # table of contents numbers
      if (row == start_rows[3] + 1) {
        addStyle(wb, current_worksheet, createStyle(textDecoration = "underline"), 
                 rows = row, cols = start_cols:(start_cols + 2), stack = TRUE, gridExpand = TRUE)
      }
      # feedback
    } else {
      mergecol <- start_cols
      addStyle(wb, current_worksheet, createStyle(textDecoration = "underline"), rows = row, 
               cols = start_cols, stack = TRUE, gridExpand = TRUE)
    }
    # format for all
    addStyle(wb, current_worksheet, contents, rows = row, cols = start_cols:sheetWidth, 
             stack = TRUE, gridExpand = TRUE)
    mergeCells(wb, current_worksheet, rows = row, cols = mergecol:sheetWidth)
  }
  
  ## Add cell sizes -------------
  
  setRowHeights(wb, current_worksheet, rows = c(start_rows[1] - 1, start_rows[1],
    start_rows[2], start_rows[2] + 1, start_rows[3], start_rows[3] + 1,
    end_rows[3], end_rows[3] + 1, start_rows[4] - 1, start_rows[4]),
                heights = c(8.3, 42.8, 17.4, 90.8, 17.4, 15.8, 6.8, 6, 20, 17.4))
  setColWidths(wb, current_worksheet, cols = 1:12, widths = 8.33)
  setColWidths(wb, current_worksheet, cols = c(1:5, 7, 9, 11), 
               widths = c(1, 0.5, 11.5, 7.35, 4.89, 10.91, 9, 10.5))
  
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
                                              start_rows[3]:end_rows[3], start_rows[4]:end_rows[4]), 
           cols = sheetWidth, stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, left_borders, rows = c(end_rows[1], start_rows[2]:end_rows[2], 
                                             start_rows[3]:end_rows[3], start_rows[4]:end_rows[4]), 
           cols = start_cols, stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, top_borders, rows = start_rows, cols = start_cols:sheetWidth,
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, current_worksheet, bottom_borders, rows = end_rows, cols = start_cols:sheetWidth,
           stack = TRUE, gridExpand = TRUE)
  
  ## Set printing preferences ------------
  
  pageSetup(wb, current_worksheet, orientation = "portrait", fitToWidth = TRUE,
            fitToHeight =  TRUE, left = 0.5, right = 0.5, top = 0.5, bottom = 0.5)
  
}