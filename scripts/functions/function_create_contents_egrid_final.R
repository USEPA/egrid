## -------------------------------
##
## Create contents eGRID final function
## 
## Purpose: 
## 
## This file creates the contents sheet used in the final eGRID document.
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

create_contents_egrid_final <- function(year = params$eGRID_year) {
  
  #' create_contents_egrid_final
  #' 
  #' Function to create and format contents sheet for final excel file.
  #' 
  #' @param year The year of eGRID data used for formatting. 
  #' 
  #' @return One new excel sheet added to the preexisting workbook with a table 
  #'         of contents, feedback, color coding legend, notes, and conversion
  #'         rates.
  #' 
  #' @examples 
  #' # Create the contents sheet for the final eGRID file.
  #' create_contents_egrid_final()
  
  # Write styles ------------

  require(openxlsx)
  
  title_main <- createStyle(
    fontName = "Arial",
    fontSize = 14,
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE)
  
  title_reg <- createStyle(
    fontName = "Arial",
    fontSize = 12,
    textDecoration = "bold",
    valign = "center")
  
  contents <- createStyle(
    fontName = "Arial",
    valign = "center",
    fgFill = "#F2F2F2")

  top_borders <- createStyle(
    border = "Top", 
    borderColour = "black",
    borderStyle = "medium")
  
  bottom_borders <- createStyle(
    border = "Bottom", 
    borderColour = "black",
    borderStyle = "medium")
  
  right_borders <- createStyle(
    border = "Right", 
    borderColour = "black",
    borderStyle = "medium")  
  
  left_borders <- createStyle(
    border = "Left", 
    borderColour = "black",
    borderStyle = "medium") 
  
  all_borders <- createStyle(
    border = "LeftRightTopBottom", 
    borderColour = "black",
    borderStyle = "thin") 
  
  color_coding <- c(
    "#F2DCDB",
    "#E6B8B7",
    "#D58785",
    "#EBF1DE",
    "#C4D79B",
    "#76933C",
    "#DAEEF3",
    "#92CDDC",
    "#31869B",
    "#FDE9D9",
    "#FABF8F",
    "#E26B0A",
    "#FFFFCC",
    "#FFFF99",
    "#E4DFEC",
    "#B1A0C7",
    "#60497A")
  
  # Write data for sections -----------
  
  ## Main title ---------
  
  title <- c(
    "for",
    glue::glue("eGRID{year}_Data.xlsx"),
    glue::glue("eGRID{year} Unit, Generator, Plant, State, Balancing Authority Area, eGRID Subregion, NERC Region, U.S., Grid Gross Loss (%), and Demographic Data Files"),
    format(Sys.Date(), "%B %d, %Y"))
  
  ## Subsection titles -----------

    title_small <- c(
    "Table of Contents",
    "Feedback",
    "Color Coding Legend",
    "Notes",
    "Conversion Factors")
  
  ## Feedback data -------------------
  
  feedback_data <- c("https://www.abtsurvey.com/wix/23/p1172452.aspx", 
                     "https://www.epa.gov/egrid/forms/contact-us-about-egrid")
  names(feedback_data) <- c("Customer Satisfaction Survey",
                            "Contact EPA")
  class(feedback_data) <- "hyperlink"
  
  ## Table of contents --------
  
  # titles for table of contents section
  table_of_contents_titles <- matrix(c(
    "Sheet",
    "Name",
    "",
    "Description"),
    ncol = 4)
  
  # table of contents sheet names
  sheet_names <- c(
    "Unit",
    "Generator",
    "Plant",
    "State",
    "Balancing authority area",
    "eGRID subregion",
    "NERC region",
    "U.S.",
    "Grid Gross Loss (%)")
  
  table_of_contents_descrip <- append(
    paste(sheet_names, glue::glue("year {year} data")),
    glue::glue("Surrounding demographic data for eGRID{year} plants"))
  
  # table of contents production model note
  production_link <- c("https://github.com/USEPA/egrid")
  names(production_link) <- c(glue::glue("eGRID R production model {params$version}."))
  class(production_link) <- "hyperlink"

  ## Color coding legend ---------------
  
  color_coding_sections <- matrix(c(
    "Category",
    "Color",
    "Link to sheet and category"),
    ncol = 3)
    
  # color coding legend categories
  category_names <- c(
    "Annual Values (generation, emissions, and heat input)",
    "Unadjusted Annual Values (emissions, and heat input)",						
    "Adjustment Values (emissions, heat input, heat rate)",						
    "Output Emission Rates (emissions per MWh)",						
    "Input Emission rates (emissions per MMBtu)",						
    "Combustion Output Rates (emissions per MWh)",						
    "Generation by Fuel Type (MWh)",						
    "Renewable and Non-Renewable Generation (MWh)",						
    "Combustion and Non-Combustion Generation (MWh)",						
    "Resource Mix (percentages)",						
    "Renewable and Non-Renewable Resource Mix (percentages)",						
    "Combustion and Non-Combustion Resource Mix (percentages)",						
    "Output Emission Rates by Fuel Type (emissions by fuel type per MWh)",						
    "Input Emission Rates by Fuel Type (emissions by fuel type per MWh)",						
    "Nonbaseload Output Emission Rates (emissions per MWh)",						
    "Nonbaseload Generation by Fuel Type (MWh)",						
    "Nonbaseload Resource Mix (percentages)")
  
  category_labeled <- paste0(1:length(category_names), paste(")", category_names))
  
  ## Notes ------------
  
  notes <- c(
    "Values in parentheses are negative numbers.",
    "Dashes (-) are zeroes.")
  
  ## Conversion factors ---------------
  
  conversion_factors <- matrix(c(
    "1 megawatt-hour (MWh)",
    "1 short ton",	
    "1.10231 short ton", 	
    "2.2046 lb",	
    "0.9478 MMBtu", 	
    "0.27778 MWh",
    "1,000 kilowatt-hour (kWh)",	
    "2,000 pounds (lb)",
    "1 metric ton",	
    "1 kilogram (kg)",	
    "1 Gigajoule (GJ)",	
    "1 GJ"),
    ncol = 2,
    byrow = FALSE)
  

  # Create new worksheet ----------------
  
  current_worksheet <- "Contents"
  addWorksheet(wb, sheetName = current_worksheet)
  
  # set section locations
  start_rows <- c(2, 21, 25, 45, 49)
  end_rows <- c(
    start_rows[1] + 17,
    start_rows[2] + 2,
    start_rows[3] + 18,
    start_rows[4] + 2,
    start_rows[5] + 6)
  start_cols <- 2
  sheetWidth <- start_cols + 9

  ## Add data ----------------

  # table of contents
  writeData(wb, current_worksheet, table_of_contents_titles, startCol = start_cols, 
            startRow = start_rows[1]+ length(title) + 1)
  writeData(wb, current_worksheet, title, startCol = start_cols, 
            startRow = start_rows[1] + 1)
  writeData(wb, current_worksheet, x = 1:length(table_of_contents_descrip), 
            startCol = start_cols, startRow = start_rows[1] + length(title) + 3)
  writeData(wb, current_worksheet, table_of_contents_descrip, startCol = start_cols + 3, 
            startRow = start_rows[1] + length(title) + 3)
  writeData(wb, current_worksheet, "Produced with", startCol = start_cols, 
            startRow = start_rows[1]+ length(title) + 1)
  writeData(wb, current_worksheet, production_link, startCol = start_cols + 4, 
            startRow = start_rows[1]+ length(title) + 1)
  
  # feedback
  writeData(wb, current_worksheet, feedback_data, startCol = start_cols, 
            startRow = start_rows[2] + 1)
  
  # color coding legend
  writeData(wb, current_worksheet, color_coding_sections, startCol = start_cols + 6, 
            startRow = start_rows[3])
  writeData(wb, current_worksheet, color_coding_sections, startCol = start_cols, 
            startRow = start_rows[3])
  writeData(wb, current_worksheet, category_labeled, startCol = start_cols, 
            startRow = start_rows[3] + 2)
  
  # notes
  writeData(wb, current_worksheet, notes, startCol = start_cols, 
            startRow = start_rows[4] + 1)
  
  # conversion factors
  writeData(wb, current_worksheet, conversion_factors, startCol = start_cols + 1, 
            startRow = start_rows[5])
  writeData(wb, current_worksheet, conversion_factors, startCol = start_cols, 
            startRow = start_rows[5])
  
  ## Add section titles and style ---------------
  
  for (i in 1:length(title_small)) {
    writeData(wb, current_worksheet, title_small[i], startCol = start_cols, 
              startRow = start_rows[i])
    if (i != 1) {
      addStyle(wb, current_worksheet, title_reg, rows = start_rows[i], 
               cols = start_cols, stack = TRUE)
    }
  }
  
  ## Add merging, borders, and styling --------
  
  # lines within color coding legend
  addStyle(wb, current_worksheet, all_borders, cols = start_cols:(sheetWidth + 6), 
           rows = (start_rows[3] + 2):end_rows[3], gridExpand = TRUE, stack = TRUE)
  
  for (row in c(start_rows[1]:end_rows[1],
                start_rows[2]:end_rows[2],
                start_rows[3]:end_rows[3],
                start_rows[4]:end_rows[4],
                start_rows[5]:end_rows[5])) {
    # table of contents and feedback
    if (row %in% c(start_rows[1]:end_rows[1],
                   start_rows[2]:end_rows[2])) {
      format_width <- sheetWidth
      # table of contents description information
      if (row %in% c((start_rows[1] + 6):(end_rows[1] - 1))) {
        mergeCells(wb, current_worksheet, rows = row, 
                   cols = (start_cols + 1):(start_cols + 2))
        mergeCells(wb, current_worksheet, rows = row, 
                   cols = (start_cols + 3):format_width)
      }
      else if (row == end_rows[1]) {
        mergeCells(wb, current_worksheet, rows = row,
                   cols = (start_cols + 1):format_width)
      }
      # color coding legend  
    } else if (row %in% c(start_rows[3]:end_rows[3])) {
      format_width <- sheetWidth + 6
      # merging inside rows
      if (row != start_rows[3]) {
        mergeCells(wb, current_worksheet, rows = row, 
                   cols = start_cols:(start_cols + 6))
      }
      # notes and conversion factors
    } else if (row %in% c(start_rows[4]:end_rows[4],
                          start_rows[5]:end_rows[5])) {
      format_width <- start_cols + 4
      # merging conversion factors
      if (row %in% c((start_rows[5] + 1):end_rows[5])) {
        mergeCells(wb, current_worksheet, rows = row, 
                   cols = start_cols:(start_cols + 1))
        mergeCells(wb, current_worksheet, rows = row, 
                   cols = (start_cols + 2):(start_cols + 3))
      }
    }
    # main titles
    if (row %in% c(start_rows[1]:(start_rows[1] + 4), start_rows[2]:end_rows[2], 
                   start_rows[3], start_rows[4]:end_rows[4], start_rows[5])) {
      mergeCells(wb, current_worksheet, rows = row, cols = start_cols:format_width)
    }
    # borders and shading
    addStyle(wb, current_worksheet, right_borders, rows = row, cols = format_width,
             stack = TRUE, gridExpand = TRUE)
    addStyle(wb, current_worksheet, contents, rows = row, 
             cols = start_cols:format_width,
             stack = TRUE, gridExpand = TRUE)
    addStyle(wb, current_worksheet, left_borders, rows = row, cols = start_cols, 
             stack = TRUE, gridExpand = TRUE)
    if (row %in% start_rows) {
      addStyle(wb, current_worksheet, top_borders, rows = row, 
               cols = start_cols:format_width,
               stack = TRUE, gridExpand = TRUE)
    } else if (row %in% end_rows) {
      addStyle(wb, current_worksheet, bottom_borders, rows = row, 
               cols = start_cols:format_width,
               stack = TRUE, gridExpand = TRUE)
    }
  }
  
  # table of contents
  addStyle(wb, current_worksheet, title_main, cols = start_cols, 
           rows = start_rows[1]:(start_rows[1] + 4), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, current_worksheet, createStyle(textDecoration = "underline"), 
           cols = start_cols:sheetWidth, rows = start_rows[1] + 6, gridExpand = TRUE, 
           stack = TRUE)
  addStyle(wb, current_worksheet, createStyle(halign = "center"), cols = start_cols, 
           rows = (start_rows[1] + 5):end_rows[1], gridExpand = TRUE, stack = TRUE)
  addStyle(wb, current_worksheet, createStyle(fontSize = 12), cols = start_cols:sheetWidth,
           rows = (start_rows[1] + 5):end_rows[1], gridExpand = TRUE, stack = TRUE)
  addStyle(wb, current_worksheet, createStyle(halign = "right", textDecoration = "bold"),
           rows = start_rows[1] + 5, cols = start_cols, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, current_worksheet, createStyle(valign = "bottom", fontSize = 12), rows = start_rows[1] + 4, 
           cols = start_cols, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, current_worksheet, createStyle(valign = "top"),
           rows = start_rows[1] + 5, cols = start_cols:sheetWidth, gridExpand = TRUE, stack = TRUE)
  
  mergeCells(wb, current_worksheet, cols = start_cols:(start_cols + 3), rows = start_rows[1] + 5)
  mergeCells(wb, current_worksheet, cols = (start_cols + 4):sheetWidth, rows = start_rows[1] + 5)
  
  # color coding legend
  addStyle(wb, current_worksheet, createStyle(halign = "center"), cols = start_cols, 
           rows = start_rows[3], gridExpand = TRUE, stack = TRUE)
  addStyle(wb, current_worksheet, createStyle(halign = "center"), 
           cols = (start_cols + 7):(start_cols + 8), rows = start_rows[3] + 1, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, current_worksheet, createStyle(halign = "center"), 
           cols = (start_cols + 8):(start_cols + 15), rows = (start_rows[3] + 1):end_rows[3], 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, current_worksheet, title_reg, cols = start_cols:sheetWidth, 
           rows = start_rows[3] + 1, gridExpand = TRUE, stack = TRUE)
  mergeCells(wb, current_worksheet, cols = (sheetWidth - 1):(sheetWidth + 6), 
             rows = start_rows[3] + 1)
  addStyle(wb, current_worksheet, bottom_borders, cols = start_cols:(sheetWidth + 6), 
           rows = start_rows[3] + 1, gridExpand = TRUE, stack = TRUE)

  # color coding fills
  for (i in 1:length(color_coding)) {
    addStyle(wb, current_worksheet, createStyle(fgFill = color_coding[i]), 
             cols = start_cols + 7, rows = start_rows[3] + 1 + i, stack = TRUE)
  }
  
  # add font formatting
  addStyle(wb, current_worksheet, createStyle(fontName = "Arial"), cols = start_cols:(sheetWidth + 6),
           rows = 1:55, gridExpand = TRUE, stack = TRUE)
  
  ## Add cell sizes -------------

  setRowHeights(wb, current_worksheet, rows = 2:4, heights = 17.4)
  setRowHeights(wb, current_worksheet, rows = c(7, 18, 20, 24), heights = 15.6)
  setRowHeights(wb, current_worksheet, rows = c(5:7, 25), heights = c(43.8, 25.8, 25.8, 16.2))
  setColWidths(wb, current_worksheet, cols = 1:12, widths = 10.33)
  setColWidths(wb, current_worksheet, cols = c(1:2, 5:6), widths = c(1, 14.33, 8.33, 18))
  setColWidths(wb, current_worksheet, cols = 10:19, widths = 5)
}