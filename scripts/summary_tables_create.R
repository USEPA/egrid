## -------------------------------
##
## Create summary tables of finalized eGRID data.
## 
## Purpose: 
## 
## This file pulls data from .RDS files to create summary tables
## saved in an excel sheet.
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries ------------------------------------------

library(dplyr)
library(glue)
library(openxlsx)
library(readr)
library(stringr)
library(tidyr)

# Define eGRID year ------------------

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.")
  } else { # if params() is defined, but eGRID_year is not, define it here
    params$eGRID_year <- as.character(readline(prompt = "Input eGRID_year: "))
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- as.character(readline(prompt = "Input eGRID_year: "))
}

# Import .RDS data --------------------------------

# create a list of files in R directory
data_dir <- glue::glue("data/outputs/{params$eGRID_year}/")
filenames <- list("state_aggregation.RDS", "subregion_aggregation.RDS", 
                  "grid_gross_loss.RDS", "us_aggregation.RDS")

# import files in list
for (file in (filenames)){
  assign(str_remove(file, ".RDS"), read_rds(paste0(data_dir, file)))
}

# Compile and format subregion output emissions rates for TABLE 1   ------------------

# read in crosswalk assigning subregions to interconnect regions
xwalk_subregion_ggl <- 
  read.csv("data/static_tables/xwalk_subregion_interconnect.csv") %>%
  janitor::clean_names() %>%
  rename(subregion = subregion_code) %>%
  # join grid gross loss data by interconnect assignment
  left_join(grid_gross_loss %>%
              select(interconnect, ggl), by = "interconnect")

# list emission types wanted in summary tables
emission_types <- c(
  "co2",
  "ch4",
  "n2o",
  "co2e",
  "nox",
  "nox_oz",
  "so2")

# US output emissions data
us_output_emissions <-
  us_aggregation %>%
  select(any_of(paste0("us_", emission_types, "_output_rate")), 
         any_of(paste0("us_", emission_types, "_output_rate_nonbaseload"))) %>%
  rename_with(~str_c(str_remove(., "us_")))

# subregion output emissions
subregion_output_emissions <-
  subregion_aggregation %>%
  select(subregion, subregion_name, any_of(paste0("subregion_", emission_types, "_output_rate")), 
         any_of(paste0("subregion_", emission_types, "_output_rate_nonbaseload"))) %>%
  rename_with(~str_c(str_remove(., "subregion_"))) %>%
  # add US data to bottom row
  bind_rows(us_output_emissions) %>%
  mutate(subregion = replace_na(subregion, "U.S.")) %>%
  # add grid gross loss as column
  left_join(xwalk_subregion_ggl, by = "subregion") %>%
  # remove interconnect data
  select(!interconnect)

# Compile and format subregion resource mix data for TABLE 2 ------------------

# list resource types wanted in summary tables
resource_type <- c(
  "coal",
  "oil",
  "gas",
  "other_ff",
  "nuclear",
  "hydro",
  "biomass",
  "wind",
  "solar",
  "geothermal",
  "other")

# US emissions data
us_resource_mix <-
  us_aggregation %>%
  select(us_nameplate_capacity, us_generation_ann, any_of(paste0("us_ann_resource_mix_", resource_type))) %>%
  rename_with(~str_c(str_remove(., "us_")))

# subregion resource mix
subregion_resource_mix <-
  subregion_aggregation %>%
  select(subregion, subregion_name, subregion_nameplate_capacity, subregion_generation_ann, 
         any_of(paste0("subregion_ann_resource_mix_", resource_type))) %>%
  rename_with(~str_c(str_remove(., "subregion_"))) %>%
  # add US data to bottom row
  bind_rows(us_resource_mix) %>%
  mutate(subregion = replace_na(subregion, "U.S."))

# Compile and format state output emission rates for TABLE 3 -------------------

# state output emission rates
state_output_emissions <-
  state_aggregation %>%
  select(state, any_of(paste0("state_", emission_types, "_output_rate"))) %>%
  rename_with(~str_c(str_remove(., "state_"))) %>%
  # add US data to bottom row
  bind_rows(us_output_emissions %>% 
              select(!contains("nonbaseload"))) %>%
  mutate(state = replace_na(state, "U.S."))

# Compile and format state resource mix data for TABLE 4  ----------------------

# state resource mix
state_resource_mix <-
  state_aggregation %>%
  select(state, state_nameplate_capacity, state_generation_ann, 
         any_of(paste0("state_ann_resource_mix_", resource_type))) %>%
  rename_with(~str_c(str_remove(., "state_"))) %>%
  # add US data to bottom row
  bind_rows(us_resource_mix) %>%
  mutate(state = replace_na(state, "U.S."))

# List excel styles ------
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

toptitle <- createStyle(
  fontSize = 12,           
  halign = "center", 
  valign = "center", 
  wrapText = TRUE, 
  textDecoration = "bold", 
  border = "TopLeftRight", 
  borderColour = "black", 
  borderStyle = "thick")

grey <- createStyle(fgFill = "#F2F2F2")
green <- createStyle(fgFill = "#ebf1de")
purple <- createStyle(fgFill = "#e4dfec")
red <- createStyle(fgFill = "#f2dcdb")
orange <- createStyle(fgFill = "#fde9d9")

us_boundary <- createStyle(textDecoration = "bold",
                             border = "Top",
                             borderStyle = "thick",
                             borderColour = "black")

# Format as excel sheet ---------------------------

# create workbook
wb <- createWorkbook()

# set base font
modifyBaseFont(wb, fontSize = 8.5, fontName = "Arial")

# define data for sheets
table_data <- list(subregion_output_emissions,
                   subregion_resource_mix,
                   state_output_emissions,
                   state_resource_mix)

# add sheet titles
names(table_data) <- c(
  glue::glue("1. Subregion Output Emission Rates (eGRID{params$eGRID_year})"),
  glue::glue("2. Subregion Resource Mix (eGRID{params$eGRID_year})"),
  glue::glue("3. State Output Emission Rates (eGRID{params$eGRID_year})"),
  glue::glue("4. State Resource Mix (eGRID{params$eGRID_year})")
)

# define sheet parameters
startcol <- 2 # column to start data write
colname_rows <- c(4, 3, 4, 3) # number of rows for titles
sheet_orientation <- c("landscape", "landscape", "portrait", "portrait")

for (sheet in 1:4) {
  
  # create worksheets and define sheet width and lengths
  addWorksheet(wb, sheetName = paste0("Table ",sheet))
  sheetWidth <- as.numeric(ncol(table_data[[sheet]]))
  sheetLength <- nrow(table_data[[sheet]]) + colname_rows[sheet]
  
  pageSetup(wb, sheet, orientation = sheet_orientation[sheet], fitToWidth = TRUE,
            fitToHeight =  TRUE, left = 0.5, right = 0.5, top = 0.5, bottom = 0.5)

  if (sheet %in% c(2, 4)) {
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
  writeData(wb, sheet, table_data[[sheet]], startCol = startcol, startRow = colname_rows[sheet] + 1, 
            colNames = FALSE)
  
  ## Table 1 formatting ----------------------------------
  if (sheet == 1) {
    
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
    
    ### Column title formatting ---------------------------
    
    # write column titles
    writeData(wb, sheet, eval(colnames_lower), startCol = startcol, startRow = colname_rows[sheet] - 1) # column titles
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
    
    ### Data formatting ------------------------------
    
    # add number formats
    addStyle(wb, sheet, createStyle(numFmt = "#,##0.0"), rows = 5:sheetLength, 
             cols = c(4, 7:9, 11, 14:16), stack = TRUE, gridExpand = TRUE)
    addStyle(wb, sheet, createStyle(numFmt = "#,##0.000"), rows = 5:sheetLength, 
             cols = c(5:6, 10, 12:13, 17), stack = TRUE, gridExpand = TRUE)
    addStyle(wb, sheet, createStyle(numFmt = "#,##0.0%"), rows = 5:sheetLength, 
             cols = 18, stack = TRUE, gridExpand = TRUE)
    
    # merge US cells in resource mix
    mergeCells(wb, sheet, cols = 2:3, rows = sheetLength)
    
    ### General formatting ---------------------------
    
    # set cell sizes
    setRowHeights(wb, sheet, rows = 1:4, heights = c(22, 14.4, 14.4, 35))
    setRowHeights(wb, sheet, rows = 5:(sheetLength + 1), heights = 15)
    setColWidths(wb, sheet, cols = c(1:3, 18), widths = c(1, 10, 22, 8.75))
    setColWidths(wb, sheet, cols = 4:17, widths = 7)

    ## Table 2 formatting ----------------------
  } else if (sheet == 2) {
    
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
    
    ### Column title formatting ---------------------------
    
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
    
    ### Data formatting ------------------
    
    # add number formats
    addStyle(wb, sheet, createStyle(numFmt = "#,##0"), rows = 4:sheetLength, 
             cols = 4:5, stack = TRUE, gridExpand = TRUE)
    addStyle(wb, sheet, createStyle(numFmt = "#,##0.0%"), rows = 4:sheetLength, 
             cols = 6:16, stack = TRUE, gridExpand = TRUE)
    
    # merge US cells in subregion sheets
    mergeCells(wb, sheet, cols = 2:3, rows = sheetLength)
    
    ### General formatting -----------------
    
    # set cell sizes
    setRowHeights(wb, sheet, rows = 1:3, heights = c(22, 15, 45))
    setRowHeights(wb, sheet, rows = 4:(sheetLength + 2), heights = 15)
    setColWidths(wb, sheet, cols = c(1:5, 12, 16), 
                 widths = c(1, 10, 22, 10, 12, 8.2, 10))
    setColWidths(wb, sheet, cols = c(6:11, 13:15), widths = 8)
    
    ## Table 3 formatting -----------
  } else if (sheet == 3) {
    
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
    
    ### Column title formatting -----------------
    
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
    
    ### Data formatting ----------------
    
    # add number formats
    addStyle(wb, sheet, createStyle(numFmt = "#,##0.0"), rows = 5:sheetLength, cols = c(3, 6:8),
             stack = TRUE, gridExpand = TRUE)
    addStyle(wb, sheet, createStyle(numFmt = "#,##0.000"), rows = 5:sheetLength, cols = c(4:5, 9),
             stack = TRUE, gridExpand = TRUE)

    ### General formatting ----------------
    
    # set cell sizes
    setRowHeights(wb, sheet, rows = 1:4, heights = c(22, 11.25, 11.25, 22))
    setRowHeights(wb, sheet, rows = 5:(sheetLength + 1), heights = 15)
    setColWidths(wb, sheet, cols = 1:2, width = c(1, 10))
    setColWidths(wb, sheet, cols = 3:9, widths = 15)
    
    ## Table 4 formatting ------------------
  } else if (sheet == 4) {
    
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
    
    ### Column title and formatting -----------------
    
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
    
    ### Data formatting --------------

    # add number formats
    addStyle(wb, sheet, createStyle(numFmt = "#,##0"), rows = 4:sheetLength,
             cols = 3:4, stack = TRUE, gridExpand = TRUE)
    addStyle(wb, sheet, createStyle(numFmt = "#,##0.0%"), rows = 4:sheetLength, 
             cols = 5:15, stack = TRUE, gridExpand = TRUE)
    
    ### General formatting --------------------
    # set cell sizes
    setRowHeights(wb, sheet, rows = 1:3, heights = c(22, 15, 45))
    setRowHeights(wb, sheet, rows = 4:(sheetLength + 2), heights = 15)
    setColWidths(wb, sheet, cols = c(1:4, 9, 11, 14:15), 
                 width = c(1, 7.5, 10, 12, 8, 9, 8.5, 11))
    setColWidths(wb, sheet, cols = c(5:8, 10, 12:13), widths = 7)
  }  
  
  ## Add general formatting ------
  
  # add main titles and format
  writeData(wb, sheet, names(table_data[sheet]), startCol = startcol, startRow = 1)
  addStyle(wb, sheet, toptitle, rows = 1, cols = 1:sheetWidth + 1, stack = TRUE, gridExpand = TRUE)
  
  # add column titles formatting
  addStyle(wb, sheet, titles_gen, rows = 2:colname_rows[sheet], cols = 2:(sheetWidth + 1), stack = TRUE, gridExpand = TRUE)
  
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

modifyBaseFont(wb, fontSize = 8.5, fontName = "Arial")

# Create contents sheet ------------------
wb <- createWorkbook()
sheet <- 1
# styles
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

descrip <- createStyle(
  fgFill = "#F2F2F2",
  wrapText = TRUE,
  valign = "center")

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

titles <- c(
  paste("eGRID Summary Tables"),
  "Introduction",
  "Table of Contents",
  "Feedback")

title_rows <- c(2, 4, 7, 41)

test <- names(table_data)
remove <- c("\\s*\\([^\\)]+\\)", "[1-4]. ")
for (pattern in remove) {
  test <- lapply(test, str_remove, pattern = pattern)
}
test2 <- matrix(rbind(c("Table", 1:4), c("Description", test)), ncol = 2, byrow = TRUE)
introduction_data <- glue::glue("This document provides eGRID{params$eGRID_year} data summary tables. The tables include subregion and state-level emission rates and resource mix as well as grid gross loss values. Please note that the tables presented here only show a subset of the eGRID2022 data. The entire dataset is in the eGRID{params$eGRID_year} Excel file available on the eGRID website.")
feedback_data <- c("Customer Satisfaction Survey",
                   "Contact EPA")
contents_start <- 3
contents_width <- 11
addWorksheet(wb, sheetName = "Contents")
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")



writeData(wb, sheet, introduction_data, startCol = contents_start + 3, startRow = 4)
addStyle(wb, sheet, descrip, cols = contents_start + 3, rows = 5, stack = TRUE,
         gridExpand = TRUE)
mergeCells(wb, sheet, rows = 5, cols = (contents_start + 3):contents_width)

writeData(wb, sheet, test2, startCol = contents_start, startRow = 7)
addStyle(wb, sheet, descrip, cols = contents_start:4, rows = 8:13, stack = TRUE, 
         gridExpand = TRUE)
for (row in c(8:13, 42:43)) {
  if (row < 42) {
    mergecol <- 4
  } else {
    mergecol <- 3
  }
  mergeCells(wb, sheet, rows = row, cols = mergecol:contents_width)
}
addStyle(wb, sheet, createStyle(textDecoration = "underline"), rows = 8, 
         cols = 3:4, stack = TRUE, gridExpand = TRUE)
addStyle(wb, sheet, createStyle(textDecoration = "underline"), rows = 9:12, 
         cols = 3, stack = TRUE, gridExpand = TRUE)
addStyle(wb, sheet, createStyle(halign = "center"), rows = 8:12, cols = 3,
         stack = TRUE, gridExpand = TRUE)

writeData(wb, sheet, feedback_data, startCol = contents_start, startRow = 42)
addStyle(wb, sheet, createStyle(textDecoration = "underline"), rows = 42:43, 
         cols = 3, stack = TRUE, gridExpand = TRUE)
addStyle(wb, sheet, descrip, cols = contents_start:4, rows = 42:43, stack = TRUE,
         gridExpand = TRUE)

insertImage(wb, sheet, file = , width = 8, height = 6, startRow = , startCol = )
insertImage()

for (i in 1:length(titles)) {
  writeData(wb, sheet, titles[i], startCol = contents_start, startRow = title_rows[i])
  if ( i == 1) {
    addStyle(wb, sheet, title_main, rows = title_rows[i], cols = contents_start)
  } else {
    addStyle(wb, sheet, title_reg, rows = title_rows[i], cols = contents_start)
  }
  mergeCells(wb, sheet, rows = title_rows[i], cols = contents_start:contents_width)
}

worksheetOrder(c())

setRowHeights(wb, sheet, rows = c(), heights = c())
setColWidths(wb, sheet, cols = c(), widths = ())

addStyle(wb, sheet, right_borders, rows = (), cols = ())
addStyle(wb, sheet, top_borders, rows = (), cols = ())
addStyle(wb, sheet, left_borders, rows = (), cols = ())
addStyle(wb, sheet, bottom_borders, rows = (), cols = ())

# add images

# add outlines

saveWorkbook(wb, glue::glue("data/outputs/{params$eGRID_year}/summary_tables_content.xlsx"), overwrite = TRUE)

# Save excel sheet -------------------------------

saveWorkbook(wb, glue::glue("data/outputs/{params$eGRID_year}/summary_tables.xlsx"), overwrite = TRUE)
