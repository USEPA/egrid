## -------------------------------
##
## Final Formatting PM NH3 VOC
## 
## Purpose: 
## 
## This file pulls data from .RDS files to create final version of saved data in excel sheet for PM2.5, NH3, and VOC emisisons data
##
## The resulting output are the following files saved in:
##  data/2a_pm_nh3_voc/outputs/{params$eGRID_year}
##    "eGRID{params$eGRID_year}_pmemissions.xlsx"
##    "eGRID{params$eGRID_year}_nh3emissions.xlsx"
##    "eGRID{params$eGRID_year}_vocemissions.xlsx"
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries --------
library(dplyr)
library(ggbreak)
library(ggplot2)
library(openxlsx)
library(readr)
library(readxl)
library(scales)
library(stringr)

# Define eGRID year parameter ----------------
# define parameter year if no one is currently assigned using prompted user input
if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.")
  } else { # if params() is defined, but eGRID_year is not, define it here
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year)
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Load functions -----
source("scripts/functions/function_formatting_pm_nh3_voc.R")
source("scripts/functions/function_create_subregion_figures_pm_nh3_voc.R")

# Define selection of style types -----
headers_long <- createStyle(
  fontName = "Arial",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  fgFill = "#F2F2F2",
  border = "TopBottomLeftRight",
  borderColour = "black",
  borderStyle = "thin")

headers_abbrev <- createStyle(
  fontName = "Arial",
  textDecoration = "bold",
  halign = "left",
  valign = "bottom",
  fgFill = "#F2F2F2",
  border = "TopBottomLeftRight",
  borderColour = "black",
  borderStyle = "thin")

us_row <- createStyle(
  fontName = "Arial",
  textDecoration = "bold",
  fgFill = "#F2F2F2")

graph_headers <- createStyle(
  fontName = "Arial",
  fontSize = 16,
  textDecoration = "bold",
  halign = "center",
  valign = "center")

# Load abbreviated name to snake_case matches -----
base::load("data/1_production_model/static_tables/name_matches.Rdata")

# define level abbreviations to assist in renaming
level_abbrev <- c("unit" = "",
                  "plant" = "PL",
                  "state" = "ST",
                  "subregion" = "SR")

# Define URLs for previous data -----
wb_urls <- c(
  "pm" = "https://www.epa.gov/system/files/documents/2024-06/egrid-draft-pm-emissions.xlsx",
  "nh3" = "https://www.epa.gov/system/files/documents/2024-06/egrid2021-draft-nh3-emissions.xlsx",
  "voc" = "https://www.epa.gov/system/files/documents/2024-06/egrid2021-draft-voc-emissions.xlsx")

# Loop through emission types -----
for (emission_type in c("pm", "nh3", "voc")) {
    # assign emission type formatting for headers
  if (emission_type == "pm") {
    emission_header <- "PM2.5"
    emission_label <- "pm25"
  } else {
    emission_header <- toupper(emission_type)
    emission_label <- emission_type
  }
  
  # Import .RDS data -----
  
  # create a list of files in R directory
  data_dir <- glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/")
  filename_types <- list("unit_file_",
                         "plant_file_",
                         "state_aggregation_",
                         "subregion_aggregation_",
                         "us_aggregation_")
  filenames <- lapply(filename_types, function(filename) paste0(filename, paste0(emission_type, ".RDS")))
  
  # import files in filenames list
  for (file in (filenames)){
    if (grepl("_aggregation", file)) {
      assign(str_replace(file, glue::glue("aggregation_{emission_type}.RDS"), "file"), read_rds(paste0(data_dir, file)))
    } else {
    assign(str_remove(file, glue::glue("_{emission_type}.RDS")), read_rds(paste0(data_dir, file)))
    }
  }
  
  # Select variables to be included in final version -----
  unit_file <-
    unit_file %>%
    # remove unadjusted PM2.5 data (used in plant aggregation)
    select(-glue::glue("{emission_label}"))
  
  plant_file <-
    plant_file %>%
    # remove unadjusted annual PM2.5 data (used in regional aggregation)
    select(-glue::glue("{emission_label}_ann_orig"))
  
  # Add US data to bottom of subregion data -----
  us_formatted <-
    us_file %>%
    mutate(subregion = "U.S.", 
           subregion_name = "") %>%
    rename(subregion_generation_ann = generation_ann)
  
  subregion_file <-
    subregion_file %>%
    bind_rows(us_formatted)
  
  # Load in previous year workbook -----
  
  ## Download previous years' data ----
  
  # assign previous year number
  year_prev <- as.numeric(params$eGRID_year) - 1
  # set download url as that assigned by emission type
  wb_url <- wb_urls[emission_type]
  # set directory to store previous year's file
  wb_dir <- glue::glue("data/2a_pm_nh3_voc/inputs/pm_nh3_voc_historic/{year_prev}/")
  # set name of previous year's file
  wb_name <- glue::glue("{wb_dir}eGRID{year_prev}_{emission_type}_emissions.xlsx")
  
  # check for presence of directories and create if doesn't exist
  if (!dir.exists(wb_dir)) {
    dir.create(wb_dir, recursive = TRUE)
  }
  
  # download previous year's data and save in desired folder
  download.file(url = wb_url, destfile = wb_name, mode = "wb")
  
  # Load workbook -----
  wb <- loadWorkbook(wb_name)
  
  # set base font
  modifyBaseFont(wb, fontName = "Arial", fontSize = 8.5)
  
  # Loop through emission levels and write and format data -----
  for(emission_level in c("unit", "plant", "state", "subregion")) {
    
    ## Rename and format level data -----
    
    # define names of name match and emissions data variables
    name_matches <- paste0(emission_level, "_nonmetric")
    emission_data <- paste0(emission_level, "_file")
    
    # match additional column names present in emissions data not in name_matches
    colnames_new <- setNames(c(paste0(emission_label, "_ann"),
                               paste0(emission_label, "_output_rate"),
                               paste0(emission_label, "_input_rate"),
                               paste0(emission_label, "_rate"), 
                               paste0(emission_label, "_source"),
                               paste0("unadj_", emission_label)),
                             c(paste0(level_abbrev[[emission_level]], toupper(emission_label), "AN"), 
                               paste0(level_abbrev[[emission_level]], toupper(emission_label), "RTA"),
                               paste0(level_abbrev[[emission_level]], toupper(emission_label), "RA"),
                               paste0(level_abbrev[[emission_level]], toupper(emission_label), "RT"), 
                               paste0(toupper(emission_label), "SRC"),
                               paste0("UN", toupper(emission_label))))
  
    # select name matches and new column names present in emissions data
    colnames <- c(get(name_matches)[get(name_matches) %in% colnames(get(emission_data))],
                  colnames_new[colnames_new %in% colnames(get(emission_data))])
    
    # rename emission data column names
    emission_data_formatted <-
      get(emission_data) %>%
      rename(!!!setNames(lapply(colnames, sym), names(colnames)))
    
    ## Gather header names from function -----
    headers <- format_headers_pm_nh3_voc(emission_level)
    names(headers) <- colnames(emission_data_formatted)
    headers_longform <- matrix(unname(headers), ncol = length(headers))
    headers_shortform <- matrix(names(headers), ncol = length(headers))
    
    ## Update formatting errors in previous data ----
    update_wb_formatting_pm_nh3_voc(wb, emission_type)
  
    ## Create new worksheet for emissions level -----
  
    current_worksheet <- glue::glue("{params$eGRID_year} {toupper(emission_type)} {str_to_title(emission_level)}-level Data")
    addWorksheet(wb, sheetName = current_worksheet)
    
    ## Write data to new worksheet -----
    
    sheetWidth <- length(emission_data_formatted)
    sheetLength <- nrow(emission_data_formatted) + 2
    
    # full description headers
    writeData(wb, current_worksheet, headers_longform, startCol = 1, startRow = 1, colNames = FALSE)
    
    # emissions data
    writeData(wb, current_worksheet,emission_data_formatted, startCol = 1, startRow = 2)
  
    ## Format worksheet -----
  
    ### Headers -----
    
    # assign column type for color-coding based on variable naming
    if (emission_level == "unit") {
      annual_cols <- c()
      unadj_annual_cols <- which(grepl("AN$|RT$|SRC$", colnames(emission_data_formatted)))
    } else {
      annual_cols <- which(grepl("AN$", colnames(emission_data_formatted)))
      unadj_annual_cols <- which(grepl("^UN", colnames(emission_data_formatted)))
    }
    output_rate_cols <- which(grepl("RTA$", colnames(emission_data_formatted)))
    input_rate_cols <- which(grepl("RA$", colnames(emission_data_formatted)))
    
    # grey headers
    addStyle(wb, current_worksheet, headers_long, rows = 1, cols = 1:sheetWidth, 
             stack = TRUE, gridExpand = TRUE)
    addStyle(wb, current_worksheet, headers_abbrev, rows = 2, cols = 1:sheetWidth, 
             stack = TRUE, gridExpand = TRUE)
    
    # color-coded headers
    addStyle(wb, current_worksheet, createStyle(fgFill = "#F2DCDB"), rows = 1:2, 
             cols = annual_cols, stack = TRUE, gridExpand = TRUE)
    addStyle(wb, current_worksheet, createStyle(fgFill = "#E6B8B7"), rows = 1:2, 
             cols = unadj_annual_cols, stack = TRUE, gridExpand = TRUE)
    addStyle(wb, current_worksheet, createStyle(fgFill = "#EBF1DE"), rows = 1:2, 
             cols = output_rate_cols, stack = TRUE, gridExpand = TRUE)
    addStyle(wb, current_worksheet, createStyle(fgFill = "#C4D79B"), rows = 1:2, 
             cols = input_rate_cols, stack = TRUE, gridExpand = TRUE)
    
    ### Cell sizes ------
    setRowHeights(wb, current_worksheet, rows = 1, heights = 54)
    setRowHeights(wb, current_worksheet, rows = 2:sheetLength, heights = 13.5)
    setColWidths(wb, current_worksheet, cols = c(1:sheetWidth), widths = 13.5)
    setColWidths(wb, current_worksheet, cols = which(grepl("NAME$", colnames(emission_data_formatted))), widths = 38.25)
    setColWidths(wb, current_worksheet, cols = which(grepl("^SRNAME$", colnames(emission_data_formatted))), widths = 24)
    setColWidths(wb, current_worksheet, cols = which(grepl("SRC$", colnames(emission_data_formatted))), widths = 30)
    
    ### Freeze pane -----
    if (emission_level == "unit") {
      freezePane(wb, current_worksheet, firstActiveCol = 6, firstActiveRow = 3)
    } else if (emission_level == "plant") {
      freezePane(wb, current_worksheet, firstActiveCol = 5, firstActiveRow = 3)
    } else {
      freezePane(wb, current_worksheet, firstActiveRow = 3)
    }
    
    ### US data row -----
    if (emission_level == "subregion") {
      addStyle(wb, current_worksheet, us_row, rows = sheetLength, cols = 1:sheetWidth)
    }
  
    ### Number formats -----
    
    # generation annual, heat annual, operating hours, unadjusted heat input, state annual emissions, subregion annual emissions
    addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0"), rows = 3:sheetLength,
             cols = which(grepl(glue::glue("GENAN$|HTIAN$|^HRSOP$|UNHTI$|^ST{toupper(emission_label)}AN$|^SR{toupper(emission_label)}AN$"), colnames(emission_data_formatted))),
             stack = TRUE, gridExpand = TRUE)
    # nameplate capacity
    addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.0"), rows = 3:sheetLength,
             cols = which(grepl("^NAMEPCAP$", colnames(emission_data_formatted))),
             stack = TRUE, gridExpand = TRUE)
    # unit annual emission values, plant annual emission values, unadjusted annual rates
    addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.000"), rows = 3:sheetLength,
             cols = which(grepl(glue::glue("^{toupper(emission_label)}AN|^PL{toupper(emission_label)}AN|^UN{toupper(emission_label)}"), colnames(emission_data_formatted))),
             stack = TRUE, gridExpand = TRUE)
    # emission rates 
    addStyle(wb, current_worksheet, createStyle(numFmt = "#,##0.0000"), rows = 3:sheetLength,
             cols = which(grepl("RT$|RTA$|RA$|^ELCALLOC$", colnames(emission_data_formatted))), 
             stack = TRUE, gridExpand = TRUE)
  }
  
  # Add subregion emissions graphs  -----
  
  # reset graphs worksheet by removing and adding new sheet
  removeWorksheet(wb, "Graphs")
  addWorksheet(wb, "Graphs")
  
  # run script to save subregion graphs
  create_subregion_emission_figures(wb, emission_type)
  
  # define graph directory, names, and years
  graph_dir <- glue::glue("data/2a_pm_nh3_voc/static_tables/formatting/")
  graph_types <- c("annual_generation", "emissions", "rate")
  graph_years <- seq(2018, as.numeric(params$eGRID_year), 1)
  
  # set graph starting location and location intervals
  graph_col <- 1
  graph_col_step <- 12
  graph_row_step <- 20
  
  # loop through years of data
  for(year in graph_years) {
    # add year label and formatting
    graph_row <- 2
    writeData(wb, "Graphs", x = year, startCol = graph_col, startRow = graph_row - 1)
    mergeCells(wb, "Graphs", rows = 1, cols = graph_col:(graph_col + graph_col_step - 1))
    # add images
    for(graph in graph_types) {
      graph_file <- glue::glue("{graph_dir}{emission_type}_{graph}_{year}.png")
      insertImage(wb, "Graphs", file = graph_file,
              width = 8.5, height = 3.2, startRow = graph_row, startCol = graph_col)
      # shift image location down
      graph_row <- graph_row + graph_row_step }
    # shift image location right
    graph_col <- graph_col + graph_col_step }
  
  # add styling to year headers
  addStyle(wb, "Graphs", style = graph_headers, rows = 1, cols = 1:(graph_row_step * length(graph_years)), gridExpand = TRUE)
  # set row heights
  setRowHeights(wb, "Graphs", rows = 2:100, heights = 13.5)
  
  # Add eGRID subregion map -----
  map_row <- graph_row + 1
  map_file <- "data/1_production_model/static_tables/formatting/eGRID_subregions.png"
  insertImage(wb, "Graphs", file = map_file,
              width = 8.5, height = 6.42, startRow = map_row, startCol = 1)
  
  # Order worksheets and save workbook -----
  
  # order worksheets - move graphs and EIA crosswalk to the end
  wb_order <- worksheetOrder(wb)
  wb_new_order <- c(wb_order[1:(length(wb_order) - 6)], wb_order[(length(wb_order) - 4):(length(wb_order) - 1)], tail(wb_order, n = 1), wb_order[(length(wb_order) - 5)])
  worksheetOrder(wb) <-wb_new_order
  
  # save workbook
  saveWorkbook(wb, glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_type}emissions.xlsx"), 
               overwrite = TRUE)
}