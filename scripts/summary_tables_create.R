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
library(readxl)
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
              select(!contains("nonbaseload")))  

# Compile and format state resource mix data for TABLE 4  ----------------------

# state resource mix
state_resource_mix <-
  state_aggregation %>%
  select(state, state_nameplate_capacity, state_generation_ann, 
         any_of(paste0("state_ann_resource_mix_", resource_type))) %>%
  rename_with(~str_c(str_remove(., "state_"))) %>%
  # add US data to bottom row
  bind_rows(us_resource_mix)  

# Format as excel sheet ---------------------------

# styles
toptitle <- createStyle(fontSize = 12, 
                        halign = "center", 
                        valign = "center", 
                        wrapText = TRUE, 
                        textDecoration = "bold", 
                        border = "TopLeftRight", 
                        borderColour = "black", 
                        borderStyle = "thick")

grey_subtitle <- createStyle(fgFill = "#F2F2F2", 
                             halign = "center", 
                             valign = "center",
                             wrapText = TRUE, 
                             textDecoration = "bold",
                             border = "TopBottomLeftRight", 
                             borderColour = "black",
                             borderStyle = "thin")

green_subtitle <- createStyle(fgFill = "#ebf1de", 
                              halign = "center", 
                              valign = "center", 
                              wrapText = TRUE, 
                              textDecoration = "bold", 
                              border = "TopBottomLeftRight", 
                              borderColour = "black", 
                              borderStyle = "thin")

green_subtitle_topborder <- createStyle(fgFill = "#ebf1de", 
                                        halign = "center", 
                                        valign = "center", 
                                        wrapText = TRUE, 
                                        textDecoration = "bold",
                                        border = "Top", 
                                        borderColour = "black", 
                                        borderStyle = "thin")

green_subtitle_noborder <- createStyle(fgFill = "#ebf1de", 
                                       halign = "center", 
                                       valign = "center", 
                                       wrapText = TRUE, 
                                       textDecoration = "bold")

purple_subtitle <- createStyle(fgFill = "#e4dfec", 
                               halign = "center", 
                               valign = "center", 
                               wrapText = TRUE, 
                               textDecoration = "bold", 
                               border = "TopBottomLeftRight", 
                               borderColour = "black",
                               borderStyle = "thin")

purple_subtitle_topborder <- createStyle(fgFill = "#e4dfec", 
                                         halign = "center", 
                                         valign = "center", 
                                         wrapText = TRUE, 
                                         textDecoration = "bold",
                                         border = "TopLeft", 
                                         borderColour = "black", 
                                         borderStyle = "thin")

purple_subtitle_leftborder <- createStyle(fgFill = "#e4dfec", 
                                          halign = "center", 
                                          valign = "center", 
                                          wrapText = TRUE, 
                                          textDecoration = "bold",
                                          border = "Left", 
                                          borderColour = "black", 
                                          borderStyle = "thin")

red_subtitle <- createStyle(fgFill = "#f2dcdb", 
                              halign = "center", 
                              valign = "center", 
                              wrapText = TRUE, 
                              textDecoration = "bold", 
                              border = "TopBottomLeftRight", 
                              borderColour = "black", 
                              borderStyle = "thin")

orange_subtitle <- createStyle(fgFill = "#fde9d9", 
                              halign = "center", 
                              valign = "center", 
                              wrapText = TRUE, 
                              textDecoration = "bold", 
                              border = "TopBottomLeftRight", 
                              borderColour = "black", 
                              borderStyle = "thin")

us_smalltitle <- createStyle(textDecoration = "bold",
                             border = "Top",
                             borderStyle = "thick",
                             borderColour = "black")

largenum <- createStyle(numFmt = "#,##0.0",  
                        border = "TopBottomLeftRight", 
                        borderStyle = "thin", 
                        borderColour = "black")

smallnum <- createStyle(numFmt = "#,##0.000",  
                        border = "TopBottomLeftRight", 
                        borderStyle = "thin", 
                        borderColour = "black")

percentnum <- createStyle(numFmt = "#,##0.0%",  
                          border = "TopBottomLeftRight", 
                          borderStyle = "thin", 
                          borderColour = "black")

nodecimnum <- createStyle(numFmt = "#,##0",  
                          border = "TopBottomLeftRight", 
                          borderStyle = "thin", 
                          borderColour = "black")

us_largenum <- createStyle(numFmt = "#,##0.0",
                           textDecoration = "bold",
                           border = "TopBottom", 
                           borderStyle = "thick", 
                           borderColour = "black")

us_smallnum <- createStyle(numFmt = "#,##0.000",
                           textDecoration = "bold",
                           border = "TopBottom", 
                           borderStyle = "thick", 
                           borderColour = "black")

us_percentnum <- createStyle(numFmt = "#,##0.0%",  
                             textDecoration = "bold",
                             border = "TopBottom", 
                             borderStyle = "thick", 
                             borderColour = "black")

us_nodecimnum <- createStyle(numFmt = "#,##0",  
                             textDecoration = "bold",
                             border = "TopBottom", 
                             borderStyle = "thick", 
                             borderColour = "black")

#red_bf <- createStyle(fgFill = "#F2F2F2", halign = "center", valign = "center", wrapText = TRUE, textDecoration = "bold")
#orange_bf <- createStyle(fgFill = "#F2F2F2", halign = "center", valign = "center", wrapText = TRUE, textDecoration = "bold")

# create workbook
wb <- createWorkbook()
startcol <- 2
modifyBaseFont(wb, fontSize = 8.5, fontName = "Arial")

table_data <- list(subregion_output_emissions,
                   subregion_resource_mix,
                   state_output_emissions,
                   state_resource_mix)

names(table_data) <- c(
  glue::glue("1. Subregion Output Emission Rates (eGRID{params$eGRID_year})"),
  glue::glue("2. Subregion Resource Mix (eGRID{params$eGRID_year})"),
  glue::glue("3. State Output Emission Rates (eGRID{params$eGRID_year})"),
  glue::glue("4. State Resource Mix (eGRID{params$eGRID_year})")
)

# SHEET 1 
sheet <- 1
addWorksheet(wb, sheetName = paste0("Table ",sheet))
sheetWidth <- as.numeric(ncol(table_data[[sheet]]))
sheetLength <- nrow(table_data[[sheet]]) + 4

colnames_lower <- matrix(c(
  "eGRID subregion acronym",
  "eGRID subregion name",
  "CO2",
  "CH4",
  "N2O",
  "CO2E",
  "Annual NOx",
  "Ozone Season NOx",
  "SO2",
  "CO2",
  "CH4",
  "N2O",
  "CO2E",
  "Annual NOx",
  "Ozone Season NOx",
  "SO2",
  "Grid Gross Loss (%)"), ncol = sheetWidth)

colnames_upper <- colnames_lower
colnames_upper[3] <- "Total output emission rates"
colnames_upper[10] <-  "Non-baseload output emission rates"

colnames_units <- rbind(colnames_upper, 
                         matrix(c("", "", "lb/MWh", "", "", "", "", "", "",
                                  "lb/MWh", "", "", "", "", "", "", ""), 
                                ncol = sheetWidth))
# add data
writeData(wb, sheet, colnames_lower, startCol = startcol, startRow = 3)
writeData(wb, sheet, colnames_units, startCol = startcol, startRow = 1)
writeData(wb, sheet, names(table_data[sheet]), startCol = startcol, startRow = 1)
writeData(wb, sheet, table_data[[sheet]], startCol = startcol, startRow = 5, 
          borders = "all", colNames = FALSE)
writeData(wb, sheet, table_data[[sheet]] %>% 
            filter(subregion == "U.S."), startCol = startcol, startRow = sheetLength, 
          borders = "surrounding", borderStyle = "thick", colNames = FALSE)

# merge cells
mergeCells(wb, sheet, cols = 2:18, rows = 1)
for (colnum in c(2, 3, 18)) {
  mergeCells(wb, sheet, cols = colnum, rows = 2:4)
}

for (colnum in c(4, 11)) {
  for (rownum in c(2, 3)) {
    mergeCells(wb, sheet, cols = c(colnum, colnum + 6), rows = rownum)
}
}

# set cell sizes
setRowHeights(wb, sheet, rows = 1:sheetLength, heights = 15)
setRowHeights(wb, sheet, rows = 1, heights = 21.75)
setColWidths(wb, sheet, cols = 1, width = 1)
setColWidths(wb, sheet, cols = 2, widths = 8.14)
setColWidths(wb, sheet, cols = 3, widths = 16.57)
setColWidths(wb, sheet, cols = 4:17, widths = 6.86)
setColWidths(wb, sheet, cols = 18, widths = 7.14)

# add formatting
# titles
addStyle(wb, sheet, toptitle, rows = 1, cols = 2:sheetWidth, gridExpand = TRUE)
addStyle(wb, sheet, grey_subtitle, rows = 2:4, cols = c(2:3, 18), gridExpand = TRUE)
addStyle(wb, sheet, green_subtitle, rows = 4, cols = 4:10, gridExpand = TRUE)
addStyle(wb, sheet, green_subtitle_topborder, rows = 2, cols = 4:10, gridExpand = TRUE)
addStyle(wb, sheet, green_subtitle_noborder, rows = 3, cols = 4:10, gridExpand = TRUE)
addStyle(wb, sheet, purple_subtitle, rows = 4, cols = 11:17, gridExpand = TRUE)
addStyle(wb, sheet, purple_subtitle_topborder, rows = 2, cols = 11:17, gridExpand = TRUE)
addStyle(wb, sheet, purple_subtitle_leftborder, rows = 3, cols = 11:17, gridExpand = TRUE)
addStyle(wb, sheet, us_smalltitle, rows = sheetLength, cols = 2, gridExpand = TRUE)

# data
addStyle(wb, sheet, largenum, rows = 5:30, cols = c(4, 7:9, 11, 14:16), gridExpand = TRUE)
addStyle(wb, sheet, smallnum, rows = 5:30, cols = c(5:6, 10, 12:13, 17), gridExpand = TRUE)
addStyle(wb, sheet, percentnum, rows = 5:30, cols = 18, gridExpand = TRUE)
addStyle(wb, sheet, us_largenum, rows = 31, cols = c(4, 7:9, 11, 14:16), gridExpand = TRUE)
addStyle(wb, sheet, us_smallnum, rows = 31, cols = c(5:6, 10, 12:13, 17), gridExpand = TRUE)
addStyle(wb, sheet, us_percentnum, rows = 31, cols = 18, gridExpand = TRUE)

# outer borders
addStyle(wb, sheet, createStyle(border = "Left", borderColour = "black", borderStyle = "thick"),
         rows = 1:sheetLength, cols = 19, gridExpand = TRUE)
addStyle(wb, sheet, createStyle(border = "Right", borderColour = "black", borderStyle = "thick"),
         rows = 1:sheetLength, cols = 1, gridExpand = TRUE)
addStyle(wb, sheet, createStyle(border = "Top", borderColour = "black", borderStyle = "thick"),
         rows = sheetLength + 1, cols = 2:18, gridExpand = TRUE)


# SHEET 2

sheet <- 2 # CHANGED
addWorksheet(wb, sheetName = paste0("Table ",sheet))
sheetWidth <- as.numeric(ncol(table_data[[sheet]]))
sheetLength <- nrow(table_data[[sheet]]) + 3 # CHANGED

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

colnames_upper <- colnames_lower
colnames_upper[5] <- "Generation Resource Mix (percent)*"

# add data
writeData(wb, sheet, colnames_lower, startCol = startcol, startRow = 2)
writeData(wb, sheet, colnames_upper, startCol = startcol, startRow = 1)
writeData(wb, sheet, names(table_data[sheet]), startCol = startcol, startRow = 1)
writeData(wb, sheet, table_data[[sheet]], startCol = startcol, startRow = 4, 
          borders = "all", colNames = FALSE)
writeData(wb, sheet, table_data[[sheet]] %>% 
            filter(subregion == "U.S."), startCol = startcol, startRow = sheetLength, 
          borders = "surrounding", borderStyle = "thick", colNames = FALSE)

# merge cells
mergeCells(wb, sheet, cols = 2:16, rows = 1)
mergeCells(wb, sheet, cols = 6:16, rows = 2)
for (colnum in 2:5) {
  mergeCells(wb, sheet, cols = colnum, rows = 2:3)
}

# set cell sizes
setRowHeights(wb, sheet, rows = 1:sheetLength, heights = 15)
setRowHeights(wb, sheet, rows = 1, heights = 21.75)
setColWidths(wb, sheet, cols = 1, width = 1)
setColWidths(wb, sheet, cols = c(2, 4), widths = 8.43)
setColWidths(wb, sheet, cols = 3, widths = 20.29)
setColWidths(wb, sheet, cols = 5, widths = 10.14)
setColWidths(wb, sheet, cols = 6:11, widths = 6)
setColWidths(wb, sheet, cols = 12, widths = 6.86)
setColWidths(wb, sheet, cols = 13:14, widths = 6)
setColWidths(wb, sheet, cols = 15, widths = 6.57)
setColWidths(wb, sheet, cols = 16, widths = 9)

# add formatting
# titles
addStyle(wb, sheet, toptitle, rows = 1, cols = 2:sheetWidth, gridExpand = TRUE)
addStyle(wb, sheet, grey_subtitle, rows = 2:3, cols = 2:4, gridExpand = TRUE)
addStyle(wb, sheet, red_subtitle, rows = 2:3, cols = 5, gridExpand = TRUE)
addStyle(wb, sheet, orange_subtitle, rows = 2:3, cols = 6:17, gridExpand = TRUE)
# addStyle(wb, sheet, green_subtitle_noborder, rows = 3, cols = 4:10, gridExpand = TRUE)
# addStyle(wb, sheet, purple_subtitle, rows = 4, cols = 11:17, gridExpand = TRUE)
# addStyle(wb, sheet, purple_subtitle_topborder, rows = 2, cols = 11:17, gridExpand = TRUE)
# addStyle(wb, sheet, purple_subtitle_leftborder, rows = 3, cols = 11:17, gridExpand = TRUE)
addStyle(wb, sheet, us_smalltitle, rows = sheetLength, cols = 2, gridExpand = TRUE)

# data
addStyle(wb, sheet, nodecimnum, rows = 5:sheetLength - 1, cols = 4:5, gridExpand = TRUE)
addStyle(wb, sheet, percentnum, rows = 5:sheetLength, cols = 6:16, gridExpand = TRUE)
addStyle(wb, sheet, us_nodecimnum, rows = sheetLength, cols = 4:5, gridExpand = TRUE)
addStyle(wb, sheet, us_percentnum, rows = sheetLength, cols = 6:16, gridExpand = TRUE)

# outer borders
addStyle(wb, sheet, createStyle(border = "Left", borderColour = "black", borderStyle = "thick"),
         rows = 1:sheetLength, cols = sheetWidth + 2, gridExpand = TRUE)
addStyle(wb, sheet, createStyle(border = "Right", borderColour = "black", borderStyle = "thick"),
         rows = 1:sheetLength, cols = 1, gridExpand = TRUE)
addStyle(wb, sheet, createStyle(border = "Top", borderColour = "black", borderStyle = "thick"),
         rows = sheetLength + 1, cols = 2:sheetWidth, gridExpand = TRUE)

saveWorkbook(wb, "data/outputs/2023/summary_tables.xlsx", overwrite = TRUE)


# Save -------------------------------

