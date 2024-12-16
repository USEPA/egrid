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

# US output aemissions data
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
  bind_rows(us_resource_mix)  


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

table_data <- list("Table 1" = subregion_output_emissions, 
             "Table 2" = subregion_resource_mix, 
             "Table 3" = state_output_emissions, 
             "Table 4" = state_resource_mix)

table_title <- c(
  glue::glue("1. Subregion Output Emission Rates (eGRID{params$eGRID_year})"),
  glue::glue("2. Subregion Resource Mix (eGRID{params$eGRID_year})"),
  glue::glue("3. State Output Emission Rates (eGRID{params$eGRID_year})"),
  glue::glue("4. State Resource Mix (eGRID{params$eGRID_year})")
)


colvars <- matrix(c("", "", "lb/MWh", "", "", "", "", "", "", "lb/MWh", "", "", 
                    "", "", "", "", ""), ncol = 17)

colnames(colvars) <- c(
  "eGRID subregion acronym",
  "eGRID subregion name",
  "Total output emission rates",
  "", "", "", "", "", "",
  "Non-baseload output emission rates",
  "", "", "", "", "", "",
  "Grid Gross Loss (%)")

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
  "Grid Gross Loss (%)"), ncol = 17)


# create workbook
wb <- createWorkbook()
startcol <- 2

# set overall formats
modifyBaseFont(wb, fontSize = 8.5, fontName = "Arial")

# SHEET 1 
sheet <- 1
addWorksheet(wb, sheetName = names(table_data[sheet]))
writeData(wb, sheet = sheet, x = colnames_lower, startCol = startcol, startRow = 3)
writeData(wb, sheet = sheet, x = colvars, startCol = startcol, startRow = 2)
writeData(wb, sheet = sheet, x = table_title[sheet], startCol = startcol, startRow = 1)

mergeCells(wb, sheet = sheet, cols = 2:18, rows = 1)
for (colnum in c(2, 3, 18)) {
  mergeCells(wb, sheet = sheet, cols = colnum, rows = 2:4)
}
for (colnum in c(4, 11)) {
  for (rownum in c(2, 3)) {
    mergeCells(wb, sheet = sheet, cols = c(colnum, colnum + 6), rows = rownum)
}
}

writeData(wb, sheet = sheet, x = subregion_output_emissions %>% filter(subregion != "U.S."), startCol = startcol, startRow = 5, borders = "all", colNames = FALSE)
writeData(wb, sheet = sheet, x = subregion_output_emissions %>% filter(subregion == "U.S."), startCol = startcol, startRow = 31, borders = "surrounding", borderStyle = "thick", colNames = FALSE)

# format worksheet 1
setRowHeights(wb, sheet = sheet, rows = seq_len(nrow(table_data[[sheet]])), heights = 15)
setRowHeights(wb, sheet = sheet, rows = 1, heights = 21.75)
setColWidths(wb, sheet = sheet, cols = 1, width = 1)
setColWidths(wb, sheet = sheet, cols = 2, widths = 8.14)
setColWidths(wb, sheet = sheet, cols = 3, widths = 16.57)
setColWidths(wb, sheet = sheet, cols = 4:17, widths = 6.86)
setColWidths(wb, sheet = sheet, cols = 18, widths = 7.14)

title_style <- createStyle(fontSize = 12, halign = "center", valign = "center", 
                           wrapText = TRUE, textDecoration = "bold", 
                           border = "TopLeftRight", borderColour = "black", 
                           borderStyle = "thick")
grey_bf <- createStyle(fgFill = "#F2F2F2", halign = "center", valign = "center",
                       wrapText = TRUE, textDecoration = "bold", 
                       border = "TopBottomLeftRight", borderColour = "black",
                       borderStyle = "thin")
green_bf <- createStyle(fgFill = "#ebf1de", halign = "center", valign = "center", 
                        wrapText = TRUE, textDecoration = "bold", 
                        border = "TopBottomLeftRight", borderColour = "black", 
                        borderStyle = "thin")
green_bf_topborder <- createStyle(fgFill = "#ebf1de", halign = "center", valign = "center", 
                        wrapText = TRUE, textDecoration = "bold",
                        border = "Top", borderColour = "black", 
                        borderStyle = "thin")
green_bf_noborder <- createStyle(fgFill = "#ebf1de", halign = "center", valign = "center", 
                                 wrapText = TRUE, textDecoration = "bold")
purple_bf <- createStyle(fgFill = "#e4dfec", halign = "center", valign = "center", 
                         wrapText = TRUE, textDecoration = "bold", 
                         border = "TopBottomLeftRight", borderColour = "black",
                         borderStyle = "thin")
purple_bf_topborder <- createStyle(fgFill = "#e4dfec", halign = "center", valign = "center", 
                         wrapText = TRUE, textDecoration = "bold",
                         border = "TopLeft", borderColour = "black", 
                         borderStyle = "thin")
purple_bf_noborder <- createStyle(fgFill = "#e4dfec", halign = "center", valign = "center", 
                             wrapText = TRUE, textDecoration = "bold",
                             border = "Left", borderColour = "black", 
                             borderStyle = "thin")
#red_bf <- createStyle(fgFill = "#F2F2F2", halign = "center", valign = "center", wrapText = TRUE, textDecoration = "bold")
#orange_bf <- createStyle(fgFill = "#F2F2F2", halign = "center", valign = "center", wrapText = TRUE, textDecoration = "bold")

addStyle(wb, sheet = sheet, title_style, rows = 1, cols = 2:18, gridExpand = TRUE)
addStyle(wb, sheet = sheet, grey_bf, rows = 2:4, cols = c(2:3, 18), gridExpand = TRUE)
addStyle(wb, sheet = sheet, green_bf, rows = 4, cols = 4:10, gridExpand = TRUE)
addStyle(wb, sheet = sheet, green_bf_topborder, rows = 2, cols = 4:10, gridExpand = TRUE)
addStyle(wb, sheet = sheet, green_bf_noborder, rows = 3, cols = 4:10, gridExpand = TRUE)
addStyle(wb, sheet = sheet, purple_bf, rows = 4, cols = 11:17, gridExpand = TRUE)
addStyle(wb, sheet = sheet, purple_bf_topborder, rows = 2, cols = 11:17, gridExpand = TRUE)
addStyle(wb, sheet = sheet, purple_bf_noborder, rows = 3, cols = 11:17, gridExpand = TRUE)
addStyle(wb, sheet = sheet, createStyle(border = "Left", borderColour = "black", borderStyle = "thick"),
         rows = 1:31, cols = 19)
addStyle(wb, sheet = sheet, createStyle(border = "Right", borderColour = "black", borderStyle = "thick"),
         rows = 1:31, cols = 1)
addStyle(wb, sheet = sheet, createStyle(border = "Top", borderColour = "black", borderStyle = "thick"),
         rows = 32, cols = 2:18)
#addStyle(wb, sheet = sheet, createStyle(border = ))

saveWorkbook(wb, "data/outputs/2023/test.xlsx", overwrite = TRUE)

# Save -------------------------------

