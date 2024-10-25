## -------------------------------
##
## Final formatting
## 
## Purpose: 
## 
## This file formats all of the outputs created for eGRID 
## This includes all operating units for the specified eGRID data year
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------

# set working directory - remove later
setwd("~/sdrive/projects/eGrid/production_model/users/zhangm/egrid-git")

### Load libraries ------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)

### Load in data ------ 
unit_file <- readRDS('data/outputs/unit_file.RDS')
generator_file <- readRDS('data/outputs/generator_file.RDS')
plant_file <- readRDS('data/outputs/plant_file.RDS')
ggl_file <- readRDS('data/outputs/egrid_ggl_final.RDS') # maybe change file name to match others


# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

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


# extract last two digits of year for universal labeling
year <- as.numeric(params$eGRID_year) %% 1000

### Set up output file
contents <- "data/outputs/formatting/egrid_contents_page.xlsx"
wb <- loadWorkbook(contents)


### Create styles ------
# extract base style
default_style <- getStyles(wb)
  
# header style
header_style <- createStyle(textDecoration = "bold",
                            fgFill = "#F2F2F2", 
                            wrapText = TRUE,
                            fontName = "Arial",
                            fontSize = 8.5,
                            border = "TopBottomLeftRight",
                            borderStyle = "thin")
# description style
desc_style <- createStyle(wrapText = TRUE,
                          halign = "center",
                          valign = "center",
                          textDecoration = "bold",
                          fgFill = "#F2F2F2", 
                          fontName = "Arial",
                          fontSize = 8.5,
                          border = "TopBottomLeftRight",
                          borderStyle = "thin")

# bold style (for text/characters)
bold <- createStyle(fontName = "Arial",
                    fontSize = 8.5,
                    textDecoration = "bold")

# basic style (for text/characters)
basic <- createStyle(fontName = "Arial",
                     fontSize = 8.5)

# basic style (for larger integers)
integer <- createStyle(numFmt = "#,##0",
                       fontName = "Arial",
                       fontSize = 8.5)

# basic style (for percentages with 1 decimal place)
percent <- createStyle(numFmt = "0.0%",
                       fontName = "Arial",
                       fontSize = 8.5)

# bold style (for large integers)
integer_bold <- createStyle(numFmt = "#,##0",
                            fontName = "Arial",
                            fontSize = 8.5,
                            textDecoration = "bold")

# bold style (for percentages with 1 decimal place)
percent_bold <- createStyle(numFmt = "0.0%",
                            fontName = "Arial",
                            fontSize = 8.5,
                            textDecoration = "bold")

# number <- createStyle(numFmt = "0")

### GGL Formatting -----
ggl <- glue::glue("GGL{year}")
addWorksheet(wb, ggl)
ggl_output <- readRDS('data/outputs/egrid_ggl_final.RDS')


# column names
ggl_header <- c("YEAR",
                "REGION",
                "ESTLOSS",
                "TOTDISP",
                "DIRCTUSE",
                "GGRSLOSS")

# description of column names
ggl_desc <- c("Data Year",
              "One of the three interconnect power grids in the U.S. (plus Alaska, Hawaii, and the entire U.S.)",
              "Estimated losses (MWh)",
              "Total disposition (MWh) without exports",
              "Direct use (MWh)",
              "Grid gross loss [Estimated losses/(Total disposition without exports - Direct use)]")

# set up header and descriptions format
#colnames(ggl_output) <- ggl_desc
#ggl_output <- rbind(ggl_header, ggl_output)
colnames(ggl_output) <- ggl_header


# FIX THIS: ONLY PRODUCING ONE CELL!!
writeData(wb, sheet = ggl, ggl_desc, startRow = 1)
addStyle(wb, sheet = ggl, style = desc_style, rows = 1, cols = 1:6, gridExpand = TRUE)

# write data to sheet
writeData(wb, 
          sheet = ggl, 
          ggl_output,
          startRow = 2)

## set column widths

setColWidths(wb, sheet = ggl, cols = 1, widths = 10)
setColWidths(wb, sheet = ggl, cols = 2, widths = 21.29)
setColWidths(wb, sheet = ggl, cols = 3:5, widths = 11.14)
setColWidths(wb, sheet = ggl, cols = 6, widths = 23)

## set row heights

setRowHeights(wb, sheet = ggl, row = 1, heights = 60.75)

## add header + desc styles

addStyle(wb, sheet = ggl, style = header_style, rows = 2, cols = 1:6, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = desc_style, rows = 1, cols = 1:6, gridExpand = TRUE)

## add number styles

addStyle(wb, sheet = ggl, style = integer, rows = 3:7, cols = 3:5, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = percent, rows = 3:7, cols = 6, gridExpand = TRUE)

# bolded
addStyle(wb, sheet = ggl, style = integer_bold, rows = 8, cols = 3:5, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = percent_bold, rows = 8, cols = 6, gridExpand = TRUE)

## add text styles

# first two columns
addStyle(wb, sheet = ggl, style = basic, rows = 3:7, cols = 1:2, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = bold, rows = 8, cols = 1:2, gridExpand = TRUE)



saveWorkbook(wb, "data/outputs/text1.xlsx", overwrite=TRUE)

### Unit File Formatting -----
#   rename("sequnt" = "sequnt",
#          #"year" = "year", 
#          "plant_id" = "orispl",
#          "plant_name" = "pname", 
#          "plant_state" = "pstatabb", 
#          "unit_id" = "unitid", 
#          "prime_mover" = "prmvr", 
#          "operating_status" = "untopst", 
#          "camd_flag" = "camdflag", 
#          "program_code" = "prgcode", 
#          "botfirty" = "botfirty", 
#          "num_generators" = "numgen", 
#          "primary_fuel_type" = "fuelu1", 
#          "operating_hours" = "hrsop", 
#          "heat_input" = "htian", 
#          "heat_input_oz" = "htioz", 
#          "nox_mass" = "noxan", 
#          "nox_oz_mass" = "noxoz", 
#          "so2_mass" = "so2an", 
#          "co2_mass" = "co2an", 
#          "hg_mass" = "hgan", 
#          "heat_input_source" = "htiansrc", 
#          "heat_input_oz_source" = "htiozsrc", 
#          "nox_source" = "noxansrc", 
#          "nox_oz_source" = "noxozsrc", 
#          "so2_source" = "so2src", 
#          "co2_source" = "co2src", 
#          "hg_controls" = "hgsrc", 
#          "so2_controls" = "so2ctldv", 
#          "nox_controls" = "noxctldv", 
#          "hg_controls_flag" = "hgctldv",
#          "year_online" = "untyronl")