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
unt_file <- readRDS('data/outputs/unit_file.RDS')
gen_file <- readRDS('data/outputs/generator_file.RDS')
plnt_file <- readRDS('data/outputs/plant_file.RDS')
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


### header & description style ###

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
### text styles ###

# bold style (for text/characters)
bold <- createStyle(fontName = "Arial",
                    fontSize = 8.5,
                    textDecoration = "bold")

# basic style (for text/characters)
basic <- createStyle(fontName = "Arial",
                     fontSize = 8.5)

### number styles ### 

# basic style (for larger integers)
integer <- createStyle(numFmt = "#,##0",
                       fontName = "Arial",
                       fontSize = 8.5)

# basic style (for percentages with 1 decimal place)
percent <- createStyle(numFmt = "0.0%",
                       fontName = "Arial",
                       fontSize = 8.5)

decimal1 <- createStyle(numFmt = "#,##0; (#,##0)",
                    fontName = "Arial",
                    fontSize = 8.5)

decimal2 <- createStyle(numFmt = "#,##0.0",
                        fontName = "Arial",
                        fontSize = 8.5)

decimal3 <- createStyle(numFmt = "#,##0.000; (#,##0.000)",
                        fontName = "Arial",
                        fontSize = 8.5)

### bold number styles ### 

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

### Header Styles (colors) -----

# 1. Annual Values (#F2DCDB)
ann_vals_header <- createStyle(textDecoration = "bold",
                                fgFill = "#F2DCDB", 
                                wrapText = TRUE,
                                fontName = "Arial",
                                fontSize = 8.5,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")

ann_vals_desc <- createStyle(wrapText = TRUE,
                            halign = "center",
                            valign = "center",
                            textDecoration = "bold",
                            fgFill = "#F2DCDB", 
                            fontName = "Arial",
                            fontSize = 8.5,
                            border = "TopBottomLeftRight",
                            borderStyle = "thin")

# 2. Unadjusted Annual Values ()
unadj_ann_style <- createStyle(textDecoration = "bold",
                                fgFill = "#F2DCDB", 
                                wrapText = TRUE,
                                fontName = "Arial",
                                fontSize = 8.5,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")

# 3. Adjustment Values ()
adj_vals_style <- createStyle(textDecoration = "bold",
                                fgFill = "#F2DCDB", 
                                wrapText = TRUE,
                                fontName = "Arial",
                                fontSize = 8.5,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")

### GEN Formatting -----

## create "GEN" sheet
gen_file <- readRDS('data/outputs/generator_file.RDS')
gen <- glue::glue("GEN{year}")
addWorksheet(wb, gen)

seqgen <- glue::glue("SEQGEN{year}") 
gen_rows <- nrow(gen_file)

gen_header <- c(seqgen,
                "YEAR",
                "PSTATEABB", 
                "PNAME",
                "ORISPL",
                "GENID",
                "NUMBLR",
                "GENSTAT",
                "PRMVR",
                "FUELG1",
                "NAMEPCAP",
                "CFACT",
                "GENNTAN",
                "GENNTOZ",
                "GENERSRC",
                "GENYRONL",
                "GENYRRET")

gen_desc <- c("Generator file sequence number",
              "Data Year",
              "Plant state abbreviation",
              "Plant name",
              "DOE/EIA ORIS plant or facility code",
              "Generator ID",
              "Number of associated boilers",
              "Generator status",
              "Generator prime mover type",
              "Generator primary fuel",
              "Generator nameplate capacity (MW)",
              "Generator capacity factor",
              "Generator annual net generation (MWh)",
              "Generator ozone season net generation (MWh)",
              "Generation data source",
              "Generator year on-line",
              "Generator planned or actual retirement year")

colnames(gen_file) <- gen_header

# write data for first row only
writeData(wb, sheet = gen, t(gen_desc), startRow = 1, colNames = FALSE)

# add first row styles
addStyle(wb, sheet = gen, style = desc_style, rows = 1, cols = 1:12, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = ann_vals_desc, rows = 1, cols = 13:14, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = desc_style, rows = 1, cols = 15:17, gridExpand = TRUE)

# write data to sheet
writeData(wb, 
          sheet = gen, 
          gen_file,
          startRow = 2)

## set column widths

setColWidths(wb, sheet = gen, cols = 1, widths = 12.57)
setColWidths(wb, sheet = gen, cols = 3, widths = 12.43)
setColWidths(wb, sheet = gen, cols = 4, widths = 34.71)
setColWidths(wb, sheet = gen, cols = 5:7, widths = 12.57)
setColWidths(wb, sheet = gen, cols = 8:10, widths = 12.43)
setColWidths(wb, sheet = gen, cols = 11, widths = 12.57)
setColWidths(wb, sheet = gen, cols = 12:14, widths = 13.14)
setColWidths(wb, sheet = gen, cols = 15, widths = 29.43)
setColWidths(wb, sheet = gen, cols = 16, widths = 12.29)
setColWidths(wb, sheet = gen, cols = 17, widths = 15.14)

## set row heights

setRowHeights(wb, sheet = gen, row = 1, heights = 60.75)

## add header style

addStyle(wb, sheet = gen, style = header_style, rows = 2, cols = 1:12, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = ann_vals_header, rows = 2, cols = 13:14, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = header_style, rows = 2, cols = 15:17, gridExpand = TRUE)

## add number styles

addStyle(wb, sheet = gen, style = integer, rows = 3:gen_rows, cols = 7, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = decimal2, rows = 3:gen_rows, cols = 11, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = decimal3, rows = 3:gen_rows, cols = 12, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = decimal1, rows = 3:gen_rows, cols = 13:14, gridExpand = TRUE)

## add text styles
addStyle(wb, sheet = gen, style = basic, rows = 3:gen_rows, cols = 1:6, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = basic, rows = 3:gen_rows, cols = 6:10, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = basic, rows = 3:gen_rows, cols = 14:17, gridExpand = TRUE)

### GGL Formatting -----

# create "GGL" sheet
ggl <- glue::glue("GGL{year}")
addWorksheet(wb, ggl)

# convert year to numeric value
ggl_file <- ggl_file %>%
  mutate(year = as.numeric(year))


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
colnames(ggl_file) <- ggl_header

# write data for first row only
writeData(wb, sheet = ggl, t(ggl_desc), startRow = 1, colNames = FALSE)
addStyle(wb, sheet = ggl, style = desc_style, rows = 1, cols = 1:6, gridExpand = TRUE)

# write data to sheet
writeData(wb, 
          sheet = ggl, 
          ggl_file,
          startRow = 2)

## set column widths

setColWidths(wb, sheet = ggl, cols = 1, widths = 10)
setColWidths(wb, sheet = ggl, cols = 2, widths = 21.29)
setColWidths(wb, sheet = ggl, cols = 3:5, widths = 11.14)
setColWidths(wb, sheet = ggl, cols = 6, widths = 23)

## set row heights

setRowHeights(wb, sheet = ggl, row = 1, heights = 60.75)

## add header style

addStyle(wb, sheet = ggl, style = header_style, rows = 2, cols = 1:6, gridExpand = TRUE)

## add number styles

addStyle(wb, sheet = ggl, style = integer, rows = 3:7, cols = 3:5, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = percent, rows = 3:7, cols = 6, gridExpand = TRUE)

# bold
addStyle(wb, sheet = ggl, style = integer_bold, rows = 8, cols = 3:5, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = percent_bold, rows = 8, cols = 6, gridExpand = TRUE)

## add text styles

# first two columns
addStyle(wb, sheet = ggl, style = basic, rows = 3:7, cols = 1:2, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = bold, rows = 8, cols = 1:2, gridExpand = TRUE)


### Save and export -----
saveWorkbook(wb, "data/outputs/text1.xlsx", overwrite=TRUE)




### UNT Formatting -----
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