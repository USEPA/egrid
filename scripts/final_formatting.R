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

integer2 <- createStyle(numFmt = "#,##0; (#,##0)",
                        fontName = "Arial",
                        fontSize = 8.5)

# basic style (for percentages with 1 decimal place)
percent <- createStyle(numFmt = "0.0%",
                       fontName = "Arial",
                       fontSize = 8.5)

# different decimal styles for different purposes
decimal1 <- createStyle(numFmt = "#,##0.000",
                        fontName = "Arial",
                        fontSize = 8.5)

decimal2 <- createStyle(numFmt = "#,##0.0",
                        fontName = "Arial",
                        fontSize = 8.5)

decimal3 <- createStyle(numFmt = "#,##0.000; (#,##0.000)",
                        fontName = "Arial",
                        fontSize = 8.5)

decimal4 <- createStyle(numFmt = "#,##0.0000",
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
color1_header <- createStyle(textDecoration = "bold",
                                fgFill = "#F2DCDB", 
                                wrapText = TRUE,
                                fontName = "Arial",
                                fontSize = 8.5,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")

color1_desc <- createStyle(wrapText = TRUE,
                            halign = "center",
                            valign = "center",
                            textDecoration = "bold",
                            fgFill = "#F2DCDB", 
                            fontName = "Arial",
                            fontSize = 8.5,
                            border = "TopBottomLeftRight",
                            borderStyle = "thin")

# 2. Unadjusted Annual Values (#E6B8B7)
color2_header <- createStyle(textDecoration = "bold",
                                fgFill = "#E6B8B7", 
                                wrapText = TRUE,
                                fontName = "Arial",
                                fontSize = 8.5,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")

color2_desc <- createStyle(wrapText = TRUE,
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              fgFill = "#E6B8B7", 
                              fontName = "Arial",
                              fontSize = 8.5,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")

# 3. Adjustment Values (#D58785)
color3_header <- createStyle(textDecoration = "bold",
                                fgFill = "#D58785", 
                                wrapText = TRUE,
                                fontName = "Arial",
                                fontSize = 8.5,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")

color3_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#D58785", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin")

# 4. Output Emission Rates (#EBF1DE)
color4_header <- createStyle(textDecoration = "bold",
                             fgFill = "#EBF1DE", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")

color4_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#EBF1DE", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin")

# 5. Input Emission Rates (#C4D79B)
color5_header <- createStyle(textDecoration = "bold",
                             fgFill = "#C4D79B", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")

color5_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#C4D79B", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin")

# 6. Combustion Output Rates
color6_header <- createStyle(textDecoration = "bold",
                             fgFill = "#76933C", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin",
                             fontColour = "white")

color6_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#76933C", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin",
                           fontColour = "white")

# 7. Generation by Fuel Type (#DAEEF3)
color7_header <- createStyle(textDecoration = "bold",
                             fgFill = "#DAEEF3", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")

color7_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#DAEEF3", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin")

# 8. Renewable and Non-Renewable Generation (#92CDDC)
color8_header <- createStyle(textDecoration = "bold",
                             fgFill = "#92CDDC", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")

color8_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#92CDDC", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin")

# 9. Combustion and Non-Combustion Generation (#3DA2BD) #31869B
color9_header <- createStyle(textDecoration = "bold",
                             fgFill = "#3DA2BD", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")

color9_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#3DA2BD", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin")

color9v2_header <- createStyle(textDecoration = "bold",
                             fgFill = "#31869B", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin",
                             fontColour = "white")

color9v2_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#31869B", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin",
                           fontColour = "white")

# 10. Resource Mix (#FDE9D9)
color10_header <- createStyle(textDecoration = "bold",
                             fgFill = "#FDE9D9", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")

color10_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#FDE9D9", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin")

# 11. Renewable and Non-Renewable Resource Mix (#FABF8F)
color11_header <- createStyle(textDecoration = "bold",
                             fgFill = "#FABF8F", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")

color11_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#FABF8F", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin")

# 12. Combustion and Non-Combustion Resource Mix (#F6903C) (#E26B0A)
color12_header <- createStyle(textDecoration = "bold",
                             fgFill = "#F6903C", 
                             wrapText = TRUE,
                             fontName = "Arial",
                             fontSize = 8.5,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")

color12_desc <- createStyle(wrapText = TRUE,
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           fgFill = "#F6903C", 
                           fontName = "Arial",
                           fontSize = 8.5,
                           border = "TopBottomLeftRight",
                           borderStyle = "thin")

color12v2_header <- createStyle(textDecoration = "bold",
                              fgFill = "#E26B0A", 
                              wrapText = TRUE,
                              fontName = "Arial",
                              fontSize = 8.5,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")

color12v2_desc <- createStyle(wrapText = TRUE,
                            halign = "center",
                            valign = "center",
                            textDecoration = "bold",
                            fgFill = "#E26B0A", 
                            fontName = "Arial",
                            fontSize = 8.5,
                            border = "TopBottomLeftRight",
                            borderStyle = "thin")


### UNT Formatting -----
# create sheet
unt <- glue::glue("UNT{year}")
addWorksheet(wb, unt)

# changing values to numeric
unt_file <- unt_file %>%
  mutate(year = as.numeric(year),
         plant_id = as.numeric(plant_id))

# create name for sequnt based on data year
sequnt <- glue::glue("SEQUNT{year}") 

# collect number of rows based on data frame
unt_rows <- nrow(unt_file)+2

# column names
unt_header <-  c(sequnt,
                 "YEAR",
                 "PSTATABB", 
                 "PNAME", 
                 "ORISPL",
                 "UNITID", 
                 "PRMVR", 
                 "UNTOPST", 
                 "CAMDFLAG", 
                 "PRGCODE", 
                 "BOTFIRTY", 
                 "NUMGEN", 
                 "FUELU1", 
                 "HRSOP", 
                 "HTIAN", 
                 "HTIOZ",
                 "NOXAN",
                 "NOXOZ",
                 "SO2AN",
                 "CO2AN",
                 "HGAN",
                 "HTIANSRC",
                 "HTIOZSRC",
                 "NOXANSRC",
                 "NOXOZSRC",
                 "SO2SRC",
                 "CO2SRC",
                 "HGSRC",
                 "SO2CTLDV",
                 "NOXCTLDV",
                 "HGCTLDV",
                 "UNTYRONL")

# description of column names
unt_desc <- c("Unit file sequence number",
              "Data Year",
              "Plant state abbreviation",
              "Plant name",
              "DOE/EIA ORIS plant or facility code",
              "Unit ID",
              "Prime Mover",
              "Unit Operational Status",
              "CAMD Program flag",
              "Program code(s)",
              "Unit bottom and firing type",
              "Number of associated generators",
              "Unit primary fuel",
              "Unit operating hours",
              "Unit unadjusted annual heat input (MMBtu)",
              "Unit unadjusted ozone season heat input (MMBtu)",
              "Unit unadjusted annual NOx emissions (tons)",
              "Unit unadjusted ozone season NOx emissions (tons)",
              "Unit unadjusted annual SO2 emissions (tons)",
              "Unit unadjusted annual CO2 emissions (tons)",
              "Unit unadjusted annual Hg emissions (lbs)",
              "Unit unadjusted annual heat input source",
              "Unit unadjusted ozone season heat input source",
              "Unit unadjusted annual NOx emissions source",
              "Unit unadjusted ozone season NOx emissions source",
              "Unit unadjusted annual SO2 emissions source",
              "Unit unadjusted annual CO2 emissions source",
              "Unit unadjusted annual Hg emissions source",
              "Unit SO2 (scrubber) first control device",
              "Unit NOx first control device",
              "Unit Hg Activated carbon injection system flag",
              "Unit year on-line")


# change column names
colnames(unt_file) <- unt_header

# write data for first row only
writeData(wb, sheet = unt, t(unt_desc), startRow = 1, colNames = FALSE)

# add first row styles
addStyle(wb, sheet = unt, style = desc_style, rows = 1, cols = 1:14, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = color2_desc, rows = 1, cols = 15:28, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = desc_style, rows = 1, cols = 29:32, gridExpand = TRUE)

# write data to sheet
writeData(wb, 
          sheet = unt, 
          unt_file,
          startRow = 2)

## set column widths

setColWidths(wb, sheet = unt, cols = 1, widths = 12.43)
setColWidths(wb, sheet = unt, cols = 3, widths = 12.43)
setColWidths(wb, sheet = unt, cols = 4, widths = 34.71)
setColWidths(wb, sheet = unt, cols = 5:9, widths = 12.43)
setColWidths(wb, sheet = unt, cols = 10, widths = 17)
setColWidths(wb, sheet = unt, cols = 11:32, widths = 12.43)

## set row heights

setRowHeights(wb, sheet = unt, row = 1, heights = 60.75)

## add header style

addStyle(wb, sheet = unt, style = header_style, rows = 2, cols = 1:14, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = color2_header, rows = 2, cols = 15:28, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = header_style, rows = 2, cols = 29:32, gridExpand = TRUE)

## add number styles

addStyle(wb, sheet = unt, style = integer, rows = 3:unt_rows, cols = 15:16, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = decimal2, rows = 3:unt_rows, cols = 14, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = decimal2, rows = 3:unt_rows, cols = 17:21, gridExpand = TRUE)

## add text styles

addStyle(wb, sheet = unt, style = basic, rows = 3:unt_rows, cols = 1:13, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = basic, rows = 3:unt_rows, cols = 22:32, gridExpand = TRUE)


### GEN Formatting -----

## create "GEN" sheet
gen <- glue::glue("GEN{year}")
addWorksheet(wb, gen)

# changing values to numeric
gen_file <- gen_file %>%
  mutate(year = as.numeric(year),
         plant_id = as.numeric(plant_id),
         operating_year = as.numeric(operating_year),
         retirement_year = as.numeric(retirement_year))

# create name for seqgen based on data year
seqgen <- glue::glue("SEQGEN{year}") 

# select number of rows based on length of dataframe
gen_rows <- nrow(gen_file)+2

# column names
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

# description of column names
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

# change dataframe column names
colnames(gen_file) <- gen_header

# write data for first row only
writeData(wb, sheet = gen, t(gen_desc), startRow = 1, colNames = FALSE)

# add first row styles
addStyle(wb, sheet = gen, style = desc_style, rows = 1, cols = 1:12, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = color1_desc, rows = 1, cols = 13:14, gridExpand = TRUE)
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
addStyle(wb, sheet = gen, style = color1_header, rows = 2, cols = 13:14, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = header_style, rows = 2, cols = 15:17, gridExpand = TRUE)

## add number styles

addStyle(wb, sheet = gen, style = integer, rows = 3:gen_rows, cols = 7, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = decimal2, rows = 3:gen_rows, cols = 11, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = decimal3, rows = 3:gen_rows, cols = 12, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = integer2, rows = 3:gen_rows, cols = 13:14, gridExpand = TRUE)

## add text styles

addStyle(wb, sheet = gen, style = basic, rows = 3:gen_rows, cols = 1:6, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = basic, rows = 3:gen_rows, cols = 6:10, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = basic, rows = 3:gen_rows, cols = 14:17, gridExpand = TRUE)

### PLNT Formatting -----

## create "PLNT" sheet
plnt <- glue::glue("PLNT{year}")
addWorksheet(wb, plnt)

# convert variables to numeric value
plnt_file <- plnt_file %>%
  mutate(year = as.numeric(year),
         plant_id = as.numeric(plant_id),
         system_owner_id = as.numeric(system_owner_id),
         utility_id = as.numeric(utility_id))

# create sqtplnt name using year
seqplt <- glue::glue("SEQPLT{year}") 

# select number of rows from data frame
plnt_rows <- nrow(plnt_file)+2 

## vector of names -----

# column names
plnt_header <- c(seqplt, # 1 
                 "YEAR",
                 "PSTATABB",
                 "PNAME",
                 "ORISPL",
                 "OPRNAME",
                 "OPRCODE",
                 "UTLSRVNM",
                 "UTLSRVID",
                 "SECTOR",
                 "BANAME",
                 "BACODE",
                 "NERC",
                 "SUBRGN",
                 "SRNAME",
                 "ISORTO",
                 "FIPSST",
                 "FIPSCNTY",
                 "CNTYNAME",
                 "LAT",
                 "LON",
                 "CAMDFLAG",
                 "NUMUNT",
                 "NUMGEN",
                 "PLPRMFL",
                 "PLFUELCT",
                 "COALFLAG",
                 "CAPFAC",
                 "NAMEPCAP",
                 "NBFACTOR",
                 "RMBMFLAG",
                 "CHPFLAG",
                 "USETHRMO",
                 "PWRTOHT",
                 "ELCALLOC",
                 "PSFLAG",
                 "PLHTIAN",
                 "PLHTIOZ",
                 "PLHTIANT",
                 "PLHTIOZT",
                 "PLNGENAN",
                 "PLNGENOZ",
                 "PLNOXAN",
                 "PLNOXOZ",
                 "PLSO2AN",
                 "PLCO2AN",
                 "PLCH4AN",
                 "PLN2OAN",
                 "PLCO2EQA",
                 "PLHGAN",
                 "PLNOXRTA",
                 "PLNOXRTO",
                 "PLSO2RTA",
                 "PLCO2RTA",
                 "PLCH4RTA",
                 "PLN2ORTA",
                 "PLC2ERTA",
                 "PLHGRTA",
                 "PLNOXRA",
                 "PLNOXRO",
                 "PLSO2RA",
                 "PLCO2RA",
                 "PLCH4RA",
                 "PLN2ORA",
                 "PLC2ERA",
                 "PLHGRA",
                 "PLNOXCRT",
                 "PLNOXCRO",
                 "PLSO2CRT",
                 "PLCO2CRT",
                 "PLCH4CRT",
                 "PLN2OCRT",
                 "PLC2ECRT",
                 "PLHGCRT",
                 "UNNOX",
                 "UNNOXOZ",
                 "UNSO2",
                 "UNCO2",
                 "UNCH4",
                 "UNN2O",
                 "UNHG",
                 "UNHTI",
                 "UNHTIOZ",
                 "UNHTIT",
                 "UNHTIOZT",
                 "UNNOXSRC",
                 "UNNOZSRC",
                 "UNSO2SRC",
                 "UNCO2SRC",
                 "UNCH4SRC",
                 "UNN2OSRC",
                 "UNHGSRC",
                 "UNHTISRC",
                 "UNHOZSRC",
                 "BIONOX",
                 "BIONOXOZ",
                 "BIOSO2",
                 "BIOCO2",
                 "BIOCH4",
                 "BION2O",
                 "CHPCHTI",
                 "CHPCHTIOZ",
                 "CHPNOX",
                 "CHPNOXOZ",
                 "CHPSO2",
                 "CHPCO2",
                 "CHPCH4",
                 "CHPN2O",
                 "PLHTRT",
                 "PLGENACL",
                 "PLGENAOL",
                 "PLGENAGS",
                 "PLGENANC",
                 "PLGENAHY",
                 "PLGENABM",
                 "PLGENAWI",
                 "PLGENASO",
                 "PLGENAGT",
                 "PLGENAOF",
                 "PLGENAOP",
                 "PLGENATN",
                 "PLGENATR",
                 "PLGENATH",
                 "PLGENACY",
                 "PLGENACN",
                 "PLCLPR",
                 "PLOLPR",
                 "PLGSPR",
                 "PLNCPR",
                 "PLHYPR",
                 "PLBMPR",
                 "PLWIPR",
                 "PLSOPR",
                 "PLGTPR",
                 "PLOFPR",
                 "PLOPPR",
                 "PLTNPR",
                 "PLTRPR",
                 "PLTHPR",
                 "PLCYPR",
                 "PLCNPR")

# description of column names 
plnt_desc <- c("Plant file sequence number",
               "Data Year",
               "Plant state abbreviation",
               "Plant name",
               "DOE/EIA ORIS plant or facility code",
               "Plant transmission or distribution system owner name",
               "Plant transmission or distribution system owner ID",
               "Utility name",
               "Utility ID",
               "Plant-level sector",
               "Balancing Authority Name",
               "Balancing Authority Code",
               "NERC region acronym",
               "eGRID subregion acronym",
               "eGRID subregion name",
               "Plant associated ISO/RTO Territory",
               "Plant FIPS state code",
               "Plant FIPS county code",
               "Plant county name",
               "Plant latitude",
               "Plant longitude",
               "CAMD Program Flag",
               "Number of units",
               "Number of generators",
               "Plant primary fuel",
               "Plant primary fuel category",
               "Flag indicating if the plant burned or generated any amount of coal",
               "Plant capacity factor",
               "Plant nameplate capacity (MW)",
               "Nonbaseload Factor",
               "Biogas/ biomass plant adjustment flag","Combined heat and power (CHP) plant adjustment flag",
               "CHP plant useful thermal output (MMBtu)",
               "CHP plant power to heat ratio",
               "CHP plant electric allocation factor",
               "Plant pumped storage flag",
               "Plant annual heat input from combustion (MMBtu)",
               "Plant ozone season heat input from combustion (MMBtu)",
               "Plant total annual heat input (MMBtu)",
               "Plant total ozone season heat input (MMBtu)",
               "Plant annual net generation (MWh)",
               "Plant ozone season net generation (MWh)",
               "Plant annual NOx emissions (tons)",
               "Plant ozone season NOx emissions (tons)",
               "Plant annual SO2 emissions (tons)",
               "Plant annual CO2 emissions (tons)",
               "Plant annual CH4 emissions (lbs)",
               "Plant annual N2O emissions (lbs)",
               "Plant annual CO2 equivalent emissions (tons)",
               "Plant annual Hg emissions (lbs)",
               "Plant annual NOx total output emission rate (lb/MWh)",
               "Plant ozone season NOx total output emission rate (lb/MWh)",
               "Plant annual SO2 total output emission rate (lb/MWh)",
               "Plant annual CO2 total output emission rate (lb/MWh)",
               "Plant annual CH4 total output emission rate (lb/MWh)",
               "Plant annual N2O total output emission rate (lb/MWh)",
               "Plant annual CO2 equivalent total output emission rate (lb/MWh)",
               "Plant annual Hg total output emission rate (lb/MWh)",
               "Plant annual NOx input emission rate (lb/MMBtu)",
               "Plant ozone season NOx input emission rate (lb/MMBtu)",
               "Plant annual SO2 input emission rate (lb/MMBtu)",
               "Plant annual CO2 input emission rate (lb/MMBtu)",
               "Plant annual CH4 input emission rate (lb/MMBtu)",
               "Plant annual N2O input emission rate (lb/MMBtu)",
               "Plant annual CO2 equivalent input emission rate (lb/MMBtu)",
               "Plant annual Hg input emission rate (lb/MMBtu)",
               "Plant annual NOx combustion output emission rate (lb/MWh)",
               "Plant ozone season NOx combustion output emission rate (lb/MWh)",
               "Plant annual SO2 combustion output emission rate (lb/MWh)",
               "Plant annual CO2 combustion output emission rate (lb/MWh)",
               "Plant annual CH4 combustion output emission rate (lb/MWh)",
               "Plant annual N2O combustion output emission rate (lb/MWh)",
               "Plant annual CO2 equivalent combustion output emission rate (lb/MWh)",
               "Plant annual Hg combustion output emission rate (lb/MWh)",
               "Plant unadjusted annual NOx emissions (tons)",
               "Plant unadjusted ozone season NOx emissions (tons)",
               "Plant unadjusted annual SO2 emissions (tons)",
               "Plant unadjusted annual CO2 emissions (tons)",
               "Plant unadjusted annual CH4 emissions (lbs)",
               "Plant unadjusted annual N2O emissions (lbs)",
               "Plant unadjusted annual Hg emissions (lbs)",
               "Plant unadjusted annual heat input from combustion (MMBtu)",
               "Plant unadjusted ozone season heat input from combustion (MMBtu)",
               "Plant unadjusted total annual heat input (MMBtu)",
               "Plant unadjusted total ozone season heat input (MMBtu)",
               "Plant unadjusted annual NOx emissions source",
               "Plant unadjusted ozone season NOx emissions source",
               "Plant unadjusted annual SO2 emissions source",
               "Plant unadjusted annual CO2 emissions source",
               "Plant unadjusted annual CH4 emissions source",
               "Plant unadjusted annual N2O emissions source",
               "Plant unadjusted annual Hg emissions source",
               "Plant unadjusted annual heat input source",
               "Plant unadjusted ozone season heat input source",
               "Plant annual NOx biomass emissions (tons)",
               "Plant ozone season NOx biomass emissions (tons)",
               "Plant annual SO2 biomass emissions (tons)",
               "Plant annual CO2 biomass emissions (tons)",
               "Plant annual CH4 biomass emissions (lbs)",
               "Plant annual N2O biomass emissions (lbs)",
               "Plant combustion heat input CHP adjustment value (MMBtu)",
               "Plant combustion annual ozone season heat input CHP adjustment value (MMBtu)",
               "Plant annual NOx emissions CHP adjustment value (tons)",
               "Plant ozone season NOx emissions CHP adjustment value (tons)",
               "Plant annual SO2 emissions CHP adjustment value (tons)",
               "Plant annual CO2 emissions CHP adjustment value (tons)",
               "Plant annual CH4 emissions CHP adjustment value (lbs)",
               "Plant annual N2O emissions CHP adjustment value (lbs)",
               "Plant nominal heat rate (Btu/kWh)",
               "Plant annual coal net generation (MWh)",
               "Plant annual oil net generation (MWh)",
               "Plant annual gas net generation (MWh)",
               "Plant annual nuclear net generation (MWh)",
               "Plant annual hydro net generation (MWh)",
               "Plant annual biomass net generation (MWh)",
               "Plant annual wind net generation (MWh)",
               "Plant annual solar net generation (MWh)",
               "Plant annual geothermal net generation (MWh)",
               "Plant annual other fossil net generation (MWh)",
               "Plant annual other unknown/ purchased fuel net generation (MWh)",
               "Plant annual total nonrenewables net generation (MWh)",
               "Plant annual total renewables net generation (MWh)",
               "Plant annual total nonhydro renewables net generation (MWh)",
               "Plant annual total combustion net generation (MWh)",
               "Plant annual total noncombustion net generation (MWh)",
               "Plant coal generation percent (resource mix)",
               "Plant oil generation percent (resource mix)",
               "Plant gas generation percent (resource mix)",
               "Plant nuclear generation percent (resource mix)",
               "Plant hydro generation percent (resource mix)",
               "Plant biomass generation percent (resource mix)",
               "Plant wind generation percent (resource mix)",
               "Plant solar generation percent (resource mix)",
               "Plant geothermal generation percent (resource mix)",
               "Plant other fossil generation percent (resource mix)",
               "Plant other unknown / purchased fuel generation percent (resource mix)",
               "Plant total nonrenewables generation percent (resource mix)",
               "Plant total renewables generation percent (resource mix)",
               "Plant total nonhydro renewables generation percent (resource mix)",
               "Plant total combustion generation percent (resource mix)",
               "Plant total noncombustion generation percent (resource mix)")

# change columns name                   
colnames(plnt_file) <- plnt_header

## add data and styles ----

# write data for first row only
writeData(wb, sheet = plnt, t(plnt_desc), startRow = 1, colNames = FALSE)

# add style for first row only
addStyle(wb, sheet = plnt, style = desc_style, rows = 1, cols = 1:36, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color1_desc, rows = 1, cols = 37:50, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color4_desc, rows = 1, cols = 51:58, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color5_desc, rows = 1, cols = 59:66, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color6_desc, rows = 1, cols = 67:74, gridExpand = TRUE) # white font
addStyle(wb, sheet = plnt, style = color2_desc, rows = 1, cols = 75:94, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color3_desc, rows = 1, cols = 95:108, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = desc_style, rows = 1, cols = 109, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color7_desc, rows = 1, cols = 110:120, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color8_desc, rows = 1, cols = 121:122, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color9_desc, rows = 1, cols = 123, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color9v2_desc, rows = 1, cols = 124:125, gridExpand = TRUE) # white font
addStyle(wb, sheet = plnt, style = color10_desc, rows = 1, cols = 126:136, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color11_desc, rows = 1, cols = 137:138, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color12_desc, rows = 1, cols = 139, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color12v2_desc, rows = 1, cols = 140:141, gridExpand = TRUE)


# write data to sheet
writeData(wb, 
          sheet = plnt, 
          plnt_file,
          startRow = 2)

## add header styles
addStyle(wb, sheet = plnt, style = header_style, rows = 2, cols = 1:36, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color1_header, rows = 2, cols = 37:50, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color4_header, rows = 2, cols = 51:58, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color5_header, rows = 2, cols = 59:66, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color6_header, rows = 2, cols = 67:74, gridExpand = TRUE) # white font
addStyle(wb, sheet = plnt, style = color2_header, rows = 2, cols = 75:94, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color3_header, rows = 2, cols = 95:108, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = header_style, rows = 2, cols = 109, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color7_header, rows = 2, cols = 110:120, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color8_header, rows = 2, cols = 121:122, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color9_header, rows = 2, cols = 123, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color9v2_header, rows = 2, cols = 124:125, gridExpand = TRUE) # white font
addStyle(wb, sheet = plnt, style = color10_header, rows = 2, cols = 126:136, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color11_header, rows = 2, cols = 137:138, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color12_header, rows = 2, cols = 139, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = color12v2_header, rows = 2, cols = 140:141, gridExpand = TRUE)

## set column widths

setColWidths(wb, sheet = plnt, cols = 1, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 3, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 4, widths = 34.71)
setColWidths(wb, sheet = plnt, cols = 5, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 6, widths = 32.14)
setColWidths(wb, sheet = plnt, cols = 7, widths = 12.57)
setColWidths(wb, sheet = plnt, cols = 8, widths = 35)
setColWidths(wb, sheet = plnt, cols = 9, widths = 12.57)
setColWidths(wb, sheet = plnt, cols = 10, widths = 14.86)
setColWidths(wb, sheet = plnt, cols = 11, widths = 32)
setColWidths(wb, sheet = plnt, cols = 12:14, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 15, widths = 17.29)
setColWidths(wb, sheet = plnt, cols = 16:22, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 23, widths = 12.57)
setColWidths(wb, sheet = plnt, cols = 24, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 25:26, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 27, widths = 12.57)
setColWidths(wb, sheet = plnt, cols = 28, widths = 13.14)
setColWidths(wb, sheet = plnt, cols = 29, widths = 13.29)
setColWidths(wb, sheet = plnt, cols = 30, widths = 13)
setColWidths(wb, sheet = plnt, cols = 31:33, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 34, widths = 13.29)
setColWidths(wb, sheet = plnt, cols = 35:53, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 54, widths = 13.14)
setColWidths(wb, sheet = plnt, cols = 55:56, widths = 12.86)
setColWidths(wb, sheet = plnt, cols = 57:76, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 77, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 78, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 79, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 80, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 81, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 82, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 83, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 84:87, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 88, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 89, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 90:101, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 102, widths = 14)
setColWidths(wb, sheet = plnt, cols = 103:115, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 116:141, widths = 12.71)


## set row heights

setRowHeights(wb, sheet = plnt, row = 1, heights = 67.5)

## add number styles

addStyle(wb, sheet = plnt, style = integer, rows = 3:plnt_rows, cols = 23:24, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = decimal4, rows = 3:plnt_rows, cols = 28, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = decimal2, rows = 3:plnt_rows, cols = 29, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = decimal4, rows = 3:plnt_rows, cols = 30, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = integer, rows = 3:plnt_rows, cols = 33:34, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = decimal4, rows = 3:plnt_rows, cols = 35, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = integer2, rows = 3:plnt_rows, cols = 37:50, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = decimal1, rows = 3:plnt_rows, cols = 51:74, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = integer, rows = 3:plnt_rows, cols = 75:85, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = integer, rows = 3:plnt_rows, cols = 95:108, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = decimal3, rows = 3:plnt_rows, cols = 109, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = integer2, rows = 3:plnt_rows, cols = 110:125, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = percent, rows = 3:plnt_rows, cols = 126:141, gridExpand = TRUE)

## add text styles
addStyle(wb, sheet = plnt, style = basic, rows = 3:plnt_rows, cols = 1:22, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = basic, rows = 3:plnt_rows, cols = 25:27, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = basic, rows = 3:plnt_rows, cols = 31:32, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = basic, rows = 3:plnt_rows, cols = 36, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = basic, rows = 3:plnt_rows, cols = 86:94, gridExpand = TRUE)

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




