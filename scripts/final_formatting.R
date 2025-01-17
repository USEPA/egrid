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

### Load libraries ------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)

### Load in data ------ 

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params) & "version" %in% names(params)) { # if params() and params$eGRID_year, params$version exist, do not re-define
    print("eGRID year and version parameters are already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year) 
    params$version <- readline(prompt = "Input version (format X.X.X): ")
    params$version <- as.character(params$version) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
  params$version <- readline(prompt = "Input version (format X.X.X): ")
  params$version <- as.character(params$version) 
}


# load files
unt_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))
gen_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/generator_file.RDS"))
plnt_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS"))
st_file    <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/state_aggregation.RDS"))
ba_file    <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/ba_aggregation.RDS"))
srl_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/subregion_aggregation.RDS"))
nrl_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/nerc_aggregation.RDS"))
us_file    <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/us_aggregation.RDS"))
ggl_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/grid_gross_loss.RDS"))

if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/demographics_file.RDS"))) {
  demo_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/demographics_file.RDS"))
}


# extract last two digits of year for universal labeling
year <- as.numeric(params$eGRID_year) %% 1000

# set up output file
### Note: check for updates or changes each data year ###
wb <- createWorkbook()
source("scripts/functions/function_create_contents_egrid_final.R")
create_contents_egrid_final()


### Create styles ------

# call helper functions into script
source("scripts/functions/function_format_styles.R")
source("scripts/functions/function_format_region.R")
source("scripts/functions/function_add_hyperlink.R")

# create eGRID output style list using function
s <- create_format_styles()

### Standard Column Names -----
# names for data sets: ST, BA, SRL, NRL, US
# data for region aggregated files contain same columns and information
# therefore, can assign a standardized list of columns, names, and styles

standard_labels <- c("NAMEPCAP" = "nameplate capacity (MW)",	
                     "HTIAN"    = "annual heat input from combustion (MMBtu)",	
                     "HTIOZ"    = "annual heat input from combustion (MMBtu)",	
                     "HTIANT"   = "total annual heat input (MMBtu)",
                     "HTIOZT"   = "total ozone season heat input (MMBtu)",	
                     "NGENAN"   = "annual net generation (MWh)",	
                     "NGENOZ"   = "ozone season net generation (MWh)",
                     "NGENNB"   = "annual nonbaseload generation (MWh)",
                     "NOXAN"    = "annual NOx emissions (tons)",	
                     "NOXOZ"    = "ozone season NOx emissions (tons)",	
                     "SO2AN"    = "annual SO2 emissions (tons)",	
                     "CO2AN"    = "annual CO2 emissions (tons)",
                     "CH4AN"    = "annual CH4 emissions (lbs)",	
                     "N2OAN"    = "annual N2O emissions (lbs)",	
                     "CO2EQA"   = "annual CO2 equivalent emissions (tons)",	
                     "HGAN"     = "annual Hg emissions (lbs)",
                     "NOXRTA"   = "annual NOx total output emission rate (lb/MWh)",
                     "NOXRTO"   = "ozone season NOx total output emission rate (lb/MWh)",	
                     "SO2RTA"   = "annual SO2 total output emission rate (lb/MWh)",	
                     "CO2RTA"   = "annual CO2 total output emission rate (lb/MWh)",	
                     "CH4RTA"   = "annual CH4 total output emission rate (lb/MWh)",
                     "N2ORTA"   = "annual N2O total output emission rate (lb/MWh)",
                     "C2ERTA"   = "annual CO2 equivalent total output emission rate (lb/MWh)",
                     "HGRTA"    = "annual Hg total output emission rate (lb/MWh)",
                     "NOXRA"    = "annual NOx input emission rate (lb/MMBtu)",
                     "NOXRO"    = "ozone season NOx input emission rate (lb/MMBtu)",
                     "SO2RA"    = "annual SO2 input emission rate (lb/MMBtu)",	
                     "CO2RA"    = "annual CO2 input emission rate (lb/MMBtu)",
                     "CH4RA"    = "annual CH4 input emission rate (lb/MMBtu)",	
                     "N2ORA"    = "annual N2O input emission rate (lb/MMBtu)",	
                     "C2ERA"    = "annual CO2 equivalent input emission rate (lb/MMBtu)",	
                     "HGRA"     = "annual Hg input emission rate (lb/MMBtu)",	
                     "NOXCRT"   = "annual NOx combustion output emission rate (lb/MWh)",
                     "NOXCRO"   = "ozone season NOx combustion output emission rate (lb/MWh)",	
                     "SO2CRT"   = "annual SO2 combustion output emission rate (lb/MWh)",
                     "CO2CRT"   = "annual CO2 combustion output emission rate (lb/MWh)",	
                     "CH4CRT"   = "annual CH4 combustion output emission rate (lb/MWh)",	
                     "N2OCRT"   = "annual N2O combustion output emission rate (lb/MWh)",	
                     "C2ECRT"   = "annual CO2 equivalent combustion output emission rate (lb/MWh)",
                     "HGCRT"    = "Hg combustion output emission rate (lb/MWh)",	
                     "CNOXRT"   = "annual NOx coal output emission rate (lb/MWh)",	
                     "ONOXRT"   = "annual NOx oil output emission rate (lb/MWh)",	
                     "GNOXRT"   = "annual NOx gas output emission rate (lb/MWh)",	
                     "FSNXRT"   = "annual NOx fossil fuel output emission rate (lb/MWh)",	
                     "CNXORT"   = "ozone season NOx coal output emission rate (lb/MWh)",
                     "ONXORT"   = "ozone season NOx oil output emission rate (lb/MWh)",	
                     "GNXORT"   = "ozone season NOx gas output emission rate (lb/MWh)",	
                     "FSNORT"   = "ozone season NOx fossil fuel output emission rate (lb/MWh)",
                     "CSO2RT"   = "annual SO2 coal output emission rate (lb/MWh)",	
                     "OSO2RT"   = "annual SO2 oil output emission rate (lb/MWh)",	
                     "GSO2RT"   = "annual SO2 gas output emission rate (lb/MWh)",	
                     "FSS2RT"   = "annual SO2 fossil fuel output emission rate (lb/MWh)",
                     "CCO2RT"   = "annual CO2 coal output emission rate (lb/MWh)",	
                     "OCO2RT"   = "annual CO2 oil output emission rate (lb/MWh)",	
                     "GCO2RT"   = "annual CO2 gas output emission rate (lb/MWh)",	
                     "FSC2RT"   = "annual CO2 fossil fuel output emission rate (lb/MWh)",	
                     "CCH4RT"   = "annual CH4 coal output emission rate (lb/MWh)",
                     "OCH4RT"   = "annual CH4 oil output emission rate (lb/MWh)",	
                     "GCH4RT"   = "annual CH4 gas output emission rate (lb/MWh)",	
                     "FCH4RT"   = "annual CH4 fossil fuel output emission rate (lb/MWh)",	
                     "CN2ORT"   = "annual N2O coal output emission rate (lb/MWh)",
                     "ON2ORT"   = "annual N2O oil output emission rate (lb/MWh)",	
                     "GN2ORT"   = "annual N2O gas output emission rate (lb/MWh)",
                     "FN2ORT"   = "annual N2O fossil fuel output emission rate (lb/MWh)",	
                     "CC2ERT"   = "annual CO2 equivalent coal output emission rate (lb/MWh)",	
                     "OC2ERT"   = "annual CO2 equivalent oil output emission rate (lb/MWh)",
                     "GC2ERT"   = "annual CO2 equivalent gas output emission rate (lb/MWh)",	
                     "FSC2ERT"  = "annual CO2 equivalent fossil fuel output emission rate (lb/MWh)",	
                     "CHGRT"    = "annual Hg coal output emission rate (lb/MWh)",	
                     "FSHGRT"   = "annual Hg fossil fuel output emission rate (lb/MWh)",
                     "CNOXR"    = "annual NOx coal input emission rate (lb/MMBtu)",
                     "ONOXR"    = "annual NOx oil input emission rate (lb/MMBtu)",	
                     "GNOXR"    = "annual NOx gas input emission rate (lb/MMBtu)",	
                     "FSNXR"    = "annual NOx fossil fuel input emission rate (lb/MMBtu)",	
                     "CNXOR"    = "ozone season NOx coal input emission rate (lb/MMBtu)",	
                     "ONXOR"    = "ozone season NOx oil input emission rate (lb/MMBtu)",	
                     "GNXOR"    = "ozone season NOx gas input emission rate (lb/MMBtu)",	
                     "FSNOR"    = "ozone season NOx fossil fuel input emission rate (lb/MMBtu)",
                     "CSO2R"    = "annual SO2 coal input emission rate (lb/MMBtu)",	
                     "OSO2R"    = "annual SO2 oil input emission rate (lb/MMBtu)",	
                     "GSO2R"    = "annual SO2 gas input emission rate (lb/MMBtu)",	
                     "FSS2R"    = "annual SO2 fossil fuel input emission rate (lb/MMBtu)",	
                     "CCO2R"    = "annual CO2 coal input emission rate (lb/MMBtu)",	
                     "OCO2R"    = "annual CO2 oil input emission rate (lb/MMBtu)",	
                     "GCO2R"    = "annual CO2 gas input emission rate (lb/MMBtu)",	
                     "FSC2R"    = "annual CO2 fossil fuel input emission rate (lb/MMBtu)",	
                     "CCH4R"    = "annual CH4 coal input emission rate (lb/MMBtu)",	
                     "OCH4R"    = "annual CH4 oil input emission rate (lb/MMBtu)",	
                     "GCH4R"    = "annual CH4 gas input emission rate (lb/MMBtu)",
                     "FCH4R"    = "annual CH4 fossil fuel input emission rate (lb/MMBtu)",	
                     "CN2OR"    = "annual N2O coal input emission rate (lb/MMBtu)",
                     "ON2OR"    = "annual N2O oil input emission rate (lb/MMBtu)",	
                     "GN2OR"    = "annual N2O gas input emission rate (lb/MMBtu)",	
                     "FN2OR"    = "annual N2O fossil fuel input emission rate (lb/MMBtu)",	
                     "CC2ER"    = "annual CO2 equivalent coal input emission rate (lb/MMBtu)",	
                     "OC2ER"    = "annual CO2 equivalent oil input emission rate (lb/MMBtu)",	
                     "GC2ER"    = "annual CO2 equivalent gas input emission rate (lb/MMBtu)",	
                     "FSC2ER"   = "annual CO2 equivalent fossil fuel input emission rate (lb/MMBtu)",	
                     "CHGR"     = "annual Hg coal input emission rate (lb/MMBtu)",	
                     "FSHGR"    = "annual Hg fossil fuel input emission rate (lb/MMBtu)",	
                     "NBNOX"    = "annual NOx non-baseload output emission rate (lb/MWh)",	
                     "NBNXO"    = "ozone season NOx non-baseload output emission rate (lb/MWh)",	
                     "NBSO2"    = "annual SO2 non-baseload output emission rate (lb/MWh)",	
                     "NBCO2"    = "annual CO2 non-baseload output emission rate (lb/MWh)",
                     "NBCH4"    = "annual CH4 non-baseload output emission rate (lb/MWh)",	
                     "NBN2O"    = "annual N2O non-baseload output emission rate (lb/MWh)",	
                     "NBC2E"    = "annual CO2 equivalent non-baseload output emission rate (lb/MWh)",	
                     "NBHG"     = "annual Hg non-baseload output emission rate (lb/MWh)",	
                     "GENACL"   = "annual coal net generation (MWh)",	
                     "GENAOL"   = "annual oil net generation (MWh)",	
                     "GENAGS"   = "annual gas net generation (MWh)",	
                     "GENANC"   = "annual nuclear net generation (MWh)",	
                     "GENAHY"   = "annual hydro net generation (MWh)",	
                     "GENABM"   = "annual biomass net generation (MWh)",	
                     "GENAWI"   = "annual wind net generation (MWh)",	
                     "GENASO"   = "annual solar net generation (MWh)",	
                     "GENAGT"   = "annual geothermal net generation (MWh)",	
                     "GENAOF"   = "annual other fossil net generation (MWh)",	
                     "GENAOP"   = "annual other unknown/ purchased fuel net generation (MWh)",	
                     "GENATN"   = "annual total nonrenewables net generation (MWh)",	
                     "GENATR"   = "annual total renewables net generation (MWh)",	
                     "GENATO"   = "annual total nonrenewable other unknown/purchased net generation (MWh)", 
                     "GENATH"   = "annual total nonhydro renewables net generation (MWh)",	
                     "GENACY"   = "annual total combustion net generation (MWh)",
                     "GENACN"   = "annual total noncombustion net generation (MWh)",
                     "GENACO"   = "annual total noncombustion other unknown/purchased net generation (MWh)", 
                     "CLPR"     = "coal generation percent (resource mix)",
                     "OLPR"     = "oil generation percent (resource mix)",	
                     "GSPR"     = "gas generation percent (resource mix)",	
                     "NCPR"     = "nuclear generation percent (resource mix)",	
                     "HYPR"     = "hydro generation percent (resource mix)",	
                     "BMPR"     = "biomass generation percent (resource mix)",
                     "WIPR"     = "wind generation percent (resource mix)",	
                     "SOPR"     = "solar generation percent (resource mix)",
                     "GTPR"     = "geothermal generation percent (resource mix)",	
                     "OFPR"     = "other fossil generation percent (resource mix)",
                     "OPPR"     = "other unknown/ purchased fuel generation percent (resource mix)",
                     "TNPR"     = "total nonrenewables generation percent (resource mix)",	
                     "TRPR"     = "total renewables generation percent (resource mix)",	
                     "TOPR"     = "total nonrenewables other unknown/purchased generation percent (resource mix)", 
                     "THPR"     = "total nonhydro renewables generation percent (resource mix)",	
                     "CYPR"     = "total combustion generation percent (resource mix)",	
                     "CNPR"     = "total noncombustion generation percent (resource mix)",
                     "COPR"     = "total noncombustion other unknown/purchased generation percent (resource mix)", 
                     "NBGNCL"   = "annual nonbaseload coal net generation (MWh)",	
                     "NBGNOL"   = "annual nonbaseload oil net generation (MWh)",	
                     "NBGNGS"   = "annual nonbaseload gas net generation (MWh)",	
                     "NBGNNC"   = "annual nonbaseload nuclear net generation (MWh)",	
                     "NBGNHY"   = "annual nonbaseload hydro net generation (MWh)",	
                     "NBGNBM"   = "annual nonbaseload biomass net generation (MWh)",	
                     "NBGNWI"   = "annual nonbaseload wind net generation (MWh)",	
                     "NBGNSO"   = "annual nonbaseload solar net generation (MWh)",
                     "NBGNGT"   = "annual nonbaseload geothermal net generation (MWh)",	
                     "NBGNOF"   = "annual nonbaseload other fossil net generation (MWh)",
                     "NBGNOP"   = "annual nonbaseload other unknown/ purchased fuel net generation (MWh)",	
                     "NBCLPR"   = "nonbaseload coal generation percent (resource mix)",	
                     "NBOLPR"   = "nonbaseload oil generation percent (resource mix)",	
                     "NBGSPR"   = "nonbaseload gas generation percent (resource mix)",	
                     "NBNCPR"   = "nonbaseload nuclear generation percent (resource mix)",	
                     "NBHYPR"   = "nonbaseload hydro generation percent (resource mix)",	
                     "NBBMPR"   = "nonbaseload biomass generation percent (resource mix)",	
                     "NBWIPR"   = "nonbaseload wind generation percent (resource mix)",	
                     "NBSOPR"   = "nonbaseload solar generation percent (resource mix)",	
                     "NBGTPR"   = "nonbaseload geothermal generation percent (resource mix)",	
                     "NBOFPR"   = "nonbaseload other fossil generation percent (resource mix)",	
                     "NBOPPR"   = "nonbaseload other unknown/ purchased fuel generation percent (resource mix)")


standard_header <- names(standard_labels)  # column names
standard_desc   <- unname(standard_labels) # description of column names


### UNT Formatting -----

## create "UNT" sheet
unt <- glue::glue("UNT{year}")
addWorksheet(wb, unt)

# changing values to numeric
unt_file <- unt_file %>%
            mutate(year = as.numeric(year),
                   plant_id = as.numeric(plant_id))

# create name for sequnt based on data year
sequnt_label <- setNames("Unit file sequence number", glue::glue("SEQUNT{year}") )

# collect number of rows based on data frame
# add two to number of rows (nrows) to account for header + description rows
unt_rows <- nrow(unt_file) + 2

## column names and descriptions
unt_labels <-  c(sequnt_label,
                 "YEAR"     = "Data Year",
                 "PSTATABB" = "Plant state abbreviation",
                 "PNAME"    = "Plant name",
                 "ORISPL"   = "DOE/EIA ORIS plant or facility code",
                 "UNITID"   = "Unit ID",
                 "PRMVR"    = "Prime Mover",
                 "UNTOPST"  = "Unit Operational Status",
                 "CAMDFLAG" = "CAMD program flag",
                 "PRGCODE"  = "Program code(s)",
                 "BOTFIRTY" = "Unit bottom and firing type",
                 "NUMGEN"   = "Number of associated generators",
                 "FUELU1"   = "Unit primary fuel",
                 "HRSOP"    = "Unit operating hours",
                 "HTIAN"    = "Unit unadjusted annual heat input (MMBtu)",
                 "HTIOZ"    = "Unit unadjusted ozone season heat input (MMBtu)",
                 "NOXAN"    = "Unit unadjusted annual NOx emissions (tons)",
                 "NOXOZ"    = "Unit unadjusted ozone season NOx emissions (tons)",
                 "SO2AN"    = "Unit unadjusted annual SO2 emissions (tons)",
                 "CO2AN"    = "Unit unadjusted annual CO2 emissions (tons)",
                 "HGAN"     = "Unit unadjusted annual Hg emissions (lbs)",
                 "HTIANSRC" = "Unit unadjusted annual heat input source",
                 "HTIOZSRC" = "Unit unadjusted ozone season heat input source",
                 "NOXANSRC" = "Unit unadjusted annual NOx emissions source",
                 "NOXOZSRC" = "Unit unadjusted ozone season NOx emissions source",
                 "SO2SRC"   = "Unit unadjusted annual SO2 emissions source",
                 "CO2SRC"   = "Unit unadjusted annual CO2 emissions source",
                 "HGSRC"    = "Unit unadjusted annual Hg emissions source",
                 "SO2CTLDV" = "Unit SO2 (scrubber) first control device",
                 "NOXCTLDV" = "Unit NOx first control device",
                 "HGCTLDV"  = "Unit Hg Activated carbon injection system flag",
                 "UNTYRONL" = "Unit year on-line",
                 "STACKHT"  = "Stack height (feet)")

unt_header <- names(unt_labels)  # column names
unt_desc   <- unname(unt_labels) # description of column names

# add new column names
colnames(unt_file) <- unt_header

## write data
# write data for first row only
writeData(wb, 
          sheet = unt, 
          t(unt_desc), 
          startRow = 1, 
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = unt, 
          unt_file,
          startRow = 2)

## add styles to document
# add description styles
addStyle(wb, sheet = unt, style = s[['desc_style']],  rows = 1, cols = 1:14,  gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['color2_desc']], rows = 1, cols = 15:28, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['desc_style']],  rows = 1, cols = 29:33, gridExpand = TRUE)

# add header style
addStyle(wb, sheet = unt, style = s[['header_style']],  rows = 2, cols = 1:14,  gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['color2_header']], rows = 2, cols = 15:28, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['header_style']],  rows = 2, cols = 29:33, gridExpand = TRUE)

# set column widths
setColWidths(wb, sheet = unt, cols = 1,     widths = 12.43)
setColWidths(wb, sheet = unt, cols = 3,     widths = 12.43)
setColWidths(wb, sheet = unt, cols = 4,     widths = 34.71)
setColWidths(wb, sheet = unt, cols = 5:9,   widths = 12.43)
setColWidths(wb, sheet = unt, cols = 10,    widths = 17)
setColWidths(wb, sheet = unt, cols = 11:33, widths = 12.43)

# set row heights
setRowHeights(wb, sheet = unt, row = 1, heights = 60.75)

# add number styles
addStyle(wb, sheet = unt, style = s[['integer']],  rows = 3:unt_rows, cols = 15:16, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['decimal2']], rows = 3:unt_rows, cols = 14,    gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['decimal2']], rows = 3:unt_rows, cols = 17:21, gridExpand = TRUE)

# add text styles
addStyle(wb, sheet = unt, style = s[['basic']], rows = 3:unt_rows, cols = 1:13,  gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['basic']], rows = 3:unt_rows, cols = 22:33, gridExpand = TRUE)

# freeze panes
freezePane(wb, sheet = unt, firstActiveCol = 7, firstActiveRow = 3)

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
seqgen_label <- setNames("Generator file sequence number", glue::glue("SEQGEN{year}"))

# select number of rows based on length of dataframe
# add two to number of rows (nrows) to account for header + description rows
gen_rows <- nrow(gen_file) + 2

## column names and descriptions
gen_labels <- c(seqgen_label,
                "YEAR"      = "Data Year",
                "PSTATEABB" = "Plant state abbreviation", 
                "PNAME"     = "Plant name",
                "ORISPL"    = "DOE/EIA ORIS plant or facility code",
                "GENID"     = "Generator ID",
                "NUMBLR"    = "Number of associated boilers",
                "GENSTAT"   = "Generator status",
                "PRMVR"     = "Generator prime mover type",
                "FUELG1"    = "Generator primary fuel",
                "NAMEPCAP"  = "Generator nameplate capacity (MW)",
                "CFACT"     = "Generator capacity factor",
                "GENNTAN"   = "Generator annual net generation (MWh)",
                "GENNTOZ"   = "Generator ozone season net generation (MWh)",
                "GENERSRC"  = "Generation data source",
                "GENYRONL"  = "Generator year on-line",
                "GENYRRET"  = "Generator planned or actual retirement year")

gen_header <- names(gen_labels)  # column names
gen_desc   <- unname(gen_labels) # description of column names

# add new column names
colnames(gen_file) <- gen_header

## write data
# write data for first row only
writeData(wb, 
          sheet = gen, 
          t(gen_desc), 
          startRow = 1, 
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = gen, 
          gen_file,
          startRow = 2)

## add styles
# add description styles
addStyle(wb, sheet = gen, style = s[['desc_style']],  rows = 1, cols = 1:12,  gridExpand = TRUE)
addStyle(wb, sheet = gen, style = s[['color1_desc']], rows = 1, cols = 13:14, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = s[['desc_style']],  rows = 1, cols = 15:17, gridExpand = TRUE)

# add header style
addStyle(wb, sheet = gen, style = s[['header_style']],  rows = 2, cols = 1:12,  gridExpand = TRUE)
addStyle(wb, sheet = gen, style = s[['color1_header']], rows = 2, cols = 13:14, gridExpand = TRUE)
addStyle(wb, sheet = gen, style = s[['header_style']],  rows = 2, cols = 15:17, gridExpand = TRUE)

# set column widths
setColWidths(wb, sheet = gen, cols = 1,     widths = 12.57)
setColWidths(wb, sheet = gen, cols = 3,     widths = 12.43)
setColWidths(wb, sheet = gen, cols = 4,     widths = 34.71)
setColWidths(wb, sheet = gen, cols = 5:7,   widths = 12.57)
setColWidths(wb, sheet = gen, cols = 8:10,  widths = 12.43)
setColWidths(wb, sheet = gen, cols = 11,    widths = 12.57)
setColWidths(wb, sheet = gen, cols = 12:14, widths = 13.14)
setColWidths(wb, sheet = gen, cols = 15,    widths = 29.43)
setColWidths(wb, sheet = gen, cols = 16,    widths = 12.29)
setColWidths(wb, sheet = gen, cols = 17,    widths = 15.14)

# set row heights
setRowHeights(wb, sheet = gen, row = 1, heights = 60.75)

# add number styles
addStyle(wb, sheet = gen, style = s[['integer']],  rows = 3:gen_rows, cols = 7,     gridExpand = TRUE)
addStyle(wb, sheet = gen, style = s[['decimal2']], rows = 3:gen_rows, cols = 11,    gridExpand = TRUE)
addStyle(wb, sheet = gen, style = s[['decimal3']], rows = 3:gen_rows, cols = 12,    gridExpand = TRUE)
addStyle(wb, sheet = gen, style = s[['integer2']], rows = 3:gen_rows, cols = 13:14, gridExpand = TRUE)

# add text style
addStyle(wb, sheet = gen, style = s[['basic']], rows = 3:gen_rows, cols = 1:6,     gridExpand = TRUE)
addStyle(wb, sheet = gen, style = s[['basic']], rows = 3:gen_rows, cols = 6:10,    gridExpand = TRUE)
addStyle(wb, sheet = gen, style = s[['basic']], rows = 3:gen_rows, cols = 14:17,   gridExpand = TRUE)

# freeze panes
freezePane(wb, sheet = gen, firstActiveCol = 7, firstActiveRow = 3)

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

# create sqtplt name using year
seqplt_label <- setNames("Plant file sequence number", glue::glue("SEQPLT{year}"))

# select number of rows from data frame
# add two to number of rows (nrows) to account for header + description rows
plnt_rows <- nrow(plnt_file) + 2 

## column names and descriptions
plnt_labels <- c(seqplt_label, 
                 "YEAR"      = "Data Year",
                 "PSTATABB"  = "Plant state abbreviation",
                 "PNAME"     = "Plant name",
                 "ORISPL"    = "DOE/EIA ORIS plant or facility code",
                 "OPRNAME"   = "Plant transmission or distribution system owner name",
                 "OPRCODE"   = "Plant transmission or distribution system owner ID",
                 "UTLSRVNM"  = "Utility name",
                 "UTLSRVID"  = "Utility ID",
                 "SECTOR"    = "Plant-level sector",
                 "BANAME"    = "Balancing Authority Name",
                 "BACODE"    = "Balancing Authority Code",
                 "NERC"      = "NERC region acronym",
                 "SUBRGN"    = "eGRID subregion acronym",
                 "SRNAME"    = "eGRID subregion name",
                 "ISORTO"    = "Plant associated ISO/RTO Territory",
                 "FIPSST"    = "Plant FIPS state code",
                 "FIPSCNTY"  = "Plant FIPS county code",
                 "CNTYNAME"  = "Plant county name",
                 "LAT"       = "Plant latitude",
                 "LON"       = "Plant longitude",
                 "CAMDFLAG"  = "CAMD Program Flag",
                 "NUMUNT"    = "Number of units",
                 "NUMGEN"    = "Number of generators",
                 "PLPRMFL"   = "Plant primary fuel",
                 "PLFUELCT"  = "Plant primary fuel category",
                 "COALFLAG"  = "Flag indicating if the plant burned or generated any amount of coal",
                 "CAPFAC"    = "Plant capacity factor",
                 "NAMEPCAP"  = "Plant nameplate capacity (MW)",
                 "NBFACTOR"  = "Nonbaseload Factor",
                 "RMBMFLAG"  = "Biogas/ biomass plant adjustment flag",
                 "CHPFLAG"   = "Combined heat and power (CHP) plant adjustment flag",
                 "USETHRMO"  = "CHP plant useful thermal output (MMBtu)",
                 "PWRTOHT"   = "CHP plant power to heat ratio",
                 "ELCALLOC"  = "CHP plant electric allocation factor",
                 "PSFLAG"    = "Plant pumped storage flag",
                 "PLHTIAN"   = "Plant annual heat input from combustion (MMBtu)",
                 "PLHTIOZ"   = "Plant ozone season heat input from combustion (MMBtu)",
                 "PLHTIANT"  = "Plant total annual heat input (MMBtu)",
                 "PLHTIOZT"  = "Plant total ozone season heat input (MMBtu)",
                 "PLNGENAN"  = "Plant annual net generation (MWh)",
                 "PLNGENOZ"  = "Plant ozone season net generation (MWh)",
                 "PLNGENNB"  = "Plant annual nonbaseload generation (MWh)", 
                 "PLNOXAN"   = "Plant annual NOx emissions (tons)",
                 "PLNOXOZ"   = "Plant ozone season NOx emissions (tons)",
                 "PLSO2AN"   = "Plant annual SO2 emissions (tons)",
                 "PLCO2AN"   = "Plant annual CO2 emissions (tons)",
                 "PLCH4AN"   = "Plant annual CH4 emissions (lbs)",
                 "PLN2OAN"   = "Plant annual N2O emissions (lbs)",
                 "PLCO2EQA"  = "Plant annual CO2 equivalent emissions (tons)",
                 "PLHGAN"    = "Plant annual Hg emissions (lbs)",
                 "PLNOXRTA"  = "Plant annual NOx total output emission rate (lb/MWh)",
                 "PLNOXRTO"  = "Plant ozone season NOx total output emission rate (lb/MWh)",
                 "PLSO2RTA"  = "Plant annual SO2 total output emission rate (lb/MWh)",
                 "PLCO2RTA"  = "Plant annual CO2 total output emission rate (lb/MWh)",
                 "PLCH4RTA"  = "Plant annual CH4 total output emission rate (lb/MWh)",
                 "PLN2ORTA"  = "Plant annual N2O total output emission rate (lb/MWh)",
                 "PLC2ERTA"  = "Plant annual CO2 equivalent total output emission rate (lb/MWh)",
                 "PLHGRTA"   = "Plant annual Hg total output emission rate (lb/MWh)",
                 "PLNOXRA"   = "Plant annual NOx input emission rate (lb/MMBtu)",
                 "PLNOXRO"   = "Plant ozone season NOx input emission rate (lb/MMBtu)",
                 "PLSO2RA"   = "Plant annual SO2 input emission rate (lb/MMBtu)",
                 "PLCO2RA"   = "Plant annual CO2 input emission rate (lb/MMBtu)",
                 "PLCH4RA"   = "Plant annual CH4 input emission rate (lb/MMBtu)",
                 "PLN2ORA"   = "Plant annual N2O input emission rate (lb/MMBtu)",
                 "PLC2ERA"   = "Plant annual CO2 equivalent input emission rate (lb/MMBtu)",
                 "PLHGRA"    = "Plant annual Hg input emission rate (lb/MMBtu)",
                 "PLNOXCRT"  = "Plant annual NOx combustion output emission rate (lb/MWh)",
                 "PLNOXCRO"  = "Plant ozone season NOx combustion output emission rate (lb/MWh)",
                 "PLSO2CRT"  = "Plant annual SO2 combustion output emission rate (lb/MWh)",
                 "PLCO2CRT"  = "Plant annual CO2 combustion output emission rate (lb/MWh)",
                 "PLCH4CRT"  = "Plant annual CH4 combustion output emission rate (lb/MWh)",
                 "PLN2OCRT"  = "Plant annual N2O combustion output emission rate (lb/MWh)",
                 "PLC2ECRT"  = "Plant annual CO2 equivalent combustion output emission rate (lb/MWh)",
                 "PLHGCRT"   = "Plant annual Hg combustion output emission rate (lb/MWh)",
                 "UNNOX"     = "Plant unadjusted annual NOx emissions (tons)",
                 "UNNOXOZ"   = "Plant unadjusted ozone season NOx emissions (tons)",
                 "UNSO2"     = "Plant unadjusted annual SO2 emissions (tons)", 
                 "UNCO2"     = "Plant unadjusted annual CO2 emissions (tons)",
                 "UNCH4"     = "Plant unadjusted annual CH4 emissions (lbs)",
                 "UNN2O"     = "Plant unadjusted annual N2O emissions (lbs)",
                 "UNCO2E"    = "Plant unadjusted annual CO2 equivalent emissions (tons)", 
                 "UNHG"      = "Plant unadjusted annual Hg emissions (lbs)",
                 "UNHTI"     = "Plant unadjusted annual heat input from combustion (MMBtu)",
                 "UNHTIOZ"   = "Plant unadjusted ozone season heat input from combustion (MMBtu)",
                 "UNHTIT"    = "Plant unadjusted total annual heat input (MMBtu)",
                 "UNHTIOZT"  = "Plant unadjusted total ozone season heat input (MMBtu)",
                 "UNNOXSRC"  = "Plant unadjusted annual NOx emissions source",
                 "UNNOZSRC"  = "Plant unadjusted ozone season NOx emissions source",
                 "UNSO2SRC"  = "Plant unadjusted annual SO2 emissions source",
                 "UNCO2SRC"  = "Plant unadjusted annual CO2 emissions source",
                 "UNCH4SRC"  = "Plant unadjusted annual CH4 emissions source",
                 "UNN2OSRC"  = "Plant unadjusted annual N2O emissions source",
                 "UNC2ESRC"  = "Plant unadjusted annual CO2 equivalent emissions source", 
                 "UNHGSRC"   = "Plant unadjusted annual Hg emissions source",
                 "UNHTISRC"  = "Plant unadjusted annual heat input source",
                 "UNHOZSRC"  = "Plant unadjusted ozone season heat input source",
                 "BIONOX"    = "Plant annual NOx biomass emissions (tons)",
                 "BIONOXOZ"  = "Plant ozone season NOx biomass emissions (tons)",
                 "BIOSO2"    = "Plant annual SO2 biomass emissions (tons)",
                 "BIOCO2"    = "Plant annual CO2 biomass emissions (tons)",
                 "BIOCH4"    = "Plant annual CH4 biomass emissions (lbs)",
                 "BION2O"    = "Plant annual N2O biomass emissions (lbs)",
                 "BIOCO2E"   = "Plant annual CO2 equivalent biomass emissions (tons)", 
                 "CHPCHTI"   = "Plant combustion heat input CHP adjustment value (MMBtu)",
                 "CHPCHTIOZ" = "Plant combustion annual ozone season heat input CHP adjustment value (MMBtu)",
                 "CHPNOX"    = "Plant annual NOx emissions CHP adjustment value (tons)",
                 "CHPNOXOZ"  = "Plant ozone season NOx emissions CHP adjustment value (tons)",
                 "CHPSO2"    = "Plant annual SO2 emissions CHP adjustment value (tons)",
                 "CHPCO2"    = "Plant annual CO2 emissions CHP adjustment value (tons)",
                 "CHPCH4"    = "Plant annual CH4 emissions CHP adjustment value (lbs)", 
                 "CHPN2O"    = "Plant annual N2O emissions CHP adjustment value (lbs)",
                 "CHPCO2E"   = "Plant annual CO2 equivalent emissions CHP adjustement value (tons)", 
                 "PLHTRT"    = "Plant nominal heat rate (Btu/kWh)",
                 "PLGENACL"  = "Plant annual coal net generation (MWh)",
                 "PLGENAOL"  = "Plant annual oil net generation (MWh)",
                 "PLGENAGS"  = "Plant annual gas net generation (MWh)",
                 "PLGENANC"  = "Plant annual nuclear net generation (MWh)",
                 "PLGENAHY"  = "Plant annual hydro net generation (MWh)",
                 "PLGENABM"  = "Plant annual biomass net generation (MWh)",
                 "PLGENAWI"  = "Plant annual wind net generation (MWh)",
                 "PLGENASO"  = "Plant annual solar net generation (MWh)",
                 "PLGENAGT"  = "Plant annual geothermal net generation (MWh)",
                 "PLGENAOF"  = "Plant annual other fossil net generation (MWh)",
                 "PLGENAOP"  = "Plant annual other unknown/ purchased fuel net generation (MWh)",
                 "PLGENATN"  = "Plant annual total nonrenewables net generation (MWh)",
                 "PLGENATR"  = "Plant annual total renewables net generation (MWh)",
                 "PLGENATO"  = "Plant annual total renewables other unknown/purchased net generation (MWh)", 
                 "PLGENATH"  = "Plant annual total nonhydro renewables net generation (MWh)",
                 "PLGENACY"  = "Plant annual total combustion net generation (MWh)",
                 "PLGENACN"  = "Plant annual total noncombustion net generation (MWh)",
                 "PLGENACO"  = "Plant annual total noncombustion other unknown/purchased net generation (MWh)", 
                 "PLCLPR"    = "Plant coal generation percent (resource mix)",
                 "PLOLPR"    = "Plant oil generation percent (resource mix)",
                 "PLGSPR"    = "Plant gas generation percent (resource mix)",
                 "PLNCPR"    = "Plant nuclear generation percent (resource mix)",
                 "PLHYPR"    = "Plant hydro generation percent (resource mix)",
                 "PLBMPR"    = "Plant biomass generation percent (resource mix)",
                 "PLWIPR"    = "Plant wind generation percent (resource mix)",
                 "PLSOPR"    = "Plant solar generation percent (resource mix)",
                 "PLGTPR"    = "Plant geothermal generation percent (resource mix)",
                 "PLOFPR"    = "Plant other fossil generation percent (resource mix)",
                 "PLOPPR"    = "Plant other unknown / purchased fuel generation percent (resource mix)",
                 "PLTNPR"    = "Plant total nonrenewables generation percent (resource mix)",
                 "PLTRPR"    = "Plant total renewables generation percent (resource mix)",
                 "PLTOPR"    = "Plant total nonrenewables other unknown/purchased generation percent (resource mix)", 
                 "PLTHPR"    = "Plant total nonhydro renewables generation percent (resource mix)",
                 "PLCYPR"    = "Plant total combustion generation percent (resource mix)",
                 "PLCNPR"    = "Plant total noncombustion generation percent (resource mix)", 
                 "PLCOPR"    = "Plant total noncombustion other unknown/purchased generation percent (resource mix)")

plnt_header <- names(plnt_labels)  # column names
plnt_desc   <- unname(plnt_labels) # description of column names

# add new column names                 
colnames(plnt_file) <- plnt_header

## write data
# write data for first row only
writeData(wb, 
          sheet = plnt, 
          t(plnt_desc), 
          startRow = 1, 
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = plnt, 
          plnt_file,
          startRow = 2)

## add styles to document
# add description styles
addStyle(wb, sheet = plnt, style = s[['desc_style']],     rows = 1, cols = 1:36,    gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color1_desc']],    rows = 1, cols = 37:51,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color4_desc']],    rows = 1, cols = 52:59,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color5_desc']],    rows = 1, cols = 60:67,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color6_desc']],    rows = 1, cols = 68:75,   gridExpand = TRUE) 
addStyle(wb, sheet = plnt, style = s[['color2_desc']],    rows = 1, cols = 76:97,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color3_desc']],    rows = 1, cols = 98:113,  gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['desc_style']],     rows = 1, cols = 114,     gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color7_desc']],    rows = 1, cols = 115:125, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color8_desc']],    rows = 1, cols = 126:128, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color9_desc']],    rows = 1, cols = 129,     gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color9v2_desc']],  rows = 1, cols = 130:132, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color10_desc']],   rows = 1, cols = 133:143, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color11_desc']],   rows = 1, cols = 144:146, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color12_desc']],   rows = 1, cols = 147,     gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color12v2_desc']], rows = 1, cols = 148:150, gridExpand = TRUE)

# add header styles
addStyle(wb, sheet = plnt, style = s[['header_style']],     rows = 2, cols = 1:36,    gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color1_header']],    rows = 2, cols = 37:51,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color4_header']],    rows = 2, cols = 52:59,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color5_header']],    rows = 2, cols = 60:67,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color6_header']],    rows = 2, cols = 68:75,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color2_header']],    rows = 2, cols = 76:97,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color3_header']],    rows = 2, cols = 98:113,  gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['header_style']],     rows = 2, cols = 114,     gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color7_header']],    rows = 2, cols = 115:125, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color8_header']],    rows = 2, cols = 126:128, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color9_header']],    rows = 2, cols = 129,     gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color9v2_header']],  rows = 2, cols = 130:132, gridExpand = TRUE) 
addStyle(wb, sheet = plnt, style = s[['color10_header']],   rows = 2, cols = 133:143, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color11_header']],   rows = 2, cols = 144:146, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color12_header']],   rows = 2, cols = 147,     gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['color12v2_header']], rows = 2, cols = 148:150, gridExpand = TRUE)

# set column widths
setColWidths(wb, sheet = plnt, cols = 1:2,     widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 3,       widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 4,       widths = 34.71)
setColWidths(wb, sheet = plnt, cols = 5,       widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 6,       widths = 32.14)
setColWidths(wb, sheet = plnt, cols = 7,       widths = 12.57)
setColWidths(wb, sheet = plnt, cols = 8,       widths = 35)
setColWidths(wb, sheet = plnt, cols = 9,       widths = 12.57)
setColWidths(wb, sheet = plnt, cols = 10,      widths = 14.86)
setColWidths(wb, sheet = plnt, cols = 11,      widths = 32)
setColWidths(wb, sheet = plnt, cols = 12:14,   widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 15,      widths = 17.29)
setColWidths(wb, sheet = plnt, cols = 16:22,   widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 23,      widths = 12.57)
setColWidths(wb, sheet = plnt, cols = 24,      widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 25:26,   widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 27,      widths = 12.57)
setColWidths(wb, sheet = plnt, cols = 28,      widths = 13.14)
setColWidths(wb, sheet = plnt, cols = 29,      widths = 13.29)
setColWidths(wb, sheet = plnt, cols = 30,      widths = 13)
setColWidths(wb, sheet = plnt, cols = 31:33,   widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 34,      widths = 13.29)
setColWidths(wb, sheet = plnt, cols = 35:54,   widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 55,      widths = 13.14)
setColWidths(wb, sheet = plnt, cols = 56:57,   widths = 12.86)
setColWidths(wb, sheet = plnt, cols = 58:77,   widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 78,      widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 79,      widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 80,      widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 81,      widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 82,      widths = 15.09)
setColWidths(wb, sheet = plnt, cols = 83,      widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 84,      widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 85,      widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 86:88,   widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 89,      widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 90,      widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 91:102,  widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 103,     widths = 14)
setColWidths(wb, sheet = plnt, cols = 104:116, widths = 12.43)
setColWidths(wb, sheet = plnt, cols = 117:127, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 128,     widths = 16.57)
setColWidths(wb, sheet = plnt, cols = 129:131, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 132,     widths = 16.57)
setColWidths(wb, sheet = plnt, cols = 133:145, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 146,     widths = 16.29)
setColWidths(wb, sheet = plnt, cols = 147:149, widths = 12.71)
setColWidths(wb, sheet = plnt, cols = 150,     widths = 16.29)

# set row heights
setRowHeights(wb, sheet = plnt, row = 1, heights = 67.5)

# add number styles
addStyle(wb, sheet = plnt, style = s[['integer']],  rows = 3:plnt_rows, cols = 23:24,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['decimal4']], rows = 3:plnt_rows, cols = 28,      gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['decimal2']], rows = 3:plnt_rows, cols = 29,      gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['decimal4']], rows = 3:plnt_rows, cols = 30,      gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['integer']],  rows = 3:plnt_rows, cols = 33:34,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['decimal4']], rows = 3:plnt_rows, cols = 35,      gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['integer2']], rows = 3:plnt_rows, cols = 37:50,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['decimal1']], rows = 3:plnt_rows, cols = 51:74,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['integer']],  rows = 3:plnt_rows, cols = 75:85,   gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['integer']],  rows = 3:plnt_rows, cols = 95:108,  gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['decimal3']], rows = 3:plnt_rows, cols = 109,     gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['integer2']], rows = 3:plnt_rows, cols = 110:132, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['percent']],  rows = 3:plnt_rows, cols = 133:150, gridExpand = TRUE)

# add text styles
addStyle(wb, sheet = plnt, style = s[['basic']], rows = 3:plnt_rows, cols = 1:22,  gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['basic']], rows = 3:plnt_rows, cols = 25:27, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['basic']], rows = 3:plnt_rows, cols = 31:32, gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['basic']], rows = 3:plnt_rows, cols = 36,    gridExpand = TRUE)
addStyle(wb, sheet = plnt, style = s[['basic']], rows = 3:plnt_rows, cols = 86:94, gridExpand = TRUE)

# freeze panes
freezePane(wb, sheet = plnt, firstActiveCol = 6, firstActiveRow = 3)

### ST Formatting -----

## create "ST" sheet
st <- glue::glue("ST{year}")
addWorksheet(wb, st)

# convert variables to numeric values
st_file <- st_file %>%
           mutate(year = as.numeric(year))

# select number of rows from data frame
# add two to number of rows (nrows) to account for header + description rows
st_rows <- nrow(st_file) + 2 

## column names and descriptions
# column names
st_header <- c("YEAR",
               "PSTATABB",
               "FIPSST",
               paste0("ST", standard_header))

# description of column names
st_desc <- c("Data Year",
             "State abbreviation",
             "FIPS State code",
             paste0("State ", standard_desc))

# add new column names
colnames(st_file) <- st_header

## write data
# write data for first row only
writeData(wb, 
          sheet = st, 
          t(st_desc),
          startRow = 1, 
          colNames = FALSE)

# write data to sheet
writeData(wb,
          sheet = st,
          st_file,
          startRow = 2)

## add styles to document
format_region(st, st_rows)


### BA Formatting -----

## create "BA" sheet
ba <- glue::glue("BA{year}")
addWorksheet(wb, ba)

# select number of rows from data frame
# add two to number of rows (nrows) to account for header + description rows
ba_rows <- nrow(ba_file) + 2 

# convert variables to numeric value
ba_file <- ba_file %>%
           mutate(year = as.numeric(year))


## column names and descriptions
# column names
ba_header <- c("YEAR",	
               "BANAME",	
               "BACODE",
               paste0("BA", standard_header))

# description of column names
ba_desc <- c("Data Year",
             "Balancing Authority Name",
             "Balancing Authority Code",
             paste0("BA ", standard_desc))

# add new column names
colnames(ba_file) <- ba_header

## write data
# write data for first row only
writeData(wb, 
          sheet = ba, 
          t(ba_desc), 
          startRow = 1, 
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = ba, 
          ba_file,
          startRow = 2)

## add styles to document
format_region(ba, ba_rows)

setColWidths(wb, sheet = ba, cols = 2, widths = 75.55)


### SRL Formatting -----

## create "SRL" sheet
srl <- glue::glue("SRL{year}")
addWorksheet(wb, srl)

# convert variables to numeric value
srl_file <- srl_file %>%
            mutate(year = as.numeric(year))

# select number of rows from data frame
# add two to number of rows (nrows) to account for header + description rows
srl_rows <- nrow(srl_file) + 2

## column names and descriptions
# column names
srl_header <- c("YEAR",	
               "SUBRGN",	
               "SRNAME",
               paste0("SR", standard_header))

# description of column names
srl_desc <- c("Data Year",
             "eGRID subregion acronym",
             "eGRID subregion name",
             paste0("eGRID subregion ", standard_desc))

# add new column names
colnames(srl_file) <- srl_header

## write data
# write data for first row only
writeData(wb, 
          sheet = srl, 
          t(srl_desc), 
          startRow = 1, 
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = srl, 
          srl_file,
          startRow = 2)

## add styles to document
format_region(srl, srl_rows)

setColWidths(wb, sheet = srl, cols = 3, widths = 18.45)


### NRL Formatting -----

## create "NRL" sheet
nrl <- glue::glue("NRL{year}")
addWorksheet(wb, nrl)

# convert variables to numeric value
nrl_file <- nrl_file %>%
            mutate(year = as.numeric(year))

# select number of rows from data frame
# add two to number of rows (nrows) to account for header + description rows
nrl_rows <- nrow(nrl_file) + 2

## column names and descriptions
# column names
nrl_header <- c("YEAR",	
                "NERC",	
                "NERCNAME",
                paste0("NR", standard_header))

# description of column names
nrl_desc <- c("Data Year",
              "NERC region acronym",
              "NERC region name",
              paste0("NERC region ", standard_desc))

# add new column names
colnames(nrl_file) <- nrl_header

## write data
# write data for first row only
writeData(wb, 
          sheet = nrl, 
          t(nrl_desc), 
          startRow = 1, 
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = nrl, 
          nrl_file,
          startRow = 2)

## add styles to document
format_region(nrl, nrl_rows)

setColWidths(wb, sheet = nrl, cols = 3, widths = 29.45)


### US Formatting -----

## create "US" sheet
us <- glue::glue("US{year}")
addWorksheet(wb, us)

# convert variables to numeric value
us_file <- us_file %>%
           mutate(year = as.numeric(year))

# select number of rows from data frame
# add two to number of rows (nrows) to account for header + description rows
us_rows <- nrow(us_file) + 2

## column names and descriptions
# column names
us_header <- c("YEAR",
               paste0("US", standard_header))

# description of column names
us_desc <- c("Data Year",
              paste0("U.S. ", standard_desc))

# add new column names
colnames(us_file) <- us_header

## write data
# write data for first row only
writeData(wb,
          sheet = us,
          t(us_desc),
          startRow = 1,
          colNames = FALSE)

# write data to sheet
writeData(wb,
          sheet = us,
          us_file,
          startRow = 2)

## add styles to document
format_region(us, us_rows)

### GGL Formatting -----

## create "GGL" sheet
ggl <- glue::glue("GGL{year}")
addWorksheet(wb, ggl)

# convert year to numeric value
ggl_file <- ggl_file %>%
            mutate(data_year = as.numeric(data_year))


## column names and descriptions
ggl_labels <- c("YEAR"     = "Data Year",
                "REGION"   = "One of the three interconnect power grids in the U.S. (plus Alaska, Hawaii, and the entire U.S.)",
                "ESTLOSS"  = "Estimated losses (MWh)",
                "TOTDISP"  = "Total disposition (MWh) without exports",
                "DIRCTUSE" = "Direct use (MWh)",
                "GGRSLOSS" = "Grid gross loss [Estimated losses/(Total disposition without exports - Direct use)]")

ggl_header <- names(ggl_labels)  # column names
ggl_desc   <- unname(ggl_labels) # description of column names

# add new column names
colnames(ggl_file) <- ggl_header

## write data
# write data for first row only
writeData(wb, 
          sheet = ggl, 
          t(ggl_desc), 
          startRow = 1, 
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = ggl, 
          ggl_file,
          startRow = 2)

## add styles to document
# add description styles
addStyle(wb, sheet = ggl, style = s[['desc_style']], rows = 1, cols = 1:6, gridExpand = TRUE)

# add header style
addStyle(wb, sheet = ggl, style = s[['header_style']], rows = 2, cols = 1:6, gridExpand = TRUE)

# set column widths
setColWidths(wb, sheet = ggl, cols = 1,   widths = 10)
setColWidths(wb, sheet = ggl, cols = 2,   widths = 21.29)
setColWidths(wb, sheet = ggl, cols = 3:5, widths = 11.14)
setColWidths(wb, sheet = ggl, cols = 6,   widths = 23)

# set row heights
setRowHeights(wb, sheet = ggl, row = 1, heights = 60.75)

# add number styles
addStyle(wb, sheet = ggl, style = s[['integer']], rows = 3:7, cols = 3:5, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = s[['percent']], rows = 3:7, cols = 6,   gridExpand = TRUE)

# add number styles (bold)
addStyle(wb, sheet = ggl, style = s[['integer_bold']], rows = 8, cols = 3:5, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = s[['percent_bold']], rows = 8, cols = 6,   gridExpand = TRUE)

# add text styles
addStyle(wb, sheet = ggl, style = s[['basic']], rows = 3:7, cols = 1:2, gridExpand = TRUE)
addStyle(wb, sheet = ggl, style = s[['bold']],  rows = 8,   cols = 1:2, gridExpand = TRUE)

### DEMO Formatting -----
# only build demographics file if the file exists in outputs
# this is because pulling data from the EJScreen API to build the demographics file takes several hours
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/demographics_file.RDS"))) {
  
  ## create "DEMO" sheet
  demo <- glue::glue("DEMO{year}")
  addWorksheet(wb, demo)
  
  # convert year to numeric value
  demo_file <- 
    demo_file %>%
    mutate(year = as.numeric(year))
  
  demo_rows <- nrow(demo_file) + 2
  
  ## column names and descriptions
  demo_labels <- c("SEQPLT"              = "Plant file sequence number",
                   "YEAR"                = "Data Year",
                   "PSTATABB"            = "Plant state abbreviation",
                   "PNAME"               = "Plant name",
                   "ORISPL"              = "DOE/EIA ORIS plant or facility code",
                   "LAT"                 = "Plant latitude",
                   "LON"                 = "Plant longitude",
                   "PLPRMFL"             = "Plant primary fuel", 
                   "PLFUELCT"            = "Plant primary fuel category",
                   "NAMEPCAP"            = "Plant nameplate capacity (MW)",
                   "COALFLAG"            = "Flag indicating if the plant burned or generated any amount of coal",
                   "TOTALPOP"            = "Total Population", 
                   "RAW_D_PEOPCOLOR"     = "People of Color (%)",
                   "RAW_D_INCOME"        = "Low Income (%)",
                   "RAW_D_LESSHS"        = "Less Than High School Education (%)",
                   "RAW_D_LING"          = "Limited English Speaking (%)",
                   "RAW_D_UNDER5"        = "Under Age 5 (%)",
                   "RAW_D_OVER64"        = "Over Age 64 (%)",
                   "RAW_D_UNEMPLOYED"    = "Unemployment Rate (%)",
                   "RAW_D_LIFEEXP"       = "Limited Life Expectancy (%)",
                   "RAW_D_DEMOGIDX2"     = "Demographic Index",
                   "RAW_D_DEMOGIDX5"     = "Supplemental Demographic Index",
                   "RAW_D_DEMOGIDX2ST"   = "State Demographic Index",
                   "RAW_D_DEMOGIDX5ST"   = "State Supplemental Demographic Index",
                   "S_D_PEOPCOLOR"       = "State Average of People of Color (%)",
                   "S_D_INCOME"          = "State Average of Low Income (%)",
                   "S_D_LESSHS"          = "State Average of Less Than High School Education (%)",
                   "S_D_LING"            = "State Average of Limited English Speaking (%)",
                   "S_D_UNDER5"          = "State Average of Under Age 5 (%)",
                   "S_D_OVER64"          = "State Average of Over Age 64 (%)", 
                   "S_D_UNEMPLOYED"      = "State Average of Unemployment Rate (%)",
                   "S_D_LIFEEXP"         = "State Average of Limited Life Expectancy (%)",
                   "S_D_DEMOGIDX2ST"     = "State Average of Demographic Index",
                   "S_D_DEMOGIDX5ST"     = "State Average of Supplemental Demographic Index",
                   "S_D_PEOPCOLOR_PER"   = "State Percentile of People of Color", 
                   "S_D_INCOME_PER"      = "State Percentile of Low Income",
                   "S_D_LESSHS_PER"      = "State Percentile of Less Than High School Education",
                   "S_D_LING_PER"        = "State Percentile of Limited English Speaking",
                   "S_D_UNDER5_PER"      = "State Percentile of Under Age 5",
                   "S_D_OVER64_PER"      = "State Percentile of Over Age 64",
                   "S_D_UNEMPLOYED_PER"  = "State Percentile of Unemployment Rate",
                   "S_D_LIFEEXP_PER"     = "State Percentile of Limited Life Expectancy",        
                   "S_D_DEMOGIDX2ST_PER" = "State Percentile of Demographic Index",                                                 
                   "S_D_DEMOGIDX5ST_PER" = "State Percentile of Supplemental Demographic Index",
                   "N_D_PEOPCOLOR"       = "National Average of People of Color (%)",
                   "N_D_INCOME"          = "National Average of Low Income (%)",
                   "N_D_LESSHS"          = "National Average of Less Than High School Education (%)",
                   "N_D_LING"            = "National Average of Limited English Speaking (%)",
                   "N_D_UNDER5"          = "National Average of Under Age 5 (%)",
                   "N_D_OVER64"          = "National Average of Over Age 64 (%)",
                   "N_D_UNEMPLOYED"      = "National Average of Unemployment Rate (%)",
                   "N_D_LIFEEXP"         = "National Average of Limited Life Expectancy (%)",
                   "N_D_DEMOGIDX2"       = "National Average of Demographic Index",
                   "N_D_DEMOGIDX5"       = "National Average of Supplemental Demographic Index",
                   "N_D_MINOR_PER"       = "National Percentile of People of Color",
                   "N_D_INCOME_PER"      = "National Percentile of Low Income",
                   "N_D_LESSHS_PER"      = "National Percentile of Less Than High School Education",
                   "N_D_LING_PER"        = "National Percentile of Limited English Speaking",
                   "N_D_UNDER5_PER"      = "National Percentile of Under Age 5",
                   "N_D_OVER64_PER"      = "National Percentile of Over Age 64",
                   "N_D_UNEMPLOYED_PER"  = "National Percentile of Unemployment Rate",
                   "N_D_LIFEEXP_PER"     = "National Percentile of Limited Life Expectancy",
                   "N_D_DEMOGIDX2_PER"   = "National Percentile of Demographic Index",
                   "N_D_DEMOGIDX5_PER"   = "National Percentile of Supplemental Demographic Index",
                   "DISTANCE"            = "Distance (miles)")
  
  demo_header <- names(demo_labels)  # column names
  demo_desc   <- unname(demo_labels) # description of column names
  
  # add new column names
  colnames(demo_file) <- demo_header
  
  ## write data
  # write data for first row only
  writeData(wb, 
            sheet = demo, 
            t(demo_desc), 
            startRow = 1, 
            colNames = FALSE)
  
  # write data to sheet
  writeData(wb, 
            sheet = demo, 
            demo_file,
            startRow = 2)
  
  ## add styles to document
  # add description styles
  addStyle(wb, sheet = demo, style = s[['desc_style']], rows = 1, cols = 1:65, gridExpand = TRUE)
  
  # add header style
  addStyle(wb, sheet = demo, style = s[['header_style']], rows = 2, cols = 1:65, gridExpand = TRUE)
  
  # set column widths
  setColWidths(wb, sheet = demo, cols = 1:2,     widths = 12.71)
  setColWidths(wb, sheet = demo, cols = 3,       widths = 12.43)
  setColWidths(wb, sheet = demo, cols = 4,       widths = 34.71)
  setColWidths(wb, sheet = demo, cols = 5:10,    widths = 12.45)
  setColWidths(wb, sheet = demo, cols = 11,      widths = 12.55)
  setColWidths(wb, sheet = demo, cols = 12,      widths = 13)
  setColWidths(wb, sheet = demo, cols = 13,      widths = 18)
  setColWidths(wb, sheet = demo, cols = 14:18,   widths = 13)
  setColWidths(wb, sheet = demo, cols = 19,      widths = 17.18)
  setColWidths(wb, sheet = demo, cols = 20,      widths = 13)
  setColWidths(wb, sheet = demo, cols = 21:22,   widths = 15.55)
  setColWidths(wb, sheet = demo, cols = 23:24,   widths = 17.64)
  setColWidths(wb, sheet = demo, cols = 25:29,   widths = 13.84)
  setColWidths(wb, sheet = demo, cols = 30:34,   widths = 15)
  setColWidths(wb, sheet = demo, cols = 35,      widths = 18)
  setColWidths(wb, sheet = demo, cols = 36:40,   widths = 15)
  setColWidths(wb, sheet = demo, cols = 41,      widths = 18.57)
  setColWidths(wb, sheet = demo, cols = 42,      widths = 15)
  setColWidths(wb, sheet = demo, cols = 43:44,   widths = 18.86)
  setColWidths(wb, sheet = demo, cols = 45:60,   widths = 15)
  setColWidths(wb, sheet = demo, cols = 61,      widths = 20)
  setColWidths(wb, sheet = demo, cols = 62,      widths = 15)
  setColWidths(wb, sheet = demo, cols = 63:64,   widths = 18.29)
  setColWidths(wb, sheet = demo, cols = 65,      widths = 15)
  
  # set row heights
  setRowHeights(wb, sheet = demo, row = 1, heights = 67.5)

  # add number styles
  addStyle(wb, sheet = demo, style = s[['integer']],   rows = 3:demo_rows, cols = 12:20,  gridExpand = TRUE)
  addStyle(wb, sheet = demo, style = s[['decimal5']],  rows = 3:demo_rows, cols = 21:24,  gridExpand = TRUE)
  addStyle(wb, sheet = demo, style = s[['integer']],   rows = 3:demo_rows, cols = 25:32,  gridExpand = TRUE)
  addStyle(wb, sheet = demo, style = s[['decimal5']],  rows = 3:demo_rows, cols = 33:34,  gridExpand = TRUE)
  addStyle(wb, sheet = demo, style = s[['integer']],   rows = 3:demo_rows, cols = 35:52,  gridExpand = TRUE)
  addStyle(wb, sheet = demo, style = s[['decimal5']],  rows = 3:demo_rows, cols = 53:54,  gridExpand = TRUE)
  addStyle(wb, sheet = demo, style = s[['integer']],   rows = 3:demo_rows, cols = 55:65,  gridExpand = TRUE)
  
  # add text styles
  addStyle(wb, sheet = demo, style = s[['basic']], rows = 3:demo_rows, cols = 1:11, gridExpand = TRUE)
  
  # freeze panes
  freezePane(wb, sheet = demo, firstActiveCol = 6, firstActiveRow = 3)
}


### Contents Formatting -------------

# add link to sheets 
add_hyperlink(glue::glue("UNT{year}"),  row_link = 1, col_link = 1, loc = c(3, 9), text_to_show = glue::glue("UNT{year}"))
add_hyperlink(glue::glue("GEN{year}"),  row_link = 1, col_link = 1, loc = c(3, 10), text_to_show = glue::glue("GEN{year}"))
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 1, loc = c(3, 11), text_to_show = glue::glue("PLNT{year}"))
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 1, loc = c(3, 12), text_to_show = glue::glue("ST{year}"))
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 1, loc = c(3, 13), text_to_show = glue::glue("BA{year}"))
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 1, loc = c(3, 14), text_to_show = glue::glue("SRL{year}"))
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 1, loc = c(3, 15), text_to_show = glue::glue("NRL{year}"))
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 1, loc = c(3, 16), text_to_show = glue::glue("US{year}"))
add_hyperlink(glue::glue("GGL{year}"),  row_link = 1, col_link = 1, loc = c(3, 17), text_to_show = glue::glue("GGL{year}"))

if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/demographics_file.RDS"))) {
  add_hyperlink(glue::glue("DEMO{year}"),  row_link = 1, col_link = 1, loc = c(3, 18), text_to_show = glue::glue("DEMO{year}"))
}

# add hyperlinks to specific columns
# annual values 
add_hyperlink(glue::glue("GEN{year}"),  row_link = 1, col_link = 13, loc = c(11, 27), text_to_show = "GEN")
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 37, loc = c(12, 27), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 5, loc = c(13, 27), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 5, loc = c(14, 27), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 5, loc = c(15, 27), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 5, loc = c(16, 27), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 3, loc = c(17, 27), text_to_show = "US")

# unadjusted values 
add_hyperlink(glue::glue("UNT{year}"),  row_link = 1, col_link = 15, loc = c(10, 28), text_to_show = "UNT")
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 76, loc = c(12, 28), text_to_show = "PLNT")

# adjustment values (biomass and CHP)
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 98, loc = c(12, 29), text_to_show = "PLNT")

# output emissions rates 
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 52, loc = c(12, 30), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 20, loc = c(13, 30), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 20, loc = c(14, 30), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 20, loc = c(15, 30), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 20, loc = c(16, 30), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 18, loc = c(17, 30), text_to_show = "US")

# input emissions rates
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 60, loc = c(12, 31), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 28, loc = c(13, 31), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 28, loc = c(14, 31), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 28, loc = c(15, 31), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 28, loc = c(16, 31), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 26, loc = c(17, 31), text_to_show = "US")

# combustion output emissions rates
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 68, loc = c(12, 32), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 36, loc = c(13, 32), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 36, loc = c(14, 32), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 36, loc = c(15, 32), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 36, loc = c(16, 32), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 34, loc = c(17, 32), text_to_show = "US")

# generation by fuel type
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 115, loc = c(12, 33), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 112, loc = c(13, 33), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 112, loc = c(14, 33), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 112, loc = c(15, 33), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 112, loc = c(16, 33), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 110, loc = c(17, 33), text_to_show = "US")

# renewable and non-renewable generation
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 126, loc = c(12, 34), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 123, loc = c(13, 34), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 123, loc = c(14, 34), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 123, loc = c(15, 34), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 123, loc = c(16, 34), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 121, loc = c(17, 34), text_to_show = "US")

# combustion and non-combustion generation
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 130, loc = c(12, 35), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 127, loc = c(13, 35), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 127, loc = c(14, 35), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 127, loc = c(15, 35), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 127, loc = c(16, 35), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 125, loc = c(17, 35), text_to_show = "US")

# resource mix
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 133, loc = c(12, 36), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 130, loc = c(13, 36), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 130, loc = c(14, 36), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 130, loc = c(15, 36), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 130, loc = c(16, 36), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 128, loc = c(17, 36), text_to_show = "US")

# renewable and non-renewable resource mix
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 144, loc = c(12, 37), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 141, loc = c(13, 37), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 141, loc = c(14, 37), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 141, loc = c(15, 37), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 141, loc = c(16, 37), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 139, loc = c(17, 37), text_to_show = "US")

# combustion and non-combustion resource mix
add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 148, loc = c(12, 38), text_to_show = "PLNT")
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 145, loc = c(13, 38), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 145, loc = c(14, 38), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 145, loc = c(15, 38), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 145, loc = c(16, 38), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 143, loc = c(17, 38), text_to_show = "US")

# output emission rates by fuel type
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 44, loc = c(13, 39), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 44, loc = c(14, 39), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 44, loc = c(15, 39), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 44, loc = c(16, 39), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 42, loc = c(17, 39), text_to_show = "US")

# input emission rates by fuel type
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 74, loc = c(13, 40), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 74, loc = c(14, 40), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 74, loc = c(15, 40), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 74, loc = c(16, 40), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 72, loc = c(17, 40), text_to_show = "US")

# nonbaseload output emission rates 
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 104, loc = c(13, 41), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 104, loc = c(14, 41), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 104, loc = c(15, 41), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 104, loc = c(16, 41), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 102, loc = c(17, 41), text_to_show = "US")

# nonbaseload generation by fuel type
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 148, loc = c(13, 42), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 148, loc = c(14, 42), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 148, loc = c(15, 42), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 148, loc = c(16, 42), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 146, loc = c(17, 42), text_to_show = "US")

# nonbaseload resource mix
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 159, loc = c(13, 43), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 159, loc = c(14, 43), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 159, loc = c(15, 43), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 159, loc = c(16, 43), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 157, loc = c(17, 43), text_to_show = "US")

### Save and export -----
output <- glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx")
saveWorkbook(wb, output, overwrite = TRUE)

print(glue::glue("Saving final formatted file to folder data/outputs/{params$eGRID_year}/"))

# remove to save space
rm(unt_file, gen_file, plnt_file)


