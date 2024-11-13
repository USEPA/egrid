## -------------------------------
##
## Metric file formatting. 
## Call function format_styles and format_region_m to format the metric file. 
## 
## Purpose: 
## 
## This file formats all of the outputs created for eGRID 
## This includes all operating units for the specified eGRID data year
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------

# Load libraries --------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(openxlsx)

### Load in data ------ 

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


# load files (will need to be changed to for metric)
unt_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))
gen_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/generator_file.RDS"))
plnt_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS"))
st_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/state_aggregation.RDS"))
ba_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/ba_aggregation.RDS"))
srl_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/subregion_aggregation.RDS"))
nrl_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/nerc_aggregation.RDS"))
us_file   <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/us_aggregation.RDS"))
ggl_file  <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/grid_gross_loss.RDS"))


# extract last two digits of year for universal labeling
year <- as.numeric(params$eGRID_year) %% 1000

# set up output file
contents <- "data/static_tables/formatting/egrid_contents_page.xlsx" # may need adjustments for metric version
wb <- loadWorkbook(contents)

# load helper functions into script
source("scripts/functions/function_format_styles.R") # add once merge final formatting to development
source("scripts/functions/function_format_region_m.R") # create metric version of function

### Create styles ------

# create eGRID output style list using function
s <- create_format_styles() 

### Standard Column Names -----
# names for data sets: ST, BA, SRL, NRL, US
# data for region aggregated files contain same columns and information
# therefore, can assign a standardized list of columns, names, and styles

standard_labels <- c("NAMEPCAP" = "nameplate capacity (MW)",	
                     
                     "HTIAN"    = "annual heat input from combustion (GJ)",	
                     "HTIOZ"    = "annual heat input from combustion (GJ)",	
                     "HTIANT"   = "total annual heat input (GJ)",
                     "HTIOZT"   = "total ozone season heat input (GJ)",	
                     "NGENAN"   = "annual net generation (MWh)",	
                     "NGENAN2"  = "annual net generation (GJ)",
                     "NGENOZ"   = "ozone season net generation (MWh)",
                     "NGENOZ2"  = "ozone season net generation (GJ)",
                     "NOXAN"    = "annual NOx emissions (metric tons)",	
                     "NOXOZ"    = "ozone season NOx emissions (metric tons)",	
                     "SO2AN"    = "annual SO2 emissions (metric tons)",	
                     "CO2AN"    = "annual CO2 emissions (metric tons)",
                     "CH4AN"    = "annual CH4 emissions (kg)",	
                     "N2OAN"    = "annual N2O emissions (kg)",	
                     "CO2EQA"   = "annual CO2 equivalent emissions (metric tons)",	
                     "HGAN"     = "annual Hg emissions (kg)", # +2 rows
                     
                     "NOXRTA"   = "annual NOx total output emission rate (kg/MWh)",
                     "NOXRTA2"  = "annual NOx total output emission rate (kg/GJ)",
                     "NOXRTO"   = "ozone season NOx total output emission rate (kg/MWh)",
                     "NOXRTO2"  = "ozone season NOx total output emission rate (kg/GJ)",
                     "SO2RTA"   = "annual SO2 total output emission rate (kg/MWh)",
                     "SO2RTA2"  = "annual SO2 total output emission rate (kg/GJ)",
                     "CO2RTA"   = "annual CO2 total output emission rate (kg/MWh)",	
                     "CO2RTA2"  = "annual CO2 total output emission rate (kg/GJ)",
                     "CH4RTA"   = "annual CH4 total output emission rate (kg/MWh)",
                     "CH4RTA2"  = "annual CH4 total output emission rate (kg/GJ)",
                     "N2ORTA"   = "annual N2O total output emission rate (kg/MWh)",
                     "N2ORTA2"  = "annual N2O total output emission rate (kg/GJ)",
                     "C2ERTA"   = "annual CO2 equivalent total output emission rate (kg/MWh)",
                     "C2ERTA2"  = "annual CO2 equivalent total output emission rate (kg/GJ)",
                     "HGRTA"    = "annual Hg total output emission rate (kg/MWh)",
                     "HGRTA2"   = "annual Hg total output emission rate (kg/GJ)", # + 8 rows
                     
                     "NOXRA"    = "annual NOx input emission rate (kg/GJ)",
                     "NOXRO"    = "ozone season NOx input emission rate (kg/GJ)",
                     "SO2RA"    = "annual SO2 input emission rate (kg/GJ)",	
                     "CO2RA"    = "annual CO2 input emission rate (kg/GJ)",
                     "CH4RA"    = "annual CH4 input emission rate (kg/GJ)",	
                     "N2ORA"    = "annual N2O input emission rate (kg/GJ)",	
                     "C2ERA"    = "annual CO2 equivalent input emission rate (kg/GJ)",	
                     "HGRA"     = "annual Hg input emission rate (kg/GJ)",	
                     "NOXCRT"   = "annual NOx combustion output emission rate (kg/MWh)",
                     "NOXCRT2"  = "annuak NOx combustion output emission rate (kg/GJ)",
                     "NOXCRO"   = "ozone season NOx combustion output emission rate (kg/MWh)",
                     "NOXCRO2"  = "ozone season NOx combustion output emission rate (kg/GJ)",
                     "SO2CRT"   = "annual SO2 combustion output emission rate (kg/MWh)",
                     "SO2CRT2"  = "annual SO2 combustion output emission rate (kg/GJ)",
                     "CO2CRT"   = "annual CO2 combustion output emission rate (kg/MWh)",
                     "CO2CRT2"  = "annual CO2 combustion output emission rate (kg/GJ)",
                     "CH4CRT"   = "annual CH4 combustion output emission rate (kg/MWh)",
                     "CH4CRT2"  = "annual CH4 combustion output emission rate (kg/GJ)",
                     "N2OCRT"   = "annual N2O combustion output emission rate (kg/MWh)",
                     "N2OCRT2"  = "annual N2O combustion output emission rate (kg/GJ)",
                     "C2ECRT"   = "annual CO2 equivalent combustion output emission rate (kg/MWh)",
                     "C2ECRT2"  = "annual CO2 equivalent combustion output emission rate (kg/GJ)",
                     "HGCRT"    = "Hg combustion output emission rate (kg/MWh)",	
                     "HGCRT2"   = "Hg combustion output emission rate (kg/GJ)",  # +8 rows
                     
                     "CNOXRT"   = "annual NOx coal output emission rate (kg/MWh)",
                     "CNOXRT2"  = "annual NOx coal output emission rate (kg/GJ)",
                     "ONOXRT"   = "annual NOx oil output emission rate (kg/MWh)",
                     "ONOXRT2"  = "annual NOx oil output emission rate (kg/GJ)",
                     "GNOXRT"   = "annual NOx gas output emission rate (kg/MWh)",
                     "GNOXRT"   = "annual NOx gas output emission rate (kg/GJ)",
                     "FSNXRT"   = "annual NOx fossil fuel output emission rate (kg/MWh)",
                     "FSNXRT2"  = "annual NOx fossil fuel output emission rate (kg/GJ)",
                     "CNXORT"   = "ozone season NOx coal output emission rate (kg/MWh)",
                     "CNXORT2"  = "ozone season NOx coal output emission rate (kg/GJ)",
                     "ONXORT"   = "ozone season NOx oil output emission rate (kg/MWh)",	
                     "ONXORT2"  = "ozone season NOx oil output emission rate (kg/GJ)",
                     "GNXORT"   = "ozone season NOx gas output emission rate (kg/MWh)",
                     "GNXORT2"  = "ozone season NOx gas output emission rate (kg/GJ)",
                     "FSNORT"   = "ozone season NOx fossil fuel output emission rate (kg/MWh)",
                     "FSNORT2"  = "ozone season NOx fossil fuel output emission rate (kg/GJ)",
                     "CSO2RT"   = "annual SO2 coal output emission rate (kg/MWh)",
                     "CSO2RT2"  = "annual SO2 coal output emission rate (kg/GJ)",
                     "OSO2RT"   = "annual SO2 oil output emission rate (kg/MWh)",
                     "OSO2RT2"  = "annual SO2 oil output emission rate (kg/GJ)",
                     "GSO2RT"   = "annual SO2 gas output emission rate (kg/MWh)",	
                     "GSO2RT2"  = "annual SO2 gas output emission rate (kg/GJ)",	
                     "FSS2RT"   = "annual SO2 fossil fuel output emission rate (kg/MWh)",
                     "FSS2RT2"  = "annual SO2 fossil fuel output emission rate (kg/GJ)",
                     "CCO2RT"   = "annual CO2 coal output emission rate (kg/MWh)",	
                     "CCO2RT2"  = "annual CO2 coal output emission rate (kg/GJ)",	
                     "OCO2RT"   = "annual CO2 oil output emission rate (kg/MWh)",	
                     "OCO2RT2"  = "annual CO2 oil output emission rate (kg/GJ)",	
                     "GCO2RT"   = "annual CO2 gas output emission rate (kg/MWh)",	
                     "GCO2RT2"  = "annual CO2 gas output emission rate (kg/GJ)",
                     "FSC2RT"   = "annual CO2 fossil fuel output emission rate (kg/MWh)",	
                     "FSC2RT2"  = "annual CO2 fossil fuel output emission rate (kg/GJ)",	
                     "CCH4RT"   = "annual CH4 coal output emission rate (kg/MWh)",
                     "CCH4RT2"  = "annual CH4 coal output emission rate (kg/GJ)",
                     "OCH4RT"   = "annual CH4 oil output emission rate (kg/MWh)",	
                     "OCH4RT2"  = "annual CH4 oil output emission rate (kg/GJ)",	
                     "GCH4RT"   = "annual CH4 gas output emission rate (kg/MWh)",	
                     "GCH4RT2"  = "annual CH4 gas output emission rate (kg/GJ)",	
                     "FCH4RT"   = "annual CH4 fossil fuel output emission rate (kg/MWh)",	
                     "FCH4RT2"  = "annual CH4 fossil fuel output emission rate (kg/GJ)",	
                     "CN2ORT"   = "annual N2O coal output emission rate (kg/MWh)",
                     "CN2ORT2"  = "annual N2O coal output emission rate (kg/GJ)",
                     "ON2ORT"   = "annual N2O oil output emission rate (kg/MWh)",	
                     "ON2ORT2"  = "annual N2O oil output emission rate (kg/GJ)",
                     "GN2ORT"   = "annual N2O gas output emission rate (kg/MWh)",
                     "GN2ORT2"  = "annual N2O gas output emission rate (kg/GJ)",
                     "FN2ORT"   = "annual N2O fossil fuel output emission rate (kg/MWh)",
                     "FN2ORT2"  = "annual N2O fossil fuel output emission rate (kg/GJ)",	
                     "CC2ERT"   = "annual CO2 equivalent coal output emission rate (kg/MWh)",	
                     "CC2ERT2"  = "annual CO2 equivalent coal output emission rate (kg/GJ)",	
                     "OC2ERT"   = "annual CO2 equivalent oil output emission rate (kg/MWh)",
                     "OC2ERT2"  = "annual CO2 equivalent oil output emission rate (kg/GJ)",
                     "GC2ERT"   = "annual CO2 equivalent gas output emission rate (kg/MWh)",	
                     "GC2ERT2"  = "annual CO2 equivalent gas output emission rate (kg/GJ)",	
                     "FSC2ERT"  = "annual CO2 equivalent fossil fuel output emission rate (kg/MWh)",	
                     "FSC2ERT2" = "annual CO2 equivalent fossil fuel output emission rate (kg/GJ)",	
                     "CHGRT"    = "annual Hg coal output emission rate (kg/MWh)",
                     "CHGRT2"   = "annual Hg coal output emission rate (kg/GJ)",	
                     "FSHGRT"   = "annual Hg fossil fuel output emission rate (kg/MWh)",
                     "FSHGRT2"  = "annual Hg fossil fuel output emission rate (kg/GJ)", # + 30 rows
                     
                     "CNOXR"    = "annual NOx coal input emission rate (kg/GJ)",
                     "ONOXR"    = "annual NOx oil input emission rate (kg/GJ)",	
                     "GNOXR"    = "annual NOx gas input emission rate (kg/GJ)",	
                     "FSNXR"    = "annual NOx fossil fuel input emission rate (kg/GJ)",	
                     "CNXOR"    = "ozone season NOx coal input emission rate (kg/GJ)",	
                     "ONXOR"    = "ozone season NOx oil input emission rate (kg/GJ)",	
                     "GNXOR"    = "ozone season NOx gas input emission rate (kg/GJ)",	
                     "FSNOR"    = "ozone season NOx fossil fuel input emission rate (kg/GJ)",
                     "CSO2R"    = "annual SO2 coal input emission rate (kg/GJ)",	
                     "OSO2R"    = "annual SO2 oil input emission rate (kg/GJ)",	
                     "GSO2R"    = "annual SO2 gas input emission rate (kg/GJ)",	
                     "FSS2R"    = "annual SO2 fossil fuel input emission rate (kg/GJ)",	
                     "CCO2R"    = "annual CO2 coal input emission rate (kg/GJ)",	
                     "OCO2R"    = "annual CO2 oil input emission rate (kg/GJ)",	
                     "GCO2R"    = "annual CO2 gas input emission rate (kg/GJ)",	
                     "FSC2R"    = "annual CO2 fossil fuel input emission rate (kg/GJ)",	
                     "CCH4R"    = "annual CH4 coal input emission rate (kg/GJ)",	
                     "OCH4R"    = "annual CH4 oil input emission rate (kg/GJ)",	
                     "GCH4R"    = "annual CH4 gas input emission rate (kg/GJ)",
                     "FCH4R"    = "annual CH4 fossil fuel input emission rate (kg/GJ)",	
                     "CN2OR"    = "annual N2O coal input emission rate (kg/GJ)",
                     "ON2OR"    = "annual N2O oil input emission rate (kg/GJ)",	
                     "GN2OR"    = "annual N2O gas input emission rate (kg/GJ)",	
                     "FN2OR"    = "annual N2O fossil fuel input emission rate (kg/GJ)",	
                     "CC2ER"    = "annual CO2 equivalent coal input emission rate (kg/GJ)",	
                     "OC2ER"    = "annual CO2 equivalent oil input emission rate (kg/GJ)",	
                     "GC2ER"    = "annual CO2 equivalent gas input emission rate (kg/GJ)",	
                     "FSC2ER"   = "annual CO2 equivalent fossil fuel input emission rate (kg/GJ)",	
                     "CHGR"     = "annual Hg coal input emission rate (kg/GJ)",	
                     "FSHGR"    = "annual Hg fossil fuel input emission rate (kg/GJ)",	# + 0 rows
                     
                     "NBNOX"    = "annual NOx non-baseload output emission rate (kg/MWh)",	
                     "NBNOX2"   = "annual NOx non-baseload output emission rate (kg/GJ)",	
                     "NBNXO"    = "ozone season NOx non-baseload output emission rate (kg/MWh)",
                     "NBNXO2"   = "ozone season NOx non-baseload output emission rate (kg/GJ)",	
                     "NBSO2"    = "annual SO2 non-baseload output emission rate (kg/MWh)",	
                     "NBSO22"   = "annual SO2 non-baseload output emission rate (kg/GJ)",	
                     "NBCO2"    = "annual CO2 non-baseload output emission rate (kg/MWh)",
                     "NBCO22"   = "annual CO2 non-baseload output emission rate (kg/GJ)",
                     "NBCH4"    = "annual CH4 non-baseload output emission rate (kg/MWh)",	
                     "NBCH42"   = "annual CH4 non-baseload output emission rate (kg/GJ)",
                     "NBN2O"    = "annual N2O non-baseload output emission rate (kg/MWh)",	
                     "NBN2O2"   = "annual N2O non-baseload output emission rate (kg/GJ)",	
                     "NBC2E"    = "annual CO2 equivalent non-baseload output emission rate (kg/MWh)",	
                     "NBC2E2"   = "annual CO2 equivalent non-baseload output emission rate (kg/GJ)",	
                     "NBHG"     = "annual Hg non-baseload output emission rate (kg/MWh)",
                     "NBHG2"    = "annual Hg non-baseload output emission rate (kg/GJ)", # + 8 rows
                     
                     "GENACL"   = "annual coal net generation (MWh)",	
                     "GENACL2"  = "annual coal net generation (GJ)",	
                     "GENAOL"   = "annual oil net generation (MWh)",	
                     "GENAOL2"  = "annual oil net generation (GJ)",
                     "GENAGS"   = "annual gas net generation (MWh)",	
                     "GENAGS2"  = "annual gas net generation (GJ)",	
                     "GENANC"   = "annual nuclear net generation (MWh)",	
                     "GENANC2"  = "annual nuclear net generation (GJ)",	
                     "GENAHY"   = "annual hydro net generation (MWh)",	
                     "GENAHY2"  = "annual hydro net generation (GJ)",
                     "GENABM"   = "annual biomass net generation (MWh)",
                     "GENABM2"  = "annual biomass net generation (GJ)",
                     "GENAWI"   = "annual wind net generation (MWh)",	
                     "GENAWI2"  = "annual wind net generation (GJ)",	
                     "GENASO"   = "annual solar net generation (MWh)",
                     "GENASO2"  = "annual solar net generation (GJ)",
                     "GENAGT"   = "annual geothermal net generation (MWh)",	
                     "GENAGT2"  = "annual geothermal net generation (GJ)",
                     "GENAOF"   = "annual other fossil net generation (MWh)",	
                     "GENAOF2"  = "annual other fossil net generation (GJ)",	
                     "GENAOP"   = "annual other unknown/ purchased fuel net generation (MWh)",
                     "GENAOP2"  = "annual other unknown/ purchased fuel net generation (GJ)", # + 11 rows
                     
                     "GENATN"   = "annual total nonrenewables net generation (MWh)",	
                     "GENATN2"  = "annual total nonrenewables net generation (GJ)",	
                     "GENATR"   = "annual total renewables net generation (MWh)",	
                     "GENATR2"  = "annual total renewables net generation (GJ)",	#  +2 rows
                     
                     "GENATH"   = "annual total nonhydro renewables net generation (MWh)",
                     "GENATH2"   = "annual total nonhydro renewables net generation (GJ)", # +1
                     
                     "GENACY"   = "annual total combustion net generation (MWh)",
                     "GENACY2"  = "annual total combustion net generation (GJ)",
                     "GENACN"   = "annual total noncombustion net generation (MWh)",	
                     "GENACN2"  = "annual total noncombustion net generation (GJ)", # +2 rows
                     
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
                     
                     "THPR"     = "total nonhydro renewables generation percent (resource mix)",	
                     
                     "CYPR"     = "total combustion generation percent (resource mix)",	
                     "CNPR"     = "total noncombustion generation percent (resource mix)",	
                     
                     "NBGNCL"   = "annual nonbaseload coal net generation (MWh)",
                     "NBGNCL2"  = "annual nonbaseload coal net generation (GJ)",
                     "NBGNOL"   = "annual nonbaseload oil net generation (MWh)",	
                     "NBGNOL2"  = "annual nonbaseload oil net generation (GJ)",
                     "NBGNGS"   = "annual nonbaseload gas net generation (MWh)",	
                     "NBGNGS2"  = "annual nonbaseload gas net generation (GJ)",
                     "NBGNNC"   = "annual nonbaseload nuclear net generation (MWh)",	
                     "NBGNNC2"  = "annual nonbaseload nuclear net generation (GJ)",
                     "NBGNHY"   = "annual nonbaseload hydro net generation (MWh)",	
                     "NBGNHY2"  = "annual nonbaseload hydro net generation (GJ)",	
                     "NBGNBM"   = "annual nonbaseload biomass net generation (MWh)",	
                     "NBGNBM2"  = "annual nonbaseload biomass net generation (GJ)",	
                     "NBGNWI"   = "annual nonbaseload wind net generation (MWh)",
                     "NBGNWI2"  = "annual nonbaseload wind net generation (GJ)",	
                     "NBGNSO"   = "annual nonbaseload solar net generation (MWh)",
                     "NBGNSO2"  = "annual nonbaseload solar net generation (GJ)",
                     "NBGNGT"   = "annual nonbaseload geothermal net generation (MWh)",	
                     "NBGNGT2"  = "annual nonbaseload geothermal net generation (GJ)",	
                     "NBGNOF"   = "annual nonbaseload other fossil net generation (MWh)",
                     "NBGNOF2"  = "annual nonbaseload other fossil net generation (GJ)",
                     "NBGNOP"   = "annual nonbaseload other unknown/ purchased fuel net generation (MWh)",
                     "NBGNOP2"  = "annual nonbaseload other unknown/ purchased fuel net generation (GJ)", # +11 rows
                     
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
unt_rows <- nrow(unt_file)+2

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
                 "HTIAN"    = "Unit unadjusted annual heat input (GJ)",
                 "HTIOZ"    = "Unit unadjusted ozone season heat input (GJ)",
                 "NOXAN"    = "Unit unadjusted annual NOx emissions (metric tons)",
                 "NOXOZ"    = "Unit unadjusted ozone season NOx emissions (metric tons)",
                 "SO2AN"    = "Unit unadjusted annual SO2 emissions (metric tons)",
                 "CO2AN"    = "Unit unadjusted annual CO2 emissions (metric tons)",
                 "HGAN"     = "Unit unadjusted annual Hg emissions (kg)",
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
                 "UNTYRONL" = "Unit year on-line")

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
addStyle(wb, sheet = unt, style = s[['desc_style']],  rows = 1, cols = 29:32, gridExpand = TRUE)

# add header style
addStyle(wb, sheet = unt, style = s[['header_style']],  rows = 2, cols = 1:14,  gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['color2_header']], rows = 2, cols = 15:28, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['header_style']],  rows = 2, cols = 29:32, gridExpand = TRUE)

# set column widths
setColWidths(wb, sheet = unt, cols = 1,     widths = 12.43)
setColWidths(wb, sheet = unt, cols = 3,     widths = 12.43)
setColWidths(wb, sheet = unt, cols = 4,     widths = 34.71)
setColWidths(wb, sheet = unt, cols = 5:9,   widths = 12.43)
setColWidths(wb, sheet = unt, cols = 10,    widths = 17)
setColWidths(wb, sheet = unt, cols = 11:32, widths = 12.43)

# set row heights
setRowHeights(wb, sheet = unt, row = 1, heights = 60.75)

# add number styles
addStyle(wb, sheet = unt, style = s[['integer']],  rows = 3:unt_rows, cols = 15:16, gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['decimal2']], rows = 3:unt_rows, cols = 14,    gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['decimal2']], rows = 3:unt_rows, cols = 17:21, gridExpand = TRUE)

# add text styles
addStyle(wb, sheet = unt, style = s[['basic']], rows = 3:unt_rows, cols = 1:13,  gridExpand = TRUE)
addStyle(wb, sheet = unt, style = s[['basic']], rows = 3:unt_rows, cols = 22:32, gridExpand = TRUE)

# freeze panes
freezePane(wb, sheet = unt, firstActiveCol = 5)