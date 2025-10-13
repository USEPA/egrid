## -------------------------------
##
## Final formatting (monthly)
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

# Load libraries ----------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)

# Load necessary functions
source("scripts/functions/function_check_params.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and temporal resolution parameters are already defined.")
}

if (exists("params")){
  if ("version" %in% names(params)) { # if params(), params$eGRID_year, and params$temporal_res exist, do not re-define
    print("eGRID version parameter is already defined.")
  } else {
    params$version <- readline(prompt = "Input eGRID version: ")
    params$version <- as.character(params$version)
  }
}


# Load in data ------------------------------

# load files
st_file    <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/state_aggregation_monthly.RDS"))
ba_file    <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/ba_aggregation_monthly.RDS"))
srl_file   <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/subregion_aggregation_monthly.RDS"))
nrl_file   <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/nerc_aggregation_monthly.RDS"))
us_file    <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/us_aggregation_monthly.RDS"))
ggl_file   <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/grid_gross_loss.RDS"))


st_file_ann    <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/annual/state_aggregation_annual.RDS"))
ba_file_ann    <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/annual/ba_aggregation_annual.RDS"))
srl_file_ann   <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/annual/subregion_aggregation_annual.RDS"))
nrl_file_ann   <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/annual/nerc_aggregation_annual.RDS"))
us_file_ann    <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/annual/us_aggregation_annual.RDS"))


# load in name_matching.R
source("scripts/1_production_model/name_matching.R")

# extract last two digits of year for universal labeling
year <- as.numeric(params$eGRID_year) %% 1000

# set up output file
### Note: check for updates or changes each data year ###
wb <- createWorkbook()

# create contents page
source("scripts/functions/function_create_contents_egrid_final.R")
create_contents_egrid_final(temporal_res = "monthly")

# vector for month names
month_abbr_lower <- month.abb
month_abbr_upper <- toupper(month.abb)

# Create styles ------------------------------

# call helper functions into script
source("scripts/functions/function_format_styles.R")
source("scripts/functions/function_final_formatting.R")
source("scripts/functions/function_add_hyperlink.R")

# create eGRID output style list using function
s <- create_format_styles()

# style map for monthly data
style_map <- c(
 "HTIT"   = "color1", 
 "NGEN"   = "color1",
 "NGENNB" = "color1",
 "NOX"    = "color1",
 "SO2"    = "color1",
 "CO2"    = "color1",
 "CH4"    = "color1",
 "N2O"    = "color1",
 "CO2EQA" = "color1",
 "HG"     = "color1",
 "NOXRT"  = "color4",
 "SO2RT"  = "color4",
 "CO2RT"  = "color4",
 "CH4RT"  = "color4",
 "N2ORT"  = "color4",
 "C2ERT"  = "color4",
 "HGRT"   = "color4",
 "NOXR"   = "color5",
 "SO2R"   = "color5",	
 "CO2R"   = "color5",
 "CH4R"   = "color5",	
 "N2OR"   = "color5",	
 "C2ER"   = "color5",	
 "HGR"    = "color5",	
 "NBNOX"  = "color15",	
 "NBSO2"  = "color15",	
 "NBCO2"  = "color15",
 "NBCH4"  = "color15",	
 "NBN2O"  = "color15",	
 "NBC2E"  = "color15",	
 "NBHG"   = "color15"
)

# Standard Column Names -----------------------------
# names for data sets: ST, BA, SRL, NRL, US
# data for region aggregated files contain same columns and information
# therefore, can assign a standardized list of columns, names, and styles

file_names <- c("ST", "BA", "SR", "NR", "US")
file_names_long <- c("State", "Balancing authority", "eGRID subregion", "NERC region", "U.S.")

standard_labels <- c(
                     "HTIT"   = "total heat input (MMBtu)",
                     "NGEN"   = "net generation (MWh)",
                     "NGENNB" = "nonbaseload generation (MWh)",
                     "NOX"    = "NOx emissions (tons)",	
                     "SO2"    = "SO2 emissions (tons)",	
                     "CO2"    = "CO2 emissions (tons)",
                     "CH4"    = "CH4 emissions (tons)",	
                     "N2O"    = "N2O emissions (tons)",	
                     "CO2EQA" = "CO2 equivalent emissions (tons)",	
                     "HG"     = "Hg emissions (lbs)",
                     "NOXRT"  = "NOx total output emission rate (lb/MWh)",
                     "SO2RT"  = "SO2 total output emission rate (lb/MWh)",	
                     "CO2RT"  = "CO2 total output emission rate (lb/MWh)",	
                     "CH4RT"  = "CH4 total output emission rate (lb/MWh)",
                     "N2ORT"  = "N2O total output emission rate (lb/MWh)",
                     "C2ERT"  = "CO2 equivalent total output emission rate (lb/MWh)",
                     "HGRT"   = "Hg total output emission rate (lb/MWh)",
                     "NOXR"   = "NOx input emission rate (lb/MMBtu)",
                     "SO2R"   = "SO2 input emission rate (lb/MMBtu)",	
                     "CO2R"   = "CO2 input emission rate (lb/MMBtu)",
                     "CH4R"   = "CH4 input emission rate (lb/MMBtu)",	
                     "N2OR"   = "N2O input emission rate (lb/MMBtu)",	
                     "C2ER"   = "CO2 equivalent input emission rate (lb/MMBtu)",	
                     "HGR"    = "Hg input emission rate (lb/MMBtu)",	
                     "NBNOX"  = "NOx non-baseload output emission rate (lb/MWh)",	
                     "NBSO2"  = "SO2 non-baseload output emission rate (lb/MWh)",	
                     "NBCO2"  = "CO2 non-baseload output emission rate (lb/MWh)",
                     "NBCH4"  = "CH4 non-baseload output emission rate (lb/MWh)",	
                     "NBN2O"  = "N2O non-baseload output emission rate (lb/MWh)",	
                     "NBC2E"  = "CO2 equivalent non-baseload output emission rate (lb/MWh)",	
                     "NBHG"   = "Hg non-baseload output emission rate (lb/MWh)")


standard_header <- names(standard_labels)  # column names
standard_desc   <- unname(standard_labels) # description of column names

# text replacement vector
header_ann_replace <- c("^HTIT$" = "HTIANT",
                        "^NGEN$" = "NGENAN",
                        "^NOX$"  = "NOXAN",
                        "^SO2$"  = "SO2AN",
                        "^CO2$"  = "CO2AN",
                        "^CH4$"  = "CH4AN",
                        "^N2O$"  = "N2OAN",
                        "^HG$"   = "HGAN")

# convert standard header variables to annual version for ease of indexing
standard_header_ann <- data.frame(standard_header) %>%
                       mutate(standard_header = gsub("RT", "RTA", standard_header),
                              standard_header = ifelse(endsWith(standard_header, "R"), gsub("R", "RA", standard_header), standard_header),
                              standard_header = str_replace_all(standard_header, header_ann_replace),
                              standard_header = gsub("^NOXCRTA$", "NOXCRT", standard_header)) 

# function to convert back to monthly version
rename_headers <- function(file, original_header){
    
    # reverse text replacement vector
    header_ann_replace_2 <- c(setNames(glue::glue("{file}HTIT"), glue::glue("^{file}HTIANT$")),
                              setNames(glue::glue("{file}NGEN"), glue::glue("^{file}NGENAN$")),
                              setNames(glue::glue("{file}NOX"), glue::glue("^{file}NOXAN$")),
                              setNames(glue::glue("{file}SO2"), glue::glue("^{file}SO2AN$")),
                              setNames(glue::glue("{file}CO2"), glue::glue("^{file}CO2AN$")),
                              setNames(glue::glue("{file}CH4"), glue::glue("^{file}CH4AN$")),
                              setNames(glue::glue("{file}N2O"), glue::glue("^{file}N2OAN$")),
                              setNames(glue::glue("{file}HG"), glue::glue("{file}HGAN")))
    
    new_header <- original_header
    
    # replace RTA with RT
    new_header <- gsub("RTA", "RT", new_header)
    
    # if it ends with RA, replace with R
    new_header <- ifelse(endsWith(new_header, "RA"),
                         sub("RA$", "R", new_header),
                         new_header)
    
    # apply pattern replacements
    new_header <- str_replace_all(new_header, header_ann_replace_2)
    
    return(new_header)
    
}


# initialize lists to store header and description names for each file
all_labels_month_list <- list()
all_labels_annual_list <- list()

for (j in 1:length(file_names)){
  
  # initialize lists
  file_header_month_list <- list()
  file_desc_month_list <- list()
  
  file_header_annual_list <- list()
  file_desc_annual_list <- list()
  
  for (i in 1:length(standard_labels)) {
    file_header_month <- paste0(file_names[j], names(standard_labels)[i], "_", month_abbr_upper)
    file_header_annual <- paste0(file_names[j], names(standard_labels)[i], "_ANNUAL")
    
    file_desc_month <- paste0(file_names_long[j], " ", unname(standard_labels)[i], " - ", month_abbr_upper)
    file_desc_annual <- paste0(file_names_long[j], " ", unname(standard_labels)[i], " - ANNUAL")
    
    file_header_month_list <- c(file_header_month_list, file_header_month)
    file_desc_month_list <- c(file_desc_month_list, file_desc_month)
    
    file_header_annual_list <- c(file_header_annual_list, file_header_annual)
    file_desc_annual_list <- c(file_desc_annual_list, file_desc_annual)
  }
  
  file_labels_month <- setNames(file_header_month_list, file_desc_month_list)
  file_labels_annual <- setNames(file_header_annual_list, file_desc_annual_list)
  
  all_labels_month_list[[file_names[j]]] <- file_labels_month
  all_labels_annual_list[[file_names[j]]] <- file_labels_annual
  
}

# ST Formatting --------------------------------------

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

st_file <- rename_variables(st_file, state_nonmetric_monthly)

st_file_wider_cols <- st_file %>%
                      select(all_of(paste0("ST", standard_header))) %>%
                      colnames()

# Structure dataframe
st_file_formatted <- st_file %>%
                     select("YEAR",
                            "MONTH",
                            "PSTATABB",
                            "FIPSST",
                            all_of(paste0("ST", standard_header))) %>%
                     mutate(MONTH = month_abbr_upper[MONTH]) %>%
                     pivot_wider(names_from = MONTH, values_from = all_of(st_file_wider_cols))

st_desc <- c("Data Year" = "YEAR",
             "State abbreviation" = "PSTATABB",
             "FIPS State code" = "FIPSST",
              all_labels_month_list[["ST"]])

st_file_desc <- rename_variables(st_file_formatted, st_desc) %>%
                colnames()

st_file_ann <- rename_variables(st_file_ann, state_nonmetric_annual)

st_file_ann_formatted <- st_file_ann %>%
                         select(all_of(paste0("ST", as.matrix(standard_header_ann)))) %>%
                         rename_with(~ rename_headers("ST", .)) %>%
                         rename_with(~ paste0(., "_ANNUAL"))

st_file_ann_desc <- rename_variables(st_file_ann_formatted, all_labels_annual_list[["ST"]]) %>%
                    colnames()
                                       

# check if shorthand names match and stop if not. 
st_header <- c(colnames(st_file_formatted), colnames(st_file_ann_formatted))
st_header_check <- c("YEAR", 
                     "PSTATABB", 
                     "FIPSST", 
                     unname(all_labels_month_list[["ST"]]), 
                     unname(all_labels_annual_list[["ST"]]))

check_var_names("state", st_header, st_header_check, params$temporal_res)

## write monthly data
# write monthly descriptions for first row only
writeData(wb,
          sheet = st,
          t(st_file_desc),
          startRow = 1,
          colNames = FALSE)

# write monthly data to sheet
writeData(wb,
          sheet = st,
          st_file_formatted,
          startRow = 2)

## write annual data
writeData(wb,
          sheet = st,
          t(st_file_ann_desc),
          startRow = 1,
          startCol = length(st_file_formatted)+1,
          colNames = FALSE)

writeData(wb,
          sheet = st,
          st_file_ann_formatted,
          startRow = 2,
          startCol = length(st_file_formatted)+1)

## add styles to document
format_sheet(df_month = st_file_formatted,
             df_ann = st_file_ann_formatted,
             file_name = "ST",
             temporal_res = params$temporal_res,
             default_style_map = style_map)

freezePane(wb, sheet = st, firstActiveCol = 4, firstActiveRow = 3)

# BA Formatting ----------------------------------

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

ba_file <- rename_variables(ba_file, ba_nonmetric_monthly)

ba_file_wider_cols <- ba_file %>%
                      select(all_of(paste0("BA", standard_header))) %>%
                      colnames()

# Structure dataframe
ba_file_formatted <- ba_file %>%
                     select("YEAR",
                            "MONTH",
                            "BANAME",
                            "BACODE",
                            all_of(paste0("BA", standard_header))) %>%
                     mutate(MONTH = month_abbr_upper[MONTH]) %>%
                     pivot_wider(names_from = MONTH, values_from = all_of(ba_file_wider_cols))


ba_desc <- c("Data Year" = "YEAR",
             "Balancing Authority Name" = "BANAME",
             "Balancing Authority Code" = "BACODE",
             all_labels_month_list[["BA"]])

ba_file_desc <- colnames(rename_variables(ba_file_formatted, ba_desc))

ba_file_ann <- rename_variables(ba_file_ann, ba_nonmetric_annual)

ba_file_ann_formatted <- ba_file_ann %>%
                         select(all_of(paste0("BA", as.matrix(standard_header_ann)))) %>%
                         rename_with(~ rename_headers("BA", .)) %>%
                         rename_with(~ paste0(., "_ANNUAL"))

ba_file_ann_desc <- rename_variables(ba_file_ann_formatted, all_labels_annual_list[["BA"]]) %>%
                    colnames()

# check if shorthand names match and stop if not. 
ba_header <- c(colnames(ba_file_formatted), colnames(ba_file_ann_formatted))
ba_header_check <- c("YEAR", 
                     "BANAME", 
                     "BACODE", 
                     unname(all_labels_month_list[["BA"]]), 
                     unname(all_labels_annual_list[["BA"]]))

check_var_names("balancing authority", ba_header, ba_header_check, params$temporal_res)

## write monthly data
# write monthly descriptions for first row only
writeData(wb,
          sheet = ba,
          t(ba_file_desc),
          startRow = 1,
          colNames = FALSE)

# write monthly data to sheet
writeData(wb, 
          sheet = ba, 
          ba_file_formatted,
          startRow = 2)

## write annual data
writeData(wb,
          sheet = ba,
          t(ba_file_ann_desc),
          startRow = 1,
          startCol = length(ba_file_formatted)+1,
          colNames = FALSE)

writeData(wb,
          sheet = ba,
          ba_file_ann_formatted,
          startRow = 2,
          startCol = length(ba_file_formatted)+1)


## add styles to document
format_sheet(df_month = ba_file_formatted,
             df_ann = ba_file_ann_formatted,
             file_name = "BA",
             temporal_res = params$temporal_res,
             default_style_map = style_map)


setColWidths(wb, sheet = ba, cols = 2, widths = 75.55)
freezePane(wb, sheet = ba, firstActiveCol = 4, firstActiveRow = 3)

# SRL Formatting -----------------------------------------

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
srl_file <- rename_variables(srl_file, subregion_nonmetric_monthly)

srl_file_wider_cols <- srl_file %>%
                       select(all_of(paste0("SR", standard_header))) %>%
                       colnames()

# Structure dataframe
srl_file_formatted <- srl_file %>%
                      select("YEAR",
                             "MONTH",
                             "SUBRGN",
                             "SRNAME",
                             all_of(paste0("SR", standard_header))) %>%
                       mutate(MONTH = month_abbr_upper[MONTH]) %>%
                       pivot_wider(names_from = MONTH, values_from = all_of(srl_file_wider_cols))

srl_file_ann <- rename_variables(srl_file_ann, subregion_nonmetric_annual)

srl_file_ann_formatted <- srl_file_ann %>%
                          select(all_of(paste0("SR", as.matrix(standard_header_ann)))) %>%
                          rename_with(~ rename_headers("SR", .)) %>%
                          rename_with(~ paste0(., "_ANNUAL"))

srl_file_ann_desc <- rename_variables(srl_file_ann_formatted, all_labels_annual_list[["SR"]]) %>%
                     colnames()

srl_desc <- c("Data Year" = "YEAR",
             "eGRID subregion acronym" = "SUBRGN",
             "eGRID subregion name" = "SRNAME",
             all_labels_month_list[["SR"]])

srl_file_desc <- colnames(rename_variables(srl_file_formatted, srl_desc))

# check if shorthand names match and stop if not. 
srl_header <- c(colnames(srl_file_formatted), colnames(srl_file_ann_formatted))
srl_header_check <- c("YEAR", 
                      "SUBRGN", 
                      "SRNAME", 
                      unname(all_labels_month_list[["SR"]]), 
                      unname(all_labels_annual_list[["SR"]]))

check_var_names("subregion", srl_header, srl_header_check, params$temporal_res)

## write monthly data
# write monthly data for first row only
writeData(wb,
          sheet = srl,
          t(srl_file_desc),
          startRow = 1,
          colNames = FALSE)

# write monthly data to sheet
writeData(wb, 
          sheet = srl, 
          srl_file_formatted,
          startRow = 2)

## write annual data
writeData(wb,
          sheet = srl,
          t(srl_file_ann_desc),
          startRow = 1,
          startCol = length(srl_file_formatted)+1,
          colNames = FALSE)

writeData(wb,
          sheet = srl,
          srl_file_ann_formatted,
          startRow = 2,
          startCol = length(srl_file_formatted)+1)

## add styles to document
format_sheet(df_month = srl_file_formatted,
             df_ann = srl_file_ann_formatted,
             file_name = "SR",
             temporal_res = params$temporal_res,
             default_style_map = style_map)

setColWidths(wb, sheet = srl, cols = 3, widths = 18.45)
freezePane(wb, sheet = srl, firstActiveCol = 4, firstActiveRow = 3)

# NRL Formatting ----------------------------------------

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
nrl_file <- rename_variables(nrl_file, nerc_nonmetric_monthly)

nrl_file_wider_cols <- nrl_file %>%
                       select(all_of(paste0("NR", standard_header))) %>%
                       colnames()

# Structure dataframe
nrl_file_formatted <- nrl_file %>%
                      select("YEAR",
                             "MONTH",
                             "NERC",
                             "NERCNAME",
                             all_of(paste0("NR", standard_header))) %>%
                      mutate(MONTH = month_abbr_upper[MONTH]) %>%
                      pivot_wider(names_from = MONTH, values_from = all_of(nrl_file_wider_cols))

# description of column names
nrl_desc <- c("Data Year" = "YEAR",
             "NERC region acronym" = "NERC",
             "NERC region name" = "NERCNAME",
             all_labels_month_list[["NR"]])

nrl_file_desc <- colnames(rename_variables(nrl_file_formatted, nrl_desc))

nrl_file_ann <- rename_variables(nrl_file_ann, nerc_nonmetric_annual)

nrl_file_ann_formatted <- nrl_file_ann %>%
                          select(all_of(paste0("NR", as.matrix(standard_header_ann)))) %>%
                          rename_with(~ rename_headers("NR", .)) %>%
                          rename_with(~ paste0(., "_ANNUAL"))

nrl_file_ann_desc <- rename_variables(nrl_file_ann_formatted, all_labels_annual_list[["NR"]]) %>%
  colnames()

# check if shorthand names match and stop if not. 
nrl_header <- c(colnames(nrl_file_formatted), colnames(nrl_file_ann_formatted))
nrl_header_check <- c("YEAR", 
                      "NERC", 
                      "NERCNAME", 
                      unname(all_labels_month_list[["NR"]]), 
                      unname(all_labels_annual_list[["NR"]]))

check_var_names("NERC region", nrl_header, nrl_header_check, params$temporal_res)

## write monthly data
# write monthly description for first row only
writeData(wb,
          sheet = nrl,
          t(nrl_file_desc),
          startRow = 1,
          colNames = FALSE)

# write monthly data to sheet
writeData(wb, 
          sheet = nrl, 
          nrl_file_formatted,
          startRow = 2)

## write annual data
writeData(wb,
          sheet = nrl,
          t(nrl_file_ann_desc),
          startRow = 1,
          startCol = length(nrl_file_formatted)+1,
          colNames = FALSE)

writeData(wb,
          sheet = nrl,
          nrl_file_ann_formatted,
          startRow = 2,
          startCol = length(nrl_file_formatted)+1)

## add styles to document
format_sheet(df_month = nrl_file_formatted,
             df_ann = nrl_file_ann_formatted,
             file_name = "NR",
             temporal_res = params$temporal_res,
             default_style_map = style_map)

setColWidths(wb, sheet = nrl, cols = 3, widths = 29.45)
freezePane(wb, sheet = nrl, firstActiveCol = 4, firstActiveRow = 3)

# US Formatting ---------------------------------------

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


us_file <- rename_variables(us_file, us_nonmetric_monthly)

us_file_wider_cols <- us_file %>%
                      select(all_of(paste0("US", standard_header))) %>%
                      colnames()

# Structure dataframe
us_file_formatted <- us_file %>%
                     select("YEAR",
                            "MONTH",
                            all_of(paste0("US", standard_header))) %>%
                     mutate(MONTH = month_abbr_upper[MONTH]) %>%
                     pivot_wider(names_from = MONTH, values_from = all_of(us_file_wider_cols))

# description of column names
us_desc <- c("Data Year" = "YEAR",
             all_labels_month_list[["US"]])

us_file_desc <- colnames(rename_variables(us_file_formatted, us_desc))

us_file_ann <- rename_variables(us_file_ann, us_nonmetric_annual)

us_file_ann_formatted <- us_file_ann %>%
                         select(all_of(paste0("US", as.matrix(standard_header_ann)))) %>%
                         rename_with(~ rename_headers("US", .)) %>%
                         rename_with(~ paste0(., "_ANNUAL"))

us_file_ann_desc <- rename_variables(us_file_ann_formatted, all_labels_annual_list[["US"]]) %>%
                    colnames()

# check if shorthand names match and stop if not. 
us_header <- c(colnames(us_file_formatted), colnames(us_file_ann_formatted))
us_header_check <- c("YEAR", 
                     unname(all_labels_month_list[["US"]]), 
                     unname(all_labels_annual_list[["US"]]))

check_var_names("U.S.", us_header, us_header_check, params$temporal_res)

## write monthly data
# write monthly description for first row only
writeData(wb,
          sheet = us,
          t(us_file_desc),
          startRow = 1,
          colNames = FALSE)

# write data to sheet
writeData(wb,
          sheet = us,
          us_file_formatted,
          startRow = 2)

## write annual data
writeData(wb,
          sheet = us,
          t(us_file_ann_desc),
          startRow = 1,
          startCol = length(us_file_formatted)+1,
          colNames = FALSE)

writeData(wb,
          sheet = us,
          us_file_ann_formatted,
          startRow = 2,
          startCol = length(us_file_formatted)+1)

## add styles to document
format_sheet(df_month = us_file_formatted,
             df_ann = us_file_ann_formatted,
             file_name = "US",
             temporal_res = params$temporal_res,
             default_style_map = style_map)

freezePane(wb, sheet = us, firstActiveCol = 2, firstActiveRow = 3)

# Contents Formatting -----------------------------------------

# # add link to sheets
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 1, loc = c(3, 9), text_to_show = glue::glue("ST{year}"))
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 1, loc = c(3, 10), text_to_show = glue::glue("BA{year}"))
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 1, loc = c(3, 11), text_to_show = glue::glue("SRL{year}"))
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 1, loc = c(3, 12), text_to_show = glue::glue("NRL{year}"))
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 1, loc = c(3, 13), text_to_show = glue::glue("US{year}"))

# add hyperlinks to specific columns
# annual values
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 4, loc = c(10, 22), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 4, loc = c(11, 22), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 4, loc = c(12, 22), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 4, loc = c(13, 22), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 2, loc = c(14, 22), text_to_show = "US")

# output emissions rates
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 124, loc = c(10, 23), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 124, loc = c(11, 23), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 124, loc = c(12, 23), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 124, loc = c(13, 23), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 122, loc = c(14, 23), text_to_show = "US")
 
# input emissions rates
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 208, loc = c(10, 24), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 208, loc = c(11, 24), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 208, loc = c(12, 24), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 208, loc = c(13, 24), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 206, loc = c(14, 24), text_to_show = "US")

# nonbaseload output emission rates
add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 292, loc = c(10, 25), text_to_show = "ST")
add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 292, loc = c(11, 25), text_to_show = "BA")
add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 292, loc = c(12, 25), text_to_show = "SRL")
add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 292, loc = c(13, 25), text_to_show = "NRL")
add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 290, loc = c(14, 25), text_to_show = "US")


# Save and export -------------------------------------------
output <- glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/egrid{params$eGRID_year}_monthly_data.xlsx")
saveWorkbook(wb, output, overwrite = TRUE)

print(glue::glue("Saving final formatted file to folder data/1_production_model/outputs/{params$eGRID_year}/monthly/"))

