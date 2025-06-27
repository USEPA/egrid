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
source("scripts/functions/function_check_var_names.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")
}


# Load in data ------------------------------

# load files
# unt_file   <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/unit_file_monthly.RDS"))
# gen_file   <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/generator_file_monthly.RDS"))
# plnt_file  <- read_rds(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/plant_file_monthly.RDS"))
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
# source("scripts/functions/function_create_contents_egrid_final.R")
# create_contents_egrid_final()

# vector for month names
month_abbr_lower <- month.abb
month_abbr_upper <- toupper(month.abb)

# Create styles ------------------------------

# call helper functions into script
source("scripts/functions/function_format_styles.R")
source("scripts/functions/function_format_region.R")
source("scripts/functions/function_add_hyperlink.R")

# create eGRID output style list using function
s <- create_format_styles()

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
                     "CH4"    = "CH4 emissions (lbs)",	
                     "N2O"    = "N2O emissions (lbs)",	
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
                     "NOXCRT" = "annual NOx combustion output emission rate (lb/MWh)",
                     "NBNOX"  = "NOx non-baseload output emission rate (lb/MWh)",	
                     "NBSO2"  = "SO2 non-baseload output emission rate (lb/MWh)",	
                     "NBCO2"  = "CO2 non-baseload output emission rate (lb/MWh)",
                     "NBCH4"  = "CH4 non-baseload output emission rate (lb/MWh)",	
                     "NBN2O"  = "N2O non-baseload output emission rate (lb/MWh)",	
                     "NBC2E"  = "CO2 equivalent non-baseload output emission rate (lb/MWh)",	
                     "NBHG"   = "Hg non-baseload output emission rate (lb/MWh)")


standard_header <- names(standard_labels)  # column names
standard_desc   <- unname(standard_labels) # description of column names

header_ann_replace <- c("^HTIT$" = "HTIANT",
                        "^NGEN$" = "NGENAN",
                        "^NOX$"  = "NOXAN",
                        "^SO2$"  = "SO2AN",
                        "^CO2$"  = "CO2AN",
                        "^CH4$"  = "CH4AN",
                        "^N2O$"  = "N2OAN",
                        "^HG$"   = "HGAN")

standard_header_ann <- data.frame(standard_header) %>%
                       mutate(standard_header = gsub("RT", "RTA", standard_header),
                              standard_header = ifelse(endsWith(standard_header, "R"), gsub("R", "RA", standard_header), standard_header),
                              standard_header = str_replace_all(standard_header, header_ann_replace),
                              standard_header = gsub("^NOXCRTA$", "NOXCRT", standard_header)) 
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
    file_header_annual <- paste0(file_names[j], standard_header_ann[i,1], "_ANNUAL")
    
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
# check_var_names(colnames(st_file), state_nonmetric_monthly) # doesnt woprk for monthly ver? creating too many outputs
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
                         rename_with(~ paste0(., "_ANNUAL"))

st_file_ann_desc <- colnames(rename_variables(st_file_ann_formatted, all_labels_annual_list[["ST"]]))
                                       

# check if shorthand names match name_matching.R and stop if not. 
# state_check_cols <- c()
# for (i in 1:length((st_header))) { 
#   if (st_header[i] != names(state_nonmetric_annual)[i]) { 
#     state_check_cols <- c(state_check_cols, st_header[i]) }} 
# 
# if (!is.null(state_check_cols)){ 
#   stop(print(glue::glue("These columns do not match name_matching.R state_nonmetric_annual: {glue::glue_collapse(state_check_cols, sep = ', ')}. Check for errors.")))
# } else {
#   print("All shorthand columns match name_matching.R state_nonmetric_annual.")
# }

# description of column names
# st_desc <- c("Data Year",
#              "State abbreviation",
#              "FIPS State code",
#              paste0("State ", standard_desc))

## write data
# write data for first row only
writeData(wb,
          sheet = st,
          t(st_file_desc),
          startRow = 1,
          colNames = FALSE)

# write data to sheet
writeData(wb,
          sheet = st,
          st_file_formatted,
          startRow = 2)

## add styles to document
# format_region(st, st_rows)


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

# check if shorthand names match name_matching.R and stop if not. 
# ba_check_cols <- c()
# for (i in 1:length((ba_header))) { 
#   if (ba_header[i] != names(ba_nonmetric_annual)[i]) { 
#     ba_check_cols <- c(ba_check_cols, ba_header[i]) }} 
# 
# if (!is.null(ba_check_cols)){ 
#   stop(print(glue::glue("These columns do not match name_matching.R ba_nonmetric_annual: {glue::glue_collapse(ba_check_cols, sep = ', ')}. Check for errors.")))
# } else {
#   print("All shorthand columns match name_matching.R ba_nonmetric_annual.")
# }

# description of column names
# ba_desc <- c("Data Year",
#              "Balancing Authority Name",
#              "Balancing Authority Code",
#              paste0("BA ", standard_desc))

## write data
# write data for first row only
writeData(wb,
          sheet = ba,
          t(ba_file_desc),
          startRow = 1,
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = ba, 
          ba_file_formatted,
          startRow = 2)

## add styles to document
# format_region(ba, ba_rows)

# setColWidths(wb, sheet = ba, cols = 2, widths = 75.55)


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
# column names
# srl_header <- c("YEAR",	
#                 "SUBRGN",	
#                 "SRNAME",
#                 paste0("SR", standard_header))

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

# check if shorthand names match name_matching.R and stop if not. 
# subregion_check_cols <- c()
# for (i in 1:length((srl_header))) { 
#   if (srl_header[i] != names(subregion_nonmetric_annual)[i]) { 
#     subregion_check_cols <- c(subregion_check_cols, srl_header[i]) }} 
# 
# if (!is.null(subregion_check_cols)){ 
#   stop(print(glue::glue("These columns do not match name_matching.R subregion_nonmetric_annual: {glue::glue_collapse(subregion_check_cols, sep = ', ')}. Check for errors.")))
# } else {
#   print("All shorthand columns match name_matching.R subregion_nonmetric_annual.")
# }

# description of column names
# srl_desc <- c("Data Year",
#               "eGRID subregion acronym",
#               "eGRID subregion name",
#               paste0("eGRID subregion ", standard_desc))


srl_desc <- c("Data Year" = "YEAR",
             "eGRID subregion acronym" = "SUBRGN",
             "eGRID subregion name" = "SRNAME",
             all_labels_month_list[["SR"]])

srl_file_desc <- colnames(rename_variables(srl_file_formatted, srl_desc))

## write data
# write data for first row only
writeData(wb,
          sheet = srl,
          t(srl_file_desc),
          startRow = 1,
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = srl, 
          srl_file_formatted,
          startRow = 2)

## add styles to document
# format_region(srl, srl_rows)
# 
# setColWidths(wb, sheet = srl, cols = 3, widths = 18.45)


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
# column names
# nrl_header <- c("YEAR",	
#                 "NERC",	
#                 "NERCNAME",
#                 paste0("NR", standard_header))

# check if shorthand names match name_matching.R and stop if not. 
# nerc_check_cols <- c()
# for (i in 1:length((nrl_header))) { 
#   if (nrl_header[i] != names(nerc_nonmetric_annual)[i]) { 
#     nerc_check_cols <- c(nerc_check_cols, nrl_header[i]) }} 
# 
# if (!is.null(nerc_check_cols)){ 
#   stop(print(glue::glue("These columns do not match name_matching.R nerc_nonmetric_annual: {glue::glue_collapse(nerc_check_cols, sep = ', ')}. Check for errors.")))
# } else {
#   print("All shorthand columns match name_matching.R nerc_nonmetric_annual.")
# }

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
# nrl_desc <- c("Data Year",
#               "NERC region acronym",
#               "NERC region name",
#               paste0("NERC region ", standard_desc))

nrl_desc <- c("Data Year" = "YEAR",
             "NERC region acronym" = "NERC",
             "NERC region name" = "NERCNAME",
             all_labels_month_list[["NR"]])

nrl_file_desc <- colnames(rename_variables(nrl_file_formatted, nrl_desc))

## write data
# write data for first row only
writeData(wb,
          sheet = nrl,
          t(nrl_file_desc),
          startRow = 1,
          colNames = FALSE)

# write data to sheet
writeData(wb, 
          sheet = nrl, 
          nrl_file_formatted,
          startRow = 2)

## add styles to document
# format_region(nrl, nrl_rows)

# setColWidths(wb, sheet = nrl, cols = 3, widths = 29.45)


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

# check if shorthand names match name_matching.R and stop if not. 
# us_check_cols <- c()
# for (i in 1:length((us_header))) { 
#   if (us_header[i] != names(us_nonmetric_annual)[i]) { 
#     us_check_cols <- c(us_check_cols, us_header[i]) }} 
# 
# if (!is.null(us_check_cols)){ 
#   stop(print(glue::glue("These columns do not match name_matching.R us_nonmetric_annual: {glue::glue_collapse(us_check_cols, sep = ', ')}. Check for errors.")))
# } else {
#   print("All shorthand columns match name_matching.R us_nonmetric_annual.")
# }

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
# us_desc <- c("Data Year",
#              paste0("U.S. ", standard_desc))


us_desc <- c("Data Year" = "YEAR",
             all_labels_month_list[["US"]])

us_file_desc <- colnames(rename_variables(us_file_formatted, us_desc))

## write data
# write data for first row only
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

## add styles to document
# format_region(us, us_rows)

# GGL Formatting --------------------------------------------

## create "GGL" sheet
# ggl <- glue::glue("GGL{year}")
# addWorksheet(wb, ggl)
# 
# # convert year to numeric value
# ggl_file <- ggl_file %>%
#   mutate(year = as.numeric(year))
# 
# 
# ## column names and descriptions
# ggl_labels <- c("YEAR"     = "Data Year",
#                 "REGION"   = "One of the three interconnect power grids in the U.S. (plus Alaska, Hawaii, and the entire U.S.)",
#                 "ESTLOSS"  = "Estimated losses (MWh)",
#                 "TOTDISP"  = "Total disposition (MWh) without exports",
#                 "DIRCTUSE" = "Direct use (MWh)",
#                 "GGRSLOSS" = "Grid gross loss [Estimated losses/(Total disposition without exports - Direct use)]")
# 
# # check if shorthand names match name_matching.R and stop if not. 
# ggl_check_cols <- c()
# for (i in 1:length(names(ggl_labels))) { # skip SEQUNT since this will always be different
#   if (names(ggl_labels)[i] != names(ggl_nonmetric)[i]) { 
#     ggl_check_cols <- c(ggl_check_cols, names(ggl_labels)[i]) }} 
# 
# if (!is.null(ggl_check_cols)){ 
#   stop(print(glue::glue("These columns do not match name_matching.R ggl_nonmetric: {glue::glue_collapse(ggl_check_cols, sep = ', ')}. Check for errors.")))
# } else {
#   print("All shorthand columns match name_matching.R ggl_nonmetric.")
# }
# 
# ggl_header <- names(ggl_labels)  # column names
# ggl_desc   <- unname(ggl_labels) # description of column names
# 
# # add new column names
# colnames(ggl_file) <- ggl_header
# 
# ## write data
# # write data for first row only
# writeData(wb, 
#           sheet = ggl, 
#           t(ggl_desc), 
#           startRow = 1, 
#           colNames = FALSE)
# 
# # write data to sheet
# writeData(wb, 
#           sheet = ggl, 
#           ggl_file,
#           startRow = 2)
# 
# ## add styles to document
# # add description styles
# addStyle(wb, sheet = ggl, style = s[['desc_style']], rows = 1, cols = 1:6, gridExpand = TRUE)
# 
# # add header style
# addStyle(wb, sheet = ggl, style = s[['header_style']], rows = 2, cols = 1:6, gridExpand = TRUE)
# 
# # set column widths
# setColWidths(wb, sheet = ggl, cols = 1,   widths = 10)
# setColWidths(wb, sheet = ggl, cols = 2,   widths = 21.29)
# setColWidths(wb, sheet = ggl, cols = 3:5, widths = 11.14)
# setColWidths(wb, sheet = ggl, cols = 6,   widths = 23)
# 
# # set row heights
# setRowHeights(wb, sheet = ggl, row = 1, heights = 60.75)
# 
# # add number styles
# addStyle(wb, sheet = ggl, style = s[['integer']], rows = 3:7, cols = 3:5, gridExpand = TRUE)
# addStyle(wb, sheet = ggl, style = s[['percent']], rows = 3:7, cols = 6,   gridExpand = TRUE)
# 
# # add number styles (bold)
# addStyle(wb, sheet = ggl, style = s[['integer_bold']], rows = 8, cols = 3:5, gridExpand = TRUE)
# addStyle(wb, sheet = ggl, style = s[['percent_bold']], rows = 8, cols = 6,   gridExpand = TRUE)
# 
# # add text styles
# addStyle(wb, sheet = ggl, style = s[['basic']], rows = 3:7, cols = 1:2, gridExpand = TRUE)
# addStyle(wb, sheet = ggl, style = s[['bold']],  rows = 8,   cols = 1:2, gridExpand = TRUE)

# DEMO Formatting -------------------------------------------
# only build demographics file if the file exists in outputs
# this is because pulling data from the EJScreen API to build the demographics file takes several hours
# if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/demographics_file.RDS"))) {
#   
#   ## create "DEMO" sheet
#   demo <- glue::glue("DEMO{year}")
#   addWorksheet(wb, demo)
#   
#   # convert year to numeric value
#   demo_file <- 
#     demo_file %>%
#     mutate(year = as.numeric(year))
#   
#   demo_rows <- nrow(demo_file) + 2
#   
#   ## column names and descriptions
#   demo_labels <- c("SEQPLT"              = "Plant file sequence number",
#                    "YEAR"                = "Data Year",
#                    "PSTATABB"            = "Plant state abbreviation",
#                    "PNAME"               = "Plant name",
#                    "ORISPL"              = "DOE/EIA ORIS plant or facility code",
#                    "LAT"                 = "Plant latitude",
#                    "LON"                 = "Plant longitude",
#                    "PLPRMFL"             = "Plant primary fuel", 
#                    "PLFUELCT"            = "Plant primary fuel category",
#                    "NAMEPCAP"            = "Plant nameplate capacity (MW)",
#                    "COALFLAG"            = "Flag indicating if the plant burned or generated any amount of coal",
#                    "TOTALPOP"            = "Total Population", 
#                    "RAW_D_PEOPCOLOR"     = "People of Color (%)",
#                    "RAW_D_INCOME"        = "Low Income (%)",
#                    "RAW_D_LESSHS"        = "Less Than High School Education (%)",
#                    "RAW_D_LING"          = "Limited English Speaking (%)",
#                    "RAW_D_UNDER5"        = "Under Age 5 (%)",
#                    "RAW_D_OVER64"        = "Over Age 64 (%)",
#                    "RAW_D_UNEMPLOYED"    = "Unemployment Rate (%)",
#                    "RAW_D_LIFEEXP"       = "Limited Life Expectancy (%)",
#                    "RAW_D_DEMOGIDX2"     = "Demographic Index",
#                    "RAW_D_DEMOGIDX5"     = "Supplemental Demographic Index",
#                    "RAW_D_DEMOGIDX2ST"   = "State Demographic Index",
#                    "RAW_D_DEMOGIDX5ST"   = "State Supplemental Demographic Index",
#                    "S_D_PEOPCOLOR"       = "State Average of People of Color (%)",
#                    "S_D_INCOME"          = "State Average of Low Income (%)",
#                    "S_D_LESSHS"          = "State Average of Less Than High School Education (%)",
#                    "S_D_LING"            = "State Average of Limited English Speaking (%)",
#                    "S_D_UNDER5"          = "State Average of Under Age 5 (%)",
#                    "S_D_OVER64"          = "State Average of Over Age 64 (%)", 
#                    "S_D_UNEMPLOYED"      = "State Average of Unemployment Rate (%)",
#                    "S_D_LIFEEXP"         = "State Average of Limited Life Expectancy (%)",
#                    "S_D_DEMOGIDX2ST"     = "State Average of Demographic Index",
#                    "S_D_DEMOGIDX5ST"     = "State Average of Supplemental Demographic Index",
#                    "S_D_PEOPCOLOR_PER"   = "State Percentile of People of Color", 
#                    "S_D_INCOME_PER"      = "State Percentile of Low Income",
#                    "S_D_LESSHS_PER"      = "State Percentile of Less Than High School Education",
#                    "S_D_LING_PER"        = "State Percentile of Limited English Speaking",
#                    "S_D_UNDER5_PER"      = "State Percentile of Under Age 5",
#                    "S_D_OVER64_PER"      = "State Percentile of Over Age 64",
#                    "S_D_UNEMPLOYED_PER"  = "State Percentile of Unemployment Rate",
#                    "S_D_LIFEEXP_PER"     = "State Percentile of Limited Life Expectancy",        
#                    "S_D_DEMOGIDX2ST_PER" = "State Percentile of Demographic Index",                                                 
#                    "S_D_DEMOGIDX5ST_PER" = "State Percentile of Supplemental Demographic Index",
#                    "N_D_PEOPCOLOR"       = "National Average of People of Color (%)",
#                    "N_D_INCOME"          = "National Average of Low Income (%)",
#                    "N_D_LESSHS"          = "National Average of Less Than High School Education (%)",
#                    "N_D_LING"            = "National Average of Limited English Speaking (%)",
#                    "N_D_UNDER5"          = "National Average of Under Age 5 (%)",
#                    "N_D_OVER64"          = "National Average of Over Age 64 (%)",
#                    "N_D_UNEMPLOYED"      = "National Average of Unemployment Rate (%)",
#                    "N_D_LIFEEXP"         = "National Average of Limited Life Expectancy (%)",
#                    "N_D_DEMOGIDX2"       = "National Average of Demographic Index",
#                    "N_D_DEMOGIDX5"       = "National Average of Supplemental Demographic Index",
#                    "N_D_MINOR_PER"       = "National Percentile of People of Color",
#                    "N_D_INCOME_PER"      = "National Percentile of Low Income",
#                    "N_D_LESSHS_PER"      = "National Percentile of Less Than High School Education",
#                    "N_D_LING_PER"        = "National Percentile of Limited English Speaking",
#                    "N_D_UNDER5_PER"      = "National Percentile of Under Age 5",
#                    "N_D_OVER64_PER"      = "National Percentile of Over Age 64",
#                    "N_D_UNEMPLOYED_PER"  = "National Percentile of Unemployment Rate",
#                    "N_D_LIFEEXP_PER"     = "National Percentile of Limited Life Expectancy",
#                    "N_D_DEMOGIDX2_PER"   = "National Percentile of Demographic Index",
#                    "N_D_DEMOGIDX5_PER"   = "National Percentile of Supplemental Demographic Index",
#                    "DISTANCE"            = "Distance (miles)")
#   
#   demo_header <- names(demo_labels)  # column names
#   demo_desc   <- unname(demo_labels) # description of column names
#   
#   # add new column names
#   colnames(demo_file) <- demo_header
#   
#   ## write data
#   # write data for first row only
#   writeData(wb, 
#             sheet = demo, 
#             t(demo_desc), 
#             startRow = 1, 
#             colNames = FALSE)
#   
#   # write data to sheet
#   writeData(wb, 
#             sheet = demo, 
#             demo_file,
#             startRow = 2)
#   
#   ## add styles to document
#   # add description styles
#   addStyle(wb, sheet = demo, style = s[['desc_style']], rows = 1, cols = 1:65, gridExpand = TRUE)
#   
#   # add header style
#   addStyle(wb, sheet = demo, style = s[['header_style']], rows = 2, cols = 1:65, gridExpand = TRUE)
#   
#   # set column widths
#   setColWidths(wb, sheet = demo, cols = 1:2,     widths = 12.71)
#   setColWidths(wb, sheet = demo, cols = 3,       widths = 12.43)
#   setColWidths(wb, sheet = demo, cols = 4,       widths = 34.71)
#   setColWidths(wb, sheet = demo, cols = 5:10,    widths = 12.45)
#   setColWidths(wb, sheet = demo, cols = 11,      widths = 12.55)
#   setColWidths(wb, sheet = demo, cols = 12,      widths = 13)
#   setColWidths(wb, sheet = demo, cols = 13,      widths = 18)
#   setColWidths(wb, sheet = demo, cols = 14:18,   widths = 13)
#   setColWidths(wb, sheet = demo, cols = 19,      widths = 17.18)
#   setColWidths(wb, sheet = demo, cols = 20,      widths = 13)
#   setColWidths(wb, sheet = demo, cols = 21:22,   widths = 15.55)
#   setColWidths(wb, sheet = demo, cols = 23:24,   widths = 17.64)
#   setColWidths(wb, sheet = demo, cols = 25:29,   widths = 13.84)
#   setColWidths(wb, sheet = demo, cols = 30:34,   widths = 15)
#   setColWidths(wb, sheet = demo, cols = 35,      widths = 18)
#   setColWidths(wb, sheet = demo, cols = 36:40,   widths = 15)
#   setColWidths(wb, sheet = demo, cols = 41,      widths = 18.57)
#   setColWidths(wb, sheet = demo, cols = 42,      widths = 15)
#   setColWidths(wb, sheet = demo, cols = 43:44,   widths = 18.86)
#   setColWidths(wb, sheet = demo, cols = 45:60,   widths = 15)
#   setColWidths(wb, sheet = demo, cols = 61,      widths = 20)
#   setColWidths(wb, sheet = demo, cols = 62,      widths = 15)
#   setColWidths(wb, sheet = demo, cols = 63:64,   widths = 18.29)
#   setColWidths(wb, sheet = demo, cols = 65,      widths = 15)
#   
#   # set row heights
#   setRowHeights(wb, sheet = demo, row = 1, heights = 67.5)
#   
#   # add number styles
#   addStyle(wb, sheet = demo, style = s[['integer']],   rows = 3:demo_rows, cols = 12:20,  gridExpand = TRUE)
#   addStyle(wb, sheet = demo, style = s[['decimal5']],  rows = 3:demo_rows, cols = 21:24,  gridExpand = TRUE)
#   addStyle(wb, sheet = demo, style = s[['integer']],   rows = 3:demo_rows, cols = 25:32,  gridExpand = TRUE)
#   addStyle(wb, sheet = demo, style = s[['decimal5']],  rows = 3:demo_rows, cols = 33:34,  gridExpand = TRUE)
#   addStyle(wb, sheet = demo, style = s[['integer']],   rows = 3:demo_rows, cols = 35:52,  gridExpand = TRUE)
#   addStyle(wb, sheet = demo, style = s[['decimal5']],  rows = 3:demo_rows, cols = 53:54,  gridExpand = TRUE)
#   addStyle(wb, sheet = demo, style = s[['integer']],   rows = 3:demo_rows, cols = 55:65,  gridExpand = TRUE)
#   
#   # add text styles
#   addStyle(wb, sheet = demo, style = s[['basic']], rows = 3:demo_rows, cols = 1:11, gridExpand = TRUE)
#   
#   # freeze panes
#   freezePane(wb, sheet = demo, firstActiveCol = 6, firstActiveRow = 3)
# }


# Contents Formatting -----------------------------------------

# # add link to sheets 
# add_hyperlink(glue::glue("UNT{year}"),  row_link = 1, col_link = 1, loc = c(3, 9), text_to_show = glue::glue("UNT{year}"))
# add_hyperlink(glue::glue("GEN{year}"),  row_link = 1, col_link = 1, loc = c(3, 10), text_to_show = glue::glue("GEN{year}"))
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 1, loc = c(3, 11), text_to_show = glue::glue("PLNT{year}"))
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 1, loc = c(3, 12), text_to_show = glue::glue("ST{year}"))
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 1, loc = c(3, 13), text_to_show = glue::glue("BA{year}"))
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 1, loc = c(3, 14), text_to_show = glue::glue("SRL{year}"))
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 1, loc = c(3, 15), text_to_show = glue::glue("NRL{year}"))
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 1, loc = c(3, 16), text_to_show = glue::glue("US{year}"))
# add_hyperlink(glue::glue("GGL{year}"),  row_link = 1, col_link = 1, loc = c(3, 17), text_to_show = glue::glue("GGL{year}"))
# 
# if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/demographics_file.RDS"))) {
#   add_hyperlink(glue::glue("DEMO{year}"),  row_link = 1, col_link = 1, loc = c(3, 18), text_to_show = glue::glue("DEMO{year}"))
# }
# 
# # add hyperlinks to specific columns
# # annual values 
# add_hyperlink(glue::glue("GEN{year}"),  row_link = 1, col_link = 13, loc = c(11, 27), text_to_show = "GEN")
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 37, loc = c(12, 27), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 5, loc = c(13, 27), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 5, loc = c(14, 27), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 5, loc = c(15, 27), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 5, loc = c(16, 27), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 3, loc = c(17, 27), text_to_show = "US")
# 
# # unadjusted values 
# add_hyperlink(glue::glue("UNT{year}"),  row_link = 1, col_link = 15, loc = c(10, 28), text_to_show = "UNT")
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 76, loc = c(12, 28), text_to_show = "PLNT")
# 
# # adjustment values (biomass and CHP)
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 98, loc = c(12, 29), text_to_show = "PLNT")
# 
# # output emissions rates 
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 52, loc = c(12, 30), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 20, loc = c(13, 30), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 20, loc = c(14, 30), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 20, loc = c(15, 30), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 20, loc = c(16, 30), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 18, loc = c(17, 30), text_to_show = "US")
# 
# # input emissions rates
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 60, loc = c(12, 31), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 28, loc = c(13, 31), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 28, loc = c(14, 31), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 28, loc = c(15, 31), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 28, loc = c(16, 31), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 26, loc = c(17, 31), text_to_show = "US")
# 
# # combustion output emissions rates
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 68, loc = c(12, 32), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 36, loc = c(13, 32), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 36, loc = c(14, 32), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 36, loc = c(15, 32), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 36, loc = c(16, 32), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 34, loc = c(17, 32), text_to_show = "US")
# 
# # generation by fuel type
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 115, loc = c(12, 33), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 112, loc = c(13, 33), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 112, loc = c(14, 33), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 112, loc = c(15, 33), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 112, loc = c(16, 33), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 110, loc = c(17, 33), text_to_show = "US")
# 
# # renewable and non-renewable generation
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 126, loc = c(12, 34), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 123, loc = c(13, 34), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 123, loc = c(14, 34), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 123, loc = c(15, 34), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 123, loc = c(16, 34), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 121, loc = c(17, 34), text_to_show = "US")
# 
# # combustion and non-combustion generation
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 130, loc = c(12, 35), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 127, loc = c(13, 35), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 127, loc = c(14, 35), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 127, loc = c(15, 35), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 127, loc = c(16, 35), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 125, loc = c(17, 35), text_to_show = "US")
# 
# # resource mix
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 133, loc = c(12, 36), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 130, loc = c(13, 36), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 130, loc = c(14, 36), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 130, loc = c(15, 36), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 130, loc = c(16, 36), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 128, loc = c(17, 36), text_to_show = "US")
# 
# # renewable and non-renewable resource mix
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 144, loc = c(12, 37), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 141, loc = c(13, 37), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 141, loc = c(14, 37), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 141, loc = c(15, 37), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 141, loc = c(16, 37), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 139, loc = c(17, 37), text_to_show = "US")
# 
# # combustion and non-combustion resource mix
# add_hyperlink(glue::glue("PLNT{year}"), row_link = 1, col_link = 148, loc = c(12, 38), text_to_show = "PLNT")
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 145, loc = c(13, 38), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 145, loc = c(14, 38), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 145, loc = c(15, 38), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 145, loc = c(16, 38), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 143, loc = c(17, 38), text_to_show = "US")
# 
# # output emission rates by fuel type
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 44, loc = c(13, 39), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 44, loc = c(14, 39), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 44, loc = c(15, 39), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 44, loc = c(16, 39), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 42, loc = c(17, 39), text_to_show = "US")
# 
# # input emission rates by fuel type
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 74, loc = c(13, 40), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 74, loc = c(14, 40), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 74, loc = c(15, 40), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 74, loc = c(16, 40), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 72, loc = c(17, 40), text_to_show = "US")
# 
# # nonbaseload output emission rates 
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 104, loc = c(13, 41), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 104, loc = c(14, 41), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 104, loc = c(15, 41), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 104, loc = c(16, 41), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 102, loc = c(17, 41), text_to_show = "US")
# 
# # nonbaseload generation by fuel type
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 148, loc = c(13, 42), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 148, loc = c(14, 42), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 148, loc = c(15, 42), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 148, loc = c(16, 42), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 146, loc = c(17, 42), text_to_show = "US")
# 
# # nonbaseload resource mix
# add_hyperlink(glue::glue("ST{year}"),   row_link = 1, col_link = 159, loc = c(13, 43), text_to_show = "ST")
# add_hyperlink(glue::glue("BA{year}"),   row_link = 1, col_link = 159, loc = c(14, 43), text_to_show = "BA")
# add_hyperlink(glue::glue("SRL{year}"),  row_link = 1, col_link = 159, loc = c(15, 43), text_to_show = "SRL")
# add_hyperlink(glue::glue("NRL{year}"),  row_link = 1, col_link = 159, loc = c(16, 43), text_to_show = "NRL")
# add_hyperlink(glue::glue("US{year}"),   row_link = 1, col_link = 157, loc = c(17, 43), text_to_show = "US")

# Save and export -------------------------------------------
output <- glue::glue("data/1_production_model/outputs/{params$eGRID_year}/monthly/egrid{params$eGRID_year}_monthly_data.xlsx")
saveWorkbook(wb, output, overwrite = TRUE)

print(glue::glue("Saving final formatted file to folder data/1_production_model/outputs/{params$eGRID_year}/monthly/"))

# remove to save space
# rm(unt_file, gen_file, plnt_file)

