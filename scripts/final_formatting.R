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
unit_file <- readRDS('data/outputs/unit_file.RDS')
generator_file <- readRDS('data/outputs/generator_file.RDS')
plant_file <- readRDS('data/outputs/plant_file.RDS')
ggl_file <- readRDS('data/outputs/egrid_ggl_final.RDS') # maybe change file name to match others

### Set up output file
contents <- "data/outputs/formatting/egrid_contents_page.xlsx"
wb <- loadWorkbook(contents)
  
# setStyleAction?

### GGL Formatting

output <- read_excel()


### Unit File Formatting
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