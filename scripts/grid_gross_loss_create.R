## -------------------------------
##
## Grid gross loss create 
## 
## Purpose: 
## 
## This file creates the grid gross loss calculation for eGRID. 
## This includes all operating units for the specified eGRID data year
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------

# Load libraries ------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

## Read in EIA files ------

eia_860 <- read_rds("data/clean_data/eia/eia_860_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia/eia_923_clean.RDS")



  