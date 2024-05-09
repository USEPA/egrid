
# load required libraries

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)



# Load necessary data ----------

## eia ------------

eia_860 <- read_rds("data/clean_data/eia_860_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia_923_clean.RDS")

## lower-level eGRID files ------------

generator_file <- read_rds("data/outputs/generator_file.RDS")
#unit_file <- read_rds("data/outputs/unit_file.RDS") # This is where Unit file will be sourced once completed

clean_names <- # renaming column names to match naming conventions used in other files. This can be removed once new unit file is completed
  c("seq" = "SEQUNT",
    "year" = "YEAR",
    "plant_state" = "PSTATABB",
    "plant_name" = "PNAME",
    "plant_id" = "ORISPL",
    "unit_id" = "UNITID",
    "prime_mover" = "PRMVR",
    "operating_status" = "UNTOPST",
    "camd_flag" = "CAMDFLAG",
    "program_code" = "PRGCODE",
    "botfirty" = "BOTFIRTY",
    "n_generators" = "NUMGEN",
    "primary_fuel_type" = "FUELU1",
    "operating_hours" = "HRSOP",
    "heat_input" = "HTIAN",
    "heat_input_oz" = "HTIOZ",
    "nox" = "NOXAN",
    "nox_oz" = "NOXOZ",
    "so2" = "SO2AN",
    "co2" = "CO2AN",
    "hg" = "HGAN",
    "heat_input_source" = "HTIANSRC",
    "heat_input_oz_source" = "HTIOZSRC",
    "nox_source" = "NOXANSRC",
    "nox_oz_source" = "NOXOZSRC",
    "so2_source" = "SO2SRC",
    "co2_source" = "CO2SRC",
    "hg_source" = "HGSRC",
    "so2_controls" = "SO2CTLDV",
    "nox_controls" = "NOXCTLDV",
    "hg_controls" = "HGCTLDV",
    "year_online" = "UNTYRONL")


unit_file <- # using unit file from Access production for now
  read_excel("archive/eGRID_2021.xlsx", 
             sheet = "UNT21",
             skip = 1) %>% 
  rename(all_of(clean_names)) # rename columns based on named vector above


