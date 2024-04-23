

# Load libraries  ----


library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)


# Load necessary data ------

## camd ------
camd_vars_to_keep <- 
  c("year",
    "plant_state",
    "plant_name",
    "plant_id",
    "unit_id",
    "operating_status",
    "program_code",
    "primary_fuel_type",
    "unit_type" = "unit_type_abb",
    "operating_hours" = "operating_time_count",
    "heat_input" = "heat_input_mmbtu",
    "heat_input_oz" = "heat_input_mmbtu_ozone",
    "nox_mass" = "nox_mass_short_tons",
    "nox_mass_oz" = "nox_mass_short_tons_ozone",
    "so2_mass" = "so2_mass_short_tons",
    "so2_mass_oz" = "so2_mass_short_tons_ozone",
    "co2_mass" = "co2_mass_short_tons",
    "hg_mass" = "hg_mass_lbs",
    "heat_input_source",
    "heat_input_oz_source",
    "nox_source",
    "so2_source",
    "co2_source",
    "hg_source",
    "so2_controls",
    "nox_controls",
    "hg_controls",
    "year_online" = "commercial_operation_date"
    
    )


camd <- 
  read_rds("data/clean_data/camd/camd_clean.RDS") %>%
  select(all_of(camd_vars_to_keep)) # keeping only necessary variables

## eia ------------

eia_860 <- read_rds("data/clean_data/eia_860_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia_923_clean.RDS")


# Modifying CAMD data ---------

## Harmonizing fields with EIA ----------

## Gap fill ozone season reporters with EIA data ----------

# Add in EIA units

## EIA-923 boilers --------

## EIA-860 generators ---------------

# Distribute heat input -------------

# Estimate emissions --------

## NOx --------

## SO2 --------

## C02 --------

## Geothermal -------

# Create final structure for unit file

# Final edits to unit file ------------



