## -------------------------------
##
## Create summary tables of finalized eGRID data.
## 
## Purpose: 
## 
## This file pulls data from .RDS files to create summary tables
## saved in an excel sheet.
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries ------------------------------------------

library(dplyr)
library(glue)
library(readr)
library(readxl)
#library(xlsx)
library(stringr)

# Define eGRID year ------------------

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.")
  } else { # if params() is defined, but eGRID_year is not, define it here
    params$eGRID_year <- as.character(readline(prompt = "Input eGRID_year: "))
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- as.character(readline(prompt = "Input eGRID_year: "))
}

# Import .RDS data --------------------------------

# create a list of files in R directory
rdata_dir <- glue::glue("data/outputs/{params$eGRID_year}/")
r_files <- list.files(path = rdata_dir, pattern = "*[^c].RDS")

# import files in list
for (file in (r_files)){
  assign(str_remove(file, ".RDS"), read_rds(paste0(rdata_dir, file)))
}

# associate larger regions to subregions, assign ggl
#subregion_ggl <-

# add US rows
  
# Pull desired data

# table 1
emission_types <- c(
  "co2",
  "ch4",
  "n2o",
  "co2e",
  "nox",
  "nox_oz",
  "so2")

# create dataset with US emissions data
us_output_emissions <-
  us_aggregation %>%
  select(any_of(paste0("us_", emission_types, "_output_rate")), 
         any_of(paste0("us_", emission_types, "_output_rate_nonbaseload"))) %>%
  rename_with(~str_c(str_remove(.,"us_")))
glimpse(us_reduced)

# create dataset with subregion emissions
subregion_output_emissions <-
  subregion_aggregation %>%
  select(subregion, subregion_name, any_of(paste0("subregion_", emission_types, "_output_rate")), 
                any_of(paste0("subregion_", emission_types, "_output_rate_nonbaseload"))) %>%
  rename_with(~str_c(str_remove(.,"subregion_"))) %>%
  # add US data to bottom row
  bind_rows(us_output_emissions)
glimpse(subregion_output_emissions)

  
# table 2
resource_type <- c(
  "coal",
  "oil",
  "gas",
  "other_ff",
  "nuclear",
  "hydro",
  "biomass",
  "wind",
  "solar",
  "geothermal",
  "other")

# create dataset with US emissions data
us_resource_mix <-
  us_aggregation %>%
  select(us_nameplate_capacity, us_generation_ann, any_of(paste0("us_ann_resource_mix_", resource_type))) %>%
  rename_with(~str_c(str_remove(.,"us_")))
glimpse(us_resource_mix)
  
# subregion resource mix
subregion_resource_mix <-
  subregion_aggregation %>%
  select(subregion, subregion_name, subregion_nameplate_capacity, subregion_generation_ann, 
         any_of(paste0("subregion_ann_resource_mix_", resource_type))) %>%
  rename_with(~str_c(str_remove(.,"subregion_"))) %>%
  # add US data to bottom row
  bind_rows(us_resource_mix)  
glimpse(subregion_resource_mix)
  
# table 3
# state output emission rates
state_output_emissions <-
  state_aggregation %>%
  select(state, any_of(paste0("state_", emission_types, "_output_rate")))
glimpse(state_output_emissions)        

# table 4
# state resource mix
state_resource_mix <-
  state_aggregation %>%
  select(state, state_nameplate_capacity, state_generation_ann, 
         any_of(paste0("state_ann_resource_mix_", resource_type)))
glimpse(state_resource_mix)

