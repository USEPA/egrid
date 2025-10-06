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
library(openxlsx)
library(readr)
library(stringr)
library(tidyr)

# Define eGRID year ------------------
# Load necessary functions
source("scripts/functions/function_check_params.R")

# Define parameters if necessary and check for valid params()
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

# Import .RDS data --------------------------------

# create a list of files in R directory
data_dir <- glue::glue("data/1_production_model/outputs/{params$eGRID_year}/")
filenames <- list(glue::glue("{params$temporal_res}/state_aggregation_{params$temporal_res}.RDS"), 
                  glue::glue("{params$temporal_res}/subregion_aggregation_{params$temporal_res}.RDS"), 
                  "grid_gross_loss.RDS", 
                  glue::glue("{params$temporal_res}/us_aggregation_{params$temporal_res}.RDS"))

# import files in list
for (file in (filenames)){
  assign(str_remove_all(file, "_annual|_monthly|.RDS|annual/|monthly/"), read_rds(paste0(data_dir, file)))
}

# Format subregion output emissions rates for TABLE 1   ------------------

# read in crosswalk assigning subregions to interconnect regions
xwalk_subregion_ggl <- 
  read.csv("data/1_production_model/static_tables/xwalk_subregion_interconnect.csv") %>%
  janitor::clean_names() %>%
  rename(subregion = subregion_code) %>%
  # join grid gross loss data by interconnect assignment
  left_join(grid_gross_loss %>%
              select(interconnect, ggl), by = "interconnect")

# list emission types wanted in summary tables
emission_types <- c(
  "co2",
  "ch4",
  "n2o",
  "co2e",
  "nox",
  "nox_oz",
  "so2")

# US output emissions data
us_output_emissions <-
  us_aggregation %>%
  select(any_of(paste0("us_", emission_types, "_output_rate")), 
         any_of(paste0("us_", emission_types, "_output_rate_nonbaseload"))) %>%
  rename_with(~str_c(str_remove(., "us_")))

# subregion output emissions
subregion_output_emissions <-
  subregion_aggregation %>%
  select(subregion, subregion_name, any_of(paste0("subregion_", emission_types, "_output_rate")), 
         any_of(paste0("subregion_", emission_types, "_output_rate_nonbaseload"))) %>%
  rename_with(~str_c(str_remove(., "subregion_"))) %>%
  # add US data to bottom row
  bind_rows(us_output_emissions) %>%
  mutate(subregion = replace_na(subregion, "U.S.")) %>%
  # add grid gross loss as column
  left_join(xwalk_subregion_ggl, by = "subregion") %>%
  # remove interconnect data
  select(!interconnect)

# Format subregion resource mix data for TABLE 2 ------------------

# list resource types wanted in summary tables
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

# US emissions data
us_resource_mix <-
  us_aggregation %>%
  select(us_nameplate_capacity, us_generation, any_of(paste0("us_", resource_type, "_resource_mix"))) %>%
  rename_with(~str_c(str_remove(., "us_")))

# subregion resource mix
subregion_resource_mix <-
  subregion_aggregation %>%
  select(subregion, subregion_name, subregion_nameplate_capacity, subregion_generation, 
         any_of(paste0("subregion_", resource_type, "_resource_mix"))) %>%
  rename_with(~str_c(str_remove(., "subregion_"))) %>%
  # add US data to bottom row
  bind_rows(us_resource_mix) %>%
  mutate(subregion = replace_na(subregion, "U.S."))

# Format state output emission rates for TABLE 3 -------------------

# state output emission rates
state_output_emissions <-
  state_aggregation %>%
  select(state, any_of(paste0("state_", emission_types, "_output_rate"))) %>%
  rename_with(~str_c(str_remove(., "state_"))) %>%
  # add US data to bottom row
  bind_rows(us_output_emissions %>% 
              select(!contains("nonbaseload"))) %>%
  mutate(state = replace_na(state, "U.S."))

# Format state resource mix data for TABLE 4  ----------------------

# state resource mix
state_resource_mix <-
  state_aggregation %>%
  select(state, state_nameplate_capacity, state_generation, 
         any_of(paste0("state_", resource_type, "_resource_mix"))) %>%
  rename_with(~str_c(str_remove(., "state_"))) %>%
  # add US data to bottom row
  bind_rows(us_resource_mix) %>%
  mutate(state = replace_na(state, "U.S."))


# Create excel workbook -------------------

wb <- createWorkbook()

# set base font
modifyBaseFont(wb, fontName = "Arial")

# define data for sheets
table_data <- list(subregion_output_emissions,
                   subregion_resource_mix,
                   state_output_emissions,
                   state_resource_mix)

# add sheet titles
names(table_data) <- c(
  glue::glue("1. Subregion Output Emission Rates (eGRID{params$eGRID_year})"),
  glue::glue("2. Subregion Resource Mix (eGRID{params$eGRID_year})"),
  glue::glue("3. State Output Emission Rates (eGRID{params$eGRID_year})"),
  glue::glue("4. State Resource Mix (eGRID{params$eGRID_year})"))

# write contents data on sheet 1
source("scripts/functions/function_create_contents_summary_tables.R")
create_contents_summary_tables()

# write table data on sheets 2-5
source("scripts/functions/function_create_summary_tables.R")
create_summary_tables()
                      
# Save excel sheet -------------------------------

saveWorkbook(wb, glue::glue("data/1_production_model/outputs/{params$eGRID_year}/{params$temporal_res}/egrid{params$eGRID_year}_summary_tables_{params$temporal_res}.xlsx"), 
             overwrite = TRUE)
