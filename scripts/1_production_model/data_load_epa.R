 ## -------------------------------
##
## Data load EPA
## 
## Purpose: 
## 
## This file loads the EPA data set from an API and aggregates data to specified temporal resolution.
## 
## Authors:  
##      Sean Bock, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries ------------

library(httr)
library(jsonlite)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)

# Load necessary functions
source("scripts/functions/function_coalesce_join_vars.R")
source("scripts/functions/function_temporal_res_cols.R")
source("scripts/functions/function_check_params.R")
source("scripts/functions/function_save_output_data.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")
}

# Set your API key here
api_key <- read_lines("api_keys/epa_api_key.txt")

# API base url
api_url_base <- "https://api.epa.gov/easey"

# S3 bucket url base + s3Path (in get request) = the full path to the files
bucket_url_base <- 'https://api.epa.gov/easey/bulk-files/'

# CAMPD Administrative Services API url to bulk data files endpoint
services_url <- paste0(api_url_base,"/camd-services/bulk-files?API_KEY=", api_key)

# executing get request
res = GET(services_url)

# printing the response error message if the response is not successful
if (res$status_code > 399){
  errorFrame <- fromJSON(rawToChar(res$content))
  stop(paste("Error Code:",errorFrame$error$code,errorFrame$error$message))
}

# converting the content from json format to a data frame
bulk_files <- fromJSON(rawToChar(res$content))

## Get facility data --------

facility_path <- 
  bulk_files %>% 
  unnest(cols = metadata) %>% 
  filter(year == params$eGRID_year,
         dataType == "Facility") %>% 
  pull(s3Path)

# annual version will use monthly version of EPA data 
temporal_res_cols_to_add <- cols_to_add("monthly")
temporal_res_cols <- create_temporal_res_cols("monthly")

facility_df <- 
  read_csv(paste0(bucket_url_base,facility_path)) %>% 
  rename_with(tolower) %>% # this protects NOx rates from getting split with clean_names()
  janitor::clean_names() %>% 
  mutate(
    generator_ids = str_extract_all(associated_generators_nameplate_capacity_mwe, "\\S+(?= \\()"), # extracting associated generators
    nameplate_capacity_char = (str_extract_all(associated_generators_nameplate_capacity_mwe, "(?<=\\()\\d+(\\.\\d+)?(?=\\))")), # extracting nameplate capacity values
    associated_generators = purrr::map_chr(generator_ids, ~ paste(.x, collapse = ", ")), # pasting together associated generators
    nameplate_capacity = purrr::map_dbl(nameplate_capacity_char, ~ sum(as.numeric(.x), na.rm = TRUE)),
    year = as.character(year)) %>% # summing nameplate capacity from associated generators
  select(-"nameplate_capacity_char") %>% 
  full_join(temporal_res_cols_to_add)

## Get emissions data -------

# specify different endpoints for Emissions data based on temporal_res parameter
temporal_res_api_endpoint <- 
  c("annual"  = "Daily", 
    "monthly" = "Daily")

# select respective file paths based on temporal_res
emissions_files <-
  bulk_files %>% 
  tidyr::unnest(cols = metadata) %>% 
  filter(dataType == "Emissions",
         dataSubType == temporal_res_api_endpoint[params$temporal_res],
         year == params$eGRID_year,
         !is.na(quarter)) %>% # this identifies quarterly aggregations
  mutate(file_path = paste0(bucket_url_base,s3Path)) 

# now iterating over each file path and binding into one dataframe
emissions_data <- 
  purrr::map_df(emissions_files$file_path, ~ read_csv(.x))

# data columns to aggregate and sum after grouping by 
cols_to_sum <- 
  c("operating_time_count",
    "sum_of_the_operating_time",
    "gross_load_mwh",
    "steam_load_1000_lb",
    "so2_mass_short_tons",
    "so2_rate_lbs_mmbtu",
    "co2_mass_short_tons",
    "co2_rate_short_tons_mmbtu",
    "nox_mass_short_tons",
    "nox_rate_lbs_mmbtu",
    "heat_input_mmbtu")

ozone_months <- c(5:9) # setting ozone months, which are May through September

# process and clean emissions data for all temporal_res conditions
emissions_data_r <-
  emissions_data %>%
  rename_with(tolower) %>% # this protects NOx rates from getting split with clean_names()
  janitor::clean_names() %>%
  mutate(year = as.character(lubridate::year(date)), # extracting year from date
         month = lubridate::month(date), # extracting month from date (needed for ozone)
         day = lubridate::day(date) # extracting day from date
       ) %>%
  select(-date) %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, "\\|", ","))) %>% # fix for issue in API where there are a mix of pipes and commas in some character values
  group_by(year, facility_id, unit_id, primary_fuel_type, unit_type) %>% # group by year, identify ozone reporters and aggregate data to monthly level
  mutate(reporting_months = paste(unique(month), collapse = ", "), # creating column with list of reporting months
         reporting_frequency = if_else(grepl("1|2|3|10|11|12", # filtering out non-ozone season reporting months, excluding april
                                               reporting_months), "Q", "OS")) %>% # assigning reporting frequency
  ungroup()

# conditional groupby columns, separated for easier comprehension
emissions_groupby_cols <-
  emissions_data_r %>%
  select(-c(all_of(cols_to_sum), reporting_months, reporting_frequency, year, month, day)) %>% # sum by columns outside of cols_to_sum & new vars
  colnames()

emissions_select_cols <- c(temporal_res_cols, emissions_groupby_cols, cols_to_sum, "reporting_months", "reporting_frequency") # used to drop columns based on temporal_res (i.e. annual = drop "month", "day")
emissions_groupby_cols <- c(temporal_res_cols, emissions_groupby_cols) # only sum to the specified temporal_res (i.e. monthly = c("year", "month))

emissions_data_r_2 <-
  emissions_data_r %>%
  group_by(pick(all_of(emissions_groupby_cols))) %>%
  mutate(across(all_of(cols_to_sum), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup() %>%
  select(all_of(emissions_select_cols)) %>% # will only keep temporal_res columns specified by params$temporal_res
  distinct()

## Get MATS data --------------

# creating dataframe of MATS data and relevant files paths to download
mats_files <- 
  bulk_files %>% 
  tidyr::unnest(cols = metadata) %>% # unnesting bulk data for easier filtering
  filter(dataType == "Mercury and Air Toxics Emissions (MATS)", # only MATS data
         year == params$eGRID_year, # setting year
         dataSubType == "Hourly") %>% # Hourly data contains the quarterly aggregations
  mutate(file_path = paste0(bucket_url_base, s3Path)) # creating file path for reading in data

# now iterating over each file path and binding into one dataframe.
mats_data <- 
  purrr::map_df(mats_files$file_path, ~ read_csv(.x, col_types = cols(.default = "c"))) # making all columns characters to avoid mismatches

cols <- # columns for grouping and for selecting
  c("state",
    "facility_name",
    "facility_id",
    "unit_id",
    "primary_fuel_type",
    "secondary_fuel_type",
    "hg_controls")

# updating relevant columns to numeric and aggregating to month
mats_data_r <- 
  mats_data %>% 
  rename_with(tolower) %>% # this protects NOx rates from getting split with clean_names()
  janitor::clean_names() %>%
  select(c(date, hour, all_of(cols), hg_mass_lbs)) %>% # specifying columns to keep
  mutate(hg_mass_lbs = as.numeric(hg_mass_lbs),
         facility_id = as.numeric(facility_id), # change to numeric to match for joins  
         year = as.character(lubridate::year(date)),
         month = lubridate::month(date),
         day = as.character(lubridate::day(date))
         ) %>%
  select(-date) %>% # remove date for easier group/summation
  group_by(pick(all_of(c(temporal_res_cols, cols)))) %>% # group by depending on temporal_res
  summarize(hg_mass_lbs = sum(hg_mass_lbs, na.rm = TRUE)) %>% # aggregate to the monthly level
  ungroup() %>%
  distinct()

## Join facility, emissions, and MATS data --------

epa_data_combined <- 
  facility_df %>% 
  left_join(emissions_data_r_2, 
            by = c(temporal_res_cols, "facility_id", "unit_id", "primary_fuel_type")) %>% 
  coalesce_join_vars() %>% 
  left_join(mats_data_r) %>% 
  arrange(facility_id, unit_id)
  
## Saving EPA data 

print(glue::glue("Writing file epa_raw.RDS to folder data/1_production_model/raw_data/epa/{params$eGRID_year}."))

file <- "epa_raw.RDS"

save_output_data(epa_data_combined, "data/1_production_model/raw_data/epa", file)

