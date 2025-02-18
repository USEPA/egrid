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
library(lubridate)
library(tidyr)

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input parameters in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params) & "temporal_res" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year) 
    params$temporal_res <- readline(prompt = "Input temporal resolution (annual or monthly): ")
    params$temporal_res <- as.character(params$temporal_res) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
  params$temporal_res <- readline(prompt = "Input temporal resolution (annual or monthly): ")
  params$temporal_res <- as.character(params$temporal_res) 
}

# Specify grouping columns based on temporal_res parameter
temporal_res_cols_all <- 
  list("annual"  = c("year"), 
       "monthly" = c("year", "month"), 
       "daily"   = c("year", "month", "day"), 
       "hourly"  = c("year", "month", "day", "hour"))

temporal_res_cols <- unlist(temporal_res_cols_all[params$temporal_res], use.names = FALSE)

# Check if folder to store raw data exists, if not - create it
if (!dir.exists(glue::glue("data/raw_data/epa/{params$eGRID_year}"))) {
  dir.create(glue::glue("data/raw_data/epa/{params$eGRID_year}"), recursive = TRUE)
}

# Load necessary functions
source("scripts/functions/function_coalesce_join_vars.R")
source("scripts/functions/function_cols_to_add.R")

# Set your API key here
api_key <- read_lines("api_keys/epa_api_key.txt")

# API base url
api_url_base <- "https://api.epa.gov/easey"

# S3 bucket url base + s3Path (in get request) = the full path to the files
bucket_url_base <- 'https://api.epa.gov/easey/bulk-files/'

# CAMD Administrative Services API url to bulk data files endpoint
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

temporal_res_cols_to_add <- cols_to_add(params$temporal_res)

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
# this is done to reduce run time and memory for non-hourly aggregation resolutions
temporal_res_api_endpoint <- 
  c("annual"  = "Daily", 
    "monthly" = "Daily", 
    "daily"   = "Daily", 
    "hourly"  = "Hourly")

emissions_files <-
  bulk_files %>% 
  tidyr::unnest(cols = metadata) %>% 
  filter(dataType == "Emissions",
         dataSubType == temporal_res_api_endpoint[params$temporal_res],
         year == params$eGRID_year,
         !is.na(quarter)) %>% # this identifies quarterly aggregations
  mutate(file_path = paste0(bucket_url_base,s3Path)) 


# now iterating over each file path and binding into one dataframe.
emissions_data <- 
  purrr::map_df(emissions_files$file_path, ~ read_csv(.x))

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

emissions_id_cols <-  
  emissions_data %>%
  rename_with(tolower) %>% 
  janitor::clean_names() %>% 
  select(-c(all_of(cols_to_sum), "date")) %>%
  colnames()

month_name_map <- # creating map to recode numeric monthly values to names
  tolower(month.name) %>% 
  purrr::set_names(1:12)

ozone_months <- tolower(month.name)[5:9] # setting ozone months, which are May through September

emissions_data_r <- 
  emissions_data %>% 
  rename_with(tolower) %>% # this protects NOx rates from getting split with clean_names()
  janitor::clean_names() %>% 
  mutate(year = as.character(year(date)), # extracting year from date
         month = month(date), # extracting month from date
         #day = as.character(day(date)), # extracting day from date
         ) %>% 
  select(-date) %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, "\\|", ","))) %>% # SB 6/4/2024: Temporary fix for issue in API where there are a mix of pipes and commas in some character values
  group_by(pick(-c(all_of(cols_to_sum)))) %>% 
  summarize(across(all_of(cols_to_sum), ~ sum(.x, na.rm = TRUE))) %>% # aggregating to monthly values first 
  ungroup() %>% 
  group_by(facility_id, unit_id, primary_fuel_type, unit_type) %>% 
  mutate(reporting_months = paste(month, collapse = ", "), # creating column with list of reporting months 
         reporting_frequency = if_else(grepl("1|2|3|10|11|12", # filtering out non-ozone season reporting months, excluding april
                                             reporting_months), "Q", "OS")) %>% # assigning reporting frequency
  ungroup() %>% 
  group_by(pick(-all_of(cols_to_sum), -c(month, reporting_months, reporting_frequency))) %>% # group data by year
  mutate(across(all_of(cols_to_sum), ~ sum(.x, na.rm = TRUE)), # calculating annual emissions 
         across(all_of(cols_to_sum), ~ sum(.x[month %in% ozone_months], na.rm = TRUE), .names = "{.col}_ozone")) %>% # now calculating ozone month emissions
  ungroup() %>%
  select(all_of(temporal_res_cols), 
         facility_id, 
         unit_id, 
         primary_fuel_type, 
         unit_type, 
         program_code,
         reporting_frequency, 
         all_of(cols_to_sum),
         contains("ozone")) %>% 
  distinct() # removing duplicate rows that aren't needed after ozone calculation 
  #pivot_wider(id_cols = c(emissions_id_cols, year, reporting_months, reporting_frequency, paste0(cols_to_sum, "_annual"), paste0(cols_to_sum, "_ozone")), # pivot
  #            names_from = "month",
  #            values_from = all_of(cols_to_sum),
  #            names_glue = "{.value}_{month}") %>%
  #rename_with(.cols = contains("_annual"), # removing annual suffix 
  #            .fn = ~ str_remove(.x, "_annual")) 

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
  purrr::map_df(mats_files$file_path, ~ read_csv(.x)) # making all columns characters to avoid mismatches


cols <- # specifying columns to keep
  c("date",
    "state",
    "facility_name",
    "facility_id",
    "unit_id",
    "primary_fuel_type",
    "secondary_fuel_type",
    "hg_mass_lbs",
    "hg_controls")

# updating relevant columns to numeric and aggregating to month
mats_data_r <- 
  mats_data %>% 
  rename_with(tolower) %>% # this protects NOx rates from getting split with clean_names()
  janitor::clean_names() %>%
  select(all_of(cols)) %>%
  mutate(hg_mass_lbs = as.numeric(hg_mass_lbs),
         year = as.character(year(date)),
         month = month(date),
         #day = as.character(day(date))
         ) %>% 
  group_by(pick(all_of(temporal_res_cols)), state, facility_name, facility_id, unit_id, primary_fuel_type, secondary_fuel_type, hg_controls) %>% 
  summarize(hg_mass_lbs = sum(hg_mass_lbs, na.rm = TRUE)) %>% # aggregate to the monthly level
  ungroup() %>%
  distinct() #%>%
  #pivot_wider(id_cols = c("state", "facility_name", "facility_id", "unit_id", "primary_fuel_type", "secondary_fuel_type"), # pivot
  #            names_from = "month", 
  #            values_from = "hg_mass_lbs",
  #            names_glue = "{.value}_{month}") %>%
  #mutate(hg_mass_lbs = rowSums(select(.,starts_with("hg_mass_lbs_")), na.rm = TRUE)) # sum all months to create annual value


## Join facility, emissions, and MATS data --------

epa_data_combined <- 
  facility_df %>% 
  left_join(emissions_data_r,
            by = c(all_of(temporal_res_cols), "facility_id", "unit_id", "primary_fuel_type")) %>% 
  coalesce_join_vars() %>% 
  left_join(mats_data_r) %>% 
  arrange(facility_id, unit_id)
  
## Saving EPA data 

print(glue::glue("Writing file epa_raw_{params$temporal_res}.RDS to folder data/raw_data/epa/{params$eGRID_year}."))

readr::write_rds(epa_data_combined, 
                 file = glue::glue("data/raw_data/epa/{params$eGRID_year}/epa_raw_{params$temporal_res}.RDS"))

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/raw_data/epa/{params$eGRID_year}/epa_raw_{params$temporal_res}.RDS"))){
  print(glue::glue("File epa_raw_{params$temporal_res}.RDS successfully written to folder data/raw_data/epa/{params$eGRID_year}"))
} else {
   print(glue::glue("File epa_raw_{params$temporal_res}.RDS failed to write to folder."))
}

