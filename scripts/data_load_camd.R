

library(httr)
library(jsonlite)
library(stringr)
library(readr)
library(dplyr)


# Set your API key here
api_key <- read_lines("api_keys/camd_api_key.txt")


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



## facility data --------

facility_path <- 
  bulk_files %>% 
  unnest(cols = metadata) %>% 
  filter(year == params$eGRID_year,
         dataType == "Facility" ) %>% 
  pull(s3Path)


facility_df <- 
  read_csv(paste0(bucket_url_base,facility_path)) %>% 
  rename_with(tolower) %>% # this protects NoX rates from getting split with clean_names()
  janitor::clean_names() %>% 
  mutate(year = as.character(year))


## emissions data -------
emissions_files <-
  bulk_files %>% 
  tidyr::unnest(cols = metadata) %>% 
  filter(dataType == "Emissions",
         dataSubType == "Daily",
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

month_name_map <- # creating map to recode numeric monthly values to names
  tolower(month.name) %>% 
  purrr::set_names(1:12)

ozone_months <- tolower(month.name)[5:9] # setting ozone months, which are May through September

emissions_data_r <- 
  emissions_data %>% 
  rename_with(tolower) %>% # this protects NoX rates from getting split with clean_names()
  janitor::clean_names() %>% 
  mutate(year = as.character(year(date)), # extracting year from date
        month = as.character(month(date)), # extracting month from date
        month = recode(month, !!!month_name_map)) %>% # updating month to name
  select(-date) %>%
  group_by(pick(-c(all_of(cols_to_sum)))) %>% 
  summarize(across(cols_to_sum, ~ sum(.x, na.rm = TRUE))) %>% # aggregating to monthly values first
  ungroup() %>% 
  group_by(facility_id, unit_id, primary_fuel_type, unit_type) %>% 
  mutate(n_months = n(), # counting months to determine reporting frequency
         reporting_frequency = if_else(n_months == 12, "Q", "OS")) %>% # 12 months identifies Q reporters
  group_by(pick(-all_of(cols_to_sum), -c(month, n_months, reporting_frequency))) %>% 
  mutate(across(cols_to_sum, ~ sum(.x, na.rm = TRUE), .names = "{.col}_annual")) %>% # calculating annual emissions
  filter(month %in% ozone_months) %>% # filtering to only ozone months 
  mutate(across(cols_to_sum, ~ sum(.x, na.rm = TRUE), .names = "{.col}_ozone")) %>% # now calculating ozone month emissions
  select(-month) %>% # removing month so distinct() will aggregate to unit level
  select(-all_of(cols_to_sum), n_months, reporting_frequency) %>% 
  rename_with(.cols = contains("_annual"), # removing annual suffix
              .fn = ~ str_remove(.x, "_annual")) %>% 
  ungroup() %>% 
  distinct() # removing duplicate rows that aren't needed after ozone calculation

## mats data --------------

# creating dataframe of MATS data and relevant files paths to download
mats_files <- 
  bulk_files %>% 
  tidyr::unnest(cols = metadata) %>% # unnesting bulk data for easier filtering
  filter(dataType == "Mercury and Air Toxics Emissions (MATS)", # only MATS data
         year == params$eGRID_year, # setting year
         dataSubType == "Hourly") %>% # Hourly data contains the quarterly aggregations
  mutate(file_path = paste0(bucket_url_base,s3Path)) # creating file path for reading in data

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
    "hg_controls"
  )

# updating relevant columns to numeric and aggregating to mon
mats_data_r <- 
  mats_data %>% 
  rename_with(tolower) %>% # this protects NoX rates from getting split with clean_names()
  janitor::clean_names() %>%
  select(all_of(cols)) %>%
  mutate(hg_mass_lbs = as.numeric(hg_mass_lbs),
         year = as.character(year(date)),
         month = as.character(month(date))) %>% 
  group_by(year, state, facility_name, facility_id, unit_id, primary_fuel_type, secondary_fuel_type, hg_controls) %>% 
  summarize(hg_mass_lbs = sum(hg_mass_lbs, na.rm = TRUE)) %>% 
  ungroup() 

## joining facility, emissions, and MATS data --------


camd_data_combined <- 
  facility_df %>% 
  left_join(emissions_data_r,
            by = c("facility_id", "unit_id", "primary_fuel_type")) %>% 
  coalesce_join_vars() %>% 
  left_join(mats_data_r) %>% 
  arrange(facility_id, unit_id)
  
## Saving camd data 

print("Writing file camd.RDS to folder data/raw_data/camd.")

readr::write_rds(camd_data_combined, 
                 file = "data/raw_data/camd/camd_raw.RDS")


