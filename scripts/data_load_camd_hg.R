
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


# creating dataframe of MATS data and relevant files paths to download
mats_files <- 
    bulk_files %>% 
    tidyr::unnest(cols = metadata) %>% # unnesting bulk data for easier filtering
    filter(dataType == "Mercury and Air Toxics Emissions (MATS)", # only MATS data
           year == Sys.getenv("eGRID_year"), # setting year
           dataSubType == "Daily") %>% # selecting daily data
    mutate(file_path = paste0(bucket_url_base,s3Path)) # creating file path for reading in data

# now iterating over each file path and binding into one dataframe.
mats_data <- 
  purrr::map_df(mats_files$file_path, ~ read_csv(.x, 
                                                 col_types = cols(.default = "c"))) # making all columns characters to avoid mismatches



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
  janitor::clean_names() %>%
  select(all_of(cols)) %>%
  mutate(hg_mass_lbs = as.numeric(hg_mass_lbs),
         year = as.character(year(date)),
         month = as.character(month(date))) %>% 
  group_by(year, state, facility_name, facility_id, unit_id, primary_fuel_type, secondary_fuel_type, hg_controls) %>% 
  summarize(hg_mass_lbs = sum(hg_mass_lbs, na.rm = TRUE))
  
# saving data to combine with other camd data

write_rds(mats_data_r, "data/raw_data/camd/camd_hg.RDS")
