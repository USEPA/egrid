

library(dplyr)
library(readr)
library(httr)

# Define year to pull

egrid_year <- Sys.getenv("eGRID_year")


# Define the base URL and API key
api_key <- read_lines("api_keys/camd_api_key.txt") # read in api key

url_emissions <- glue::glue("https://api.epa.gov/easey/emissions-mgmt/emissions/apportioned/annual?year={egrid_year}&page=1&perPage=500")
url_emissions_ozone <- glue::glue("https://api.epa.gov/easey/emissions-mgmt/emissions/apportioned/ozone?year={egrid_year}&page=1&perPage=500")
url_facilities <- glue::glue("https://api.epa.gov/easey/facilities-mgmt/facilities/attributes?year={egrid_year}&page=1&perPage=500")


## Add in year as option here?
get_data <- function(url, api_key) {
    
    # Function to get data from one page
    parse_data <- function(page) {
      response <- GET(url, add_headers(`accept` = "*/*", `x-api-key` = api_key), query = list(page = page, perPage = 500))
      parsed_data <- content(response, as = "parsed") # extract data from request
      data <- as.data.frame(bind_rows(parsed_data))
      return(data)
    }
    
    # Get total count of rows and calculate number of required pages to pull
    response <- GET(url, add_headers(`accept` = "*/*", `x-api-key` = api_key)) # pulling first page to get count
    total_count <- as.numeric(response[["headers"]][["x-total-count"]]) # identifying total count of rows for query of interest
    n_pages <- ceiling(total_count / 500) # calculating # of required pages to pull
    
    # Pull data from all pages and combine together into dataframe
    all_data <- purrr::map_dfr(1:n_pages, parse_data)
    
    return(all_data)
  }

camd_facilities <- get_data(url_facilities, api_key)
camd_emissions <- get_data(url_emissions, api_key)
camd_emissions_ozone <- get_data(url_emissions_ozone, api_key) 


# save downloaded data as .RDS files in raw_data folder. Creating new "camd" folder

folder <- "data/raw_data/camd"

if(!dir.exists(folder)) { # checking to see if folder exists, and if not, creating one
  dir.create(folder)
}


readr::write_rds(camd_facilities, file = glue::glue("{folder}/camd_facilities.RDS"))
readr::write_rds(camd_emissions, file = glue::glue("{folder}/camd_emissions.RDS"))
readr::write_rds(camd_emissions_ozone, file = glue::glue("{folder}/camd_emissions_ozone.RDS"))

# Verifying successful writing

files <- list.files(folder)

if(length(files) > 0) {
print(glue::glue("Success! Files {glue::glue_collapse(files, sep = ', ', last = ', and ')} written to folder {folder}."))
} else {
  print("CAMD files failed to download. Check script `data_load_camd.R`")
}       
         




