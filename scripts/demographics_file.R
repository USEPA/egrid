## -------------------------------
##
## Demographics create 
## 
## Purpose: 
## 
## This file accesses EPA's EJScreen API to collect information about neighboring demographics
## of plants at a 3-mile radius. 
##
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------


### Load libraries ------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)
library(rjson)
library(httr)
library(jsonlite)

### Set Params ------

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}


### Load in datasets ------

plant_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS"))

# subset data to columns

plant_file <- plant_file %>%
              select(seqplt, 
                     year, 
                     plant_state, 
                     plant_name, 
                     plant_id, 
                     lat, 
                     lon, 
                     primary_fuel_type, 
                     primary_fuel_category, 
                     nameplate_capacity, 
                     coal_flag)

## create inputs 

# set up input id, lat, lon

id_input  <- plant_file %>%
             select(plant_id)

lat_input <- plant_file %>%
             select(lat)

lon_input <- plant_file %>%
             select(lon)

# set up adjustable mile setting
mile <- 3


### EJScreen API Loop -----

# create empty list to store data
demo_data <- list()

for (i in 1:nrow(id_input)) {
  
  # inputs for API
  id  <- id_input[i,]
  
  lat <- lat_input[i,]
  
  lon <- lon_input[i,]
  
  # print which plant_id
  print(paste("Running plant", as.character(id), "[", i, "/", nrow(id_input), "]"))
  
  # url strings that make up EJScreen API
  url1 <- 'https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&geometry='
  url2 <- '{"spatialReference":{"wkid":4326},'
  lat_lon_input <- glue::glue('"x":{lon},"y":{lat}}')  # latitude, longitude input for plant
  url3 <- glue::glue('&distance={mile}&unit=9035&areatype=&areaid=&f=pjson')
  
  # call EJScreen API
  api_call <- paste0(url1, url2, lat_lon_input, url3)
  
  # load response
  res = GET(api_call)
  
  
  # status = 200 means data is collected
  if (status_code(res) == 200) {
    
    # extract data 
    data <- content(res, as = "text", encoding ="UTF-8")
    
    # from extracted data, transform from JSON format
    parsed_data <- fromJSON(data, flatten = TRUE)
    
    # unlist data into useable format
    # parsed_data <- data.frame(matrix(unlist(parsed_data), 
    #                           ncol = length(parsed_data[[1]]), byrow = TRUE), 
    #                           stringsAsFactors = FALSE)
    
    # converts to useable format
    output_data <- cbind(parsed_data)
    
    # inverses row/column direction
    output_data <- t(output_data)
    
    # add output data to list
    demo_data[[i]] <- output_data
    
  } else {
    
    print("Error:", status_code(response))
  }
  
}

demo_data <- t(demo_data)
demo_data <- rbind(demo_data)
demo_data <- as.data.frame(demo_data)

write.xlsx(demo_data, file = glue::glue("data/outputs/{params$eGRID_year}/demo_file.xlsx"))

demo_file <- read_xlsx(glue::glue("data/outputs/{params$eGRID_year}/demo_file.xlsx"))

# keep list within the loop 

# colnames(output_data) <- c("headers", "values")


# for length of id column
# iterate over the loop
# set lat and lon to be have i inside
# add stuffing inside

# clean up columns to only ones that are needed

# do this at the end outside of loop 
# rbind together demo_data 
# attach desired plant_file columns
# view_data <- as.data.frame(view_data)

