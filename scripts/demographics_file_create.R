## -------------------------------
##
## Demographics file create 
## 
## Purpose: 
## 
## This file accesses EPA's EJScreen API to collect information about neighboring demographics
## of plants at a 3-mile radius. 
##
## Note: This file takes 4-5 hours to download data from the EJScreen API. 
##       params$run_demo_file in egrid_master.qmd is used as a flag whether to create this file when running eGRID. 
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------


# Load libraries ------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)
library(httr)
library(jsonlite)
library(data.table)

# Set parameters ------

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

# Load in datasets ------

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

# assign radius around plants to fetch demographic data from
mile <- 3


# Load plant data from EJScreen API -----

# create empty list to store data
demo_data <- list()

# this loop takes around 4- 5 hours to full load and process data from the EJScreen API
for (i in 1:nrow(id_input)) {
  
  # inputs for API
  id  <- id_input[i,]
  
  lat <- lat_input[i,]
 
  lon <- lon_input[i,]
  
  # print which plant_id
  print(paste("Running plant", as.character(id), "[", i, "/", nrow(id_input), "]"))
  
  # url strings that make up EJScreen API
  url1 <- 'https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=&geometry='
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
    data <- fromJSON(data, flatten = TRUE)
    
    # unnest individual rows (need to unnest twice)
    data <- purrr::flatten(data)
    data <- purrr::flatten(data)
    
    # converts to useable format
    data <- cbind(data)

    # inverses row/column direction
    data <- t(data)
    
    # add id input column for identification
    output_data <- c(id, data)
  
    # add output data to list
    demo_data[[i]] <- output_data
    

  } else {
    
    print("Error:", status_code(response))
    next
  }
  
}

# Clean and save Data -----

# select column names
column_names <- c("plant_id", colnames(data))

# filter for only full length output data
demo_data <- Filter(function(x) length(x) == 315, demo_data)

# convert list to dataframe
demo_data <- do.call(rbind, demo_data)
demo_data <- as.data.frame(demo_data) %>%
              unnest()

# rename columns
colnames(demo_data) <- column_names

# remove duplicated columns 
demo_data <- demo_data[, !duplicated(colnames(demo_data))]

# keep only demographic data columns
demo_data <- demo_data %>%
              select(totalPop, grep("_D_", colnames(demo_data), value = TRUE), distance, plant_id) %>%
              unique() %>% # unique since unnest() produced duplicate columns
              mutate(across(-plant_id, ~ as.numeric(sub("%", "",.))))

# join with plant file columns to keep by plant_id
demo_file <- plant_file %>%
             left_join(demo_data, by = "plant_id") %>%
             relocate(RAW_D_UNEMPLOYED, .before = RAW_D_LIFEEXP) %>%
             relocate(S_D_UNEMPLOYED, .before = S_D_LIFEEXP) %>%
             relocate(S_D_UNEMPLOYED_PER, .before = S_D_LIFEEXP_PER) %>%
             relocate(N_D_UNEMPLOYED, .before = N_D_LIFEEXP) %>%
             relocate(N_D_UNEMPLOYED_PER, .before = N_D_LIFEEXP_PER) 

# save output as RDS
write_rds(demo_file, file = glue::glue("data/outputs/{params$eGRID_year}/demographics_file.RDS"))


