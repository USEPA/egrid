
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)
library(rjson)
library(httr)
library(jsonlite)
library(data.table)


ejscreen_api <- function(lat, lon, id, mile) {
    # url strings that make up EJScreen API
    url1 <- 'https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=&geometry='
    url2 <- '{"spatialReference":{"wkid":4326},'
    lat_lon_input <- glue::glue('"x":{lon},"y":{lat}}')  # latitude, longitude input for plant
    url3 <- glue::glue('&distance={mile}&unit=9035&areatype=&areaid=&f=pjson')
    
    # call EJScreen API
    api_call <- paste0(url1, url2, lat_lon_input, url3)
    
    # load response
    res = GET(api_call)
    
    tryCatch (
      
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
        return (output_data) 
        
        }
  
      else {
        
        print("Error:", status_code(response))
        return(NA)
    } 
    , error = function(e) {
      return(NA)
    } )
  }

