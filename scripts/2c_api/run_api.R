## -------------------------------
##
## Run plumber API
## 
## Purpose: 
## 
## This file runs plumber API to connect RDS output files to the ETL process, and ultimately populate the eGRID API database. 
## 
## Authors:  
##      Elisabeth Ashley, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------

library(plumber)
library(this.path)

# Identify directory path for api.R
file_path <- this.path::this.path()
dir_path <- sub("run_api.R", "", file_path)

# Load and run the Plumber API
pr <- plumber::plumb("api.R", dir = dir_path) # Load the route definitions from api.R
pr$run(port = 8001)           # Specify the port
