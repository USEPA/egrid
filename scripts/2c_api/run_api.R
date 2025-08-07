library(plumber)
library(rstudioapi)

# Identify directory path for api.R
file_path <- getSourceEditorContext()$path
dir_path <- sub("run_api.R", "", file_path)

# Load and run the Plumber API
pr <- plumber::plumb("api.R", dir = dir_path) # Load the route definitions from api.R
pr$run(port = 8001)           # Specify the port
