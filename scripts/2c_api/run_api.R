library(plumber)

# Load and run the Plumber API
pr <- plumber::plumb("api.R") # Load the route definitions from api.R
pr$run(port = 8001)           # Specify the port
