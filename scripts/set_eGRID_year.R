

# read in text file containing year
text <- readLines("eGRID_year.txt", warn = FALSE)

# extract only the year -- removing white spaces
year <- stringr::str_trim(text[[5]])


# save year in environment

Sys.setenv(eGRID_year = year)