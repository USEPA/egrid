

# read in text file containing year
text <- readr::read_lines("eGRID_year.txt")

# extract only the year -- removing white spaces
year <- stringr::str_trim(text[[5]])


# save year in environment

Sys.setenv(eGRID_year = year)