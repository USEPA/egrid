## -------------------------------
##
## Function check name variables
## 
## Purpose: 
## 
## This file formats the regional aggregation files for final_formatting.R
## This includes all operating units for the specified eGRID data year
##
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------

# check if shorthand names match name_matching.R and stop if not.
state_check_cols <- c()
for (i in 1:length((st_header))) {
  if (st_header[i] != names(state_nonmetric_annual)[i]) {
    state_check_cols <- c(state_check_cols, st_header[i]) }}

if (!is.null(state_check_cols)){
  stop(print(glue::glue("These columns do not match name_matching.R state_nonmetric_annual: {glue::glue_collapse(state_check_cols, sep = ', ')}. Check for errors.")))
} else {
  print("All shorthand columns match name_matching.R state_nonmetric_annual.")
}

# if x var is x name then change name
# if not then flag the column
