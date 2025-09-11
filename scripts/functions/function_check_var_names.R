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
# input in header vars, input in variable vars
check_var_names <- function(file = "NA", header, header_check, temporal_res) {
  check_cols <- c()
  
  # annual check cols
  if (temporal_res == "annual"){
    
    for (i in 1:length((header))) {
      if (header[i] != names(header_check)[i]) {
        check_cols <- c(check_cols, header[i]) }}
    if (!is.null(check_cols)){
      print(glue::glue("These columns do not match name_matching.R: {glue::glue_collapse(check_cols, sep = ', ')}. Check for errors."))
    } else {
      print(glue::glue("All shorthand columns match name_matching.R."))
    }
    
  # monthly check cols
  } else if (temporal_res == "monthly"){
    
    for (i in 1:length((header))) {
      if (header[i] != header_check[i]) {
        check_cols <- c(check_cols, header[i]) }}
    
    if (!is.null(check_cols)){
      stop(print(glue::glue("These columns do not match {file} monthly columns: {glue::glue_collapse(check_cols, sep = ', ')}. Check for errors.")))
    } else {
      print(glue::glue("All shorthand columns match {file} monthly columns."))
    }
  }

}


rename_variables <- function(df, name_map, strict = TRUE, rename = TRUE) {
  # Flatten the mapping: alias -> standard name
  alias_to_standard <- unlist(
    lapply(names(name_map), function(std) {
      aliases <- name_map[[std]]
      setNames(rep(std, length(aliases)), aliases)
    })
  )
  
  # Current names in the data
  old_names <- names(df)
  new_names <- sapply(old_names, function(nm) {
    if (nm %in% names(alias_to_standard)) {
      alias_to_standard[[nm]]
    } else {
      if (strict) {
        stop(paste("Unknown variable name:", nm))
      } else {
        warning(paste("Unknown variable name, keeping as-is:", nm))
        nm
      }
    }
  })
  
  if (rename) {
    # Apply renaming
    names(df) <- new_names
    return(df)
  } else {
    return(new_names)
  }

}
