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
check_var_names <- function(header, name_matching_cols) {
  check_cols <- c()
  for (i in 1:length((header))) {
    if (header[i] != names(name_matching_cols)[i]) {
      check_cols <- c(check_cols, header[i]) }}
  
  if (!is.null(check_cols)){
    stop(print(glue::glue("These columns do not match name_matching.R {name_matching_cols}: {glue::glue_collapse(check_cols, sep = ', ')}. Check for errors.")))
  } else {
    print("All shorthand columns match name_matching.R {name_matching_cols}.")
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

test <- rename_variables(st_file_ann, state_nonmetric_annual)

format_headers <- function(df, name_map) {
  for (colname in names(df)) {
    if (colname %in% names(style_map)) {
      col_index <- which(names(df) == colname)
      addStyle(
        wb, 
        sheet = "Data", 
        style = style_map[[colname]], 
        cols = col_index, 
        rows = 2, 
        gridExpand = TRUE
      )
    }
  }
}


format_cols <- function(df, name_map) {
  
  # format names 
  for (colname in names(df)) {
    if (colname %in% names(style_map)) {
      col_index <- which(names(df) == colname)
      addStyle(
        wb, 
        sheet = "Data", 
        style = style_map[[colname]], 
        cols = col_index, 
        rows = 2, 
        gridExpand = TRUE
      )
    }
  }
  
  # format descriptions
  
  # format text 
  
  # format widths and heights 
}