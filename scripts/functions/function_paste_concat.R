## -------------------------------
##
## Paste and concatenate
## 
## Purpose: 
## 
## This file creates a function that concatenates text values to a list if they are not NA
##
## Authors:  
##      Sara Sokolinski, Abt Global
##
## -------------------------------

# since we aggregate the unit and generator file to the plant ID level, this function keeps all text entries

paste_concat <- function(l, number = FALSE, concat = TRUE) {
  
  #' paste_concat
  #' 
  #' Function to concatenate unique values that are not NA
  #' 
  #' @param l column to concatenate 
  #' @param number flag whether or not the column is a numeric column
  #' @param concat flag whether or not the column should be collapsed into a list
  #' @return column with concatenated values that were not blank or NA
  #' @examples
  #' paste_concat(so2_source) # returns the column so2_source with concatenated strings where applicable
  
  l <- l[!is.na(l)]
  l <- l[l != "NA" & l != ""]
  l[order(l)]
  
  if(length(unique(l)) == 0) {
    txt <- NA_character_
  } else if(concat) {
     txt <- paste0(unique(l), collapse = ", ")
    if(txt == "") {
      txt <- NA_character_}
  } else {
     txt = unique(l)
  }
  if(number) {
    return(as.numeric(txt)) # convert to numeric if it can
  } else { 
    return(txt)}
}
