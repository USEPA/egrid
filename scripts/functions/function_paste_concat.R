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

# Create a function to string concatenate unique values that are not NA
# since we aggregate the unit and generator file to the plant ID level, this function keeps all text entries

paste_concat <- function(l, number = FALSE, concat = TRUE){
  l <- l[!is.na(l)]
  l <- l[l!="NA" & l!=""]
  l[order(l)]
  
  if(length(unique(l)) == 0){txt <- NA_character_
  } else if(concat){
    txt <- paste0(unique(l), collapse = ", ")
    if(txt == ""){txt <- NA_character_}
  } else{
    txt = unique(l)
  }
  if(number){return(as.numeric(txt)) # convert to numeric if it can
  } else{return(txt)}
}
