## -------------------------------
##
## Update sources 
## 
## Purpose: 
## 
## This file creates a function that updates sources to be more general in the plant file
##
## Authors:  
##      Sara Sokolinski, Abt Global
##
## -------------------------------

# this function simplifies the source and removes any duplicate sources
update_source <- function(x, unit_f) {
  
  #' update_source
  #' 
  #' Function to update sources to more generalized sources in plant file. 
  #' 
  #' @param x Column to update source
  #' @param unit_f Unit file to update sources with
  #' @return Data frame with plant_id and column (x) with updated sources
  #' @examples
  #' update_source("heat_input_source", unit_file) # Updates sources in the "heat_input_source" column in the unit file specified
  
  
  str_col <- x
  x <- as.name(x)
  
  # update sources in unit file
  unit_source <- 
    unit_f %>% 
    select(plant_id, !!x) %>% 
    group_by(plant_id, !!x) %>% 
    filter(!is.na(!!x)) %>%
    mutate(source_update = case_when(!!x == "EIA Unit-level Data" ~ "EIA",
                                     !!x == "EIA Unit-Level Data" ~ "EIA",
                                     !!x == "EIA Prime Mover-level Data" ~ "EIA",
                                     !!x == "EPA/CAPD" ~ "EPA/CAPD", 
                                     !!x == "EIA non-ozone season distributed and EPA/CAPD ozone season" ~ "EPA/CAPD; EIA", 
                                     !!x == "Estimated using emissions factor" ~ "EIA"))
  
  # identify unique plant ID source updates
  unit_source <- 
    unit_source %>% 
    ungroup() %>%
    select(plant_id, source_update) %>%
    unique()
  
  # identify which plant_ids have multiple sources and pull their plant ID
  ids <- unit_source$plant_id[which(duplicated(unit_source$plant_id))] %>% unique()
  
  # if plant_ids have multiple sources, assign "EPA/CAMD; EIA"
  unit_source <- 
    unit_source %>% 
    mutate(source_update = if_else(plant_id %in% ids, "EPA/CAPD; EIA", source_update)) %>% 
    unique() # take unique again to remove duplicates
  
  colnames(unit_source) <- c("plant_id", str_col)
  
  return(unique(unit_source))
}