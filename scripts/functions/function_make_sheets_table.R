## -------------------------------
##
## Make sheets table
## 
## Purpose: 
## 
## This file creates a function to build GT HTML table
##
## Authors:  
##      Sean Bock, Abt Global
##
## -------------------------------

make_sheets_table <- function(sheets, form){
  
  #' make_sheets_table
  #' 
  #' Takes a list of Excel sheet names, created with get_sheet, and returns a nicely formatted GT html table for a given EIA form.
  #' @param sheets Character vector of Excel sheet names
  #' @param form Name of EIA form (e.g., "923")
  #' @return GT HTML table
  #' @examples
  #' make_sheets_table(eia_923_sheets, "923")
  
  
  
  if(form != "860m") {
    
    gt_table <- 
      tibble(sheets) %>% 
      mutate(File = names(sheets)) %>% 
      relocate(File) %>%
      tidyr::unnest(sheets) %>% 
      rename(!!glue::glue("EIA-{form}") := sheets) %>%
      group_by(File) %>% 
      gt::gt() %>% 
      gt::tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
      ) %>% 
      gt::tab_style(
        style = cell_text(size = 14, weight = "bold"),
        locations = cells_column_labels()
      )
    
    return(gt_table)
    
  } else{
    
    gt_table <- 
      tibble(sheets) %>% 
      # mutate(File = names(sheets)) %>% 
      # relocate(File) %>%
      # tidyr::unnest(sheets) %>% 
      rename(!!glue::glue("EIA-{form}") := sheets) %>%
      # group_by(File) %>% 
      gt::gt() %>% 
      gt::tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
      ) %>% 
      gt::tab_style(
        style = cell_text(size = 14, weight = "bold"),
        locations = cells_column_labels()
      )
    
    return(gt_table)
    
    
  }
  
}