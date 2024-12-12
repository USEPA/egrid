## -------------------------------
##
## Create summary table
## 
## Purpose: 
## 
## This file creates a function to create summary GT summary table. 
##
## Authors:  
##      Sean Bock, Abt Global
##
## -------------------------------


create_summary_table <- function(data) {
  
    #' create_summary_table
    #' 
    #' Takes a dataframe and returns a summary GT summary table, similar to what is produced with the glimpse function.
    #' @param data A dataframe
    #' @return GT table object
    #' @example
    #' create_summary_table(eia_923$generation_and_fuel_data)
  
  
    if(nrow(data) == 0 ){
      print("This dataframe is empty. No summary table to create.")
    } else {
      # Function to limit character values and add "..." if needed
      limit_and_ellipsis <- function(vec, max_length) {
        # Apply the function to each element in the vector
        result <- sapply(vec, function(x) {
          if (nchar(x) <= max_length) {
            # If the length is within the limit, keep the original value
            return(x)
          } else {
            # Otherwise, truncate and add "..."
            return(paste0(substr(x, 1, max_length - 3), "..."))
          }
        })
        return(result)
      }
      
      first_five <- 
        data %>%
        dplyr::slice(1:5) %>%
        dplyr::mutate(across(everything(), ~paste(.x, collapse = ", "))) %>%
        slice(1) %>%
        tidyr::pivot_longer(cols = everything(), values_to = "Values", names_to = "Columns") %>% 
        mutate(Values = limit_and_ellipsis(Values, max_length = 40))
      
      col_types <- sapply(data, class)
      
      n_missing <- 
        data %>% 
        summarize(across(everything(), ~ sum(is.na(.x))))
        
      
      nrows <- nrow(data)
      
      summary <- 
        first_five %>% 
        mutate(Class = col_types,
               `N Missing` = as.numeric(n_missing[1,])) %>% 
        relocate(Class, .after = Columns)
      
     gt_table <- 
      summary %>% 
        gt::gt() %>% 
        gt::tab_caption(caption = glue::glue("N = {nrows}"))
     
     return(gt_table)
    }
    
}


