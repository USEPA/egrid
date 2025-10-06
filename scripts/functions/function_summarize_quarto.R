## -------------------------------
##
## Group of functions that summarize data in Quarto document
## 
## Purpose: 
## 
## Summarize data in Quarto documents
##
## Authors:  
##      Sean Bock, Abt Global
##
## -------------------------------


get_sheets <- function(form){
  
  #' get_sheets
  #' 
  #' Return names of sheets in EIA Excel files
  #' @param form Name of EIA form (e.g., "923")
  #' @return A character vector with the names of all Excel sheets within a given EIA form.
  #' @examples
  #' get_sheets("923")
  
  
  path <- glue::glue("data/1_production_model/raw_data/{form}/{params$eGRID_year}")
  
  if(form != "860m") {
    
    files <- list.files(path)
    
    # Filter only Excel files
    excel_files <- stringr::str_subset(files, ".xls|.xlsx")
    
    # read each sheet and store them in a list
    sheet_data <- purrr::map(excel_files, ~ readxl::excel_sheets(glue::glue("{path}/{.x}")))
    
    # set names for each element based on file name
    sheet_data_named <- setNames(sheet_data, excel_files)
    
    return(sheet_data_named)
    
  } else{
    
    sheet_data <- readxl::excel_sheets(glue::glue("data/1_production_model/raw_data/860/{params$eGRID_year}/eia_pr_860m.xlsx"))
    
    return(sheet_data)
    
  }
  
}

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
      rename(!!glue::glue("EIA-{form}") := sheets) %>%
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


create_summary_gt_table <- function(data) {
  
  #' create_summary_gt_table
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
