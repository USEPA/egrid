
style_sheets_table <- function(sheets_to_color, sheets_table) {
  
  
  sheet <- sheets_table %>%
           tibble(sheets_table) %>%
           tidyr::unnest(sheets_table)
  
  
  if (sheet %in% sheets_to_color) {
    sheets_table <- sheets_table %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgoldenrod")
        ),
    locations = cells_body(columns = everything())
      )
  }
  
  
  return(table)
}