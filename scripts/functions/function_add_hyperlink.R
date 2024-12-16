

add_hyperlink <- function(sheet_name, row_link, col_link, loc) { 
  
  writeFormula(
    wb, sheet = "Contents", 
    x = makeHyperlinkString(sheet = sheet_name, row = row_link, col = col_link, text = sheet_name), 
    xy = loc)
  
}
