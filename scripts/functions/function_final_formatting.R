## -------------------------------
##
## Function format region files
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


format_region <- function(region) {
  
  #' format_region
  #' 
  #' Helper function to format region aggregation files, specifically column widths
  #' Needs function header style create 
  #' 
  #' @param region Region for data to be aggregated to
  #' @return Formatted dataframe with style
  #' @examples 
  #' format_region(st) # Where st is the sheet name 
  
  require(openxlsx)
  
  # if region is not for US, then region uses this style
  if(as.character(region) != glue::glue("US{year}")) {

  ## set column widths
  setColWidths(wb, sheet = region, cols = 1,       widths = 12)
  setColWidths(wb, sheet = region, cols = 2,       widths = 14)
  setColWidths(wb, sheet = region, cols = 3,       widths = 18.43)
  setColWidths(wb, sheet = region, cols = 4:124,   widths = 14)
  setColWidths(wb, sheet = region, cols = 125,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 126:128, widths = 14)
  setColWidths(wb, sheet = region, cols = 129,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 130:142, widths = 14)
  setColWidths(wb, sheet = region, cols = 143,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 144:146, widths = 14)
  setColWidths(wb, sheet = region, cols = 147,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 149:152, widths = 14)
  setColWidths(wb, sheet = region, cols = 153,     widths = 15.11)
  setColWidths(wb, sheet = region, cols = 154:163, widths = 14)
  setColWidths(wb, sheet = region, cols = 164,     widths = 15.11)
  setColWidths(wb, sheet = region, cols = 165:169, widths = 14)

  
  ## set row heights
  setRowHeights(wb, sheet = region, row = 1, heights = 67.5)
  
  ## freeze pane
  freezePane(wb, sheet = region, firstActiveCol = 4, firstActiveRow = 3)
  
  } else {
  
  # else (for US) 
  
  ## set column widths
  setColWidths(wb, sheet = region, cols = 1:4,     widths = 14.14)
  setColWidths(wb, sheet = region, cols = 5:6,     widths = 14.43)
  setColWidths(wb, sheet = region, cols = 7:124,   widths = 14.14)
  setColWidths(wb, sheet = region, cols = 123,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 124:126, widths = 14.14)
  setColWidths(wb, sheet = region, cols = 127,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 128:140, widths = 14.14)
  setColWidths(wb, sheet = region, cols = 141,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 142:144, widths = 14.14)
  setColWidths(wb, sheet = region, cols = 145,     widths = 16.57)
  setColWidths(wb, sheet = region, cols = 146:165, widths = 14.14)
  
  ## set row heights
  setRowHeights(wb, sheet = region, row = 1, heights = 67.5)
  }
}



format_cols <- function(df, sheet, style_map, text_style_map, start_col = 0) {
  
  #' format_cols
  #' 
  #' Helper function to format column names and styles
  #' Needs function header style create 
  #' 
  #' @param df Dataframe that needs formatting
  #' @param sheet Which spreadsheet to add formatting
  #' @param style_map Specific style map needed for the file
  #' @param style_map Specific text style map needed for the file
  #' @param start_col Which column to start formatting (depending on if it is annual data or monthly data)
  #' @return Formatted dataframe with style
  #' @examples 
  #' format_cols(df, "ST", region_style_map, text_style_map, start_col = 0)
  
  # format names 
  for (colname in names(df)) {
    if (colname %in% names(style_map)) {
      col_index <- which(names(df) == colname) + start_col
      
      color_index <- style_map[[colname]]
      
      #  add header style
      style_header_name <- paste0(color_index,"_header")

      addStyle(
        wb, 
        sheet = sheet, 
        style = s[[style_header_name]], 
        cols = col_index, 
        rows = 2, 
        gridExpand = TRUE
      )
      
      # add description style
      style_desc_name <- paste0(color_index,"_desc")
      
      addStyle(
        wb, 
        sheet = sheet, 
        style = s[[style_desc_name]], 
        cols = col_index, 
        rows = 1, 
        gridExpand = TRUE
      )
      
      # add text style
      if (params$temporal_res == "monthly"){
        text_style_name <- text_style_map[[color_index]]
      } else {
        text_style_name <- text_style_map[[colname]]
      }
      
      addStyle(
        wb, 
        sheet = sheet, 
        style = s[[text_style_name]], 
        cols = col_index, 
        rows = 3:(nrow(df)+2), 
        gridExpand = TRUE
      )
      
      col_width <- if_else(grepl("ANNUAL$", colname), 16.5, 13.14)
      
      # set row heights
      setRowHeights(wb, 
                    sheet = sheet, 
                    row = 1, 
                    heights = 67.5)
      
      # set column widths
      setColWidths(wb, 
                   sheet = sheet, 
                   cols = col_index,     
                   widths = col_width)
      
    }
  }
}

# function to modify style_maps in order to match monthly 
modify_style_name <- function(name_map, old_name, new_name) {
  
  #' modify_style_name
  #' 
  #' Helper function to change style names within a named vector
  #' 
  #' @param name_map Named vector to be altered
  #' @param old_name Name to alter
  #' @param new_name Replacement name for the old name
  #' @return Altered named vector
  #' @examples 
  #' modify_style_name(unit_nonmetric_annual, "SEQUNT", names(sequnt_label)) # change SEQUNT to dynamic name of SEQUNT(year)
  
  names(name_map)[names(name_map) == old_name] <- new_name
  
  return(name_map)
}


# function for formatting a sheet
format_sheet <- function(df_month = "", df_ann, file_name, temporal_res, default_style_map, text_style_map = c("base"    = "basic",
                                                                                                              "color1"  = "integer2",
                                                                                                              "color4"  = "decimal1",
                                                                                                              "color5"  = "decimal1",
                                                                                                              "color15" = "decimal1")) {
  
  #' format_sheet
  #' 
  #' Helper function to format sheet based on file 
  #' 
  #' @param df_month Dataframe for month data (only needed for monthly temporal res)
  #' @param df_ann Dataframe for annual data
  #' @param file_name Name of the file for formatting
  #' @param temporal_res Either "annual" or "monthly" depending on params
  #' @param default_style_map Style map for colors 
  #' @param text_style_map Style map for text
  #' @return Formatted dataframe
  #' @examples 
  #' format_sheet(
  #' df_ann = unt_file,                    # annual unit file data
  #' file_name = "UNT",                    # name of file 
  #' temporal_res = params$temporal_res,   # either monthly or annual (will have different style modifications)
  #' default_style_map = unt_style_map,    
  #' text_style_map = unt_text_style_map)  
  
  # get sheet name from file_name
  if (file_name %in% c("SR", "NR")) {
    sheet <- paste0(file_name,"L",year)
  } else {
    sheet <- paste0(file_name, year)
  }
  
  # style map for all base columns
  base_style_map <- c("YEAR" = "base",
                      "PSTATABB" = "base",
                      "FIPSST" = "base",
                      "BANAME" = "base",
                      "BACODE" = "base",
                      "SUBRGN" = "base",
                      "SRNAME" = "base",
                      "NERC" = "base",
                      "NERCNAME" = "base",
                      "PSTATABB" = "base",
                      "FIPSST" = "base")
  
  base_text_style_map <- c("YEAR" = "basic",
                           "PSTATABB" = "basic",
                           "FIPSST" = "basic",
                           "BANAME" = "basic",
                           "BACODE" = "basic",
                           "SUBRGN" = "basic",
                           "SRNAME" = "basic",
                           "NERC" = "basic",
                           "NERCNAME" = "basic",
                           "PSTATABB" = "basic",
                           "FIPSST" = "basic")

  ### Monthly Formatting ###
  if (temporal_res == "monthly") {
    
    
    # uppercase month abbreviations
    month_abbr_upper <- toupper(month.abb)
    
    # format monthly columns
    for (month in month_abbr_upper){
      style_map <- default_style_map # set up style map
      
      for (i in 1:length(style_map)) { 
        old_name <- names(default_style_map)[i]
        new_name <- paste0(file_name, old_name, "_", month)
        style_map <- modify_style_name(style_map, old_name, new_name) # update names from base style map to monthly ver
      }
      
      format_cols(df_month, sheet, style_map, text_style_map) # format
    }
    
    # format annual columns
    if (!missing(df_ann)) {
      style_map_ann <- default_style_map
      for (i in 1:length(style_map)){
        old_name <- names(default_style_map)[i]
        
        # name cleaning 
        new_name <- paste0(file_name, old_name, "_", "ANNUAL")
        
        style_map_ann <- modify_style_name(style_map_ann, old_name, new_name)
      }
      
      format_cols(df_ann, sheet, style_map_ann, text_style_map, start_col = length(df_month))
    }

    # format base identifier columns
    format_cols(df_month, sheet, base_style_map, text_style_map)
    

  } else if (temporal_res == "annual") {
    
    style_map <- default_style_map # set up style map
    
    # region formatting
    if (file_name %in% c("ST","BA","SR","NR","US")) {
      for (i in 1:length(style_map)) { 
        old_name <- names(default_style_map)[i]
        new_name <- paste0(file_name, old_name)
        style_map <- modify_style_name(style_map, old_name, new_name) # update names from base style map to monthly ver
        
        text_style_map <- modify_style_name(text_style_map, old_name, new_name) # update names from base style map to monthly ver
        
      }
      
      format_cols(df_ann, sheet, style_map, text_style_map)
      # format base identifier columns
      format_cols(df_ann, sheet, base_style_map, base_text_style_map)
    
    # all other sheets formatting
    } else {
      format_cols(df_ann, sheet, default_style_map, text_style_map)
    }
  }
}

# check if shorthand names match name_matching.R and stop if not.
# input in header vars, input in variable vars
check_var_names <- function(file = "NA", header, header_check, temporal_res) {
  
  #' check_var_names
  #' 
  #' Helper function to check if all the names required are present in the dataframe
  #' 
  #' @param file Name of file
  #' @param header Vector of column names of the current dataframe
  #' @param header_check Vector of column names that is correct
  #' @param temporal_res Either "annual" or "monthly" based on file output
  #' @return Message that either will force stop to check previous outputs or continue with formatting
  #' @examples 
  #' check_var_names("generator", colnames(gen_file)[-1], generator_nonmetric_annual[-1], "annual")
  
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
  
  #' rename_variables
  #' 
  #' Helper function to rename all variable names to column names
  #' 
  #' @param df Dataframe being renamed
  #' @param name_map Named vector of desired name to change to 
  #' @param strict Toggle as TRUE if all variables need to be matched
  #' @param rename Toggle as TRUE if dataframe needs to be renamed
  #' @return Renamed dataframe or vector of new names
  #' @examples 
  #' rename_variables(gen_file, generator_nonmetric_annual)
  
  
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

