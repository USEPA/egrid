## -------------------------------
##
## Download EIA grid gross loss files
## 
## Purpose: 
## 
## This file creates a function to download EIA grid gross loss files from website 
## (adapted from function: "download_eia_files")
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------


download_eia_ggl <- function(year) {
  
  #' download_eia_ggl
  #' 
  #' Function to download data from EIA website (https://www.eia.gov/electricity/) for all states. 
  #' Each states' data is stored in a Excel file on the EIA website. This function extracts all of the tables and combines it into one and creates a summary table.
  
  #' @year Year desired for formatted table
  #' @return Unzipped Excel file
  #' @examples
  #' download_eia_files(2022) # Downloads all data and summarizes all states for year 2022
  
  state_lower <- gsub(" ", "", tolower(datasets::state.name))
  state_title <- datasets::state.name
  state_abbr <- tolower(datasets::state.abb)
  
  state_lower <- c(state_lower, "districtofcolumbia")
  state_title <- c(state_title, "District of Columbia")
  state_abbr <- c(state_abbr, "dc")
  
  
  # create a eia_ggl folder for raw_data & clean_data if one does not already exists
  new_folder <- "data/raw_data/eia_ggl"
  new_folder2 <- "data/clean_data/eia_ggl"
  
  if (!dir.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
  }
  
  
  if (!dir.exists(new_folder2)) {
    dir.create(new_folder2, recursive = TRUE)
  }
  
  # function that extracts files from EIA website
  download_file <- function(url, dest_file, new_folder) {
    
    download.file(url, dest_file, mode = "wb")
    end <- paste("Downloading to:", new_folder)
    
    return(end)
  }

  # check if there are other files in each folder
  existing_files <- list.files(new_folder)
  
  # file name for file checking
  ggl_file <- glue::glue("{new_folder2}/ggl_{year}.xlsx")
  
  ## if there are no files in the raw_data/eia_ggl folder, download data from EIA website
  # files contain data for all previous data years
  # therefore, only needs one round of downloading
  # when there is a new data update, please re-download all files (ex. new release of 2024 data after running 2023 data)
  
  if (length(existing_files) < 1) {
    
    for (i in 1:length(state_abbr)){
      
      # initialize variables for i
      name <- state_lower[i]
      label <- state_title[i]
      abbr <- state_abbr[i]
      
      # download file for corresponding state i from EIA
      url <- glue::glue("https://www.eia.gov/electricity/state/{name}/xls/SEP%20Tables%20for%20{toupper(abbr)}.xlsx")
      dest_file <- glue::glue("{new_folder}/{abbr}.xlsx")
      
      download_file(url, dest_file, new_folder) 
      
    }
    
  } else {
    
    print(glue::glue("Files already exist in folder:{new_folder}. Skipping file download."))
    
  }
  
  ## if the ggl file does not exist, aggregate the downloaded data to create a ggl file
  
  if (!file.exists(ggl_file)) {
  
  
    # initialize list of tables and workbook
    ggl_data <- list()
    ggl_wb <- createWorkbook()
    ggl_summary_data <- list()
    
    summary_sheet_name <- glue::glue("FormattedTable_{year}")
    addWorksheet(ggl_wb, summary_sheet_name)
    
    # loop for all of the states and files
    for (i in 1:length(state_abbr)){
      
      # initialize variables for i
      name <- state_lower[i]
      label <- state_title[i]
      abbr <- state_abbr[i]
      
      # extract file for corresponding state i from folder
      dest_file <- glue::glue("{new_folder}/{abbr}.xlsx")
      
      # select table needed for GGL calculation (Table 10: Supply and disposition of energy)
      select_table <- read_excel(dest_file, sheet = 11)
      ggl_data[[i]] <- select_table
      
      # add table to workbook as a sheet
      addWorksheet(ggl_wb, sheetName = toupper(abbr))
      writeData(ggl_wb, sheet = toupper(abbr), ggl_data[[i]])
      
      # clean up select table for easier data extraction
      new_header <- select_table[3,]
      colnames(select_table) <- new_header # changes header to be by year
      colnames(select_table) <- gsub("Year", "", colnames(select_table)) # removes extra spaces in header for easy extraction
      colnames(select_table) <- gsub("\r\n", "", colnames(select_table))
      
      ## extracting desired rows for the indicated year
      direct_use <- 
        select_table %>%
        filter(Category == "Direct use") %>%
        pull(year) %>%
        as.numeric() 
      
      estimated_losses <- 
        select_table %>%
        filter(Category == "Estimated losses") %>%
        pull(year) %>%
        as.numeric() 
      
      total_disp <- 
        select_table %>%
        filter(Category == "Total disposition") %>%
        pull(year) %>%
        as.numeric() 
      
      net_interstate_i <- 
        select_table %>%
        filter(Category == "Net interstate imports") %>%
        pull(year) %>%
        as.numeric() 
      
      net_interstate_e <- 
        select_table %>%
        filter(Category == "Net interstate exports") %>%
        pull(year) %>%
        as.numeric() 
      
      total_disp_sub_e <- (total_disp - net_interstate_e)
      
      # compile all variables into a single row
      ggl_summary_data[[i]] <- c(toupper(abbr), 
                                 label, 
                                 direct_use, 
                                 estimated_losses, 
                                 total_disp, 
                                 net_interstate_i, 
                                 net_interstate_e, 
                                 total_disp_sub_e)
      
    }
    
    
    # adding formatted table
    
    formatted_table <- do.call(rbind, ggl_summary_data)
    formatted_table <- as.data.frame(formatted_table)
    colnames(formatted_table) <- c("State Postal Code", 
                                   "State", 
                                   "Direct Use", 
                                   "Estimated Losses", 
                                   "Total Disposition", 
                                   "Net Interstate Imports",
                                   "Net Interstate Exports",
                                   "Total Disposition-Exports")
    
    formatted_table <- 
      formatted_table %>%
      mutate(across(c("Direct Use", 
                      "Estimated Losses", 
                      "Total Disposition", 
                      "Net Interstate Imports",
                      "Net Interstate Exports",
                      "Total Disposition-Exports"), ~as.numeric(.)))
    
    # create total_row for US using sums of all states
    total_row <- c("US", "United States",
                   sum(formatted_table$`Direct Use`),
                   sum(formatted_table$`Estimated Losses`),
                   sum(formatted_table$`Total Disposition`),
                   sum(formatted_table$`Net Interstate Imports`),
                   sum(formatted_table$`Net Interstate Exports`),
                   sum(formatted_table$`Total Disposition-Exports`))
    
    # add row to FormattedTable
    formatted_table <- rbind(formatted_table, total_row)
    
    # write FormattedTable to Excel file
    writeData(ggl_wb, sheet = summary_sheet_name, formatted_table)
    
    # save file as ggl_r_(year).xlsx
    saveWorkbook(ggl_wb, ggl_file, overwrite = TRUE)
  
  } else {
    
    print(glue::glue("File already exists in folder:{new_folder2}. Skipping file aggregation."))

  }
  
}
