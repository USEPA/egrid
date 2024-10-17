## -------------------------------
##
## Grid gross loss create 
## 
## Purpose: 
## 
## This file creates the grid gross loss calculation for eGRID. 
## This includes all operating units for the specified eGRID data year
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------

# Load libraries ------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)

## Read in EIA files ------

eia_860 <- read_rds("data/clean_data/eia/eia_860_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia/eia_923_clean.RDS")

# define params for eGRID data year 
# this is only necessary when running the script outside of egrid_master.qmd

params <- list()
params$eGRID_year <- "2021"


## viewing data tables (remove later)
eia_operable <- eia_860$operable
eia_860_combined <- eia_860$combined
eia_923_generation_and_fuel <- eia_923$generation_and_fuel_data

## Function to download EIA files from website (adapted from function: "download_eia_files" )-------
## These urls are different depending whether or not they're the most recent data. If not most recent year, "archive" is in the url

download_eia_ggl <- function(year) {
  
  #' download_eia_ggl
  #' 
  #' Function to download data from EIA website (https://www.eia.gov/electricity/) for all states. Each states' data is stored in a Excel file on the EIA website. This function extracts all of the tables and combines it into one and creates a summary table.
  
  #' @year Year desired for formatted table
  #' @return Unzipped Excel file
  #' @examples
  #' download_eia_files("2022") # Downloads all data and summarizes all states for year 2022
  
  state <- gsub(" ", "", tolower(datasets::state.name))
  state_abbr <- tolower(datasets::state.abb)

  
  # create a eia_ggl folder if one does not already exists
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
  
  
  # Check if there are other files in the folder
  existing_files <- list.files(new_folder)
  
  if (length(existing_files) > 1) {
    print(glue::glue("Files already exist in folder:{new_folder}. Stopping"))
    return(TRUE)
  }
  
  existing_files2 <- list.files(new_folder2)
  
  if (length(existing_files2) > 1) {
    print(glue:glue("Files already exist in folder:{new_folder2}. Stopping"))
    return(TRUE)
  }
  
  # initialize list of tables and workbook
  ggl_data <- list()
  ggl_wb <- createWorkbook()
  ggl_summary_data <- list()
  
  summary_sheet_name <- glue::glue("FormattedTable_{year}")
  addWorksheet(ggl_wb, summary_sheet_name)
  
  # loop for all of the states and files
  for (i in 1:length(state_abbr)){
    
    # initialize variables for i
    name <- state[i]
    abbr <- state_abbr[i]
    
    # download file for corresponding state i from EIA
    url <- glue::glue("https://www.eia.gov/electricity/state/{name}/xls/{abbr}.xlsx")
    dest_file <- glue::glue("{new_folder}/{abbr}.xlsx")
    
    download_file(url,dest_file,new_folder) 
    
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
    direct_use <- select_table %>%
      filter(Category == "Direct use") %>%
      pull(year) %>%
      as.numeric() 
    
    estimated_losses <- select_table %>%
      filter(Category == "Estimated losses") %>%
      pull(year) %>%
      as.numeric() 
    
    total_disp <- select_table %>%
      filter(Category == "Total disposition") %>%
      pull(year) %>%
      as.numeric() 
    
    net_interstate_i <- select_table %>%
      filter(Category == "Net interstate imports") %>%
      pull(year) %>%
      as.numeric() 
    
    net_interstate_e <- select_table %>%
      filter(Category == "Net interstate exports") %>%
      pull(year) %>%
      as.numeric() 
    
    total_disp_sub_e <- (total_disp - net_interstate_e)
    
    # compile all variables into a single row
    ggl_summary_data[[i]] <- c(toupper(abbr), 
                    tools::toTitleCase(name), 
                    direct_use, estimated_losses, total_disp, net_interstate_i, net_interstate_e, total_disp_sub_e)
    
    
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
  
  formatted_table <- formatted_table %>%
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
  
  
  ggl_file <- glue::glue("{new_folder2}/R_ggl.xlsx")
  saveWorkbook(ggl_wb, ggl_file, overwrite = TRUE)
  
  # if (!download_and_unzip(url, dest_file, new_folder) && stringr::str_detect(url, "/archive")) {
  #   url <- stringr::str_replace(url, "/archive", "")
  #   if (!download_and_unzip(url, dest_file, new_folder)) {
  #     print(paste("Failed to download or unzip:", dest_file))
  #  }
  #}
  
  
}

download_eia_ggl("2022")

## area to develop new parts of the function :-3 ------

# new_header <- select_table1[3,]
# colnames(select_table1) <- new_header # changes header to be by year
# colnames(select_table1) <- gsub("Year", "", colnames(select_table1)) # removes extra spaces in header for easy extraction
# colnames(select_table1) <- gsub("\r\n", "", colnames(select_table1))
# 
# ## extracting desired rows for the indicated year
# direct_use1 <- select_table1 %>%
#   filter(Category == "Direct use") %>%
#   pull("2022") %>%
#   as.numeric() 
# 
# estimated_losses1 <- select_table1 %>%
#   filter(Category == "Estimated losses") %>%
#   pull("2022") %>%
#   as.numeric() 
# 
# total_disp1 <- select_table1 %>%
#   filter(Category == "Total disposition") %>%
#   pull("2022") %>%
#   as.numeric() 
# 
# net_interstate_i1 <- select_table1 %>%
#   filter(Category == "Net interstate imports") %>%
#   pull("2022") %>%
#   as.numeric() 
# 
# net_interstate_e1 <- select_table1 %>%
#   filter(Category == "Net interstate exports") %>%
#   pull("2022") %>%
#   as.numeric() 
# 
# total_disp_sub_e1 <- (total_disp1 - net_interstate_e1)
# 
# 
# # compile all variables into a single row
# test <- list()
# test[[1]] <- c("AL", "Alabama", direct_use1, estimated_losses1, total_disp1, net_interstate_i1, net_interstate_e1, total_disp_sub_e1)  
# test[[2]] <- c("AL", "Alabama", direct_use1, estimated_losses1, total_disp1, net_interstate_i1, net_interstate_e1, total_disp_sub_e1)    
# 
# formattest <- do.call(rbind, test)
# formattest <- as.data.frame(formattest)
# colnames(formattest) <- c("State Postal Code", 
#                                "State", 
#                                "Direct Use", 
#                                "Estimated Losses", 
#                                "Total Disposition", 
#                                "Net Interstate Imports",
#                                "Net Interstate Exports",
#                                "Total Disposition-Exports")
# 
# formattest <- formattest %>%
#   mutate(across(c("Direct Use", 
#                   "Estimated Losses", 
#                   "Total Disposition", 
#                   "Net Interstate Imports",
#                   "Net Interstate Exports",
#                   "Total Disposition-Exports"), ~as.numeric(.)))
# 
# test2 <- c("US", "United States",
#            sum(formattest$`Direct Use`),
#            sum(formattest$`Estimated Losses`),
#            sum(formattest$`Total Disposition`),
#            sum(formattest$`Net Interstate Imports`),
#            sum(formattest$`Net Interstate Exports`),
#            sum(formattest$`Total Disposition-Exports`))
# 
# formattest <- rbind(formattest, test2)
# 
# str(formattest$`Direct Use`)



## load in datasets ----
R_ggl <- read_xlsx("data/clean_data/eia_ggl/R_ggl.xlsx")
state_interconnect <- read_csv("data/static_tables/state_and_interconnection.csv")
nerc_interconnect <- read_xlsx("data/static_tables/nerc_region_and_interconnect.xlsx")


## 01: Create GGL table at state level for non duplicate states -----

# vector of duplicate states
dup_state <- c('Montana','Nebrasksa','New Mexico','South Dakota','Texas')

# cleaning R_ggl file and creating initial GGL calculation
R_ggl <- R_ggl %>%
  rename(state = State,
         state_abbr = "State Postal Code",
         direct_use = "Direct Use",
         est_losses = "Estimated Losses",
         tot_disp = "Total Disposition",
         net_interstate_im = "Net Interstate Imports",
         net_interstate_ex = "Net Interstate Exports",
         tot_disp_sub_ex = "Total Disposition-Exports") %>%
  mutate(across(c(direct_use, 
                  est_losses,
                  tot_disp, 
                  net_interstate_im, 
                  net_interstate_ex, 
                  tot_disp_sub_ex), ~as.numeric(.))) %>%
  select(state, direct_use, est_losses, tot_disp, net_interstate_ex, tot_disp_sub_ex) %>%
  mutate(ggl = est_losses / (tot_disp_sub_ex - direct_use))

# creating initial GGL table with non-duplicate states
ggl_state <- state_interconnect %>% inner_join(R_ggl, by = c(State = "state")) %>%
  filter(!State %in% dup_state)

### 02: States with multiple interconnects -----

# create vector, count number of interconnects
multi_interconnect_states <- state_interconnect %>%
  rename(state_abbr = `State Postal Code`,
         state = State,
         interconnect = Interconnect) %>%
  group_by(state_abbr, state) %>%
  mutate(interconnect_count = n_distinct(interconnect)) %>%
  filter(interconnect_count > 1) %>%
  select(state_abbr, state, interconnect_count) %>% 
  unique()

### 03: Sum of generation for multiple interconnect states ------

# create sum of total generation by state
sum_tot_generation <- eia_923_generation_and_fuel %>%
  select(plant_state, nerc_region, net_generation_megawatthours) %>%
  group_by(plant_state) %>%
  summarise(sum_net_generation = sum(net_generation_megawatthours)) 

# join with states with multiple interconnection
multi_interconnect_states <- multi_interconnect_states %>% 
  inner_join(sum_tot_generation, by = c("state_abbr" = "plant_state"))

### 04: Sum of gen for multiple interconnect states by NERC ---------

# find the sum of generation for only the states with multiple interconnects 
multi_interconnect_sum <- eia_923_generation_and_fuel %>%
  select(-nerc_region) %>%    # prevent duplicate nerc_region columns
  inner_join(eia_860_plant, by = c("plant_id" , "plant_state")) %>%
  inner_join(nerc_interconnect, by = c("nerc_region", "plant_state" = "state")) %>%
  inner_join(multi_interconnect_states, by = c("plant_state" = "state_abbr", "interconnect")) %>%
  select(plant_state, net_generation_megawatthours, nerc_region, interconnect) %>%
  group_by(plant_state, interconnect, nerc_region) %>%
  summarise(sum_tot_generation = sum(net_generation_megawatthours)) 


### 05: Grouped generation by interconnect ------- 

# sum of generation at each interconnect level within the state
grouped_gen_by_interconnect <- multi_interconnect_states_sum %>%
  select(plant_state, sum_tot_generation, interconnect) %>%
  group_by(plant_state, interconnect) %>%
  summarise(sum_tot_gen_interconnect = sum(sum_tot_generation))

### 06: Ratio of generation for multiple interconnect states ------

# create a ratio using the sum of the generation at the interconnect level divided by the total generation in the state
ratio_gen_multi_interconnect <- grouped_gen_by_interconnect %>%
  inner_join(multi_interconnect_states, by = c("plant_state" = "state_abbr")) %>%
  select_all %>%
  mutate(sum_tot_gen_interconnect = as.numeric(sum_tot_gen_interconnect),
         sum_net_generation = as.numeric(sum_net_generation),
         ratio = sum_tot_gen_interconnect / sum_net_generation)

### 07: Append multiple interconnect states to GGL -----

# create new values for energy distribution using ratio created in step 6
insert_multi_interconnect <- ratio_gen_multi_interconnect %>% 
  inner_join(R_ggl, by =  c("plant_state" = "state_abbr", "state")) %>%
  mutate(direct_use_dist = direct_use * ratio,
         est_losses_dist = est_losses * ratio,
         tot_disp_raw = tot_disp * ratio,
         net_interstate_ex_raw = net_interstate_ex * ratio, 
         tot_disp_dist = tot_disp_sub_ex * ratio,
         ggl = est_losses_dist / (tot_disp_dist - direct_use_dist))

