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

### Read in EIA files ------

# define params for eGRID data year 
# this is only necessary when running the script outside of egrid_master.qmd

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

data_year <- params$eGRID_year

# read files based on eGRID year
eia_860 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS"))
eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))

### Extracting EIA tables and data -----

## assign variables for necessary EIA tables
eia_860_combined <- eia_860$combined
eia_860_plant <- eia_860$plant
eia_923_generation_and_fuel <- eia_923$generation_and_fuel_data


### Download EIA files necessary for GGL calculation (unless already downloaded)
# load necessary function
source("scripts/functions/function_download_eia_ggl.R") 

# downloads and/or aggregates data needed for GGL calculations
download_eia_ggl(params$eGRID_year)


### Load in datasets ----

# ggl_r = Table 10: State supply and disposition data (from EIA website) compiled into a summary sheet
ggl_r <- read_xlsx(glue::glue("data/clean_data/eia_ggl/ggl_{params$eGRID_year}.xlsx")) 

# state_and_interconnection = states and their interconnections
state_interconnect <- read_csv("data/static_tables/state_and_interconnection.csv")

# nerc_region_and_interconnect = states, interconnections, and NERC region
nerc_interconnect <- read_xlsx("data/static_tables/nerc_region_and_interconnect.xlsx")

# standardizing column names for easier manipulations
state_interconnect <- 
  state_interconnect %>%
  rename(state = State,
         state_abbr = `State Postal Code`,
         interconnect = Interconnect)

nerc_interconnect <- 
  nerc_interconnect %>%
  rename(state = State,
         nerc_region = `NERC Region`,
         interconnect = Interconnect)


# Create GGL table at state level for non duplicate states -----

# vector of duplicate states 
# check if needed to update every year
dup_state <- c('Montana','Nebraska','New Mexico','South Dakota','Texas')

# cleaning ggl_r file and creating initial GGL calculation
ggl_r <- 
  ggl_r %>%
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
  mutate(ggl = est_losses / (tot_disp_sub_ex - direct_use))

# creating initial GGL table with non-duplicate states
ggl_state <- 
  state_interconnect %>% 
  inner_join(ggl_r, by = c("state", "state_abbr")) %>%
  filter(!state %in% dup_state) # only states that fall into one interconnection 

# Identify states with multiple interconnects -----

# create vector, count number of interconnects
multi_interconnect_states <- 
  state_interconnect %>%
  group_by(state_abbr, state) %>%
  mutate(interconnect_count = n_distinct(interconnect)) %>%
  filter(interconnect_count > 1) 


# Sum generation for multiple states with multiple interconnects ------

# create sum of total generation by state
sum_tot_generation <- 
  eia_923_generation_and_fuel %>%
  select(plant_state, nerc_region, net_generation_megawatthours) %>%
  group_by(plant_state) %>%
  summarise(sum_net_generation = sum(net_generation_megawatthours)) 

# join with states with multiple interconnection
multi_interconnect_states <- multi_interconnect_states %>% 
  inner_join(sum_tot_generation, by = c("state_abbr" = "plant_state"))


# Sum generation for states with multiple interconnect by NERC region ---------

# find the sum of generation for only the states with multiple interconnects 
multi_interconnect_sum <- 
  eia_923_generation_and_fuel %>%
  inner_join(eia_860_plant, by = c("plant_id" , "plant_state")) %>%
  mutate(nerc_region = if_else(!is.na(nerc_region.y), nerc_region.y, nerc_region.x)) %>%
  inner_join(nerc_interconnect, by = c("nerc_region", "plant_state" = "state")) %>%
  inner_join(multi_interconnect_states, by = c("plant_state" = "state_abbr", "interconnect")) %>%
  select(plant_state, net_generation_megawatthours, nerc_region, interconnect) %>%
  group_by(plant_state, interconnect, nerc_region) %>%
  summarise(sum_net_generation = sum(net_generation_megawatthours)) 

# Group generation by interconnect ------- 

# sum of generation at each interconnect level within the state
grouped_gen_by_interconnect <- 
  multi_interconnect_sum %>%
  select(plant_state, sum_net_generation, interconnect) %>%
  group_by(plant_state, interconnect) %>%
  summarise(sum_net_gen_interconnect = sum(sum_net_generation))

# Create ratio of generation for states with multiple interconnects ------

# alter dataframe - prevents duplication in joining
multi_interconnect_states2 <- 
  multi_interconnect_states %>%
  select(-interconnect) %>%
  unique()

# create a ratio using the sum of the generation at the interconnect level divided by the total generation in the state
ratio_gen_multi_interconnect <- 
  grouped_gen_by_interconnect %>%
  inner_join(multi_interconnect_states2, by = c("plant_state" = "state_abbr")) %>%
  select_all %>%
  mutate(sum_tot_gen_interconnect = as.numeric(sum_net_gen_interconnect),
         sum_net_generation = as.numeric(sum_net_generation),
         ratio = sum_net_gen_interconnect / sum_net_generation)

# Append multiple interconnect states to GGL table -----

# create new values for energy distribution using ratio created in step 6
insert_multi_interconnect <- 
  ratio_gen_multi_interconnect %>% 
  inner_join(ggl_r, by =  c("plant_state" = "state_abbr", "state")) %>%
  mutate(direct_use_dist = direct_use * ratio,
         est_losses_dist = est_losses * ratio,
         tot_disp_raw = tot_disp * ratio,
         net_interstate_im_raw = net_interstate_im * ratio,
         net_interstate_ex_raw = net_interstate_ex * ratio, 
         tot_disp_dist = tot_disp_sub_ex * ratio,
         ggl = est_losses_dist / (tot_disp_dist - direct_use_dist)) %>%
  select(plant_state, 
         state, 
         interconnect, 
         direct_use_dist, 
         est_losses_dist, 
         tot_disp_raw, 
         net_interstate_im_raw,
         net_interstate_ex_raw,
         tot_disp_dist,
         ggl) %>%
  rename(state_abbr = plant_state,
         direct_use = direct_use_dist,
         est_losses = est_losses_dist,
         tot_disp = tot_disp_raw,
         net_interstate_im = net_interstate_im_raw,
         net_interstate_ex = net_interstate_ex_raw,
         tot_disp_sub_ex = tot_disp_dist)

# add rows into initially created ggl_state table  
ggl_state <- rbind(ggl_state, insert_multi_interconnect)

# Create GGL table at interconnect level -----------
ggl_interconnect <- 
  ggl_state %>%
  group_by(interconnect) %>%
  summarise(est_losses_sum = sum(est_losses),
            tot_disp_sub_ex_sum = sum(tot_disp_sub_ex),
            direct_use_sum = sum(direct_use))

# Update GGL interconnect table with GGL values ----------------
ggl_interconnect_2 <- 
  ggl_interconnect %>%
  mutate(ggl = est_losses_sum / (tot_disp_sub_ex_sum - direct_use_sum),
         ggl = round(ggl, 3))

# Sum generation and disposition to US level ----------------
ggl_us <- 
  ggl_interconnect_2 %>%
  summarise(est_losses_sum = sum(est_losses_sum),
            tot_disp_sub_ex_sum = sum (tot_disp_sub_ex_sum),
            direct_use_sum = sum(direct_use_sum)) %>%
  mutate(interconnect = "U.S.",
         ggl = est_losses_sum /(tot_disp_sub_ex_sum - direct_use_sum),
         ggl = round(ggl, 3))

# Append US GGL data -------------
ggl_interconnect_3 <- rbind(ggl_interconnect_2, ggl_us)

# Add year to dataframe ------------
ggl_interconnect_4 <- cbind(data_year, ggl_interconnect_3)

# Export GGL file -------------- 

# check if folders exist 
if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
} else {
  dir.create("data/outputs")
}

if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
  print("Folder output already exists.")
} else {
  dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
}

# write RDS file
print(glue::glue("Saving grid gross loss file to folder data/outputs/{params$eGRID_year}"))

write_rds(ggl_interconnect_4, glue::glue("data/outputs/{params$eGRID_year}/grid_gross_loss.RDS"))

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/grid_gross_loss.RDS"))){
  print(glue::glue("File grid_gross_loss.RDS successfully written to folder data/outputs/{params$eGRID_year}"))
} else {
  print("File grid_gross_loss.RDS failed to write to folder.")
} 

