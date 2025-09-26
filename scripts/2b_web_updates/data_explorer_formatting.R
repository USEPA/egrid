## -------------------------------
##
## Data explorer formatting 
## 
## Purpose: 
## 
## This file creates the input CSV files for the eGRID data explorer.
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries --------------------------

library(dplyr)
library(readxl)
library(readr)
library(stringr)

# Load functions and check parameters -----------------------

source("scripts/functions/function_check_params.R")
source("scripts/functions/function_save_output_data.R")
source("scripts/functions/function_check_valid_url.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")}

# Check for necessary folders ---------------------

if(!dir.exists("data/1_production_model/static_tables/historical_egrid")) { 
  dir.create("data/1_production_model/static_tables/historical_egrid")}

# Download historical eGRID years ------------------------

file_paths <- 
  c("2018" = "data/1_production_model/static_tables/historical_egrid/egrid2018_data.xlsx", 
    "2019" = "data/1_production_model/static_tables/historical_egrid/egrid2019_data.xlsx",
    "2020" = "data/1_production_model/static_tables/historical_egrid/egrid2020_data.xlsx",
    "2021" = "data/1_production_model/static_tables/historical_egrid/egrid2021_data.xlsx",
    "2022" = "data/1_production_model/static_tables/historical_egrid/egrid2022_data.xlsx") # add previous data year to this list every year 

urls <- ### Note: check for updates or changes each data year ###
  c("2018" = "https://www.epa.gov/sites/default/files/2020-03/egrid2018_data_v2.xlsx", 
    "2019" = "https://www.epa.gov/sites/default/files/2021-02/egrid2019_data.xlsx", 
    "2020" = "https://www.epa.gov/system/files/documents/2022-09/eGRID2020_Data_v2.xlsx", 
    "2021" = "https://www.epa.gov/system/files/documents/2023-01/eGRID2021_data.xlsx",
    "2022" = "https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx") # add previous data year to this list every year 

for(i in 1:length(urls)) { # download files online if they have not already been downloaded. 
  if(!file.exists(file_paths[i])) { 
    if(check_valid_url(urls[i])) {
      download.file(url = urls[i], 
                    destfile = file_paths[i], 
                    mode = "wb")
    } else {print(glue::glue("URL {urls[i]} is not valid. Check and update URL."))}
    } else {
    print(glue::glue("Stopping. File {file_paths[i]} already downloaded."))
  }}

# Load historical eGRID years --------------------------

years <- 2018:as.numeric(params$eGRID_year) # years to add to data explorer

for(year in years) { ### Note: check for updates or changes each data year ### Add previous data year here
  if(year %in% names(urls)) { 
    plant <- read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                        sheet = glue::glue("PLNT{year %% 1000}"),
                        skip = 1)
    
    state <- read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                        sheet = glue::glue("ST{year %% 1000}"),
                        skip = 1)
    
    ba <- read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                     sheet = glue::glue("BA{year %% 1000}"),
                     skip = 1)
    
    subregion <- read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                            sheet = glue::glue("SRL{year %% 1000}"),
                            skip = 1)
    
    nerc <- read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                       sheet = glue::glue("NRL{year %% 1000}"),
                       skip = 1)
    
    us <- read_excel(glue::glue("data/1_production_model/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                     sheet = glue::glue("US{year %% 1000}"),
                     skip = 1)
  } else { # if URL does not exist, especially if the current eGRID year has not yet been published, load data from output folder
    plant <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                        sheet = glue::glue("PLNT{as.numeric(params$eGRID_year) %% 1000}"),
                        skip = 1)
    
    state <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                        sheet = glue::glue("ST{as.numeric(params$eGRID_year) %% 1000}"),
                        skip = 1)
    
    ba <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                     sheet = glue::glue("BA{as.numeric(params$eGRID_year) %% 1000}"),
                     skip = 1)
    
    subregion <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                            sheet = glue::glue("SRL{as.numeric(params$eGRID_year) %% 1000}"),
                            skip = 1)
    
    nerc <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                       sheet = glue::glue("NRL{as.numeric(params$eGRID_year) %% 1000}"),
                       skip = 1)
    
    us <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                     sheet = glue::glue("US{as.numeric(params$eGRID_year) %% 1000}"),
                     skip = 1)
  }
  
  # assign names for each eGRID year downloaded 
  name_plant <- glue::glue("egrid_{as.character(year)}_plant")
  name_state <- glue::glue("egrid_{as.character(year)}_state")
  name_ba <- glue::glue("egrid_{as.character(year)}_ba")
  name_subregion <- glue::glue("egrid_{as.character(year)}_subregion")
  name_nerc <- glue::glue("egrid_{as.character(year)}_nerc")
  name_us <- glue::glue("egrid_{as.character(year)}_us")
  
  assign(name_plant, plant)
  assign(name_state, state)
  assign(name_ba, ba)
  assign(name_subregion, subregion)
  assign(name_nerc, nerc)
  assign(name_us, us)
  
} 

# Combine all eGRID years --------------------

### Plant file ---------------------------
resource_mix_cols <-
  c("CLPR" = "COAL", # match resource mixes to primary fuel categories for secondary fuel calculation
    "OLPR" = "OIL",	
    "GSPR" = "GAS",	
    "NCPR" = "NUCLEAR",	
    "HYPR" = "HYDRO",	
    "BMPR" = "BIOMASS",
    "WIPR" = "WIND",	
    "SOPR" = "SOLAR",
    "GTPR" = "GEOTHERMAL",	
    "OFPR" = "OSFL",
    "OPPR" = "OTHF",
    "TNPR" = NA_character_, # these are renewable and combustion resource mixes, no match to primary fuel category
    "TRPR" = NA_character_,	
    "TOPR" = NA_character_, 
    "THPR" = NA_character_,	
    "CYPR" = NA_character_,	
    "CNPR" = NA_character_,
    "COPR" = NA_character_, 
    
    # nonbaseload resource mix 
    "NBCLPR" = NA_character_, 
    "NBOLPR" = NA_character_, 
    "NBGSPR" = NA_character_, 
    "NBNCPR" = NA_character_, 
    "NBHYPR" = NA_character_, 
    "NBBMPR" = NA_character_, 
    "NBWIPR" = NA_character_, 
    "NBSOPR" = NA_character_, 
    "NBGTPR" = NA_character_, 
    "NBOFPR" = NA_character_, 
    "NBOPPR" = NA_character_)

fuel_type_map <-
  c("AB"  = "Agricultural byproduct",
    "BFG" =	"Blast furnace gas",
    "BIT" =	"Bituminous coal",
    "BLQ" =	"Black liquor",
    "COG" =	"Coke oven gas",
    "DFO" =	"Distillate fuel oil",
    "GEO" =	"Geothermal",
    "JF"  = "Jet fuel",
    "KER" =	"Kerosene",
    "LFG" =	"Landfill gas",
    "LIG" =	"Lignite coal",
    "MSW" =	"Municipal solid waste",
    "MWH" =	"Stored electricity",
    "NG"  = "Natural gas",
    "NUC" =	"Nuclear",
    "OBG" =	"Other biomass gas",
    "OBL" =	"Other biomass liquids",
    "OBS" =	"Other biomass solids",
    "OG"  = "Other gas",
    "OTH" =	"Other unknown",
    "PC"  = "Petroleum coke",
    "PRG" =	"Process gas",
    "PUR" =	"Purchased steam",
    "RC"  = "Refined coal",
    "RFO" =	"Residual fuel oil",
    "SGC" =	"Coal-derived synthetic gas", 
    "SUB" =	"Subbituminous coal",
    "SUN"	= "Solar",
    "TDF" =	"Tire-derived fuel",
    "WAT" =	"Hydro",
    "WC"  = "Waste coal", 
    "WDL" =	"Wood, wood waste liquid",
    "WDS" =	"Wood, wood waste solid",
    "WH"  =	"Waste heat",
    "WND" =	"Wind",
    "WO"  = "Waste oil")

egrid_plant_list <- 
  lapply(years, function(year) {
    plant_df <- get(paste0("egrid_", year, "_plant"))
    
    plant_df <- 
      plant_df %>%
      mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))),
                    .fns = ~ .x * 100)) %>% 
      select(-contains("seqplt")) %>% 
      rename("Year" = YEAR) 
    
    if(year == 2023) { 
      plant_df <- 
        plant_df %>% 
        rename("CAMDFLAG" = CAPDFLAG)}
    
    plant_df
  })

egrid_plant <- 
  bind_rows(egrid_plant_list) %>% 
  mutate(PLPRMFL2 = recode(PLPRMFL, !!!fuel_type_map, default = PLPRMFL)) %>% # add column with long hand fuel type names
  rename("FUEL" = PLFUELCT,
         "PLNAMEPCAP" = NAMEPCAP) 

# identify secondary fuel type for each plant (if exists) 
secondary_fuel_category <- # map fuel category to secondary fuel type
  c("COAL"       = "Coal", 
    "OIL"        = "Oil", 
    "GAS"        = "Gas", 
    "NUCLEAR"    = "Nuclear", 
    "HYDRO"      = "Hydro", 
    "BIOMASS"    = "Biomass", 
    "WIND"       = "Wind", 
    "SOLAR"      = "Solar", 
    "GEOTHERMAL" = "Geothermal", 
    "OSFL"       = "Other Fossil", 
    "OTHF"       = "Unknown")

plant_resource_mix_cols <- 
  resource_mix_cols[1:18] # do not include nonbaseload columns
names(plant_resource_mix_cols) <- paste0("PL", names(resource_mix_cols[1:18]))

secondary_fuel <- 
  egrid_plant %>% 
  select(Year, ORISPL, PLPRMFL, FUEL, names(plant_resource_mix_cols)) %>% 
  tidyr::pivot_longer(cols = paste0("PL", names(resource_mix_cols[1:18])), 
               names_to = "resource_mix_fuel", 
               values_to = "resource_mix") %>% 
  mutate(resource_mix_fuel = recode(resource_mix_fuel, !!!plant_resource_mix_cols)) %>% 
  filter(!is.na(resource_mix), 
         resource_mix > 0, 
         !is.na(resource_mix_fuel), 
         resource_mix_fuel != FUEL) %>% 
  mutate(resource_mix_fuel = recode(resource_mix_fuel, !!!secondary_fuel_category)) %>% 
  group_by(Year, ORISPL) %>% 
  mutate(SECFUEL = paste(resource_mix_fuel, collapse = ", ")) %>% 
  ungroup() %>% 
  select(Year, ORISPL, SECFUEL) %>% 
  distinct()

egrid_plant_2 <- # merge secondary fuel into plant file
  egrid_plant %>% 
  left_join(secondary_fuel, by = c("Year", "ORISPL")) %>% 
  rename("PLPRMFL_OLD" = PLPRMFL, # rename primary fuel columns
         "PLPRMFL" = PLPRMFL2) %>% 
  # re-order new columns to location in 2023 data
  relocate(SECFUEL, .after = "FUEL") %>% 
  relocate(PLPRMFL, .after = "PLPRMFL_OLD") 

### State file --------------------

egrid_state_list <- 
  lapply(years, function(year) {
    state_df <- get(paste0("egrid_", year, "_state"))
    
    state_df <- 
      state_df %>%
      mutate(across(.cols = any_of(paste0("ST", names(resource_mix_cols))),
                    .fns = ~ .x * 100)) %>% 
      rename("Year" = YEAR) 
    
    if(year == 2018) {
      state_df <- 
        state_df %>% 
        rename("STNAMEPCAP" = NAMEPCAP)}
    
    state_df
  })

egrid_state <- bind_rows(egrid_state_list)

### Balancing authority file --------------------

egrid_ba_list <- 
  lapply(years, function(year) {
    ba_df <- get(paste0("egrid_", year, "_ba"))
    
    ba_df <- 
      ba_df %>%
      mutate(across(.cols = any_of(paste0("BA", names(resource_mix_cols))),
                    .fns = ~ .x * 100)) %>% 
      rename("Year" = YEAR) 
    
    if(year == 2018) {
      ba_df <- 
        ba_df %>% 
        rename("BANAMEPCAP" = NAMEPCAP)}
    
    ba_df
  })

egrid_ba <- bind_rows(egrid_ba_list)

### Subregion file ----------------------------

egrid_subregion_list <- 
  lapply(years, function(year) {
    subregion_df <- get(paste0("egrid_", year, "_subregion"))
    
    subregion_df <- 
      subregion_df %>%
      mutate(across(.cols = any_of(paste0("SR", names(resource_mix_cols))),
                    .fns = ~ .x * 100)) %>% 
      rename("Year" = YEAR) 
    
    if(year == 2018) {
      subregion_df <- 
        subregion_df %>% 
        rename("SRNAMEPCAP" = NAMEPCAP)}
    
    subregion_df
  })

egrid_subregion <- bind_rows(egrid_subregion_list)

### NERC file -------------------------------

egrid_nerc_list <- 
  lapply(years, function(year) {
    nerc_df <- get(paste0("egrid_", year, "_nerc"))
    
    nerc_df <- 
      nerc_df %>%
      mutate(across(.cols = any_of(paste0("NR", names(resource_mix_cols))),
                    .fns = ~ .x * 100)) %>% 
      rename("Year" = YEAR) 
    
    if(year == 2018) {
      nerc_df <- 
        nerc_df %>% 
        rename("NRNAMEPCAP" = NAMEPCAP)}
      
    if(year %in% 2019:2020) { 
      nerc_df <- 
        nerc_df %>% 
        rename("NRGENASO" = NRGENAOP,
               "NRGENAOP" = SumOfPLGENAOP)}
    
    nerc_df
  })

egrid_nerc <- bind_rows(egrid_nerc_list)

### US file -------------------------------------

egrid_us_list <- 
  lapply(years, function(year) {
  us_df <- get(paste0("egrid_", year, "_us"))
  
  us_df <- 
    us_df %>%
    mutate(across(.cols = any_of(paste0("US", names(resource_mix_cols))),
                  .fns = ~ .x * 100)) %>% 
    rename("Year" = YEAR) 
  
  if(year == 2018) {
    us_df <- 
      us_df %>% 
      rename("USNAMEPCAP" = NAMEPCAP)
  }
  
  us_df
})

egrid_us <- bind_rows(egrid_us_list)


# Export data  ---------------------------------

save_output_data(egrid_plant_2, "data/2b_web_updates", "data_explorer_plant_file.csv", file_type = "CSV")
save_output_data(egrid_state, "data/2b_web_updates", "data_explorer_state_file.csv", file_type = "CSV")
save_output_data(egrid_ba, "data/2b_web_updates", "data_explorer_ba_file.csv", file_type = "CSV")
save_output_data(egrid_subregion, "data/2b_web_updates", "data_explorer_subregion_file.csv", file_type = "CSV")
save_output_data(egrid_nerc, "data/2b_web_updates", "data_explorer_nerc_file.csv", file_type = "CSV")
save_output_data(egrid_us, "data/2b_web_updates", "data_explorer_us_file.csv", file_type = "CSV")

