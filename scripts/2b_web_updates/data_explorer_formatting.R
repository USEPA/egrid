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

# Check for params() --------

source("scripts/functions/function_check_params.R")

# Create and check parameters 
if (!exists("params")) {
  params <- check_params()
} else {
  print("eGRID year and version parameters are already defined.")
}

# Check for necessary folders ---------------------

if(!dir.exists("data/1_production_model/static_tables/historical_egrid")) { 
  dir.create("data/1_production_model/static_tables/historical_egrid")}

# Download historical eGRID years ------------------------

file_paths <- 
  c("data/1_production_model/static_tables/historical_egrid/egrid2018_data.xlsx", 
    "data/1_production_model/static_tables/historical_egrid/egrid2019_data.xlsx",
    "data/1_production_model/static_tables/historical_egrid/egrid2020_data.xlsx",
    "data/1_production_model/static_tables/historical_egrid/egrid2021_data.xlsx",
    "data/1_production_model/static_tables/historical_egrid/egrid2022_data.xlsx") # add previous data year to this list every year 

urls <- ### Note: check for updates or changes each data year ###
  c("https://www.epa.gov/sites/default/files/2020-03/egrid2018_data_v2.xlsx", 
    "https://www.epa.gov/sites/default/files/2021-02/egrid2019_data.xlsx", 
    "https://www.epa.gov/system/files/documents/2022-09/eGRID2020_Data_v2.xlsx", 
    "https://www.epa.gov/system/files/documents/2023-01/eGRID2021_data.xlsx",
    "https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx") # add previous data year to this list every year 

for(i in 1:length(urls)) { # download files online if they have not already been downloaded. 
  if(!file.exists(file_paths[i])) { 
    download.file(url = urls[i], 
                  destfile = file_paths[i], 
                  mode = "wb")
    } else {
    print(glue::glue("Stopping. File {file_paths[i]} already downloaded."))
  }}

# Load historical eGRID years --------------------------

for(year in c(2018:(as.numeric(params$eGRID_year) - 1))) { ### Note: check for updates or changes each data year ### Add previous data year here
  name_plant <- glue::glue("egrid_{as.character(year)}_plant")
  name_state <- glue::glue("egrid_{as.character(year)}_state")
  name_ba <- glue::glue("egrid_{as.character(year)}_ba")
  name_subregion <- glue::glue("egrid_{as.character(year)}_subregion")
  name_nerc <- glue::glue("egrid_{as.character(year)}_nerc")
  name_us <- glue::glue("egrid_{as.character(year)}_us")
  
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
  
  assign(name_plant, plant)
  assign(name_state, state)
  assign(name_ba, ba)
  assign(name_subregion, subregion)
  assign(name_nerc, nerc)
  assign(name_us, us)}

# Load current year data ------------------

egrid_current_plant <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                                 sheet = glue::glue("PLNT{as.numeric(params$eGRID_year) %% 1000}"),
                                 skip = 1)
  
egrid_current_state <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                                 sheet = glue::glue("ST{as.numeric(params$eGRID_year) %% 1000}"),
                                 skip = 1)

egrid_current_ba <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                              sheet = glue::glue("BA{as.numeric(params$eGRID_year) %% 1000}"),
                              skip = 1)

egrid_current_subregion <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                                     sheet = glue::glue("SRL{as.numeric(params$eGRID_year) %% 1000}"),
                                     skip = 1)

egrid_current_nerc <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                                sheet = glue::glue("NRL{as.numeric(params$eGRID_year) %% 1000}"),
                                skip = 1)

egrid_current_us <- read_excel(glue::glue("data/1_production_model/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                              sheet = glue::glue("US{as.numeric(params$eGRID_year) %% 1000}"),
                              skip = 1)

name_plant <- glue::glue("egrid_{as.character(params$eGRID_year)}_plant")
name_state <- glue::glue("egrid_{as.character(params$eGRID_year)}_state")
name_ba <- glue::glue("egrid_{as.character(params$eGRID_year)}_ba")
name_subregion <- glue::glue("egrid_{as.character(params$eGRID_year)}_subregion")
name_nerc <- glue::glue("egrid_{as.character(params$eGRID_year)}_nerc")
name_us <- glue::glue("egrid_{as.character(params$eGRID_year)}_us")

assign(name_plant, egrid_current_plant)
assign(name_state, egrid_current_state)
assign(name_ba, egrid_current_ba)
assign(name_subregion, egrid_current_subregion)
assign(name_nerc, egrid_current_nerc)
assign(name_us, egrid_current_us)

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

egrid_plant <- 
  bind_rows(# 2018 
            egrid_2018_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")), 
            # 2019
            egrid_2019_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")), 
            # 2020 
            egrid_2020_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")),
            # 2021
            egrid_2021_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")),
            # 2022
            egrid_2022_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")),
            # 2023
            egrid_2023_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              rename("CAMDFLAG" = CAPDFLAG) %>% 
              select(-contains("seqplt"))) %>% 
  mutate(PLPRMFL2 = recode(PLPRMFL, !!!fuel_type_map, default = PLPRMFL)) %>% # add column with long hand fuel type names
  rename("FUEL" = PLFUELCT,
         "Year" = YEAR, 
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

egrid_state <- 
  bind_rows(egrid_2018_state %>% 
              mutate(across(.cols = any_of(paste0("ST", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              rename("STNAMEPCAP" = NAMEPCAP), 
            egrid_2019_state %>% 
              mutate(across(.cols = any_of(paste0("ST", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
            egrid_2020_state %>% 
              mutate(across(.cols = any_of(paste0("ST", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2021_state %>% 
              mutate(across(.cols = any_of(paste0("ST", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2022_state %>% 
              mutate(across(.cols = any_of(paste0("ST", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2023_state %>% 
              mutate(across(.cols = any_of(paste0("ST", names(resource_mix_cols))), 
                            .fns = ~ .x * 100))) %>% 
  rename("Year" = YEAR)

### Balancing authority file --------------------

egrid_ba <- 
  bind_rows(egrid_2018_ba %>% 
              mutate(across(.cols = any_of(paste0("BA", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              rename("BANAMEPCAP" = NAMEPCAP), 
            egrid_2019_ba %>% 
              mutate(across(.cols = any_of(paste0("BA", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
            egrid_2020_ba %>% 
              mutate(across(.cols = any_of(paste0("BA", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2021_ba %>% 
              mutate(across(.cols = any_of(paste0("BA", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2022_ba %>% 
              mutate(across(.cols = any_of(paste0("BA", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2023_ba %>% 
              mutate(across(.cols = any_of(paste0("BA", names(resource_mix_cols))), 
                            .fns = ~ .x * 100))) %>% 
  rename("Year" = YEAR)

### Subregion file ----------------------------

egrid_subregion <- 
  bind_rows(egrid_2018_subregion %>% 
              mutate(across(.cols = any_of(paste0("SR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              rename("SRNAMEPCAP" = NAMEPCAP), 
            egrid_2019_subregion %>% 
              mutate(across(.cols = any_of(paste0("SR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
            egrid_2020_subregion %>% 
              mutate(across(.cols = any_of(paste0("SR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2021_subregion %>% 
              mutate(across(.cols = any_of(paste0("SR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2022_subregion %>% 
              mutate(across(.cols = any_of(paste0("SR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2023_subregion %>% 
              mutate(across(.cols = any_of(paste0("SR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100))) %>% 
  rename("Year" = YEAR)

### NERC file -------------------------------

egrid_nerc <- 
  bind_rows(egrid_2018_nerc %>% 
              mutate(across(.cols = any_of(paste0("NR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              rename("NRNAMEPCAP" = NAMEPCAP), 
            egrid_2019_nerc %>% 
              mutate(across(.cols = any_of(paste0("NR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              rename("NRGENASO" = NRGENAOP,
                     "NRGENAOP" = SumOfPLGENAOP), 
            egrid_2020_nerc %>% 
              mutate(across(.cols = any_of(paste0("NR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              rename("NRGENASO" = NRGENAOP, 
                     "NRGENAOP" = SumOfPLGENAOP),
            egrid_2021_nerc %>% 
              mutate(across(.cols = any_of(paste0("NR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2022_nerc %>% 
              mutate(across(.cols = any_of(paste0("NR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2023_nerc %>% 
              mutate(across(.cols = any_of(paste0("NR", names(resource_mix_cols))), 
                            .fns = ~ .x * 100))) %>% 
  rename("Year" = YEAR)

### US file -------------------------------------

egrid_us <- 
  bind_rows(egrid_2018_us %>% 
              mutate(across(.cols = any_of(paste0("US", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              rename("USNAMEPCAP" = NAMEPCAP), 
            egrid_2019_us %>% 
              mutate(across(.cols = any_of(paste0("US", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
            egrid_2020_us %>% 
              mutate(across(.cols = any_of(paste0("US", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2021_us %>% 
              mutate(across(.cols = any_of(paste0("US", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2022_us %>% 
              mutate(across(.cols = any_of(paste0("US", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2023_us %>% 
              mutate(across(.cols = any_of(paste0("US", names(resource_mix_cols))), 
                            .fns = ~ .x * 100))) %>% 
  rename("Year" = YEAR)


# Export data  ---------------------------------

if(!dir.exists(glue::glue("data/2b_web_updates/{params$eGRID_year}"))){
  dir.create(glue::glue("data/2b_web_updates/{params$eGRID_year}"), recursive = TRUE)
}

write.csv(egrid_plant_2, glue::glue("data/2b_web_updates/{params$eGRID_year}/data_explorer_plant_file.csv"), na = "", row.names = FALSE)
write.csv(egrid_state, glue::glue("data/2b_web_updates/{params$eGRID_year}/data_explorer_state_file.csv"), na = "", row.names = FALSE)
write.csv(egrid_ba, glue::glue("data/2b_web_updates/{params$eGRID_year}/data_explorer_ba_file.csv"), na = "", row.names = FALSE)
write.csv(egrid_subregion, glue::glue("data/2b_web_updates/{params$eGRID_year}/data_explorer_subregion_file.csv"), na = "", row.names = FALSE)
write.csv(egrid_nerc, glue::glue("data/2b_web_updates/{params$eGRID_year}/data_explorer_nerc_file.csv"), na = "", row.names = FALSE)
write.csv(egrid_us, glue::glue("data/2b_web_updates/{params$eGRID_year}/data_explorer_us_file.csv"), na = "", row.names = FALSE)


