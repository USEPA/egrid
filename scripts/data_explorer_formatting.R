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

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- (params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Check for necessary folders ---------------------

if(!dir.exists("data/static_tables/historical_egrid")) { 
  dir.create("data/static_tables/historical_egrid")}

# Download historical eGRID years ------------------------

file_paths <- 
  c("data/static_tables/historical_egrid/egrid2018_data.xlsx", 
    "data/static_tables/historical_egrid/egrid2019_data.xlsx",
    "data/static_tables/historical_egrid/egrid2020_data.xlsx",
    "data/static_tables/historical_egrid/egrid2021_data.xlsx",
    "data/static_tables/historical_egrid/egrid2022_data.xlsx") # add previous data year to this list every year 

urls <- ### Note: check for updates or changes each data year ###
  c("https://www.epa.gov/sites/default/files/2020-03/egrid2018_data_v2.xlsx", 
    "https://www.epa.gov/sites/default/files/2021-02/egrid2019_data.xlsx", 
    "https://www.epa.gov/system/files/documents/2022-09/eGRID2020_Data_v2.xlsx", 
    "https://www.epa.gov/system/files/documents/2023-01/eGRID2021_data.xlsx",
    "https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx") # add previous data year to this list every year 

for(i in 1:length(urls)) { # download files online if they have not already been downloaded. 
  if(file.exists(file_paths[i])) { 
    download.file(url = urls[i], 
                  destfile = file_paths[i], 
                  mode = "wb")
    } else {
    print(glue::glue("Stopping. File {file_paths[i]} already downloaded."))
  }}

# Load historical eGRID years --------------------------

for(year in c(2018:2022)) { ### Note: check for updates or changes each data year ### Add previous data year here
  name_plant <- glue::glue("egrid_{as.character(year)}_plant")
  name_state <- glue::glue("egrid_{as.character(year)}_state")
  name_ba <- glue::glue("egrid_{as.character(year)}_ba")
  name_subregion <- glue::glue("egrid_{as.character(year)}_subregion")
  name_nerc <- glue::glue("egrid_{as.character(year)}_nerc")
  name_us <- glue::glue("egrid_{as.character(year)}_us")
  
  plant <- read_excel(glue::glue("data/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                      sheet = glue::glue("PLNT{year %% 1000}"),
                      skip = 1)
  
  state <- read_excel(glue::glue("data/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                      sheet = glue::glue("ST{year %% 1000}"),
                      skip = 1)
  
  ba <- read_excel(glue::glue("data/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                   sheet = glue::glue("BA{year %% 1000}"),
                   skip = 1)
  
  subregion <- read_excel(glue::glue("data/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                          sheet = glue::glue("SRL{year %% 1000}"),
                          skip = 1)
  
  nerc <- read_excel(glue::glue("data/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                     sheet = glue::glue("NRL{year %% 1000}"),
                     skip = 1)
  
  us <- read_excel(glue::glue("data/static_tables/historical_egrid/egrid{as.character(year)}_data.xlsx"), 
                   sheet = glue::glue("US{year %% 1000}"),
                   skip = 1)
  
  assign(name_plant, plant)
  assign(name_state, state)
  assign(name_ba, ba)
  assign(name_subregion, subregion)
  assign(name_nerc, nerc)
  assign(name_us, us)}

# Load current year data ------------------

egrid_2023_plant <- read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                               sheet = glue::glue("PLNT{as.numeric(params$eGRID_year) %% 1000}"),
                               skip = 1)
  
egrid_2023_state <- read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                               sheet = glue::glue("ST{as.numeric(params$eGRID_year) %% 1000}"),
                               skip = 1)

egrid_2023_ba <- read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                            sheet = glue::glue("BA{as.numeric(params$eGRID_year) %% 1000}"),
                            skip = 1)

egrid_2023_subregion <- read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                                   sheet = glue::glue("SRL{as.numeric(params$eGRID_year) %% 1000}"),
                                   skip = 1)

egrid_2023_nerc <- read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                              sheet = glue::glue("NRL{as.numeric(params$eGRID_year) %% 1000}"),
                              skip = 1)

egrid_2023_us <- read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                            sheet = glue::glue("US{as.numeric(params$eGRID_year) %% 1000}"),
                            skip = 1)

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
    "COPR" = NA_character_)

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
  bind_rows(egrid_2018_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")), 
            egrid_2019_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                                               .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")), 
            egrid_2020_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")),
            egrid_2021_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")),
            egrid_2022_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt")),
            egrid_2023_plant %>% 
              mutate(across(.cols = any_of(paste0("PL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)) %>% 
              select(-contains("seqplt"))) %>% 
  mutate(PLPRMFL2 = recode(PLPRMFL, !!!fuel_type_map, default = PLPRMFL)) %>% # add column with long hand fuel type names
  rename("FUEL" = PLFUELCT) 

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
  resource_mix_cols 
names(plant_resource_mix_cols) <- paste0("PL", names(resource_mix_cols))

secondary_fuel <- 
  egrid_plant %>% 
  select(YEAR, ORISPL, PLPRMFL, FUEL, names(plant_resource_mix_cols)) %>% 
  tidyr::pivot_longer(cols = paste0("PL", names(resource_mix_cols)), 
               names_to = "resource_mix_fuel", 
               values_to = "resource_mix") %>% 
  mutate(resource_mix_fuel = recode(resource_mix_fuel, !!!plant_resource_mix_cols)) %>% 
  filter(!is.na(resource_mix), 
         resource_mix > 0, 
         !is.na(resource_mix_fuel), 
         resource_mix_fuel != FUEL) %>% 
  mutate(resource_mix_fuel = recode(resource_mix_fuel, !!!secondary_fuel_category)) %>% 
  group_by(YEAR, ORISPL) %>% 
  mutate(SECFUEL = paste(resource_mix_fuel, collapse = ", ")) %>% 
  ungroup() %>% 
  select(YEAR, ORISPL, SECFUEL) %>% 
  distinct()

egrid_plant_2 <- # merge secondary fuel into plant file
  egrid_plant %>% 
  left_join(secondary_fuel, by = c("YEAR", "ORISPL")) %>% 
  rename("PLPRMFL_OLD" = PLPRMFL, # rename primary fuel columns
         "PLPRMFL" = PLPRMFL2) %>% 
  # re-order new columns to location in 2023 data
  relocate(SECFUEL, .after = "FUEL") %>% 
  relocate(PLPRMFL, .after = "PLPRMFL_OLD") %>% 
  relocate(CAMDFLAG, .after = "LON") %>% 
  relocate(CAPDFLAG, .after = "CAMDFLAG") %>% 
  relocate(PLNGENNB, .after = "PLNGENOZ") %>% 
  relocate(PLC2ECRT, .after = "PLN2OCRT") %>% 
  relocate(UNCO2E, .after = "UNN2O") %>% 
  relocate(UNC2ESRC, .after = "UNN2OSRC") %>% 
  relocate(BIOCO2E, .after = "BION2O") %>% 
  relocate(CHPCO2E, .after = "CHPN2O") %>% 
  relocate(PLGENATO, .after = "PLGENATR") %>% 
  relocate(PLGENACO, .after = "PLGENACN") %>% 
  relocate(PLTOPR, .after = "PLTRPR") %>% 
  relocate(PLCOPR, .after = "PLCNPR")

### State file --------------------

egrid_state <- 
  bind_rows(egrid_2018_state %>% 
              mutate(across(.cols = any_of(paste0("ST", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
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
                            .fns = ~ .x * 100)))

### Balancing authority file --------------------

egrid_ba <- 
  bind_rows(egrid_2018_ba %>% 
              mutate(across(.cols = any_of(paste0("BA", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
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
                            .fns = ~ .x * 100)))

### Subregion file ----------------------------

egrid_subregion <- 
  bind_rows(egrid_2018_subregion %>% 
              mutate(across(.cols = any_of(paste0("SRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
            egrid_2019_subregion %>% 
              mutate(across(.cols = any_of(paste0("SRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
            egrid_2020_subregion %>% 
              mutate(across(.cols = any_of(paste0("SRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2021_subregion %>% 
              mutate(across(.cols = any_of(paste0("SRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2022_subregion %>% 
              mutate(across(.cols = any_of(paste0("SRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2023_subregion %>% 
              mutate(across(.cols = any_of(paste0("SRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)))

### NERC file -------------------------------

egrid_nerc <- 
  bind_rows(egrid_2018_nerc %>% 
              mutate(across(.cols = any_of(paste0("NRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
            egrid_2019_nerc %>% 
              mutate(across(.cols = any_of(paste0("NRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
            egrid_2020_nerc %>% 
              mutate(across(.cols = any_of(paste0("NRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2021_nerc %>% 
              mutate(across(.cols = any_of(paste0("NRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2022_nerc %>% 
              mutate(across(.cols = any_of(paste0("NRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)),
            egrid_2023_nerc %>% 
              mutate(across(.cols = any_of(paste0("NRL", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)))

### US file -------------------------------------

egrid_us <- 
  bind_rows(egrid_2018_us %>% 
              mutate(across(.cols = any_of(paste0("US", names(resource_mix_cols))), 
                            .fns = ~ .x * 100)), 
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
                            .fns = ~ .x * 100)))


# Export data  ---------------------------------

write_csv(egrid_plant_2, glue::glue("data/outputs/{params$eGRID_year}/data_explorer_plant_file.csv"))
write_csv(egrid_state, glue::glue("data/outputs/{params$eGRID_year}/data_explorer_state_file.csv"))
write_csv(egrid_ba, glue::glue("data/outputs/{params$eGRID_year}/data_explorer_ba_file.csv"))
write_csv(egrid_subregion, glue::glue("data/outputs/{params$eGRID_year}/data_explorer_subregion_file.csv"))
write_csv(egrid_nerc, glue::glue("data/outputs/{params$eGRID_year}/data_explorer_nerc_file.csv"))
write_csv(egrid_us, glue::glue("data/outputs/{params$eGRID_year}/data_explorer_us_file.csv"))


