## -------------------------------
##
## Plant file create 
## 
## Purpose: 
## 
## This file creates the plant file for eGRID. 
## This includes all operating plants for the specified eGRID data year
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Sara Sokolinski, Abt Global, sara.sokolinksi@abtglobal.com
##      Teagan Goforth, Abt Global, teagan.goforth@abtglobal.com
##
## -------------------------------

# Load required libraries --------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# Load necessary data ----------

### Load EIA data ------------

eia_860 <- read_rds("data/clean_data/eia/eia_860_clean.RDS")
eia_861 <- read_rds("data/clean_data/eia/eia_861_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia/eia_923_clean.RDS") 

### Load lower-level eGRID files (unit and generator files) ------------
#source(here("scripts", "gen_rename.r"))
generator_file <- read_rds("data/outputs/generator_file.RDS")
#generator_file <- read_xlsx(here("data/outputs/qa/Gen file 9_25_24.xlsx")) 
#generator_file <- gen_rename(generator_file)
#generator_file <- generator_file %>%
  #select("plant_id", "generator_id", ends_with("_access")) %>% 
#  mutate(seqgen = 1:nrow(generator_file) ) 
#colnames(generator_file) <- gsub("_access","", colnames(generator_file))

#unit_file <- # using unit file from Access production for now
#  read_xlsx(here("data/outputs/qa/Unit File 9_17_24.xlsx")
#  )

unit_file <- read_rds("data/outputs/unit_file.RDS")

# Create a file to string concatenate unique values that are not NA ----------

paste_concat <- function(l, number = FALSE, concat = TRUE){
  l <- l[!is.na(l)]
  l <- l[l!="NA" & l!=""]
  l[order(l)]
  
  if(length(unique(l)) == 0){txt <- NA
  }else if(concat){
    txt <- paste0(unique(l), collapse = ", ")
    if(txt == ""){txt <- NA}
  }else{
    txt = unique(l)
  }
  if(number){return(as.numeric(txt)) # convert to numeric if it can
    }else{return(txt)}
}


# Aggregate unit file to plant level ------------------------

plant_unit <- 
  unit_file %>% 
  group_by(year, plant_id, plant_state, plant_name) %>%
  summarize(camd_flag = paste_concat(camd_flag),
            num_units = n(), # count units in a plant
            unadj_heat_input = sum(heat_input, na.rm = TRUE), # units: MMBtu
            unadj_heat_input_source = paste_concat(heat_input_source),
            unadj_heat_input_oz = sum(heat_input_oz, na.rm = TRUE), # units: MMBtu 
            unadj_heat_input_oz_source = paste_concat(heat_input_oz_source),
            unadj_nox_mass = sum(nox_mass, na.rm = TRUE), # units: tons
            unadj_nox_source = paste_concat(nox_source),
            unadj_nox_oz_mass = sum(nox_oz_mass, na.rm = TRUE), # units: tons
            unadj_nox_oz_source = paste_concat(nox_oz_source),
            unadj_so2_mass = sum(so2_mass, na.rm = TRUE), # units: tons
            unadj_so2_source = paste_concat(so2_source),
            unadj_co2_mass = sum(co2_mass, na.rm = TRUE), # units: tons
            unadj_co2_source = paste_concat(co2_source),
            unadj_hg_mass = ifelse("hg_mass" %in% colnames(unit_file), sum(hg_mass, na.rm = TRUE) , NA_character_),
            unadj_hg_source = ifelse("hg_source" %in% colnames(unit_file), paste_concat(hg_source) , NA_character_)) %>% 
  ungroup()

# Aggregate gen file to plant level----------------------------

plant_gen <- 
  generator_file %>% 
  filter(!is.na(plant_id)) %>% # remove rows with NA plant ID 
  group_by(year, plant_id, plant_state, plant_name) %>%
  summarize(num_gen = n(), # count
            nameplate_capacity = sum(nameplate_capacity, na.rm = TRUE), # units?
            generation_ann = sum(generation_ann, na.rm = TRUE), # units?
            generation_oz = sum(generation_oz, na.rm = TRUE), # units?
            fuel_code = paste_concat(fuel_code)) %>% 
  ungroup()

# Pull in plant identifying information from the EIA-860 Plant file  -------------------------------

# join EIA-860 data to the aggregated generator file
plant_gen_2 <- 
  plant_gen %>% 
  left_join(eia_860$plant %>% 
            select(plant_id, 
                   county, 
                   "lat" = latitude, 
                   "lon" = longitude,
                   utility_name, 
                   utility_id,
                   "system_owner" = transmission_or_distribution_system_owner, 
                   "system_owner_id" = transmission_or_distribution_system_owner_id,
                   "ba_name" = balancing_authority_name,
                   "ba_code" = balancing_authority_code,
                   sector_name, 
                   "nerc" = nerc_region) %>%
             distinct()) %>% 
  mutate(county = if_else(grepl('not in file|Not in file|NOT IN FILE', county), NA_character_ , county))


# Combustion heat input ---------------------------------------
# calculate heat input from combustion fuels

unit_heat_input <- 
  unit_file %>% 
  group_by(primary_fuel_type, prime_mover, plant_id) %>%
  summarize(unadj_heat_input = sum(heat_input, na.rm = TRUE),
            unadj_heat_input_oz = sum(heat_input_oz, na.rm = TRUE)) %>% 
  ungroup()

combustion_fuels <- c("AB","ANT", "BFG","BIT","BLQ","COG","DFO","JF","KER","LFG","LIG",
                      "MSB","MSN","MSW","NG","OBG","OBL","OBS","OG","PC","PG","RC","RFO",
                      "SGC","SGP","SLW","SUB","TDF","WC","WDL","WDS","WO") 

combust_heat_input <- 
  unit_heat_input %>% 
  filter(primary_fuel_type %in% combustion_fuels ) %>%
  group_by(plant_id) %>% 
  summarize(unadj_combust_heat_input = sum(unadj_heat_input, na.rm = TRUE), # sum heat input for combustion fuels
            unadj_combust_heat_input_oz = sum(unadj_heat_input_oz, na.rm = TRUE)) %>% 
  ungroup()

# join with aggregated unit file

plant_unit_2 <- 
  plant_unit %>% 
  left_join(combust_heat_input)  


# Update capacity factor  -----------------------------------------------

plant_gen_3 <- 
  plant_gen_2 %>% 
  mutate(capfac = if_else(generation_ann / (nameplate_capacity * 8760) < 0, 0, 
                          generation_ann / (nameplate_capacity * 8760)))

# Calculate CH4 emissions N2O emissions  ----------------------------------------

# load static table with emissions factors

ef_co2_ch4_n2o <- 
  read_csv("data/static_tables/co2_ch4_n2o_ef.csv") %>% 
  filter(!is.na(eia_fuel_code)) 

# join with eia to calculate at plant level
    
emissions_ch4_n2o <- 
  eia_923$generation_and_fuel_combined %>% 
  filter(prime_mover != "FC") %>% 
  select(plant_id, total_fuel_consumption_mmbtu, fuel_type) %>%
  left_join(ef_co2_ch4_n2o, by = c("fuel_type" = "eia_fuel_code")) %>%
  mutate(unadj_ch4_mass = ch4_ef * total_fuel_consumption_mmbtu,
         unadj_n2o_mass = n2o_ef * total_fuel_consumption_mmbtu) %>%
  group_by(plant_id) %>%
  summarize(unadj_ch4_mass = sum(unadj_ch4_mass, na.rm = TRUE),
            unadj_n2o_mass = sum(unadj_n2o_mass, na.rm = TRUE),
            ch4_source = "EIA",
            n2o_source = "EIA") %>% 
  ungroup()

# Create plant file ------------

# combine generator and unit aggregation files into one plant file

plant_file <- 
  plant_gen_3 %>% 
  select(-plant_name) %>% # names differ between sources, so default to names in unit file
  full_join(plant_unit_2, by = c("plant_id", "plant_state", "year")) %>% 
  left_join(emissions_ch4_n2o) # add in CH4 and N2O emission masses


# Plant region assignments -----------

# assign plants to counties, states, ISORTOs, balancing authorities, and NERC regions

### Add in FIPS codes for state and county ----------------

# read in crosswalk that updates county names to match EIA data
xwalk_county_names <- read_csv("data/static_tables/xwalk_fips_names_update.csv") %>% 
  janitor::clean_names() 

# read in table that lists state and county FIPS codes 
state_county_fips <- 
  read_csv("data/static_tables/state_county_fips.csv") %>% janitor::clean_names() %>%  # load in table and clean names to snake_case
  left_join(xwalk_county_names, by = c("postal_state_code" = "state",
                                       "county_name" = "appendix_6_county_name")) %>%
  mutate(county_name = if_else(is.na(eia_county_name), county_name, eia_county_name)) %>% # update county names to match EIA data if applicable
  select("plant_state" = postal_state_code, # rename to match plant file 
         "county" = county_name, 
         fips_state_code, 
         fips_county_code) 

# adjust Alaska county names via static crosswalk table
xwalk_alaska_fips <- 
  read_csv("data/static_tables/xwalk_alaska_fips.csv") %>%
  janitor::clean_names() %>% 
  select("plant_state" = state, "plant_id" = plant_code, county, "new_county" = appendix_6_county_name) %>% 
  mutate(plant_id = as.character(plant_id))

# update plant file with FIPS information 
plant_file_2 <- 
  plant_file %>% 
  left_join(state_county_fips, by = c("plant_state", "county")) %>% 
  left_join(xwalk_alaska_fips, by = c("plant_state", "plant_id", "county")) %>% 
  mutate(county = ifelse(!is.na(new_county), new_county, county)) %>% 
  select(-new_county)

### Balancing authority assignments --------------

# count ba_ids for each utility to find those with only 1
utility_ba <- 
  plant_file_2 %>% 
  group_by(utility_id) %>% 
  summarize(count = n(), #length(paste_concat(ba_id, concat = FALSE)),
            ba_code = paste_concat(ba_code),
            ba_name = paste_concat(ba_name)) %>% 
  filter(count == 1)

# replace ba_code and ba_name for these utilities 
plant_file_3 <- 
  plant_file_2 %>%
  rows_update(utility_ba %>% select(-count), 
              by = c("utility_id"), unmatched = "ignore") %>% 
  mutate(ba_code = if_else(ba_name == "Hawaiian Electric Co Inc", "HECO", ba_code),
         ba_name = if_else(ba_name %in% c("No BA", "NA"), NA_character_, ba_name),
         ba_code = if_else(ba_code == "NA", NA_character_, ba_code))

# check all codes have names and all names have codes
stopifnot(nrow(plant_file_3[which(!is.na(plant_file_3$ba_code) & is.na(plant_file_3$ba_name)),])==0)
stopifnot(nrow(plant_file_3[which(is.na(plant_file_3$ba_code) & !is.na(plant_file_3$ba_name)),])==0)


# look up plant_ids with both missing that exist in EIA_861
lookup_ba <- plant_file_3[which(is.na(plant_file_3$ba_code) & is.na(plant_file_3$ba_name)),]


# merge the balancing_authority and sales_ult_cust to get utility_id, ba_name, and ba_id in one df
# filter to utilities with missing ba data that are not NA
eia_861_utility <- 
  eia_861$sales_ult_cust %>% 
  full_join(eia_861$balancing_authority %>% 
              mutate(year = as.character(year))) %>% 
  select(utility_id = utility_number, 
         ba_code, 
         ba_name = balancing_authority_name) %>% 
  mutate(utility_id = as.character(utility_id)) %>% 
  filter(utility_id %in% lookup_ba$utility_id & !is.na(ba_name) & !is.na(utility_id)) 

# update BA names via ba_codes.csv 
ba_codes <- 
  read_csv("data/static_tables/ba_codes.csv") %>% janitor::clean_names() %>% 
  rename("ba_code" = bacode, 
         "ba_name_new"= baname)

# update plant_file with the lookup
plant_file_4 <- 
  plant_file_3 %>% 
  rows_patch(eia_861_utility, by = c("utility_id"), unmatched = "ignore") %>% 
  mutate(ba_name = if_else(ba_name == "No BA", NA_character_, ba_name), 
         ba_name = if_else(ba_code == "" | is.na(ba_code), "No balancing authority", ba_name),
         ba_code = if_else(ba_code == "" | is.na(ba_code), "NA", ba_code), 
         ba_code = if_else(ba_code == "PS", "PSCO", ba_code)) %>% # manually update PS ba_code to PSCO
  left_join(ba_codes, by = c("ba_code")) %>%  # update ba_name by ba_code 
  mutate(ba_name = ba_name_new) %>% select(-ba_name_new)

### NERC and eGRID subregion assignments ----------------------------------------

# load crosswalk that matches ba_code and ba_name to egrid_subregions
xwalk_ba <- 
  read_csv("data/static_tables/xwalk_balancing_authority.csv") %>%
  janitor::clean_names() %>% 
  select(ba_code = "balancing_authority_code",
         ba_name = "balancing_authority_name",
         egrid_subregion = "subrgn") 

# load crosswalk that matches BA code and system_owner_id to update egrid_subregions
xwalk_ba_transmission <- 
  read_csv("data/static_tables/xwalk_ba_transmission.csv") %>% janitor::clean_names() %>% 
  rename(nerc = "nerc_region",
         ba_code = "balancing_authority_code",
         system_owner_id = "transmission_or_distribution_system_owner_id",
         egrid_subregion = "subrgn") %>% 
  mutate(system_owner_id = as.character(system_owner_id)) %>% distinct()

# load crosswalk that matches ba_code, system_owner_id, and utility_id to update NA egrid_subregions
xwalk_ba_pjm <- 
  read_csv("data/static_tables/xwalk_ba_pjm.csv") %>% janitor::clean_names() %>% 
  rename(nerc = "nerc_region",
         ba_code = "balancing_authority_code",
         system_owner_id = "transmission_or_distribution_system_owner_id",
         egrid_subregion = "subrgn") %>% 
  mutate(system_owner_id = as.character(system_owner_id),
         utility_id = as.character(utility_id)) %>% distinct() %>% 
  drop_na()

# load crosswalk that matches plant_id and plant_state to plant file to update NA egrid_subregions
xwalk_oris_wecc <- 
  read_csv("data/static_tables/xwalk_oris_wecc.csv") %>% janitor::clean_names() %>% 
  select(plant_state = "pstatabb",
         plant_id = "orispl", 
         egrid_subregion = "subrgn") %>% distinct() %>% 
  mutate(plant_id = as.character(plant_id))

# load files that match plant_id to plant file update NA egrid_subregions
xwalk_nerc_assessment <- 
  read_csv("data/static_tables/nerc_assessment_areas_grouped_by_plant.csv") %>% janitor::clean_names() %>% 
  full_join(read_csv("data/static_tables/xwalk_nerc_assessment.csv") %>% janitor::clean_names()) %>% 
  rename(plant_id = "plant_code",
         egrid_subregion = "subrgn") %>% 
  mutate(plant_id = as.character(plant_id)) %>% 
  filter(!is.na(egrid_subregion) & !is.na(plant_id)) %>%
  select(-assessment_area) %>% distinct() %>% 
  group_by(plant_id) %>% filter(n() == 1) # remove plant_ids with multiple NERC regions listed

# load file that matchesegrid_subregions to egrid_subregion_name
egrid_subregions <-  
  read_csv("data/static_tables/egrid_nerc_subregions.csv") %>% janitor::clean_names() %>% 
  rename("egrid_subregion" = "subrgn",
         "egrid_subregion_name" = "srname")

# update plant file with NERC subregion updates
plant_file_5 <- 
  plant_file_4 %>% 
  mutate(nerc = case_when(plant_state == "PR" ~ "PR",
                          plant_state == "AK" ~ "AK",
                          plant_state == "HI" ~ "HI",
                          is.na(nerc) ~ "NA",
                          nerc == "" ~ "NA",
                          TRUE ~ nerc), 
         egrid_subregion = case_when(nerc == "TRE" ~ "ERCT",
                                    nerc == "FRCC" ~ "FRCC",
                                    nerc == "PR" ~ "PRMS",
                                    TRUE ~ NA_character_),
         system_owner_id = if_else(is.na(system_owner_id), "-9999", system_owner_id)) %>% 
  rows_patch(xwalk_ba, by = c("ba_code", "ba_name"), unmatched = "ignore") %>% 
  rows_patch(xwalk_ba_transmission, by = c("nerc", "ba_code", "system_owner_id"), unmatched = "ignore") %>% 
  rows_patch(xwalk_ba_pjm, by =  c("nerc", "ba_code", "system_owner_id", "utility_id"), unmatched = "ignore") %>% 
  rows_patch(xwalk_oris_wecc, by = c("plant_state", "plant_id"), unmatched = "ignore") %>% 
  rows_patch(xwalk_nerc_assessment, by = c("plant_id"), unmatched = "ignore") %>% 
  left_join(egrid_subregions, by = c("egrid_subregion"))

### ISO/RTO assignments -----------------

plant_file_6 <- 
  plant_file_5 %>% 
  mutate(isorto = case_when(ba_code == "CISO" ~ "CAISO",
                            ba_code == "ERCO" ~ "ERCOT",
                            ba_code == "ISNE" ~ "ISONE",
                            ba_code == "MISO" ~ "MISO",
                            ba_code == "NYIS" ~ "NYISO",
                            ba_code == "PJM" ~ "PJM",
                            ba_code == "SPA" ~ "SPP",
                            TRUE ~ NA_character_)) %>%
  mutate(isorto = if_else(plant_state %in% c("AL","AK","AZ","CO","FL","GA", "HI", "ID", "OR", "SC", "TN", "UT", "WA"),
                          NA_character_, isorto))

# Update plant lat/lon ----------------------------------------

plant_file_7 <- 
  plant_file_6 %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         # if any lat/lon coordinates are 0, make them NA 
         lat = if_else(lat == 0, NA_real_, lat),
         lon = if_else(lon == 0, NA_real_, lon), 
         # manual lat/lon updates for some plant IDs
         ### Note: check for updates or changes each data year ###
         lat = case_when(plant_id==54975 ~ 32.380032,
                         plant_id==62262 ~ 42.899029,
                         plant_id==63003 ~ 41,
                         plant_id==64850 ~ 36.169,
                         TRUE ~ lat),
         lon = case_when(plant_id==54975 ~ -106.753716,
                         plant_id==62262 ~ -75.458456,
                         plant_id==63003 ~ -89.996844,
                         plant_id==64850 ~ -81.042,
                         TRUE ~ lon))


# Assign plant primary fuel -----------------------

# first summarize the unit file to get the sum of heat input for each fuel type
# then ungroup and filter to the row (fuel type) with the max value
unit_fuel_by_plant <- 
  unit_file %>% 
  group_by(plant_id, primary_fuel_type) %>% 
  summarize(heat_input = sum(heat_input, na.rm = TRUE)) %>%
  mutate(heat_input = if_else(is.na(heat_input), 0, heat_input)) %>% # replace NA with 0 so rows with only NA do not get filtered out
  ungroup() %>% 
  group_by(plant_id) %>% 
  filter(heat_input == max(heat_input))

# do the same for the generator file with nameplate_capacity       
gen_fuel_by_plant <- 
  generator_file %>% group_by(plant_id, fuel_code) %>% 
  summarize(nameplate_capacity = sum(nameplate_capacity, na.rm = TRUE)) %>%
  mutate(nameplate_capacity = if_else(is.na(nameplate_capacity), 0, nameplate_capacity)) %>% # replace NA with 0 so rows with only NA do not get filtered out
  ungroup() %>% 
  group_by(plant_id) %>% 
  filter(nameplate_capacity == max(nameplate_capacity)) 

# join these tables together
# update primary fuel type as fuel_code (from gen file) if heat_input is 0 and primary_fuel_type (from unit file) otherwise
# drop fuel_code and take distinct values 
fuel_by_plant <- 
  unit_fuel_by_plant %>% 
  full_join(gen_fuel_by_plant, relationship = "many-to-many") %>%
  mutate(primary_fuel_type = if_else(heat_input == 0, fuel_code, primary_fuel_type)) %>% 
  select(-fuel_code) %>% distinct()

# from eia_923 replace na values in total_fuel_consumption_mmbtu with 0 
eia_923_gen_fuel <- 
  eia_923$generation_and_fuel_combined %>% 
  select(plant_id, fuel_type, total_fuel_consumption_mmbtu) %>% 
  mutate(total_fuel_consumption_mmbtu = if_else(is.na(total_fuel_consumption_mmbtu), 0, total_fuel_consumption_mmbtu))

# get plant_ids that are duplicated in fuel_by_plant
# filter to duplicated ids and join with eia_923_gen_fuel
# filter to rows with the max total_fuel_consumption_mmbtu
dup_ids_fuel <- 
  fuel_by_plant$plant_id[which(duplicated(fuel_by_plant$plant_id))] %>% unique() 

fuel_dups <- 
  fuel_by_plant %>% filter(plant_id %in% dup_ids_fuel) %>%
  left_join(eia_923_gen_fuel,
            by = c("plant_id" = "plant_id",
                   "primary_fuel_type" = "fuel_type")) %>% 
  distinct() %>% 
  group_by(plant_id) %>% 
  filter(total_fuel_consumption_mmbtu == max(total_fuel_consumption_mmbtu))

# drop dups from fuel_by_plant for easier joining later
fuel_by_plant_2 <- 
  fuel_by_plant %>% 
  filter(!plant_id %in% dup_ids_fuel) %>% 
  select(-heat_input, -nameplate_capacity)

# get plant_ids that are STILL duplicated
dup_ids_fuel_2 <- fuel_dups$plant_id[which(duplicated(fuel_dups$plant_id))] %>% unique()

# update those to NA and take unique values to remove remaining duplicates
# This didn't occur in SQL and I need to confirm this is what we want to do
fuel_dups_2 <- 
  fuel_dups %>% 
  mutate(primary_fuel_type = if_else(plant_id %in% dup_ids_fuel_2, NA_character_, primary_fuel_type)) %>%
  select(-heat_input, -nameplate_capacity, -total_fuel_consumption_mmbtu) %>% distinct()

# join the duplicates back into fuel_by_plant
fuel_by_plant_3 <- 
  fuel_by_plant_2 %>% full_join(fuel_dups_2)

stopifnot(all(!duplicated(fuel_by_plant_3$plant_id))) # check there are no more duplicates

# Assign plant primary fuel category  ---------------

# identify oil, gas, other fossil, and other fuel types
oil_fuels <- c("DFO", "JF", "KER", "PC", "RFO", "WO", "SGP") 
gas_fuels <- c("NG", "PG", "BU") 
oth_FF <- c("BFG", "OG", "TDF", "MSN") 
oth_fuels <- c("H", "MWH", "OTH", "PRG", "PUR", "WH")

fuel_by_plant_4 <- 
  fuel_by_plant_3 %>%
  mutate(primary_fuel_category = case_when(primary_fuel_type %in% coal_fuels ~ "COAL", # DD didn't include "ANT"
                                           primary_fuel_type %in% oil_fuels ~ "OIL",
                                           primary_fuel_type %in% gas_fuels ~ "GAS",
                                           primary_fuel_type %in% oth_FF ~ "OFSL", # other fossil fuel
                                           primary_fuel_type == "NUC" ~ "NUCLEAR",
                                           primary_fuel_type == "WAT" ~ "HYDRO",
                                           primary_fuel_type == "SUN" ~ "SOLAR",
                                           primary_fuel_type == "WND" ~ "WIND",
                                           primary_fuel_type == "GEO" ~ "GEOTHERMAL",
                                           primary_fuel_type %in% oth_fuels ~ "OTHF", # derived from waste heat/hydrogen/purchased/unknown
                                           primary_fuel_type %in% biomass_fuels ~ "BIOMASS")) # DD didn't include "BG", "DG", "MSB". It did include "MSW" which was excluded here 

plant_file_8 <- 
  plant_file_7 %>% 
  full_join(fuel_by_plant_4, by = c("plant_id")) %>% 
  # manual updates to primary fuel type and category for plants 55970 and 10154 
  ### Note: check for updates or changes each data year ###
  mutate(primary_fuel_type = case_when(plant_id == 55970 ~ "NG",
                                       plant_id == 10154 ~ "NG",
                                       TRUE ~ primary_fuel_type),
         primary_fuel_category = case_when(plant_id == 55970 ~ "GAS",
                                           plant_id == 10154 ~ "GAS",
                                           TRUE ~ primary_fuel_category))


# Create plant flags ------------
### Create coal flag -----------------

oth_og_recode <- 
  read_csv("data/static_tables/og_oth_units_to_change_fuel_type.csv") %>% 
  select(plant_id, "fuel_type" = primary_fuel_type, fuel_code) %>% 
  mutate(plant_id = as.character(plant_id))

coal_fuels <- c("ANT","BIT", "COG", "LIG", "RC", "SGC", "SUB", "WC")

# create a flag that identifies which plants are coal
coal_plants <- 
  eia_923$generation_and_fuel_data %>% 
  group_by(plant_id, fuel_type) %>% 
  summarize(total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) %>%
  full_join(oth_og_recode) %>%
  mutate(coal_flag = if_else((fuel_type %in% coal_fuels | fuel_code %in% coal_fuels) & 
                               total_fuel_consumption_mmbtu > 0, "Yes", NA_character_)) %>%  # if the fuel type is in coal_fuels, set the flag to "Yes", otherwise NA
  ungroup() %>% 
  group_by(plant_id) %>% 
  summarize(coal_flag = max(coal_flag)) # if any generation comes from coal, flag as a coal plant

plant_file_9 <- 
  plant_file_8 %>% 
  left_join(coal_plants, by = c("plant_id")) 

#### Update coal flag via primary fuel category ----------------

# the coal flag is first assigned through EIA-923 Gen and Fuel data 
# check if plants missing coal flags have a primary_fuel_category == "COAL"
update_coal <- 
  plant_file_9 %>% 
  filter(is.na(coal_flag)) %>% 
  select(plant_id, primary_fuel_category) %>%
  filter(primary_fuel_category == "COAL")

plant_file_10 <- 
  plant_file_9 %>% 
  mutate(coal_flag = if_else(plant_id %in% update_coal$plant_id, "Yes", NA_character_))

### Create combustion flag --------------------

eia_923_combust <- 
  eia_923$generation_and_fuel_data  %>%
  mutate(combustion = if_else(fuel_type %in% combustion_fuels, 1,0)) %>% 
  group_by(plant_id) %>%
  summarize(sum_combustion = sum(combustion, na.rm = TRUE),
            count_combustion = n()) %>%
  mutate(combust_flag = case_when(sum_combustion == 0 ~ 0,
                                  sum_combustion != count_combustion ~ 0.5,
                                  TRUE ~ 1)) %>% 
  select(-sum_combustion, -count_combustion)

plant_file_11 <- 
  plant_file_10 %>% 
  left_join(eia_923_combust, by = c("plant_id"))


### Create pumped storage flag ----------------------------------------

# identify plants with prime mover PS (pumped)
pumped_storage <- 
  generator_file %>% 
  select(plant_id, plant_state, plant_name, prime_mover) %>%
  filter(prime_mover == "PS") %>% distinct()

plant_file_12 <- 
  plant_file_11 %>% 
  mutate(ps_flag = if_else(plant_id %in% pumped_storage$plant_id, "Yes", NA_character_))

# Biomass adjustment -------------

## Biomass Fuels adjustment (CO2) ----------------

# separate out CO2 from biofuels and CO2 from other sources
biomass_fuels <- c("AB",  "BLQ",  "LFG", "MSB","MSW", "OBG", "OBL", "OBS", "PP", "SLW", "WDL", "WDS")

# subtract CO2 from biofuels from the unadj_co2_mass to  create co2_mass
eia_923_biomass <- 
  eia_923$generation_and_fuel_combined %>% 
  select(plant_id, total_fuel_consumption_mmbtu, fuel_type) %>%
  left_join(ef_co2_ch4_n2o, by = c("fuel_type" = "eia_fuel_code")) %>% 
  filter(fuel_type %in% biomass_fuels & total_fuel_consumption_mmbtu > 0) %>%
  mutate(co2_biomass = co2_ef * total_fuel_consumption_mmbtu) %>%
  group_by(plant_id) %>%
  summarize(co2_biomass = sum(co2_biomass, na.rm = TRUE),
            total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) %>%
  mutate(biomass_adj_flag = "Yes")  # add a flag for this adjustment

plant_file_13 <- 
  plant_file_12 %>% 
  left_join(eia_923_biomass, by = c("plant_id"))  %>%
  mutate(co2_mass = unadj_co2_mass - co2_biomass) 

# now update co2_mass with the non-biomass plants
eia_923_non_biomass <- 
  eia_923$generation_and_fuel_combined %>% 
  select(plant_id, total_fuel_consumption_mmbtu, fuel_type) %>%
  left_join(ef_co2_ch4_n2o, by = c("fuel_type" = "eia_fuel_code")) %>% 
  filter(!fuel_type %in% biomass_fuels & total_fuel_consumption_mmbtu > 0) %>%
  mutate(co2_non_biomass = co2_ef * total_fuel_consumption_mmbtu) %>%
  group_by(plant_id) %>%
  summarize(co2_mass = sum(co2_non_biomass, na.rm = TRUE),
            total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) %>%
  filter(co2_mass > 0) 

plant_file_14 <- 
  plant_file_13 %>% 
  rows_patch(eia_923_non_biomass, by = c("plant_id"), unmatched = "ignore")


## Landfill Gas adjustment (NOX, CH4, N2O, SO2) -----------------

eia_923_lfg <- 
  eia_923$generation_and_fuel_combined %>% 
  filter(fuel_type == "LFG" & plant_id != 999999) %>% 
  left_join(ef_co2_ch4_n2o, by = c("fuel_type" = "eia_fuel_code")) %>%
  group_by(plant_id) %>%
  summarize(heat_input_oz_season = sum(tot_mmbtu_may, tot_mmbtu_june, tot_mmbtu_july, tot_mmbtu_august, tot_mmbtu_september, na.rm = TRUE), 
            total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE),
            ch4_ef = paste_concat(ch4_ef, number = TRUE),
            n2o_ef = paste_concat(n2o_ef, number = TRUE)) %>%
  ungroup() %>% 
  mutate(
    nox_biomass = 0.8 * pmax(total_fuel_consumption_mmbtu, 0),
    nox_bio_oz = 0.8 * pmax(heat_input_oz_season, 0),
    ch4_biomass = ch4_ef * total_fuel_consumption_mmbtu,
    n2o_biomass = n2o_ef * total_fuel_consumption_mmbtu,
    so2_biomass = 0.0115 / 2000 * pmax(total_fuel_consumption_mmbtu, 0), 
    biomass_adj_flag1 = "Yes") %>%
  select(-heat_input_oz_season, -total_fuel_consumption_mmbtu, -ch4_ef, -n2o_ef) 
  
# Assign emission values 
# The adjusted emission mass is the same as the unadjusted emission mass if the plant is not a biomass plant
# Otherwise, the adjusted emission mass subtracts the biomass emission mass
plant_file_15 <- 
  plant_file_14 %>% 
  left_join(eia_923_lfg) %>%
  mutate(nox_mass = if_else(!is.na(nox_biomass), pmax(unadj_nox_mass - nox_biomass, 0), unadj_nox_mass), # take the max to avoid negative values
         nox_oz_mass = if_else(!is.na(nox_bio_oz), pmax(unadj_nox_oz_mass - nox_bio_oz, 0), unadj_nox_oz_mass),
         ch4_mass = if_else(!is.na(ch4_biomass), pmax(unadj_ch4_mass - ch4_biomass, 0), unadj_ch4_mass),
         n2o_mass = if_else(!is.na(n2o_biomass), pmax(unadj_n2o_mass - n2o_biomass, 0), unadj_n2o_mass),
         so2_mass = if_else(!is.na(so2_biomass), pmax(unadj_so2_mass - so2_biomass, 0), unadj_so2_mass),
         hg_mass = pmax(unadj_hg_mass, 0), # no biomass variable for hg 
         nox_oz_mass = pmin(nox_oz_mass, nox_mass), # if annual NOx mass is lower than ozone NOx mass, use the annual NOx mass 
         biomass_adj_flag = if_else(biomass_adj_flag == "Yes" | biomass_adj_flag1 == "Yes", "Yes", NA_character_)) %>%
  select(-biomass_adj_flag1) %>% 
  mutate(nox_biomass = pmin(nox_biomass, unadj_nox_mass), # assign the minimum between biomass mass and unadjusted mass to biomass emissions
         nox_bio_oz = pmin(nox_bio_oz, unadj_nox_oz_mass),
         ch4_biomass = pmin(ch4_biomass, unadj_ch4_mass),
         n2o_biomass = pmin(n2o_biomass, unadj_n2o_mass),
         so2_biomass = pmin(so2_biomass, unadj_so2_mass),
         co2_biomass = pmin(co2_biomass, unadj_co2_mass))


# Sum generation by fuel type and plant ID ------------------

# use generator file to summarize generation by fuel type 
ann_gen_by_fuel <- 
  generator_file %>% group_by(fuel_code, plant_id) %>% 
  summarize(ann_gen = sum(generation_ann, na.rm = TRUE),
            ann_gen_oz = sum(generation_oz, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(plant_id) %>%
  summarize(ann_gen = sum(ann_gen, na.rm = TRUE),
            ann_gen_coal = sum(ann_gen[which(fuel_code %in% coal_fuels)], na.rm = TRUE),
            ann_gen_oil = sum(ann_gen[which(fuel_code %in% oil_fuels)], na.rm = TRUE),
            ann_gen_gas = sum(ann_gen[which(fuel_code %in% gas_fuels)], na.rm = TRUE),
            ann_gen_nuclear = sum(ann_gen[which(fuel_code == "NUC")], na.rm = TRUE),
            ann_gen_hydro = sum(ann_gen[which(fuel_code == "WAT")], na.rm = TRUE),
            ann_gen_biomass = sum(ann_gen[which(fuel_code %in% biomass_fuels)], na.rm = TRUE),
            ann_gen_wind = sum(ann_gen[which(fuel_code == "WND")], na.rm = TRUE),
            ann_gen_solar = sum(ann_gen[which(fuel_code == "SUN")], na.rm = TRUE),
            ann_gen_geothermal = sum(ann_gen[which(fuel_code == "GEO")], na.rm = TRUE),
            ann_gen_other_ff = sum(ann_gen[which(fuel_code %in% oth_FF)], na.rm = TRUE),
            ann_gen_other = sum(ann_gen[which(fuel_code %in% oth_fuels)], na.rm = TRUE)) %>% 
  rowwise() %>%
  mutate(ann_gen = if_else(is.na(ann_gen) | 
                          abs(ann_gen - sum(ann_gen_coal, ann_gen_oil, ann_gen_gas, ann_gen_nuclear, 
                                            ann_gen_hydro, ann_gen_biomass, ann_gen_wind, ann_gen_solar,
                                            ann_gen_geothermal, ann_gen_other_ff, ann_gen_other, na.rm = TRUE)) > 2,
                          sum(ann_gen_coal, ann_gen_oil, ann_gen_gas, ann_gen_nuclear, 
                              ann_gen_hydro, ann_gen_biomass, ann_gen_wind, ann_gen_solar,
                              ann_gen_geothermal, ann_gen_other_ff, ann_gen_other, na.rm = TRUE),
                          ann_gen)) %>% 
  replace_na(list(ann_gen = 0, ann_gen_coal = 0, ann_gen_oil = 0, 
                  ann_gen_gas = 0,ann_gen_nuclear = 0,ann_gen_hydro = 0,ann_gen_biomass = 0,
                  ann_gen_wind = 0,ann_gen_solar = 0,ann_gen_geothermal = 0 ,ann_gen_other_ff = 0, 
                  ann_gen_other = 0))

## Calculate resource mix generation by fuel type and % resource mix by fuel type ------------

# calculate generation by non-renewables, renewables (and non-hydro renewables), combustion fuels, and non-combustion fuels
ann_gen_by_fuel_2 <- 
  ann_gen_by_fuel %>% 
  mutate(ann_gen_non_renew = ann_gen_coal + ann_gen_oil + ann_gen_gas + ann_gen_other_ff + ann_gen_nuclear + ann_gen_other,
         ann_gen_renew = ann_gen_biomass + ann_gen_wind + ann_gen_solar + ann_gen_geothermal + ann_gen_hydro,
         ann_gen_renew_nonhydro = ann_gen_biomass + ann_gen_wind + ann_gen_solar + ann_gen_geothermal ,
         ann_gen_combust = ann_gen_coal + ann_gen_oil + ann_gen_gas + ann_gen_other_ff + ann_gen_biomass + ann_gen_other,
         ann_gen_non_combust = ann_gen_nuclear + ann_gen_wind + ann_gen_solar + ann_gen_geothermal + ann_gen_hydro)

ann_gen_by_fuel_3 <- 
  ann_gen_by_fuel_2 %>% 
  mutate(perc_ann_gen_coal = 100 * ann_gen_coal / ann_gen,
         perc_ann_gen_oil = 100 * ann_gen_oil / ann_gen,
         perc_ann_gen_gas = 100 * ann_gen_gas / ann_gen,
         perc_ann_gen_nuclear = 100 * ann_gen_nuclear / ann_gen,
         perc_ann_gen_hydro = 100 * ann_gen_hydro / ann_gen,
         perc_ann_gen_biomass = 100 * ann_gen_biomass / ann_gen,
         perc_ann_gen_wind = 100 * ann_gen_wind / ann_gen,
         perc_ann_gen_solar = 100 * ann_gen_solar / ann_gen,
         perc_ann_gen_geothermal = 100 * ann_gen_geothermal / ann_gen,
         perc_ann_gen_solar = 100 * ann_gen_solar / ann_gen,
         perc_ann_gen_other_ff = 100 * ann_gen_other_ff / ann_gen,
         perc_ann_gen_other = 100 * ann_gen_other / ann_gen,
         perc_ann_gen_non_renew = 100 * ann_gen_non_renew / ann_gen,
         perc_ann_gen_renew = 100 * ann_gen_renew / ann_gen,
         perc_ann_gen_renew_nonhydro = 100 * ann_gen_renew_nonhydro / ann_gen,
         perc_ann_gen_combust = 100 * ann_gen_combust / ann_gen,
         perc_ann_gen_non_combust = 100 * ann_gen_combust / ann_gen)

# checks for negative generations
stopifnot(sum(isTRUE(as.matrix(ann_gen_by_fuel) < 0), na.rm = TRUE) == 0)

plant_file_16 <- 
  plant_file_15 %>% 
  left_join(ann_gen_by_fuel_3) %>% 
  mutate(primary_fuel_type = if_else(perc_ann_gen_nuclear > 50, "NUC", primary_fuel_type),
         primary_fuel_category = if_else(perc_ann_gen_nuclear > 50, "NUCLEAR", primary_fuel_category))

# there is some weird v small differences, but rounding fixes
# checks the sum of all = 100
stopifnot(all(100 == round(plant_file_16$perc_ann_gen_renew + plant_file_16$perc_ann_gen_non_renew, 0) |
      is.na(round(plant_file_16$perc_ann_gen_renew + plant_file_16$perc_ann_gen_non_renew, 0))))
      

# CHP plants ---------------
## Calculate useful thermal output, power heat ratio , and electric allocation factor ---------------------

# sum total fuel consumption and electric fuel consumption to the plant level
eia_923_thermal_output <- 
  eia_923$generation_and_fuel_combined %>% 
  group_by(plant_id) %>%
  summarize(total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE),
            elec_fuel_consumption_mmbtu = sum(elec_fuel_consumption_mmbtu, na.rm = TRUE))

chp <- 
  read_csv("data/static_tables/chp_plants.csv") %>% janitor::clean_names() %>% 
  filter(total > 0) %>% 
  mutate(plant_id = as.character(plant_code)) %>%
  left_join(eia_923_thermal_output) %>%
  mutate(useful_thermal_output = 0.8 * (total_fuel_consumption_mmbtu - elec_fuel_consumption_mmbtu), # calculate useful thermal output
         chp_flag = "Yes") %>% # assign CHP flags to all plants in this object
  select(plant_id, useful_thermal_output, chp_flag) %>% 
  left_join(plant_file_11 %>% select(plant_id, generation_ann), by = c("plant_id")) %>% 
  mutate(power_to_heat = if_else(useful_thermal_output != 0, 
                                 3.413 * generation_ann / useful_thermal_output, # calculate power to heat ratio
                                 NA_real_), 
         elec_allocation = if_else(useful_thermal_output != 0, # calculate electric allocation
                                   3.413 * generation_ann / (0.75 * useful_thermal_output + 3.413 * generation_ann), 
                                   1), 
         elec_allocation = if_else(elec_allocation < 0, 0, elec_allocation), 
         elec_allocation = if_else(elec_allocation > 1, 1, elec_allocation)) %>% 
  select(-generation_ann)

plant_file_17 <- 
  plant_file_16 %>% 
  left_join(chp, by = c("plant_id")) 


## CHP adjustment  ------------------

plant_chp <- 
  plant_file_17 %>% 
  filter(chp_flag == "Yes") %>% 
  mutate(# calculate adjusted values 
         heat_input = (elec_allocation * unadj_combust_heat_input) + (unadj_heat_input - unadj_combust_heat_input),
         heat_input_oz = (elec_allocation * unadj_combust_heat_input_oz) + (unadj_heat_input_oz - unadj_combust_heat_input_oz),
         combust_heat_input = elec_allocation * unadj_combust_heat_input,
         combust_heat_input_oz = elec_allocation * unadj_combust_heat_input_oz, 
         nox_mass = elec_allocation * nox_mass,
         nox_oz_mass = elec_allocation * nox_oz_mass, 
         so2_mass = elec_allocation * so2_mass, 
         co2_mass =  elec_allocation * co2_mass, 
         ch4_mass = elec_allocation * ch4_mass, 
         n2o_mass =  elec_allocation * n2o_mass, 
         # calculate CHP adjustment values
         chp_combust_heat_input = unadj_combust_heat_input - combust_heat_input,
         chp_combust_heat_input_oz = unadj_combust_heat_input_oz - combust_heat_input_oz,
         chp_nox = nox_mass / elec_allocation - nox_mass, 
         chp_nox_oz = nox_oz_mass / elec_allocation - nox_oz_mass,
         chp_so2 = so2_mass / elec_allocation - so2_mass,
         chp_co2 = co2_mass / elec_allocation - co2_mass,
         chp_ch4 = ch4_mass / elec_allocation - ch4_mass,
         chp_n2o = n2o_mass / elec_allocation - n2o_mass, 
         # check if CHP emission masses are greater than unadjusted values, and assign unadjusted values if TRUE
         chp_nox = if_else(chp_nox > unadj_nox_mass | chp_nox < 0, unadj_nox_mass, chp_nox),
         chp_nox_oz = if_else(chp_nox_oz > unadj_nox_oz_mass | chp_nox_oz < 0, unadj_nox_oz_mass, chp_nox_oz),
         chp_so2 = if_else(chp_so2 > unadj_so2_mass| chp_so2 < 0, unadj_so2_mass, chp_so2),
         chp_co2 = if_else(chp_co2 > unadj_co2_mass| chp_co2 < 0, unadj_co2_mass, chp_co2),
         chp_ch4 = if_else(chp_ch4 > unadj_ch4_mass| chp_ch4 < 0, unadj_ch4_mass, chp_ch4),
         chp_n2o = if_else(chp_n2o > unadj_n2o_mass| chp_n2o < 0, unadj_n2o_mass, chp_n2o), 
         # calculate nominal heat rate
         nominal_heat_rate = combust_heat_input * 1000 / ann_gen_combust)

plant_file_18 <- 
  plant_file_17 %>% 
  filter(is.na(chp_flag)) %>% # filter out CHP flags to easily join in new CHP data
  full_join(plant_chp) %>% 
  mutate(power_heat_ratio = if_else(combust_flag == 1 | combust_flag == 0.5, nominal_heat_rate, NA_real_), 
         # assign adjusted values to non-CHP plants 
         heat_input = if_else(is.na(heat_input), unadj_heat_input, heat_input), 
         heat_input_oz = if_else(is.na(heat_input_oz), unadj_heat_input_oz, heat_input_oz), 
         combust_heat_input = if_else(is.na(combust_heat_input), unadj_combust_heat_input, combust_heat_input), 
         combust_heat_input_oz = if_else(is.na(combust_heat_input_oz), unadj_combust_heat_input_oz, combust_heat_input_oz))





# Calculate CO2 equivalent and update negative emissions values ----------------------------------------

# CO2 equivalent adds in other greenhouse gas emission masses 
# CH4 and N2O are multiplied by their associated global warming potential value (25 and 298 respectively)
plant_file_19 <- 
  plant_file_18 %>% 
  mutate(co2e_mass = 
             if_else(is.na(co2_mass), 0, co2_mass) + 
             if_else(is.na(ch4_mass), 0, 25 * ch4_mass / 2000) + 
             if_else(is.na(n2o_mass), 0, 298 * n2o_mass / 2000), 
         # if emission mass is less than 0, re-assign as 0 
         co2_mass = if_else(co2_mass < 0, 0, co2_mass),
         ch4_mass = if_else(ch4_mass < 0, 0, ch4_mass),
         n2o_mass = if_else(n2o_mass < 0, 0, n2o_mass),
         co2e_mass = if_else(co2e_mass < 0, 0, co2e_mass))


# Calculate emissions rates  --------------------
### Combustion output emissions rates ----------------------------------------

plant_file_20 <- 
  plant_file_19 %>% 
  mutate(nox_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * nox_mass / ann_gen_combust),
         nox_combust_out_emission_rate_oz = if_else(ann_gen_combust * (generation_oz / generation_ann) < 0, 0, 
                                                    2000 * nox_oz_mass / (ann_gen_combust * (generation_oz / generation_ann))),
         so2_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * so2_mass / ann_gen_combust),
         co2_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * co2_mass / ann_gen_combust),
         ch4_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * ch4_mass / ann_gen_combust),
         n2o_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * n2o_mass / ann_gen_combust),
         co2e_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * co2e_mass / ann_gen_combust),
         hg_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0,2000 * hg_mass / ann_gen_combust)  )


### Input emission rates ----------------------------------------
  
plant_file_21 <- 
  plant_file_20 %>% 
  mutate(nox_in_emission_rate = 2000 * nox_mass / heat_input,
         nox_in_emission_rate_oz = 2000 * nox_oz_mass / heat_input_oz,
         so2_in_emission_rate = 2000 * so2_mass / heat_input,
         co2_in_emission_rate = 2000 * co2_mass / heat_input,
         ch4_in_emission_rate = 2000 * ch4_mass / heat_input,
         n2o_in_emission_rate = 2000 *  n2o_mass / heat_input,
         co2e_in_emission_rate = 2000 * co2e_mass / heat_input,
         hg_in_emission_rate = 2000 * hg_mass / heat_input)  

### Output emission rates ----------------------------------------

plant_file_22 <- 
  plant_file_21 %>% 
  mutate(nox_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * nox_mass / generation_ann),
         nox_out_emission_rate_oz =if_else(generation_oz < 0, 0,  2000 * nox_oz_mass / generation_oz),
         so2_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * so2_mass / generation_ann),
         co2_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * co2_mass / generation_ann),
         ch4_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * ch4_mass / generation_ann),
         n2o_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * n2o_mass / generation_ann),
         co2e_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * co2e_mass / generation_ann),
         hg_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * hg_mass / generation_ann))

# Calculate nonbaseload factor ---------------------------------

plant_file_23 <- 
  plant_file_22 %>% 
  mutate(nonbaseload = if_else(!primary_fuel_type %in% c("GEO", "MWH", "NUC", "PUR", "SUN", "WAT", "WND"), 
                               case_when(capfac < 0.2 ~ 1,
                                         capfac > 0.8 ~ 0,
                                         capfac == 0 ~ NA_real_,
                                         TRUE ~ (-5/3 * capfac) + 4/3),
                                         NA_real_))

# Update source columns ------------------------------

# this function simplifies the source and removes any duplicate sources
update_source <- function(x, unit_f){
  str_col <- x
  x <- as.name(x)
  
  # update sources in unit file
  unit_source <- unit_f %>% select(plant_id, !!x) %>% group_by(plant_id, !!x) %>% filter(!is.na(!!x)) %>%
    mutate(source_update = case_when(!!x == "EIA Unit-level Data" ~ "EIA",
                                     !!x == "EIA Unit-Level Data" ~ "EIA",
                                     !!x == "EIA Prime Mover-level Data" ~ "EIA",
                                     !!x == "EPA/CAMD" ~ "EPA/CAMD", 
                                     !!x == "EIA non-ozone season distributed and EPA/CAMD ozone season" ~ "EPA/CAMD; EIA"))
  
  # identify unique plant ID source updates
  unit_source <- unit_source %>% ungroup() %>%
    select(plant_id, source_update) %>%
    unique
  
  # identify which plant_ids have multiple sources and pull their plant ID
  ids <- unit_source$plant_id[which(duplicated(unit_source$plant_id))] %>% unique()
  
  # if plant_ids have multiple sources, assign "EPA/CAMD; EIA"
  unit_source <- 
    unit_source %>% 
    mutate(source_update = if_else(plant_id %in% ids, "EPA/CAMD; EIA", source_update)) 
    unique # take unique again to remove duplicates
    
  colnames(unit_source) <- c("plant_id", str_col)
  
  return(unique(unit_source))
}

check_list <- c("EIA" ,"EPA/CAMD", NA_character_, "EPA/CAMD; EIA")


### Update heat_input_source ----------

unit_source_heat_input <- 
  update_source(x = "heat_input_source", unit_f = unit_file) %>% 
  rename(unadj_heat_input_source = heat_input_source) # rename to match plant file
  
# check if sources updated correctly, stop if not
stopifnot(all(unit_source_heat_input$unadj_heat_input_source %in% check_list))


### Update heat_input_oz_source ----------

unit_source_heat_input_oz <- 
  update_source(x = "heat_input_oz_source", unit_f = unit_file) %>% 
  rename(unadj_heat_input_oz_source = heat_input_oz_source) # rename to match plant file

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_heat_input_oz$unadj_heat_input_oz_source %in% check_list))


### Update nox_source ---------

unit_source_nox <- update_source(x = "nox_source", unit_f = unit_file) %>% 
  rename(unadj_nox_source = nox_source)

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_nox$unadj_nox_source %in% check_list))


### Update nox_oz_source --------

unit_source_nox_oz <- 
  update_source(x = "nox_oz_source", unit_f = unit_file) %>% 
  rename(unadj_nox_oz_source = nox_oz_source) # rename to match plant file

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_nox_oz$unadj_nox_oz_source %in% check_list))


### Update co2_source ---------

unit_source_co2 <- 
  update_source(x = "co2_source", unit_f = unit_file) %>% 
  rename(unadj_co2_source = co2_source) # rename to match plant file

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_co2$unadj_co2_source %in% check_list))


### Update so2_source ----------

unit_source_so2 <- 
  update_source(x = "so2_source", unit_f = unit_file) %>% 
  rename(unadj_so2_source = so2_source) # rename to match plant file

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_so2$unadj_so2_source %in% check_list))

### Update plant file with source updates -------------

plant_file_24 <- 
  plant_file_23 %>% 
  rows_update(unit_source_heat_input, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_heat_input_oz, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_nox, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_nox_oz, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_co2, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(unit_source_so2, by = c("plant_id"), unmatched = "ignore") %>% 
  mutate(unadj_ch4_source = if_else(!is.na(ch4_mass), "EIA", NA_character_), 
         unadj_n2o_source = if_else(!is.na(n2o_mass), "EIA", NA_character_))

# Final modifications --------------------
### Round columns -------------------------

plant_file_25 <- 
  plant_file_24 %>% 
  mutate(capfac = round(capfac, 5), # capacity factor
         power_heat_ratio = round(power_heat_ratio, 3), #CHP power heat ration
         elec_allocation = round(elec_allocation, 6), # CHP electric allocation
         # heat input
         combust_heat_input = round(combust_heat_input, 3),
         combust_heat_input_oz = round(combust_heat_input_oz, 3), 
         heat_input = round(heat_input, 3),
         heat_input_oz = round(heat_input_oz, 3),
         # annual generation
         generation_ann = round(generation_ann, 3), 
         generation_oz = round(generation_oz, 3), 
         # emissions mass
         nox_mass = round(nox_mass, 3), 
         nox_oz_mass = round(nox_oz_mass, 3),
         so2_mass = round(so2_mass, 3),
         co2_mass = round(co2_mass, 3),
         ch4_mass = round(ch4_mass, 3),
         n2o_mass = round(n2o_mass, 3),
         co2e_mass = round(co2e_mass, 3),
         hg_mass = round(hg_mass, 6),
         # output emissions rates
         nox_out_emission_rate = round(nox_out_emission_rate, 3), 
         nox_out_emission_rate_oz = round(nox_out_emission_rate_oz, 3),
         so2_out_emission_rate = round(so2_out_emission_rate, 3),
         co2_out_emission_rate = round(co2_out_emission_rate, 3),
         ch4_out_emission_rate = round(ch4_out_emission_rate, 3),
         n2o_out_emission_rate = round(n2o_out_emission_rate, 3),
         co2e_out_emission_rate = round(co2e_out_emission_rate, 3),
         hg_out_emission_rate = round(hg_out_emission_rate, 6),
         # input emissions rates
         nox_in_emission_rate = round(nox_in_emission_rate, 3), 
         nox_in_emission_rate_oz = round(nox_in_emission_rate_oz, 3),
         so2_in_emission_rate = round(so2_in_emission_rate, 3),
         co2_in_emission_rate = round(co2_in_emission_rate, 3),
         ch4_in_emission_rate = round(ch4_in_emission_rate, 3),
         n2o_in_emission_rate = round(n2o_in_emission_rate, 3),
         co2e_in_emission_rate = round(co2e_in_emission_rate, 3),
         hg_in_emission_rate = round(hg_in_emission_rate, 6),
         # combustion output emissions rates
         nox_combust_out_emission_rate = round(nox_combust_out_emission_rate, 3), 
         nox_combust_out_emission_rate_oz = round(nox_combust_out_emission_rate_oz, 3),
         so2_combust_out_emission_rate = round(so2_combust_out_emission_rate, 3),
         co2_combust_out_emission_rate = round(co2_combust_out_emission_rate, 3),
         ch4_combust_out_emission_rate = round(ch4_combust_out_emission_rate, 3),
         n2o_combust_out_emission_rate = round(n2o_combust_out_emission_rate, 3),
         co2e_combust_out_emission_rate = round(co2e_combust_out_emission_rate, 3),
         hg_combust_out_emission_rate = round(hg_combust_out_emission_rate, 6),
         # undjusted emission mass
         unadj_nox_mass = round(unadj_nox_mass, 3), 
         unadj_nox_oz_mass = round(unadj_nox_oz_mass, 3),
         unadj_so2_mass = round(unadj_so2_mass, 3),
         unadj_co2_mass = round(unadj_co2_mass, 3),
         unadj_ch4_mass = round(unadj_ch4_mass, 3),
         unadj_n2o_mass = round(unadj_n2o_mass, 3),
         # no co2e_mass unadjusted mass
         unadj_hg_mass = round(unadj_hg_mass, 6),
         # unadjusted heat input
         unadj_combust_heat_input = round(unadj_combust_heat_input, 3),
         unadj_combust_heat_input_oz = round( unadj_combust_heat_input_oz, 3), 
         unadj_heat_input = round( unadj_heat_input, 3),
         unadj_heat_input_oz = round( unadj_heat_input_oz, 3),
         # nominal heat rate
         nominal_heat_rate = round(nominal_heat_rate, 6),
         # annual generation (by fuel)
         ann_gen_coal = round(ann_gen_coal, 3),
         ann_gen_oil = round(ann_gen_oil, 3),
         ann_gen_gas = round(ann_gen_gas, 3),
         ann_gen_combust = round(ann_gen_combust, 3),
         ann_gen_hydro = round(ann_gen_hydro, 3),
         ann_gen_biomass = round(ann_gen_biomass, 3),
         ann_gen_wind = round(ann_gen_wind, 3),
         ann_gen_solar = round(ann_gen_solar, 3),
         ann_gen_geothermal = round(ann_gen_geothermal, 3),
         ann_gen_other_ff = round(ann_gen_other_ff, 3),
         ann_gen_other = round(ann_gen_other, 3),
         ann_gen_non_renew = round(ann_gen_non_renew, 3),
         ann_gen_renew = round(ann_gen_renew, 3),
         ann_gen_non_renew = round(ann_gen_non_renew, 3),
         ann_gen_renew_nonhydro = round(ann_gen_renew_nonhydro, 3),
         ann_gen_combust = round(ann_gen_combust, 3),
         ann_gen_non_combust = round(ann_gen_non_combust, 3),
         # biomass 
         nox_biomass = round(nox_biomass, 3), 
         nox_bio_oz = round(nox_bio_oz, 3),
         so2_biomass = round(so2_biomass, 3),
         co2_biomass = round(co2_biomass, 3),
         ch4_biomass = round(ch4_biomass, 3),
         n2o_biomass = round(n2o_biomass, 3),
         # no co2 equivalent biomass
         # no hg biomass
         # CHP emission mass
         chp_nox = round(chp_nox, 3), 
         chp_nox_oz = round(chp_nox_oz, 3),
         chp_so2 = round(chp_so2, 3),
         chp_co2 = round(chp_co2, 3),
         chp_ch4 = round(chp_ch4, 3),
         chp_n2o = round(chp_n2o, 3),
         # no co2 equivalent CHP
         # no hg CHP
         # useful_thermal_output
         useful_thermal_output = round(useful_thermal_output,3)
         ) 


# Format plant file --------------

# creating named vector of final variable order and variable name included in plant file
final_vars <-
  c("SEQUNT" = "seqplt",
    "YEAR" = "year",
    "PSTATABB" = "plant_state",
    "PNAME" = "plant_name",
    "ORISPL" = "plant_id",
    "OPRNAME" = "system_owner", 
    "OPRCODE" = "system_owner_id", 
    "UTLSRVNM" = "utility_name", 
    "UTLSRVID" = "utility_id", 
    "SECTOR" = "sector_name", 
    "BANAME" = "ba_name", 
    "BACODE" = "ba_code", 
    "NERC" = "nerc", 
    "SUBRGN" = "egrid_subregion", 
    "SRNAME" = "egrid_subregion_name", 
    "ISORTO" = "isorto", 
    "FIPSST" = "fips_state_code", 
    "FIPSCNTY" = "fips_county_code", 
    "CNTYNAME" = "county",
    "LAT" = "lat", 
    "LON" = "lon", 
    "CAMDFLAG" = "camd_flag", 
    "NUMUNT" = "num_units", 
    "NUMGEN" = "num_gen", 
    "PLPRMFL" = "primary_fuel_type", 
    "PLFUELCT" = "primary_fuel_category", 
    "COALFLAG" = "coal_flag", 
    "CAPFAC" = "capfac", 
    "NAMEPCAP" = "nameplate_capacity", 
    "NBFACTOR" = "nonbaseload", 
    "RMBMFLAG" = "biomass_adj_flag", 
    "CHPFLAG" = "chp_flag", 
    "USETHRMO" = "useful_thermal_output", 
    "PWRTOHT" = "power_heat_ratio", 
    "ELCALLOC" = "elec_allocation", 
    "PSFLAG" = "ps_flag", 
    "PLHTIAN" = "combust_heat_input", 
    "PLHTIOZ" = "combust_heat_input_oz", 
    "PLHTIANT" = "heat_input", 
    "PLHTIOZT" = "heat_input_oz", 
    "PLNGENAN" = "generation_ann", 
    "PLNGENOZ" = "generation_oz", 
    "PLNOXAN" = "nox_mass", 
    "PLNOXOZ" = "nox_oz_mass", 
    "PLSO2AN" = "so2_mass", 
    "PLCO2AN" = "co2_mass", 
    "PLCH4AN" = "ch4_mass", 
    "PLN2OAN" = "n2o_mass", 
    "PLCO2EQA" = "co2e_mass", 
    "PLHGAN" = "hg_mass", 
    "PLNOXRTA" = "nox_out_emission_rate", 
    "PLNOXRTO" = "nox_out_emission_rate_oz", 
    "PLSO2RTA" = "so2_out_emission_rate", 
    "PLCO2RTA" = "co2_out_emission_rate", 
    "PLCH4RTA" = "ch4_out_emission_rate", 
    "PLN2ORTA" = "n2o_out_emission_rate", 
    "PLC2ERTA" = "co2e_out_emission_rate", 
    "PLHGRTA" = "hg_out_emission_rate", 
    "PLNOXRA" = "nox_in_emission_rate", 
    "PLNOXRO" = "nox_in_emission_rate_oz", 
    "PLSO2RA" = "so2_in_emission_rate", 
    "PLCO2RA" = "co2_in_emission_rate", 
    "PLCH4RA" = "ch4_in_emission_rate", 
    "PLN2ORA" = "n2o_in_emission_rate", 
    "PLC2ERA" = "co2e_in_emission_rate", 
    "PLHGRA" = "hg_in_emission_rate", 
    "PLNOXCRT" = "nox_combust_out_emission_rate", 
    "PLNOXCRO" = "nox_combust_out_emission_rate_oz", 
    "PLSO2CRT" = "so2_combust_out_emission_rate", 
    "PLCO2CRT" = "co2_combust_out_emission_rate", 
    "PLCH4CRT" = "ch4_combust_out_emission_rate", 
    "PLN2OCRT" = "n2o_combust_out_emission_rate", 
    "PLC2ERT" = "co2e_combust_out_emission_rate", 
    "PLHGCRT" = "hg_combust_out_emission_rate", 
    "UNNOX" = "unadj_nox_mass", 
    "UNNOXOZ" = "unadj_nox_oz_mass", 
    "UNSO2" = "unadj_so2_mass", 
    "UNCO2" = "unadj_co2_mass", 
    "UNCH4" = "unadj_ch4_mass", 
    "UNN2O" = "unadj_n2o_mass", 
    "UNHG" = "unadj_hg_mass", 
    "UNHTI" = "unadj_combust_heat_input", 
    "UNHTIOZ" = "unadj_combust_heat_input_oz", 
    "UNHTIT" = "unadj_heat_input", 
    "UNHTIOZT" = "unadj_heat_input_oz", 
    "UNNOXSRC" = "unadj_nox_source", 
    "UNNOZSRC" = "unadj_nox_oz_source", 
    "UNSO2SRC" = "unadj_so2_source", 
    "UNCO2SRC" = "unadj_co2_source", 
    "UNCH4SRC" = "unadj_ch4_source", 
    "UNN2OSRC" = "unadj_n2o_source", 
    "UNHGSRC" = "unadj_hg_source", 
    "UNHTISRC" = "unadj_heat_input_source", 
    "UNHOZSRC" = "unadj_heat_input_oz_source", 
    "BIONOX" = "nox_biomass", 
    "BIONOXOZ" = "nox_bio_oz", 
    "BIOSO2" = "so2_biomass", 
    "BIOCO2" = "co2_biomass", 
    "BIOCH4" = "ch4_biomass", 
    "BION2O" = "n2o_biomass", 
    "CHPCHTI" = "chp_combust_heat_input", 
    "CHPCHTIOZ" = "chp_combust_heat_input_oz", 
    "CHPNOX" = "chp_nox", 
    "CHPNOXOZ" = "chp_nox_oz", 
    "CHPSO2" = "chp_so2", 
    "CHPCH4" = "chp_ch4", 
    "CHPN2O" = "chp_n2o", 
    "PLHTRT" = "nominal_heat_rate", 
    "PLGENACL" = "ann_gen_coal", 
    "PLGENAOL" = "ann_gen_oil", 
    "PLGENAGS" = "ann_gen_gas", 
    "PLGENANC" = "ann_gen_nuclear", 
    "PLGENAHY" = "ann_gen_hydro", 
    "PLGENABM" = "ann_gen_biomass", 
    "PLGENAWI" = "ann_gen_wind", 
    "PLGENASO" = "ann_gen_solar", 
    "PLGENAGT" = "ann_gen_geothermal", 
    "PLGENAOF" = "ann_gen_other_ff", 
    "PLGENAOP" = "ann_gen_other", 
    "PLGENATN" = "ann_gen_non_renew", 
    "PLGENATR" = "ann_gen_renew", 
    "PLGENATH" = "ann_gen_renew_nonhydro", 
    "PLGENACY" = "ann_gen_combust", 
    "PLGENACN" = "ann_gen_non_combust", 
    "PLCLPR" = "perc_ann_gen_coal", 
    "PLOLPR" = "perc_ann_gen_oil", 
    "PLGSPR" = "perc_ann_gen_gas", 
    "PLNCPR" = "perc_ann_gen_nuclear", 
    "PLHYPR" = "perc_ann_gen_hydro", 
    "PLBMPR" = "perc_ann_gen_biomass", 
    "PLWIPR" = "perc_ann_gen_wind", 
    "PLSOPR" = "perc_ann_gen_solar", 
    "PLGTPR" = "perc_ann_gen_geothermal", 
    "PLOFPR" = "perc_ann_gen_other_ff", 
    "PLOPPR" = "perc_ann_gen_other", 
    "PLTNPR" = "perc_ann_gen_non_renew", 
    "PLTRPR" = "perc_ann_gen_renew", 
    "PLTHPR" = "perc_ann_gen_renew_nonhydro", 
    "PLCYPR" = "perc_ann_gen_combust", 
    "PLCNPR" = "perc_ann_gen_non_combust")

plant_formatted <-
  plant_file_25 %>%
  arrange(plant_state, plant_name) %>% 
  mutate(seqplt = row_number(), 
         year = params$eGRID_year) %>% 
  select(as_tibble(final_vars)$value) # keeping columns with tidy names for QA steps


# Export plant file -------------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving unit file to folder data/outputs/")

write_rds(plant_formatted, "data/outputs/plant_file.RDS")
