
# load required libraries --------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
#library(here)

# Load necessary data ----------

## eia ------------

eia_860 <- read_rds("data/clean_data/eia/eia_860_clean.RDS")
eia_861 <- read_rds("data/clean_data/eia/eia_861_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia/eia_923_clean.RDS")
# note: eia-923 generation and fuel = generation_and_fuel_combined

## lower-level eGRID files ------------
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

#source(here("scripts", "unit_rename.r"))
#unit_file <- unit_rename(unit_file, alt = TRUE)
#library(tidyverse)

# 0. create a file to string concatenate unique values that are not NA ----------

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


# 1. aggregate unit file to plant level ------------------------

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

# 2. aggregate gen file to plant level----------------------------

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

# 3. Pull in info from the EIA-860 Plant file  -------------------------------

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


# 4.	Combustion heat input ---------------------------------------

unit_heat_input <- 
  unit_file %>% 
  group_by(primary_fuel_type, prime_mover, plant_id) %>%
  summarize(unadj_heat_input = sum(heat_input, na.rm = TRUE),
            unadj_heat_input_oz = sum(heat_input_oz, na.rm = TRUE)) %>% 
  ungroup()

combustion_fuels <- c("AB","ANT", "BFG","BIT","BLQ","COG","DFO","JF","KER","LFG","LIG",
                      "MSB","MSN","MSW","NG","OBG","OBL","OBS","OG","PC","PG","RC","RFO",
                      "SGC","SGP","SLW","SUB","TDF","WC","WDL","WDS","WO") 

# calculate combustion heat input

combust_heat_input <- 
  unit_heat_input %>% 
  filter(primary_fuel_type %in% combustion_fuels ) %>%
  group_by(plant_id) %>% 
  summarize(unadj_combust_heat_input = sum(unadj_heat_input, na.rm = TRUE), 
            unadj_combust_heat_input_oz = sum(unadj_heat_input_oz, na.rm = TRUE)) %>% 
  ungroup()

# join with aggregated unit file

plant_unit_2 <- 
  plant_unit %>% 
  left_join(combust_heat_input)  


# 5.	Update capacity factor  -----------------------------------------------

plant_gen_3 <- 
  plant_gen_2 %>% 
  mutate(capfac = if_else(generation_ann / (nameplate_capacity * 8760) < 0, 0, 
                          generation_ann / (nameplate_capacity * 8760)))

# 6.	Calculate CH4 emissions & 7.	Calculate N2O emissions  ----------------------------------------

# load static table with emissions factors
ef_ch4_n2o <- 
  read_csv("data/static_tables/co2_ch4_n2o_ef.csv") %>% 
  filter(!is.na(eia_fuel_code)) %>% 
  select(eia_fuel_code, ch4_ef, n2o_ef)
    
#if(any((!plant_gen_3$fuel_code %in% ef_ch4_n2o$eia_fuel_code) & !is.na(plant_gen_3$fuel_code))) { 
#  print(paste0(distinct(plant_gen_3$fuel_code[which(!plant_gen_3$fuel_code %in% ef_ch4_n2o$eia_fuel_code& !is.na(plant_gen_3$fuel_code))]), " is not in emissions data!"))
#}

# join with eia to calculate at plant level
    
emissions_ch4_n2o <- 
  eia_923$generation_and_fuel_combined %>% 
  filter(prime_mover != "FC") %>% 
  select(plant_id, total_fuel_consumption_mmbtu, fuel_type) %>%
  left_join(ef_ch4_n2o, by = c("fuel_type" = "eia_fuel_code")) %>%
  mutate(unadj_ch4_mass = ch4_ef * total_fuel_consumption_mmbtu,
         unadj_n2o_mass = n2o_ef * total_fuel_consumption_mmbtu) %>%
  group_by(plant_id) %>%
  summarize(unadj_ch4_mass = sum(unadj_ch4_mass, na.rm = TRUE),
            unadj_n2o_mass = sum(unadj_n2o_mass, na.rm = TRUE),
            ch4_source = "EIA",
            n2o_source = "EIA") %>% 
  ungroup()

# combine generator and unit aggregation files into one plant file
plant_file <- 
  plant_gen_3 %>% 
  select(-plant_name) %>% # names differ between sources, so use names in unit file
  full_join(plant_unit_2, by = c("plant_id", "plant_state", "year")) %>% 
  left_join(emissions_ch4_n2o)

# 8.	Add in FIPS codes for state and county ----------------

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


# 9.	Coal flag -----------------

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
                               total_fuel_consumption_mmbtu > 0, 1, 0)) %>%  # if the fuel type is in coal_fuels, set the flag to 1, otherwise 0
  ungroup() %>% 
  group_by(plant_id) %>% 
  summarize(coal_flag = max(coal_flag)) # if any generation comes from coal, flag as a coal plant

plant_file_3 <- 
  plant_file_2 %>% 
  left_join(coal_plants, by = c(plant_id))

# 10.	Create combustion flag --------------------

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

plant_file_4 <- 
  plant_file_3 %>% 
  left_join(eia_923_combust, by = c("plant_id"))


# 11.	Biomass adjustment -------------
## Biomass Fuels adjustment (CO2) ----------------

ef_co2_ch4_n2o <- 
  read_csv("data/static_tables/co2_ch4_n2o_ef.csv") %>% 
  filter(!is.na(eia_fuel_code)) 

# seperate out co2 from biofuels and co2 from other sources

biomass_fuels <- c("AB",  "BLQ",  "LFG", "MSB","MSW", "OBG", "OBL", "OBS", "PP", "SLW", "WDL", "WDS")

# subtract co2 from biofuels from the unadj_co2_mass to  create co2_mass

eia_923_biomass <- 
  eia_923$generation_and_fuel_combined %>% 
  select(plant_id, total_fuel_consumption_mmbtu, fuel_type) %>%
  left_join(ef_co2_ch4_n2o, by = c("fuel_type" = "eia_fuel_code")) %>% 
  filter(fuel_type %in% biomass_fuels & total_fuel_consumption_mmbtu > 0) %>%
  mutate(co2_biomass = co2_ef * total_fuel_consumption_mmbtu) %>%
  group_by(plant_id) %>%
  summarize(co2_biomass = sum(co2_biomass, na.rm = TRUE),
            total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) %>%
  mutate(biomass_adj_flag = 1)  # add a flag for this adjustment

plant_file_5 <- 
  plant_file_4 %>% 
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

plant_file_6 <- 
  plant_file_5 %>% 
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
    biomass_adj_flag1 = 1) %>%
  select(-heat_input_oz_season, -total_fuel_consumption_mmbtu, -ch4_ef, -n2o_ef) 
  
# Assign emission values 
# The adjusted emission mass is the same as the unadjusted emission mass if the plant is not a biomass plant
# Otherwise, the adjusted emission mass subtracts the biomass emission mass
plant_file_7 <- 
  plant_file_6 %>% 
  left_join(eia_923_lfg) %>%
  mutate(nox_mass = if_else(!is.na(nox_biomass), pmax(unadj_nox_mass - nox_biomass, 0), unadj_nox_mass), # take the max to avoid negative values
         nox_oz_mass = if_else(!is.na(nox_bio_oz), pmax(unadj_nox_oz_mass - nox_bio_oz, 0), unadj_nox_oz_mass),
         ch4_mass = if_else(!is.na(ch4_biomass), pmax(unadj_ch4_mass - ch4_biomass, 0), unadj_ch4_mass),
         n2o_mass = if_else(!is.na(n2o_biomass), pmax(unadj_n2o_mass - n2o_biomass, 0), unadj_n2o_mass),
         so2_mass = if_else(!is.na(so2_biomass), pmax(unadj_so2_mass - so2_biomass, 0), unadj_so2_mass),
         hg_mass = pmax(unadj_hg_mass, 0), # no biomass variable for hg 
         nox_oz_mass = pmin(nox_oz_mass, nox_mass), # if annual NOx mass is lower than ozone NOx mass, use the annual NOx mass 
         biomass_adj_flag = if_else(biomass_adj_flag == 1 | biomass_adj_flag1 == 1, 1, 0)) %>%
  select(-biomass_adj_flag1) %>% 
  mutate(nox_biomass = pmin(nox_biomass, unadj_nox_mass), # assign the minimum between biomass mass and unadjusted mass to biomass emissions
         nox_bio_oz = pmin(nox_bio_oz, unadj_nox_oz_mass),
         ch4_biomass = pmin(ch4_biomass, unadj_ch4_mass),
         n2o_biomass = pmin(n2o_biomass, unadj_n2o_mass),
         so2_biomass = pmin(so2_biomass, unadj_so2_mass),
         co2_biomass = pmin(co2_biomass, unadj_co2_mass))

# 12. BA codes/names --------------
# 

# count ba_ids for each utility to find those with only 1
utility_ba <- 
  plant_file_7 %>% 
  group_by(utility_id) %>% 
  summarize(count = n(), #length(paste_concat(ba_id, concat = FALSE)),
            ba_code = paste_concat(ba_code),
            ba_name = paste_concat(ba_name)) %>% 
  filter(count == 1)

# replace ba_code and ba_name for these utilities 

plant_file_8 <- 
  plant_file_7 %>%
  rows_update(utility_ba %>% select(-count), 
              by = c("utility_id"), unmatched = "ignore") %>% 
  mutate(ba_code = if_else(ba_name == "Hawaiian Electric Co Inc", "HECO", ba_code),
         ba_name = if_else(ba_name %in% c("No BA", "NA"), NA_character_, ba_name),
         ba_code = if_else(ba_code == "NA", NA_character_, ba_code))

plant_file_check <- 
  plant_file_7 %>%
  rowwise %>% # run through each row and reassign ba_name and ba_code if needed
  mutate(ba_code = if_else(utility_id %in% utility_ba$utility_id, 
                           get_ba_code(utility_id, data = utility_ba), ba_code),
         ba_name = if_else(utility_id %in% utility_ba$utility_id, 
                          get_ba_name(utility_id, data = utility_ba), ba_name),
         ba_code = if_else(ba_name == "Hawaiian Electric Co Inc", "HECO", ba_code),
         ba_name = if_else(ba_name %in% c("No BA", "NA"), NA_character_, ba_name),
         ba_code = if_else(ba_code == "NA", NA_character_, ba_code))

# check all codes have names and all names have codes
stopifnot(nrow(plant_file_8[which(!is.na(plant_file_8$ba_code) & is.na(plant_file_8$ba_name)),])==0)
stopifnot(nrow(plant_file_8[which(is.na(plant_file_8$ba_code) & !is.na(plant_file_8$ba_name)),])==0)


# look up plant_ids with both missing that exist in EIA_861
lookup_ba <- plant_file_8[which(is.na(plant_file_8$ba_code) & is.na(plant_file_8$ba_name)),]

# filter to non-NA values and use as guide to recoding
# merge the balancing_authority and sales_ult_cust to get utility_id, ba_name, and ba_id in one df
# filter to utilities with missing ba data that are not NA
eia_861_use <- 
  eia_861$sales_ult_cust %>% 
  full_join(eia_861$balancing_authority %>% 
              mutate(year = as.character(year))) %>% 
  select(utility_id = utility_number, 
         ba_code, 
         ba_name = balancing_authority_name) %>% 
  mutate(utility_id = as.character(utility_id)) %>% 
  filter(utility_id %in% lookup_ba$utility_id & !is.na(ba_name) & !is.na(utility_id)) 

# update plant_file with the lookup
plant_file_9 <- 
  plant_file_8 %>% 
  rows_patch(eia_861_use, by = c("utility_id"), unmatched = "ignore") %>% 
  mutate(ba_name = if_else(ba_name == "No BA", NA_character_, ba_name), 
         ba_name = if_else(ba_code == "" | is.na(ba_code), "No balancing authority", ba_name),
         ba_code = if_else(ba_code == "" | is.na(ba_code), "NA", ba_code))


# 13.	Plant primary fuel -----------------------

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

# 14.	Plant primary fuel category  ---------------
oil_fuels <- c("DFO", "JF", "KER", "PC", "RFO", "WO", "SGP") # , "OO"
gas_fuels <- c("NG", "PG", "BU") 
oth_FF <- c("BFG", "OG", "TDF", "MSN") #   "HY", "LB", "MH", "MSF"
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

plant_file_10 <- 
  plant_file_9 %>% 
  full_join(fuel_by_plant_4, by = c("plant_id"))


# 15.	Create Generation by Fuel table and update Plant file ------------------
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

# 16.	Resource mix a.	generation by fuel type b.	% resource mix ------------
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

plant_file_11 <- 
  plant_file_10 %>% 
  left_join(ann_gen_by_fuel_3) %>% 
  mutate(primary_fuel_type = if_else(perc_ann_gen_nuclear > 50, "NUC", primary_fuel_type),
         primary_fuel_category = if_else(perc_ann_gen_nuclear > 50, "NUCLEAR", primary_fuel_category))

# there is some weird v small differences, but rounding fixes
# checks the sum of all = 100
stopifnot(all(100 == round(plant_file$perc_ann_gen_renew + plant_file$perc_ann_gen_non_renew, 0) |
      is.na(round(plant_file$perc_ann_gen_renew + plant_file$perc_ann_gen_non_renew, 0))))
      

# 17.	Useful thermal output & 18. Power Heat Ratio & 19. Electric allocation Factor---------------------
eia_923_thermal_output <- 
  eia_923$generation_and_fuel_combined %>% 
  group_by(plant_id) %>%
  summarize(total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE),
            elec_fuel_consumption_mmbtu = sum(elec_fuel_consumption_mmbtu, na.rm = TRUE))

chp <- 
  read_xlsx("data/static_tables/CHP list.xlsx") %>% janitor::clean_names() %>% 
  filter(total > 0) %>% 
  mutate(plant_id = as.character(plant_code)) %>%
  left_join(eia_923_thermal_output) %>%
  mutate(useful_thermal_output = 0.8 * (total_fuel_consumption_mmbtu - elec_fuel_consumption_mmbtu),
         chp_flag = "Yes") %>% 
  select(plant_id, useful_thermal_output, chp_flag) %>% 
  left_join(plant_file_11 %>% select(plant_id, generation_ann), by = c("plant_id")) %>% 
  mutate(power_to_heat = if_else(useful_thermal_output != 0, 
                                 3.413 * generation_ann / useful_thermal_output, 
                                 NA_real_), 
         elec_allocation = if_else(useful_thermal_output != 0, 
                                   3.413 * generation_ann / (0.75 * useful_thermal_output + 3.413 * generation_ann), 
                                   1),
         elec_allocation = if_else(elec_allocation < 0, 0, elec_allocation), 
         elec_allocation = if_else(elec_allocation > 1, 1, elec_allocation)) %>% 
  select(-generation_ann)

plant_file_12 <- 
  plant_file_11 %>% 
  left_join(chp, by = c("plant_id")) 


# 20.	CHP adjustment  ------------------

plant_chp <- 
  plant_file_12 %>% 
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

plant_file_13 <- 
  plant_file_12 %>% 
  filter(is.na(chp_flag)) %>% # filter out CHP flags to easily join in new CHP data
  full_join(plant_chp) %>% 
  mutate(power_heat_ratio = if_else(combust_flag == 1 | combust_flag == 0.5, nominal_heat_rate, NA_real_))


# 21. Pumped storage PM ----------------------------------------

pumped_storage <- 
  generator_file %>% 
  select(plant_id, plant_state, plant_name, prime_mover) %>%
  filter(prime_mover == "PS") %>% distinct()

plant_file_14 <- 
  plant_file_13 %>% 
  mutate(ps_flag = if_else(plant_id %in% pumped_storage$plant_id, "Yes", NA_character_))


# NERC subregion updates ----------------------------------------

# load crosswalk that matches ba_code and ba_name to nerc_subregions
xwalk_ba <- 
  read_csv("data/static_tables/xwalk_balancing_authority.csv") %>%
  janitor::clean_names() %>% 
  select(ba_code = "balancing_authority_code",
         ba_name = "balancing_authority_name",
         nerc_subregion = "subrgn") 

# load crosswalk that matches BA code and system_owner_id to update nerc_subregions
xwalk_ba_transmission <- 
  read_csv("data/static_tables/xwalk_ba_transmission.csv") %>% janitor::clean_names() %>% 
  rename(nerc = "nerc_region",
         ba_code = "balancing_authority_code",
         system_owner_id = "transmission_or_distribution_system_owner_id",
         nerc_subregion = "subrgn") %>% 
  mutate(system_owner_id = as.character(system_owner_id)) %>% distinct()

# load crosswalk that matches ba_code, system_owner_id, and utility_id to update NA nerc_subregion
xwalk_ba_pjm <- 
  read_csv("data/static_tables/xwalk_ba_pjm.csv") %>% janitor::clean_names() %>% 
  rename(nerc = "nerc_region",
         ba_code = "balancing_authority_code",
         system_owner_id = "transmission_or_distribution_system_owner_id",
         nerc_subregion = "subrgn") %>% 
  mutate(system_owner_id = as.character(system_owner_id),
         utility_id = as.character(utility_id)) %>% distinct() %>% 
  drop_na()

# load crosswalk that matches plant_id and plant_state to plant file to update NA nerc_subregion
xwalk_oris_wecc <- 
  read_csv("data/static_tables/xwalk_oris_wecc.csv") %>% janitor::clean_names() %>% 
  select(plant_state = "pstatabb",
         plant_id = "orispl", 
         nerc_subregion = "subrgn") %>% distinct() %>% 
  mutate(plant_id = as.character(plant_id))

# load files that match plant_id to plant file update NA nerc_subregion
xwalk_nerc_assessment <- 
  read_csv("data/static_tables/nerc_assessment_areas_grouped_by_plant.csv") %>% janitor::clean_names() %>% 
  full_join(read_csv("data/static_tables/xwalk_nerc_assessment.csv") %>% janitor::clean_names()) %>% 
  rename(plant_id = "plant_code",
         nerc_subregion = "subrgn") %>% 
  mutate(plant_id = as.character(plant_id)) %>% 
  filter(!is.na(nerc_subregion) & !is.na(plant_id)) %>%
  select(-assessment_area) %>% distinct() %>% 
  group_by(plant_id) %>% filter(n() == 1) # remove plant_ids with multiple NERC regions listed

# load file that matches nerc_subregion to nerc_subregion_name
nerc_subregions <-  
  read_csv("data/static_tables/egrid_nerc_subregions.csv") %>% janitor::clean_names() %>% 
  rename("nerc_subregion" = "subrgn",
         "nerc_subregion_name" = "srname")

# update plant file with NERC subregion updates
plant_file_15 <- 
  plant_file_14 %>% 
  mutate(nerc = case_when(plant_state == "PR" ~ "PR",
                          plant_state == "AK" ~ "AK",
                          plant_state == "HI" ~ "HI",
                          is.na(nerc) ~ "NA",
                          nerc == "" ~ "NA",
                          TRUE ~ nerc), 
         nerc_subregion = case_when(nerc == "TRE" ~ "ERCT",
                                    nerc == "FRCC" ~ "FRCC",
                                    nerc == "PR" ~ "PRMS",
                                    TRUE ~ NA_character_),
         system_owner_id = if_else(is.na(system_owner_id), "-9999", system_owner_id)) %>% 
  rows_patch(xwalk_ba, by = c("ba_code", "ba_name"), unmatched = "ignore") %>% 
  rows_patch(xwalk_ba_transmission, by = c("nerc", "ba_code", "system_owner_id"), unmatched = "ignore") %>% 
  rows_patch(xwalk_ba_pjm, by =  c("nerc", "ba_code", "system_owner_id", "utility_id"), unmatched = "ignore") %>% 
  rows_patch(xwalk_oris_wecc, by = c("plant_state", "plant_id"), unmatched = "ignore") %>% 
  rows_patch(xwalk_nerc_assessment, by = c("plant_id"), unmatched = "ignore") %>% 
  left_join(nerc_subregions, by = c("nerc_subregion"))


# Update plant lat/lon ----------------------------------------

plant_file_16 <- 
  plant_file_15 %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         # if any lat/lon coordinates are 0, make them NA 
         lat = if_else(lat == 0, NA_real_, lat),
         lon = if_else(lon == 0, NA_real_, lon), 
         # manual lat/lon updates for some plant IDs
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

# 28. ISORTO ----------

# assign isorto to plants where applicable 
plant_file_17 <- 
  plant_file_16 %>% 
  mutate(isorto = case_when(ba_code == "CISO" ~ "CAISO",
                            ba_code == "ERCO" ~ "ERCOT",
                            ba_code == "ISNE" ~"ISONE",
                            ba_code == "MISO" ~"MISO",
                            ba_code == "NYIS" ~"NYISO",
                            ba_code == "PJM" ~"PJM",
                            ba_code == "SPA" ~"SPP",
                            TRUE ~ NA_character_)) %>%
  mutate(isorto = if_else(plant_state %in% c("AL","AK","AZ","CO","FL","GA", "HI", "ID", "OR", "SC", "TN", "UT", "WA"),
                          NA_character_, isorto))

# 29. Calculate CO2 equivalent and 30. update negative emissions values ----------------------------------------

# CO2 equivalent adds in other greenhouse gas emission masses 
# CH4 and N2O are multiplied by their associated global warming potential value (25 and 298 respectively)
plant_file_18 <- 
  plant_file_17 %>% 
  mutate(co2_equivalent = 
             if_else(is.na(co2_mass), 0, co2_mass) + 
             if_else(is.na(ch4_mass), 0, 25 * ch4_mass / 2000) + 
             if_else(is.na(n2o_mass), 0, 298 * n2o_mass / 2000), 
         # if emission mass is less than 0, re-assign as 0 
         co2_mass = if_else(co2_mass < 0, 0, co2_mass),
         ch4_mass = if_else(ch4_mass < 0, 0, ch4_mass),
         n2o_mass = if_else(n2o_mass < 0, 0, n2o_mass),
         co2_equivalent = if_else(co2_equivalent < 0, 0, co2_equivalent))


# Calculate emissions rates  --------------------
## Combustion output emissions rates ----------------------------------------

plant_file_19 <- 
  plant_file_18 %>% 
  mutate(nox_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * nox_mass / ann_gen_combust),
         nox_combust_out_emission_rate_oz = if_else(ann_gen_combust * (generation_oz / generation_ann) < 0, 0, 
                                                    2000 * nox_oz_mass / (ann_gen_combust * (generation_oz / generation_ann))),
         so2_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * so2_mass / ann_gen_combust),
         co2_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * co2_mass / ann_gen_combust),
         ch4_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * ch4_mass / ann_gen_combust),
         n2o_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * n2o_mass / ann_gen_combust),
         co2_equiv_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0, 2000 * co2_equivalent / ann_gen_combust),
         hg_combust_out_emission_rate = if_else(ann_gen_combust < 0, 0,2000 * hg_mass / ann_gen_combust)  )


## Input emission rates ----------------------------------------
  
plant_file_20 <- 
  plant_file_19 %>% 
  mutate(nox_in_emission_rate = 2000 * nox_mass / heat_input,
         nox_in_emission_rate_oz = 2000 * nox_oz_mass / heat_input_oz,
         so2_in_emission_rate = 2000 * so2_mass / heat_input,
         co2_in_emission_rate = 2000 * co2_mass / heat_input,
         ch4_in_emission_rate = 2000 * ch4_mass / heat_input,
         n2o_in_emission_rate = 2000 *  n2o_mass / heat_input,
         co2_equiv_in_emission_rate = 2000 * co2_equivalent / heat_input,
         hg_in_emission_rate = 2000 * hg_mass / heat_input)  

## Output emission rates ----------------------------------------

plant_file_21 <- 
  plant_file_20 %>% 
  mutate(nox_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * nox_mass / generation_ann),
         nox_out_emission_rate_oz =if_else(generation_oz < 0, 0,  2000 * nox_oz_mass / generation_oz),
         so2_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * so2_mass / generation_ann),
         co2_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * co2_mass / generation_ann),
         ch4_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * ch4_mass / generation_ann),
         n2o_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * n2o_mass / generation_ann),
         co2_equiv_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * co2_equivalent / generation_ann),
         hg_out_emission_rate = if_else(generation_ann < 0, 0, 2000 * hg_mass / generation_ann))

# Calculate nonbaseload factor ---------------------------------

plant_file_22 <- 
  plant_file_21 %>% 
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


## update heat_input_source ----------

unit_source_heat_input <- update_source(x = "heat_input_source", unit_f = unit_file) 

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_heat_input$heat_input_source %in% check_list))


## update heat_input_oz_source ----------

unit_source_heat_input_oz <- update_source(x = "heat_input_oz_source", unit_f = unit_file) 

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_heat_input_oz$heat_input_oz_source %in% check_list))


## update nox_source ---------

unit_source_nox <- update_source(x = "nox_source", unit_f = unit_file) 

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_nox$nox_source %in% check_list))


## update nox_oz_source --------

unit_source_nox_oz <- update_source(x = "nox_oz_source", unit_f = unit_file) 

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_nox_oz$nox_oz_source %in% check_list))


## update co2_source ---------

unit_source_co2_source <- update_source(x = "co2_source", unit_f = unit_file) 

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_co2_source$co2_source %in% check_list))


## update so2_source ----------

unit_source_so2_source <- update_source(x ="so2_source", unit_f = unit_file) 

# check if sources updated correctly, stop if not
stopifnot(all(unit_source_so2_source$so2_source %in% check_list))

# update plant file with source updates
### PICK UP 10/9


plant_file <- plant_file %>% mutate(ch4_source = ifelse(!is.na(ch4_mass), "EIA", NA))
plant_file <- plant_file %>% mutate(n2o_source = ifelse(!is.na(n2o_mass), "EIA", NA))

rm(unit_source, chk_list)

# 34. Update NULL adjusted heat input --------------

# heat_input was only populated for chp_flag == Yes
x <- which(!is.na(plant_file$heat_input))
y <- which(plant_file$chp_flag == "Yes")
z <- which(plant_file$chp_flag == "Yes" & (is.na(plant_file$elec_allocation) | is.na(plant_file$unadj_heat_input)) )
stopifnot(length(x) == length(y) - length(z))
rm(x,y,z)

# fill it with the unadjusted value for others
# this should be moved into one step in re-organization
# what about heat_input_oz?
plant_file <- plant_file %>% mutate(heat_input = ifelse(is.na(heat_input), unadj_heat_input, heat_input))

# 35. Round plant file -------------------------

plant_file <- plant_file %>% mutate(capfac = round(capfac, 5), # a - capacity factor
                                    power_heat_ratio = round(power_heat_ratio, 3), # b - CHP power heat ration
                                    elec_allocation = round(elec_allocation, 6), # c - CHP electric allocation
                                    # SQL - heat input
                                    combust_heat_input = round(combust_heat_input, 3),
                                    combust_heat_input_oz = round(combust_heat_input_oz, 3), 
                                    heat_input = round(heat_input, 3),
                                    heat_input_oz = round(heat_input_oz, 3),
                                    # j - annual generation
                                    generation_ann = round(generation_ann, 3), 
                                    generation_oz = round(generation_oz, 3), 
                                    # d - emissions mass
                                    nox_mass = round(nox_mass, 3), 
                                    nox_oz = round(nox_oz, 3),
                                    so2_mass = round(so2_mass, 3),
                                    co2_mass = round(co2_mass, 3),
                                    ch4_mass = round(ch4_mass, 3),
                                    n2o_mass = round(n2o_mass, 3),
                                    co2_equivalent = round(co2_equivalent, 3),
                                    hg_mass = round(hg_mass, 6),
                                    # e - output emissions rates
                                    nox_out_emission_rate = round(nox_out_emission_rate, 3), 
                                    nox_out_emission_rate_oz = round(nox_out_emission_rate_oz, 3),
                                    so2_out_emission_rate = round(so2_out_emission_rate, 3),
                                    co2_out_emission_rate = round(co2_out_emission_rate, 3),
                                    ch4_out_emission_rate = round(ch4_out_emission_rate, 3),
                                    n2o_out_emission_rate = round(n2o_out_emission_rate, 3),
                                    co2_equiv_out_emission_rate = round(co2_equiv_out_emission_rate, 3),
                                    hg_out_emission_rate = round(hg_out_emission_rate, 6),
                                    # f - input emissions rates
                                    nox_in_emission_rate = round(nox_in_emission_rate, 3), 
                                    nox_in_emission_rate_oz = round(nox_in_emission_rate_oz, 3),
                                    so2_in_emission_rate = round(so2_in_emission_rate, 3),
                                    co2_in_emission_rate = round(co2_in_emission_rate, 3),
                                    ch4_in_emission_rate = round(ch4_in_emission_rate, 3),
                                    n2o_in_emission_rate = round(n2o_in_emission_rate, 3),
                                    co2_equiv_in_emission_rate = round(co2_equiv_in_emission_rate, 3),
                                    hg_in_emission_rate = round(hg_in_emission_rate, 6),
                                    # e - combustion output emissions rates
                                    nox_combust_out_emission_rate = round(nox_combust_out_emission_rate, 3), 
                                    nox_combust_out_emission_rate_oz = round(nox_combust_out_emission_rate_oz, 3),
                                    so2_combust_out_emission_rate = round(so2_combust_out_emission_rate, 3),
                                    co2_combust_out_emission_rate = round(co2_combust_out_emission_rate, 3),
                                    ch4_combust_out_emission_rate = round(ch4_combust_out_emission_rate, 3),
                                    n2o_combust_out_emission_rate = round(n2o_combust_out_emission_rate, 3),
                                    co2_equiv_combust_out_emission_rate = round(co2_equiv_combust_out_emission_rate, 3),
                                    hg_combust_out_emission_rate = round(hg_combust_out_emission_rate, 6),
                                    # g - undjusted mass
                                    unadj_nox_mass = round(unadj_nox_mass, 3), 
                                    unadj_nox_oz = round(unadj_nox_oz, 3),
                                    unadj_so2_mass = round(unadj_so2_mass, 3),
                                    unadj_co2_mass = round(unadj_co2_mass, 3),
                                    unadj_ch4_mass = round(unadj_ch4_mass, 3),
                                    unadj_n2o_mass = round(unadj_n2o_mass, 3),
                                    # no co2_equivalent unadjusted mass
                                    unadj_hg_mass = round(unadj_hg_mass, 6),
                                    # g - unadjusted heat input
                                    unadj_combust_heat_input = round(unadj_combust_heat_input, 3),
                                    unadj_combust_heat_input_oz = round( unadj_combust_heat_input_oz, 3), 
                                    unadj_heat_input = round( unadj_heat_input, 3),
                                    unadj_heat_input_oz = round( unadj_heat_input_oz, 3),
                                    # i - nominal heat rate
                                    nominal_heat_rate = round(nominal_heat_rate, 6),
                                    # j - annual generation (by fuel)
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
                                    # h - biomass 
                                    nox_biomass = round(nox_biomass, 3), 
                                    nox_bio_oz = round(nox_bio_oz, 3),
                                    so2_biomass = round(so2_biomass, 3),
                                    co2_biomass = round(co2_biomass, 3),
                                    ch4_biomass = round(ch4_biomass, 3),
                                    n2o_biomass = round(n2o_biomass, 3),
                                    # no co2 equivalent biomass
                                    # no hg biomass
                                    # SQL - CHP mass
                                    chp_nox = round(chp_nox, 3), 
                                    chp_nox_oz = round(chp_nox_oz, 3),
                                    chp_so2 = round(chp_so2, 3),
                                    chp_co2 = round(chp_co2, 3),
                                    chp_ch4 = round(chp_ch4, 3),
                                    chp_n2o = round(chp_n2o, 3),
                                    # no co2 equivalent CHP
                                    # no hg CHP
                                    # i - uto
                                    uto = round(uto,3)
                                    ) 


# 36. Correct fuel types ----------------------

# manually updating the type changes in Fuel Type Corrections
# move this to be near other primary fuel type code
# update before creating primary_fuel_category
plant_file <- plant_file %>% 
  mutate(primary_fuel_type = case_when(plant_id == 55970 ~ "NG",
                                       plant_id == 10154 ~ "NG",
                                       TRUE ~ primary_fuel_type),
         primary_fuel_category = case_when(plant_id == 55970 ~ "GAS",
                                       plant_id == 10154 ~ "GAS",
                                       TRUE ~ primary_fuel_category))

# 37. Update PS BA code to PSCO for one plant ---------------------

which(plant_file$ba_id == "PS")
unique(plant_file$ba_id)

# there are no PS in the ba_id, add conversion anyway
plant_file <- plant_file %>% mutate(ba_id = ifelse(ba_id == "PS", "PSCO", ba_id))


# 38. Update BA names ------------------------


BA_codes <- read_xlsx(here("data", "static_tables", "BA Codes.xlsx"))

plant_file$ba_id[which(!plant_file$ba_id %in% BA_codes$BACODE)] %>% unique
plant_file$ba_name[which(!plant_file$ba_id %in% BA_codes$BACODE)] %>% unique
# NVE does not appear in this list, but is named "Nevada Power Company" which is NEVP in this list
# find out where "character(0)" gets introduced and make it NA before here
plant_file <- plant_file %>% mutate(ba_id = case_when(ba_id == "character(0)" ~ "NA",
                                                      ba_id == "NVE" ~ "NEVP",
                                                      TRUE ~ ba_id))


plant_file$ba_name[which(!plant_file$ba_name %in% BA_codes$BANAME)] %>% unique
plant_file$ba_id[which(!plant_file$ba_name %in% BA_codes$BANAME)] %>% unique


BA_codes <- BA_codes %>% rename(ba_id = BACODE,
                                new_ba_name = BANAME)

plant_file <- plant_file %>% left_join(BA_codes) %>%
  mutate(ba_name = ifelse(is.na(new_ba_name), ba_name, new_ba_name)) %>%
  select(-new_ba_name)

# 39. CAMD flag ----------------------
# handled in step # 1 in originally aggregating the unit file by plant_id

# 40. Update coal flag ----------------


unique(plant_file$coal_flag)
# also allowed coal_flag == 0 to be converted 
update_coal <- plant_file %>% filter(is.na(coal_flag) | coal_flag == 0) %>% select(plant_id, primary_fuel_category) %>%
  filter(primary_fuel_category == "COAL")
# I had programmed as 0 & 1, so I also convert to Yes and No here
plant_file  <- plant_file %>% mutate(coal_flag = ifelse(plant_id %in% update_coal$plant_id | coal_flag == 1, "Yes", "No" ))

rm(update_coal)

# Additional cleaning
colnames(plant_file)

# add sequence
plant_file$seq <- 1:nrow(plant_file)
# convert 1 flags to "Yes"
plant_file <- plant_file %>% 
  mutate(biomass_adj_flag = ifelse(biomass_adj_flag==1, "Yes", biomass_adj_flag))

# output
saveRDS(plant_file, here("data","outputs","plant_file.RDS"))
