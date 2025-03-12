## -------------------------------
##
## Unit file create 
## 
## Purpose: 
## 
## This file creates the unit file for eGRID. 
## This includes all operating units for the specified eGRID data year
##
## For data or manual changes that need to be checked every year, they are denoted with this note:
## ### Note: check for updates or changes each data year ###
##
## Authors:  
##      Sean Bock, Abt Global
##      Caroline Watson, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------

# Load libraries ------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# define params for eGRID data year 
# this is only necessary when running the script outside of egrid_master.qmd

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

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


# Load necessary data ------

## EPA ------
epa_vars_to_keep <- 
  c("year",
    "plant_state",
    "plant_name",
    "plant_id",
    "unit_id",
    "operating_status",
    "reporting_frequency",
    "program_code",
    "primary_fuel_type",
    "nameplate_capacity",
    "unit_type" = "unit_type_abb",
    "operating_hours" = "operating_time_count",
    "heat_input" = "heat_input_mmbtu",
    "heat_input_oz" = "heat_input_mmbtu_ozone",
    "nox_mass" = "nox_mass_short_tons",
    "nox_oz_mass" = "nox_mass_short_tons_ozone",
    "so2_mass" = "so2_mass_short_tons",
    "so2_mass_oz" = "so2_mass_short_tons_ozone",
    "co2_mass" = "co2_mass_short_tons",
    "hg_mass" = "hg_mass_lbs",
    "heat_input_source",
    "heat_input_oz_source",
    "nox_source",
    "nox_oz_source",
    "so2_source",
    "co2_source",
    "hg_source",
    "so2_controls",
    "nox_controls",
    "hg_controls",
    "year_online"
  )


if(file.exists(glue::glue("data/clean_data/epa/{params$eGRID_year}/epa_clean.RDS"))) { 
  epa <- 
    read_rds(glue::glue("data/clean_data/epa/{params$eGRID_year}/epa_clean.RDS")) %>% 
    select(all_of(epa_vars_to_keep)) # keeping only necessary variables
} else { 
   stop("epa_clean.RDS does not exist. Run data_load_epa.R and data_clean_epa.R to obtain.")}

## EIA ------------

# Load EIA-860
if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS"))) {
  eia_860 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS"))
} else { 
  stop("eia_860_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

# Load EIA-923
if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) { 
  eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))
} else { 
   stop("eia_860_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}


## Crosswalks and static tables ---------
# for each crosswalk or static table, we specify the column types (ex: character or numeric) to avoid misread values

# Power Sector Data Crosswalk matches units between EPA and EIA data sets
# this will be used to help update Coal units in EPA and assign correct primary fuel type
xwalk_eia_epa <- read_csv("data/static_tables/xwalk_epa_eia_power_sector.csv",
                          col_types = cols_only(EPA_PLANT_ID = "c", 
                                                EPA_UNIT_ID = "c", 
                                                EPA_FUEL_TYPE = "c", 
                                                EIA_PLANT_ID = "c", 
                                                EIA_GENERATOR_ID = "c", 
                                                EIA_FUEL_TYPE = "c", 
                                                EIA_UNIT_TYPE = "c")) %>% # define col_types to all be characters
                  janitor::clean_names() 

# Boiler Firing Type Crosswalk
xwalk_botfirty <- read_csv("data/static_tables/xwalk_boiler_firing_type.csv", 
                           col_types = "ccccc")

# Crosswalk to add additional boiler IDs 
### Note: check for updates or changes each data year ###
xwalk_control_ids <- read_csv("data/static_tables/xwalk_860_boiler_control_id.csv", 
                              col_types = "cccccccccc") 

# Crosswalk for Puerto Rico units to match EIA plant/unit IDs to EPA plant/unit IDs
xwalk_pr_oris <- read_csv("data/static_tables/xwalk_pr_oris.csv", 
                          col_types = "cccccc") 

# Biomass units to add, these units are identified from the plant file each data year
### Note: check for updates or changes each data year ###
biomass_units <- read_csv("data/static_tables/biomass_units_to_add_to_unit_file.csv", 
                          col_types = "ccccccc") %>% 
  janitor::clean_names() %>% 
  filter(year == params$eGRID_year) # only keep units from eGRID_year

# Some plants in EPA are not connected to the grid or are retired, so they are excluded from eGRID
### Note: check for updates or changes each data year ###
epa_plants_to_delete <- read_csv("data/static_tables/epa_plants_to_delete.csv", 
                                  col_types = "c") %>% 
  select("plant_id" = "ORIS Code") 

# Emission factors 
# physical units emission factors
emission_factors_pu <- read_csv("data/static_tables/emission_factors_physicalunits.csv", 
                             col_types = "cccdccccccdccccc")

# heat input emission factors
emission_factors_hi <- read_csv("data/static_tables/emission_factors_heatinput.csv", 
                                col_types = "cccdccccccdccccc")

# CO2 emission factors
co2_ef <- read_csv("data/static_tables/co2_ch4_n2o_ef.csv", 
                   col_types = "cccdcdcdcc")

# OG fuel types  
### Note: check for updates or changes each data year ###
og_fuel_types_update <- 
  read_csv("data/static_tables/og_oth_units_to_change_fuel_type.csv", 
           col_types = cols_only(plant_id = "c", 
                                 unit_id = "c", 
                                 prime_mover = "c",
                                 primary_fuel_type = "c",
                                 fuel_code = "c")) 

# NREL geothermal plants
### Note: check for updates or changes each data year ###
# this table is sourced from NREL, they will not update it until 2025
# check in 2025 for updated table 
nrel_geo_type <- read_csv("data/static_tables/nrel_geothermal_table.csv", 
                          col_types = "c") %>% 
  rename("plant_id" = "ORISPL") %>% janitor::clean_names() %>% distinct()

# Geothermal emission factors
geo_emission_factors <- read_csv("data/static_tables/geothermal_emission_factors.csv", 
                                 col_types = "ccdddddd") %>% 
  janitor::clean_names()

# Units to remove 
### Note: check for updates or changes each data year ###
units_to_remove <- 
  read_csv("data/static_tables/units_to_remove.csv", 
           col_types = "c") 

# EIA plants to delete 
# we delete plants that are already in EPA and matched to EIA from this crosswalk
eia_plants_to_delete <- read_csv("data/static_tables/xwalk_oris_epa.csv", 
                                 col_types = cols_only(eia_plant_id = "c")) %>% 
  mutate(plant_id = eia_plant_id) %>% select(plant_id)

# Manual edits
# there are some units and plants that need manual changes 
# we document them in this Excel sheet 
### Note: check for updates or changes each data year ### 
manual_corrections <- 
  read_excel("data/static_tables/manual_corrections.xlsx", 
             sheet = "unit_file", 
             col_types = c("text", "text", "text", "text", "text"))

# Fuel types by category 
fuel_type_category <- 
  read_csv("data/static_tables/fuel_type_categories.csv", 
           col_types = cols_only(coal_fuels = "c",
                                 combustion_fuels = "c")) 

coal_fuels <- fuel_type_category[["coal_fuels"]]
coal_fuels <- coal_fuels[!is.na(coal_fuels)]

combustion_fuels <- fuel_type_category[["combustion_fuels"]]

# Modifying EPA data ---------

## Harmonizing fields with EIA ----------

# vectors of unit types matched to their respective EIA prime mover values
pm_st <- c("BFB", "C", "CB", "CFB", "DB", "DTF", "DVF", "IGC", "KLN", "OB", "PRH", "S", "T", "WBF", "WBT")
pm_gt <- c("AF", "CT")
pm_ct <- c("CC")
pm_ot <- c("OT")

epa_2 <- 
  epa %>% 
  left_join(manual_corrections %>% filter(column_to_update == "unit_id"), 
            by = c("plant_id", "unit_id")) %>% 
  mutate(
    # creating prime_mover based on mapping above
    prime_mover = case_when( 
      unit_type %in% pm_st ~ "ST",
      unit_type %in% pm_gt ~ "GT",
      unit_type %in% pm_ct ~ "CT",
      unit_type %in% pm_ot ~ "OT",
      TRUE ~ "EIA PM"),
    # modifying unit IDs to match 860 files
    ### Note: check for updates or changes each data year ###
    unit_id = case_when( 
      !is.na(update) ~ update, # update unit ID from manual_corrections
      TRUE ~ unit_id) 
  )

### Update fuel types ---------

#### Update coal fuel types ---------

# Units identified in EPA as "Coal" are updated in this order: 
    # 1) matched via an EPA / EIA crosswalk and energy_source_1 assigned for Coal units default to EIA-860 
    # 2) Coal fuel type with highest fuel consumption in EIA-923 
    # 3) Plants with Coal and Biomass units require a manual update to Coal fuel types, ignoring max fuel consumption from Biomass fuel types

# identify fuels in EPA use crosswalk to identify primary fuel type  

# some EPA units have leading zeroes in their unit_id
# We remove them to match the EIA-EPA crosswalk, and then replace them using a lookup table
epa_leading_zeroes <- 
  epa_2 %>% 
  filter(str_detect(unit_id, "^0+")) %>%  
  select(plant_id, unit_id) %>% 
  mutate(unit_id_clean = str_remove(unit_id, "^0+"),
         id = paste0(plant_id, "_", unit_id_clean))

lookup_epa_leading_zeroes <- with(epa_leading_zeroes, setNames(unit_id, id))

coal_xwalk_update <- 
  xwalk_eia_epa %>% 
  select(epa_plant_id, epa_unit_id, eia_plant_id, eia_generator_id, eia_unit_type) %>% 
  inner_join(epa_2 %>% filter(str_detect(primary_fuel_type, "^Coal")) %>% 
               mutate(unit_id = str_remove(unit_id, "^0+")), 
             by = c("epa_plant_id" = "plant_id", "epa_unit_id" = "unit_id", "eia_unit_type" = "prime_mover")) %>% 
  left_join(eia_860$combined, 
            by = c("eia_plant_id" = "plant_id", "eia_generator_id" = "generator_id", "eia_unit_type" = "prime_mover")) %>% 
  mutate(id = paste0(epa_plant_id, "_", epa_unit_id), 
         epa_unit_id = recode(id, !!!lookup_epa_leading_zeroes, .default = epa_unit_id)) %>% # replace leading zeroes that were removed when matching to crosswalk
  select(plant_id = epa_plant_id, 
         unit_id = epa_unit_id, 
         prime_mover = eia_unit_type, 
         energy_source_1) %>%
  distinct() %>% group_by(plant_id, unit_id) %>% filter(!n() > 1) %>% # keep units with only 1 fuel type 
  drop_na() %>% ungroup()  

# identify coal fuel types in EPA and match to primary coal fuel types in EIA-923
# these coal fuel types will update the primary fuel type for those that do not have an energy_source_1 listed in EIA-860
coal_fuel_type_923 <- 
  epa_2 %>% filter(str_detect(primary_fuel_type, "^Coal")) %>% # identify any fuel types that start with Coal in EPA data
  left_join(eia_923$generation_and_fuel_combined %>% # identify Coal units in EPA and match to EIA-923, and default to highest consumption Coal fuel in EIA-923
              select(plant_id, prime_mover, fuel_type, total_fuel_consumption_mmbtu) %>%
              filter(fuel_type %in% coal_fuels) %>% 
              group_by(plant_id, prime_mover) %>%
              slice_max(total_fuel_consumption_mmbtu, n = 1, with_ties = FALSE) %>% ungroup() %>% # identify fuel type associate with max fuel consumption by plant
              select(plant_id, prime_mover, fuel_type),
            by = c("plant_id", "prime_mover")) %>% 
  mutate(primary_fuel_type = fuel_type) %>% 
  select(-fuel_type) %>% distinct()

# some plants have both biomass and coal units, and biomass fuel types may have highest fuel consumption
# for EPA identified coal units, we want to default to Coal fuel types for the primary fuel type assignment
coal_biomass_plants <- 
  epa_2 %>% filter(str_detect(primary_fuel_type, "^Coal") & plant_id %in% biomass_units$plant_code) %>% 
  left_join(eia_923$generation_and_fuel_combined %>%  # joining generation and fuel file, based on max fuel consumption by plant
              select(plant_id, prime_mover, fuel_type, total_fuel_consumption_mmbtu) %>%
              filter(fuel_type %in% coal_fuels) %>% 
              group_by(plant_id, prime_mover) %>%
              slice_max(total_fuel_consumption_mmbtu, n = 1, with_ties = FALSE) %>% ungroup() %>% # identify fuel type associate with max fuel consumption by plant
              select(plant_id, prime_mover, fuel_type),
            by = c("plant_id", "prime_mover")) %>% 
  mutate(primary_fuel_type = fuel_type) %>%  # filling missing primary_fuel_types with 923 value
  select(plant_id, unit_id, plant_name, primary_fuel_type) 

#### Update all primary fuels --------  

# Updating fuel types to EIA codes for oil, other solid fuel, and coal 
# When there is match in EIA-860, we take the EIA-860 fuel code. For non-matches, we check EIA-923 Gen and Fuel file
# and take the primary fuel from each plant (i.e., the fuel code of the plant/prime mover with the highest fuel consumption).
# There are some left over, resulting from a) `UNIT ID` and GENID not matching and b) 0 fuel consumption values in the EIA-923 Gen and Fuel file.
# These cases may require individual updates once identified.

# update EPA with primary fuel types by matching EIA fuel codes or using EIA data  
epa_3 <-
  epa_2 %>%
  left_join(eia_860$combined %>% # joining with 860_combined to get EIA primary fuel codes
              distinct(plant_id, generator_id, energy_source_1), 
            by = c("plant_id", "unit_id" = "generator_id")) %>% 
  rows_update(coal_xwalk_update, by = c("plant_id", "unit_id", "prime_mover")) %>% # update Coal energy sources via EIA-EPA crosswalk
  mutate(primary_fuel_type = case_when(
    primary_fuel_type %in% c("Other Oil", "Other Solid Fuel", "Coal") ~ energy_source_1,
    primary_fuel_type == "Diesel Oil" ~ "DFO",
    primary_fuel_type == "Natural Gas" ~ "NG",
    primary_fuel_type == "Pipeline Natural Gas" ~ "NG",
    primary_fuel_type == "Other Gas" ~ "OG",
    primary_fuel_type == "Petroleum Coke" ~ "PC",
    primary_fuel_type == "Process Gas" ~ "PRG",
    primary_fuel_type == "Residual Oil" ~ "RFO",
    primary_fuel_type == "Tire Derived Fuel" ~ "TDF",
    primary_fuel_type == "Coal Refuse" ~ "WC",
    primary_fuel_type == "Wood" ~ "WDS",
    TRUE ~ NA_character_)) %>% 
  rows_patch(coal_fuel_type_923, by = c("plant_id", "unit_id"), unmatched = "ignore") %>% # update Coal units from Coal fuel type with highest consumption in EIA-923
  rows_update(coal_biomass_plants, by = c("plant_id", "unit_id"), unmatched = "ignore") %>% # update Coal units from plants that also have biomass units
  left_join(eia_923$generation_and_fuel_combined %>%  # joining generation and fuel file, based on max fuel consumption by plant
              select(plant_id, prime_mover, fuel_type, total_fuel_consumption_mmbtu) %>%
              group_by(plant_id, prime_mover) %>%
              slice_max(total_fuel_consumption_mmbtu, n = 1, with_ties = FALSE) %>% ungroup() %>% # identify fuel type associate with max fuel consumption by plant
              select(plant_id, prime_mover, fuel_type),
            by = c("plant_id", "prime_mover")) %>% 
  mutate(primary_fuel_type = if_else(is.na(primary_fuel_type), fuel_type, primary_fuel_type)) %>%  # filling missing primary_fuel_types with 923 value
  select(-fuel_type) 


## identify units remaining with missing primary_fuel_types
missing_fuel_types <- 
  epa_3 %>% 
  filter(is.na(epa_3$primary_fuel_type))

print(glue::glue("{nrow(missing_fuel_types)} units having missing primary fuel types."))


### Add boiler firing type ------------
# A crosswalk is used to create a boiler firing type variable, based on unit type. 
# some units have more specific boiler firing types available in the 860 boiler info file
# and these are used where available
### Note: check for updates or changes each data year ###

epa_4 <- 
  epa_3 %>% 
  left_join(xwalk_botfirty,
            by = c("unit_type" = "EPA")) %>% 
  mutate(botfirty = eGRID) %>% 
  select(-any_of(names(xwalk_botfirty)))

botfirty_to_update <- # matching with 860 data to see if more specific botfirty type exists
  epa_4 %>% 
  select(plant_id, unit_id, botfirty) %>% 
  filter(botfirty %in% c("OTHER BOILER", "OTHER TURBINE", NA_character_)) %>% 
  left_join(eia_860$boiler_info_design_parameters %>% 
              select(plant_id,
                     "unit_id" = boiler_id,
                     firing_type_1)) %>%
  left_join(xwalk_botfirty %>% select(`EIA-860`, eGRID) %>% filter(!is.na(`EIA-860`)),
            by = c("firing_type_1" = "EIA-860")) %>% 
  filter(!is.na(eGRID)) %>% 
  select(plant_id, unit_id, eGRID)


epa_5 <- # updating units where available
  epa_4 %>% 
  left_join(botfirty_to_update) %>% 
  mutate(botfirty = if_else(!is.na(eGRID), eGRID, botfirty)) %>% 
  select(-eGRID) 


### Delete specified plants -----------

# delete plants with null heat inputs for all units
# some plants will be added back in from EIA data
epa_null_heat_input <- 
  epa_5 %>% group_by(plant_id) %>% 
  filter(all(is.na(heat_input))) %>% 
  select(plant_id) %>% distinct() %>% ungroup()

epa_6 <- 
  epa_5 %>% 
  rows_delete(epa_plants_to_delete, by = c("plant_id"), unmatched = "ignore") %>% # delete specified plants from EPA dataframe
  rows_delete(epa_null_heat_input, by = c("plant_id"), unmatched = "ignore") # delete plants with null heat input


## Gap fill EPA ozone season reporters with EIA data ----------

# a.	Some EPA plants are “ozone season reporters” – which means that they only report data during the ozone season, which is May to September each year. 
# b.	These plants are listed in the EPA data with a frequency variable of “OS” instead of “Q” (quarterly)
# c.	We gap fill the missing months (January, February, March, April, October, November, and December) with EIA data. 

### Gap fill heat input for OS reporters --------------

epa_oz_reporter_plants <- # getting list of plant ids that include ozone season reporters. Note that some plants include both OS only and OS and Annual reporters.
  epa_6 %>% 
  filter(reporting_frequency == "OS") %>% 
  pull(plant_id)

epa_oz_reporters <- # creating dataframe with all plants and associated units that include "OS" reporters.
  epa_6 %>%
  filter(plant_id %in% epa_oz_reporter_plants)

### Summing heat input values in the EIA-923 Gen and Fuel file. These totals will be used to distribute heat for the non-ozone months in the EPA data.
### We also add consumption totals here, which will be used later when estimating NOx emissions.

heat_923_oz_months <- paste0("tot_mmbtu_", tolower(month.name)[5:9]) # creating vector of monthly heat columns for 923 gen and fuel
heat_923_nonoz_months <- paste0("tot_mmbtu_", tolower(month.name)[c(1:4,10:12)]) # creating vector of monthly heat columns for 923 gen and fuel
consum_923_nonoz_months <- paste0("quantity","_", tolower(month.name)[c(1:4,10:12)]) # creating vector of monthly consumption
consum_923_oz_months <- paste0("quantity","_", tolower(month.name)[5:9])

eia_fuel_consum_pm <- # summing fuel and consum to PM level
  eia_923$generation_and_fuel_combined %>%
  mutate(unit_heat_nonoz = rowSums(pick(all_of(heat_923_nonoz_months)), na.rm = TRUE),
         unit_heat_oz = rowSums(pick(all_of(heat_923_oz_months)), na.rm = TRUE),
         unit_consum_nonoz = rowSums(pick(all_of(consum_923_nonoz_months)), na.rm = TRUE),
         unit_consum_oz = rowSums(pick(all_of(consum_923_oz_months)), na.rm = TRUE)) %>%
  group_by(plant_id, prime_mover) %>%
  summarize(heat_input_nonoz_923 = sum(unit_heat_nonoz, na.rm = TRUE),
            heat_input_oz_923 = sum(unit_heat_oz, na.rm = TRUE),
            heat_input_ann_923 = sum(total_fuel_consumption_mmbtu, na.rm = TRUE), # consumption in mmbtus is referred to as "heat input"
            fuel_consum_nonoz_923 = sum(unit_consum_nonoz, na.rm = TRUE),
            fuel_consum_oz_923 = sum(unit_consum_oz, na.rm = TRUE),
            fuel_consum_ann_923 = sum(total_fuel_consumption_quantity, na.rm = TRUE) # consumption in quantity is referred to as "fuel consumption"
  ) %>% ungroup() 

eia_fuel_consum_fuel_type <- # summing fuel and consum to PM and fuel_type level
  eia_923$generation_and_fuel_combined %>%
  mutate(unit_heat_nonoz = rowSums(pick(all_of(heat_923_nonoz_months)), na.rm = TRUE),
         unit_heat_oz = rowSums(pick(all_of(heat_923_oz_months)), na.rm = TRUE),
         unit_consum_nonoz = rowSums(pick(all_of(consum_923_nonoz_months)), na.rm = TRUE),
         unit_consum_oz = rowSums(pick(all_of(consum_923_oz_months)), na.rm = TRUE)) %>%
  group_by(plant_id, prime_mover, fuel_type) %>%
  summarize(heat_input_nonoz_923 = sum(unit_heat_nonoz, na.rm = TRUE),
            heat_input_oz_923 = sum(unit_heat_oz, na.rm = TRUE),
            heat_input_ann_923 = sum(total_fuel_consumption_mmbtu, na.rm = TRUE), # consumption in mmbtus is referred to as "heat input"
            fuel_consum_nonoz_923 = sum(unit_consum_nonoz, na.rm = TRUE),
            fuel_consum_oz_923 = sum(unit_consum_oz, na.rm = TRUE),
            fuel_consum_ann_923 = sum(total_fuel_consumption_quantity, na.rm = TRUE) # consumption in quantity is referred to as "fuel consumption"
  ) %>% ungroup() 

# distributing heat input from only ozone reporting plants to non-ozone months
epa_oz_reporters_dist <- 
  epa_oz_reporters %>%
  group_by(plant_id, prime_mover) %>% 
  left_join(eia_fuel_consum_pm, 
            by = c("plant_id", "prime_mover")) %>% 
  mutate(sum_heat_input_oz = sum(heat_input_oz, na.rm = TRUE)) %>% # sum to plant/pm for distributional proportion 
  ungroup() %>% 
  group_by(plant_id) %>% 
  filter(!any(reporting_frequency == "Q")) %>% # Remove any plants with annual reporters
  ungroup() %>% 
  mutate(prop = if_else(sum_heat_input_oz != 0, heat_input_oz / sum_heat_input_oz, NA_real_),
         heat_input_nonoz = heat_input_nonoz_923 * prop, # distributing nonoz 
         heat_input = if_else(is.na(heat_input_nonoz), heat_input_oz, heat_input_oz + heat_input_nonoz), # annual heat input = distributed non-oz heat + ozone heat
         heat_input_source = if_else(is.na(heat_input) | heat_input == heat_input_oz, "EPA/CAPD", "EIA non-ozone season distributed and EPA/CAPD ozone season"))

# distributing heat input from plants with both annual and ozone reporting units
epa_q_oz_reporters_dist <- 
  epa_oz_reporters %>% 
  group_by(plant_id) %>% 
  filter(any(reporting_frequency == "Q")) %>% # selecting plants that have both annual and ozone reporting units
  ungroup() %>% 
  group_by(plant_id, unit_id, prime_mover, botfirty) %>% 
  mutate(sum_heat_input_oz = sum(heat_input_oz, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(plant_id, prime_mover) %>% 
  left_join(eia_fuel_consum_pm, 
            by = c("plant_id", "prime_mover")) %>% 
  mutate(tot_heat_input = sum(heat_input, na.rm = TRUE),
         sum_heat_input_oz_pm = sum(heat_input_oz, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(reporting_frequency == "OS") %>% # select only ozone reporting units 
  mutate(annual_heat_diff = heat_input_ann_923 - tot_heat_input, # calculating annual heat difference 
         annual_heat_diff_wout_oz = annual_heat_diff - heat_input_oz_923) %>% # calculating difference - ozone month totals to get the leftover nonozone heat input
  filter(annual_heat_diff_wout_oz > 0) %>%  # annual heat difference without ozone must be positive to have heat input to distribute
  mutate(prop = if_else(sum_heat_input_oz_pm != 0, sum_heat_input_oz / sum_heat_input_oz_pm, NA_real_), # calculate proportion of heat input for each unit
         heat_input_nonoz = if_else(annual_heat_diff_wout_oz > 0, annual_heat_diff_wout_oz * prop, 0), # calculate non-ozone heat input 
         heat_input = if_else(is.na(heat_input_nonoz), heat_input_oz, heat_input_oz + heat_input_nonoz), # calculate total heat input
         heat_input_source = if_else(is.na(heat_input) | heat_input == heat_input_oz, "EPA/CAPD", "EIA non-ozone season distributed and EPA/CAPD ozone season")) 
  
# combine ozone reporters datasets 
epa_oz_reporters_dist_2 <- 
  epa_oz_reporters_dist %>% 
  bind_rows(epa_q_oz_reporters_dist)


### Gap fill NOx emissions ozone season reporters -----------

#### Schedule 8c updates ------

# To identify unit level NOx rates, we need to fill EIA-923 Air Emissions & Control Info with Boiler IDs 

# EIA-923 does not have Boiler IDs in the Air & Emissions Control Info 
# We use EIA-860 Boiler data to fill the Boiler IDs
# Because EIA-823 Air & Emissions Control Info lists rows by unique control technologies, 
# we have to update the Boiler ID as a 4-step process:
#     1) match EIA-860 Boiler data for each emission control type 
#     2) update boiler ID using a crosswalk between EIA-860 and EIA-923 data 
#     3) identify where boiler ID is NA, and fill with boiler IDs from each emission control type 
#     4) fill any remaining NA boiler IDs with the NOx control ID 

schedule_8c <- 
  eia_923$air_emissions_control_info %>%
  # join EIA-860 boiler data with unique boiler ID columns 
  left_join(eia_860$boiler_nox %>% select(plant_id, nox_control_id, "boiler_id_nox" = boiler_id) %>% 
              filter(!is.na(boiler_id_nox)) %>% distinct(), 
            by = c("plant_id","nox_control_id")) %>%  
  left_join(eia_860$boiler_so2 %>% select(plant_id, so2_control_id, "boiler_id_so2" = boiler_id) %>% 
              filter(!is.na(boiler_id_so2)) %>% distinct(), 
            by = c("plant_id", "so2_control_id")) %>%  
  left_join(eia_860$boiler_particulate_matter %>% select(plant_id, "pm_control_id" = particulate_matter_control_id, "boiler_id_pm" = boiler_id) %>% 
              filter(!is.na(boiler_id_pm)) %>% distinct(), 
            by = c("plant_id", "pm_control_id")) %>% 
  left_join(eia_860$boiler_mercury %>% select(plant_id, mercury_control_id, "boiler_id_hg" = boiler_id) %>% 
              filter(!is.na(boiler_id_hg)) %>% distinct(), 
            by = c("plant_id", "mercury_control" = "mercury_control_id")) %>% 
  # use control ID crosswalk to match EIA-860 boiler IDs to EIA-923 control IDs
  rows_patch(xwalk_control_ids %>% select(plant_id, "boiler_id_nox" = boiler_id, "nox_control_id" = `923_8c_nox_control_id`) %>% drop_na(),
             by = c("plant_id", "nox_control_id"),
             unmatched = "ignore") %>% 
  rows_patch(xwalk_control_ids %>% select(plant_id, "boiler_id_so2" = boiler_id, "so2_control_id" =  `923_8c_so2_control`) %>% drop_na(),
             by = c("plant_id", "so2_control_id"),
             unmatched = "ignore") %>% 
  rows_patch(xwalk_control_ids %>% select(plant_id, "boiler_id_pm" = boiler_id, "pm_control_id" =  `923_8c_pm_control_id`) %>% drop_na(),
             by = c("plant_id", "pm_control_id"),
             unmatched = "ignore") %>%
  rows_patch(xwalk_control_ids %>% select(plant_id, "boiler_id_hg" = boiler_id, "mercury_control" =  `hg_8c_pm_control_id`) %>% drop_na(),
             by = c("plant_id", "mercury_control"),
             unmatched = "ignore") %>% 
  # assign boiler ID by identifying non-NA IDs
  mutate(boiler_id = case_when(
    !is.na(boiler_id_nox) ~ boiler_id_nox, 
    is.na(boiler_id_nox) & !is.na(boiler_id_so2) ~ boiler_id_so2, 
    is.na(boiler_id_nox) & is.na(boiler_id_so2) & !is.na(boiler_id_pm) ~ boiler_id_pm, 
    is.na(boiler_id_nox) & is.na(boiler_id_so2) & is.na(boiler_id_pm) & !is.na(boiler_id_hg) ~ boiler_id_hg, 
    TRUE ~ NA_character_), 
    boiler_id = if_else(is.na(boiler_id), nox_control_id, boiler_id)) %>% # if boiler ID is still NA, assign NOx control ID as the Boiler ID
  select(-contains("boiler_id_"))

#### NOx Rate ---------

# where available, NOx rates are used to estimates NOx emissions

nox_rates_ann <- # calculating annual NOx emission rates used to estimate NOx emissions
  schedule_8c %>% 
  rename(unit_id = boiler_id) %>%
  select(plant_id, unit_id, nox_emission_rate_entire_year_lbs_mmbtu, status) %>%
  filter(!is.na(nox_emission_rate_entire_year_lbs_mmbtu)) %>% 
  group_by(plant_id, unit_id) %>%
  summarize(nox_rate_ann = min(nox_emission_rate_entire_year_lbs_mmbtu, na.rm = TRUE)) %>% 
  ungroup()

nox_rates_oz <- # calculating ozone NOx emission rates used to estimate NOx emissions
  schedule_8c %>% 
  rename(unit_id = boiler_id) %>%
  select(plant_id, unit_id, nox_emission_rate_may_through_september_lbs_mmbtu, status) %>%
  filter(!is.na(nox_emission_rate_may_through_september_lbs_mmbtu)) %>% 
  group_by(plant_id, unit_id) %>%
  summarize(nox_rate_oz = min(nox_emission_rate_may_through_september_lbs_mmbtu, na.rm = TRUE)) %>% 
  ungroup()

epa_oz_reporters_dist_nox_rates <- # filling annual NOx mass with nox_rates where available
  epa_oz_reporters_dist_2 %>%
  inner_join(nox_rates_ann,
             by = c("plant_id", "unit_id")) %>%
  mutate(id = paste0(plant_id, "_", unit_id, "_", prime_mover),
         nox_nonoz_mass = (heat_input_nonoz * nox_rate_ann) / 2000, 
         nox_mass = if_else(reporting_frequency == "OS", nox_nonoz_mass + nox_oz_mass, nox_mass), 
         nox_source = if_else(is.na(nox_mass), nox_source, "Estimated based on unit-level NOx emission rates and EPA/CAPD ozone season emissions"))


#### NOx EF -----------------

# Joining EFs data frame with EPA ozone reporters to calculate non-ozone NOx mass

epa_oz_reporters_dist_nox_ef <-
  epa_oz_reporters_dist_2 %>% 
  select(-contains("923")) %>% 
  left_join(eia_fuel_consum_fuel_type, 
            by = c("plant_id", "prime_mover", "primary_fuel_type" = "fuel_type")) %>% 
  group_by(plant_id, prime_mover, primary_fuel_type, botfirty) %>% 
  mutate(sum_heat_input_oz = sum(heat_input_oz, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(id = paste0(plant_id, "_", unit_id, "_", prime_mover)) %>% 
  filter(!id %in% epa_oz_reporters_dist_nox_rates$id) %>% # removing units that were filled with nox_rates
  left_join(emission_factors_pu %>%
              filter(nox_unit_flag == "PhysicalUnits") %>% 
              mutate(botfirty = if_else(botfirty %in% c("null", "N/A"), NA_character_, botfirty)) %>% 
              select(prime_mover, botfirty, primary_fuel_type, nox_ef) %>% 
              distinct()) %>% # there are duplicates based on other columns in the emission_factors data. Getting distinct rows.
  mutate(prop = if_else(sum_heat_input_oz != 0, heat_input_oz / sum_heat_input_oz, NA_real_), 
         fuel_consum_nonoz = fuel_consum_nonoz_923 * prop, # calculating distributed non-ozone fuel consumption 
         nox_nonoz_mass = fuel_consum_nonoz * nox_ef / 2000, 
         nox_mass = if_else(reporting_frequency == "OS", nox_nonoz_mass + nox_oz_mass, nox_mass), 
         nox_source = if_else(is.na(nox_mass), "EPA/CAPD", "Estimated using emissions factor and EIA data for non-ozone season and EPA/CAPD ozone season emissions")) 


# Combining two dataframes with distributed annual NOx mass to join back with rest of EPA

distinct_cols <- c("plant_id", "unit_id", "prime_mover", "heat_input", "heat_input_source", "nox_mass", "nox_source")

epa_oz_reporters_dist_final <- # combining all distributed ozone season reporters and keeping only necessary columns 
  epa_oz_reporters_dist_nox_rates %>% 
  distinct(pick(all_of(distinct_cols))) %>%
  bind_rows(epa_oz_reporters_dist_nox_ef %>% 
              distinct(pick(all_of(distinct_cols))), 
            epa_oz_reporters_dist_2 %>% 
              filter(!(paste0(plant_id, "_", unit_id, "_", prime_mover) %in% epa_oz_reporters_dist_nox_ef$id), 
                     !(paste0(plant_id, "_", unit_id, "_", prime_mover) %in% epa_oz_reporters_dist_nox_rates$id)) %>% 
              distinct(pick(all_of(distinct_cols))))  

# check number of ozone reporting units, and compare to number of units that have distributed values
print(glue::glue("There are {nrow(epa_oz_reporters_dist_2)} total OS reporting units. The dataframe with distributed values contains {nrow(epa_oz_reporters_dist_final)}"))

if(nrow(epa_oz_reporters_dist_2) < nrow(epa_oz_reporters_dist_final)) { # check if there are any units with duplicate entries 
  print(glue::glue("There are {nrow(epa_oz_reporters_dist_2)} total OS reporting units. The dataframe with distributed values contains {nrow(epa_oz_reporters_dist_final)}"))
  
  dupe_ids <- 
  epa_oz_reporters_dist_final %>%
    count(plant_id, unit_id, sort =  TRUE) %>% 
    filter(n > 1) %>% 
    mutate(plant_unit = glue::glue("Plant :{plant_id}, Unit: {unit_id}")) %>% 
    pull(plant_unit) %>% 
    str_c(., collapse = "\n")
  
  stop(glue::glue("There are more rows than there should be in the distributed dataframe. There are multiple rows for the following units: {\n dupe_ids}.\n Check for possible sources of duplicate unit_ids."))
} else {
   "The number of rows in the distributed dataframe matches the total OS-reporting units."
}
  

# Updating heat input and emissions for OS reporters in full EPA dataframe

epa_7 <- # Updated with gap-filled OS reporters
  epa_6 %>% 
  rows_update(epa_oz_reporters_dist_final %>% 
                select(plant_id, unit_id, prime_mover, heat_input, heat_input_source) %>% 
                drop_na(), 
              by = c("plant_id", "unit_id", "prime_mover")) %>% # Updating OS reporters with new distributed heat input and source
  rows_update(epa_oz_reporters_dist_final %>% 
                select(plant_id, unit_id, prime_mover, nox_mass, nox_source) %>% 
                drop_na(), 
              by = c("plant_id", "unit_id", "prime_mover")) %>% # Updating OS reporters with new distributed NOx mass and source
  mutate(id = paste0(plant_id, "_", unit_id, "_", prime_mover))
  

# Including EIA units (generators and boilers) -----------

## EIA boilers ----------

# identify manual updates to prime movers
### Note: check for updates or changes each data year ###
prime_mover_corrections <- 
  manual_corrections %>% 
  filter(column_to_update == "prime_mover" & !is.na(prime_mover)) %>% 
  select(plant_id, "boiler_id" = unit_id, prime_mover, update)

prime_mover_corrections_2 <- 
  manual_corrections %>% 
  filter(column_to_update == "prime_mover" & is.na(prime_mover)) %>% 
  select(plant_id, "boiler_id" = unit_id, prime_mover = update)

eia_923_boilers <- 
  eia_923$boiler_fuel_data %>% 
  mutate(across( # calculating monthly unit heat input, based on corresponding consumption and mmbtu_per_unit
                .cols = starts_with("quantity_of_fuel_consumed_"),
                .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "mmbtu_per_unit_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
                .names = "heat_input_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
         heat_input = rowSums(pick(all_of(starts_with("heat_input")))), # getting annual heat_input, summing across all monthly heat columns
         heat_input_oz = rowSums(pick(all_of(paste0("heat_input_",tolower(month.name[5:9])))))) %>%  # summing across ozone months
  select(plant_id, plant_name, plant_state, prime_mover, boiler_id, fuel_type, heat_input, heat_input_oz, total_fuel_consumption_quantity) %>% 
  left_join(prime_mover_corrections, by = c("plant_id", "boiler_id", "prime_mover")) %>% 
  mutate(prime_mover = if_else(!is.na(update), update, prime_mover)) %>% 
  rows_update(prime_mover_corrections_2, by = c("plant_id", "boiler_id"), unmatched = "ignore") %>% 
  select(-update)

eia_923_boilers_grouped <- 
  eia_923_boilers %>% 
  group_by(plant_id,
           boiler_id, 
           prime_mover,
           fuel_type) %>%
  mutate(heat_input = sum(heat_input),
         heat_input_oz = sum(heat_input_oz)) %>% 
  ungroup()

eia_923_boilers_heat <- 
  eia_923_boilers %>% 
  group_by(pick(starts_with("plant")), prime_mover, boiler_id) %>% 
  summarize(heat_input = sum(heat_input),
            heat_input_oz = sum(heat_input_oz)) %>% 
  ungroup()

## Determining primary fuel type for EIA-923 boilers based on type of unit with max fuel consumption 

primary_fuel_types_923_boilers <-  # this will be joined with final dataframe of boilers to be added 
  eia_923_boilers_grouped %>% 
  group_by(plant_id, boiler_id, prime_mover) %>% 
  arrange(plant_id, boiler_id, fuel_type) %>% # order primary fuels alphabetically 
  slice_max(if_else(!is.na(heat_input), heat_input, total_fuel_consumption_quantity), # identifying row with highest heat input to get primary_fuel_type
            n = 1, 
            with_ties = FALSE) %>% # only retain first row, if there are ties, it will take the first fuel alphabetically
  select(plant_id, 
         boiler_id, 
         prime_mover,
         "primary_fuel_type" = fuel_type) %>% 
  ungroup()


# We only want to keep EIA-923 boilers that are: 
# 1) from plants that are not already in EPA
# 2) matches in the EIA-860 combined file
# 3) matches to plant/gen_id in the EIA-860 combined

# getting vectors of ids for 860 tables to filter on
eia_860_boil_gen_ids <- eia_860$boiler_generator %>% mutate(id = paste0(plant_id, "_", boiler_id)) %>% pull(id)
eia_860_combined_ids <- eia_860$combined %>% mutate(id_pm = paste0(plant_id, "_", generator_id, "_", prime_mover)) %>% pull(id_pm)

eia_boilers_to_add <- 
  eia_923_boilers_heat %>% 
  mutate(id = paste0(plant_id, "_", boiler_id), 
         id_pm = paste0(plant_id, "_", boiler_id, "_", prime_mover)) %>%
  filter(!plant_id %in% epa_7$plant_id, # removing boilers that are in plants in EPA
         !plant_id %in% epa_plants_to_delete$plant_id) %>% # removing plants that are in epa_plants_to_delete
  filter(id %in% eia_860_boil_gen_ids | id_pm %in% eia_860_combined_ids) %>%  # keeping only boilers that are in 860, under boiler or unit id
  mutate(heat_input_source = "EIA Unit-level Data",
         heat_input_oz_source = "EIA Unit-level Data") %>% 
  left_join(primary_fuel_types_923_boilers) # adding primary fuel type as determined by fuel type of max heat input of boiler 
  
print(glue::glue("{nrow(eia_923_boilers_heat) - nrow(eia_boilers_to_add)} EIA-923 boilers removed because:\n 
                  1) plant_id is already in EPA, or\n
                  2) boiler does not match plant/boiler in EIA-860 files (EIA-860 Boiler Generator, EIA-860 Combined)"))


## EIA generators -----

# We add additional generators from the EIA-860 (combined file). 
# We remove certain generators: 
  # 1) Generators from plants already included from EPA, unless it is a renewable EIA generator that is not included in EPA
  # 2) Generators that are included within EIA-923 boilers. These are identified using the EIA-860 Boiler Generator file, which serves as a crosswalk between associated boiler and generator ids

# creating unique ids for plant/boiler and plant/generator combos. These will be used to identify units already included from 923 boiler file 
eia_860_boil_gen <- 
  eia_860$boiler_generator %>%
    mutate(id_gen = paste0(plant_id, "_", generator_id), # creating unique id for generator ids
           id_boil = paste0(plant_id, "_", boiler_id)) # creating unique id for boiler id

eia_860_gens_to_remove <-
  eia_860_boil_gen %>%
  ### Note: check for updates or changes each data year ###
  filter(id_boil %in% eia_boilers_to_add$id & plant_id != 50489) %>% # ensure we include plant 50489 generator C3 due to prime mover changes
  pull(id_gen) %>%
  c(., eia_boilers_to_add$id) # adding additional units that are in eia_boilers but not in 860_boiler_generator file


# identify EIA renewable units from plants in EPA. These will be excluded when filtering out EPA plants below
renewable_ids <- 
  eia_860$combined %>% 
  filter(plant_id %in% epa_7$plant_id, # only plants in EPA
         energy_source_1 %in% c("SUN", "WAT", "WND")) %>% # identifying renewable sources
  mutate(id = paste0(plant_id, "_", generator_id, "_", prime_mover)) %>%
  pull(id)
  
eia_860_generators_to_add <- 
  eia_860$combined %>% 
  mutate(id = paste0(plant_id, "_", generator_id),
         id_pm = paste0(plant_id, "_", generator_id, "_", prime_mover)) %>% 
  filter(!(plant_id %in% epa_7$plant_id & !id_pm %in% renewable_ids), # removing generators from plants in EPA, unless it is a renewable generator
         !id %in% eia_860_gens_to_remove, 
         !plant_id %in% epa_plants_to_delete$plant_id) %>%  
  select(plant_id, 
         plant_name,
         plant_state, 
         generator_id, 
         nameplate_capacity,
         prime_mover,
         "operating_status" = status,
         "primary_fuel_type" = energy_source_1, 
         "id" = id_pm)


### Update fuel types where mismatch between EIA sources -----

gen_fuel_types_to_update <- 
  eia_923$generation_and_fuel_combined %>% 
  group_by(plant_id) %>% 
  slice_max(n = 1, total_fuel_consumption_mmbtu) %>% # identifying primary fuel
  ungroup() %>%
  filter(total_fuel_consumption_mmbtu != 0, # where max fuel isn't 0
         !is.na(total_fuel_consumption_mmbtu)) %>% 
  select(plant_id, prime_mover, total_fuel_consumption_mmbtu, fuel_type) %>% 
  inner_join(
    (eia_860$combined %>% # finding primary fuels in 860
       select(plant_id,
              generator_id,
              prime_mover_860 = prime_mover,
              fuel_type_860 = energy_source_1) %>%
       group_by(plant_id) %>% 
       mutate(n_gens = n()) %>% 
       filter(n_gens == 1) %>% # only for plants with 1 generator
       ungroup()), 
    by = c("plant_id")) %>% 
  mutate(diffs = if_else(fuel_type != fuel_type_860, TRUE, FALSE)) %>%  # identifying discrepancies
  filter(diffs == TRUE & fuel_type != "MSN") %>% # exclude MSN since this is eventually updated to MSW later
  mutate(primary_fuel_type = fuel_type) %>% # use EIA-923 fuel type for differences
  select(plant_id, 
         generator_id,
         prime_mover,
         primary_fuel_type)

# update primary fuel types 

eia_860_generators_to_add_2 <- 
  eia_860_generators_to_add %>%
  rows_update(gen_fuel_types_to_update, # updating with new fuel codes
            by = c("plant_id", "generator_id"), unmatched = "ignore") %>% 
  left_join(prime_mover_corrections %>% rename("generator_id" = boiler_id), 
              by = c("plant_id", "generator_id", "prime_mover")) %>% 
  mutate(prime_mover = if_else(!is.na(update), update, prime_mover)) %>% 
  select(-update)


### Add NUC and GEO generators -------
# some generators that are type "NUC" and "GEO" are in EIA-860 but not included in EPA. These don't get added above because their associated plant_ids are in EPA. 
# We add these and distribute heat from EIA-923 file 

nuc_geo_gens_to_add <- 
  eia_860$combined %>% 
  filter(plant_id %in% epa$plant_id,
         energy_source_1 %in% c("NUC", "GEO")) %>% 
  left_join(eia_923$generation_and_fuel_combined %>% select(plant_id, nuclear_unit_id, prime_mover, starts_with("tot"), total_fuel_consumption_mmbtu),
            by = c("plant_id", "generator_id" = "nuclear_unit_id", "prime_mover")) %>% 
  mutate(heat_input = total_fuel_consumption_mmbtu,
         heat_input_oz = rowSums(pick(all_of(heat_923_oz_months)), na.rm = TRUE), 
         heat_input_source = "EIA Prime Mover-level Data", 
         heat_input_oz_source = "EIA Prime Mover-level Data") %>% 
  select(plant_id,
         plant_name, 
         plant_state,
         generator_id,
         prime_mover,
         primary_fuel_type = energy_source_1,
         operating_status = status,
         heat_input,
         heat_input_oz, 
         heat_input_source, 
         heat_input_oz_source)

eia_860_generators_to_add_3 <-
  eia_860_generators_to_add_2 %>%
  bind_rows(nuc_geo_gens_to_add) 


# Include additional biomass units ------

# We include additional biomass units. This is a static table that is created each year after the plant file.
### Note: check for updates or changes each data year ###

biomass_units_to_add <- 
  biomass_units %>% # this CSV is also used while updating coal fuel types, use now to add in biomass plants
  rename("primary_fuel_type" = fuel_type, 
         "plant_id" = plant_code) %>% 
  mutate(plant_id = as.character(plant_id), 
         source = "plant_file_biomass") %>% 
  select(-year)


# Fill missing heat inputs for all units --------

# Now we combine all units, and fill heat inputs where missing with various approaches

## Combine all units ------

### Update prime mover -----------

prime_mover_corrections_3 <- 
  manual_corrections %>% 
  filter(column_to_update == "prime_mover" & !is.na(prime_mover)) %>% 
  select(plant_id, unit_id, prime_mover, update)

primary_fuel_corrections <- 
  manual_corrections %>% 
  filter(column_to_update == "primary_fuel_type") %>% 
  select(plant_id, unit_id, prime_mover, primary_fuel_type = update)

all_units <- # binding all units together, and adding a source column to track row origins
  bind_rows((epa_7 %>% mutate(source = "EPA", id = paste0(plant_id, "_", unit_id, "_", prime_mover))),
            (eia_boilers_to_add %>% select(-id) %>% mutate(source = "923_boilers") %>% 
               rename("id" = id_pm) %>% rename("unit_id" = boiler_id)),
            (eia_860_generators_to_add_3 %>% mutate(source = "860_generators", id = paste0(plant_id, "_", generator_id, "_", prime_mover)) %>% 
               rename("unit_id" = generator_id)),
            (biomass_units_to_add %>% mutate(source = "plant_file", id = paste0(plant_id, "_", unit_id, "_", prime_mover)) %>% 
               filter(!id %in% epa_7$id, 
                      !id %in% eia_boilers_to_add$id, 
                      !id %in% eia_860_generators_to_add_3$id))) %>% 
  select(-update) %>% 
  left_join(prime_mover_corrections_3, by = c("plant_id", "unit_id", "prime_mover")) %>% 
  mutate(prime_mover = if_else(!is.na(update), update, prime_mover)) %>% 
  select(-update) %>% 
  rows_update(primary_fuel_corrections, by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore")

# Remove plants from EIA that are in EPA 
# These plants are duplicated since they have different plant IDs, so we are removing the EIA units 

all_units_2 <- 
  all_units %>% 
  rows_delete(eia_plants_to_delete, by = "plant_id")


# identify units missing heat input values 

units_missing_heat <- # creating separate dataframe of units with missing heat input to update
  all_units_2 %>% 
  filter(is.na(heat_input)) %>% 
  mutate(id = paste0(plant_id, "_", unit_id, "_", prime_mover))

units_missing_heat_w_heat_oz <- # some units have positive ozone heat inputs (heat_input_oz) - identify them here
  units_missing_heat %>% 
  filter(!is.na(heat_input_oz)) %>% pull(id)

print(glue::glue("{nrow(units_missing_heat)} units with missing heat inputs to update."))


## Update heat input with EIA prime-mover level data --------

# We calculate a distributional proportion to distribute heat to generators based on nameplate capacity using 923 gen and fuel and generator file.
# calculating ratio from generator file based on nameplate capacity to distribute heat

gen_file <- # load generator file
  read_rds(glue::glue("data/outputs/{params$eGRID_year}/generator_file.RDS")) 

dist_props <- # determining distributional proportions to distribute heat inputs
  gen_file %>% 
  select(plant_id, generator_id, prime_mover, nameplate_capacity, generation_ann) %>%
  filter(generation_ann != 0) %>% 
  group_by(plant_id, prime_mover) %>% 
  mutate(sum_namecap = sum(nameplate_capacity)) %>%
  ungroup() %>% 
  mutate(prop = if_else(sum_namecap != 0, nameplate_capacity / sum_namecap, NA_real_)) %>% 
  select(plant_id, prime_mover, generator_id, prop) 

distributed_heat_input <- # determining distributional heat input via proportion of nameplate capacity
  dist_props %>% 
  left_join(eia_fuel_consum_pm) %>% 
  mutate(heat_input = prop * heat_input_ann_923,
         heat_input_oz = prop * heat_input_oz_923) %>% 
  select(plant_id, 
         prime_mover, 
         generator_id, 
         heat_input, 
         heat_input_oz) %>% 
  filter(!is.na(heat_input), # keeping only heat inputs that aren't missing or aren't 0
         heat_input != 0) 

units_heat_updated_pm_data <- # dataframe with units having heat input updated by 923 gen and fuel file
  units_missing_heat %>% 
  rows_patch(distributed_heat_input %>% rename("unit_id" = generator_id), # updating heat input and source columns where available
             by = c("plant_id", "unit_id", "prime_mover"),
             unmatched = "ignore") %>% # ignore rows in distributed_heat_input that aren't in units_missing_heat
  filter(!is.na(heat_input)) %>% 
  mutate(heat_input_source = "EIA Prime Mover-level Data", 
         heat_input_oz_source = if_else(id %in% units_missing_heat_w_heat_oz, heat_input_oz_source, "EIA Prime Mover-level Data")) # only update source for NA ozone heat input values

print(glue::glue("{nrow(units_heat_updated_pm_data)} units updated with EIA Prime Mover-level Data. {nrow(units_missing_heat) - nrow(units_heat_updated_pm_data)} with missing heat input remain."))

units_missing_heat_2 <- # creating updated dataframe with remaining missing heat inputs
  units_missing_heat %>% 
  anti_join(units_heat_updated_pm_data, # removing units that were updated with PM data.
            by = c("plant_id", "unit_id", "prime_mover"))


## Update heat input for direct boiler matches ------

### Match units EIA-923 boiler file on plant and boiler id -------

units_heat_updated_boiler_matches <- 
  units_missing_heat_2 %>% 
  inner_join(eia_923_boilers %>% select(plant_id, boiler_id, prime_mover, "total_fuel_consumption_quantity", heat_input, heat_input_oz),
             by = c("plant_id", "unit_id" = "boiler_id", "prime_mover")) %>% 
  group_by(plant_id, unit_id) %>% 
  filter(!is.na(heat_input.y) & heat_input.y != 0) %>%  #keeping only non-missing heat input values and non-zero values
  slice_max(total_fuel_consumption_quantity, n = 1) %>% # taking unit row with highest fuel consumption
  mutate(heat_input = heat_input.y, # replacing heat input with value from 923 boilers
         heat_input_oz = if_else(!is.na(heat_input_oz.x), heat_input_oz.x, heat_input_oz.y), # if ozone heat is missing, use eia_923_boilers
         heat_input_source = "EIA Unit-level Data",
         heat_input_oz_source = if_else(!is.na(heat_input_oz_source), heat_input_oz_source, "EIA Unit-level Data")) %>% 
  select(plant_id, 
         unit_id, 
         prime_mover,
         heat_input,
         heat_input_oz,
         heat_input_source,
         heat_input_oz_source) %>% 
  ungroup()

units_missing_heat_3 <- 
  units_missing_heat_2 %>% 
  anti_join(units_heat_updated_boiler_matches,
            by = c("plant_id", "unit_id", "prime_mover"))

print(glue::glue("{nrow(units_heat_updated_boiler_matches)} units updated with EIA Unit-level Data from direct matches in EIA 923 boiler file. {nrow(units_missing_heat_3)} with missing heat input remain."))


## Update heat input with distributed heat to boilers ----------

# here we estimate heat to be distributed to boilers based on differences between prime mover level heat we have included and what is in EIA-923 Gen and Fuel file

boiler_dist_props <-  # determining distributional proportions for EIA-923 boilers 
  eia_923_boilers %>%
  group_by(plant_id,
           boiler_id,
           prime_mover) %>% 
  slice_max(total_fuel_consumption_quantity, 
            n = 1,
            with_ties = FALSE) %>%  
  ungroup() %>%
  group_by(plant_id, prime_mover) %>% 
  mutate(sum_totfuel = sum(total_fuel_consumption_quantity),
         prop = if_else(sum_totfuel != 0, total_fuel_consumption_quantity / sum_totfuel, NA_real_)) %>% 
  select(plant_id, prime_mover, boiler_id, prop) %>% 
  ungroup()

heat_differences <- # calculating prime mover-level heat differences between units included in unit file and EIA-923 Gen & Fuel file
  all_units_2 %>% 
  rows_update(units_heat_updated_pm_data, by = c("plant_id", "unit_id")) %>% 
  rows_update(units_heat_updated_boiler_matches, by = c("plant_id", "unit_id")) %>% 
  group_by(plant_id, prime_mover) %>% 
  summarize(across(c("heat_input", "heat_input_oz"), ~ sum(.x, na.rm = TRUE))) %>% 
  left_join(eia_fuel_consum_pm) %>% 
  mutate(heat_diff = heat_input_ann_923 - heat_input,
         heat_oz_diff = heat_input_oz_923 - heat_input_oz) %>% 
  filter(heat_diff > 0) %>% # keeping only differences where 923 values are greater than boiler values
  select(plant_id, prime_mover, ends_with("diff")) %>% 
  ungroup()

units_heat_updated_boiler_distributed <- # distributing heat input via proportion to units that do not yet have heat input values
  units_missing_heat_3 %>% 
  select(plant_id, unit_id, prime_mover, starts_with("heat_input")) %>%
  inner_join(boiler_dist_props, # now joining missing heat units with boiler dist group to keep boilers where we have a distributional proportion
            by = c("plant_id", "unit_id" = "boiler_id", "prime_mover")) %>% 
  filter(!is.na(prop)) %>%
  left_join(heat_differences) %>%
  filter(!is.na(heat_diff)) %>% 
  mutate(heat_input = heat_diff * prop,
         heat_input_oz = if_else(!is.na(heat_input_oz), heat_input_oz, heat_oz_diff * prop), # keep ozone heat inputs that already exist
         heat_input_source = "EIA Prime Mover-level Data",
         heat_input_oz_source = if_else(!is.na(heat_input_oz_source), heat_input_oz_source, "EIA Prime Mover-level Data")) %>%
  select(plant_id, 
         unit_id, 
         prime_mover,
         starts_with("heat_input"),
         ends_with("source"))

units_missing_heat_4 <- # identify units still missing heat input
  units_missing_heat_3 %>% 
  anti_join(units_heat_updated_boiler_distributed,
            by = c("plant_id", "unit_id", "prime_mover"))

print(glue::glue("{nrow(units_heat_updated_boiler_distributed)} units updated with EIA Prime Mover-level Data, distributed from 923 Generation and Fuel File. {nrow(units_missing_heat_4)} with missing heat input remain."))


## Updating all units with filled heat input

filled_heat_inputs <- 
  bind_rows(units_heat_updated_pm_data, 
            units_heat_updated_boiler_matches, 
            units_heat_updated_boiler_distributed) %>% 
  select(plant_id, unit_id, prime_mover, heat_input, heat_input_oz, heat_input_source, heat_input_oz_source)


all_units_3 <- 
  all_units_2 %>% 
  rows_update(filled_heat_inputs,
              by = c("plant_id", "unit_id", "prime_mover"))


# Additional updates before emissions ------

## 860 NOx and SO2 controls -----

so2_controls_860 <- # creating DF of so2_controls to update where available
  eia_860$emissions_control_equipment %>% 
  filter(status == "OP") %>%
  select(plant_id, so2_control_id, equipment_type) %>% distinct() %>% # delete any duplicates
  inner_join(eia_860$boiler_so2 %>% select(plant_id, boiler_id, so2_control_id)) %>%  # adding matching boiler ids for so2
  group_by(plant_id, boiler_id) %>% 
  mutate(so2_controls = paste(equipment_type, collapse =  ", ")) %>% 
  ungroup() %>% 
  distinct(plant_id, boiler_id, so2_controls)

nox_controls_860 <- # creating DF of no2_controls to update where available
  eia_860$emissions_control_equipment %>% 
  filter(status == "OP") %>%
  select(plant_id, nox_control_id, equipment_type) %>% distinct() %>% # delete any duplicates
  inner_join(eia_860$boiler_nox %>% select(plant_id, boiler_id, nox_control_id)) %>%  # adding matching boiler ids for so2
  group_by(plant_id, boiler_id) %>% 
  mutate(nox_controls = paste(equipment_type, collapse =  ", ")) %>% 
  ungroup() %>% 
  distinct(plant_id, boiler_id, nox_controls)

## 860 firing types ------------

botfirty_eia <- 
  eia_860$boiler_info_design_parameters %>% 
  select(plant_id, boiler_id, firing_type_1) %>%
  filter(!is.na(firing_type_1)) %>%
  left_join(xwalk_botfirty %>% select("EIA-860", eGRID),
            by = c("firing_type_1" = "EIA-860")) %>%
  select(-firing_type_1)

## 860 Boiler Prime Movers --------

pm_860 <- 
  eia_923$boiler_fuel_data %>% 
  select(plant_id, boiler_id) %>% 
  inner_join(eia_860$operable %>% select(plant_id, generator_id, prime_mover),
             by = c( "plant_id","boiler_id" = "generator_id")) %>% 
  distinct()

## Calculate number of generators -------

num_gens_860 <- # determining count of generator per boiler 
  eia_860$boiler_generator %>% 
  group_by(plant_id, boiler_id) %>% 
  mutate(num_generators = n()) %>% 
  ungroup() %>% 
  distinct(plant_id,
           boiler_id,
           num_generators)

## Update Hg Controls Flag -------

hg_flags_to_update <- # boilers that have strategy == "ACI" get mercury controls updated to "Yes" 
  eia_860$emission_standards_strategies %>%
  filter(mercury_control_existing_strategy_1 == "ACI") %>% 
  select(plant_id, boiler_id) %>% 
  distinct() %>%
  mutate(hg_controls_flag = "Yes")


## Updating units with available values ------

all_units_4 <-  
  all_units_3 %>% 
  rows_patch(nox_controls_860 %>%  # updating with available 860 NOx controls
               rename("unit_id" = "boiler_id"),
             by = c("plant_id", "unit_id"),
             unmatched = "ignore") %>%
  mutate(primary_fuel_type = if_else(primary_fuel_type %in% c("MSB", "MSN"), "MSW", primary_fuel_type)) %>%  # Changing "MSB" and "MSN" fuel codes to "MSW", necessary for EF methodology
  rows_patch(so2_controls_860 %>% # updating with available 860 SO2 controls
               rename("unit_id" = "boiler_id"),
            by = c("plant_id","unit_id"),
            unmatched = "ignore") %>%
  rows_patch(botfirty_eia %>% 
               rename("unit_id" = "boiler_id", 
                      "botfirty" = eGRID),
             by = c("plant_id", "unit_id"),
             unmatched = "ignore") %>%
  rows_patch(pm_860 %>% rename("unit_id" = "boiler_id"), # updating missing prime movers
             by = c("plant_id", "unit_id"),
             unmatched = "ignore") %>% 
  left_join(num_gens_860, # Adding num_generators variable from 860 boiler generator file
            by = c("plant_id", "unit_id" = "boiler_id")) %>%
  left_join(hg_flags_to_update, # Adding HG flag variable
            by = c("unit_id" = "boiler_id", "plant_id")) %>%
  rows_update(og_fuel_types_update %>% select(plant_id, unit_id, "primary_fuel_type" = "fuel_code"), # updating fuel codes for "OG" primary fuel types
              by = c("plant_id", "unit_id"),
              unmatched = "ignore")


# Calculating emissions -----------
  
## SO2 emissions - sulfur content --------
  
### Determine default sulfur content -------

avg_sulfur_content <- 
  eia_923$boiler_fuel_data %>% 
  group_by(plant_id, boiler_id, prime_mover, fuel_type, physical_unit_label) %>% 
  summarize(across(c(starts_with(c("quantity", "mmbtu_")), "total_fuel_consumption_quantity"), ~ sum(.x, na.rm = TRUE)),
            across(starts_with("sulfur_content"), ~ max(.x))) %>% 
  ungroup() %>% 
  mutate(across( # calculating monthly boiler heat input, based on corresponding consumption and mmbtu_per_unit
              .cols = starts_with("quantity_of_fuel_consumed_"),
              .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "mmbtu_per_unit_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
              .names = "heat_input_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
         across( # calculating monthly boiler heat input, based on corresponding consumption and mmbtu_per_unit
           .cols = starts_with("quantity_of_fuel_consumed_"),
           .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "sulfur_content_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
           .names = "sulfur_content_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
         total_heat_input = rowSums(pick(starts_with("heat_input")), na.rm = TRUE),
         avg_sulfur_content = if_else(total_fuel_consumption_quantity > 0, # calculating avg sulfur content with condition to ignore if total_fuel_consumption is 0
                                      rowSums(pick(starts_with("sulfur_content")), na.rm = TRUE) / total_fuel_consumption_quantity, 
                                      rowSums(pick(starts_with("sulfur_content")), na.rm = TRUE) / 1), 
         fuel_type = if_else(fuel_type %in% c("MSN", "MSB"), "MSW", fuel_type)) %>% 
  group_by(plant_id, boiler_id, prime_mover, fuel_type, physical_unit_label) %>% 
  slice_max(total_fuel_consumption_quantity) %>% # take maximum value of total fuel consumption if there are duplicates
  ungroup() %>% 
  arrange(as.numeric(plant_id)) %>%
  select(plant_id, 
         boiler_id,
         prime_mover,
         fuel_type,
         physical_unit_label,
         avg_sulfur_content,
         total_heat_input,
         total_fuel_consumption_quantity
         ) 

avg_sulfur_content_fuel <- # avg sulfur content grouped by fuel type
  avg_sulfur_content %>%
  ungroup() %>% 
  filter(avg_sulfur_content > 0) %>%
  group_by(fuel_type) %>% 
  summarize(avg_sulfur_content = mean(avg_sulfur_content, na.rm = TRUE),
            sd_sulfur_content = sd(avg_sulfur_content, na.rm = TRUE),
            min_sulfur_content = min(avg_sulfur_content),
            max_sufur_content = max(avg_sulfur_content)) %>% 
  arrange(fuel_type) %>% 
  ungroup()


### Estimate SO2 emissions using sulfur content ---------

# combine EF tables 
emission_factors_all <- 
  rbind(emission_factors_pu, 
        emission_factors_hi) %>% 
  distinct()

estimated_so2_emissions_content <- 
  all_units_4 %>% select(plant_id, unit_id, prime_mover, botfirty, primary_fuel_type) %>% 
  inner_join(avg_sulfur_content %>% filter(avg_sulfur_content > 0), 
             by = c("plant_id", "unit_id" = "boiler_id", "primary_fuel_type" = "fuel_type", "prime_mover")) %>% # inner join to only include units with sulfur content
  left_join(schedule_8c %>% 
              select(plant_id, boiler_id, so2_removal_efficiency_rate_at_annual_operating_factor) %>% 
              group_by(plant_id, boiler_id) %>% 
              slice_max(so2_removal_efficiency_rate_at_annual_operating_factor, # identify maximum SO2 removal values for each unit 
                        with_ties = FALSE) %>% 
              ungroup(), 
            by = c("plant_id", "unit_id" = "boiler_id")) %>% 
  mutate(so2_removal_efficiency_rate_at_annual_operating_factor = 
           if_else(is.na(so2_removal_efficiency_rate_at_annual_operating_factor), 0, 
                   so2_removal_efficiency_rate_at_annual_operating_factor)) %>%  
  left_join(emission_factors_all %>%
              select(prime_mover, botfirty, so2_ef, so2_flag, unit_flag, primary_fuel_type) %>% 
              mutate(botfirty = if_else(botfirty %in% c("null", "N/A"), NA_character_, botfirty)) %>% # fill null or N/A botfirty with NA
                       distinct(), 
            by = c("prime_mover", "botfirty", "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S" & !is.na(so2_flag), avg_sulfur_content, 1), # if so2_flag is not S, change avg_sulfur_content to 1
         so2_mass = if_else(unit_flag == "PhysicalUnits", 
                                 (so2_ef * avg_sulfur_content * total_fuel_consumption_quantity * (1 - so2_removal_efficiency_rate_at_annual_operating_factor)) / 2000,
                                 (so2_ef * avg_sulfur_content * total_heat_input * (1 - so2_removal_efficiency_rate_at_annual_operating_factor)) / 2000)
         ) %>% 
  filter(so2_mass >= 0) %>% 
  mutate(so2_source = "Estimated using emissions factor and plant-specific sulfur content") %>% 
  select(plant_id, unit_id, prime_mover, so2_mass, so2_source, unit_flag) %>%
  add_count(plant_id, unit_id, prime_mover, sort = TRUE) %>% # Some units match to multiple EF under different unit_flags. We want default to be "PhysicalUnits", so where there are multiple rows per unit, we take only "PhysicalUnits"
  group_by(plant_id, unit_id, prime_mover) %>% 
  mutate(flags = tolower(paste(unit_flag, collapse = ", "))) %>% ungroup() %>% # listing unit flags to only keep units that have 1 "PhysicalUnits" EF estimation
  filter(unit_flag == "PhysicalUnits" & n == 1 |
         unit_flag == "PhysicalUnits" & n == 2 & flags == "physicalunits, heatinput" | 
         unit_flag != "PhysicalUnits" & n == 1) %>%
  select(-n, -flags) 
  
# determine SO2 mass for PR coal plants 
### Note: check for updates or changes each data year ###

pr_coal_plants <- 
  all_units_4 %>% 
  filter(plant_state == "PR", primary_fuel_type %in% coal_fuels) %>% 
  select(plant_id, primary_fuel_type, prime_mover, botfirty) %>% distinct()

so2_pr <- # calculate average sulfur content and removal rate for coal types by fuel, PM, and botfirty
  avg_sulfur_content %>% 
  left_join(schedule_8c %>% 
              select(plant_id, boiler_id, so2_removal_efficiency_rate_at_annual_operating_factor) %>% 
              group_by(plant_id, boiler_id) %>% 
              slice_max(so2_removal_efficiency_rate_at_annual_operating_factor, # identify max so2 removal rate
                        with_ties = FALSE) %>% ungroup(), 
            by = c("plant_id", "boiler_id")) %>% 
  mutate(botfirty = NA_character_) %>% # create a botfirty column, and fill in with data from all_units_4
  rows_update(all_units_4 %>% select(plant_id, boiler_id = unit_id, botfirty, prime_mover), 
              by = c("plant_id", "boiler_id", "prime_mover"), 
              unmatched = "ignore") %>% 
  filter(fuel_type %in% pr_coal_plants$primary_fuel_type, 
         prime_mover %in% pr_coal_plants$prime_mover, 
         botfirty == "FLUIDIZED") %>% # check each year if botfirty needs to change
  group_by(fuel_type, prime_mover, botfirty) %>% 
  summarize(avg_sulfur_content = mean(avg_sulfur_content, na.rm = TRUE), 
            so2_removal_efficiency_rate_at_annual_operating_factor = mean(so2_removal_efficiency_rate_at_annual_operating_factor, na.rm = TRUE)) %>% 
  ungroup()

estimated_so2_emissions_content_pr <- # estimate SO2 mass for PR coal plants
  eia_923$generation_and_fuel_combined %>% 
  filter(plant_id %in% pr_coal_plants$plant_id & fuel_type %in% pr_coal_plants$primary_fuel_type) %>% 
  select(plant_id, prime_mover, 
         "primary_fuel_type" = fuel_type, 
         physical_unit_label, 
         total_fuel_consumption_quantity, 
         total_fuel_consumption_mmbtu) %>% 
  left_join(dist_props) %>% 
  mutate(total_fuel_consumption_quantity = prop * total_fuel_consumption_quantity, 
         total_heat_input = prop * total_fuel_consumption_mmbtu) %>%
  left_join(so2_pr, by = c("primary_fuel_type" = "fuel_type", "prime_mover")) %>% 
  left_join(emission_factors_all %>%
              select(prime_mover, botfirty, so2_ef, so2_flag, unit_flag, primary_fuel_type),
            by = c("prime_mover", "botfirty", "primary_fuel_type")) %>%
  mutate(so2_mass = if_else(unit_flag == "PhysicalUnits", 
                            (so2_ef * avg_sulfur_content * total_fuel_consumption_quantity * (1 - so2_removal_efficiency_rate_at_annual_operating_factor)) / 2000,
                            (so2_ef * avg_sulfur_content * total_heat_input * (1 - so2_removal_efficiency_rate_at_annual_operating_factor)) / 2000), 
         so2_source = "Estimated using emissions factor and plant-specific sulfur content") %>% 
  select(plant_id, unit_id = generator_id, prime_mover, so2_mass, so2_source) 

### Join sulfur emissions to all units df  --------
  
all_units_5 <- # update all units
  all_units_4 %>%
  rows_patch(estimated_so2_emissions_content %>% select(-unit_flag),  
              by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>% 
  rows_patch(estimated_so2_emissions_content_pr, 
              by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") 


## SO2 emissions - physical units ----------

biomass_consum_edits <- 
  eia_fuel_consum_fuel_type %>% 
  filter(fuel_type %in% c("MSN", "MSB")) %>% 
  mutate(fuel_type = "MSW") %>% 
  group_by(plant_id, prime_mover, fuel_type) %>% 
  summarize(across(contains("923"), 
                   ~ sum(.)))

eia_fuel_consum_fuel_type_2 <- 
  rbind(eia_fuel_consum_fuel_type %>% filter(!fuel_type %in% c("MSN", "MSB")), 
        biomass_consum_edits)

prop_manual_corrections <- 
  manual_corrections %>% 
  filter(column_to_update == "prop") %>% 
  select(plant_id, unit_id, "prop" = update) %>% 
  mutate(prop = as.numeric(prop))

units_estimated_fuel <- # df that will be used to calculate SO2 and NOx emissions with distributed heat input and fuel consumption
  all_units_5 %>% 
  group_by(plant_id, plant_state, prime_mover, primary_fuel_type) %>% 
  mutate(total_heat_input = sum(heat_input, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(heat_input)) %>% 
  mutate(prop = if_else(total_heat_input != 0, heat_input / total_heat_input, 0)) %>% # handle divide by 0 by assigning prop of 0 to zero total_heat_input values
  select(plant_id, 
         plant_state,
         prime_mover, 
         unit_id, 
         botfirty,
         primary_fuel_type,
         prop) %>%
  rows_update(prop_manual_corrections, by = c("plant_id", "unit_id"), unmatched = "ignore") %>% # manually update some proportions due to wrong assignment of 1 instead of 0
  rows_update(og_fuel_types_update %>% select(plant_id, unit_id, primary_fuel_type), # convert to OG fuel type to match EIA-923 data
              by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  left_join(eia_fuel_consum_fuel_type_2,
             by = c("plant_id", "prime_mover", "primary_fuel_type" = "fuel_type")) %>%
  rows_update(og_fuel_types_update %>% select(plant_id, unit_id, "primary_fuel_type" = fuel_code), # convert fuel type back to more specific gas to calculate emissions
              by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  mutate(fuel_consumption = fuel_consum_ann_923 * prop,
         fuel_consumption_oz = fuel_consum_oz_923 * prop,
         heat_input = heat_input_ann_923 * prop,
         heat_input_oz = heat_input_oz_923 * prop) %>% 
  select(-c(ends_with("_923")))   

# calculating sulfur content for coal fuel types by state

default_sulfur_content_coal <- 
  eia_923$boiler_fuel_data %>% 
  group_by(plant_state, fuel_type) %>% 
  summarize(across(c(starts_with(c("quantity", "mmbtu_")), "total_fuel_consumption_quantity"), ~ sum(.x, na.rm = TRUE)),
            across(starts_with("sulfur_content"), ~ max(.x))) %>% 
  ungroup() %>% 
  mutate(across( # calculating monthly boiler heat input, based on corresponding consumption and mmbtu_per_unit
    .cols = starts_with("quantity_of_fuel_consumed_"),
    .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "mmbtu_per_unit_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
    .names = "heat_input_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
    across( # calculating monthly boiler heat input, based on corresponding consumption and mmbtu_per_unit
      .cols = starts_with("quantity_of_fuel_consumed_"),
      .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "sulfur_content_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
      .names = "sulfur_content_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
    total_heat_input = rowSums(pick(starts_with("heat_input")), na.rm = TRUE),
    avg_sulfur_content = if_else(total_fuel_consumption_quantity > 0, # calculating avg sulfur content with condition to ignore if total_fuel_consumption is 0
                                 rowSums(pick(starts_with("sulfur_content")), na.rm = TRUE) / total_fuel_consumption_quantity, 
                                 rowSums(pick(starts_with("sulfur_content")), na.rm = TRUE) / 1)) %>%
  filter(fuel_type %in% coal_fuels & avg_sulfur_content > 0) %>% 
  select(plant_state,
         fuel_type,
         avg_sulfur_content)


### creating physical units table for SO2

emission_factors_so2_pu <- 
  emission_factors_pu %>%
  filter(primary_fuel_type != "OTH" |
           so2_ef != 2.8,
         unit_flag == "PhysicalUnits") %>%
  select(!starts_with("nox")) %>% 
  mutate(botfirty = if_else(botfirty %in% c("null", "N/A"), NA_character_, botfirty)) %>% # fill null or N/A botfirty with NA
  distinct()
  
## creating heat input table for SO2

emission_factors_so2_heat <- 
  emission_factors_hi %>%
  mutate(unit_flag = if_else(unit_flag == "heatinput", "HeatInput", unit_flag)) %>%
  filter(unit_flag == "HeatInput") %>%
  select(!starts_with("nox")) %>% 
  mutate(botfirty = if_else(botfirty %in% c("null", "N/A"), NA_character_, botfirty)) %>%  # fill null or N/A botfirty with NA
  distinct()

### estimating so2 emissions - coal --------

so2_emissions_pu_coal <-
  units_estimated_fuel %>% 
  inner_join(default_sulfur_content_coal, 
            by = c("plant_state",
                   "primary_fuel_type" = "fuel_type")) %>% 
  left_join(emission_factors_so2_pu %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S" & !is.na(so2_flag), avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "PhysicalUnits",
                            so2_ef * avg_sulfur_content * fuel_consumption / 2000, 
                            so2_ef * avg_sulfur_content * heat_input / 2000)) %>% 
  filter(so2_mass >= 0 & !is.na(heat_input)) %>% 
  select(plant_id,
         unit_id,
         prime_mover,
         so2_mass) %>% 
  mutate(so2_source = "Estimated using emissions factor") 

### estimating so2 emissions - non coal ----------

so2_emissions_pu_noncoal <- 
  units_estimated_fuel %>% 
  left_join(avg_sulfur_content_fuel %>% select(fuel_type, avg_sulfur_content), 
            by = c("primary_fuel_type" = "fuel_type")) %>% 
  left_join(emission_factors_so2_pu %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S" & !is.na(so2_flag), avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "PhysicalUnits",
                            so2_ef * avg_sulfur_content * fuel_consumption / 2000, 
                            so2_ef * avg_sulfur_content * heat_input / 2000)) %>% 
  filter(so2_mass >= 0 & !is.na(heat_input)) %>% 
  select(plant_id,
         unit_id,
         prime_mover,
         so2_mass) %>% 
  mutate(so2_source = "Estimated using emissions factor") 
  
  
### estimating so2 emissions with heat input - coal ------

so2_emissions_heat_coal <- 
  units_estimated_fuel %>% 
  rows_update(all_units_5 %>% # update NA heat inputs with those from the unit file
                select(plant_id, unit_id, prime_mover, heat_input) %>% 
                filter(!is.na(heat_input)) %>% 
                distinct(), 
              by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>% 
  left_join(default_sulfur_content_coal, # renaming for consistent naming
            by = c("primary_fuel_type" = "fuel_type",
                   "plant_state")) %>% 
  left_join(emission_factors_so2_heat %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S" & !is.na(so2_flag), avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "HeatInput",
                            so2_ef * avg_sulfur_content * heat_input / 2000, 0)) %>%
  filter(so2_mass >= 0 & !is.na(heat_input)) %>% 
  select(plant_id,
         unit_id,
         prime_mover,
         so2_mass) %>% 
  mutate(so2_source = "Estimated using emissions factor")


### estimating so2 emissions with heat input - non coal ---------

so2_emissions_heat_noncoal <- 
  units_estimated_fuel %>% 
  rows_update(all_units_5 %>% # update NA heat inputs with those from the unit file
                select(plant_id, unit_id, prime_mover, heat_input) %>% 
                filter(!is.na(heat_input)) %>% 
                distinct(), 
              by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>% 
  left_join(avg_sulfur_content_fuel %>% select(fuel_type, avg_sulfur_content), # renaming for consistent naming
            by = c("primary_fuel_type" = "fuel_type")) %>%
  left_join(emission_factors_so2_heat %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S" & !is.na(so2_flag), avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "HeatInput",
                            so2_ef * avg_sulfur_content * heat_input / 2000, 0)) %>%
  filter(so2_mass >= 0 & !is.na(heat_input)) %>% 
  select(plant_id,
         unit_id,
         prime_mover,
         so2_mass) %>% 
  mutate(so2_source = "Estimated using emissions factor") 


### Updating units with estimated SO2 --------

# update all_units DF
all_units_6 <- 
  all_units_5 %>% 
  rows_patch(so2_emissions_pu_coal, 
             by = c("plant_id", "unit_id", "prime_mover")) %>% 
  rows_patch(so2_emissions_pu_noncoal, 
             by = c("plant_id", "unit_id", "prime_mover")) %>% 
  rows_patch(so2_emissions_heat_coal, 
             by = c("plant_id", "unit_id", "prime_mover")) %>% 
  rows_patch(so2_emissions_heat_noncoal, 
             by = c("plant_id", "unit_id", "prime_mover"))

## CO2 emissions --------

### Estimating CO2 emissions --------  

co2_emissions <- 
  all_units_6 %>%
  select(plant_id, unit_id, primary_fuel_type, prime_mover, heat_input, co2_mass) %>%
  inner_join(co2_ef %>%
              filter(!is.na(eia_fuel_code)) %>%
              select(eia_fuel_code, co2_ef), by = c("primary_fuel_type" = "eia_fuel_code")) %>%
  mutate(co2_mass = heat_input * co2_ef, 
         co2_source = "Estimated using emissions factor") %>%
  select(-c(co2_ef, heat_input, primary_fuel_type)) %>% 
  filter(!is.na(co2_mass))

### Updating units with estimated CO2 emissions --------

all_units_7 <- 
  all_units_6 %>% 
  rows_patch(co2_emissions %>% distinct(),
             by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") 

## NOx Emissions -----

### NOx emissions - rate -------

nox_emissions_ann <- 
  all_units_7 %>% select(plant_id, unit_id, prime_mover, heat_input) %>% 
  inner_join(nox_rates_ann) %>% 
  mutate(nox_mass = (nox_rate_ann * heat_input) / 2000,
         nox_source = "Estimated based on unit-level NOx emission rates") %>%
  select(-nox_rate_ann) %>% 
  filter(!is.na(nox_mass))

nox_emissions_oz <- 
  all_units_7 %>% select(plant_id, unit_id, prime_mover, heat_input_oz) %>% 
  inner_join(nox_rates_oz) %>% 
  mutate(nox_oz_mass = (nox_rate_oz * heat_input_oz) / 2000,
         nox_oz_source = "Estimated based on unit-level NOx ozone season emission rates") %>%
  select(-nox_rate_oz) %>% 
  filter(!is.na(nox_oz_mass))

nox_emissions_rates <- # update all_units DF
  all_units_7 %>%
  rows_patch(nox_emissions_ann,
            by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>%
  rows_patch(nox_emissions_oz, 
             by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") 

### NOx emissions - emissions factor -----

### creating NOx physical units table

emission_factors_nox_pu <- 
  emission_factors_pu %>%
  filter(nox_unit_flag == "PhysicalUnits") %>%
  select(!starts_with("so2") & -c(unit_flag)) %>%
  mutate(botfirty = if_else(botfirty %in% c("null", "N/A"), NA_character_, botfirty)) %>% # some null or NA botfirty are text, fill with NA
  distinct() 
  

#### estimating NOx annual emissions with EF --------

nox_emissions_factor <-
  units_estimated_fuel %>% 
  left_join(emission_factors_nox_pu %>%
              select(prime_mover, primary_fuel_type, botfirty, nox_ef, nox_ef_num, nox_ef_denom), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  group_by(plant_id, unit_id) %>%
  mutate(nox_mass = (fuel_consumption * nox_ef) / 2000,
         nox_source = "Estimated using emissions factor") %>%
  ungroup() %>% 
  filter(nox_mass >= 0 & !is.na(heat_input)) %>% 
  select(plant_id,
         unit_id,
         prime_mover,
         nox_mass, 
         nox_source) 


#### estimating NOx ozone emissions with EF --------

nox_oz_emissions_factor <- 
  units_estimated_fuel %>% 
  left_join(emission_factors_nox_pu %>%
              select(prime_mover, primary_fuel_type, botfirty, nox_ef, nox_ef_num, nox_ef_denom), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  group_by(plant_id, unit_id) %>%
  mutate(nox_oz_mass = (fuel_consumption_oz * nox_ef) / 2000,
         nox_oz_source = "Estimated using emissions factor") %>% 
  ungroup() %>% 
  filter(nox_oz_mass >= 0 & !is.na(heat_input)) %>% 
  select(plant_id,
         unit_id,
         prime_mover,
         nox_oz_mass, 
         nox_oz_source) 


#### estimating NOx annual emissions with heat input --------

emission_factors_nox_hi <- 
  emission_factors_hi %>%
  filter(nox_unit_flag == "HeatInput") %>%
  select(!starts_with("so2") & -c(unit_flag)) %>%
  mutate(botfirty = if_else(botfirty %in% c("null", "N/A"), NA_character_, botfirty)) %>% # some null or NA botfirty are text, fill with NA
  distinct() 

nox_emissions_heat_input <- 
  nox_emissions_rates %>%
  rows_update(all_units_7 %>% # update NA heat inputs with those from the unit file
                select(plant_id, unit_id, prime_mover, heat_input) %>% 
                filter(!is.na(heat_input)) %>% 
                distinct(), 
              by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>% 
  inner_join(emission_factors_nox_hi %>%
              select(prime_mover, primary_fuel_type, botfirty, nox_ef, nox_ef_num, nox_ef_denom), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  select(plant_id, unit_id, primary_fuel_type, prime_mover, heat_input, nox_ef, nox_ef, nox_ef_num, nox_ef_denom) %>%
  group_by(plant_id, unit_id, primary_fuel_type, nox_ef, nox_ef_num, nox_ef_denom) %>%
  mutate(nox_mass = (heat_input * nox_ef) / 2000,
         nox_source = "Estimated using emissions factor") %>%
  ungroup() %>%
  filter(nox_mass >= 0 & !is.na(heat_input)) %>% 
  select(plant_id,
         unit_id,
         prime_mover,
         primary_fuel_type, 
         nox_mass,
         nox_source) 

#### estimating NOx ozone emissions with heat input --------

nox_oz_emissions_heat_input <- 
  nox_emissions_rates %>%
  rows_update(all_units_7 %>% # update NA heat inputs with those from the unit file
                select(plant_id, unit_id, prime_mover, heat_input) %>% 
                filter(!is.na(heat_input)) %>% 
                distinct(), 
              by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>% 
  left_join(emission_factors_nox_hi %>% 
              select(prime_mover, primary_fuel_type, botfirty, nox_ef, nox_ef_num, nox_ef_denom), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  select(plant_id, unit_id, primary_fuel_type, prime_mover, heat_input_oz, heat_input, nox_ef, nox_ef_num, nox_ef_denom) %>%
  group_by(unit_id, plant_id, primary_fuel_type, nox_ef, nox_ef_num, nox_ef_denom) %>%
  mutate(nox_oz_mass = (heat_input_oz * nox_ef) / 2000,
         nox_oz_source = "Estimated using emissions factor") %>%
  ungroup() %>%
  filter(nox_oz_mass >= 0 & !is.na(heat_input)) %>% 
  select(plant_id,
         unit_id,
         prime_mover,
         nox_oz_mass,
         nox_oz_source) 

#### Updating units with estimating NOx --------

all_units_8 <- 
  nox_emissions_rates %>%
  rows_patch(nox_emissions_factor,
             by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>%
  rows_patch(nox_oz_emissions_factor, 
             by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>%
  rows_patch(nox_emissions_heat_input,
             by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>% 
  rows_patch(nox_oz_emissions_heat_input,
             by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") 


#### Check ozone season NOx ----------
# IF NOx ozone emissions > annual NOx emissions, make the NOx ozone emissions equal the annual NOx emissions

all_units_9 <- 
  all_units_8 %>%
  mutate(nox_oz_mass = case_when(nox_oz_mass > nox_mass ~ nox_mass,
                                 TRUE ~ nox_oz_mass))

## Geothermal Emissions --------

geo_emissions <- 
  all_units_9 %>%
  filter(primary_fuel_type == "GEO") %>% 
  left_join(nrel_geo_type %>% filter(table_flag == "new"), 
            by = "plant_id") %>% 
  rows_patch(nrel_geo_type %>% filter(table_flag == "old"), # gap fill with old geothermal data for units that do not have a geo_code in newest data table
                by = "plant_id", unmatched = "ignore") %>% 
  left_join(geo_emission_factors , by = "geo_type_code") %>% 
  mutate(nox_mass = (heat_input * nox_ef_lb_mmbtu) / 2000, 
         nox_oz_mass = (heat_input_oz * nox_ef_lb_mmbtu) / 2000, 
         so2_mass = (heat_input * so2_ef_lb_mmbtu) / 2000, 
         co2_mass = (heat_input * co2_ef_lb_mmbtu) / 2000, 
         nox_source = if_else(!is.na(nox_mass), "Estimated using emissions factor", NA_character_), 
         nox_oz_source = if_else(!is.na(nox_oz_mass), "Estimated using emissions factor", NA_character_), 
         so2_source = if_else(!is.na(so2_mass), "Estimated using emissions factor", NA_character_), 
         co2_source = if_else(!is.na(co2_mass), "Estimated using emissions factor", NA_character_)) %>% 
  select(plant_id, unit_id, nox_mass, nox_oz_mass, so2_mass, co2_mass, 
         nox_source, nox_oz_source, so2_source, co2_source)


# update all_units with geothermal emissions 

all_units_10 <- 
  all_units_9 %>% 
  rows_patch(geo_emissions, by = c("plant_id", "unit_id"))

# Final modifications -----  

## Add stack height ----------

stack_info <- 
  all_units_10 %>% 
  left_join(eia_860$boiler_stack_flue %>% select(plant_id, # match boiler ID to stack flue ID
                                                 boiler_id, 
                                                 stack_flue_id), 
            by = c("plant_id", "unit_id" = "boiler_id")) %>% 
  inner_join(eia_860$stack_flue %>% select(plant_id, # merge in stack flue status and stack height using stack flue ID
                                          "stack_flue_id" = stack_or_flue_id, 
                                          stack_flue_status, 
                                          "stack_height" = stack_height_feet),
            by = c("plant_id", "stack_flue_id")) %>% 
  group_by(plant_id, unit_id, prime_mover) %>% 
  filter(stack_flue_status == "OP") %>% # only include operating stacks
  mutate(stack_height = as.numeric(stack_height)) %>% # convert stack height to numeric
  slice_max(stack_height) %>% # only include highest stack 
  ungroup() %>% 
  select(plant_id, unit_id, prime_mover, stack_height) %>% 
  distinct()

## Clean up source flags --------

clean_source_flags <- 
  all_units_10 %>% 
  mutate(nox_source = replace(nox_source, is.na(nox_mass), NA_character_), # replacing NA emission masses with NA sources
         so2_source = replace(so2_source, is.na(so2_mass), NA_character_), 
         co2_source = replace(co2_source, is.na(co2_mass), NA_character_), 
         hg_source = replace(hg_source, is.na(hg_mass), NA_character_))


## Change necessary plant names -------
# Check for duplicate plant names and default to EPA names

check_plant_names <- 
  all_units_10 %>% select(plant_id, plant_name, source) %>% 
  group_by(plant_id, plant_name) %>% 
  distinct() %>% ungroup() %>% 
  group_by(plant_id) %>% 
  filter(n() > 1 & !is.na(plant_name), 
         source == "EPA") %>% # default to EPA names
  ungroup()
  
  
## Update FC prime mover CO2 emissions data ------
# Update FC prime mover to null CO2 emissions 

update_fc_data <- 
  all_units_10 %>% select(plant_id, unit_id, prime_mover, co2_mass, co2_source) %>% 
  filter((prime_mover == "FC") & ((!is.na(co2_mass)) | (!is.na(co2_source)))) %>%  # only update necessary rows 
  mutate(co2_mass = NA_character_, 
         co2_source = NA_character_, 
         co2_mass = as.numeric(co2_mass), 
         co2_source = as.character(co2_source)) %>% 
  select(plant_id, unit_id, prime_mover, co2_mass, co2_source) 

## Delete specified units -------- 
### Delete units in "Units to remove" table, which is manually updated each year ----------

# See static table units_to_remove that is loaded in "Crosswalk and static tables" section

### Delete retired units  ------- 
# delete units with operating status "RE" based on operating status ("RE") from EIA-860 Boiler Info & Design Parameters 
# check if the unit retires before the current data year

delete_retired_units <- 
  eia_860$boiler_info_design_parameters %>% 
  left_join(all_units_10 %>% select(plant_id, unit_id, operating_status), 
            by = c("plant_id", "boiler_id" = "unit_id")) %>% 
  filter(boiler_status == "RE" & 
           (as.numeric(retirement_year) < as.numeric(params$eGRID_year) | is.na(retirement_year)) &
           (operating_status == "RE" | is.na(operating_status))) %>% 
  select(plant_id, 
         unit_id = "boiler_id", 
         boiler_status, retirement_year)


### Delete noted EIA units ------ 
### Note: check for updates or changes each data year ###

eia_units_to_delete <- 
  manual_corrections %>% # check if manual_corrections has any units to delete 
  filter(column_to_update == "delete") %>% 
  select(plant_id) %>% distinct()


## Update status ------
# Update operating status via EIA-860 Boiler Info and Design Parameters for boilers and EIA-860 Generator file for EIA generators

update_status_generators <- # operating status for generators updated via EIA-860 
  all_units_10 %>% select(plant_id, unit_id, operating_status, source) %>% 
  filter(source == "860_generators", is.na(operating_status)) %>% # only update status for those that are NA
  rows_patch(eia_860$combined %>% select(plant_id, "operating_status" = status, "unit_id" = generator_id), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  mutate(operating_status = toupper(operating_status)) %>% 
  filter(!is.na(operating_status)) %>% distinct() %>% select(-source)
  
  
update_status_boilers <- # operating status for boilers updated via EIA-860 Boiler Info and Design Parameters
  all_units_10 %>% select(plant_id, unit_id, operating_status, source) %>% 
  filter(source == "923_boilers", is.na(operating_status)) %>% # only update status for those that are NA
  rows_patch(eia_860$boiler_info_design_parameters %>% select(plant_id, "unit_id" = boiler_id, "operating_status" = boiler_status), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  mutate(operating_status = toupper(operating_status)) %>% 
  filter(!is.na(operating_status)) %>% distinct() %>% select(-source)


## Create and edit EPA flag ----- 

# ID EPA units 
epa_units <- epa_7 %>% pull(id)
epa_units_no_pm <- epa_7 %>% mutate(id = paste0(plant_id, "_", unit_id)) %>% pull(id) # PR crosswalk does not have PM data, just match on plant and unit ID

# update PR EPA flag from crosswalk
update_pr_epa_flag <- 
  xwalk_pr_oris %>% 
  select("plant_id" = "eia_plant_id", 
         "unit_id" = "eia_unit_id", 
         "plant_name" = "eia_plant_name") %>% 
  mutate(id = paste0(plant_id, "_", unit_id), 
         capd_flag = if_else(id %in% epa_units_no_pm, "Yes", NA_character_)) %>% 
  filter(capd_flag == "Yes")

# create CAPD flag 
all_units_11 <- 
  all_units_10 %>%  
  mutate(capd_flag = if_else(paste0(plant_id, "_", unit_id, "_", prime_mover) %in% epa_units, "Yes", NA_character_)) %>% 
  rows_update(update_pr_epa_flag, by = c("plant_id", "unit_id"))


## Implement changes in main unit file ------

# plant ID and plant name manual corrections 
plant_id_corrections <- 
  manual_corrections %>% filter(column_to_update == "plant_id") %>% 
  select(plant_id, plant_id_update = update)

plant_name_corrections <- 
  manual_corrections %>% filter(column_to_update == "plant_name") %>% 
  select(plant_id, plant_name_update = update)

# emissions and heat input manual corrections
heat_emissions_corrections <- 
  manual_corrections %>% filter(plant_id == "52152") %>% 
  pivot_wider(id_cols = c("plant_id", "unit_id", "prime_mover"), 
              names_from = column_to_update, 
              values_from = update) %>% 
  mutate(heat_input = as.numeric(heat_input), 
         heat_input_oz = as.numeric(heat_input_oz), 
         nox_mass = as.numeric(nox_mass), 
         nox_oz_mass = as.numeric(nox_oz_mass), 
         so2_mass = as.numeric(so2_mass), 
         co2_mass = as.numeric(co2_mass))
  

all_units_12 <- 
  all_units_11 %>% # update to most recent unit file data frame
  left_join(stack_info, by = c("plant_id", "unit_id", "prime_mover")) %>% 
  left_join(plant_id_corrections, by = c("plant_id")) %>% 
  mutate(plant_id = if_else(!is.na(plant_id_update), plant_id_update, plant_id)) %>% 
  left_join(plant_name_corrections, by = c("plant_id")) %>% 
  mutate(plant_name = if_else(!is.na(plant_name_update), plant_name_update, plant_name)) %>% 
  rows_delete(units_to_remove, 
                by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_delete(delete_retired_units, 
                by= c("plant_id", "unit_id"), unmatched = "ignore") %>%
  rows_delete(eia_units_to_delete, 
                by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(update_fc_data, 
                by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_update(check_plant_names, 
                by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(heat_emissions_corrections, 
              by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_patch(update_status_generators, 
                by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_patch(update_status_boilers, 
                by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  mutate(heat_input = round(heat_input, 3), 
         heat_input_oz = round(heat_input_oz, 3), 
         nox_mass = round(nox_mass, 3), 
         nox_oz_mass = round(nox_oz_mass, 3), 
         so2_mass = round(so2_mass, 4), 
         co2_mass = round(co2_mass, 3), 
         hg_mass = round(hg_mass, 3)) 

# Format unit file --------------

# creating named vector of final variable order and variable name included in unit file
final_vars <-
  c("SEQUNT" = "sequnt",
    "YEAR" = "year",
    "PSTATABB" = "plant_state",
    "PNAME" = "plant_name",
    "ORISPL" = "plant_id",
    "UNITID" = "unit_id",
    "PRMVR" =  "prime_mover",
    "UNTOPST" = "operating_status",
    "CAPDFLAG" = "capd_flag", 
    "PRGCODE" = "program_code", 
    "BOTFIRTY" = "botfirty", 
    "NUMGEN" = "num_generators", 
    "FUELU1" = "primary_fuel_type",
    "HRSOP" = "operating_hours", 
    "HTIAN" = "heat_input", 
    "HTIOZ" = "heat_input_oz",
    "NOXAN" = "nox_mass",
    "NOXOZ" = "nox_oz_mass", 
    "SO2AN" = "so2_mass",
    "CO2AN" = "co2_mass",
    "HGAN" = "hg_mass",
    "HTIANSRC" = "heat_input_source",
    "HTIOZSRC" = "heat_input_oz_source",
    "NOXANSRC" = "nox_source", 
    "NOXOZSRC" = "nox_oz_source", 
    "SO2SRC" = "so2_source", 
    "CO2SRC" = "co2_source", 
    "HGSRC" = "hg_source", 
    "SO2CTLDV" = "so2_controls", 
    "NOXCTLDV" = "nox_controls", 
    "HGCTLDV" = "hg_controls_flag", 
    "UNTYRONL" = "year_online", 
    "STACKHT" = "stack_height")

units_formatted <-
  all_units_12 %>%
  arrange(plant_state, plant_name) %>% 
  mutate(sequnt = row_number(), 
         year = params$eGRID_year) %>% 
  select(as_tibble(final_vars)$value) %>% # keeping columns with tidy names for QA steps
  drop_na(plant_id, unit_id)

# Export unit file -------------

if(dir.exists("data/outputs")) {
  print("Folder outputs already exists.")
} else {
   dir.create("data/outputs")
}

if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
  print(glue::glue("Folder outputs/{params$eGRID_year} already exists."))
} else {
   dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
}

print(glue::glue("Saving unit file to folder data/outputs/{params$eGRID_year}"))

write_rds(units_formatted, glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))){
  print(glue::glue("File unit_file.RDS successfully written to folder data/outputs/{params$eGRID_year}"))
} else {
  print("File unit_file.RDS failed to write to folder.")
} 

