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
##      Caroline Watson, Abt Global, caroline.watson@abtglobal.com
##      Teagan Goforth, Abt Global, teagan.goforth@abtglobal.com
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

params <- list()
params$eGRID_year <- "2021"

# Load necessary data ------

## CAMD ------
camd_vars_to_keep <- 
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


camd <- 
  read_rds("data/clean_data/camd/camd_clean.RDS") %>% 
  select(all_of(camd_vars_to_keep)) # keeping only necessary variables

## EIA ------------

eia_860 <- read_rds("data/clean_data/eia/eia_860_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia/eia_923_clean.RDS")


# Modifying CAMD data ---------

## Harmonizing fields with EIA ----------

# vectors of unit types matched to their respective EIA prime mover values
pm_st <- c("BFB", "C", "CB", "CFB", "DB", "DTF", "DVF", "IGC", "KLN", "OB", "PRH", "S", "T", "WBF", "WBT")
pm_gt <- c("AF", "CT")
pm_ct <- c("CC")
pm_ot <- c("OT")


camd_2 <- 
  camd %>% 
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
      plant_id == 2707 & unit_id == 1 ~ "GT1",
      plant_id == 2707 & unit_id == 2 ~ "GT2",
      plant_id == 2707 & unit_id == 3 ~ "GT3",
      plant_id == 2707 & unit_id == 4 ~ "GT4",
      TRUE ~ unit_id) 
  )


### Update fuel types ---------

#### Update coal fuel types ---------

# Units identified in CAMD as "Coal" are matched via an EPA / EIA crosswalk
# Energy_source_1 for Coal units default to EIA-860 
# Plants with Coal and Biomass units require a manual update to Coal fuel types, ignoring max fuel consumption from Biomass fuel types

# Power Sector Data Crosswalk matches units between CAMD and EIA data sets
# this will be used to help update Coal units in CAMD and assign correct primary fuel type
xwalk_eia_epa <- read_csv("data/static_tables/xwalk_epa_eia_power_sector.csv") %>% janitor::clean_names() %>% 
  mutate(camd_plant_id = as.character(camd_plant_id), eia_plant_id = as.character(eia_plant_id)) %>% 
  filter(camd_fuel_type == "Coal") %>% # extract only Coal units 
  select(camd_plant_id, camd_unit_id, camd_fuel_type, eia_plant_id, eia_generator_id, eia_fuel_type, eia_unit_type)

# identify coal fuels in CAMD use crosswalk to identify primary fuel type  
coal_fuel_type_update <- 
  xwalk_eia_epa %>% 
  left_join(camd_2, by = c("camd_plant_id" = "plant_id", "camd_unit_id" = "unit_id", "eia_unit_type" = "prime_mover")) %>%  # use power sector crosswalk to update energy source 1 in EIA-860 
  inner_join(eia_860$combined, by = c("eia_plant_id" = "plant_id", "eia_generator_id" = "generator_id", "eia_unit_type" = "prime_mover")) %>% 
  mutate(energy_source_1 = eia_fuel_type) %>% 
  select(plant_id = camd_plant_id, 
         unit_id = camd_unit_id, 
         prime_mover = eia_unit_type, 
         energy_source_1) %>%
  drop_na() %>% distinct() %>% group_by(plant_id, unit_id) %>% filter(!n() > 1) # only include units with 1 fuel type listed

# some plants have both biomass and coal units, and biomass fuel types may have highest fuel consumption
# for CAMD identified coal units, we want to default to Coal fuel types for the primary fuel type assignment
biomass_units <- 
  read_csv("data/static_tables/biomass_units_to_add_to_unit_file.csv")  

coal_biomass_plants <- 
  camd_2 %>% filter(primary_fuel_type == "Coal" & plant_id %in% biomass_units$plant_code) %>% 
  left_join(eia_923$generation_and_fuel_combined %>%  # joining generation and fuel file, based on max fuel consumption by plant
              select(plant_id, prime_mover, fuel_type, total_fuel_consumption_mmbtu) %>%
              filter(fuel_type %in% c("ANT", "BIT", "LIG", "SUB", "RC", "WC", "SGC", "COG")) %>% 
              group_by(plant_id, prime_mover) %>%
              slice_max(total_fuel_consumption_mmbtu, n = 1, with_ties = FALSE) %>% # identify fuel type associate with max fuel consumption by plant
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

# update CAMD with primary fuel types by matching EIA fuel codes or using EIA data  
camd_3 <-
  camd_2 %>%
  left_join(eia_860$combined %>% # joining with 860_combined to get EIA primary fuel codes
              distinct(plant_id, generator_id, energy_source_1), 
            by = c("plant_id", "unit_id" = "generator_id")) %>% 
  rows_patch(coal_fuel_type_update, by = c("plant_id", "unit_id", "prime_mover"), unmatched = "ignore") %>% # update Coal energy sources
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
  rows_update(coal_biomass_plants, by = c("plant_id", "unit_id"), unmatched = "ignore") %>% # update Coal units from plants that also have biomass units
  left_join(eia_923$generation_and_fuel_combined %>%  # joining generation and fuel file, based on max fuel consumption by plant
              select(plant_id, prime_mover, fuel_type, total_fuel_consumption_mmbtu) %>%
              group_by(plant_id, prime_mover) %>%
              slice_max(total_fuel_consumption_mmbtu, n = 1, with_ties = FALSE) %>% # identify fuel type associate with max fuel consumption by plant
              select(plant_id, prime_mover, fuel_type),
            by = c("plant_id", "prime_mover")) %>% 
  mutate(primary_fuel_type = if_else(is.na(primary_fuel_type), fuel_type, primary_fuel_type)) %>%  # filling missing primary_fuel_types with 923 value
  select(-fuel_type) 


## identify units remaining with missing primary_fuel_types
missing_fuel_types <- 
  camd_3 %>% 
  filter(is.na(camd_3$primary_fuel_type))

print(glue::glue("{nrow(missing_fuel_types)} units having missing primary fuel types."))


### Add boiler firing type ------------
# A crosswalk is used to create a boiler firing type variable, based on unit type. 
# some units have more specific boiler firing types available in the 860 boiler info file
# and these are used where available
### Note: check for updates or changes each data year ###

xwalk_botfirty <- read_csv("data/static_tables/xwalk_boiler_firing_type.csv")

camd_4 <- 
  camd_3 %>% 
  left_join(xwalk_botfirty,
            by = c("unit_type" = "CAMD/EPA")) %>% 
  mutate(botfirty = eGRID) %>% 
  select(-any_of(names(xwalk_botfirty)))

botfirty_to_update <- # matching with 860 data to see if more specific botfirty type exists
  camd_4 %>% 
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


camd_5 <- # updating units where available
  camd_4 %>% 
  left_join(botfirty_to_update) %>% 
  mutate(botfirty = if_else(!is.na(eGRID), eGRID, botfirty)) %>% 
  select(-eGRID) 


### Delete specified plants -----------
# Some plants in CAMD are not connected to the grid or are retired, so they are excluded from eGRID
### Note: check for updates or changes each data year ###

camd_plants_to_delete <- 
  read_csv("data/static_tables/camd_plants_to_delete.csv") %>% 
  select("plant_id" = "ORIS Code") %>% 
  mutate(plant_id = as.character(plant_id)) 

# delete plants with null heat inputs for all units
# some plants will be added back in from EIA data
camd_null_heat_input <- 
  camd_5 %>% group_by(plant_id) %>% 
  filter(all(is.na(heat_input))) %>% 
  select(plant_id) %>% distinct()

camd_6 <- 
  camd_5 %>% 
  rows_delete(camd_plants_to_delete, by = c("plant_id"), unmatched = "ignore") %>% # delete specified plants from CAMD dataframe
  rows_delete(camd_null_heat_input, by = c("plant_id"), unmatched = "ignore") # delete plants with null heat input


## Gap fill CAMD ozone season reporters with EIA data ----------

# a.	Some CAMD plants are “ozone season reporters” – which means that they only report data during the ozone season, which is May to September each year. 
# b.	These plants are listed in the CAMD data with a frequency variable of “OS” instead of “Q” (quarterly)
# c.	We gap fill the missing months (January, February, March, April, October, November, and December) with EIA data. 

### Gap fill heat input for OS reporters --------------

camd_oz_reporter_plants <- # getting list of plant ids that include ozone season reporters. Note that some plants include both OS only and OS and Annual reporters.
  camd_6 %>% 
  filter(reporting_frequency == "OS") %>% 
  pull(plant_id)

camd_oz_reporters <- # creating dataframe with all plants and associated units that include "OS" reporters.
  camd_6 %>%
  filter(plant_id %in% camd_oz_reporter_plants)


### Summing heat input values in the EIA-923 Gen and Fuel file. These totals will be used to distribute heat for the non-ozone months in the CAMD data.
### We also add consumption totals here, which will be used later when estimating NOx emissions.

heat_923_oz_months <- paste0("tot_mmbtu_", tolower(month.name)[5:9]) # creating vector of monthly heat columns for 923 gen and fuel
heat_923_nonoz_months <- paste0("tot_mmbtu_", tolower(month.name)[c(1:4,10:12)]) # creating vector of monthly heat columns for 923 gen and fuel
consum_923_nonoz_months <- paste0("quantity","_", tolower(month.name)[c(1:4,10:12)]) # creating vector of monthly consumption
consum_923_oz_months <- paste0("quantity","_", tolower(month.name)[5:9])

eia_fuel_consum_pm <- # summing fuel and consum to pm level
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
            fuel_consum_ann_923 = sum(total_fuel_consumption_quantity, na.rm = TRUE) # consumption in quantity is referred to as "fuel consumpsion"
  )

# distributing heat input from only ozone reporting plants to non-ozone months
camd_oz_reporters_dist <- 
  camd_oz_reporters %>%
  group_by(plant_id, prime_mover) %>% 
  left_join(eia_fuel_consum_pm, 
            by = c("plant_id", "prime_mover")) %>% 
  mutate(sum_heat_input_oz = sum(heat_input_oz, na.rm = TRUE)) %>% # sum to plant/pm/ for distributional proportion 
  ungroup() %>% 
  group_by(plant_id) %>% 
  filter(!any(reporting_frequency == "Q")) %>% # Remove any plants with annual reporters
  ungroup() %>% 
  mutate(prop = heat_input_oz / sum_heat_input_oz,
         heat_input_nonoz = heat_input_nonoz_923 * prop, # distributing nonoz 
         heat_input = heat_input_oz + heat_input_nonoz, # annual heat input = distributed non-oz heat + ozone heat
         heat_input_source = if_else(is.na(heat_input), "EPA/CAMD", "EIA non-ozone season distributed and EPA/CAMD ozone season"))

# distributing heat input from plants with both annual and ozone reporting units
camd_q_oz_reporters_dist <- 
  camd_oz_reporters %>% 
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
  mutate(prop = sum_heat_input_oz / sum_heat_input_oz_pm, # calculate proportion of heat input for each unit
         heat_input_nonoz = annual_heat_diff_wout_oz * prop, # calculate non-ozone heat input 
         heat_input = heat_input_nonoz + heat_input_oz, # calculate total heat input
         heat_input_source = if_else(is.na(heat_input), "EPA/CAMD", "EIA non-ozone season distributed and EPA/CAMD ozone season")) 
  
# combine ozone reporters datasets 
camd_oz_reporters_dist_2 <- 
  camd_oz_reporters_dist %>% 
  bind_rows(camd_q_oz_reporters_dist)


### Gap fill NOx emissions ozone season reporters -----------

#### NOx Rate ---------

# where available, NOx rates are used to estimates NOx emissions

nox_rates <- # calculating NOx emission rates used to estimate NOx emissions
  eia_923$air_emissions_control_info %>% 
  left_join(eia_860$boiler_nox %>% select(plant_id, nox_control_id, boiler_id),
            by = c("plant_id", "nox_control_id")) %>% 
  group_by(plant_id, boiler_id, nox_control_id) %>%
  summarize(nox_rate_ann = max(nox_emission_rate_entire_year_lbs_mmbtu),
            nox_rate_oz = max(nox_emission_rate_may_through_september_lbs_mmbtu)) 

camd_oz_reporters_dist_nox_rates <- # filling annual NOx mass with nox_rates where available
  camd_oz_reporters_dist_2 %>%
  inner_join(nox_rates,
             by = c("plant_id", "unit_id" = "boiler_id")) %>%
  mutate(nox_nonoz_mass = (heat_input_nonoz * nox_rate_ann) / 2000, 
         nox_mass = nox_nonoz_mass + nox_oz_mass, # I think there should be a reporting frequency condition here. 
         nox_source = if_else(is.na(nox_mass), nox_source ,"Estimated based on unit-level NOx emission rates and EPA/CAMD ozone season emissions"))

nox_rates_ids <- # creating unique ids to filter out later
  camd_oz_reporters_dist_nox_rates %>% 
  mutate(id = paste0(plant_id, prime_mover, unit_id)) %>% 
  pull(id)


#### NOx EF -----------------

emission_factors <- read_csv("data/static_tables/emission_factors.csv") # load in emission_factors data


# Joining EFs data frame with CAMD ozone reporters to calculate non-ozone NOx mass

camd_oz_reporters_dist_nox_ef <-
  camd_oz_reporters_dist %>% 
  filter(!paste0(plant_id, prime_mover, unit_id) %in% nox_rates_ids) %>% # removing units that were filled with nox_rates
  left_join(emission_factors %>%
              filter(nox_unit_flag == "PhysicalUnits") %>% # (SB: This doesn't happen in access but I think is necessary) 
              select(prime_mover, botfirty, primary_fuel_type, nox_ef) %>% 
              distinct()) %>% # there are duplicates based on other columns in the emission_factors data. Getting distinct rows.
  mutate(fuel_consum_nonoz = fuel_consum_nonoz_923 * prop, # calculating distributed non-ozone fuel consumption (SB: Need to check if this needs to be calculated at a different level)
         nox_nonoz_mass = fuel_consum_nonoz * nox_ef / 2000,
         nox_mass = nox_oz_mass + nox_nonoz_mass,
         nox_source = if_else(is.na(nox_mass), "EPA/CAMD", "Estimated using emissions factor and EIA data for non-ozone season and EPA/CAMD ozone season emissions")) 

# Combining two dataframes with distributed annual NOx mass to join back with rest of CAMD

distinct_cols <- c("plant_id", "unit_id", "heat_input", "heat_input_source", "nox_mass", "nox_source")

camd_oz_reporters_dist_final <- # combining all distributed ozone season reporters and keeping only necessary columns 
  camd_oz_reporters_dist_nox_rates %>% 
  distinct(pick(all_of(distinct_cols))) %>%
  bind_rows(camd_oz_reporters_dist_nox_ef %>% 
              distinct(pick(all_of(distinct_cols))))

# check number of ozone reporting units, and compare to number of units that have distributed values
print(glue::glue("There are {nrow(camd_oz_reporters_dist)} total OS reporting units. The dataframe with distributed values contains {nrow(camd_oz_reporters_dist_final)}"))

if(nrow(camd_oz_reporters_dist) < nrow(camd_oz_reporters_dist_final)) { # check if there are any units with duplicate entries 
  print(glue::glue("There are {nrow(camd_oz_reporters_dist)} total OS reporting units. The dataframe with distributed values contains {nrow(camd_oz_reporters_dist_final)}"))
  
  dupe_ids <- 
  camd_oz_reporters_dist_final %>%
    count(plant_id, unit_id, sort =  TRUE) %>% 
    filter(n > 1) %>% 
    mutate(plant_unit = glue::glue("Plant :{plant_id}, Unit: {unit_id}")) %>% 
    pull(plant_unit) %>% 
    str_c(., collapse = "\n")
  
  stop(glue::glue("There are more rows than there should be in the distributed dataframe. There are multiple rows for the following units: {\n dupe_ids}.\n Check for possible sources of duplicate unit_ids."))
} else{
  "The number of rows in the distributed dataframe matches the total OS-reporting units."
}
  

# Updating heat input and emissions for OS reporters in full CAMD dataframe

camd_7 <- # Updated with gap-filled OS reporters
  camd_6 %>% 
  rows_update(camd_oz_reporters_dist_final, # Updating OS reporters with new distributed heat inputs, nox_mass, and the source variables for both
              by = c("plant_id","unit_id")) %>% 
  mutate(id = paste0(plant_id, "_", unit_id))
  

# Including EIA units (generators and boilers) -----------

## EIA boilers ----------

eia_923_boilers <- 
  eia_923$boiler_fuel_data %>% 
  mutate(across( # calculating monthly unit heat input, based on corresponding consumption and mmbtu_per_unit
                .cols = starts_with("quantity_of_fuel_consumed_"),
                .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "mmbtu_per_unit_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
                .names = "heat_input_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
         heat_input = rowSums(pick(all_of(starts_with("heat_input")))), # getting annual heat_input, summing across all monthly heat columns
         heat_input_oz = rowSums(pick(all_of(paste0("heat_input_",tolower(month.name[5:9])))))) %>%  # summing across ozone months
  select(plant_id, plant_name, plant_state, prime_mover, boiler_id, fuel_type, heat_input, heat_input_oz, total_fuel_consumption_quantity)  

eia_923_boilers_grouped <- 
  eia_923_boilers %>% 
  group_by(plant_id,
           boiler_id, 
           prime_mover,
           fuel_type) %>% 
  mutate(heat_input = sum(heat_input),
         heat_input_oz = sum(heat_input_oz))

eia_923_boilers_heat <- 
  eia_923_boilers %>% 
  group_by(pick(starts_with("plant")), prime_mover, boiler_id) %>% 
  summarize(heat_input = sum(heat_input),
            heat_input_oz = sum(heat_input_oz)) 

## Determining primary fuel type for EIA-923 boilers based on type of unit with max fuel consumption 

primary_fuel_types_923_boilers <-  # this will be joined with final dataframe of boilers to be added 
  eia_923_boilers_grouped %>% 
  group_by(plant_id, boiler_id) %>% 
  arrange(plant_id, boiler_id, fuel_type) %>% # order primary fuels alphabetically 
  slice_max(heat_input, # identifying row with highest heat input to get primary_fuel_type
            n = 1, 
            with_ties = FALSE) %>% # only retain first row, if there are ties, it will take the first fuel alphabetically
  select(plant_id, 
         boiler_id, 
         "primary_fuel_type" = fuel_type) %>% 
  ungroup()


# We only want to keep EIA-923 boilers that are: 
# 1) from plants that are not already in CAMD
# 2) matches in the EIA-860 combined file
# 3) matches to plant/gen_id in the EIA-860 combined

# getting vectors of ids for 860 tables to filter on
eia_860_boil_gen_ids <- eia_860$boiler_generator %>% mutate(id = paste0(plant_id, "_", boiler_id)) %>% pull(id)
eia_860_combined_ids <- eia_860$combined %>% mutate(id = paste0(plant_id, "_", generator_id)) %>% pull(id)


eia_boilers_to_add <- 
  eia_923_boilers_heat %>% 
  mutate(id = paste0(plant_id, "_", boiler_id)) %>%
  filter(!plant_id %in% camd_7$plant_id) %>%  # removing boilers that are in plants in CAMD
  filter(id %in% eia_860_boil_gen_ids | id %in% eia_860_combined_ids) %>%  # keeping only boilers that are in 860, under boiler or unit id
  mutate(heat_input_source = "EIA Unit-level Data",
         heat_input_oz_source = "EIA Unit-level Data") %>% 
  left_join(primary_fuel_types_923_boilers) %>%  # adding primary fuel type as determined by fuel type of max heat input of boiler 
  group_by(plant_id, boiler_id) %>% # some units have multiple prime movers, identify prime mover with max heat input
  slice_max(heat_input, 
            with_ties = FALSE)
  
print(glue::glue("{nrow(eia_923_boilers_heat) - nrow(eia_boilers_to_add)} EIA-923 boilers removed because:\n 
                  1) plant_id is already in CAMD, or\n
                  2) boiler does not match plant/boiler in EIA-860 files (EIA-860 Boiler Generator, EIA-860 Combined)"))


## EIA generators -----

# We add additional generators from the EIA-860 (combined file). 
# We remove certain generators: 
  # 1) Generators from plants already included from CAMD, unless it is a renewable EIA generator that is not included in CAMD
  # 2) Generators that are included within EIA-923 boilers. These are identified using the EIA-860 Boiler Generator file, which serves as a crosswalk between associated boiler and generator ids

# creating unique ids for plant/boiler and plant/generator combos. These will be used to identify units already included from 923 boiler file 
eia_860_boil_gen <- 
  eia_860$boiler_generator %>%
    mutate(id_gen = paste0(plant_id, "_", generator_id), # creating unique id for generator ids
           id_boil = paste0(plant_id, "_", boiler_id)) # creating unique id for boiler id

eia_860_gens_to_remove <-
  eia_860_boil_gen %>%
  filter(id_boil %in% eia_boilers_to_add$id) %>%
  pull(id_gen) %>%
  c(., eia_boilers_to_add$id) # adding additional units that are in eia_boilers but not in 860_boiler_generator file


# identify EIA renewable units from plants in CAMD. These will be excluded when filtering out CAMD plants below
renewable_ids <- 
  eia_860$combined %>% 
  filter(plant_id %in% camd_7$plant_id, # only plants in CAMD
         energy_source_1 %in% c("SUN", "WAT", "WND")) %>% # identifying renewable sources
  mutate(id = paste0(plant_id, "_", generator_id)) %>%
  pull(id)
  
eia_860_generators_to_add <- 
  eia_860$combined %>% 
  mutate(id = paste0(plant_id, "_", generator_id)) %>% 
  filter(!(plant_id %in% camd_7$plant_id & !id %in% renewable_ids), # removing generators from plants in CAMD, unless it is a renewable generator
         !id %in% eia_860_gens_to_remove) %>%  
  select(plant_id, 
         plant_name,
         plant_state, 
         generator_id, 
         nameplate_capacity,
         prime_mover,
         "operating_status" = status,
         "primary_fuel_type" = energy_source_1)


### Update fuel types where mismatch between EIA sources -----

gen_fuel_types_to_update <- 
  eia_923$generation_and_fuel_combined %>% 
  group_by(plant_id, prime_mover) %>% 
  slice_max(n = 1, total_fuel_consumption_quantity) %>% # identifying primary fuel
  ungroup() %>%
  filter(total_fuel_consumption_quantity != 0, # where max fuel isn't 0
         !is.na(total_fuel_consumption_quantity)) %>% 
  select(plant_id, prime_mover, total_fuel_consumption_quantity, fuel_type) %>% 
  inner_join(
    (eia_860_generators_to_add %>% # finding primary fuels in 860
       select(plant_id,
              generator_id,
              prime_mover,
              fuel_type_860 = primary_fuel_type) %>%
       group_by(plant_id) %>% 
       mutate(n_gens = n()) %>% 
       filter(n_gens == 1)), # only for plants with 1 generator
    by = c("plant_id", "prime_mover")) %>% 
  mutate(diffs = if_else(fuel_type != fuel_type_860, TRUE, FALSE)) %>%  # identifying discrepancies
  filter(diffs == TRUE) %>% 
  mutate(primary_fuel_type = fuel_type) %>% # use EIA-923 fuel type for differences
  select(plant_id, 
         generator_id,
         prime_mover,
         primary_fuel_type)

# update primary fuel types 

eia_860_generators_to_add_2 <- 
  eia_860_generators_to_add %>%
  rows_update(gen_fuel_types_to_update, # updating with new fuel codes
            by = c("plant_id", "generator_id")) 


### Add NUC and GEO generators -------
# some generators that are type "NUC" and "GEO" are in EIA-860 but not included in CAMD. These don't get added above because their associated plant_ids are in CAMD. 
# We add these and distribute heat from EIA-923 file 

nuc_geo_gens_to_add <- 
  eia_860$combined %>% 
  filter(plant_id %in% camd$plant_id,
         energy_source_1 %in% c("NUC", "GEO")) %>% 
  left_join(eia_923$generation_and_fuel_combined %>% select(plant_id, nuclear_unit_id, prime_mover, starts_with("tot"), total_fuel_consumption_mmbtu),
            by = c("plant_id", "generator_id" = "nuclear_unit_id", "prime_mover")) %>% 
  mutate(heat_input = total_fuel_consumption_mmbtu,
         heat_input_oz = rowSums(pick(all_of(heat_923_oz_months)), na.rm = TRUE)) %>% 
  select(plant_id,
         plant_name, 
         plant_state,
         generator_id,
         prime_mover,
         primary_fuel_type = energy_source_1,
         operating_status = status,
         heat_input,
         heat_input_oz)

eia_860_generators_to_add_3 <-
  eia_860_generators_to_add_2 %>%
  bind_rows(nuc_geo_gens_to_add) %>% 
  mutate(id = paste0(plant_id, "_", generator_id))


# Include additional biomass units ------

# We include additional biomass units. This is a static table that is created each year after the plant file.
### Note: check for updates or changes each data year ###

biomass_units <- 
  biomass_units %>% # this CSV is read in while updating coal fuel types
  rename("primary_fuel_type" = fuel_type, 
         "plant_id" = plant_code) %>% 
  mutate(plant_id = as.character(plant_id))


# Fill missing heat inputs for all units --------

# Now we combine all units, and fill heat inputs where missing with various approaches

## Combine all units ------

all_units <- # binding all units together, and adding a source column to track row origins
  bind_rows((camd_7 %>% mutate(source = "CAMD", id = paste0(plant_id, "_", unit_id))),
            (eia_boilers_to_add %>% mutate(source = "923_boilers", id = paste0(plant_id, "_", boiler_id)) %>% 
               rename("unit_id" = boiler_id)),
            (eia_860_generators_to_add_3 %>% mutate(source = "860_generators", id = paste0(plant_id, "_", generator_id)) %>% 
               rename("unit_id" = generator_id)), 
            (biomass_units %>% mutate(source = "plant_file", id = paste0(plant_id, "_", unit_id)) %>% 
               filter(!id %in% camd_7$id, 
                      !id %in% eia_boilers_to_add$id, 
                      !id %in% eia_860_generators_to_add_3$id)))  # filter out plant and units already in other data sources
  

# Remove plants from EIA that are in CAMD 
# These plants are duplicated since they have different plant IDs, so we are removing the EIA units 

eia_plants_to_delete <- read_csv("data/static_tables/xwalk_oris_camd.csv") %>% 
  mutate(plant_id = as.character(eia_plant_id)) %>% select(plant_id)

all_units_2 <- 
  all_units %>% 
  rows_delete(eia_plants_to_delete, by = "plant_id")


# identify units missing heat input values 

units_missing_heat <- # creating separate dataframe of units with missing heat input to update
  all_units_2 %>% 
  filter(is.na(heat_input)) %>% 
  mutate(id = paste0(plant_id, "_", unit_id))

units_missing_heat_w_heat_oz <- # some units have positive ozone heat inputs (heat_input_oz) - identify them here
  units_missing_heat %>% 
  filter(!is.na(heat_input_oz)) %>% pull(id)

print(glue::glue("{nrow(units_missing_heat)} units with missing heat inputs to update."))

## Update heat input with EIA prime-mover level data --------

# We calculate a distributional proportion to distribute heat to generators based on nameplate capacity using 923 gen and fuel and generator file.
# calculating ratio from generator file based on nameplate capacity to distribute heat

gen_file <- # load generator file
  read_rds("data/outputs/generator_file.RDS") 

dist_props <- # determining distributional proportions to distribute heat inputs
  gen_file %>% 
  select(plant_id, generator_id, prime_mover, nameplate_capacity) %>%
  group_by(plant_id, prime_mover) %>% 
  mutate(sum_namecap = sum(nameplate_capacity)) %>%
  ungroup() %>% 
  mutate(prop = nameplate_capacity / sum_namecap) %>% 
  select(plant_id, prime_mover, generator_id, prop) 

distributed_heat_input <- # determining distributional heat input via proportion of nameplate capacity
  dist_props %>% 
  left_join(eia_fuel_consum_pm %>% select(plant_id, prime_mover, heat_input_ann_923, heat_input_oz_923)) %>% 
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
  rows_patch(distributed_heat_input %>% rename("unit_id" = generator_id, -prime_mover), # updating heat input and source columns where available
              by = c("plant_id", "unit_id"),
              unmatched = "ignore") %>% # ignore rows in distributed_heat_input that aren't in units_missing_heat
  filter(!is.na(heat_input)) %>% 
  mutate(heat_input_source = "EIA Prime Mover-level Data", 
         heat_input_oz_source = if_else(id %in% units_missing_heat_w_heat_oz, heat_input_oz_source, "EIA Prime Mover-level Data")) # only update source for NA ozone heat input values

print(glue::glue("{nrow(units_heat_updated_pm_data)} units updated with EIA Prime Mover-level Data. {nrow(units_missing_heat) - nrow(units_heat_updated_pm_data)} with missing heat input remain."))


units_missing_heat_2 <- # creating updated dataframe with remaining missing heat inputs
  units_missing_heat %>% 
  anti_join(units_heat_updated_pm_data, # removing units that were updated with PM data.
            by = c("plant_id", "unit_id"))

## Update heat input for direct boiler matches ------

### Match units EIA-923 boiler file on plant and boiler id -------

units_heat_updated_boiler_matches <- 
  units_missing_heat_2 %>% 
  inner_join(eia_923_boilers %>% select(plant_id, boiler_id, "total_fuel_consumption_quantity", heat_input, heat_input_oz),
             by = c("plant_id", "unit_id" = "boiler_id")) %>% 
  group_by(plant_id,
           unit_id) %>% 
  filter(!is.na(heat_input.y) & heat_input.y != 0) %>%  #keeping only non-missing heat input values and non-zero values
  slice_max(total_fuel_consumption_quantity, n = 1) %>% # taking unit row with highest fuel consumption
  mutate(heat_input = heat_input.y, # replacing heat input with value from 923 boilers
         heat_input_oz = if_else(!is.na(heat_input_oz.x), heat_input_oz.x, heat_input_oz.y), # if ozone heat is missing, use eia_923_boilers
         heat_input_source = "EIA Unit-level Data",
         heat_input_oz_source = if_else(!is.na(heat_input_oz_source), heat_input_oz_source, "EIA Unit-level Data")) %>% 
  select(plant_id, 
         unit_id, 
         heat_input,
         heat_input_oz,
         heat_input_source,
         heat_input_oz_source) 

units_missing_heat_3 <- 
  units_missing_heat_2 %>% 
  anti_join(units_heat_updated_boiler_matches,
            by = c("plant_id", "unit_id"))

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
         prop = total_fuel_consumption_quantity / sum_totfuel) %>% 
  select(plant_id, prime_mover, boiler_id, prop)

heat_differences <- # calculating prime mover-level heat differences between units included in unit file and EIA-923 Gen & Fuel file
  all_units_2 %>% 
  rows_update(units_heat_updated_pm_data, by = c("plant_id", "unit_id")) %>% 
  rows_update(units_heat_updated_boiler_matches, by = c("plant_id", "unit_id")) %>% 
  group_by(plant_id, prime_mover) %>% 
  summarize(across(c("heat_input", "heat_input_oz"), ~ sum(.x, na.rm = TRUE))) %>% 
  left_join(eia_fuel_consum_pm %>% select(plant_id, prime_mover, starts_with("heat"))) %>% 
  mutate(heat_diff = heat_input_ann_923 - heat_input,
         heat_oz_diff = heat_input_oz_923 - heat_input_oz) %>% 
  filter(heat_diff > 0) %>% # keeping only differences where 923 values are greater than boiler values
  select(plant_id, prime_mover, ends_with("diff"))

units_heat_updated_boiler_distributed <- # distributing heat input via proportion to units that do not yet have heat input values
  units_missing_heat_3 %>% 
  select(plant_id, unit_id, starts_with("heat_input")) %>%
  inner_join(boiler_dist_props, # now joining missing heat units with boiler dist group to keep boilers where we have a distributional proportion
            by = c("plant_id", "unit_id" = "boiler_id")) %>% 
  filter(!is.na(prop)) %>%
  left_join(heat_differences) %>%
  filter(!is.na(heat_diff)) %>% 
  mutate(heat_input = heat_diff * prop,
         heat_input_oz = if_else(!is.na(heat_input_oz), heat_input_oz, heat_oz_diff * prop), # keep ozone heat inputs that already exist
         heat_input_source = "EIA Prime Mover-level Data",
         heat_input_oz_source = if_else(!is.na(heat_input_oz_source), heat_input_oz_source, "EIA Prime Mover-level Data")) %>%
  select(plant_id, 
         unit_id, 
         starts_with("heat_input"),
         ends_with("source"))

units_missing_heat_4 <- # identify units still missing heat input
  units_missing_heat_3 %>% 
  anti_join(units_heat_updated_boiler_distributed,
            by = c("plant_id", "unit_id"))

print(glue::glue("{nrow(units_heat_updated_boiler_distributed)} units updated with EIA Prime Mover-level Data, distributed from 923 Generation and Fuel File. {nrow(units_missing_heat_4)} with missing heat input remain."))


## Updating all units with filled heat input

filled_heat_inputs <- 
  bind_rows(units_heat_updated_pm_data, 
            units_heat_updated_boiler_matches, 
            units_heat_updated_boiler_distributed) %>% 
  select(plant_id, unit_id, heat_input, heat_input_oz, heat_input_source, heat_input_oz_source)


all_units_3 <- 
  all_units_2 %>% 
  rows_update(filled_heat_inputs,
              by = c("plant_id", "unit_id"))


# Additional updates before emissions ------

## 860 NOx and SO2 controls -----

so2_controls_860 <- # creating DF of so2_controls to update where available
  eia_860$emissions_control_equipment %>% 
  filter(status == "OP") %>%
  select(plant_id, so2_control_id, equipment_type) %>% 
  inner_join(eia_860$boiler_so2 %>% select(plant_id, boiler_id, so2_control_id)) %>%  # adding matching boiler ids for so2
  group_by(plant_id, boiler_id) %>% 
  mutate(so2_controls = paste(equipment_type, collapse =  ", ")) %>% 
  ungroup() %>% 
  distinct(plant_id, boiler_id, so2_controls)

nox_controls_860 <- # creating DF of no2_controls to update where available
  eia_860$emissions_control_equipment %>% 
  filter(status == "OP") %>%
  select(plant_id, nox_control_id, equipment_type) %>% 
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


## Update OG fuel types  --------
### Note: check for updates or changes each data year ###

og_fuel_types_update <- 
  read_csv("data/static_tables/og_oth_units_to_change_fuel_type.csv") %>% 
  mutate(across(everything(), ~ as.character(.x))) %>%
  select(plant_id, unit_id, fuel_code)

## Schedule 8c updates ------

# Load crosswalk to add additional boiler IDs
### Note: check for updates or changes each data year ###

xwalk_control_ids <- # xwalk to add additional boiler ids
  read_csv("data/static_tables/xwalk_860_boiler_control_id.csv") %>% 
  mutate(across(everything(), as.character))
  

schedule_8c_nox <- 
  eia_923$air_emissions_control_info %>%
  distinct(plant_id, nox_control_id, pick(starts_with("nox_emission_rate"))) %>% 
  filter(!is.na(nox_emission_rate_entire_year_lbs_mmbtu)) %>% 
  left_join(eia_860$boiler_nox %>% select(plant_id, nox_control_id, boiler_id) %>% filter(!is.na(boiler_id)) %>% distinct(), ## SB: causes m:m join
            by = c("plant_id","nox_control_id")) %>% 
  rows_patch(xwalk_control_ids %>% select(plant_id, boiler_id, "nox_control_id" = `860_nox_control_id`) %>% drop_na(),
             by = c("plant_id", "nox_control_id"),
             unmatched = "ignore") 

schedule_8c_so2 <-
  eia_923$air_emissions_control_info %>% 
  distinct(plant_id, so2_control_id, so2_removal_efficiency_rate_at_annual_operating_factor) %>% 
  filter(!is.na(so2_removal_efficiency_rate_at_annual_operating_factor),
         !is.na(so2_control_id)) %>%
  left_join(eia_860$boiler_so2 %>% select(plant_id, so2_control_id, boiler_id) %>% filter(!is.na(boiler_id)) %>% distinct(), 
            by = c("plant_id","so2_control_id")) %>% 
  rows_patch(xwalk_control_ids %>% select(plant_id, boiler_id,"so2_control_id" =  `860_so2_control_id`) %>% drop_na(),
             by = c("plant_id", "so2_control_id"),
             unmatched = "ignore") %>% 
  select(-so2_control_id) %>%
  group_by(plant_id, boiler_id) %>% 
  slice_max(so2_removal_efficiency_rate_at_annual_operating_factor, 
            with_ties = FALSE)

schedule_8c_pm <-
  eia_923$air_emissions_control_info %>% 
  distinct(plant_id, pm_control_id, pm_emissions_rate_lbs_mmbtu) %>% 
  filter(!is.na(pm_emissions_rate_lbs_mmbtu),
         !is.na(pm_control_id)) %>%
  left_join(eia_860$boiler_particulate_matter %>% select(plant_id, "pm_control_id" = particulate_matter_control_id, boiler_id) %>% filter(!is.na(boiler_id)) %>% distinct(), 
            by = c("plant_id","pm_control_id")) %>% 
  rows_patch(xwalk_control_ids %>% select(plant_id, boiler_id,"pm_control_id" =  `860_pm_control_id`) %>% drop_na(),
             by = c("plant_id", "pm_control_id"),
             unmatched = "ignore")
  
schedule_8c_hg <- 
  eia_923$air_emissions_control_info %>% 
  distinct(plant_id, mercury_control, mercury_emission_rate) %>% 
  filter(!is.na(mercury_emission_rate),
         !is.na(mercury_control)) %>%
  left_join(eia_860$boiler_mercury %>% select(plant_id, mercury_control_id, boiler_id) %>% filter(!is.na(boiler_id)) %>% distinct(), 
            by = c("plant_id", "mercury_control" = "mercury_control_id")) %>% 
  rows_patch(xwalk_control_ids %>% select(plant_id, boiler_id, "mercury_control" =  `860_hg_control_id`) %>% drop_na(),
             by = c("plant_id", "mercury_control"),
             unmatched = "ignore")


## Updating units with available values ------

all_units_4 <-  
  all_units_3 %>% 
  rows_patch(nox_controls_860 %>%  # updating with available 860 NOx controls
               rename("unit_id" = "boiler_id"),
             by = c("plant_id", "unit_id"),
             unmatched = "ignore") %>%
  rows_patch(so2_controls_860 %>% # updating with available 860 SO2 controls
               rename("unit_id" = "boiler_id"),
            by = c("plant_id","unit_id"),
            unmatched = "ignore") %>%
  mutate(primary_fuel_type = if_else(primary_fuel_type %in% c("MSB", "MSN"), "MSW", primary_fuel_type)) %>% # Changing "MSB" and "MSN" fuel codes to "MSW"
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
  rows_update(og_fuel_types_update %>% rename("primary_fuel_type" = "fuel_code"), # updating fuel codes for "OG" primary fuel types
              by = c("plant_id", "unit_id"),
              unmatched = "ignore")


# Calculating emissions -----------
  
## SO2 emissions - sulfur content --------
  
### Determine default sulfur content -------

avg_sulfur_content <- 
  eia_923$boiler_fuel_data %>% 
    group_by(plant_id, boiler_id, fuel_type, physical_unit_label) %>% 
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
    arrange(as.numeric(plant_id)) %>%
    select(plant_id, 
           boiler_id,
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
  arrange(fuel_type)


### Estimate SO2 emissions using sulfur content ---------

estimated_so2_emissions_content <- 
  all_units_4 %>% select(plant_id, unit_id, prime_mover, botfirty, primary_fuel_type) %>% 
  inner_join(avg_sulfur_content, by = c("plant_id", "unit_id" = "boiler_id", "primary_fuel_type" = "fuel_type")) %>% # inner join to only include units with sulfur content
  left_join(schedule_8c_so2 %>% 
              select(plant_id, boiler_id, so2_removal_efficiency_rate_at_annual_operating_factor), 
            by = c("plant_id", "unit_id" = "boiler_id")) %>% 
  mutate(so2_removal_efficiency_rate_at_annual_operating_factor = 
           if_else(is.na(so2_removal_efficiency_rate_at_annual_operating_factor), 0, 
                   so2_removal_efficiency_rate_at_annual_operating_factor)) %>% 
  left_join(emission_factors %>%
              select(prime_mover, botfirty, so2_ef, so2_flag, unit_flag, primary_fuel_type),
            by = c("prime_mover", "botfirty", "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(is.na(so2_flag), 1, avg_sulfur_content), # if so2_flag is missing, change avg_sulfur_content to 1
         so2_mass = if_else(unit_flag == "PhysicalUnits", 
                                 (so2_ef * avg_sulfur_content * total_fuel_consumption_quantity * (1 - so2_removal_efficiency_rate_at_annual_operating_factor)) / 2000,
                                 (so2_ef * avg_sulfur_content * total_heat_input * (1 - so2_removal_efficiency_rate_at_annual_operating_factor)) / 2000)
         ) %>% 
  filter(so2_mass > 0) %>% 
  mutate(so2_source = "Estimated using emissions factor and plant-specific sulfur content") %>% 
  select(plant_id, unit_id, so2_mass, so2_source, unit_flag) %>%
  add_count(plant_id, unit_id, sort = TRUE) %>% # Some units match to multiple EF under different unit_flags. We want default to be "PhysicalUnits", so where there are multiple rows per unit, we take only "PhysicalUnits"
  filter(unit_flag == "PhysicalUnits" & n == 1 |
         unit_flag == "PhysicalUnits" & n == 2 |
         unit_flag != "PhysicalUnits" & n == 1  ) %>%
  select(-n)

  
### Join sulfur emissions to all units df  --------

all_units_5 <- 
  all_units_4 %>%
  rows_update(estimated_so2_emissions_content %>%  
                select(-unit_flag),
              by = c("plant_id", "unit_id"))


## SO2 emissions - physical units ----------

# Getting fuel consumption and heat input from 923 for estimates

eia_fuel_consum_fuel_type <- # summing fuel and consum to PM fuel_type level
  eia_923$generation_and_fuel_combined %>%
  mutate(unit_heat_nonoz = rowSums(pick(all_of(heat_923_nonoz_months)), na.rm = TRUE),
         unit_heat_oz = rowSums(pick(all_of(heat_923_oz_months)), na.rm = TRUE),
         unit_consum_nonoz = rowSums(pick(all_of(consum_923_nonoz_months)), na.rm = TRUE),
         unit_consum_oz = rowSums(pick(all_of(consum_923_oz_months)), na.rm = TRUE)) %>%
  group_by(plant_state, plant_id, prime_mover, fuel_type) %>%
  summarize(heat_input_nonoz_923 = sum(unit_heat_nonoz, na.rm = TRUE),
            heat_input_oz_923 = sum(unit_heat_oz, na.rm = TRUE),
            heat_input_ann_923 = sum(total_fuel_consumption_mmbtu, na.rm = TRUE), # consumption in mmbtus is referred to as "heat input"
            fuel_consum_nonoz_923 = sum(unit_consum_nonoz, na.rm = TRUE),
            fuel_consum_oz_923 = sum(unit_consum_oz, na.rm = TRUE),
            fuel_consum_ann_923 = sum(total_fuel_consumption_quantity, na.rm = TRUE) # consumption in quantity is referred to as "fuel consumption"
  )

# identifying coal fuels 

coal_fuels <- c("BIT", "LIG", "SUB", "RC", "WC", "SGC", "COG")

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
  filter(fuel_type %in% coal_fuels) %>% 
  select(plant_state,
         fuel_type,
         avg_sulfur_content)


units_estimated_fuel <- # df that will be used to calculate SO2 and NOx emissions 
  all_units_5 %>% 
  group_by(plant_state, plant_id, prime_mover, primary_fuel_type) %>% 
  mutate(total_heat_input = sum(heat_input, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(dist_prop = heat_input / total_heat_input) %>% 
  select(plant_state,
         plant_id, 
         prime_mover, 
         unit_id, 
         primary_fuel_type,
         botfirty,
         dist_prop) %>%
  inner_join(eia_fuel_consum_fuel_type,
             by = c("plant_id", "plant_state", "prime_mover", "primary_fuel_type" = "fuel_type")) %>%
  mutate(fuel_consumption = fuel_consum_ann_923 * dist_prop,
         fuel_consumption_oz = fuel_consum_oz_923 * dist_prop,
         heat_input = heat_input_ann_923 * dist_prop,
         heat_input_oz = heat_input_oz_923 * dist_prop) %>% 
  select(-c(ends_with("_923"))) 

### creating physical units table for SO2

emission_factors_so2_pu <- emission_factors %>%
  filter(primary_fuel_type != "OTH" |
           so2_ef != 2.8,
         unit_flag == "PhysicalUnits") %>%
  select(!starts_with("nox")) %>% 
  mutate(botfirty = if_else(botfirty %in% c("null", "N/A"), NA_character_, botfirty)) %>% # fill null or N/A botfirty with NA
  distinct()
  
## creating heat input table for SO2

emission_factors_so2_heat <- emission_factors %>%
  mutate(unit_flag = if_else(unit_flag == "heatinput", "HeatInput", unit_flag)) %>%
  filter(unit_flag == "HeatInput") %>%
  select(!starts_with("nox")) %>% 
  mutate(botfirty = if_else(botfirty %in% c("null", "N/A"), NA_character_, botfirty)) %>%  # fill null or N/A botfirty with NA
  distinct()

### estimating so2 emissions - coal --------

so2_emissions_pu_coal <-
  units_estimated_fuel %>% 
  left_join(default_sulfur_content_coal, # renaming for consistent naming
            by = c("plant_state",
                   "primary_fuel_type" = "fuel_type")) %>% 
  left_join(emission_factors_so2_pu %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S", avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "PhysicalUnits",
                            so2_ef * avg_sulfur_content * fuel_consumption,
                            so2_ef * avg_sulfur_content * heat_input / 2000)) %>%
  select(plant_id,
         unit_id,
         so2_mass) %>% 
  filter(so2_mass > 0) %>% 
  mutate(so2_source = "Estimated using emissions factor")


### estimating so2 emissions - non coal ----------

so2_emissions_pu_noncoal <- 
  units_estimated_fuel %>% 
  left_join(avg_sulfur_content_fuel %>% select(fuel_type, avg_sulfur_content), # renaming for consistent naming
            by = c("primary_fuel_type" = "fuel_type")) %>% 
  left_join(emission_factors_so2_pu %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S", avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "PhysicalUnits",
                            so2_ef * avg_sulfur_content * fuel_consumption,
                            so2_ef * avg_sulfur_content * heat_input / 2000)) %>%
  select(plant_id,
         unit_id,
         so2_mass) %>% 
  filter(so2_mass > 0) %>% 
  mutate(so2_source = "Estimated using emissions factor")
  
### estimating so2 emissions with heat input - coal ------

so2_emissions_heat_coal <- 
  units_estimated_fuel %>% 
  left_join(default_sulfur_content_coal, # renaming for consistent naming
            by = c("primary_fuel_type" = "fuel_type",
                   "plant_state")) %>% 
  left_join(emission_factors_so2_heat %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S", avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "HeatInput",
                            so2_ef * avg_sulfur_content * heat_input / 2000, 0)) %>%
  select(plant_id,
         unit_id,
         so2_mass) %>% 
  filter(so2_mass > 0) %>% 
  mutate(so2_source = "Estimated using emissions factor")


### estimating so2 emissions with heat input - non coal ---------

so2_emissions_heat_noncoal <- 
  units_estimated_fuel %>% 
  left_join(avg_sulfur_content_fuel %>% select(fuel_type, avg_sulfur_content), # renaming for consistent naming
            by = c("primary_fuel_type" = "fuel_type")) %>%
  left_join(emission_factors_so2_heat %>%  
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S", avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "HeatInput",
                            so2_ef * avg_sulfur_content * heat_input / 2000, 0)) %>%
  select(plant_id,
         unit_id,
         so2_mass) %>% 
  filter(so2_mass > 0) %>% 
  mutate(so2_source = "Estimated using emissions factor")


### Updating units with estimated SO2 --------

all_units_6 <- 
  all_units_5 %>% 
  rows_patch(so2_emissions_pu_coal, 
             by = c("plant_id", "unit_id")) %>% 
  rows_patch(so2_emissions_pu_noncoal, 
             by = c("plant_id", "unit_id")) %>% 
  rows_patch(so2_emissions_heat_coal, 
             by = c("plant_id", "unit_id")) %>% 
  rows_patch(so2_emissions_heat_noncoal, 
             by = c("plant_id", "unit_id"))


## CO2 emissions --------

### Estimating CO2 emissions --------  

# read in co2 emissions table
co2_ef <- 
  read_csv("data/static_tables/co2_ch4_n2o_ef.csv")

co2_emissions <- 
  all_units_6 %>%
  select(plant_id, unit_id, primary_fuel_type, prime_mover, heat_input, co2_mass) %>%
  inner_join(co2_ef %>%
              filter(!is.na(eia_fuel_code)) %>%
              select(eia_fuel_code, co2_ef), by = c("primary_fuel_type" = "eia_fuel_code")) %>%
  mutate(co2_mass = heat_input * co2_ef, 
         co2_source = "Estimated using emissions factor") %>%
  select(-c(co2_ef, heat_input, primary_fuel_type))

### Updating units with estimated CO2 emissions --------

all_units_7 <- 
  all_units_6 %>% 
  rows_patch(co2_emissions %>% distinct(),
             by = c("plant_id", "unit_id", "prime_mover"),
             unmatched = "ignore")

## NOx Emissions -----

### NOx emissions - rate -------

nox_rates_2 <- 
  schedule_8c_nox %>%
  rename(unit_id = boiler_id) %>%
  group_by(plant_id, unit_id) %>%
  summarize(nox_rate = max(nox_emission_rate_entire_year_lbs_mmbtu),
            nox_oz_rate = max(nox_emission_rate_may_through_september_lbs_mmbtu)) 

nox_rates_ann <- 
  nox_rates_2 %>%
  inner_join(all_units_7 %>%
               select(plant_id, unit_id, prime_mover, heat_input)) %>%
  mutate(nox_mass = (nox_rate * heat_input) / 2000,
         nox_source = "Estimated based on unit-level NOx emission rates") %>%
  select(-c(nox_rate, nox_oz_rate))

nox_rates_oz <- 
  nox_rates_2 %>%
  inner_join(all_units_7 %>%
               select(plant_id, unit_id, prime_mover, heat_input_oz)) %>%
  mutate(nox_oz_mass = (nox_oz_rate * heat_input_oz) / 2000,
         nox_oz_source = "Estimated based on unit-level NOx ozone season emission rates") %>%
  select(-c(nox_rate, nox_oz_rate))

nox_emissions_rates <- 
  all_units_7 %>%
  rows_patch(nox_rates_ann,
            by = c("plant_id", "unit_id", "prime_mover"), ## Joining by prime_mover because there are duplicate plant_id/unit_id combinations
            unmatched = "ignore") %>%
  rows_patch(nox_rates_oz, 
             by = c("plant_id", "unit_id", "prime_mover"),
             unmatched = "ignore")

### NOx emissions - emissions factor -----

### creating NOx physical units table

emission_factors_nox_pu <- 
  emission_factors %>%
  filter(nox_unit_flag == "PhysicalUnits") %>%
  select(!starts_with("so2") & -c(unit_flag)) %>%
  mutate(botfirty = if_else(botfirty %in% c("null", "N/A"), NA_character_, botfirty)) %>% # some null or NA botfirty are text, fill with NA
  distinct() 
  
#### estimating NOx annual emissions with EF --------

nox_emissions_factor <-
  units_estimated_fuel %>% 
  filter(plant_id != 10025) %>% ## Removing duplicate Plant ID 10025/Unit ID 4B that was causing issues with rows_patch
  left_join(emission_factors_nox_pu %>%
              select(prime_mover, primary_fuel_type, botfirty, nox_ef, nox_ef_num, nox_ef_denom), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  group_by(plant_id, unit_id) %>%
  mutate(nox_mass = (fuel_consumption * nox_ef) / 2000) %>%
  select(plant_id,
         unit_id,
         prime_mover,
         nox_mass) %>% 
  filter(nox_mass > 0) %>% 
  mutate(nox_source = "Estimated using emissions factor")

#### estimating NOx ozone emissions with EF --------

nox_oz_emissions_factor <- 
  units_estimated_fuel %>% 
  filter(plant_id != 10025) %>% ## Removing duplicate Plant ID 10025/Unit ID 4B that was causing issues with rows_patch
  left_join(emission_factors_nox_pu %>%
              select(prime_mover, primary_fuel_type, botfirty, nox_ef, nox_ef_num, nox_ef_denom), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  group_by(plant_id, unit_id) %>%
  mutate(nox_oz_mass = (fuel_consumption_oz * nox_ef) / 2000) %>%
  select(plant_id,
         unit_id,
         prime_mover,
         nox_oz_mass) %>% 
  filter(nox_oz_mass > 0) %>% 
  mutate(nox_oz_source = "Estimated using emissions factor")

#### estimating NOx annual emissions with heat input --------

nox_emissions_heat_input <- 
  nox_emissions_rates %>%
  left_join(emission_factors_nox_pu %>%
              select(prime_mover, primary_fuel_type, botfirty, nox_ef, nox_ef_num, nox_ef_denom), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  select(plant_id, unit_id, prime_mover, heat_input, nox_ef, nox_ef, nox_ef_num, nox_ef_denom) %>%
  group_by(plant_id, unit_id,nox_ef, nox_ef_num, nox_ef_denom) %>%
  mutate(nox_mass = sum((heat_input * nox_ef) / 2000),
         nox_source = "Estimated using emissions factor") %>%
  ungroup() %>%
  select(plant_id,
         unit_id,
         prime_mover,
         nox_mass,
         nox_source) %>%
  filter(nox_mass > 0)

#### estimating NOx ozone emissions with heat input --------

nox_oz_emissions_heat_input <- 
  nox_emissions_rates %>%
  left_join(emission_factors_nox_pu %>%
              select(prime_mover, primary_fuel_type, botfirty, nox_ef, nox_ef_num, nox_ef_denom), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  select(plant_id, unit_id, prime_mover, heat_input_oz, nox_ef, nox_ef_num, nox_ef_denom) %>%
  group_by(unit_id, plant_id, nox_ef, nox_ef_num, nox_ef_denom) %>%
  mutate(nox_oz_mass = sum((heat_input_oz * nox_ef) / 2000),
         nox_oz_source = "Estimated using emissions factor") %>%
  ungroup() %>%
  select(plant_id,
         unit_id,
         prime_mover,
         nox_oz_mass,
         nox_oz_source) %>%
  filter(nox_oz_mass > 0)

### Updating units with estimating NOx --------

all_units_8 <- 
  nox_emissions_rates %>%
  rows_patch(nox_emissions_factor,
             by = c("plant_id", "unit_id", "prime_mover"),
             unmatched = "ignore") %>%
  rows_patch(nox_oz_emissions_factor, 
             by = c("plant_id", "unit_id", "prime_mover"),
             unmatched = "ignore") %>%
  rows_patch(nox_emissions_heat_input,
             by = c("plant_id", "unit_id", "prime_mover"),
             unmatched = "ignore") %>% 
  rows_patch(nox_oz_emissions_heat_input,
             by = c("plant_id", "unit_id", "prime_mover"),
             unmatched = "ignore") 


## Fix Ozone Season ----------
# IF NOx ozone emissions > annual NOx emissions, make the NOx ozone emissions equal the annual NOx emissions

all_units_9 <- 
  all_units_8 %>%
  mutate(nox_oz_mass = case_when(nox_oz_mass > nox_mass ~ nox_mass,
                                 TRUE ~ nox_oz_mass))

## Geothermal Emissions --------

### Note: check for updates or changes each data year ###
# this table is sourced from NREL, they will not update it until 2025
# check in 2025 for updated table 

nrel_geo_type <- read_csv("data/static_tables/nrel_geothermal_table.csv") %>% 
  rename("plant_id" = "ORISPL") %>% janitor::clean_names() %>% 
  mutate(plant_id = as.character(plant_id)) %>% distinct()

geo_emission_factors <- read_csv("data/static_tables/geothermal_emission_factors.csv") %>% 
  janitor::clean_names()

geo_emissions <- 
  all_units_9 %>%
  filter(primary_fuel_type == "GEO") %>% 
  left_join(nrel_geo_type, by = "plant_id") %>% 
  left_join(geo_emission_factors, by = "geo_type_code") %>% 
  mutate(nox_mass = (heat_input * nox_ef_lb_mmbtu) / 2000, 
         nox_oz_mass = (heat_input_oz * nox_ef_lb_mmbtu) / 2000, 
         so2_mass = (heat_input * so2_ef_lb_mmbtu) / 2000, 
         co2_mass = (heat_input * co2_ef_lb_mmbtu) / 2000, 
         nox_source = "Estimated using emissions factor", 
         nox_oz_source = "Estimated using emissions factor", 
         so2_source = "Estimated using emissions factor", 
         co2_source = "Estimated using emissions factor") %>% 
  select(plant_id, unit_id, nox_mass, nox_oz_mass, so2_mass, co2_mass, 
         nox_source, nox_oz_source, so2_source, co2_source)


# update all_units with geothermal emissions 

all_units_10 <- 
  all_units_9 %>% 
  rows_patch(geo_emissions, by = c("plant_id", "unit_id"))

# Final modifications -----  

## Clean up source flags --------

clean_source_flags <- 
  all_units_10 %>% 
  mutate(nox_source = replace(nox_source, is.na(nox_mass), NA_character_), # replacing NA emission masses with NA sources
         so2_source = replace(so2_source, is.na(so2_mass), NA_character_), 
         co2_source = replace(co2_source, is.na(co2_mass), NA_character_), 
         hg_source = replace(hg_source, is.na(hg_mass), NA_character_))


## Change necessary plant names -------
# Check for duplicate plant names and default to CAMD names

check_plant_names <- 
  all_units_10 %>% select(plant_id, plant_name, source) %>% 
  group_by(plant_id, plant_name) %>% 
  distinct() %>% 
  group_by(plant_id) %>% 
  filter(n() > 1 & !is.na(plant_name), 
         source == "CAMD") # default to CAMD names

  
## Update FC prime mover CO2 emissions data ------
# Update FC prime mover to null CO2 emissions 

update_fc_data <- 
  all_units_10 %>% select(plant_id, unit_id, prime_mover, co2_mass, co2_source) %>% 
  filter((prime_mover == "FC") & ((!is.na(co2_mass)) | (!is.na(co2_source)))) %>%  # only update necessary rows 
  mutate(co2_mass = NA_character_, 
         co2_source = NA_character_, 
         co2_mass = as.numeric(co2_mass), 
         co2_source = as.character(co2_source)) %>% 
  select(plant_id, unit_id, co2_mass, co2_source) 

## Delete specified units -------- 
### Delete units in "Units to remove" table, which is manually updated each year ----------
### Note: check for updates or changes each data year ###

units_to_remove <- 
  read_csv("data/static_tables/units_to_remove.csv") %>% 
  mutate(plant_id = as.character(plant_id))


### Delete retired units  ------- 
# delete units with operating status "RE" based on operating status ("RE") from EIA-860 Boiler Info & Design Parameters 
# check if the unit retires before the current data year

delete_retired_units <- 
  eia_860$boiler_info_design_parameters %>% 
  left_join(all_units_10 %>% select(plant_id, unit_id, operating_status), 
            by = c("plant_id", "boiler_id" = "unit_id")) %>% 
  filter(boiler_status == "RE" & as.numeric(retirement_year) < params$eGRID_year &
           (operating_status == "RE" | is.na(operating_status))) %>% 
  select(plant_id, 
         unit_id = "boiler_id", 
         boiler_status, retirement_year)


### Delete noted EIA units ------ 
### Note: check for updates or changes each data year ###

eia_units_to_delete <- 
  all_units_10 %>% 
  filter(plant_id == "2132" | plant_id == "7832") %>% 
  select(plant_id) %>% distinct()


## Update prime mover -------
# Manual updates to prime mover for specific plants 
### Note: check for updates or changes each data year ###

update_prime_mover <- 
  all_units_10 %>% select(plant_id, unit_id, prime_mover) %>% 
  filter(plant_id == "7063" & unit_id == "**1" | 
           plant_id == "50973" & prime_mover == "CT") %>% 
  mutate(prime_mover = case_when ( 
            plant_id == "7063" & unit_id == "**1" ~ "CE", # Update prime mover for plant 7063 unit **1 from OT to CE 
            plant_id == "50973" ~ "CA", # Update PM for plant 50973 since boilers have multiple PMs
            TRUE ~ prime_mover)) 
  

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
  filter(source == "923 boilers", is.na(operating_status)) %>% # only update status for those that are NA
  rows_patch(eia_860$boiler_info_design_parameters %>% select(plant_id, "unit_id" = boiler_id, "operating_status" = boiler_status), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  mutate(operating_status = toupper(operating_status)) %>% 
  filter(!is.na(operating_status)) %>% distinct() %>% select(-source)


## Create and edit CAMD flag ----- 
# ID CAMD units 

camd_units <- camd_7 %>% mutate(id = paste0(plant_id, unit_id)) %>% pull(id)

# update PR CAMD flag from crosswalk

update_pr_camd_flag <- read_csv("data/static_tables/xwalk_pr_oris.csv") %>% 
  select("plant_id" = "eia_plant_id", 
         "unit_id" = "eia_unit_id", 
         "plant_name" = "eia_plant_name") %>% 
  mutate(across(where(is.numeric), ~as.character(.)), 
         camd_flag = "Yes")

# create CAMD flag 

all_units_11 <- 
  all_units_10 %>%  
  mutate(camd_flag = if_else(paste0(plant_id, unit_id) %in% camd_units, "Yes", NA_character_)) %>% 
  rows_update(update_pr_camd_flag, by = c("plant_id", "unit_id"))


## Implement changes in main unit file ------

all_units_12 <- 
  all_units_11 %>% # update to most recent unit file data frame
  rows_delete(units_to_remove, 
                by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_delete(delete_retired_units, 
                by= c("plant_id", "unit_id"), unmatched = "ignore") %>%
  rows_delete(eia_units_to_delete, 
                by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(update_fc_data, 
                by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_update(update_prime_mover, 
                by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_update(check_plant_names, 
                by = c("plant_id"), unmatched = "ignore") %>% 
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
         hg_mass = round(hg_mass, 3), 
         plant_id = case_when( # update plant_id and plant_name to EIA ID and name
           ### Note: check for updates or changes each data year ###
           plant_id %in% c("2847", "55248") ~ "2847", 
           TRUE ~ plant_id), 
         plant_name = case_when(
           plant_id == "2847" ~ "Tait Electric Generating Station", 
           TRUE ~ plant_name)) 

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
    "CAMDFLAG" = "camd_flag", 
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
    "UNTYRONL" = "year_online")

units_formatted <-
  all_units_12 %>%
  arrange(plant_state, plant_name) %>% 
  mutate(sequnt = row_number(), 
         year = params$eGRID_year) %>% 
  select(as_tibble(final_vars)$value) # keeping columns with tidy names for QA steps


# Export unit file -------------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

print("Saving unit file to folder data/outputs/")

write_rds(units_formatted, "data/outputs/unit_file.RDS")

