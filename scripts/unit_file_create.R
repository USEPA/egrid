

# Load libraries  ----


library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)


# Load necessary data ------

## camd ------
camd_vars_to_keep <- 
  c("year",
    "plant_state",
    "plant_name",
    "plant_id",
    "unit_id",
    "operating_status",
    "program_code",
    "primary_fuel_type",
    "unit_type" = "unit_type_abb",
    "operating_hours" = "operating_time_count",
    "heat_input" = "heat_input_mmbtu",
    "heat_input_oz" = "heat_input_mmbtu_ozone",
    "nox_mass" = "nox_mass_short_tons",
    "nox_mass_oz" = "nox_mass_short_tons_ozone",
    "so2_mass" = "so2_mass_short_tons",
    "so2_mass_oz" = "so2_mass_short_tons_ozone",
    "co2_mass" = "co2_mass_short_tons",
    "hg_mass" = "hg_mass_lbs",
    "heat_input_source",
    "heat_input_oz_source",
    "nox_source",
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

## eia ------------

eia_860 <- read_rds("data/clean_data/eia_860_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia_923_clean.RDS")


# Modifying CAMD data ---------

## Harmonizing fields with EIA ----------

# vectors of unit types matched to their respective EIA prime mover values
pm_st <- c("BFB", "C", "CB", "CFB", "DB", "DTF", "DVF", "IGC", "KLN", "OB", "PRH", "S", "T", "WBF", "WBT")
pm_gt <- c("AF", "CT")
pm_ct <- c("CC")
pm_ot <- c("OT")


camd_2 <- 
  camd %>% 
  mutate(prime_mover = case_when( # creating prime_mover based on mapping above
    unit_type %in% pm_st ~ "ST",
    unit_type %in% pm_gt ~ "GT",
    unit_type %in% pm_ct ~ "CT",
    unit_type %in% pm_ot ~ "OT",
    TRUE ~ "EIA PM"),
    operating_status = if_else(operating_status == "Operating", "OP", NA_character_) # Abbreviating operating status
    )
    
    
### Updating fuel types ---------

# updating fuel types to eia codes for oil, other solid fuel, and coal 
# When there is match in 860, we take the eia fuel code. For non-matches, we check 923 genfuel file
# and take the primary fuel from each plant (i.e., the fuel code of the plant/prime mover with the highest fuel consumption).
# There are some left over, resulting from a) `UNIT ID` and GENID not matching and b) 0 fuel consumption values in the GenFuel file.
# These cases may require individual updates once identified.



camd_3 <-
  camd_2 %>%
  left_join(eia_860$combined %>% # joining with 860_combined to get EIA primary fuel codes
              distinct(plant_id, plant_name, generator_id, energy_source_1),
            by = c("plant_id", "plant_name", "unit_id" = "generator_id")) %>% 
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
    TRUE ~ NA_character_ )) %>% 
  left_join(eia_923$generation_and_fuel_combined %>%  # joining generation and fuel file, based on max fuel consumption by plant
              select(plant_id, prime_mover, fuel_type, total_fuel_consumption_mm_btu) %>%
              group_by(plant_id) %>%
              slice_max(total_fuel_consumption_mm_btu, n = 1, with_ties = FALSE) %>% # identify fuel type associate with max fuel consumption by plant
              select(plant_id, fuel_type),
            by = c("plant_id")) %>% 
  mutate(primary_fuel_type = if_else(is.na(primary_fuel_type), fuel_type, primary_fuel_type)) %>% # filling missing primary_fuel_types with 923 value
  select(-fuel_type)

camd_4 <- # These units require manual updates (Note: Need to check to see if this is required each year (SB 4/24/24))
  camd_3 %>% 
  mutate(primary_fuel_type = case_when(
    plant_id == 10075 & unit_id == "1" ~ "SUB" ,
    plant_id == 10075 & unit_id == "2" ~ "SUB",
    plant_id == 10849 & unit_id == "PB1" ~ "SUB" ,
    plant_id == 10849 & unit_id == "PB2" ~ "SUB",
    plant_id == 54748 & unit_id == "GT1" ~ "NG",
    TRUE ~ primary_fuel_type
  ))

## identify units remaining with missing primary_fuel_types
missing_fuel_types <- 
  camd_4 %>% 
  filter(is.na(camd_4$primary_fuel_type))

print(glue::glue("{nrow(missing_fuel_types)} units having missing primary fuel types."))

### Add boiler firing type ------------
# A crosswalk is used to create a boiler firing type variable, based on unit type. 
# some units have more specific boiler firing types available in the 860 boiler info file
# and these are used where available

xwalk_botfirty <- read_csv("data/static_tables/boiler_firing_type_xwalk.csv")

camd_5 <- 
  camd_4 %>% 
  left_join(xwalk_botfirty,
            by = c("unit_type" = "CAMD/EPA")) %>% 
  mutate(botfirty = eGRID) %>% 
  select(-any_of(names(xwalk_botfirty)))
  


botfirty_to_update <- # matching with 860 data to see if more specific botfirty type exists
  camd_5 %>% 
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


camd_6 <- # updating units where available
  camd_5 %>% 
  left_join(botfirty_to_update) %>% 
  mutate(botfirty == if_else(!is.na(eGRID), eGRID, botfirty)) %>% 
  select(-eGRID) 
  




## Gap fill ozone season reporters with EIA data ----------

# Add in EIA units -------------

## EIA-923 boilers --------

## EIA-860 generators ---------------

# Distribute heat input -------------

# Estimate emissions --------

## NOx --------

## SO2 --------

## C02 --------

## Geothermal -------

# Create final structure for unit file

# Final edits to unit file ------------



