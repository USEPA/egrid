

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
    "reporting_frequency",
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
              select(plant_id, prime_mover, fuel_type, total_fuel_consumption_mmbtu) %>%
              group_by(plant_id) %>%
              slice_max(total_fuel_consumption_mmbtu, n = 1, with_ties = FALSE) %>% # identify fuel type associate with max fuel consumption by plant
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
  mutate(botfirty = if_else(!is.na(eGRID), eGRID, botfirty)) %>% 
  select(-eGRID) 
  


## Gap fill CAMD ozone season reporters with EIA data ----------

# a.	Some CAMD plants are “ozone season reporters” – which means that they only report data during the ozone season, which is May to September each year. 
# b.	These plants are listed in the CAMD data with a frequency variable of “OS” instead of “Q” (quarterly)
# c.	We gap fill the missing months (January, February, March, April, October, November, and December) with EIA data. 


### Gap fill heat input for OS reporters --------------


camd_oz_reporter_plants <- # getting list of plant ids that include ozone season reporters. Note that some plants include both OS only and OS and Annual reporters.
  camd_6 %>% 
  filter(reporting_frequency == "OS") %>% 
  pull(plant_id)


# camd_oz_q_reporters <- # not sure this is necessary to identify. Commenting out for now
#   camd_6 %>% 
#   count(plant_id, reporting_frequency) %>% 
#   count(plant_id) %>% 
#   filter(n > 1) %>% 
#   pull(plant_id) %>%
#   unique(.)


camd_oz_reporters <- # creating dataframe with all plants and associated units that include "OS" reporters.
  camd_6 %>%
  filter(plant_id %in% camd_oz_reporter_plants)


### summing heat input values in the 923 gen and fuel file. These totals will be used to distribute heat for the non-ozone months in the camd data.
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

# (SB: 5/8/2024 -- For now, I am doing this very differently than what has occurred in the past. Something seems either off or overly complicated with how this section was handled )

camd_oz_reporters_dist <- 
  camd_oz_reporters %>%
  group_by(plant_id, prime_mover) %>% 
  mutate(tot_heat_input = sum(heat_input, na.rm = TRUE)) %>%
  left_join(eia_fuel_consum_pm, 
            by = c("plant_id", "prime_mover")) %>% 
  ungroup() %>%
  #mutate(annual_heat_diff = heat_input_ann_923 - tot_heat_input) %>% # calculating annual heat difference # SB: This is old method, but I'm not sure this part works correctly or is necessary
         #annual_heat_diff_wout_oz = annual_heat_diff - heat_input_oz_923) %>% # calculating difference - ozone month totals to get the leftover nonozone heat input
  group_by(plant_id, prime_mover) %>% # sum to plant/pm/ for distributional proportion 
  mutate(sum_heat_input_oz = sum(heat_input_oz, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(prop = heat_input_oz/sum_heat_input_oz) %>%  # determining distributional proportion
  ungroup() %>% 
  mutate(heat_input_nonoz = heat_input_nonoz_923 * prop, # distributing nonoz
         heat_input = if_else(reporting_frequency == "Q", heat_input, heat_input_oz + heat_input_nonoz), # If unit is Q reporter, we use the heat input from CAMD. If OS reporter, we add distributed non-oz heat and ozone heat
         heat_input_source = if_else(reporting_frequency == "Q", "EPA/CAMD", "EIA non-ozone season distributed and EPA/CAMD ozone season"))
  



### Gap fill NOx emissions ozone season reporters -----------

#### NOx Rate ---------

# where available, NOx rates are used to estimates NOx emissions
# ? Something is missing in this part Or maybe this should be handled differently.

nox_rates <- # calculating nox emission rates used to estimtae NOx emissions
  eia_923$air_emissions_control_info %>% 
  left_join(eia_860$boiler_nox %>% select(plant_id, nox_control_id, boiler_id),
            by = c("plant_id", "nox_control_id")) %>% 
  group_by(plant_id, boiler_id) %>%
  summarize(nox_rate_ann = max(nox_emission_rate_entire_year_lbs_mmbtu),
            nox_rate_oz = max(nox_emission_rate_may_through_september_lbs_mmbtu)) 

camd_oz_reporters_dist_combined_2 <- 
 camd_oz_reporters_dist_combined %>%
  inner_join(nox_rates,
            by = c("plant_id", "unit_id" = "boiler_id")) %>%
  mutate(nox = (heat_input_nonoz * nox_rate_ann)/ 2000,
         nox_source = if_else(is.na(nox), nox_source ,"Estimated based on unit-level NOx emission rates and EPA/CAMD ozone season emissions"))

#### NOx EF ------
  
# for remaining units, we use emission factors ---

consum_923_nonoz_months <- paste0("quantity","_", tolower(month.name)[c(1:4,10:12)])


eia_fuel_consum_pm <- # summing fuel and consum to pm level
  eia_923$generation_and_fuel_combined %>%
  mutate(unit_heat_nonoz = rowSums(pick(all_of(heat_923_non_oz_months)), na.rm = TRUE),
         unit_consum_nonoz = rowSums(pick(all_of(consum_923_nonoz_months)), na.rm = TRUE)) %>%
  group_by(plant_id, prime_mover, fuel_type) %>%
  summarize(heat_input_nonoz_923 = sum(unit_heat_nonoz),
            fuel_consum_nonoz_923 = sum(unit_consum_nonoz))







### OS only reporters ----------------

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



