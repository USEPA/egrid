

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
    "nameplate_capacity",
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


# Checking for missing after update
# Each of these that are still missing were not included in unit file, so will get removed at some point, apparently (SB 7/3/2023)



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
  filter(reporting_frequency == "OS") %>% # Annual reporting units can be removed now
  ungroup() %>% 
  mutate(heat_input_nonoz = heat_input_nonoz_923 * prop, # distributing nonoz
         heat_input = heat_input_oz + heat_input_nonoz, # annual heat input =   distributed non-oz heat + ozone heat
         heat_input_source = if_else(is.na(heat_input), "EPA/CAMD", "EIA non-ozone season distributed and EPA/CAMD ozone season"))



### Gap fill NOx emissions ozone season reporters -----------

#### NOx Rate ---------

# where available, NOx rates are used to estimates NOx emissions


nox_rates <- # calculating nox emission rates used to estimate NOx emissions
  eia_923$air_emissions_control_info %>% 
  left_join(eia_860$boiler_nox %>% select(plant_id, nox_control_id, boiler_id),
            by = c("plant_id", "nox_control_id")) %>% 
  group_by(plant_id, boiler_id) %>%
  summarize(nox_rate_ann = max(nox_emission_rate_entire_year_lbs_mmbtu),
            nox_rate_oz = max(nox_emission_rate_may_through_september_lbs_mmbtu)) 

camd_oz_reporters_dist_nox_rates <- # filling annual nox mass with nox_rates where available
  camd_oz_reporters_dist %>%
  inner_join(nox_rates,
             by = c("plant_id", "unit_id" = "boiler_id")) %>%
  mutate(nox_mass_nonoz = (heat_input_nonoz * nox_rate_ann)/ 2000, 
         nox_mass = nox_mass_nonoz + nox_mass_oz, # I think there should be a reporting frequency condition here. 
         nox_source = if_else(is.na(nox_mass), nox_source ,"Estimated based on unit-level NOx emission rates and EPA/CAMD ozone season emissions"))

nox_rates_ids <- # creating unique ids to filter out later
  camd_oz_reporters_dist_nox_rates %>% 
  mutate(id = paste0(plant_id,prime_mover,unit_id)) %>% 
  pull(id)

#### NOx Ef   -----------------

emission_factors <- read_csv("data/static_tables/emission_factors.csv") # load in emission_factors data


# Joining EFs df with camd ozone reporters to calculate non-ozone NOx mass

camd_oz_reporters_dist_nox_ef <-
  camd_oz_reporters_dist %>% 
  filter(!paste0(plant_id,prime_mover,unit_id) %in% nox_rates_ids) %>% # removing units that were filled with nox_rates
  left_join(emission_factors %>%
              filter(nox_unit_flag == "PhysicalUnits") %>% # (SB: This doesn't happen in access but I think is necessary) 
              select(prime_mover, botfirty, primary_fuel_type, nox_ef) %>% 
              distinct()) %>% # there are duplicates based on other columns in the emission_factors data. Getting distinct rows.
  mutate(fuel_consum_nonoz = fuel_consum_nonoz_923 * prop, # calculating distributed non-ozone fuel consumption (SB: Need to check if this needs to be calculated at a different level)
         nox_mass_nonoz = fuel_consum_nonoz * nox_ef/2000,
         nox_mass = nox_mass_oz + nox_mass_nonoz,
         nox_source = if_else(is.na(nox_mass), "EPA/CAMD", "Estimated using emissions factor and EIA data for non-ozone season and EPA/CAMD ozone season emissions")) 

# Combing two dataframes with distributed annual NOx mass to join back with rest of CAMD

distinct_cols <- c("plant_id", "unit_id", "heat_input", "heat_input_source", "nox_mass", "nox_source")

camd_oz_reporters_dist_final <- # combinging all distributed ozone season reporters and keeping only necessary columns 
  camd_oz_reporters_dist_nox_rates %>% 
  distinct(pick(distinct_cols)) %>%
  bind_rows(camd_oz_reporters_dist_nox_ef %>% 
              distinct(pick(distinct_cols)))

print(glue::glue("There are {nrow(camd_oz_reporters_dist)} total OS reporting units. The dataframe with distributed values contains {nrow(camd_oz_reporters_dist_final)}"))

if(nrow(camd_oz_reporters_dist) < nrow(camd_oz_reporters_dist_final)) {
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
  "The number of rows in the distrubted dataframe matches the total OS-reporting units."
}
  

# Updating heat input and emissions for OS reporters in full CAMD dataframe

camd_7 <- # Updated with gap-filled OS reporters
  camd_6 %>% 
  rows_update(camd_oz_reporters_dist_final, # Updating os reporters with new distributed heat inputs, nox_mass, and the source variables for both
              by = c("plant_id","unit_id"))

# Including EIA units (generators and boilers) -----------

## EIA boilers ----------

eia_923_boilers <- 
  eia_923$boiler_fuel_data %>% 
  mutate(across( # calculating monthly unit heat input, based on correponding consumption and mmbtu_per_unit
                .cols = starts_with("quantity_of_fuel_consumed_"),
                .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "mmbtu_per_unit_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
                .names = "heat_input_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
         heat_input = rowSums(pick(all_of(starts_with("heat_input")))), # getting annual heat_input, summing across all monthly heat columns
         heat_input_oz = rowSums(pick(all_of(paste0("heat_input_",tolower(month.name[5:9])))))) %>%  # summing across ozone months
  select(plant_id, plant_name, plant_state, prime_mover, boiler_id, fuel_type, heat_input, heat_input_oz, total_fuel_consumption_quantity)  


eia_923_boilers_grouped <- 
  eia_923_boilers %>% 
  group_by(plant_id,
           prime_mover,
           fuel_type) %>% 
  mutate(heat_input = sum(heat_input),
         heat_input_oz = sum(heat_input_oz))


# read in co2 emissions table

co2_ef <- 
  read_csv("data/static_tables/co2_ch4_n2o_ef.csv")


# Manually step that happens in Access: `FirstOfPrime Mover` = if_else(`FirstOfPrime Mover` == "CT" & `Plant Id` == "50973", "CA", `FirstOfPrime Mover`


eia_923_boilers_heat_co2 <- 
  eia_923_boilers %>% 
  left_join(co2_ef %>% 
              select(eia_fuel_code, co2_ef),
            by = c("fuel_type" = "eia_fuel_code")) %>% 
  mutate(co2_emissions = heat_input * co2_ef) %>% 
  group_by(pick(starts_with("plant")), prime_mover, boiler_id) %>% 
  summarize(heat_input = sum(heat_input),
            heat_input_oz = sum(heat_input_oz),
            co2_emissions = sum(co2_emissions)) 

## Determining primary fuel type for 923 boilers based on type of unit with max fuel consumption 

primary_fuel_types_923_boilers <-  # this will be joined with final dataframe of boilers to be added 
  eia_923_boilers_grouped %>% 
  group_by(plant_id, boiler_id) %>%
  slice_max(heat_input, # identifying row with highest heat input to get primary_fuel_type
            n = 1, 
            with_ties = FALSE) %>% # only retain first row
  select(plant_id, 
         boiler_id,
         "primary_fuel_type" = fuel_type) %>% 
  ungroup()

# We only want to keep 923 boilers that are: 
# 1) from plants that are not already in CAMD
# 2) matches in the EIA-860 combined file
# 3) matches to plant/gen_id in the eia-860 combined

# getting vectors of ids for 860 tables to filter on
eia_860_boil_gen_ids <- eia_860$boiler_generator %>% mutate(id = paste0(plant_id, "_", boiler_id)) %>% pull(id)
eia_860_combined_ids <- eia_860$combined %>% mutate(id = paste0(plant_id, "_", generator_id)) %>% pull(id)



eia_boilers_to_add <- 
  eia_923_boilers_heat_co2 %>% 
  mutate(id = paste0(plant_id, "_", boiler_id)) %>%
  filter(!plant_id %in% camd_7$plant_id) %>%  # removing boilers that are in plants in CAMD
  filter(id %in% eia_860_boil_gen_ids | id %in% eia_860_combined_ids) %>%  # keeping only boilers that are in 860, under boiler or unit id
  mutate(heat_input_source = "EIA Unit-Level Data",
         heat_input_oz_source = "EIA Unit-Level Data",
         co2_source = "Estimated using emissions factor") %>% 
  left_join(primary_fuel_types_923_boilers) # adding primary fuel type as determined by fuel type of max heat input of boiler 

print(glue::glue("{nrow(eia_923_boilers_heat_co2) - nrow(eia_boilers_to_add)} EIA-923 boilers removed because:\n 
                  1) plant_id is already in CAMD, or\n
                  2) boiler does not match plant/boiler in EIA-860 files (EIA-860 Boiler Generator, EIA-860 Combined)"))



## EIA generators -----

# We add additional generators from the EIA-860 (combined file). 
# We remove certain generators: 
  # 1) Generators from plants already included from CAMD, unless it is a renewable EIA generator that is not included in CAMD
  # 3) Generators that are included within eia 923 boilers. These are identified using the EIA-860 Boiler Generator file, which serves as a crosswalk between associated boiler and generator ids

# creating unique ids for plant/boiler and plant/generator combos. These will be used to identify units already included from 923 boiler file 
eia_860_boil_gen <- 
  eia_860$boiler_generator %>%
    mutate(id_gen = paste0(plant_id, "_", generator_id), # creating unique id for generator ids
           id_boil = paste0(plant_id, "_", boiler_id)) # creating unique id for boiler id

eia_860_gens_to_remove <-
  eia_860_boil_gen %>%
  filter(id_boil %in% eia_boilers_to_add$id) %>%
  pull(id_gen) %>%
  c(.,eia_boilers_to_add$id) # adding additional units that are in eia_boilers but not in 860_boiler_generator file



# identify EIA renewable units from plants in CAMD. These will be excluded when filtering out CAMD plants below
renewable_ids <- 
  eia_860$combined %>% 
  filter(plant_id %in% camd_7$plant_id, # only plants in CAMD
         energy_source_1 %in% c("SUN", "WAT", "WND")) %>% # identifying renewable sources
  mutate(id = paste0(plant_id, "_", generator_id)) %>%
  pull(id)
  
eia_860_generators_to_add <- # TG 6/27/24 - losing plant_names here, I think we need to keep them for final output
  eia_860$combined %>% 
  mutate(id = paste0(plant_id, "_", generator_id)) %>% 
  filter(!plant_id %in% camd_7$plant_id & !id %in% renewable_ids, # removing generators from plants in CAMD, unless it is a renewable generator
         !id %in% eia_860_gens_to_remove)  %>%
  select(plant_id, 
         plant_state, 
         generator_id, 
         prime_mover,
         "operating_status" = status,
         "primary_fuel_type" = energy_source_1)

### Update fuel types where mismatch between EIA sources -----


gen_fuel_types_to_update <- 
  eia_923$generation_and_fuel_combined %>% 
  group_by(plant_id) %>% 
  slice_max(n = 1, total_fuel_consumption_quantity) %>% # identifying primary fuel
  ungroup() %>%
  filter(total_fuel_consumption_quantity !=0, # where max fuel isn't 0
         !is.na(total_fuel_consumption_quantity)) %>% 
  select(plant_id, total_fuel_consumption_quantity, fuel_type) %>% 
  inner_join(
    (eia_860_generators_to_add %>% # finding primary fuels in 860
       select(plant_id,
              generator_id,
              fuel_type_860 = primary_fuel_type) %>%
       group_by(plant_id) %>% 
       mutate(n_gens = n()) %>% 
       filter(n_gens == 1)), # only for plants with 1 generator
    by = "plant_id") %>% 
  mutate(diffs = if_else(fuel_type != fuel_type_860, TRUE, FALSE)) %>%  # identifying discrepancies
  filter(diffs == TRUE) %>% 
  mutate(primary_fuel_type = fuel_type) %>%
  select(plant_id, 
         generator_id, 
         primary_fuel_type)

# update primary fuel types 

eia_860_generators_to_add_2 <- 
  eia_860_generators_to_add %>%
  rows_update(gen_fuel_types_to_update, # updating with new fuel codes
            by = c("plant_id", "generator_id")) 

### Add NUC and GEO generators ----
# some generators that are type "NUC" and "GEO" are in 860 but not included in CAMD. These don't get added above because their associated plant_ids are in CAMD. 
# We add these and distribute heat from 923 file 

nuc_geo_gens_to_add <- 
  eia_860$combined %>% 
  filter(plant_id %in% camd$plant_id,
         energy_source_1 %in% c("NUC", "GEO")) %>% 
  left_join(eia_923$generation_and_fuel_combined %>% select(plant_id, nuclear_unit_id, prime_mover, starts_with("tot"), total_fuel_consumption_mmbtu),
            by = c("plant_id", "generator_id" = "nuclear_unit_id", "prime_mover")) %>% 
  mutate(heat_input = total_fuel_consumption_mmbtu,
         heat_input_oz = rowSums(pick(all_of(heat_923_oz_months)), na.rm = TRUE)) %>% 
  select(plant_id,
         generator_id,
         prime_mover,
         primary_fuel_type = energy_source_1,
         operating_status = status,
         heat_input,
         heat_input_oz)

eia_860_generators_to_add_3 <-
  eia_860_generators_to_add_2 %>%
  bind_rows(nuc_geo_gens_to_add)

# Include additional biomass units ------

# We include additional biomass units. This is a static table that is created each year after the plant file.
# (SB: not sure if these should be added to EIA generators/boilers, or what)

biomass_units <- 
  read_csv("data/static_tables/biomass_units_to_add_to_unit_file.csv") %>%
  rename("primary_fuel_type" = fuel_type)


# Fill missing heat inputs for all units --------

# Now we combine all units, and fill heat inputs where missing with various approaches

## Combine all units ------

all_units <- # binding all units together, and adding a source column to track row origins
  bind_rows((camd_7 %>% mutate(source = "CAMD")),
            (eia_boilers_to_add %>% mutate(source = "923 boilers") %>% rename("unit_id" = boiler_id)),
            (eia_860_generators_to_add_3 %>% mutate(source = "860 generators") %>% rename("unit_id" = generator_id)))

units_missing_heat <- # creating separate dataframe of units with missing heat input to update
  all_units %>% 
  filter(is.na(heat_input))


print(glue::glue("{nrow(units_missing_heat)} units with missing heat inputs to update."))

## Update heat input with EIA prime-mover level data --------



# We calculate a distributional proportion to distribute heat to generators based on nameplate capacity using 923 gen and fuel and generator file.

# calculating ratio from generator file based on nameplate capacity to distribute heat

gen_file <- # load generator file
  read_rds("data/outputs/generator_file.RDS")


dist_props_923 <- # determining distributional proportions to distribute heat 
  gen_file %>% 
  select(plant_id, prime_mover, generator_id, nameplate_capacity) %>%
  group_by(plant_id, prime_mover) %>% 
  mutate(sum_namecap = sum(nameplate_capacity)) %>%
  ungroup() %>% 
  mutate(proportion = nameplate_capacity/sum_namecap) %>% # ? There is an if_else statement here about missing nameplate capacity involing 860--notsure why that file is invovled
  select(plant_id, prime_mover, generator_id, proportion)

distributed_heat_input <- 
  dist_props_923 %>% 
  left_join(eia_fuel_consum_pm %>% select(plant_id, prime_mover, heat_input_ann_923, heat_input_oz_923)) %>% 
  mutate(heat_input = proportion * heat_input_ann_923,
         heat_input_oz = proportion * heat_input_oz_923) %>% 
  select(plant_id, 
         prime_mover, 
         generator_id, 
         heat_input, 
         heat_input_oz) %>% 
  filter(!is.na(heat_input), # keeping only heat inputs that aren't missing or aren't 0
         heat_input != 0) 
  

units_heat_updated_pm_data <- # dataframe with units having heat input updated by 923 gen and fuel file
  units_missing_heat %>% 
  rows_update(y = distributed_heat_input %>% rename("unit_id" = generator_id, -prime_mover), # updating heat input and source columns where available
              by = c("plant_id", "unit_id"),
              unmatched = "ignore") %>% # ignore rows in distributed_heat_input that aren't in units_missing_heat
  filter(!is.na(heat_input)) %>% 
  mutate(heat_input_source = "EIA Prime Mover-level Data",
         heat_input_oz_source = "EIA Prime Mover-level Data") # (SB: there might need to be a condition here)

print(glue::glue("{nrow(units_heat_updated_pm_data)} units updated with EIA Prime Mover-level Data. {nrow(units_missing_heat) - nrow(units_heat_updated_pm_data)} with missing heat input remain."))


units_missing_heat_2 <- # creating updated dataframe with remaining missing heat inputs
  units_missing_heat %>% 
  anti_join(units_heat_updated_pm_data, # removing units that were updated with PM data.
            by = c("plant_id", "unit_id"))

# 2u078 is a specific query regarding DC CAMD plant to update hti and emissions. Criteria is unit 5c from CAMD file. Not sure what this is about

## Update heat input for direct boiler matches ------

## Match units eia-923 boiler file on plant and boiler id ---

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

# here we estimate heat to be distributed to boilers based on differences between pm level heat we have included and what is in 923 gen and fuel 


boiler_dist_props <-  # determining distributional proportions for 923 boilers 
  eia_923_boilers %>%
  group_by(plant_id,
           boiler_id,
           prime_mover) %>% 
  slice_max(total_fuel_consumption_quantity, 
            n = 1,
            with_ties = FALSE) %>%  # This is done automatically in Access. Need to check
  ungroup() %>%
  group_by(plant_id, prime_mover) %>% 
  mutate(sum_totfuel = sum(total_fuel_consumption_quantity),
         proportion = total_fuel_consumption_quantity/sum_totfuel) %>% 
  select(plant_id, prime_mover, boiler_id, proportion)

heat_differences <- # calculating prime mover-level heat differences between units included in unit file and 923 gen&fuel file
  all_units %>% 
  rows_update(units_heat_updated_pm_data, by = c("plant_id", "unit_id")) %>% 
  rows_update(units_heat_updated_boiler_matches, by = c("plant_id", "unit_id")) %>% 
  group_by(plant_id, prime_mover) %>% 
  summarize(across(c("heat_input", "heat_input_oz"), ~ sum(.x, na.rm = TRUE))) %>% 
  left_join(eia_fuel_consum_pm %>% select(plant_id, prime_mover, starts_with("heat"))) %>% 
  mutate(heat_diff = heat_input_ann_923 - heat_input,
         heat_oz_diff = heat_input_oz_923 - heat_input_oz) %>% 
  filter(heat_diff > 0) %>% # keeping only differences where 923 values are greater than boiler values
  select(plant_id, prime_mover, ends_with("diff"))


units_heat_updated_boiler_distributed <- 
  units_missing_heat_3 %>% 
  select(plant_id, unit_id) %>%
  inner_join(boiler_dist_props, # now joining missing heat units with boiler dist group to keep boilers where we have a distributional proportion
            by = c("plant_id", "unit_id" = "boiler_id")) %>% 
  filter(!is.na(proportion)) %>%
  left_join(heat_differences) %>%
  filter(!is.na(heat_diff)) %>% 
  mutate(heat_input = heat_diff * proportion,
         heat_input_oz = heat_oz_diff * proportion,
         heat_input_source = "EIA Prime Mover-level data",
         heat_input_oz_source = "EIA Prime Mover-level data") %>%
  select(plant_id, 
         unit_id, 
         starts_with("heat_input"),
         ends_with("source"))

units_missing_heat_4 <- 
  units_missing_heat_3 %>% 
  anti_join(units_heat_updated_boiler_distributed,
            by = c("plant_id", "unit_id"))

print(glue::glue("{nrow(units_heat_updated_boiler_distributed)} units updated with EIA Prime Mover-level Data, distributed from 923 Generation and Fuel File. {nrow(units_missing_heat_4)} with missing heat input remain."))


## Update heat input for null CAMD plants ----------

# SB 6/10/24: skipping step here to fill in missing fuel types. Need to address at some point.

camd_missing_props <-   
  units_missing_heat_4 %>% 
  select(plant_id, unit_id) %>% 
  inner_join(camd_7,
             by = c("plant_id", "unit_id")) %>% 
  filter(!is.na(operating_hours), # removing units that were not operating in year
         operating_hours != 0) %>% 
  mutate(primary_fuel_type = if_else(primary_fuel_type == "PRG", "OG", primary_fuel_type)) %>% # need to temporarily change this to match 923
  group_by(plant_id, prime_mover, primary_fuel_type) %>% 
  mutate(sum_nameplate = sum(nameplate_capacity)) %>%
  ungroup() %>%
  select(plant_id, unit_id, primary_fuel_type, nameplate_capacity, sum_nameplate) %>%
  mutate(prop = nameplate_capacity/sum_nameplate) %>% 
  filter(!is.na(prop))

  
  
eia_923_heat_fuel_type <- # summing fuel to fuel type level
  eia_923$generation_and_fuel_combined %>%
  mutate(unit_heat_nonoz = rowSums(pick(all_of(heat_923_nonoz_months)), na.rm = TRUE),
         unit_heat_oz = rowSums(pick(all_of(heat_923_oz_months)), na.rm = TRUE)) %>% 
  group_by(plant_id, prime_mover, fuel_type) %>%
  summarize(heat_input_nonoz_923 = sum(unit_heat_nonoz, na.rm = TRUE),
            heat_input_oz_923 = sum(unit_heat_oz, na.rm = TRUE),
            heat_input_ann_923 = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) # consumption in mmbtus is referred to as "heat input"


units_heat_updated_camd <- 
  camd_7 %>% 
  filter(plant_id %in% camd_missing_props$plant_id) %>% 
  mutate(primary_fuel_type = if_else(primary_fuel_type == "PRG", "OG", primary_fuel_type)) %>% # need to temporarily change this to match 923
  group_by(plant_id, prime_mover, primary_fuel_type) %>% 
  summarize(across(c("heat_input","heat_input_oz"), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  left_join(eia_923_heat_fuel_type,
            by = c("plant_id", "prime_mover", "primary_fuel_type" = "fuel_type")) %>%
  mutate(heat_diff_annual = heat_input_ann_923 - heat_input,
         heat_diff_oz = heat_input_oz_923 - heat_input_oz) %>%
  filter(heat_diff_annual > 0,
         !is.na(heat_diff_annual)) %>% 
  select(plant_id, prime_mover, primary_fuel_type, heat_diff_annual, heat_diff_oz) %>% 
  right_join(camd_missing_props) %>% 
  mutate(heat_input = prop * heat_diff_annual,
         heat_input_oz = prop * heat_diff_oz,
         heat_input_source = "EIA Prime Mover-level Data",
         heat_input_oz_source = "EIA Prime Mover-level Data") %>% 
  select(plant_id, unit_id, prime_mover, heat_input, heat_input_oz) %>% 
  filter(!is.na(heat_input))


units_missing_heat_5 <-
  units_missing_heat_4 %>%
  anti_join(units_heat_updated_camd,
            by = c("plant_id", "unit_id"))


print(glue::glue("{nrow(units_heat_updated_camd)} null CAMD units updated with EIA Prime Mover-level Data, distributed from 923 Generation and Fuel File. {nrow(units_missing_heat_5)} with missing heat input remain."))


## Updating all units with filled heat input

filled_heat_inputs <- 
  bind_rows(units_heat_updated_pm_data, 
            units_heat_updated_boiler_matches, 
            units_heat_updated_boiler_distributed, 
            units_heat_updated_camd) %>% 
  select(plant_id, unit_id, heat_input, heat_input_oz, heat_input_source, heat_input_oz_source)


all_units_2 <- 
  all_units %>% 
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
  mutate(hg_controls_device = "Yes")


## Update OG fuel types  --------

og_fuel_types_update <- 
  read_csv("data/static_tables/og_oth_units_to_change_fuel_type.csv") %>% 
  mutate(across(everything(), ~ as.character(.x))) %>%
  select(plant_id, unit_id,  fuel_code)

## Schedule 8c updates ------

xwalk_control_ids <- # xwalk to add additional boiler ids
  read_csv("data/static_tables/xwalk_860_boiler_control_id.csv") %>% 
  mutate(across(everything(), as.character))
  

schedule_8c_nox <- 
  eia_923$air_emissions_control_info %>%
  distinct(plant_id, nox_control_id, pick(starts_with("nox_emission_rate"))) %>% 
  filter(!is.na(nox_emission_rate_entire_year_lbs_mmbtu)) %>% 
  left_join(eia_860$boiler_nox %>% select(plant_id, nox_control_id, boiler_id) %>% filter(!is.na(boiler_id)) %>% distinct(), ## SB: causes m:m join
            by = c("plant_id","nox_control_id")) %>% 
  rows_patch(xwalk_control_ids %>% select(plant_id, boiler_id,"nox_control_id" =  `860_nox_control_id`) %>% drop_na(),
             by = c("plant_id", "nox_control_id"),
             unmatched = "ignore")
  
schedule_8c_so2  <-
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
  rows_patch(xwalk_control_ids %>% select(plant_id, boiler_id, "mercury_control" =  `860_pm_control_id`) %>% drop_na(),
             by = c("plant_id", "mercury_control"),
             unmatched = "ignore")


## Updating units with available values ------

all_units_3 <-  
  all_units_2 %>% 
  rows_patch(so2_controls_860 %>% # updating with available 860 so2 controls
               rename("unit_id" = "boiler_id"),
            by = c("plant_id","unit_id"),
            unmatched = "ignore") %>%
  rows_patch(nox_controls_860 %>%  # updating with available 860 nox controls
               rename("unit_id" = "boiler_id"),
             by = c("plant_id", "unit_id"),
             unmatched = "ignore") %>%
  mutate(primary_fuel_type = if_else(primary_fuel_type == "MSB", "MSW", "MSB")) %>% # Changing "MSB" fuel codes to "MSW"
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
    mutate(across( # calculating monthly boiler heat input, based on correponding consumption and mmbtu_per_unit
                .cols = starts_with("quantity_of_fuel_consumed_"),
                .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "mmbtu_per_unit_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
                .names = "heat_input_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
           across( # calculating monthly boiler heat input, based on correponding consumption and mmbtu_per_unit
             .cols = starts_with("quantity_of_fuel_consumed_"),
             .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "sulfur_content_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
             .names = "sulfur_content_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
           total_heat_input = rowSums(pick(starts_with("heat_input")), na.rm = TRUE),
           avg_sulfur_content = if_else(total_fuel_consumption_quantity > 0, # calculating avg sulfar content with condition to ignore if i total_fuel_consumption is 0
                                        rowSums(pick(starts_with("sulfur_content")), na.rm = TRUE)/total_fuel_consumption_quantity, 
                                        rowSums(pick(starts_with("sulfur_content")), na.rm = TRUE)/1)) %>%
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
  avg_sulfur_content %>%
  left_join(all_units_2 %>% 
              select(plant_id, unit_id, prime_mover, botfirty, primary_fuel_type), 
            by = c("plant_id",  "boiler_id" = "unit_id",  "fuel_type" = "primary_fuel_type")) %>% 
  left_join(schedule_8c_so2 %>% 
              select(plant_id, boiler_id, so2_removal_efficiency_rate_at_annual_operating_factor)) %>% 
  left_join(emission_factors %>%
              select(prime_mover, botfirty, so2_ef, so2_flag, unit_flag, primary_fuel_type),
            by = c("prime_mover", "botfirty", "fuel_type" = "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(is.na(so2_flag), 1, avg_sulfur_content), # if so2_flag is missing, change avg_sulfur_content to 1
         so2_mass = if_else(unit_flag == "PhysicalUnits", 
                                 (so2_ef * avg_sulfur_content * total_fuel_consumption_quantity * (1 - so2_removal_efficiency_rate_at_annual_operating_factor)) / 2000,
                                 (so2_ef * avg_sulfur_content * total_heat_input * (1 - so2_removal_efficiency_rate_at_annual_operating_factor)) / 2000)
         ) %>% 
  filter(so2_mass > 0) %>% 
  mutate(so2_source = "Estimated using emissions factor and plant-specific sulfur content") %>% 
  select(plant_id, boiler_id, so2_mass, so2_source, unit_flag) %>%
  add_count(plant_id ,boiler_id, sort = TRUE) %>% # Some units match to multiple EF under different unit_flags. We want default to be "PhysicalUnits", so where there are multiple rows per unit, we take only "PhysicalUnits"
  filter(unit_flag == "PhysicalUnits" & n == 1 |
         unit_flag == "PhysicalUnits" & n == 2 |
         unit_flag != "PhsycialUnits" & n == 1  ) %>%
  select(-n)



  
### Join sulfur emissions to all units df  --------
all_units_4 <- 
  all_units_3 %>%
  rows_update(estimated_so2_emissions_content %>% rename("unit_id" = "boiler_id") %>% select(-unit_flag),
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


coal_fuels <- c("BIT", "LIG", "SUB", "RC", "WC", "SGC", "COG")

default_sulfur_content_coal <- 
  eia_923$boiler_fuel_data %>% 
  group_by(plant_state, fuel_type) %>% 
  summarize(across(c(starts_with(c("quantity", "mmbtu_")), "total_fuel_consumption_quantity"), ~ sum(.x, na.rm = TRUE)),
            across(starts_with("sulfur_content"), ~ max(.x))) %>% 
  ungroup() %>% 
  mutate(across( # calculating monthly boiler heat input, based on correponding consumption and mmbtu_per_unit
    .cols = starts_with("quantity_of_fuel_consumed_"),
    .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "mmbtu_per_unit_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
    .names = "heat_input_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
    across( # calculating monthly boiler heat input, based on correponding consumption and mmbtu_per_unit
      .cols = starts_with("quantity_of_fuel_consumed_"),
      .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "sulfur_content_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
      .names = "sulfur_content_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
    total_heat_input = rowSums(pick(starts_with("heat_input")), na.rm = TRUE),
    avg_sulfur_content = if_else(total_fuel_consumption_quantity > 0, # calculating avg sulfur content with condition to ignore if i total_fuel_consumption is 0
                                 rowSums(pick(starts_with("sulfur_content")), na.rm = TRUE)/total_fuel_consumption_quantity, 
                                 rowSums(pick(starts_with("sulfur_content")), na.rm = TRUE)/1)) %>%
  filter(fuel_type %in% coal_fuels) %>% 
  select(plant_state,
         fuel_type,
         avg_sulfur_content)



units_estimated_fuel <- # df that will be used to calculate so2 emissions 
  all_units_4 %>% 
  group_by(plant_state, plant_id, prime_mover, primary_fuel_type) %>% 
  mutate(total_heat_input = sum(heat_input, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(dist_prop = heat_input/total_heat_input) %>% 
  select(plant_state,
         plant_id, 
         prime_mover, 
         unit_id, 
         primary_fuel_type,
         botfirty,
         dist_prop) %>%
  left_join(eia_fuel_consum_fuel_type,
            by = c("plant_id", "plant_state", "prime_mover", "primary_fuel_type" = "fuel_type")) %>%
  mutate(fuel_consumption = fuel_consum_ann_923 * dist_prop,
         fuel_consumption_oz = fuel_consum_oz_923 * dist_prop,
         heat_input = heat_input_ann_923 * dist_prop,
         heat_input_oz = heat_input_oz_923 * dist_prop) %>% 
  select(-c(ends_with("_923"))) 

### estimating so2 emissions - coal --------

so2_emissions_pu_coal <-
  units_estimated_fuel %>% 
  left_join(default_sulfur_content_coal, # renaming for consistent naming
            by = c("plant_state",
                   "primary_fuel_type" = "fuel_type")) %>% 
  left_join(emission_factors %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S", avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "PhysicalUnits",
                            so2_ef * avg_sulfur_content * fuel_consumption,
                            so2_ef * avg_sulfur_content * heat_input/2000)) %>%
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
  left_join(emission_factors %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S", avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "PhysicalUnits",
                            so2_ef * avg_sulfur_content * fuel_consumption,
                            so2_ef * avg_sulfur_content * heat_input/2000)) %>%
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
  left_join(emission_factors %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S", avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "HeatInput",
                            so2_ef * avg_sulfur_content * heat_input / 2000,
                            so2_ef * avg_sulfur_content * 0 / 2000)) %>%
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
  left_join(emission_factors %>% 
              select(prime_mover, primary_fuel_type, botfirty, so2_ef, so2_flag, unit_flag), 
            by = c("prime_mover",
                   "botfirty",
                   "primary_fuel_type")) %>%
  mutate(avg_sulfur_content = if_else(so2_flag == "S", avg_sulfur_content, 1),
         so2_mass = if_else(unit_flag == "HeatInput",
                            so2_ef * avg_sulfur_content * heat_input / 2000,
                            so2_ef * avg_sulfur_content * 0 / 2000)) %>%
  select(plant_id,
         unit_id,
         so2_mass) %>% 
  filter(so2_mass > 0) %>% 
  mutate(so2_source = "Estimated using emissions factor")

### Updating units with estimated SO2 --------

all_units_5 <- 
  all_units_4 %>% 
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

co2_emissions <- 
  all_units_5 %>%
  filter(plant_id != 10025) %>% ## Removing duplicate Plant ID 10025/Unit ID 4B that was causing issues with rows_patch
  select(plant_id, unit_id, primary_fuel_type, heat_input, co2_emissions) %>%
  inner_join(co2_ef %>%
              filter(!is.na(eia_fuel_code)) %>%
              select(eia_fuel_code, co2_ef), by = c("primary_fuel_type" = "eia_fuel_code")) %>%
  filter(is.na(co2_emissions)) %>%
  mutate(co2_emissions = heat_input * co2_ef) %>%
  select(-c(co2_ef, heat_input, primary_fuel_type))

### Updating units with estimated CO2 emissions --------

all_units_6 <- 
  all_units_5 %>% 
  filter(plant_id != 10025) %>% ## Removing duplicate Plant ID 10025/Unit ID 4B that was causing issues with rows_patch
  rows_patch(co2_emissions %>% distinct(),
             by = c("plant_id", "unit_id"),
             unmatched = "ignore")

## NOx Emissions Rate -----

nox_rates_2 <- schedule_8c_nox %>%
  group_by(plant_id, boiler_id) %>%
  summarize(nox_rate = max(nox_emission_rate_entire_year_lbs_mmbtu),
            nox_oz_rate = max(nox_emission_rate_may_through_september_lbs_mmbtu))

nox_emissions_rates <- 
  all_units_6 %>%
  rows_patch(nox_rates_2, 
            by = c("plant_id", "unit_id"), ## Error with this part of the script because some columns aren't present in all_units_4 that are in nox_rates_2
            unmatched = "ignore")


# Final modifications -----  

## Clean up source flags --------

clean_source_flags <- 
  all_units_6 %>% 
  mutate(nox_source = replace(nox_source, is.na(nox_mass), NA), # replacing NA emission masses with NA sources
         so2_source = replace(so2_source, is.na(so2_mass), NA), 
         co2_source = replace(co2_source, is.na(co2_mass), NA), 
         hg_source = replace(hg_source, is.na(hg_mass), NA))


## Change necessary plant names -------
# Check for duplicates
# TG: current duplicates at plant level have NA as names 
# TG: I think this could be fixed earlier in the file by collecting plant_names in eia 860 data

check_plant_names <- 
  all_units_6 %>% select(plant_id, plant_name) %>% 
  distinct(plant_id, plant_name) %>% 
  group_by(plant_id) %>% 
  filter(n()>1 & !is.na(plant_name))
  
# update specified units

update_eia_names <- 
  all_units_6 %>% select(plant_id, plant_name) %>% 
  filter(plant_id %in% c("2847", "55248")) %>% 
  mutate(eia_plant_id = case_when(plant_id %in% c("2847", "55248") ~ "2847"), 
         eia_plant_name = case_when(plant_id %in% c("2847", "55248") ~ "Tait Electric Generating Station")) %>% 
  distinct()
   
  

## Update FC prime mover CO2 emissions data ------
# Update FC prime mover to null CO2 emissions 

update_fc_data <- 
  all_units_6 %>% select(plant_id, unit_id, prime_mover, co2_mass, co2_source) %>% 
  filter((prime_mover == "FC") & ((!is.na(co2_mass)) | (!is.na(co2_source)))) %>%  # only update necessary rows 
  mutate(co2_mass = NA, 
         co2_source = NA, 
         co2_mass = as.numeric(co2_mass), 
         co2_source = as.character(co2_source)) %>% 
  select(plant_id, unit_id, co2_mass, co2_source) 

## Delete specified units -------- 
# Delete units in "Units to remove" table, which is manually updated 

units_to_remove <- 
  read_csv("data/static_tables/units_to_remove.csv") %>% 
  mutate(plant_id = as.character(plant_id))

## Update prime mover -------
# Update prime mover for plant 7063 unit **1 from OT to CE 

update_plant_mover <- 
  all_units_6 %>% select(plant_id, unit_id, prime_mover) %>% 
  filter(plant_id == "7063" & unit_id == "**1") %>% 
  mutate(prime_mover = "CE")

## Update status ------
# Update operating status via EIA-923 schedule 8C, EIA-860 boiler info, EIA-860 combined
# EIA-923 Schedule 8C has many duplicates, defaulting to "OP", then filling in with others if "OP" does not exist for unit

schedule_8c_longer <- eia_923$air_emissions_control_info %>% 
  pivot_longer(cols = contains("control"), 
               names_to = "unit_control", 
               values_to = "unit_id") %>% 
  select(plant_id, status, unit_id) %>% 
  filter(!is.na(unit_id)) %>% distinct() %>% 
  rename("operating_status" = "status")  

# split out by operating status   
schedule_8c_op <- 
  schedule_8c_longer %>% 
  filter(operating_status == "OP") 

schedule_8c_os <- 
  schedule_8c_longer %>% 
  filter(operating_status == "OS")

schedule_8c_oa <- 
  schedule_8c_longer %>% 
  filter(operating_status == "OA")

schedule_8c_sb <- 
  schedule_8c_longer %>% 
  filter(operating_status == "SB")

schedule_8c_re <- 
  schedule_8c_longer %>% 
  filter(operating_status == "RE")

# update operating status 

update_status <- 
  all_units_6 %>% select(plant_id, unit_id, operating_status) %>% 
  filter(is.na(operating_status)) %>%
  rows_patch(schedule_8c_op %>% select(plant_id, unit_id, operating_status), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_patch(schedule_8c_os %>% select(plant_id, unit_id, operating_status), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_patch(schedule_8c_oa %>% select(plant_id, unit_id, operating_status), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_patch(schedule_8c_sb %>% select(plant_id, unit_id, operating_status), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_patch(schedule_8c_re %>% select(plant_id, unit_id, operating_status), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_patch(eia_860$boiler_info_design_parameters %>% select(plant_id, "unit_id" = boiler_id, "operating_status" = boiler_status), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_patch(eia_860$combined %>% select(plant_id, "operating_status" = status, "unit_id" = generator_id), 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  mutate(operating_status = toupper(operating_status)) %>% 
  filter(!is.na(operating_status)) %>% distinct()


## Create and edit CAMD flag ----- 

# ID camd units 

camd_units <- camd_7 %>% mutate(id = paste0(plant_id, unit_id)) %>% pull(id)

# update PR CAMD flag from crosswalk

update_pr_camd_flag <- read_csv("data/static_tables/xwalk_pr_oris.csv") %>% 
  select("plant_id" = "eia_plant_id", 
         "unit_id" = "eia_unit_id", 
         "plant_name" = "eia_plant_name") %>% 
  mutate(across(where(is.numeric), ~as.character(.)), 
         camd_flag = "Yes")

# create CAMD flag 

all_units_7 <- 
  all_units_6 %>%  
  mutate(camd_flag = if_else(paste0(plant_id, unit_id) %in% camd_units, "Yes", "No")) %>% 
  rows_update(update_pr_camd_flag, by = c("plant_id", "unit_id"))

## Retired units to delete ------- 

# delete plants based on operating status from EIA-860 Boiler Info & Design Parameters

delete_retired_units <- 
  update_status %>% 
  filter(operating_status == "RE")


## EIA units to delete ------ 

# need to check this each year

eia_units_to_delete <- 
  all_units_7 %>% 
  filter(plant_id == "2132" | plant_id == "7832") %>% 
  select(plant_id) %>% distinct()


## Implement changes in main unit file ------

all_units_8 <- 
  all_units_7 %>% # update to most recent unit file data frame
  rows_delete(units_to_remove, 
              by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_delete(delete_retired_units, 
              by= c("plant_id", "unit_id"), unmatched = "ignore") %>%
  rows_delete(eia_units_to_delete, 
              by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(update_fc_data, 
                by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_update(update_plant_mover, 
             by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  rows_patch(check_plant_names, 
             by = c("plant_id"), unmatched = "ignore") %>% 
  rows_update(update_status, 
              by = c("plant_id", "unit_id"), unmatched = "ignore") %>% 
  left_join(update_eia_names, by = c("plant_id", "plant_name")) %>%
  mutate(heat_input = round(heat_input, 3), 
         heat_input_oz = round(heat_input_oz, 3), 
         nox_mass = round(nox_mass, 3), 
         nox_mass_oz = round(nox_mass_oz, 3), 
         so2_mass = round(so2_mass, 4), 
         co2_mass = round(co2_mass, 3), 
         hg_mass = round(hg_mass, 3)) 


