

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

# Identify EIA units to add (generators and boilers) -----------

## EIA boilers ----------

eia_923_boilers <- 
  eia_923$boiler_fuel_data %>% 
  mutate(across( # calculating monthly unit heat input, based on correponding consumption and mmbtu_per_unit
                .cols = starts_with("quantity_of_fuel_consumed_"),
                .fns = ~ . * get(str_replace(cur_column(), "quantity_of_fuel_consumed_", "mmbtu_per_unit_")), # identifies corresponding mmbtu_per_unit and multiplies by quantity column
                .names = "heat_input_{str_replace(.col, 'quantity_of_fuel_consumed_','')}"),
         heat_input = rowSums(pick(all_of(starts_with("heat_input")))), # getting annual heat_input, summing across all monthly heat columns
         heat_input_oz = rowSums(pick(all_of(paste0("heat_input_",tolower(month.name[5:9])))))) %>%  # summing across ozone months
  select(plant_id, plant_name, plant_state, prime_mover, boiler_id, fuel_type, heat_input, heat_input_oz)  


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
    pull(id_gen)


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
  mutate(diffs = if_else(fuel_type != fuel_type_860, TRUE, FALSE)) %>%  # identfying discrepencies
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

# Include additional biomass units ------

# We include additional biomass units. This is a static table that is created each year after the plant file.
# (SB: not sure if these should be added to EIA generators/boilers, or what)

biomass_units <- 
  read_csv("data/static_tables/biomass_units_to_add_to_unit_file.csv") %>%
  rename("primary_fuel_type" = fuel_type)


# DC CAMD plants (queries 2u074 through 2u077b) ----

oz_months_923 <- c("TOTMAY", "TOTJUN", "TOTJUL", "TOTAUG", "TOTSEP")

# calculating heat ratio and distributed to heat for generators based on nameplate capacity using 923 and generator file.


`heat_sum_923` <- #2u074
  eia_923_gen_fuel %>% 
  group_by(PSTATEABB, PNAME, ORISPL, PRMVR) %>% 
  mutate(heat_oz = rowSums(pick(oz_months_923))) %>%
  summarize(HeatInput = sum(TOTFCONS),
            HeatInputOZ = sum(heat_oz)
  ) %>% 
  arrange(ORISPL, PSTATEABB) %>% 
  ungroup()



# calculating ratio from generator file based on nameplate capacity to distribute heat
gen_file <- 
  read_excel("output/Generator_file.xlsx")

ratio_for_distribution <- #2u077b
  gen_file %>% 
  filter(((GENSTAT %in% c("OP", "SB", "OS", "OA")) & (!is.na(GENNTAN) | GENNTAN != 0)) | (GENSTAT == "RE") & GENYRRET == 2021 & (!is.na(GENNTAN) | GENNTAN != 0)) %>% 
  group_by(ORISPL, PRMVR, GENID, GENSTAT, GENYRRET) %>% 
  mutate(sum_namecap = sum(NAMEPCAP),
         sum_genntan = sum(GENNTAN)) %>% 
  ungroup() %>% 
  group_by(ORISPL, PRMVR) %>% 
  mutate(sum_namecap_pm = sum(sum_namecap)) %>% 
  mutate(ratio = sum_namecap/sum_namecap_pm) %>% # ? There is an if_else statement here about missing nameplate capacity involing 860--notsure why that file is invovled
  #select(ORISPL, PRMVR, GENID, ratio, sum_genntan) %>% 
  arrange(ORISPL)  %>% 
  select(ORISPL, 
         GENID,
         PRMVR,
         ratio,
         "netgen" = sum_genntan)



# 2u078 is a specific query regarding DC CAMD plant to update hti and emissions. Criteria is unit 5c from CAMD file. Not sure what this is about

HeatInputToDistribute <- # 2u079 | * Getting many more rows here than in access (24,177 vs. 22,942 in access)
  ratio_for_distribution %>% 
  left_join(heat_sum_923,
            by = c("ORISPL", "PRMVR")) %>% 
  mutate(HeatInputDistributed = HeatInput * ratio,
         HeatInputOZDistributed = HeatInputOZ * ratio) %>% 
  select(ORISPL, 
         GENID,
         PRMVR,
         HeatInputDistributed,
         HeatInputOZDistributed,
         netgen)


# Update Heat Input in Unit File  ----
#2u079b Some big discrepancies stemming from this because the critera for updating heat values depends on missing values for source, etc,
# and many of those fields are incorrectly coded as "0" instead of missing in access, so aren't being updated in Access.

distributed_units <- # creating separate df of units that have been updated
  `Unit File 10` %>% 
  filter(is.na(`Annual Heat Input`),
         is.na(`Oz Seas Heat Input`),
         is.na(`Heat Input Source`),
         is.na(`Heat Input OZ Source`),
         is.na(`Annual Sum Op Time`),
  ) %>% 
  left_join(HeatInputToDistribute,
            by = c("ORIS Code" = "ORISPL" , "UNIT ID" = "GENID")) %>% 
  ungroup() %>% 
  mutate(`Annual Heat Input` = HeatInputDistributed,
         `Oz Seas Heat Input` = HeatInputOZDistributed,
         `Heat Input Source` = "EIA Prime Mover-level",
         `Heat Input OZ Source` = "EIA Prime Mover-level") %>% 
  select(all_of(unit_columns)) 

unique_id <-  function(df) paste0(df$`ORIS Code`, df$`UNIT ID`, df$PrimeMover) # function to create unique ids for filtering ** should use this in other parts


`Unit File 11` <- 
  `Unit File 10` %>% 
  filter(! unique_id(.) %in% unique_id(distributed_units)) %>% # removing units from distributed units df
  bind_rows(distributed_units) %>% 
  arrange(`ORIS Code`, `UNIT ID`)


# Update heat input for direct boiler matches - 2u080a-1-4 ------------------


## match unit file to eia-923 boiler file on plant and boiler id ---

# ??identifying the max fuel consumption for each plant/unit. That max unit is used to determine which rows to update in the unite file?


# Not clear why "max" fuel is being used???

max_tot_fuel <- 
  `Unit File 11` %>% 
  inner_join(eia_923_boilers %>% select("Plant Id","Boiler Id", "Total Fuel ConsumptionQuantity", starts_with(c("Quantity", "MMbtu"))),
             by = c("ORIS Code" = "Plant Id", "UNIT ID" = "Boiler Id"),
  ) %>% 
  group_by(`ORIS Code`,
           `UNIT ID`) %>% 
  slice_max(`Total Fuel ConsumptionQuantity`, n =1 ) %>% 
  mutate(heat_january = `Quantity Of Fuel ConsumedJanuary` * `MMbtu Per UnitJanuary`, # calculating monthly heat input
         heat_february = `Quantity Of Fuel ConsumedFebruary` * `MMbtu Per UnitFebruary`,
         heat_march = `Quantity Of Fuel ConsumedMarch` * `MMbtu Per UnitMarch`,
         heat_april = `Quantity Of Fuel ConsumedApril` * `MMbtu Per UnitApril`,
         heat_may = `Quantity Of Fuel ConsumedMay` * `MMbtu Per UnitMay`,
         heat_june = `Quantity Of Fuel ConsumedJune` * `MMbtu Per UnitJune`,
         heat_july = `Quantity Of Fuel ConsumedJuly` * `MMbtu Per UnitJuly`,
         heat_august = `Quantity Of Fuel ConsumedAugust` * `MMbtu Per UnitAugust`,
         heat_september = `Quantity Of Fuel ConsumedSeptember` * `MMbtu Per UnitSeptember`,
         heat_october = `Quantity Of Fuel ConsumedOctober` * `MMbtu Per UnitOctober`,
         heat_november = `Quantity Of Fuel ConsumedNovember` * `MMbtu Per UnitNovember`,
         heat_december = `Quantity Of Fuel ConsumedDecember` * `MMbtu Per UnitDecember`,
         heat_input = rowSums(pick(starts_with("heat_"))),
         heat_input_oz = rowSums(pick(c("heat_may","heat_june","heat_july","heat_august","heat_september")))) %>% 
  select(`ORIS Code`,
         `UNIT ID`,
         PrimeMover,
         `Fuel Type (Primary)`,
         total_fuel_con = `Total Fuel ConsumptionQuantity`,
         heat_input,
         heat_input_oz)


update_direct_boiler_matches <- 
  `Unit File 11` %>% 
  left_join(max_tot_fuel, 
            by = c("ORIS Code", "UNIT ID", "PrimeMover", "Fuel Type (Primary)")) %>%
  filter(!is.na(heat_input),
         is.na(`Annual Heat Input`),
         heat_input != 0) %>% 
  mutate(`Annual Heat Input` = heat_input,
         `Oz Seas Heat Input` = heat_input_oz,
         `Heat Input Source` = "EIA Unit-level Data",
         `Heat Input Oz Source` = "EIA Unit-level Data") %>% 
  select(all_of(unit_columns))


# updating unit file with direct match boiler heat inputs

`Unit File 12` <- 
  `Unit File 11` %>% 
  filter(!unique_id(.) %in% unique_id(update_direct_boiler_matches)) %>% 
  bind_rows(update_direct_boiler_matches) %>% 
  arrange(-`ORIS Code`)


# Distribute heat input to boilers 2u080b-1 through g ----------------

boiler_923_ratios <-     
  eia_923_boilers %>%
  group_by(`Plant Id`,
           `Boiler Id`,
           `ReportedPrime Mover`) %>% 
  slice_max(`Total Fuel ConsumptionQuantity`, 
            n = 1,
            with_ties = FALSE) %>%  # This is done automatically in Access. Need to check
  select(`Plant Id`, `Boiler Id`, `ReportedPrime Mover`, `Total Fuel ConsumptionQuantity`) %>% 
  group_by(`Plant Id`, `ReportedPrime Mover`) %>% 
  mutate(sum_totfuel = sum(`Total Fuel ConsumptionQuantity`),
         ratio = `Total Fuel ConsumptionQuantity`/sum_totfuel)


sum_unit_heat <- 
  `Unit File 12` %>% 
  group_by(`ORIS Code`, PrimeMover) %>% 
  summarize(sum_heat = sum(`Annual Heat Input`, na.rm = TRUE),
            sum_heat_oz = sum(`Oz Seas Heat Input`, na.rm = TRUE))

hti_diffs_to_distribute <- 
  heat_sum_923 %>% 
  inner_join(sum_unit_heat,
             by = c("ORISPL" = "ORIS Code", "PRMVR" = "PrimeMover")) %>% 
  mutate(diff_heat = HeatInput - sum_heat,
         diff_heat_oz = HeatInputOZ - sum_heat_oz)

heat_input_to_distributed_boilers <-   
  boiler_923_ratios %>% 
  inner_join(hti_diffs_to_distribute,
             by = c("Plant Id" = "ORISPL", "ReportedPrime Mover" = "PRMVR")) %>% 
  filter(diff_heat > 0 ) %>% 
  mutate(heat_dist= diff_heat * ratio,
         heat_oz_dist = diff_heat_oz *ratio) %>% 
  select(
    "ORIS Code" = `Plant Id`, 
    "UNIT ID" = `Boiler Id`, 
    PrimeMover = `ReportedPrime Mover`, 
    heat_dist, 
    heat_oz_dist)

# Update the Unit file with distributed heat

dis_units_to_update <- 
  `Unit File 12` %>% 
  inner_join(heat_input_to_distributed_boilers,
             by = c("ORIS Code", "UNIT ID", "PrimeMover")) %>% 
  filter(is.na(`Annual Heat Input`),
         is.na(`Oz Seas Heat Input`),
         is.na(`Heat Input Source`),
         is.na(`Heat Input OZ Source`))

`Unit File 13` <-
  `Unit File 12` %>% 
  filter(! unique_id(.) %in% unique_id(dis_units_to_update)) %>% 
  bind_rows(dis_units_to_update) %>% 
  arrange(-`ORIS Code`)

# Add in NUC and GEO EIA generators (2u082a -2u084) ------------

## !!? Not sure what is supposed to happen here. Instructions indicate that we find units from EIA-860 Combined to add, but that's not what's happening in access...

nuc_geo_gens <- #why is this happening?
  `EIA-860 Combined` %>% 
  inner_join(camd_r,
             by = c("Plant Code" = "ORIS Code")) %>% 
  filter(`Energy Source 1` %in% c("NUC", "GEO")) %>% 
  distinct(`Plant Code`,
           `Generator ID`,
           `Prime Mover`,
           `Energy Source 1`)



# ?! what is being distributed? It's just adding up fuel consumption from 923. And if that's the case, why are the EIA-860 combined and camd files necessary?


# is this calculated the 
nuc_geo_units_to_update <- 
  `Unit File 13` %>% 
  inner_join(eia_923_gen_fuel %>% 
               filter(`FUELG1` %in% c("NUC", "GEO")) %>% 
               mutate(FUELG1 = as.character(FUELG1), # changing to match
                      NUCUNITID = as.character(NUCUNITID)),
             by = c("ORIS Code" = "ORISPL", "UNIT ID" = "NUCUNITID", "PrimeMover" = "PRMVR", "Fuel Type (Primary)" = "FUELG1" )) %>% 
  mutate(`Annual Heat Input` = rowSums(pick(oz_heat)),
         `Oz Seas Heat Input` = TOTFCONS,
         `Heat Input Source` = "EIA Prime Mover-level Data",
         `Heat Input OZ Source` = "EIA Prime Mover-level Data") %>% 
  select(all_of(unit_columns)) 


# update in Unit File

`Unit File 14` <- 
  `Unit File 13` %>% 
  filter(! unique_id(.) %in% unique_id(nuc_geo_units_to_update)) %>% 
  bind_rows(nuc_geo_units_to_update) %>% 
  arrange(`ORIS Code`)


# Update plant 50489 heat input (2u085) ---------

# skipping individual plant update for now

# Delete plants from EIA that are in CAMD (2u086a) -------------

# removing plants from EIA that are in CAMD based on ORIS crosswalk


# Delete plants from CAMD (2u086b) ----------------

## This is based on a table create manually based on notes from EPA


# Change MSB to MSW (2u087) -----------------

`Unit File 15` <- 
  `Unit File 14` %>% 
  mutate(`Fuel Type (Primary)` = if_else(`Fuel Type (Primary)` == "MSW", "MSB", `Fuel Type (Primary)`))


# Update So2 controls from EIA-860 (2u088) --------------------

# This invovles single PR plant with manual table -- holding off on this 

# Update NOx controls from EIA-860  (2u089) -------------------------

# * need to import boiler nox cross walk table

