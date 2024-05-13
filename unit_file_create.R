

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
  ungroup() %>% 
  mutate(heat_input_nonoz = heat_input_nonoz_923 * prop, # distributing nonoz
         heat_input = if_else(reporting_frequency == "Q", heat_input, heat_input_oz + heat_input_nonoz), # If unit is Q reporter, we use the heat input from CAMD. If OS reporter, we add distributed non-oz heat and ozone heat
         heat_input_source = if_else(reporting_frequency == "Q", "EPA/CAMD", "EIA non-ozone season distributed and EPA/CAMD ozone season"))




# NOx emissions -------

## Using NOx Rate ---------


# *** need to add code to create NOx table here, or do it elsewhere..... ****

`NOx rates` <- 
  read_excel("data/raw_data/static_tables/NOx Rates.xlsx") %>% 
  rename("ORIS Code" = `Plant ID`,
         "UNIT ID" = "Boiler ID") %>% 
  mutate(source_NOx = TRUE)


`CAMD ozone season reporters - heat input and emissions to update 4` <- # 2u047
  `CAMD ozone season reporters - heat input and emissions to update 3` %>% 
  left_join(`NOx rates` %>% 
              select(`ORIS Code`, `UNIT ID`, `NOxRate`, source_NOx),
            by = c("ORIS Code", "UNIT ID")) %>%
  remove_suffix() %>% 
  mutate(NOx = (`Heat Input non-oz` * NOxRate) / 2000,
         `NOx source` = if_else(source_NOx == TRUE, 
                                "Estimated based on unit-level NOx emission rates and EPA/CAMD ozone season emissions", 
                                `NOx source`))


# Using NOx Ef  


`923 fuel consumption for ozone season reporters` <- #2u048a
  `CAMD ozone season reporters` %>% 
  distinct(`ORIS Code`, `Reporting Frequency`) %>% 
  inner_join(eia_923_gen_fuel,
             by = c("ORIS Code" = "ORISPL")) %>% 
  mutate(`Non-oz consump` = rowSums(pick(all_of(nonoz_consum)))) %>%
  select(`ORIS Code`,
         FUELG1,
         "PrimeMover" = "PRMVR",
         PHYSUNIT,
         `Non-oz consump`,
         TOTFCONSQ,
         `Reporting Frequency`
  ) 


## don't know exactly what's happening in 2u048b. It results in 257 rows in Access, which is bizarre

`distribute fuel consump to ozone season units just oz` <- # 2u048b *Introduces some weird multiple matches*
  `Ratio to distribute - all oz` %>% 
  inner_join(`CAMD ozone season reporters - heat input and emissions to update 4` %>% 
               select("UNIT ID", "ORIS Code"),
             by = c("ORIS Code", "UNIT ID")) %>% 
  inner_join(`923 fuel consumption for ozone season reporters` %>% 
               select("ORIS Code", FUELG1, `PrimeMover`, PHYSUNIT, `Non-oz consump`, TOTFCONSQ),
             by = c("ORIS Code", "PrimeMover")) %>% 
  mutate(`Non-oz consump to distribute` = `Non-oz consump` * ratio) %>% 
  select("ORIS Code",
         "UNIT ID",
         FUELG1,
         BOTFIRTY,
         `PrimeMover`,
         `Non-oz consump to distribute`,
         `PHYSUNIT`) 


`distribute fuel consump to ozone season units ann oz`<- # 2u048c 
  `Ratio to distribute - part 3 annual and oz` %>% 
  inner_join(`CAMD ozone season reporters - heat input and emissions to update 4` %>% 
               select("UNIT ID", "ORIS Code"),
             by = c("ORIS Code", "UNIT ID")) %>% 
  inner_join(`923 fuel consumption for ozone season reporters` %>% 
               select("ORIS Code", FUELG1, `PrimeMover`, PHYSUNIT, `Non-oz consump`, TOTFCONSQ),
             by = c("ORIS Code", "PrimeMover")) %>% 
  mutate(`Non-oz consump to distribute` = `Non-oz consump` * ratio) %>% 
  select("ORIS Code",
         "UNIT ID",
         FUELG1,
         BOTFIRTY,
         `PrimeMover`,
         `Non-oz consump to distribute`,
         `PHYSUNIT`) 


EFs <- read_excel("data/raw_data/static_tables/EFs.xlsx")


`calculate NOx non ozone season emissions just oz` <- #2u049a
  `distribute fuel consump to ozone season units just oz` %>% 
  inner_join(EFs,
             by = c("PrimeMover",
                    "BOTFIRTY",
                    "FUELG1" = "Fuel Type (Primary)")) %>% 
  mutate(`NOx non-oz` = (`Non-oz consump to distribute` * NOxEF) / 2000) %>% 
  select("ORIS Code",
         "UNIT ID",
         FUELG1,
         BOTFIRTY,
         PrimeMover,
         `Non-oz consump to distribute`,
         `NOx non-oz`,
         PHYSUNIT,
         NOxEFdenom) 


`calculate NOx non ozone season emissions ann oz` <- #2u049c 
  `distribute fuel consump to ozone season units ann oz` %>% 
  inner_join(EFs,
             by = c("PrimeMover",
                    "BOTFIRTY",
                    "FUELG1" = "Fuel Type (Primary)")) %>% 
  mutate(`NOx non-oz` = (`Non-oz consump to distribute` * NOxEF) / 2000) %>% 
  select("ORIS Code",
         "UNIT ID",
         FUELG1,
         BOTFIRTY,
         `PrimeMover`,
         `Non-oz consump to distribute`,
         `NOx non-oz`,
         PHYSUNIT,
         NOxEFdenom) 


`sum NOx emissions to unit unit level for oz seas reporters` <- #2u050a
  `calculate NOx non ozone season emissions just oz` %>% 
  group_by(`ORIS Code`,
           `UNIT ID`,
           BOTFIRTY,
           `PrimeMover`) %>% 
  summarize(`SumOfNon-oz consump to distribute` = sum(`Non-oz consump to distribute`),
            `SumOfNOx non-oz` = sum(`NOx non-oz`))

`sum NOx emissions to unit unit level for oz seas reporters ann oz` <- #2u050c 
  `calculate NOx non ozone season emissions ann oz` %>% 
  group_by(`ORIS Code`,
           `UNIT ID`,
           BOTFIRTY,
           `PrimeMover`) %>% 
  summarize(`SumOfNon-oz consump to distribute` = sum(`Non-oz consump to distribute`),
            `SumOfNOx non-oz` = sum(`NOx non-oz`))


`CAMD ozone season reporters - NOx emissions to Update` <- # 2u051a Sum of annual NOx for ozone season reporters
  `sum NOx emissions to unit unit level for oz seas reporters` %>% 
  inner_join(`CAMD ozone season reporters`,
             by = c("ORIS Code", "UNIT ID", "BOTFIRTY", "PrimeMover")) %>% 
  mutate(`Annual NOx` = `SumOfNOx non-oz` + `Annual NOx Mass`,
         source_annNOx = TRUE) %>% 
  select(`ORIS Code`,
         `UNIT ID`,
         BOTFIRTY,
         `PrimeMover`,
         `Annual NOx`,
         source_annNOx)

# ? Not sure why these steps involve the joins & why the sums couldn't be done earlier
`CAMD ozone season reporters - NOx emissions to Update ann oz` <- # 2u051c Sum of annual NOx for ozone season reporters ann oz
  `sum NOx emissions to unit unit level for oz seas reporters ann oz` %>% 
  inner_join(`CAMD ozone season reporters`,
             by = c("ORIS Code", "UNIT ID", "BOTFIRTY", "PrimeMover")) %>% 
  mutate(`Annual NOx` = `SumOfNOx non-oz` + `Annual NOx Mass`,
         source_annozNOx = TRUE) %>% 
  select(`ORIS Code`,
         `UNIT ID`,
         BOTFIRTY,
         `PrimeMover`,
         `Annual NOx`)

# Combining tables needed for 2u052a-2u05c

`CAMD ozone season reporters - NOx emissions to Update combined` <-
  `CAMD ozone season reporters - NOx emissions to Update` %>%
  bind_rows(`CAMD ozone season reporters - NOx emissions to Update ann oz`) %>%
  mutate(source_NOxEF = TRUE)

## ?* I don't understand what's happening here in access #2u052a

# `CAMD ozone season reporters - heat input and emissions to update 6` <-
#   `CAMD ozone season reporters - heat input and emissions to update 4` %>% 
#   left_join(`CAMD ozone season reporters - NOx emissions to Update` %>% 
#                ungroup() %>% 
#                select("ORIS Code", "UNIT ID", "Annual NOx", source_annNOx),
#             by = c("UNIT ID", "ORIS Code")) %>% 
#   mutate(NOx = if_else(source_annNOx == TRUE, `Annual NOx`, NOx),
#          `NOx source` = if_else(source_annNOx == TRUE, 
#                                 "Estimated using emissions factor and EIA data for non-ozone season and EPA/CAMD ozone season emissions", 
#                                 `NOx source`)) 

# Updating the heat and emissions intermediate table



`CAMD ozone season reporters - heat input and emissions to update 5` <- #2u052a & 2u052c
  `CAMD ozone season reporters - heat input and emissions to update 4` %>% 
  left_join(`CAMD ozone season reporters - NOx emissions to Update combined`,
            by = c("ORIS Code", "UNIT ID")) %>%
  remove_suffix() %>%
  mutate(`NOx source` = if_else(source_NOxEF == TRUE, 
                                "Estimated using emissions factor and EIA data for non-ozone season and EPA/CAMD ozone season emissions", 
                                `NOx source`),
         `NOx` = if_else(source_NOxEF == TRUE, `Annual NOx`, NOx),
         source_updated = TRUE) %>% 
  select(`ORIS Code`,
         `UNIT ID`,
         `State`,
         `Fuel Type`,
         `Heat Input non-oz`,
         CO2,
         NOx,
         SO2,
         `Heat input source`,
         `CO2 source`,
         `NOx source`,
         `BOTFIRTY`,
         `PrimeMover`,
         `Oz Seas Heat Input`,
         `Oz Seas NOx Mass`,
         `Oz Seas SO2 Mass`,
         `Oz Seas CO2 Mass`
  ) 




## Update the Unit file with the heat input and NOx emissions -----

# Updating `Annual Heat Input`, `Oz Seas Heat Input`, `Heat Input Source`, 

# `Annual heat input` =  If `Heat Input non-oz` is null, Oz Seas Heat Input, non-oz + oz 
# Heat input source = if (heat input non-oz is null), "EPA/CAMD", Heat input source


heat_and_NOx_to_add <- #  creating intermediate df for updating Unit File
  `CAMD ozone season reporters - heat input and emissions to update 5` %>%
  mutate(`Annual Heat Input` = if_else(is.na(`Heat Input non-oz`), `Oz Seas Heat Input`, `Heat Input non-oz` + `Oz Seas Heat Input`),
         `Heat Input Source` = if_else(is.na(`Heat Input non-oz`), "EPA/CAMD", `Heat input source`),
         `Annual NOx Mass` = if_else(is.na(NOx), `Oz Seas NOx Mass`, `NOx` + `Oz Seas NOx Mass`),
         `NOx Source` = if_else(is.na(NOx),
                                "EPA/CAMD",
                                `NOx source`)) %>%
  select(`ORIS Code`,
         "UNIT ID",
         `Annual Heat Input`,
         `Heat Input Source`,
         `Annual NOx Mass`,
         `NOx Source`) %>% 
  mutate(update = TRUE)


## updating heat and NOx values
`Unit File 5` <- # 2u053- 54
  `Unit File 4` %>% 
  left_join(heat_and_NOx_to_add,
            by = c("ORIS Code", 
                   "UNIT ID")) %>% 
  mutate( `Annual Heat Input` = if_else(is.na(update), `Annual Heat Input.x`, `Annual Heat Input.y`), # .y values represent estimated values from heat_andNOx_to_add
          `Heat Input Source` = if_else(is.na(update), `Heat Input Source.x`, `Heat Input Source.y`),
          `Annual NOx Mass` = if_else(is.na(update), `Annual NOx Mass.x`, `Annual NOx Mass.y`),
          `NOx Source` = if_else(is.na(update), `NOx Source.x`, `NOx Source.y`)) %>% 
  select(any_of(unit_columns)) # removing unnecessary columns



# Add in EIA boilers (2u056a - 2u067b) ----------

eia_923_boilers <- 
  read_excel("data/raw_data/EIA-923/2021/EIA923_Schedules_2_3_4_5_M_12_2021_Final.xlsx", 
             sheet = "Page 3 Boiler Fuel Data", skip = 5) %>% 
  rename_with( ~str_replace(.,"\r\n","")) %>%  # removing formatting characters 
  mutate(across(starts_with(c("Quantity","MMbtu", "Sulfur", "Ash")), ~ as.numeric(.x)),
         across(starts_with(c("Quantity","MMbtu", "Sulfur", "Ash")), ~ if_else(is.na(.x), 0, .x))) # Not sure this is good idea

eia_923_boilers <- 
  eia_923_boilers %>% 
  mutate(across(starts_with(c("Quantity","MMbtu", "Sulfur", "Ash")), ~ as.numeric(.x)),
         across(starts_with(c("Quantity","MMbtu", "Sulfur", "Ash")), ~ if_else(is.na(.x), 0, .x))) # Not sure this is good idea


`EIA boilers grouped` <- # 2u056a
  eia_923_boilers %>% 
  group_by(`Plant Id`, 
           `Boiler Id`,
           `Plant Name`,
           `Plant State`) %>% 
  summarize(
    `FirstOfPrime Mover` = first(`ReportedPrime Mover`), ## ?? *? It may make more sense to figure out main prime mover, or something. This results in arbitrary PM attached
    HeatInput1 = sum(
      `Quantity Of Fuel ConsumedJanuary` * `MMbtu Per UnitJanuary` + 
        `Quantity Of Fuel ConsumedFebruary` * `MMbtu Per UnitFebruary` +
        `Quantity Of Fuel ConsumedMarch` * `MMbtu Per UnitMarch` +
        `Quantity Of Fuel ConsumedApril` * `MMbtu Per UnitApril` +
        `Quantity Of Fuel ConsumedMay` * `MMbtu Per UnitMay` +
        `Quantity Of Fuel ConsumedJune` * `MMbtu Per UnitJune` 
    ),
    HeatInput2 = sum(
      `Quantity Of Fuel ConsumedJuly` * `MMbtu Per UnitJuly` +
        `Quantity Of Fuel ConsumedAugust` * `MMbtu Per UnitAugust` +
        `Quantity Of Fuel ConsumedSeptember` * `MMbtu Per UnitSeptember` +
        `Quantity Of Fuel ConsumedOctober` * `MMbtu Per UnitOctober` +
        `Quantity Of Fuel ConsumedNovember` * `MMbtu Per UnitNovember` +
        `Quantity Of Fuel ConsumedDecember` * `MMbtu Per UnitDecember` 
    ),
    HeatInput = HeatInput1 + HeatInput2,
    HeatInputOZ = sum(
      `Quantity Of Fuel ConsumedMay` * `MMbtu Per UnitMay` +
        `Quantity Of Fuel ConsumedJune` * `MMbtu Per UnitJune` +
        `Quantity Of Fuel ConsumedJuly` * `MMbtu Per UnitJuly` +
        `Quantity Of Fuel ConsumedAugust` * `MMbtu Per UnitAugust` +
        `Quantity Of Fuel ConsumedSeptember` * `MMbtu Per UnitSeptember` 
    )
  )    


`Group CAMD by ORIS` <- # 2u056b
  camd_r %>% 
  distinct(`ORIS Code`, 
           `Facility Name`,
           State)


`EIA boilers grouped by fuel type` <- # 2u057
  eia_923_boilers %>% 
  group_by(`Plant Id`, 
           `Boiler Id`,
           `Plant Name`,
           `Plant State`,
           `ReportedFuel Type Code`) %>% 
  summarize(
    `FirstOfPrime Mover` = first(`ReportedPrime Mover`), ## ?***not sure if this is correct, because it removes primes movers** Ask Marissa about this in future
    HeatInput1 = sum(
      `Quantity Of Fuel ConsumedJanuary` * `MMbtu Per UnitJanuary` + 
        `Quantity Of Fuel ConsumedFebruary` * `MMbtu Per UnitFebruary` +
        `Quantity Of Fuel ConsumedMarch` * `MMbtu Per UnitMarch` +
        `Quantity Of Fuel ConsumedApril` * `MMbtu Per UnitApril` +
        `Quantity Of Fuel ConsumedMay` * `MMbtu Per UnitMay` +
        `Quantity Of Fuel ConsumedJune` * `MMbtu Per UnitJune` 
    ),
    HeatInput2 = sum(
      `Quantity Of Fuel ConsumedJuly` * `MMbtu Per UnitJuly` +
        `Quantity Of Fuel ConsumedAugust` * `MMbtu Per UnitAugust` +
        `Quantity Of Fuel ConsumedSeptember` * `MMbtu Per UnitSeptember` +
        `Quantity Of Fuel ConsumedOctober` * `MMbtu Per UnitOctober` +
        `Quantity Of Fuel ConsumedNovember` * `MMbtu Per UnitNovember` +
        `Quantity Of Fuel ConsumedDecember` * `MMbtu Per UnitDecember` 
    ),
    HeatInput = HeatInput1 + HeatInput2,
    HeatInputOZ = sum(
      `Quantity Of Fuel ConsumedMay` * `MMbtu Per UnitMay` +
        `Quantity Of Fuel ConsumedJune` * `MMbtu Per UnitJune` +
        `Quantity Of Fuel ConsumedJuly` * `MMbtu Per UnitJuly` +
        `Quantity Of Fuel ConsumedAugust` * `MMbtu Per UnitAugust` +
        `Quantity Of Fuel ConsumedSeptember` * `MMbtu Per UnitSeptember` 
    )
  )   


# ? `C02 CH4 N20 EF table` -- not sure where this comes from

`C02 CH4 N20 EF table` <- 
  read_excel("data/raw_data/static_tables/CO2 CH4 N2O EF table.xlsx")

`923 boiler file CO2 emissions by fuel type` <- # 2u058
  `C02 CH4 N20 EF table` %>% 
  inner_join(`EIA boilers grouped by fuel type`,
             by = c("EIA Fuel Code" = "ReportedFuel Type Code")) %>% 
  mutate(`CO2 emissions` = HeatInput * `CO2 EF`) %>% 
  select(`Plant Id`,
         `Boiler Id`,
         `Plant Name`,
         `Plant State`,
         `FirstOfPrime Mover`, ## ??again, not sure if this is correct
         `Fuel Type`,
         `CO2 emissions`) %>% 
  mutate(`FirstOfPrime Mover` = if_else(`FirstOfPrime Mover` == "CT" & `Plant Id` == "50973", "CA", `FirstOfPrime Mover`)) %>% ## 2u058b
  arrange(`Plant Id`, `Boiler Id`) 

`boiler file CO2 emissions summed to boiler level` <- # 2u059
  `EIA boilers grouped` %>% 
  inner_join(`923 boiler file CO2 emissions by fuel type`,
             by = c("Plant Id", 
                    "Boiler Id", 
                    "Plant Name",
                    "Plant State",
                    "FirstOfPrime Mover")) %>% 
  group_by(`Plant Id`,
           `Boiler Id`,
           `Plant Name`,
           `Plant State`,
           `FirstOfPrime Mover`,
           `HeatInput`,
           `HeatInputOZ`) %>% 
  summarize(SumOfCO2emissions = sum(`CO2 emissions`)) %>% 
  mutate(id = paste0(`Plant Id`, "_", `Boiler Id`)) # creating unique ID for filtering later on

`EIA-860 Operable` <-
  read_excel("data/raw_data/EIA-860/2021/3_1_Generator_Y2021.xlsx",
             sheet = "Operable", skip = 1)

`EIA-860 Retired and Canceled` <-
  read_excel("data/raw_data/EIA-860/2021/3_1_Generator_Y2021.xlsx",
             sheet = "Retired and Canceled", skip = 1)

`EIA-860 Combined` <-
  `EIA-860 Operable` %>%
  bind_rows(`EIA-860 Retired and Canceled` %>%
              filter(`Retirement Year` == 2021)) %>%
  mutate(id = paste0(`Plant Code`,"_",`Generator ID`)) # creating unique ID for subsequent filtering

`EIA-860 Boiler Generator` <- 
  read_excel("data/raw_data/EIA-860/2021/6_1_EnviroAssoc_Y2021.xlsx", 
             sheet = "Boiler Generator", skip = 1)  %>% 
  mutate(id_boiler = paste0(`Plant Code`,"_",`Boiler ID`),
         id_gen = paste0(`Plant Code`,"_",`Generator ID`))

`EIA boilers matched to generators` <- #2u060 #? # not sure if I need this step
  `boiler file CO2 emissions summed to boiler level` %>% 
  left_join(`EIA-860 Boiler Generator`,
            by = c("Plant Id" = "Plant Code", "Boiler Id" = "Boiler ID")) %>% 
  remove_suffix() %>% 
  distinct(`Plant Id`,
           `Boiler Id`,
           `Plant Name`,
           `Plant State`,
           `FirstOfPrime Mover`,
           `HeatInput`,
           `HeatInputOZ`,
           `SumOfCO2emissions`) %>% 
  mutate(id = paste0(`Plant Id`,"_",`Boiler Id`))

## 2u061 ? **This is the query that would always end up not actually changing anything. Skipping for now and flagging to ask about in the future.  

# I think this ends up being same as results of 2u060-2u061


## Adding EIA boilers to unit file #2u061b


`EIA boilers to add to Unit File` <- # 2u061 & 2u062a ***there are NA values for Unit IDs in the 923 boiler file. Need to make these explicit
  `boiler file CO2 emissions summed to boiler level` %>% # 923 boilers, all units
  ungroup() %>% 
  filter(!`Plant Id` %in% camd_r$`ORIS Code`, # removing units from plants in CAMD
         (id %in% `EIA-860 Boiler Generator`$id_boiler) | (id %in% `EIA-860 Combined`$id)) %>% # plant/units matching in 860 Boiler Generator or EIA-860 Combined
  select("ORIS Code" = `Plant Id`,
         "UNIT ID" = `Boiler Id`,
         "Facility Name" = `Plant Name`,
         "State" = `Plant State`,
         "PrimeMover" = `FirstOfPrime Mover`,
         "Annual Heat Input" = `HeatInput`,
         "Oz Seas Heat Input" = `HeatInputOZ`,
         "Annual CO2 Mass" = `SumOfCO2emissions`,
         id) %>% 
  mutate(`Heat Input Source` = "EIA Unit-Level Data",
         `Heat Input OZ Source` = "EIA Unit-Level Data",
         `CO2 Source` = "Estimated using emissions factor") 



## Determining primary fuel type for 923 boilers based on type of unit with max fuel consumption 
## **? this is quite different than in access, so should spot check correct primary fuels.
##**? Double check because I think there may be some duplicates added

`Updated 923 fuel types` <- 
  `EIA boilers grouped by fuel type` %>% 
  group_by(`Plant State`, `Plant Name`, `Plant Id`, `Boiler Id`) %>%
  slice_max(HeatInput, # identifying row with highest heat input
            n = 1, 
            with_ties = FALSE) %>% # setting this to match access, but need to figure out if this is what is wanted
  ungroup() %>% 
  select("ORIS Code" = `Plant Id`,
         "UNIT ID" = `Boiler Id`,
         "PrimeMover" = `FirstOfPrime Mover`,
         `Fuel Type (Primary)` = `ReportedFuel Type Code`) 

`EIA boilers to add to Unit File 2` <- # Adding primary fuel type to 923 boilers to add
  `EIA boilers to add to Unit File` %>% 
  left_join(`Updated 923 fuel types`,
            by = c("ORIS Code", "UNIT ID", "PrimeMover")) 

## ** Units with missing heat input values seem to be removed here** 12/13


# Binding EIA boilers to unit file
`Unit File 6` <-
  `Unit File 5` %>%
  bind_rows(`EIA boilers to add to Unit File 2` %>% 
              select(-id))



# Add in EIA generators (2u067 - 2u072) -----

#*?? Something is going on here with the 860-combined table but not mentioned in instructions.
# There is a list of generators to be added, but none of them end up in final Unit File. Skipping for 
# now until we can get some clarity on what's happening in 2u067, 2u067b, 2u068, 2u069, and 2u069b

## 2u067b is adding a ton of generators



unit6_ids <- # creating list of unique ids already in unit file to remove from added generators
  `Unit File 6` %>% 
  mutate(id = paste0(`ORIS Code`, "_", `UNIT ID`)) %>% 
  pull()


gen860_ids <- # creating unique ID based on Generator to identify units that come from 860-combined
  `EIA-860 Boiler Generator` %>%
  mutate(id = paste0(`Plant Code`,"_",`Generator ID`),
         match = if_else(`Boiler ID` == `Generator ID`, 1, 0)) %>% # identyfing cases where Boiler and Gen ids match to not include
  filter(match == 0) %>% 
  pull(id)



# Finding units in 860 Boiler Generator File that were already added and finding their associated Gen Ids. Resutling plant/IDs will 
# be removed from EIA-860 Combined before adding to Unit File. 

`EIA Boilers matched to Generators 2` <- # This number of rows is too high
  `EIA-860 Boiler Generator` %>% 
  filter(id_boiler %in% `EIA boilers to add to Unit File 2`$id)



`EIA generators to add to Unit File` <- #2u068 # 
  `EIA-860 Combined` %>% # 24,872 units
  filter(!`Plant Code` %in% camd_r$`ORIS Code`) %>%  # removing plants from CAMD. 20,007 units
  filter(!id %in% unit6_ids) %>% # removing plant/unit combos already added to unit file. 19,737
  filter(!id %in% `EIA Boilers matched to Generators 2`$id_gen) %>%  # 19,110 removing units already added based on associated generator IDs. About 100 off?
  select(State,
         "Facility Name" = `Plant Name`,
         "ORIS Code" = `Plant Code`,
         "UNIT ID" = `Generator ID`,
         "PrimeMover" = `Prime Mover`,
         "Fuel Type (Primary)" = `Energy Source 1`,
         "Op Status" = Status,
         id)


`Unit File 7` <- # 2u068 adding generators. 24642 units total at this point.
  `Unit File 6` %>% 
  bind_rows(`EIA generators to add to Unit File` %>% 
              select(-id))



## Add in renewable EIA generators -----
EIA_renewable_generators <- #2u072
  `EIA-860 Combined` %>% 
  filter(`Plant Code` %in% camd_r$`ORIS Code`,
         `Energy Source 1` %in% c("SUN", "WAT", "WND")) %>% 
  select(State,
         `Plant Name`,
         "ORIS Code" = `Plant Code`,
         "UNIT ID" = `Generator ID`,
         "PrimeMover" = `Prime Mover`,
         "Fuel Type (Primary)" = `Energy Source 1`,
         "Op Status" = Status)

# Adding renewable generators to unit file
`Unit File 8` <- #2u072
  `Unit File 7` %>%
  bind_rows(EIA_renewable_generators)

# Add in additional biomass units ------

biomass_units_toadd <- 
  read_excel("data/raw_data/static_tables/Biomass units to add to unit file.xlsx") %>%
  select(State,
         `Facility Name` = `Plant Name`,
         `ORIS Code` = `Plant Code`,
         `UNIT ID` = `Unit ID`,
         `PrimeMover` = `Prime Mover`,
         `Fuel Type (Primary)` = `Fuel Type`)


`Unit File 9`<-
  `Unit File 8` %>%
  bind_rows(biomass_units_toadd) ## adding additioanl biomass units to Unit File


# Update fuel types where mismatch between EIA sources (2u073b)

`Unit file generator fuel types to change` <-
  eia_923_gen_fuel %>% 
  group_by(ORISPL) %>% 
  slice_max(n = 1, TOTFCONS) %>% # identifying primary fuel
  filter(TOTFCONS !=0, # where max fuel isn't 0
         !is.na(TOTFCONS)) %>% 
  select(ORISPL, TOTFCONS, FUELG1 ) %>% 
  inner_join(
    (`EIA-860 Combined` %>% # finding primary fuels in 860
       select(`Plant Code`,
              `Generator ID`,
              `Energy Source 1`) %>%
       group_by(`Plant Code`) %>% 
       mutate(n_gens = n()) %>% 
       filter(n_gens == 1)), # only for plants with 1 generator
    by = c("ORISPL" = "Plant Code")) %>% 
  mutate(diffs = if_else(FUELG1 != `Energy Source 1`, TRUE, FALSE)) %>%  # identfying discrepencies
  filter(diffs == TRUE) %>% 
  mutate(update_fuel = TRUE) %>% 
  select(`ORIS Code` = ORISPL, 
         `UNIT ID` = `Generator ID`,
         new_fueltype = `FUELG1`,
         update_fuel)


`Unit File 10` <- # updating Unit File | updating fuel types where mismatches between EIA
  `Unit File 9` %>% 
  left_join(`Unit file generator fuel types to change`,
            by = c("ORIS Code", "UNIT ID")) %>%
  mutate(`Fuel Type (Primary)` = if_else(is.na(update_fuel), `Fuel Type (Primary)`, new_fueltype)) %>% 
  select(-c(new_fueltype, update_fuel))


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

