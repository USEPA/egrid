
# load required libraries --------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(here)


# Load necessary data ----------

## eia ------------

eia_860 <- read_rds("data/clean_data/eia_860_clean.RDS")
eia_923 <- read_rds("data/clean_data/eia_923_clean.RDS")
# note: eia-923 generation and fuel = generation_and_fuel_combined

## lower-level eGRID files ------------

generator_file <- read_rds("data/outputs/generator_file.RDS")
#unit_file <- read_rds("data/outputs/unit_file.RDS") # This is where Unit file will be sourced once completed

clean_names <- # renaming column names to match naming conventions used in other files. This can be removed once new unit file is completed
  c("seq" = "SEQUNT",
    "year" = "YEAR",
    "plant_state" = "PSTATABB",
    "plant_name" = "PNAME",
    "plant_id" = "ORISPL",
    "unit_id" = "UNITID",
    "prime_mover" = "PRMVR",
    "operating_status" = "UNTOPST",
    "camd_flag" = "CAMDFLAG",
    "program_code" = "PRGCODE",
    "botfirty" = "BOTFIRTY",
    "n_generators" = "NUMGEN",
    "primary_fuel_type" = "FUELU1",
    "operating_hours" = "HRSOP",
    "heat_input" = "HTIAN",
    "heat_input_oz" = "HTIOZ",
    "nox_mass" = "NOXAN",
    "nox_oz" = "NOXOZ",
    "so2_mass" = "SO2AN",
    "co2_mass" = "CO2AN",
    "hg_mass" = "HGAN",
    "heat_input_source" = "HTIANSRC",
    "heat_input_oz_source" = "HTIOZSRC",
    "nox_source" = "NOXANSRC",
    "nox_oz_source" = "NOXOZSRC",
    "so2_source" = "SO2SRC",
    "co2_source" = "CO2SRC",
    "hg_source" = "HGSRC",
    "so2_controls" = "SO2CTLDV",
    "nox_controls" = "NOXCTLDV",
    "hg_controls" = "HGCTLDV",
    "year_online" = "UNTYRONL")


unit_file <- # using unit file from Access production for now
  read_excel(here("data/raw_data/eGRID_2021.xlsx"), 
             sheet = "UNT21",
             skip = 1) %>% 
  rename(all_of(clean_names)) # rename columns based on named vector above

library(tidyverse)

# 0. create a file to string concatenate unique values that are not NA ----------

paste_concat <- function(l, number = FALSE){
  l <- l[!is.na(l)]
  l <- l[l!="NA" & l!=""]
  l[order(l)]
  txt <- paste0(unique(l), collapse = ", ")
  
  if(number){return(as.numeric(txt)) # convert to numeric if it can
    }else{return(txt)}
}


# 1. aggregate unit file to plant level ------------------------
      # SQL steps group by State, Facility Name, and ORIS code
      # for this reason I included plant name and state in summarise
unit <- unit_file %>% group_by(plant_id) %>%
  summarise(plant_name = paste_concat(plant_name),
            plant_state = paste_concat(plant_state),
            year = paste_concat(year),
            num_units = length(unique(unit_id)), # count
            num_units_op = sum(unique(data.frame(unit_id, operating_status))$operating_status == "OP",na.rm = TRUE), # count operational
            unadj_heat_input = sum(heat_input, na.rm = TRUE), # MMBtu
            unadj_heat_input_source = paste_concat(heat_input_source),
            unadj_heat_input_oz = sum(heat_input_oz, na.rm = TRUE), # MMBtu 
            unadj_heat_input_oz_source = paste_concat(heat_input_oz_source),
            unadj_nox_mass = sum(nox_mass, na.rm = TRUE), # tons
            unadj_nox_source = paste_concat(nox_source),
            unadj_nox_oz = sum(nox_oz, na.rm = TRUE),# tons
            unadj_nox_oz_source = paste_concat(nox_oz_source),
            unadj_so2_mass = sum(so2_mass, na.rm = TRUE), # tons
            unadj_so2_source = paste_concat(so2_source),
            unadj_co2_mass = sum(co2_mass, na.rm = TRUE),# tons
            unadj_co2_source = paste_concat(co2_source)) %>% 
  mutate(plant_id = as.numeric(plant_id)) 

# 2. aggregate gen file to plant level----------------------------

gen <- generator_file %>% group_by(plant_id) %>%
  summarise(plant_name = paste_concat(plant_name),
            plant_state = paste_concat(plant_state),
            year = paste_concat(year),
            num_gen = length(unique(generator_id)), # count
            num_gen_op = sum(unique(data.frame(generator_id, status))$status == "OP",na.rm = TRUE), # count operational
            nameplate_capacity = sum(nameplate_capacity, na.rm = TRUE), # units?
            generation_ann = sum(generation_ann, na.rm = TRUE), # units?
            generation_oz = sum(generation_oz, na.rm = TRUE), # units?
            fuel_code = paste_concat(fuel_code))%>% 
  mutate(plant_id = as.numeric(plant_id)) 

# 3. Pull in info from the EIA-860 Plant file  -------------------------------
      # joins by plant_id, plant_state and plant_name
eia_860_use <- eia_860$plant %>% select(plant_id, 
                                        county, latitude, longitude,
                                        utility_name, utility_id,
                                   transmission_or_distribution_system_owner, 
                                   transmission_or_distribution_system_owner_id,
                                   balancing_authority_name,
                                   balancing_authority_code,
                                   sector_name, nerc_region) %>%
  rename(system_owner = transmission_or_distribution_system_owner ,
         system_owner_id = transmission_or_distribution_system_owner_id,
         sector = sector_name,
         ba_name = balancing_authority_name,
         ba_id = balancing_authority_code,
         nerc = nerc_region,
         lat = latitude, lon = longitude) %>% unique() %>%
  mutate(plant_id = as.numeric(plant_id))

gen_860 <-gen %>% left_join(eia_860_use)

if(any( grepl('not in file|Not in file', gen_860$county))){
  gen_860 <- gen_860 %>% mutate(county = ifelse(grepl('not in file|Not in file', county),NA,county ))
}

# 4.	Combustion heat input ---------------------------------------

generator_file <- generator_file %>% 
  mutate(plant_id = as.numeric(plant_id)) %>% left_join(unit)

heat_input_table = generator_file %>% group_by(fuel_code, prime_mover, plant_id) %>% #why by prime mover?
    summarise(unadj_heat_input = sum(unadj_heat_input, na.rm = TRUE),
              unadj_heat_input_oz = sum(unadj_heat_input_oz, na.rm = TRUE))

combustion_fuels <- c("AB","ANT","BFG","BIT","BLQ","COG","DFO","JF","KER","LFG","LIG",
                      "MSB","MSN","MSW","NG","OBG","OBL","OBS","OG","PC","PG","RC","RFO",
                      "SGC","SGP","SLW","SUB","TDF","WC","WDL","WDS","WO")
# going with list in Combustion Crosswalk
# values were hardcoded in SQL for this which additionally contained: "H", "OTH", "PRG","PUR", "MWH","WD"
# they were missing: "ANT", "MSB", "MSN", "SGP"

        # filter to combustable fuels 
heat_input_table <- heat_input_table %>% 
  filter(fuel_code %in% combustion_fuels ) %>%
  ungroup %>% group_by(plant_id) %>% summarise(unadj_combust_heat_input = sum(unadj_heat_input, na.rm = TRUE), 
                                               unadj_combust_heat_input_oz = sum(unadj_heat_input_oz, na.rm = TRUE))

        # join with aggregated generator file
gen_860 <- gen_860 %>% left_join(heat_input_table)  


# 5.	Update capacity factor  -----------------------------------------------
gen <- gen %>% mutate(capfac = ifelse(generation_ann / nameplate_capacity *8760 < 0, 0, 
                                      generation_ann / nameplate_capacity *8760))

# 6.	Calculate CH4 emissions & 7.	Calculate N2O emissions  ----------------------------------------

      # load static table with emissions factors
emissions_CH4_N2O <- read_csv("data/static_tables/co2_ch4_n2o_ef.csv") %>% filter(!is.na(eia_fuel_code)) %>% 
  select(eia_fuel_code, ch4_ef, n2o_ef)
    
#if(any( (!gen$fuel_code %in% emissions_CH4_N2O$eia_fuel_code )& !is.na(gen$fuel_code))) { 
#  print(paste0(unique(gen_860$fuel_code[which(!gen_860$fuel_code %in% emissions_CH4_N2O$eia_fuel_code& !is.na(gen$fuel_code))]), " is not in emissions data!"))
#}
    # join with eia to calculate at plant level
    
eia_CH4_N2O <- eia_923$generation_and_fuel_combined %>% filter(prime_mover != "FC")%>% select(plant_id, total_fuel_consumption_mmbtu, fuel_type) %>%
  left_join(emissions_CH4_N2O, by = c("fuel_type" = "eia_fuel_code")) %>%
  mutate( unadj_ch4_mass = ch4_ef * total_fuel_consumption_mmbtu,
          unadj_n2o_mass = n2o_ef * total_fuel_consumption_mmbtu,
          plant_id = as.numeric(plant_id)) %>%
  group_by(plant_id) %>%
  summarise(unadj_ch4_mass = sum(unadj_ch4_mass, na.rm = TRUE),
            unadj_n2o_mass = sum(unadj_n2o_mass, na.rm = TRUE))
  

plant_file <- gen_860 %>% full_join(unit)
plant_file <- plant_file %>% left_join(eia_CH4_N2O) 


# 8.	Add in FIPS codes  ----------------
A6_fips <- read_excel(here("data", "static_tables", "Appendix 6 State and County FIPS Codes.xlsx"))
A6_newnames <- read_excel(here("data", "static_tables", "FIPS Appendix 6 crosswalk - name updates.xlsx"))
# this contains 3 counties with no EIA name. These do not appear in A6_fips

#A6_alaska <- read_excel(here("data", "static_tables", "FIPS Alaska Crosswalk.xlsx"))
# not necessary. A6_fips is already using these names

A6_fips <- A6_fips %>% left_join(A6_newnames, by = c( "Postal State Code" = "State",
                                                       "County Name" = "Appendix 6 County Name")) %>%
  mutate("County Name" = ifelse(is.na(`EIA County Name`), `County Name`, `EIA County Name`)) %>%
  select("Postal State Code", "County Name", "FIPS State Code", "FIPS County Code") %>% 
  rename("State" ="Postal State Code", "County" = "County Name", 
         "FIPS_state" = "FIPS State Code", "FIPS_county" = "FIPS County Code")

plant_file <- plant_file %>% left_join(A6_fips, by = c( "plant_state" = "State" ,
                                                        "county" = "County"))

rm(A6_fips, A6_newnames)


# 9.	Coal flag -----------------

OTH_OG_recode <- read_excel(here("data", "static_tables", "OG_OTH units to change fuel type.xlsx"))  %>% 
  select("ORIS Code", "Fuel Type (Primary)", "Fuel Code") %>%
  rename("plant_id" = "ORIS Code", "fuel_type" = "Fuel Type (Primary)", "new_code" = "Fuel Code") %>%
  mutate(plant_id = as.numeric(plant_id))

coal_fuels <- c("ANT","BIT", "COG", "LIG", "RC", "SGC", "SUB", "WC")

coal_plants <- eia_923$generation_and_fuel_data  %>% group_by(plant_id, fuel_type)  %>% 
  summarise(total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) %>%
  mutate(plant_id = as.numeric(plant_id)) %>% full_join(OTH_OG_recode) %>%
  mutate(coal_flag = ifelse( (fuel_type %in% coal_fuels|new_code %in% coal_fuels) & 
                               total_fuel_consumption_mmbtu > 0, 1, 0))
  
# 10.	Combustion flag --------------------
EIA_923_combust <- eia_923$generation_and_fuel_data  %>%
  mutate(combustion = ifelse(fuel_type %in% combustion_fuels, 1,0)) %>% group_by(plant_id) %>%
  summarise(SumofCombustion = sum(combustion, na.rm = TRUE),
            CountofCombustion = n()) %>%
  mutate(combust_flag = case_when(SumofCombustion == 0 ~ 0,
                             SumofCombustion != CountofCombustion ~ 0.5,
                             TRUE ~ 1)) %>% select(-SumofCombustion, -CountofCombustion)
plant_file <- plant_file %>% left_join(EIA_923_combust)





## This can't be run until we add code to create adjusted variables from the unadj_ columns

# 13.	Plant primary fuel & 14.	Plant primary fuel category  -----------------------
unit_by_plant_by_fuel <- unit_file %>% group_by(plant_id, primary_fuel_type) %>% summarise(heat_input = sum(heat_input)) %>%
  mutate(heat_input = ifelse(is.na(heat_input),0,heat_input)) # replace NA with 0 so rows with only NA do not get filtered out
         
unit_primary_fuel <- unit_by_plant_by_fuel %>% ungroup %>% group_by(plant_id) %>% filter(heat_input == max(heat_input)  ) %>% select(-heat_input) %>% # filter to primary fuel "ANT","BIT", "COG", "LIG", "RC", "SGC", "SUB", "WC"
  mutate(primary_fuel_category = case_when(primary_fuel_type %in% coal_fuels ~ "COAL", # DD didn't include "ANT"
                                           primary_fuel_type %in% c("DFO", "JF", "KER", "PC", "RFO", "WO") ~ "OIL",
                                           primary_fuel_type %in% c("NG", "PG") ~ "GAS",
                                           primary_fuel_type %in% c("BFG", "OG", "TDF") ~ "OFSL", # other fossil fuel
                                           primary_fuel_type %in% c("NUC") ~ "NUCLEAR",
                                           primary_fuel_type %in% c("WAT") ~ "HYDRO",
                                           primary_fuel_type %in% c("SUN") ~ "SOLAR",
                                           primary_fuel_type %in% c("WND") ~ "WIND",
                                           primary_fuel_type %in% c("GEO") ~ "GEOTHERMAL",
                                           primary_fuel_type %in% c("H", "MWH", "OTH", "PRG", "PUR", "WH") ~ "OTHF", # derived from waste heat/hydrogen/purchased/unknown
                                           primary_fuel_type %in% c("AB", "BLQ", "LFG", "MSW", "OBG", "OBL", "OBS", "SLW", "WDL", "WDS") ~ "BIOMASS")) 
plant_file <- plant %>% full_join(unit_primary_fuel)
rm(unit_by_plant_by_fuel, unit_primary_fuel)
# 15.	Create Generation by Fuel table and update Plant file & 16.	Resource mix a.	generation by fuel type b.	% resource mix ------------------

ann_gen_by_fuel = generator_file %>% group_by(fuel_code, plant_id) %>% #why by prime mover?
  summarise(ann_gen = sum(generation_ann, na.rm = TRUE),
            ann_gen_oz = sum(generation_oz, na.rm = TRUE)) %>% ungroup %>% group_by(plant_id) %>%
  summarise(ann_gen = sum(ann_gen, na.rm = TRUE),
            ann_gen_coal = sum(ann_gen[which(fuel_code %in% c("BIT", "COG", "LIG", "RC", "SGC","SC", "SUB", "WC"))], na.rm = TRUE),
            ann_gen_oil = sum(ann_gen[which(fuel_code %in% c("DFO", "JF", "KER","OO", "PC", "RFO", "WO"))], na.rm = TRUE),
            ann_gen_gas = sum(ann_gen[which(fuel_code %in% c("NG", "PG"))], na.rm = TRUE),
            ann_gen_nuclear = sum(ann_gen[which(fuel_code %in% c("NUC"))], na.rm = TRUE),
            ann_gen_hydro = sum(ann_gen[which(fuel_code %in% c("WAT"))], na.rm = TRUE),
            ann_gen_biomass = sum(ann_gen[which(fuel_code %in% c("AB", "BLQ", "DG", "LFG", "ME", "MSW", "OBG", "OBL", "OBS","PP", "SLW", "WDL", "WDS"))], na.rm = TRUE),
            ann_gen_wind = sum(ann_gen[which(fuel_code %in% c("WND"))], na.rm = TRUE),
            ann_gen_solar = sum(ann_gen[which(fuel_code %in% c("SUN"))], na.rm = TRUE),
            ann_gen_geothermal = sum(ann_gen[which(fuel_code %in% c("GEO"))], na.rm = TRUE),
            ann_gen_other_ff = sum(ann_gen[which(fuel_code %in% c("BFG", "HY", "LB", "MH", "MSF", "OG", "TDF"))], na.rm = TRUE),
            ann_gen_other = sum(ann_gen[which(fuel_code %in% c("H", "MWH", "OTH", "PRG", "PUR", "WH"))], na.rm = TRUE),
            
  ) %>% mutate(ann_gen_non_renew = ann_gen_coal + ann_gen_oil + ann_gen_gas + ann_gen_other_ff + ann_gen_nuclear + ann_gen_other,
               ann_gen_renew = ann_gen_biomass + ann_gen_wind + ann_gen_solar + ann_gen_geothermal + ann_gen_hydro,
               ann_gen_renew_nonhydro = ann_gen_biomass + ann_gen_wind + ann_gen_solar + ann_gen_geothermal ,
               ann_gen_combust = ann_gen_coal + ann_gen_oil + ann_gen_gas + ann_gen_other_ff + ann_gen_biomass + ann_gen_other,
               ann_gen_non_combust = ann_gen_nuclear + ann_gen_wind + ann_gen_solar + ann_gen_geothermal + ann_gen_hydro
  ) %>% mutate(perc_ann_gen_coal = 100 * ann_gen_coal / ann_gen,
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
               perc_ann_gen_renew_non_hydro = 100 * ann_gen_renew_nonhydro / ann_gen,
               perc_ann_gen_combust = 100 * ann_gen_combust / ann_gen,
               perc_ann_gen_non_combust = 100 * ann_gen_combust / ann_gen,)


plant_file <- plant_file %>% full_join(ann_gen_by_fuel)



# 29. Calculate CO2 equivalent ----------------------------------------

plant_file = plant_file %>% mutate(co2_equivalent = co2_mass + (25*ch4_mass/2000) + (298*n2o_mass/2000))  
    

# 31.	Calculate emissions rates  --------------------
#   a.	Combustion output emissions rates ----------------------------------------

plant_file = plant_file %>% mutate(nox_combust_out_emission_rate = 2000 * nox_mass / ann_gen_combust,
                                   nox_combust_out_emission_rate_oz = 2000 * nox_oz / ( ann_gen_combust * (generation_oz/generation_ann)),
                                   so2_combust_out_emission_rate = 2000 * so2_mass / ann_gen_combust,
                                   co2_combust_out_emission_rate = 2000 * co2_mass / ann_gen_combust,
                                   ch4_combust_out_emission_rate = 2000 * ch4_mass / ann_gen_combust,
                                   n2o_combust_out_emission_rate = 2000 * n2o_mass / ann_gen_combust,
                                   co2_combust_equiv_out_emission_rate = 2000 * co2_equivalent / ann_gen_combust)  


#   b.	Output emission rates ----------------------------------------
  
  plant_file = plant_file %>% mutate(nox_in_emission_rate = 2000 * nox_mass / heat_input,
                                     nox_in_emission_rate_oz = 2000 * nox_oz / heat_input_oz,
                                     so2_in_emission_rate = 2000 * so2_mass / heat_input,
                                     co2_in_emission_rate = 2000 * co2_mass / heat_input,
                                     ch4_in_emission_rate = 2000 * ch4_mass / heat_input,
                                     n2o_in_emission_rate = 2000 *  n2o_mass / heat_input,
                                     co2_equiv_in_emission_rate = 2000 * co2_equivalent / heat_input)  
#   c.	Output emission rates ----------------------------------------

plant_file = plant_file %>% mutate(nox_out_emission_rate = 2000 * nox_mass / generation_ann,
                                  nox_out_emission_rate_oz = 2000 * nox_oz / generation_oz,
                                  so2_out_emission_rate = 2000 * so2_mass / generation_ann,
                                  co2_out_emission_rate = 2000 * co2_mass / generation_ann,
                                  ch4_out_emission_rate = 2000 * ch4_mass / generation_ann,
                                  n2o_out_emission_rate = 2000 * n2o_mass / generation_ann,
                                  co2_equiv_out_emission_rate = 2000 * co2_equivalent / generation_ann)  

