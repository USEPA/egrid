
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
eia_861 <- read_rds("data/clean_data/eia_861_clean.RDS")
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
      # SQL steps group by State, Facility Name, and ORIS code
      # for this reason I included plant name and state in summarise
unit <- unit_file %>% group_by(plant_id) %>%
  summarise(plant_name = paste_concat(plant_name),
            plant_state = paste_concat(plant_state),
            year = paste_concat(year),
            camd_flag = paste_concat(camd_flag),
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
            unadj_co2_source = paste_concat(co2_source),
            unadj_hg_mass = ifelse("hg_mass" %in% colnames(unit_file), sum(hg_mass, na.rm = TRUE) , NA),
            unadj_hg_source = ifelse("hg_source" %in% colnames(unit_file), paste_concat(hg_source) , NA)) %>% 
  mutate(plant_id = as.numeric(plant_id)) 

# 2. aggregate gen file to plant level----------------------------

gen <- generator_file %>% filter(!is.na(plant_id)) %>% group_by(plant_id) %>%
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

if(any( grepl('not in file|Not in file|NOT IN FILE', gen_860$county))){
  gen_860 <- gen_860 %>% mutate(county = ifelse(grepl('not in file|Not in file|NOT IN FILE', county),NA,county ))
}
rm(eia_860_use, CHP)
# 4.	Combustion heat input ---------------------------------------

heat_input_table = unit_file %>% group_by(primary_fuel_type, prime_mover, plant_id) %>% #why by prime mover?
    summarise(unadj_heat_input = sum(heat_input, na.rm = TRUE),
              unadj_heat_input_oz = sum(heat_input_oz, na.rm = TRUE))

combustion_fuels <- c("AB","ANT", "BFG","BIT","BLQ","COG","DFO","JF","KER","LFG","LIG",
                      "MSB","MSN","MSW","NG","OBG","OBL","OBS","OG","PC","PG","RC","RFO",
                      "SGC","SGP","SLW","SUB","TDF","WC","WDL","WDS","WO") 
# confirmed full list with Marissa

        # filter to combustable fuels 
heat_input_table <- heat_input_table %>% 
  filter(primary_fuel_type %in% combustion_fuels ) %>%
  ungroup %>% group_by(plant_id) %>% summarise(unadj_combust_heat_input = sum(unadj_heat_input, na.rm = TRUE), 
                                               unadj_combust_heat_input_oz = sum(unadj_heat_input_oz, na.rm = TRUE))

        # join with aggregated generator file
unit <- unit %>% left_join(heat_input_table)  

rm(heat_input_table)
# 5.	Update capacity factor  -----------------------------------------------
gen_860 <- gen_860 %>% mutate(capfac = ifelse(generation_ann / (nameplate_capacity *8760) < 0, 0, 
                                      generation_ann / (nameplate_capacity *8760)))

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
  

plant_file <- gen_860 %>% full_join(unit, by = c("plant_id", "plant_state"))  %>%
  rename(plant_name_gen = plant_name.x,
        plant_name_unit = plant_name.y )
# names differ between sources, so only join on id and state.

#View(plant_file %>% filter(plant_name.x != plant_name.y) %>% select(plant_id, plant_state, plant_name.x, plant_name.y))
plant_file <- plant_file %>% left_join(eia_CH4_N2O) 


rm(eia_CH4_N2O, emissions_CH4_N2O)
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

coal_plants <- coal_plants %>% ungroup %>% group_by(plant_id) %>% 
  summarise(coal_flag = max(coal_flag))

plant_file <- plant_file %>% left_join(coal_plants)

rm(coal_plants, OTH_OG_recode)  
# 10.	Combustion flag --------------------
EIA_923_combust <- eia_923$generation_and_fuel_data  %>%
  mutate(combustion = ifelse(fuel_type %in% combustion_fuels, 1,0),
         plant_id = as.numeric(plant_id)) %>% group_by(plant_id) %>%
  summarise(SumofCombustion = sum(combustion, na.rm = TRUE),
            CountofCombustion = n()) %>%
  mutate(combust_flag = case_when(SumofCombustion == 0 ~ 0,
                             SumofCombustion != CountofCombustion ~ 0.5,
                             TRUE ~ 1)) %>% select(-SumofCombustion, -CountofCombustion)
plant_file <- plant_file %>% left_join(EIA_923_combust)
rm(EIA_923_combust)

# 11.	Biomass adjustment -------------
## Biomass Fuels adjustment (CO2) ----------------
EFs_CO2 <- read_csv("data/static_tables/co2_ch4_n2o_ef.csv") %>% filter(!is.na(eia_fuel_code)) %>% 
  select(eia_fuel_code, co2_ef)
# seperate out co2 from biofuels and co2 from other sources
biomass_fuels <- c("AB",  "BLQ",  "LFG", "MSB","MSW", "OBG", "OBL", "OBS", "PP", "SLW", "WDL", "WDS")
# BG","DG", "ME",
# subtract co2 from biofuels from the unadj_co2_mass to  create co2_mass
EIA_923_biomass <- eia_923$generation_and_fuel_combined %>% select(plant_id, total_fuel_consumption_mmbtu, fuel_type) %>%
  left_join(EFs_CO2, by = c("fuel_type" = "eia_fuel_code")) %>% filter(fuel_type %in% biomass_fuels & total_fuel_consumption_mmbtu>0) %>%
  mutate( co2_biomass = co2_ef * total_fuel_consumption_mmbtu,
          plant_id = as.numeric(plant_id)) %>%
  group_by(plant_id) %>%
  summarise(co2_biomass = sum(co2_biomass, na.rm = TRUE),
            total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) %>%
  mutate(biogas_adj_flag = 1)  # add a flag for this adjustment

plant_file <- plant_file %>% left_join(EIA_923_biomass)  %>%
  mutate(co2_mass = unadj_co2_mass - co2_biomass)

# now replace?? co2_mass with the non-biomass ? 
EIA_923_non_biomass <- eia_923$generation_and_fuel_combined %>% select(plant_id, total_fuel_consumption_mmbtu, fuel_type) %>%
  left_join(EFs_CO2, by = c("fuel_type" = "eia_fuel_code")) %>% filter(!fuel_type %in% biomass_fuels& total_fuel_consumption_mmbtu>0) %>%
  mutate( co2_non_biomass = co2_ef * total_fuel_consumption_mmbtu,
          plant_id = as.numeric(plant_id)) %>%
  group_by(plant_id) %>%
  summarise(co2_non_biomass = sum(co2_non_biomass, na.rm = TRUE),
            total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) %>%
  filter(co2_non_biomass > 0) %>% 
  mutate(biomass_adj_flag = 1)

plant_file <- plant_file %>% left_join(EIA_923_non_biomass)  %>%
  mutate(co2_mass = ifelse( !is.na(co2_non_biomass), co2_non_biomass, co2_mass) ) # replace co2_mass with co2_non_biomass
  
rm(EFs_CO2, EIA_923_biomass, EIA_923_non_biomass)

## Landfill Gas adjustment (NOX, CH4, N2O, SO2) -----------------
EFs <- read_csv("data/static_tables/co2_ch4_n2o_ef.csv") %>% 
  select(eia_fuel_code, co2_ef, n2o_ef, ch4_ef)

EIA_923_LFG <- eia_923$generation_and_fuel_combined %>% mutate(plant_id = as.numeric(plant_id)) %>%
  filter(fuel_type == "LFG" & plant_id != 999999) %>% left_join(EFs, by =c("fuel_type" = "eia_fuel_code")) %>%
  group_by(plant_id) %>%
  summarise(heat_input_oz_season = sum(tot_mmbtu_may,tot_mmbtu_june,tot_mmbtu_july,tot_mmbtu_august,tot_mmbtu_september, na.rm = TRUE), 
            total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE),
            ch4_ef = paste_concat(ch4_ef, number = TRUE),
            n2o_ef = paste_concat(n2o_ef, number = TRUE)) %>%
  ungroup %>% 
  rowwise%>% mutate(
    nox_biomass = 0.8 * max(total_fuel_consumption_mmbtu,0),
    nox_bio_oz = 0.8 * max(heat_input_oz_season,0),
    ch4_biomass = ch4_ef* total_fuel_consumption_mmbtu,
    n2o_biomass = n2o_ef* total_fuel_consumption_mmbtu,
    so2_biomass = 0.0115/2000 * max(total_fuel_consumption_mmbtu,0), 
    biomass_adj_flag1 = 1)
  #select(-heat_input_oz_season, -total_fuel_consumption_mmbtu) 
  
plant_file <- plant_file %>% left_join(EIA_923_LFG) %>%
  mutate(nox_mass = unadj_nox_mass - nox_biomass,
         nox_oz = unadj_nox_oz - nox_bio_oz,
         ch4_mass = unadj_ch4_mass - ch4_biomass,
         n2o_mass = unadj_n2o_mass - n2o_biomass,
         so2_mass = unadj_so2_mass - so2_biomass,
         hg_mass = unadj_hg_mass) %>% # no biomass variable for hg 
  mutate(nox_oz =  min(nox_oz,nox_mass),
         biomass_adj_flag = ifelse(biomass_adj_flag == 1 | biomass_adj_flag1 == 1, 1, 0)) %>%
  select(-biomass_adj_flag)

rm(EIA_923_LFG, EFs)

## Landfill Gas adjustment (SO2) - suppressed -----------------
#LFG_EF <- read_csv("data/static_tables/emission_factors.csv") %>% filter(primary_fuel_type=="LFG") %>% 
 # mutate(so2_ef = ifelse(so2_ef_denom == "MCf", so2_ef/500, so2_ef)) %>%
  #select(prime_mover, so2_ef)

#EIA_923_LFG_PM <- eia_923$generation_and_fuel_combined %>% 
#  mutate(plant_id = as.numeric(plant_id),
#         so2_ef = 0.0115/2000) %>%
#  filter(fuel_type == "LFG" & plant_id != 999999) %>% 
#  group_by(plant_id, prime_mover)%>%
#  summarise(heat_input_oz_season = sum(tot_mmbtu_may,tot_mmbtu_june,tot_mmbtu_july,tot_mmbtu_august,tot_mmbtu_september, na.rm = TRUE), 
#            total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE)) %>%
  #left_join(LFG_EF) %>%
#  mutate(so2_mass = so2_ef * total_fuel_consumption_mmbtu)

#plant_file <- plant_file %>% left_join(EIA_923_LFG_PM) %>%
 # mutate(biogas_adj_flag = ifelse(biogas_adj_flag==1 | biogas_adj_flag1==1,1,0)) %>% 
  #select(-biogas_adj_flag1)

## This can't be run until we add code to create adjusted variables from the unadj_ columns

## Set Bio fields equal to unadj_ fields -----------------
plant_file <- plant_file %>% 
  mutate(nox_biomass = min(nox_biomass, unadj_nox_mass),
         nox_bio_oz = min(nox_bio_oz, unadj_nox_oz),
         ch4_biomass = min(ch4_biomass, unadj_ch4_mass),
         n2o_biomass = min(n2o_biomass, unadj_n2o_mass),
         so2_biomass = min(so2_biomass, unadj_so2_mass),
         co2_biomass = min(co2_biomass, unadj_co2_mass))

# 12. BA codes/names --------------
# 

# count ba_ids for each utility to find those with only 1
ult_ba <- plant_file %>% group_by(utility_id) %>% summarise(count = length(paste_concat(ba_id, concat = FALSE)),
                                                             ba_id = paste_concat(ba_id),
                                                             ba_name = paste_concat(ba_name)) %>% filter(count == 1)
# helper functions to look up ba_id and ba_name by utility
getBAname <- function(id, data){
  x <- as.character(data %>% filter(utility_id == id) %>% select(ba_name))
  x <- ifelse(x == "No BA",NA, x)
  return(x)}
getBAid <- function(id, data){as.character(data %>% filter(utility_id == id) %>% select(ba_id))}

# replace ba_id and ba_name for these utilities 
plant_file <- plant_file %>% ungroup %>% rowwise %>% mutate(
  ba_id = ifelse(utility_id %in% ult_ba$utility_id, getBAid(utility_id, data = ult_ba), ba_id),
  ba_name = ifelse(utility_id %in% ult_ba$utility_id, getBAname(utility_id, data = ult_ba), ba_name)
) 

# one ba_name was missing a ba_id
plant_file <- plant_file %>% 
  mutate(ba_id = ifelse(ba_name == "Hawaiian Electric Co Inc", "HECO", ba_id),
         ba_name = ifelse(ba_name %in% c( "No BA","NA" ), NA, ba_name)) %>%
  mutate(ba_id = ifelse(ba_id == "NA", NA, ba_id))

# check all codes have names and all names have codes
stopifnot(nrow(plant_file[which(!is.na(plant_file$ba_id) & is.na(plant_file$ba_name)),])==0)
stopifnot(nrow(plant_file[which(is.na(plant_file$ba_id) & !is.na(plant_file$ba_name)),])==0)
# look up plant_ids with both missing that exist in EIA_861
lu_ba <- plant_file[which(is.na(plant_file$ba_id) & is.na(plant_file$ba_name)),]

# filter to non-NA values and use as guide to recoding
# merge the balancing_authority and sales_ult_cust to get utility_id, ba_name, and ba_id in one df
# filter to utilities with missing ba data that are not NA
eia_861_use <- eia_861$sales_ult_cust %>% full_join(eia_861$balancing_authority %>% 
                                                      mutate(year = as.character(year))) %>% 
  select(utility_number, ba_code, balancing_authority_name) %>% 
  rename(utility_id = utility_number,
         ba_id = ba_code,
         ba_name = balancing_authority_name)  %>% filter(utility_id %in% lu_ba$utility_id & !is.na(ba_name)) 

# update plant_file with the lookup
plant_file <- plant_file %>% ungroup %>% rowwise %>% mutate(
  ba_id = ifelse(utility_id %in% eia_861_use$utility_id, getBAid(utility_id, data = eia_861_use), ba_id),
  ba_name = ifelse(utility_id %in% eia_861_use$utility_id, getBAname(utility_id, data = eia_861_use), ba_name)
) %>%
  mutate( ba_name = case_when(ba_name == "No BA" ~ NA,
                              TRUE ~ ba_name),
          ba_id = case_when(ba_id == "NA" ~ NA,
                              TRUE ~ ba_id))


# check all plants in eia_861_are programmed
stopifnot(all(eia_861_use$utility_id %in% c(1857, 3522)))
# these plants were caught by updates above and no info from EIA 861 gets incorporated
#stopifnot(nrow(eia_861_use) == 0)
rm(ult_ba, lu_ba, eia_861_use)


# 13.	Plant primary fuel -----------------------
unit_fuel_by_plant <- unit_file %>% group_by(plant_id, primary_fuel_type) %>% summarise(heat_input = sum(heat_input)) %>%
  mutate(heat_input = ifelse(is.na(heat_input),0,heat_input),
         plant_id = as.numeric(plant_id)) %>% # replace NA with 0 so rows with only NA do not get filtered out
  ungroup %>% group_by(plant_id) %>% filter(heat_input == max(heat_input)  )
         
gen_fuel_by_plant <- generator_file %>% group_by(plant_id, fuel_code) %>% summarise(nameplate_capacity = sum(nameplate_capacity)) %>%
  mutate(nameplate_capacity = ifelse(is.na(nameplate_capacity),0,nameplate_capacity),
         plant_id = as.numeric(plant_id)) %>% # replace NA with 0 so rows with only NA do not get filtered out
  ungroup %>% group_by(plant_id) %>% filter(nameplate_capacity == max(nameplate_capacity)  ) 


fuel_by_plant <- unit_fuel_by_plant %>% full_join(gen_fuel_by_plant) %>%
  mutate(primary_fuel_type = ifelse(heat_input == 0, fuel_code, primary_fuel_type)) %>% select(-fuel_code) %>% unique

eia_923_use <- eia_923$generation_and_fuel_combined %>% select(plant_id, fuel_type, total_fuel_consumption_mmbtu) %>% 
  mutate(plant_id = as.numeric(plant_id),
         total_fuel_consumption_mmbtu = ifelse(is.na(total_fuel_consumption_mmbtu), 0, total_fuel_consumption_mmbtu))

dup_ids <- fuel_by_plant$plant_id[which(duplicated(fuel_by_plant$plant_id))] %>% unique
fuel_dups <- fuel_by_plant %>% filter(plant_id %in% dup_ids)  %>%
  left_join(eia_923_use,
            by = c("plant_id" = "plant_id",
                   "primary_fuel_type" = "fuel_type")) %>% unique %>%
  ungroup %>% group_by(plant_id) %>% filter(total_fuel_consumption_mmbtu == max(total_fuel_consumption_mmbtu))
fuel_by_plant <- fuel_by_plant %>% filter(!plant_id %in% dup_ids) %>% select(-heat_input, - nameplate_capacity)

dup_ids <- fuel_dups$plant_id[which(duplicated(fuel_dups$plant_id))] %>% unique
fuel_dups <- fuel_dups %>% mutate(primary_fuel_type = ifelse(plant_id %in% dup_ids, NA, primary_fuel_type)) %>%
  select(-heat_input, - nameplate_capacity, - total_fuel_consumption_mmbtu) %>% unique

fuel_by_plant <- fuel_by_plant %>% full_join(fuel_dups)

stopifnot(all(!duplicated(fuel_by_plant$plant_id))) # check there are no more duplicates
rm(fuel_dups,eia_923_use, gen_fuel_by_plant, unit_fuel_by_plant, dup_ids)
# 14.	Plant primary fuel category  ---------------
oil_fuels <- c("DFO", "JF", "KER", "PC", "RFO", "WO", "SGP") # , "OO"
gas_fuels <- c("NG", "PG", "BU") 
oth_FF <- c("BFG", "OG", "TDF", "MSN") #   "HY", "LB", "MH", "MSF"
oth_fuels <- c("H", "MWH", "OTH", "PRG", "PUR", "WH")

fuel_by_plant <- fuel_by_plant %>%
  mutate(primary_fuel_category = case_when(primary_fuel_type %in% coal_fuels ~ "COAL", # DD didn't include "ANT"
                                           primary_fuel_type %in% oil_fuels ~ "OIL",
                                           primary_fuel_type %in% gas_fuels ~ "GAS",
                                           primary_fuel_type %in% oth_FF ~ "OFSL", # other fossil fuel
                                           primary_fuel_type %in% c("NUC") ~ "NUCLEAR",
                                           primary_fuel_type %in% c("WAT") ~ "HYDRO",
                                           primary_fuel_type %in% c("SUN") ~ "SOLAR",
                                           primary_fuel_type %in% c("WND") ~ "WIND",
                                           primary_fuel_type %in% c("GEO") ~ "GEOTHERMAL",
                                           primary_fuel_type %in% oth_fuels ~ "OTHF", # derived from waste heat/hydrogen/purchased/unknown
                                           primary_fuel_type %in% biomass_fuels ~ "BIOMASS")) # DD didn't include "BG", "DG", "MSB". It did include "MSW" which was excluded here 
plant_file <- plant_file %>% full_join(fuel_by_plant)
rm(fuel_by_plant)
# 15.	Create Generation by Fuel table and update Plant file ------------------
ann_gen_by_fuel = generator_file %>% group_by(fuel_code, plant_id) %>% #why by prime mover?
  summarise(ann_gen = sum(generation_ann, na.rm = TRUE),
            ann_gen_oz = sum(generation_oz, na.rm = TRUE)) %>% ungroup %>% group_by(plant_id) %>%
  summarise(ann_gen = sum(ann_gen, na.rm = TRUE),
            ann_gen_coal = sum(ann_gen[which(fuel_code %in% coal_fuels)], na.rm = TRUE),
            ann_gen_oil = sum(ann_gen[which(fuel_code %in% oil_fuels)], na.rm = TRUE),
            ann_gen_gas = sum(ann_gen[which(fuel_code %in% gas_fuels)], na.rm = TRUE),
            ann_gen_nuclear = sum(ann_gen[which(fuel_code %in% c("NUC"))], na.rm = TRUE),
            ann_gen_hydro = sum(ann_gen[which(fuel_code %in% c("WAT"))], na.rm = TRUE),
            ann_gen_biomass = sum(ann_gen[which(fuel_code %in% biomass_fuels)], na.rm = TRUE),
            ann_gen_wind = sum(ann_gen[which(fuel_code %in% c("WND"))], na.rm = TRUE),
            ann_gen_solar = sum(ann_gen[which(fuel_code %in% c("SUN"))], na.rm = TRUE),
            ann_gen_geothermal = sum(ann_gen[which(fuel_code %in% c("GEO"))], na.rm = TRUE),
            ann_gen_other_ff = sum(ann_gen[which(fuel_code %in% oth_FF)], na.rm = TRUE),
            ann_gen_other = sum(ann_gen[which(fuel_code %in% oth_fuels)], na.rm = TRUE),
            
  ) %>% rowwise %>%
  mutate(ann_gen = ifelse(is.na(ann_gen) | abs(ann_gen - sum(ann_gen_coal, ann_gen_oil, ann_gen_gas, ann_gen_nuclear, 
                                                            ann_gen_hydro, ann_gen_biomass, ann_gen_wind, ann_gen_solar,
                                                            ann_gen_geothermal, ann_gen_other_ff, ann_gen_other, na.rm = TRUE)) > 2,
                          sum(ann_gen_coal, ann_gen_oil, ann_gen_gas, ann_gen_nuclear, 
                              ann_gen_hydro, ann_gen_biomass, ann_gen_wind, ann_gen_solar,
                              ann_gen_geothermal, ann_gen_other_ff, ann_gen_other, na.rm = TRUE),
                          ann_gen))

ann_gen_by_fuel <- ann_gen_by_fuel %>% replace_na(list(ann_gen = 0, ann_gen_coal = 0, ann_gen_oil= 0, 
                                                  ann_gen_gas= 0,ann_gen_nuclear= 0,ann_gen_hydro= 0,ann_gen_biomass= 0,
                                                  ann_gen_wind= 0,ann_gen_solar= 0,ann_gen_geothermal= 0,ann_gen_other_ff= 0,ann_gen_other= 0))

# 16.	Resource mix a.	generation by fuel type b.	% resource mix ------------
ann_gen_by_fuel <- ann_gen_by_fuel %>% mutate(ann_gen_non_renew = ann_gen_coal + ann_gen_oil + ann_gen_gas + ann_gen_other_ff + ann_gen_nuclear + ann_gen_other,
               ann_gen_renew = ann_gen_biomass + ann_gen_wind + ann_gen_solar + ann_gen_geothermal + ann_gen_hydro,
               ann_gen_renew_nonhydro = ann_gen_biomass + ann_gen_wind + ann_gen_solar + ann_gen_geothermal ,
               ann_gen_combust = ann_gen_coal + ann_gen_oil + ann_gen_gas + ann_gen_other_ff + ann_gen_biomass + ann_gen_other,
               ann_gen_non_combust = ann_gen_nuclear + ann_gen_wind + ann_gen_solar + ann_gen_geothermal + ann_gen_hydro
  )

ann_gen_by_fuel <- ann_gen_by_fuel %>% mutate(plant_id = as.numeric(plant_id),
               perc_ann_gen_coal = 100 * ann_gen_coal / ann_gen,
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

# checks for negative generations
stopifnot(sum(isTRUE( as.matrix(ann_gen_by_fuel) < 0), na.rm = TRUE) ==0)

plant_file <- plant_file %>% left_join(ann_gen_by_fuel) %>% mutate(
  primary_fuel_type = ifelse(perc_ann_gen_nuclear > 50, "NUC", primary_fuel_type),
  primary_fuel_category = ifelse(perc_ann_gen_nuclear > 50, "NUCLEAR", primary_fuel_category),
)

rm(ann_gen_by_fuel)
# there is some weird v small differences, but rounding fixes
# checks the sum of all = 100
stopifnot(all(100==round(plant_file$perc_ann_gen_renew + plant_file$perc_ann_gen_non_renew,0) |
      is.na(round(plant_file$perc_ann_gen_renew + plant_file$perc_ann_gen_non_renew,0))))
      


# 17.	Useful thermal output ---------------------
eia_923_use <- eia_923$generation_and_fuel_combined %>% group_by(plant_id) %>%
  mutate(plant_id = as.numeric(plant_id))   %>% 
  summarise(total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, na.rm = TRUE),
            elec_fuel_consumption_mmbtu = sum(elec_fuel_consumption_mmbtu, na.rm = TRUE))

CHP <- read_xlsx(here("data", "static_tables", "CHP list.xlsx")) %>% filter(Total > 0) %>% 
  rename(plant_id = `Plant Code`) %>%
  left_join(eia_923_use) %>%
  mutate(uto = 0.8*(total_fuel_consumption_mmbtu - elec_fuel_consumption_mmbtu),
         chp_flag =  "Yes") %>% select(plant_id, uto, chp_flag)

plant_file <- plant_file %>% left_join(CHP) 
rm(CHP,eia_923_use)
# only CHP plants calculate uto??

# 18. Power Heat Ratio ---------------
plant_file <- plant_file %>% 
  mutate(power_heat_ratio = 3.413 * generation_ann / uto)

# 19. Electric allocation Factor ---------------
plant_file <- plant_file %>% 
  mutate(elec_allocation = ifelse(chp_flag == "Yes", 
                                   ifelse(uto == 0, 1, 
                                          3.413 * generation_ann / (0.75*uto + 3.413 * generation_ann)),
                                  NA))

plant_file <- plant_file %>% 
  mutate(elec_allocation = ifelse(elec_allocation < 0, 0,
                                  ifelse(elec_allocation > 1, 1, elec_allocation)))

# 20.	CHP adjustment  ------------------

# ADDED heat_input and heat_input_oz since the DD says it's adjusted by the allocation factor

plant_chp <- plant_file %>% filter( chp_flag == "Yes" ) %>% 
  mutate(heat_input =  elec_allocation * unadj_heat_input,
         heat_input_oz =  elec_allocation * unadj_heat_input_oz,
         power_heat_ratio =  elec_allocation * power_heat_ratio,
         combust_heat_input_1 = elec_allocation * unadj_combust_heat_input,
         combust_heat_input_oz_1 = elec_allocation * unadj_combust_heat_input_oz, 
         nox_mass = elec_allocation * nox_mass,
         nox_oz =  elec_allocation * nox_oz, 
         so2_mass = elec_allocation * so2_mass, 
         co2_mass =  elec_allocation * co2_mass, 
         ch4_mass =elec_allocation * ch4_mass, 
         n2o_mass =  elec_allocation * n2o_mass)

plant_file <- plant_file %>% filter( is.na(chp_flag))
# drop for easier joining later

# why is elec_allocation applied twice? 
# used total heat input because SQL suggests it's the same var combust_heat_input 
# [PLHTIAN - Combust HTI] when they are the same thing???

plant_chp <- plant_chp %>% 
  mutate(combust_heat_input = elec_allocation * combust_heat_input_1 + (unadj_heat_input - combust_heat_input_1),
         combust_heat_input_oz = elec_allocation * combust_heat_input_oz_1 + (unadj_heat_input_oz - combust_heat_input_oz_1))


plant_chp <- plant_chp %>%
  mutate(CHP_combust_heat_input = combust_heat_input_1/elec_allocation -  combust_heat_input,
         CHP_combust_heat_input_oz =combust_heat_input_oz_1/elec_allocation  -  combust_heat_input_oz,
         CHP_nox = nox_mass/elec_allocation -  nox_mass, 
         CHP_nox_oz =nox_oz/elec_allocation -  nox_oz,
         CHP_so2 = so2_mass/elec_allocation -  so2_mass,
         CHP_co2 = co2_mass/elec_allocation -  co2_mass,
         CHP_ch4 = ch4_mass/elec_allocation -  ch4_mass,
         CHP_n2o = n2o_mass/elec_allocation -  n2o_mass) %>%
  select(-combust_heat_input_1, - combust_heat_input_oz_1)


plant_chp <- plant_chp %>% 
  mutate(CHP_nox = ifelse(CHP_nox > unadj_nox_mass | CHP_nox < 0, unadj_nox_mass, CHP_nox),
         CHP_nox_oz = ifelse(CHP_nox_oz > unadj_nox_oz | CHP_nox_oz < 0, unadj_nox_oz, CHP_nox_oz),
         CHP_so2 = ifelse(CHP_so2 > unadj_so2_mass| CHP_so2 < 0, unadj_so2_mass, CHP_so2),
         CHP_co2 = ifelse(CHP_co2 > unadj_co2_mass| CHP_co2 < 0, unadj_co2_mass, CHP_co2),
         CHP_ch4 = ifelse(CHP_ch4 > unadj_ch4_mass| CHP_ch4 < 0, unadj_ch4_mass, CHP_ch4),
         CHP_n2o = ifelse(CHP_n2o > unadj_n2o_mass| CHP_n2o < 0, unadj_n2o_mass, CHP_n2o)) %>%
  mutate(nominal_heat_rate = combust_heat_input * 1000 / ann_gen_combust)

plant_file <- plant_file %>% full_join(plant_chp) %>% 
  mutate(power_heat_ratio = ifelse(combust_flag == 1 | combust_flag ==0.5, nominal_heat_rate, power_heat_ratio))

rm(plant_chp)

# 21. Pumped storage PM ----------------------------------------

pumped_storage <- generator_file %>% select(plant_id, plant_state, plant_name, prime_mover) %>%
  filter(prime_mover == "PS") %>% unique()

plant_file <- plant_file %>% mutate(ps_flag = ifelse(plant_id %in% pumped_storage$plant_id, "Yes", "No"))
rm(pumped_storage)

# 22. NULLs for BA ----------------------------------------

plant_file <- plant_file %>% mutate(ba_name = ifelse(ba_id == "" | is.na(ba_id), "No balancing authority", ba_name),
                                    ba_id = ifelse(ba_id == "" | is.na(ba_id), "NA", ba_id)) 
# do we really want string NA?
# 23. NERC region update ----------------------------------------

plant_file <- plant_file %>% mutate(nerc = case_when(plant_state == "PR" ~ "PR",
                                                     plant_state == "AK" ~ "AK",
                                                     plant_state == "HI" ~ "HI",
                                                     is.na(nerc) ~ "NA",
                                                     nerc == "" ~ "NA",
                                                     TRUE ~ nerc))
# do we really want string NA?
# 24. Update NULLs (system_owner_id) is plant_file ----------------------------------------

plant_file <- plant_file %>% mutate(system_owner_id = ifelse(is.na(system_owner_id), "-9999", system_owner_id))

# 25. Sub region crosswalks  ----------------------------------------

plant_file <- plant_file %>% mutate(nerc_subregion = case_when(nerc == "TRE" ~ "ERCT",
                                                               nerc == "FRCC" ~ "FRCC",
                                                               nerc == "PR" ~ "PRMS",
                                                               TRUE ~ NA_character_))

BAsubregionX <- read_xlsx(here("data", "static_tables", "BAsubregionCrosswalk.xlsx")) %>%
  rename(ba_id = "Balancing Authority Code",
         ba_name = "Balancing Authority Name",
         nerc_subregion = "SUBRGN") %>% select(-Flag)

plant_file_x <- plant_file %>% left_join(BAsubregionX)

BAtransmissionX<- read_xlsx(here("data", "static_tables", "BAtransmissionCrosswalk.xlsx")) %>%
  rename(nerc = "NERC Region",
         ba_id = "Balancing Authority Code",
         system_owner_id = "Transmission or Distribution System Owner ID",
         nerc_subregion = "SUBRGN") %>% mutate(system_owner_id = as.character(system_owner_id))

plant_file_x <- plant_file_x %>% left_join(BAtransmissionX)


# 26. Update NOT IN FILE Counties to blank ----------------------------------------


# 27. Lat/Long ----------------------------------------

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
                                   co2_combust_equiv_out_emission_rate = 2000 * co2_equivalent / ann_gen_combust,
                                   hg_combust_out_emission_rate = 2000 * hg_mass / ann_gen_combust)  


#   b.	Input emission rates ----------------------------------------
  
  plant_file = plant_file %>% mutate(nox_in_emission_rate = 2000 * nox_mass / heat_input,
                                     nox_in_emission_rate_oz = 2000 * nox_oz / heat_input_oz,
                                     so2_in_emission_rate = 2000 * so2_mass / heat_input,
                                     co2_in_emission_rate = 2000 * co2_mass / heat_input,
                                     ch4_in_emission_rate = 2000 * ch4_mass / heat_input,
                                     n2o_in_emission_rate = 2000 *  n2o_mass / heat_input,
                                     co2_equiv_in_emission_rate = 2000 * co2_equivalent / heat_input,
                                     hg_in_emission_rate = 2000 * hg_mass / heat_input)  
#   c.	Output emission rates ----------------------------------------

plant_file = plant_file %>% mutate(nox_out_emission_rate = 2000 * nox_mass / generation_ann,
                                  nox_out_emission_rate_oz = 2000 * nox_oz / generation_oz,
                                  so2_out_emission_rate = 2000 * so2_mass / generation_ann,
                                  co2_out_emission_rate = 2000 * co2_mass / generation_ann,
                                  ch4_out_emission_rate = 2000 * ch4_mass / generation_ann,
                                  n2o_out_emission_rate = 2000 * n2o_mass / generation_ann,
                                  co2_equiv_out_emission_rate = 2000 * co2_equivalent / generation_ann,
                                  hg_out_emission_rate = 2000 * hg_mass / generation_ann)

