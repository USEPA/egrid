
# load required libraries

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)



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
  read_excel("archive/eGRID_2021.xlsx", 
             sheet = "UNT21",
             skip = 1) %>% 
  rename(all_of(clean_names)) # rename columns based on named vector above

library(tidyverse)

# 0. create a file to string concatenate unique values that are not NA

paste_concat <- function(l, number = FALSE){
  l <- l[!is.na(l)]
  l <- l[l!="NA" & l!=""]
  txt <- paste0(unique(l), collapse = ", ")
  
  if(number){return(as.numeric(txt)) # convert to numeric if it can
    }else{return(txt)}
}


## 1. aggregate unit file to plant level ------------------------
 
      # SQL steps group by State, Facility Name, and ORIS code
      # for this reason I included plant name and state in summarise
unit <- unit_file %>% group_by(plant_id) %>%
  summarise(plant_name = paste_concat(plant_name),
            plant_state = paste_concat(plant_state),
            num_units = length(unique(unit_id)), # count
            heat_input = sum(heat_input, na.rm = TRUE), # MMBtu
            heat_input_oz = sum(heat_input_oz, na.rm = TRUE), # MMBtu 
            nox_mass = sum(nox_mass, na.rm = TRUE), # tons
            noz_oz = sum(nox_oz, na.rm = TRUE),# tons
            so2_mass = sum(so2_mass, na.rm = TRUE), # tons
            co2_mass = sum(co2_mass), na.rm = TRUE) %>% # tons
  mutate(plant_id = as.character(plant_id))

## 2. aggregate gen file to plant level----------------------------

      # No list of variables provided, so refenced the SQL steps
      # also added in Name and State for consistency
gen <- generator_file %>% group_by(plant_id) %>%
  summarise(plant_name = paste_concat(plant_name),
            plant_state = paste_concat(plant_state),
            num_gen = length(unique(generator_id)), # count
            nameplate_capacity = sum(nameplate_capacity, na.rm = TRUE), # units?
            generation_ann = sum(generation_ann, na.rm = TRUE), # units?
            generation_oz = sum(generation_oz, na.rm = TRUE)) # units?

## 3. Pull in info from the EIA-860 Plant file - suppressed -------------------------------
      # joins by plant_id, plant_statem and plant_name
      # not using any eia_860 columns yet
#plant_file_gen <-plant_file_gen %>% left_join(eia_860$plant)

#if(any( grepl('not in file|Not in file', plant_file$county))){
 # plant_file_gen <- plant_file_gen %>% mutate(county = ifelse(grepl('not in file|Not in file', county),NA,county ))
#}

## 4.	Combustion heat input ---------------------------------------

generator_file <- generator_file %>% left_join(unit)

heat_input_table = generator_file %>% group_by(fuel_code, prime_mover, plant_id) %>%
    summarise(heat_input = sum(heat_input, na.rm = TRUE),
              heat_input_oz = sum(heat_input_oz, na.rm = TRUE))

        # filter to combustable fuels
heat_input_table <- heat_input_table %>% 
  filter(fuel_code %in% c("AB","BFG","BIT","BLQ","COG","DFO","H","JF","KER","LFG","LIG","MSW","MWH","NG","OBG","OBL","OBS","OG","OTH","PC","PG","PRG","PUR","RC","RFO","SGC","SLW","SUB","TDF","WC","WDL","WDS","WD","WO"))
        # join with aggregated generator file
gen <- gen %>% left_join(heat_input_table)  
        # drop columns from unit for smoother joining later
unit <- unit %>% select(-heat_input, - heat_input_oz) 

## 5.	Update capacity factor  -----------------------------------------------
gen <- gen %>% mutate(capfac = ifelse(generation_ann*8760 < 0, 0, generation_ann*8760))

## 6.	Calculate CH4 emissions ----------------------------------------

      # load static table with emissions factors
emissions_CH4_N2O <- read_csv("data/static_tables/co2_ch4_n2o_ef.csv") %>% filter(!is.na(eia_fuel_code)) %>% 
  select(eia_fuel_code, ch4_ef, n2o_ef)
    
if(any( (!gen$fuel_code %in% emissions_CH4_N2O$eia_fuel_code )& !is.na(gen$fuel_code))) { 
  print(paste0(unique(gen$fuel_code[which(!gen$fuel_code %in% emissions_CH4_N2O$eia_fuel_code& !is.na(gen$fuel_code))]), " is not in emissions data!"))
}
    # join with eia to calculate at plant level
    
eia_CH4_N2O <- eia_923$generation_and_fuel_combined %>% filter(prime_mover != "FC")%>% select(plant_id, total_fuel_consumption_mmbtu, fuel_type) %>%
  left_join(emissions_CH4_N2O, by = c("fuel_type" = "eia_fuel_code")) %>%
  mutate( CH4 = ch4_ef * total_fuel_consumption_mmbtu,
          N2O = n2o_ef * total_fuel_consumption_mmbtu) %>%
  group_by(plant_id) %>%
  summarise(CH4 = sum(CH4, na.rm = TRUE),
            N2O = sum(N2O, na.rm = TRUE))
  

plant_file <- gen %>% full_join(unit)
plant_file <- plant_file %>% left_join(eia_CH4_N2O) 

#


  
    


