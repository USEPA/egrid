

# Load libraries  ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# Check for params() --------

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- (params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Set years to evaluate ---------

cur_year <- as.numeric(params$eGRID_year)
prev_yr1 <- as.character(cur_year - 1)
prev_yr2 <- as.character(cur_year - 2)
prev_yr3 <- as.character(cur_year - 3)
cur_year <- as.character(cur_year)

# Load and clean data -----

## Download historical eGRID data
### Note: check each year if these URLs have changed 

# 2019 data
path_2019 <- "data/static_tables/qa/egrid2019_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/sites/default/files/2021-02/egrid2019_data.xlsx", 
                destfile = path_2019, 
                mode = "wb")
} else {
  print("Stopping. File egrid2019_data.xlsx already downloaded.")
}

# 2020 data
path_2020 <- "data/static_tables/qa/egrid2020_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/system/files/documents/2022-09/eGRID2020_Data_v2.xlsx", 
                destfile = path_2020, 
                mode = "wb")
} else {
  print("Stopping. File egrid2020_data.xlsx already downloaded.")
}

# 2021 data
path_2021 <- "data/static_tables/qa/egrid2021_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/system/files/documents/2023-01/eGRID2021_data.xlsx", 
                destfile = path_2021, 
                mode = "wb")
} else {
  print("Stopping. File egrid2021_data.xlsx already downloaded.")
}

# 2022 data
path_2022 <- "data/static_tables/qa/egrid2022_data.xlsx"

if(!file.exists(path_2019)){
  download.file(url = "https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx", 
                destfile = path_2021, 
                mode = "wb")
} else {
  print("Stopping. File egrid2022_data.xlsx already downloaded.")
}


## Load and combine eGRID subregion data -----

# Rename necessary columns to snake_case 

  subregion_column_names <- c(
  "year" = "YEAR", 
  "subregion" = "SUBRGN",
  "subregion_name" = "SRNAME", 
  "nameplate_capacity" = "SRNAMEPCAP", 
  "nox_output_rate" = "SRNOXRTA", 
  "nox_oz_output_rate" = "SRNOXRTO", 
  "so2_output_rate" = "SRSO2RTA",
  "co2_output_rate" = "SRCO2RTA", 
  "ch4_output_rate" = "SRCH4RTA", 
  "n2o_output_rate" = "SRN2ORTA", 
  "co2e_output_rate" = "SRC2ERTA",
  "nox_input_rate" = "SRNOXRA", 
  "nox_oz_input_rate" = "SRNOXRO", 
  "so2_input_rate" = "SRSO2RA",
  "co2_input_rate" = "SRCO2RA", 
  "ch4_input_rate" = "SRCH4RA", 
  "n2o_input_rate" = "SRN2ORA", 
  "co2e_input_rate" = "SRC2ERA",
  "nox_combustion_rate" = "SRNOXCRT", 
  "nox_oz_combustion_rate" = "SRNOXCRO", 
  "so2_combustion_rate" = "SRSO2CRT",
  "co2_combustion_rate" = "SRCO2CRT", 
  "ch4_combustion_rate" = "SRCH4CRT", 
  "n2o_combustion_rate" = "SRN2OCRT", 
  "co2e_combustion_rate" = "SRC2ECRT",
  "nox_output_rate_coal" = "SRCNOXRT",
  "nox_output_rate_oil" = "SRONOXRT",
  "nox_output_rate_gas" = "SRGNOXRT",
  "nox_output_rate_fossil" = "SRFSNXRT",
  "nox_oz_output_rate_coal" = "SRCNXORT",
  "nox_oz_output_rate_oil" = "SRONXORT",
  "nox_oz_output_rate_gas" = "SRGNXORT",
  "nox_oz_output_rate_fossil" = "SRFSNORT",
  "so2_output_rate_coal" = "SRCSO2RT",
  "so2_output_rate_oil" = "SROSO2RT",
  "so2_output_rate_gas" = "SRGSO2RT",
  "so2_output_rate_fossil" = "SRFSS2RT",
  "co2_output_rate_coal" = "SRCCO2RT",
  "co2_output_rate_oil" = "SROCO2RT",
  "co2_output_rate_gas" = "SRGCO2RT",
  "co2_output_rate_fossil" = "SRFSC2RT",
  "ch4_output_rate_coal" = "SRCCH4RT",
  "ch4_output_rate_oil" = "SROCH4RT",
  "ch4_output_rate_gas" = "SRGCH4RT",
  "ch4_output_rate_fossil" = "SRFCH4RT",
  "n2o_output_rate_coal" = "SRCN2ORT",
  "n2o_output_rate_oil" = "SRON2ORT",
  "n2o_output_rate_gas" = "SRGN2ORT",
  "n2o_output_rate_fossil" = "SRFN2ORT",
  "n2o_output_rate_coal" = "SRCN2ORT",
  "co2e_output_rate_coal" = "SRCC2ERT",
  "co2e_output_rate_oil" = "SROC2ERT",
  "co2e_output_rate_gas" = "SRGC2ERT",
  "co2e_output_rate_fossil" = "SRFSC2ERT",
  "nox_input_rate_coal" = "SRCNOXR",
  "nox_input_rate_oil" = "SRONOXR",
  "nox_input_rate_gas" = "SRGNOXR",
  "nox_input_rate_fossil" = "SRFSNXR",
  "nox_oz_input_rate_coal" = "SRCNXOR",
  "nox_oz_input_rate_oil" = "SRONXOR",
  "nox_oz_input_rate_gas" = "SRGNXOR",
  "nox_oz_input_rate_fossil" = "SRFSNOR",
  "so2_input_rate_coal" = "SRCSO2R",
  "so2_input_rate_oil" = "SROSO2R",
  "so2_input_rate_gas" = "SRGSO2R",
  "so2_input_rate_fossil" = "SRFSS2R",
  "co2_input_rate_coal" = "SRCCO2R",
  "co2_input_rate_oil" = "SROCO2R",
  "co2_input_rate_gas" = "SRGCO2R",
  "co2_input_rate_fossil" = "SRFSC2R",
  "ch4_input_rate_coal" = "SRCCH4R",
  "ch4_input_rate_oil" = "SROCH4R",
  "ch4_input_rate_gas" = "SRGCH4R",
  "ch4_input_rate_fossil" = "SRFCH4R",
  "n2o_input_rate_coal" = "SRCN2OR",
  "n2o_input_rate_oil" = "SRON2OR",
  "n2o_input_rate_gas" = "SRGN2OR",
  "n2o_input_rate_fossil" = "SRFN2OR",
  "n2o_input_rate_coal" = "SRCN2OR",
  "co2e_input_rate_coal" = "SRCC2ER",
  "co2e_input_rate_oil" = "SROC2ER",
  "co2e_input_rate_gas" = "SRGC2ER",
  "co2e_input_rate_fossil" = "SRFSC2ER",
  "nox_nonbaseload_rate" = "SRNBNOX",
  "nox_oz_nonbaseload_rate" = "SRNBNXO",
  "so2_nonbaseload_rate" = "SRNBSO2",
  "co2_nonbaseload_rate" = "SRNBCO2",
  "ch4_nonbaseload_rate" =  "SRNBCH4",
  "n2o_nonbaseload_rate" =  "SRNBN2O",
  "co2e_nonbaseload_rate" = "SRNBC2E",
  "coal_gen" = "SRGENACL", 
  "oil_gen" = "SRGENAOL", 
  "gas_gen" = "SRGENAGS", 
  "nuclear_gen" = "SRGENANC", 
  "hydro_gen" = "SRGENAHY", 
  "biomass_gen" = "SRGENABM",
  "wind_gen" = "SRGENAWI", 
  "solar_gen" = "SRGENASO",
  "geothermal_gen" = "SRGENAGT", 
  "other_fossil_gen" = "SRGENAOF",
  "other_purchased_gen" = "SRGENAOP", 
  "net_gen" = "SRNGENAN")

# read in subregion data for each data year to compare here
subregion_prev_yr3 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr3}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(prev_yr3) %% 1000}"), 
                        skip = 1) %>% 
  select(any_of(subregion_column_names))
  
subregion_prev_yr2 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr2}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(prev_yr2) %% 1000}"),
                        skip = 1) %>% 
  select(any_of(subregion_column_names))

subregion_prev_yr1 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr1}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(prev_yr1) %% 1000}"),
                        skip = 1) %>% 
  select(any_of(subregion_column_names))

subregion_cur_yr <- 
  read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
                        sheet = glue::glue("SRL{as.numeric(cur_year) %% 1000}"),
                        skip = 1) %>% 
  select(any_of(subregion_column_names))

# combine all subregion years
subregion_comparison <- 
  subregion_prev_yr3 %>% 
  bind_rows(subregion_prev_yr2) %>% 
  bind_rows(subregion_prev_yr1) %>% 
  bind_rows(subregion_cur_yr) %>% 
  mutate(year = as.character(year))

## Load state region data -----

# Rename necessary columns to snake_case 

state_column_names <- c(
  "year" = "YEAR", 
  "state" = "PSTATABB",
  "state_nameplate_capacity" = "STNAMEPCAP", 
  "nox_output_rate" = "STNOXRTA", 
  "nox_oz_output_rate" = "STNOXRTO", 
  "so2_output_rate" = "STSO2RTA",
  "co2_output_rate" = "STCO2RTA", 
  "ch4_output_rate" = "STCH4RTA", 
  "n2o_output_rate" = "STN2ORTA", 
  "co2e_output_rate" = "STC2ERTA",
  "nox_input_rate" = "STNOXRA", 
  "nox_oz_input_rate" = "STNOXRO", 
  "so2_input_rate" = "STSO2RA",
  "co2_input_rate" = "STCO2RA", 
  "ch4_input_rate" = "STCH4RA", 
  "n2o_input_rate" = "STN2ORA", 
  "co2e_input_rate" = "STC2ERA",
  "nox_combustion_rate" = "STNOXCRT", 
  "nox_oz_combustion_rate" = "STNOXCRO", 
  "so2_combustion_rate" = "STSO2CRT",
  "co2_combustion_rate" = "STCO2CRT", 
  "ch4_combustion_rate" = "STCH4CRT", 
  "n2o_combustion_rate" = "STN2OCRT", 
  "co2e_combustion_rate" = "STC2ECRT",
  "nox_output_rate_coal" = "STCNOXRT",
  "nox_output_rate_oil" = "STONOXRT",
  "nox_output_rate_gas" = "STGNOXRT",
  "nox_output_rate_fossil" = "STFSNXRT",
  "nox_oz_output_rate_coal" = "STCNXORT",
  "nox_oz_output_rate_oil" = "STONXORT",
  "nox_oz_output_rate_gas" = "STGNXORT",
  "nox_oz_output_rate_fossil" = "STFSNORT",
  "so2_output_rate_coal" = "STCSO2RT",
  "so2_output_rate_oil" = "STOSO2RT",
  "so2_output_rate_gas" = "STGSO2RT",
  "so2_output_rate_fossil" = "STFSS2RT",
  "co2_output_rate_coal" = "STCCO2RT",
  "co2_output_rate_oil" = "STOCO2RT",
  "co2_output_rate_gas" = "STGCO2RT",
  "co2_output_rate_fossil" = "STFSC2RT",
  "ch4_output_rate_coal" = "STCCH4RT",
  "ch4_output_rate_oil" = "STOCH4RT",
  "ch4_output_rate_gas" = "STGCH4RT",
  "ch4_output_rate_fossil" = "STFCH4RT",
  "n2o_output_rate_coal" = "STCN2ORT",
  "n2o_output_rate_oil" = "STON2ORT",
  "n2o_output_rate_gas" = "STGN2ORT",
  "n2o_output_rate_fossil" = "STFN2ORT",
  "n2o_output_rate_coal" = "STCN2ORT",
  "co2e_output_rate_coal" = "STCC2ERT",
  "co2e_output_rate_oil" = "STOC2ERT",
  "co2e_output_rate_gas" = "STGC2ERT",
  "co2e_output_rate_fossil" = "STFSC2ERT",
  "nox_input_rate_coal" = "STCNOXR",
  "nox_input_rate_oil" = "STONOXR",
  "nox_input_rate_gas" = "STGNOXR",
  "nox_input_rate_fossil" = "STFSNXR",
  "nox_oz_input_rate_coal" = "STCNXOR",
  "nox_oz_input_rate_oil" = "STONXOR",
  "nox_oz_input_rate_gas" = "STGNXOR",
  "nox_oz_input_rate_fossil" = "STFSNOR",
  "so2_input_rate_coal" = "STCSO2R",
  "so2_input_rate_oil" = "STOSO2R",
  "so2_input_rate_gas" = "STGSO2R",
  "so2_input_rate_fossil" = "STFSS2R",
  "co2_input_rate_coal" = "STCCO2R",
  "co2_input_rate_oil" = "STOCO2R",
  "co2_input_rate_gas" = "STGCO2R",
  "co2_input_rate_fossil" = "STFSC2R",
  "ch4_input_rate_coal" = "STCCH4R",
  "ch4_input_rate_oil" = "STOCH4R",
  "ch4_input_rate_gas" = "STGCH4R",
  "ch4_input_rate_fossil" = "STFCH4R",
  "n2o_input_rate_coal" = "STCN2OR",
  "n2o_input_rate_oil" = "STON2OR",
  "n2o_input_rate_gas" = "STGN2OR",
  "n2o_input_rate_fossil" = "STFN2OR",
  "n2o_input_rate_coal" = "STCN2OR",
  "co2e_input_rate_coal" = "STCC2ER",
  "co2e_input_rate_oil" = "STOC2ER",
  "co2e_input_rate_gas" = "STGC2ER",
  "co2e_input_rate_fossil" = "STFSC2ER",
  "nox_nonbaseload_rate" = "STNBNOX",
  "nox_oz_nonbaseload_rate" = "STNBNXO",
  "so2_nonbaseload_rate" = "STNBSO2",
  "co2_nonbaseload_rate" = "STNBCO2",
  "ch4_nonbaseload_rate" =  "STNBCH4",
  "n2o_nonbaseload_rate" =  "STNBN2O",
  "co2e_nonbaseload_rate" = "STNBC2E",
  "coal_gen" = "STGENACL", 
  "oil_gen" = "STGENAOL", 
  "gas_gen" = "STGENAGS", 
  "nuclear_gen" = "STGENANC", 
  "hydro_gen" = "STGENAHY", 
  "biomass_gen" = "STGENABM",
  "wind_gen" = "STGENAWI", 
  "solar_gen" = "STGENASO",
  "geothermal_gen" = "STGENAGT", 
  "other_fossil_gen" = "STGENAOF",
  "other_purchased_gen" = "STGENAOP",
  "net_gen" = "STNGENAN")

# read in state data for each data year to compare here
state_prev_yr3 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr3}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(prev_yr3) %% 1000}"), 
             skip = 1) %>% 
  select(any_of(state_column_names))

state_prev_yr2 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr2}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(prev_yr2) %% 1000}"),
             skip = 1) %>% 
  select(any_of(state_column_names))

state_prev_yr1 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr1}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(prev_yr1) %% 1000}"),
             skip = 1) %>% 
  select(any_of(state_column_names))

state_cur_yr <- 
  read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
             sheet = glue::glue("ST{as.numeric(cur_year) %% 1000}"),
             skip = 1) %>% 
  select(any_of(state_column_names))

# combine all years
state_comparison <- 
  state_prev_yr3 %>% 
  bind_rows(state_prev_yr2) %>% 
  bind_rows(state_prev_yr1) %>% 
  bind_rows(state_cur_yr) %>% 
  mutate(year = as.character(year)) %>%
  mutate(coal_output_rate = rowSums(across(contains("output_rate_coal"))),
         oil_output_rate = rowSums(across(contains("output_rate_oil"))),
         gas_output_rate = rowSums(across(contains("output_rate_gas"))),
         fossil_output_rate = rowSums(across(contains("output_rate_fossil"))),
         coal_input_rate = rowSums(across(contains("input_rate_coal"))),
         oil_input_rate = rowSums(across(contains("input_rate_oil"))),
         gas_input_rate = rowSums(across(contains("input_rate_gas"))),
         fossil_input_rate = rowSums(across(contains("input_rate_fossil"))))

## Load US level data -------------

us_column_names <- c(
  "year" = "YEAR", 
  "state" = "PSTATABB",
  "nameplate_capacity" = "USNAMEPCAP", 
  "nox_output_rate" = "USNOXRTA", 
  "nox_oz_output_rate" = "USNOXRTO", 
  "so2_output_rate" = "USSO2RTA",
  "co2_output_rate" = "USCO2RTA", 
  "ch4_output_rate" = "USCH4RTA", 
  "n2o_output_rate" = "USN2ORTA", 
  "co2e_output_rate" = "USC2ERTA",
  "nox_input_rate" = "USNOXRA", 
  "nox_oz_input_rate" = "USNOXRO", 
  "so2_input_rate" = "USSO2RA",
  "co2_input_rate" = "USCO2RA", 
  "ch4_input_rate" = "USCH4RA", 
  "n2o_input_rate" = "USN2ORA", 
  "co2e_input_rate" = "USC2ERA",
  "nox_combustion_rate" = "USNOXCRT", 
  "nox_oz_combustion_rate" = "USNOXCRO", 
  "so2_combustion_rate" = "USSO2CRT",
  "co2_combustion_rate" = "USCO2CRT", 
  "ch4_combustion_rate" = "USCH4CRT", 
  "n2o_combustion_rate" = "USN2OCRT", 
  "co2e_combustion_rate" = "USC2ECRT",
  "nox_output_rate_coal" = "USCNOXRT",
  "nox_output_rate_oil" = "USONOXRT",
  "nox_output_rate_gas" = "USGNOXRT",
  "nox_output_rate_fossil" = "USFSNXRT",
  "nox_oz_output_rate_coal" = "USCNXORT",
  "nox_oz_output_rate_oil" = "USONXORT",
  "nox_oz_output_rate_gas" = "USGNXORT",
  "nox_oz_output_rate_fossil" = "USFSNORT",
  "so2_output_rate_coal" = "USCSO2RT",
  "so2_output_rate_oil" = "USOSO2RT",
  "so2_output_rate_gas" = "USGSO2RT",
  "so2_output_rate_fossil" = "USFSS2RT",
  "co2_output_rate_coal" = "USCCO2RT",
  "co2_output_rate_oil" = "USOCO2RT",
  "co2_output_rate_gas" = "USGCO2RT",
  "co2_output_rate_fossil" = "USFSC2RT",
  "ch4_output_rate_coal" = "USCCH4RT",
  "ch4_output_rate_oil" = "USOCH4RT",
  "ch4_output_rate_gas" = "USGCH4RT",
  "ch4_output_rate_fossil" = "USFCH4RT",
  "n2o_output_rate_coal" = "USCN2ORT",
  "n2o_output_rate_oil" = "USON2ORT",
  "n2o_output_rate_gas" = "USGN2ORT",
  "n2o_output_rate_fossil" = "USFN2ORT",
  "n2o_output_rate_coal" = "USCN2ORT",
  "co2e_output_rate_coal" = "USCC2ERT",
  "co2e_output_rate_oil" = "USOC2ERT",
  "co2e_output_rate_gas" = "USGC2ERT",
  "co2e_output_rate_fossil" = "USFSC2ERT",
  "nox_input_rate_coal" = "USCNOXR",
  "nox_input_rate_oil" = "USONOXR",
  "nox_input_rate_gas" = "USGNOXR",
  "nox_input_rate_fossil" = "USFSNXR",
  "nox_oz_input_rate_coal" = "USCNXOR",
  "nox_oz_input_rate_oil" = "USONXOR",
  "nox_oz_input_rate_gas" = "USGNXOR",
  "nox_oz_input_rate_fossil" = "USFSNOR",
  "so2_input_rate_coal" = "USCSO2R",
  "so2_input_rate_oil" = "USOSO2R",
  "so2_input_rate_gas" = "USGSO2R",
  "so2_input_rate_fossil" = "USFSS2R",
  "co2_input_rate_coal" = "USCCO2R",
  "co2_input_rate_oil" = "USOCO2R",
  "co2_input_rate_gas" = "USGCO2R",
  "co2_input_rate_fossil" = "USFSC2R",
  "ch4_input_rate_coal" = "USCCH4R",
  "ch4_input_rate_oil" = "USOCH4R",
  "ch4_input_rate_gas" = "USGCH4R",
  "ch4_input_rate_fossil" = "USFCH4R",
  "n2o_input_rate_coal" = "USCN2OR",
  "n2o_input_rate_oil" = "USON2OR",
  "n2o_input_rate_gas" = "USGN2OR",
  "n2o_input_rate_fossil" = "USFN2OR",
  "n2o_input_rate_coal" = "USCN2OR",
  "co2e_input_rate_coal" = "USCC2ER",
  "co2e_input_rate_oil" = "USOC2ER",
  "co2e_input_rate_gas" = "USGC2ER",
  "co2e_input_rate_fossil" = "USFSC2ER",
  "nox_nonbaseload_rate" = "USNBNOX",
  "nox_oz_nonbaseload_rate" = "USNBNXO",
  "so2_nonbaseload_rate" = "USNBSO2",
  "co2_nonbaseload_rate" = "USNBCO2",
  "ch4_nonbaseload_rate" =  "USNBCH4",
  "n2o_nonbaseload_rate" =  "USNBN2O",
  "co2e_nonbaseload_rate" = "USNBC2E",
  "coal_gen" = "USGENACL", 
  "oil_gen" = "USGENAOL", 
  "gas_gen" = "USGENAGS", 
  "nuclear_gen" = "USGENANC", 
  "hydro_gen" = "USGENAHY", 
  "biomass_gen" = "USGENABM",
  "wind_gen" = "USGENAWI", 
  "solar_gen" = "USGENASO",
  "geothermal_gen" = "USGENAGT", 
  "other_fossil_gen" = "USGENAOF",
  "other_purchased_gen" = "USGENAOP", 
  "net_gen" = "USNGENAN")

us_prev_yr3 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr3}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(prev_yr3) %% 1000}"), 
             skip = 1) %>% 
  select(any_of(us_column_names))

us_prev_yr2 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr2}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(prev_yr2) %% 1000}"),
             skip = 1) %>% 
  select(any_of(us_column_names))

us_prev_yr1 <- 
  read_excel(glue::glue("data/static_tables/qa/egrid{prev_yr1}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(prev_yr1) %% 1000}"),
             skip = 1) %>% 
  select(any_of(us_column_names))

us_cur_yr <- 
  read_excel(glue::glue("data/outputs/{params$eGRID_year}/egrid{params$eGRID_year}_data.xlsx"), 
             sheet = glue::glue("US{as.numeric(cur_year) %% 1000}"),
             skip = 1) %>% 
  select(any_of(us_column_names))


# combine all years
us_comparison <- 
  us_prev_yr3 %>% 
  bind_rows(us_prev_yr2) %>% 
  bind_rows(us_prev_yr1) %>% 
  bind_rows(us_cur_yr) %>% 
  mutate(year = as.character(year), 
         subregion = "US")

## Combine US and subregion data 
subregion_us_comparison <- 
  us_comparison %>% 
  bind_rows(subregion_comparison) %>%
  mutate(coal_output_rate = rowSums(across(contains("output_rate_coal"))),
         oil_output_rate = rowSums(across(contains("output_rate_oil"))),
         gas_output_rate = rowSums(across(contains("output_rate_gas"))),
         fossil_output_rate = rowSums(across(contains("output_rate_fossil"))),
         coal_input_rate = rowSums(across(contains("input_rate_coal"))),
         oil_input_rate = rowSums(across(contains("input_rate_oil"))),
         gas_input_rate = rowSums(across(contains("input_rate_gas"))),
         fossil_input_rate = rowSums(across(contains("input_rate_fossil")))) 

# Emission rate comparisons -------------
## Emission rate comparison across eGRID subregions -------

# calculate emission rate percent change 

subregion_rate_comparison <- 
  subregion_us_comparison %>% 
  select(year, subregion, subregion_name, contains("rate"), contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("rate") | contains("gen")) %>% 
  mutate(across(.cols = contains(c(glue::glue("rate_{cur_year}"), 
                                   glue::glue("rate_coal_{cur_year}"),
                                   glue::glue("rate_oil_{cur_year}"),
                                   glue::glue("rate_gas_{cur_year}"), 
                                   glue::glue("rate_fossil_{cur_year}"))),
                .fns = ~ (. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                                      / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100,
                .names = "{sub(glue::glue('{cur_year}'), '', .col)}pct"), 
         across(.cols = contains(glue::glue("gen_{cur_year}")), 
                .fns = ~ case_when(
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                                ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                                                / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
                .names = "{sub('_gen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  mutate(generation_notes = paste(sprintf("Coal: %+.1f%%,", coal_pct), # add summary of net generation changes
                                  sprintf("Oil: %+.1f%%,", oil_pct), 
                                  sprintf("Gas: %+.1f%%,", gas_pct), sprintf("Other fossil: %+.1f%%,", other_fossil_pct), 
                                  sprintf("Nuclear: %+.1f%%,", nuclear_pct), sprintf("Hydro: %+.1f%%,", hydro_pct), 
                                  sprintf("Biomass: %+.1f%%,", biomass_pct), sprintf("Wind: %+.1f%%,", wind_pct),
                                  sprintf("Solar: %+.1f%%,", solar_pct), sprintf("Geothermal: %+.1f%%,", geothermal_pct)))
  

## Emission rate comparison across eGRID states -------

state_rate_comparison <- 
  state_comparison %>%
  select(year, state, contains("rate"), contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("rate") | contains("gen")) %>% 
  mutate(across(.cols = contains(c(glue::glue("rate_{cur_year}"), 
                                   glue::glue("rate_coal_{cur_year}"),
                                   glue::glue("rate_oil_{cur_year}"),
                                   glue::glue("rate_gas_{cur_year}"), 
                                   glue::glue("rate_fossil_{cur_year}"))),
                .fns = ~ (. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100,
                .names = "{sub(glue::glue('{cur_year}'), '', .col)}pct"), , 
         across(.cols = contains(glue::glue("gen_{cur_year}")), 
                .fns = ~ case_when(
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                  ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                          / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
                .names = "{sub('_gen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  mutate(generation_notes = paste(sprintf("Coal: %+.1f%%,", coal_pct), # add summary of net generation changes
                                  sprintf("Oil: %+.1f%%,", oil_pct), 
                                  sprintf("Gas: %+.1f%%,", gas_pct), sprintf("Other fossil: %+.1f%%,", other_fossil_pct), 
                                  sprintf("Nuclear: %+.1f%%,", nuclear_pct), sprintf("Hydro: %+.1f%%,", hydro_pct), 
                                  sprintf("Biomass: %+.1f%%,", biomass_pct), sprintf("Wind: %+.1f%%,", wind_pct),
                                  sprintf("Solar: %+.1f%%,", solar_pct), sprintf("Geothermal: %+.1f%%,", geothermal_pct)))

# eGRID subregion and US resource mix -----

# calculate generation percent change

subregion_gen_comparison <- 
  subregion_us_comparison %>% 
  select(year, subregion, subregion_name, contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("gen")) %>% 
  mutate(across(
         .cols = contains(glue::glue("gen_{cur_year}")), 
         .fns = ~ case_when(
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                          ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                                            / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
                  (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
         .names = "{sub('_gen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  pivot_longer(cols = -c("subregion", "subregion_name"), 
               names_to = "energy_source", 
               values_to = "pct_change")

# format generation mix and merge in percent change data
subregion_resource_mix <- 
  subregion_us_comparison %>% 
  select(year, subregion, subregion_name, contains("gen"), -net_gen) %>% 
  pivot_longer(cols = contains("gen"), 
               names_to = "energy_source", 
               values_to = "generation") %>% 
  mutate(energy_source = str_replace(energy_source, "_gen", "")) 

subregion_resource_mix$energy_source <- 
  factor(subregion_resource_mix$energy_source, 
          levels = c("coal", 
                     "oil", 
                     "gas",
                     "other_fossil", 
                     "nuclear", 
                     "hydro", 
                     "biomass", 
                     "wind", 
                     "solar", 
                     "geothermal", 
                     "other_purchased"))


subregion_resource_mix_wider <- 
  subregion_resource_mix %>% 
  pivot_wider(names_from = year, 
              values_from = generation) %>% 
  left_join(subregion_gen_comparison, by = c("subregion", "subregion_name", "energy_source")) %>% 
  select(-subregion_name)

# summarize nameplate capacity and net gen 
subregion_cap_gen <- 
  subregion_us_comparison %>% 
  select(year, subregion, subregion_name, nameplate_capacity, net_gen)


# calculate us resource mix
us_resource_mix <-
  subregion_resource_mix %>%
  group_by(year, energy_source) %>%
  summarize(energy_source_generation = sum(generation, na.rm = TRUE)) %>%
  ungroup() 

cur_year_gen <- as.symbol(glue::glue("generation_{cur_year}"))
prev_year_gen <- as.symbol(glue::glue("generation_{prev_yr1}"))

us_resource_mix_formatted <-
  us_resource_mix %>%
  pivot_wider(names_from = energy_source,
              values_from = energy_source_generation) %>%
  mutate(net_gen = rowSums(across(where(is.numeric)))) %>%
  relocate(net_gen, .after = year) %>% 
  pivot_longer(cols = -c(year),
               names_to = "energy_source", 
               values_to = "generation") %>% 
  pivot_wider(names_from = year, 
              values_from = generation, 
              names_prefix = "generation_") %>% 
  mutate(percent_change = ({{cur_year_gen}} - {{prev_year_gen}}) 
                                          / {{prev_year_gen}} * 100)


# State resource mix ----- 

# calculate generation percent change
state_gen_comparison <- 
  state_comparison %>% 
  select(year, state, contains("gen")) %>% 
  pivot_wider(names_from = year, 
              values_from = contains("gen")) %>% 
  mutate(across(
    .cols = contains(glue::glue("gen_{(cur_year)}")), 
    .fns = ~ case_when(
      (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . == 0) ~ 0, 
      (get(str_replace(cur_column(), cur_year, prev_yr1)) != 0) 
                ~ round((. - get(str_replace(cur_column(), cur_year, prev_yr1))) 
                        / get(str_replace(cur_column(), cur_year, prev_yr1)) * 100, 1), 
      (get(str_replace(cur_column(), cur_year, prev_yr1)) == 0 & . > 0) ~ 100), 
    .names = "{sub('_gen.*', '', .col)}_pct")) %>% 
  select(-contains("gen")) %>% 
  pivot_longer(cols = -c("state"), 
               names_to = "energy_source", 
               values_to = "pct_change")

# format generation mix and merge in percent change data
state_resource_mix <- 
  state_comparison %>% 
  select(year, state, contains("gen"), -net_gen) %>% 
  pivot_longer(cols = contains("gen"), 
               names_to = "energy_source", 
               values_to = "generation") %>% 
  mutate(energy_source = str_replace(energy_source, "_gen", ""))
  

state_resource_mix_wider <- 
  state_resource_mix %>% 
  pivot_wider(names_from = year, 
              values_from = generation) %>% 
  left_join(state_gen_comparison, by = c("state", "energy_source"))

# summarize nameplate capacity and net gen 
state_cap_gen <- 
  state_comparison %>% 
  select(year, state, state_nameplate_capacity, net_gen)

