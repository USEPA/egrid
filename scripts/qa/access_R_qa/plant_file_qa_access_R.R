## -------------------------------
##
## Plant file QA 
##
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the plant file creation in a given year. 
##
## The checks performed will output a CSV file with any differences 
## between Access and R plant files. 
## 
## Author: Sara Sokolinski, Abt Global
## 
## Date created: 8/9/2024
##

## 0. Setup -------------------------------
# load libraries
library(dplyr)
library(here)
library(readr)
library(readxl)
library(stringr)

# set directory for saving files 
save_dir <- here("data/outputs/qa/plant_file_differences", Sys.Date(),"")

# create it if it doesn't already exist
if(!dir.exists(save_dir)) {dir.create(save_dir,recursive = T)}
do.call(file.remove, list(dir(save_dir, full.names = TRUE)))

## 1. R Version --------------
# load plant file R
plant_r <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS"))

write.csv(plant_r, paste0(save_dir,"/plant_file_qa.csv"))
# add "_r" after each variable to easily identify dataset 
colnames(plant_r) <- paste0(colnames(plant_r), "_r")

plant_r <- plant_r %>% rename("plant_id" = "plant_id_r")

## 2. Access Version ---------------------

plant_access <- read_excel(glue::glue("data/raw_data/eGRID_Data{params$eGRID_year}.xlsx"), 
                           sheet = glue::glue("PLNT{as.numeric(params$eGRID_year) %% 1000}"), 
                           skip = 1, 
                           guess_max = 4000) %>% janitor::clean_names() %>% 
  mutate(orispl = as.character(orispl), 
         numunt = as.integer(numunt), 
         numgen = as.integer(numgen), 
         utlsrvid = as.character(utlsrvid), 
         oprcode = as.character(oprcode), 
         year = as.character(year)) %>%  
  rename("seqplt_access" = glue::glue("seqplt{as.numeric(params$eGRID_year) %% 1000}"),
         "year_access" = "year", 
         "plant_id" = "orispl",
         "plant_name_access" = "pname", 
         "plant_state_access" = "pstatabb",
         "system_owner_access" = "oprname",
         "system_owner_id_access" = "oprcode",
         "utility_name_access" = "utlsrvnm",
         "utility_id_access" = "utlsrvid",
         "sector_name_access" = "sector",
         "ba_name_access" = "baname",
         "ba_code_access" = "bacode",
         "nerc_access" = "nerc",
         "egrid_subregion_access" = "subrgn",
         "egrid_subregion_name_access" = "srname",
         "isorto_access" = "isorto",
         "fips_state_code_access" = "fipsst",
         "fips_county_code_access" = "fipscnty",
         "county_access" = "cntyname",
         "lat_access" = "lat",
         "lon_access" = "lon",
         "camd_flag_access" = "camdflag",
         "num_units_access" = "numunt", 
         "num_generators_access"= "numgen",   
         "primary_fuel_type_access" = "plprmfl",
         "primary_fuel_category_access"="plfuelct",
         "coal_flag_access"="coalflag",
         "capfac_access" = "capfac",
         "nameplate_capacity_access" = "namepcap",
         "nonbaseload_access"="nbfactor",
         "biomass_adj_flag_access"="rmbmflag",
         "chp_flag_access"="chpflag",
         "useful_thermal_output_access"="usethrmo",
         "power_heat_ratio_access"="pwrtoht", 
         "elec_allocation_access"="elcalloc",
         "ps_flag_access"="psflag",
         "combust_heat_input_access"="plhtian",
         "combust_heat_input_oz_access"="plhtioz",
         "heat_input_access"="plhtiant",
         "heat_input_oz_access"="plhtiozt",
         "generation_ann_access"="plngenan", 
         "generation_oz_access" = "plngenoz",
         "nox_mass_access"="plnoxan",
         "nox_oz_mass_access"="plnoxoz",
         "so2_mass_access"="plso2an",
         "co2_mass_access"="plco2an",
         "ch4_mass_access"="plch4an",
         "n2o_mass_access"="pln2oan",
         "co2e_mass_access"="plco2eqa",
         "hg_mass_access"="plhgan",
         "nox_out_emission_rate_access"="plnoxrta", 
         "nox_oz_out_emission_rate_access"="plnoxrto",
         "so2_out_emission_rate_access"="plso2rta",
         "co2_out_emission_rate_access"="plco2rta",
         "ch4_out_emission_rate_access"="plch4rta",
         "n2o_out_emission_rate_access"="pln2orta",
         "co2e_out_emission_rate_access"="plc2erta",
         "hg_out_emission_rate_access"="plhgrta",
         "nox_in_emission_rate_access"="plnoxra",
         "nox_oz_in_emission_rate_access"="plnoxro",
         "so2_in_emission_rate_access"="plso2ra",
         "co2_in_emission_rate_access"="plco2ra",
         "ch4_in_emission_rate_access"="plch4ra",
         "n2o_in_emission_rate_access"="pln2ora",
         "co2e_in_emission_rate_access"= "plc2era",
         "hg_in_emission_rate_access"="plhgra",
         "nox_combust_out_emission_rate_access"="plnoxcrt",
         "nox_oz_combust_out_emission_rate_access"="plnoxcro",
         "so2_combust_out_emission_rate_access"="plso2crt",
         "co2_combust_out_emission_rate_access"="plco2crt",
         "ch4_combust_out_emission_rate_access"="plch4crt",
         "n2o_combust_out_emission_rate_access"="pln2ocrt",
         "co2e_combust_out_emission_rate_access"="plc2ecrt",
         "hg_combust_out_emission_rate_access"="plhgcrt",
         "unadj_nox_mass_access"="unnox",
         "unadj_nox_oz_mass_access"="unnoxoz",
         "unadj_so2_mass_access"="unso2",
         "unadj_co2_mass_access"="unco2",
         "unadj_ch4_mass_access"="unch4",
         "unadj_n2o_mass_access"="unn2o",
         "unadj_hg_mass_access"="unhg",
         "unadj_combust_heat_input_access"="unhti",
         "unadj_combust_heat_input_oz_access"="unhtioz",
         "unadj_heat_input_access"="unhtit",
         "unadj_heat_input_oz_access"="unhtiozt",
         "unadj_nox_source_access"="unnoxsrc",
         "unadj_nox_oz_source_access"="unnozsrc",
         "unadj_so2_source_access"="unso2src",
         "unadj_co2_source_access"="unco2src",
         "unadj_ch4_source_access"="unch4src",
         "unadj_n2o_source_access"="unn2osrc",
         "unadj_hg_source_access"="unhgsrc",
         "unadj_heat_input_source_access"="unhtisrc",
         "unadj_heat_input_oz_source_access"="unhozsrc",
         "nox_biomass_access"="bionox",
         "nox_bio_oz_access"="bionoxoz",
         "so2_biomass_access"="bioso2",
         "co2_biomass_access"="bioco2",
         "co2_non_biomass_access"="bioch4",
         "ch4_biomass_access"="bioch4",
         "n2o_biomass_access"="bion2o",
         "chp_combust_heat_input_access"="chpchti",
         "chp_combust_heat_input_oz_access"="chpchtioz",
         "chp_nox_access"="chpnox",
         "chp_nox_oz_access"="chpnoxoz",
         "chp_so2_access"="chpso2",
         "chp_co2_access"="chpco2",
         "chp_ch4_access"="chpch4",
         "chp_n2o_access"="chpn2o",
         "nominal_heat_rate_access"="plhtrt", 
         "ann_gen_coal_access"="plgenacl",
         "ann_gen_oil_access"="plgenaol",
         "ann_gen_gas_access"="plgenags",
         "ann_gen_nuclear_access"="plgenanc",
         "ann_gen_hydro_access"="plgenahy",
         "ann_gen_biomass_access"="plgenabm",
         "ann_gen_wind_access"="plgenawi",
         "ann_gen_solar_access"="plgenaso",
         "ann_gen_geothermal_access"="plgenagt",
         "ann_gen_other_ff_access"="plgenaof",
         "ann_gen_other_access"="plgenaop",
         "ann_gen_non_renew_access"="plgenatn",
         "ann_gen_renew_access"="plgenatr",
         "ann_gen_renew_nonhydro_access"="plgenath",
         "ann_gen_combust_access" = "plgenacy",
         "ann_gen_non_combust_access"="plgenacn",
         "perc_ann_gen_coal_access"="plclpr",
         "perc_ann_gen_oil_access"="plolpr",
         "perc_ann_gen_gas_access"="plgspr",
         "perc_ann_gen_nuclear_access"="plncpr",
         "perc_ann_gen_hydro_access"="plhypr",
         "perc_ann_gen_biomass_access"="plbmpr",
         "perc_ann_gen_wind_access"="plwipr",
         "perc_ann_gen_solar_access"="plsopr",
         "perc_ann_gen_geothermal_access"="plgtpr",
         "perc_ann_gen_other_ff_access"="plofpr",
         "perc_ann_gen_other_access"="ploppr",
         "perc_ann_gen_non_renew_access"="pltnpr",
         "perc_ann_gen_renew_access"="pltrpr",
         "perc_ann_gen_renew_nonhydro_access"="plthpr",
         "perc_ann_gen_combust_access"="plcypr",
         "perc_ann_gen_non_combust_access"="plcnpr") 

# load plant differences from unit and gen files

gen_diffs <- 
  read_csv(glue::glue("data/outputs/qa/generator_file_differences/{params$eGRID_year}/plant_gen_difference_ids.csv")) %>% 
  select(plant_id, source_diff) %>% distinct()

unit_diffs <- 
  read_csv(glue::glue("data/outputs/qa/unit_file_differences/{params$eGRID_year}/plant_unit_difference_ids.csv")) %>% 
  select(plant_id, source_diff) %>% distinct()

gen_unit_diffs <- 
  unit_diffs %>% 
  full_join(gen_diffs) %>% 
  group_by(plant_id) %>% 
  mutate(source_diff = paste(source_diff, collapse = ", "), 
         plant_id = as.character(plant_id))


## 3. Compare Columns -------------------------

# remove suffix from column names

r_cols <- lapply(colnames(plant_r), FUN=gsub, pattern="_r$", replacement="") %>% unlist()
access_cols <- lapply(colnames(plant_access), FUN=gsub, pattern="_access$", replacement="") %>% unlist()

# Check that all access columns appear in the r columns
#stopifnot(length(access_cols[!access_cols %in% r_cols])==0)


## 4. Combine ------------------------------

# combine the two datasets
plant_comparison <- 
  plant_r %>%
  full_join(plant_access, by = c("plant_id")) %>% 
  left_join(gen_unit_diffs)

## 5. Compare Values ------------------

# a1. plant_id in R that are NOT in Access dataset ---------
check_diff_plant_r <- 
  plant_r %>% 
  filter(!is.na(plant_id)) %>%
  anti_join(plant_access, by = c("plant_id"))
  
if(nrow(check_diff_plant_r) > 0) {
  write_csv(check_diff_plant_r, paste0(save_dir, "check_diff_plant_r.csv")) }

# a2. plant_id in Access that are NOT in R dataset ---------
check_diff_plant_access <- 
  plant_access %>% 
  anti_join(plant_r, by = c("plant_id" ="plant_id")) %>% 
  filter(!is.na(plant_id))

if(nrow(check_diff_plant_access) > 0) {
  write_csv(check_diff_plant_access, paste0(save_dir, "check_diff_plant_access.csv")) }


# create function to make comparison ---------------

plant_compare <- function(x, data = plant_comparison, outdir = save_dir) {
  r_name <- as.name(paste0(x,"_r"))
  access_name <- as.name(paste0(x,"_access"))
  comp <- data %>% 
    filter(mapply(identical, !!!r_name, !!!access_name) == FALSE) %>% 
    select(plant_id, !!r_name, !!access_name, source_diff) %>% distinct()
  if(nrow(comp) > 0) {
    write_csv(comp, paste0(outdir, "check_", x, ".csv")) }
  return(comp)
}

# b. Loop over list of columns ----------
#  - exclude plant_id and other_cols by looking at access_cols
cols <- access_cols[access_cols != "plant_id"]

for(i in cols) {
  x <- plant_compare(i)
  if(nrow(x)> 0) {
    assign(paste0("check_", i), x)
    rm(x)
  } else {
     rm(x)
  }
}

