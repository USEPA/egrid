## -------------------------------
##
## Plant file QA 
## 
##  Q: Should the file folder/name be for 2021? since thats the data we are testing on?
##
## Purpose: 
## 
## This file evaluates the differences in the R and Access database 
## for the plant file creation in 2021. 
##
## The checks performed will output a CSV file with any differences 
## between Access and R plant files. 
## 
## Author: Sara Sokolinski sara.sokolinski@abtglobal.com 
## 
## Date created: 8/9/2024
##
## 0. Setup -------------------------------
# load libraries
library(dplyr)
library(readr)
library(readxl)
library(stringr)

# set directory for saving files 
save_dir <- "data/outputs/qa/plant_file_differences/"

## 1. R Version --------------
# load plant file R
plant_r <- read_rds("data/outputs/plant_file.RDS")

# add "_r" after each variable to easily identify dataset 
colnames(plant_r) <- paste0(colnames(plant_r), "_r")

plant_r <- plant_r %>% rename("plant_id" = "plant_id_r")

## 2. Access Version ---------------------
excel_sheets("data/raw_data/eGRID_2021.xlsx")

plant_access <- read_excel("data/raw_data/eGRID_2021.xlsx", sheet = "PLNT21", 
                               skip = 1, 
                               guess_max = 4000) %>% janitor::clean_names() %>% 
  rename("seq_access" = "seqplt", # need to add sequnt to plant_r create script
         "year_access" = "year", 
         "plant_id" = "orispl",
         "plant_name_access" = "pname", 
         "plant_state_access" = "pstatabb", 
         #"plant_id" = "plantid", 
         "system_owner_access" = "oprname",
         "system_owner_id_access" = "oprcode",
         "utility_name_access" = "utlsrvnm",
         "utility_id_access" = "utlsrvid",
         "sector_access" = "sector",
         "ba_name_access" = "baname",
         "ba_id_access" = "bacode",
         "nerc_access" = "nerc",
         "nerc_subregion_access" = "subrgn",
         "nerc_subregion_name_access" = "srname",
         "isorto_access" = "isorto",
         "FIPS_state_access" = "fipsst",
         "FIPS_county_access" = "fipscnty",
         "county_access" = "cntyname",
         "lat_access" = "lat",
         "lon_access" = "lon",
         "camd_flag_access" = "camdflag",
         "num_units_access" = "numunt", # REMOVE num_units_op
         "num_gen_access"= "numgen",   # REMOVE num_gen_op
         "primary_fuel_type_access" = "plprmfl",
         "primary_fuel_category_access"="plfuelct",
         "coal_flag_access"="coalflag",
         #"combust_access"
         "capfac_access" = "capfac",
         "nameplate_capacity_access" = "namepcap",
         "nonbaseload_access"="nbfactor",
         "biomass_adj_flag_access"="rmbmflag",
         "chp_flag_access"="chpflag",
         "uto_access"="usethrmo",
         "power_to_heat_access"="pwrtoht", # figure out difference with power_heat_ratio
         "elec_allocation_access"="elcalloc",
         "ps_flag_access"="psflag",
         "combust_heat_input_access"="plhtian",
         "combust_heat_input_oz_access"="plhtioz",
         "heat_input_access"="plhtiant",
         "heat_input_oz_access"="plhtiozt",
         "generation_ann_access"="plngenan", # "ann_gen_access" gets created for denominator when creating fuel %
         "generation_oz_access" = "plngenoz",
         "nox_mass_access"="plnoxan",
         "nox_oz_access"="plnoxoz",
         "so2_mass_access"="plso2an",
         "co2_mass_access"="plco2an",
         "ch4_mass_access"="plch4an",
         "n2o_mass_access"="pln2oan",
         "co2_equivalent_access"="plco2eqa",
         "hg_mass_access"="plhgan",
         "nox_out_emission_rate_access"="plnoxrta", 
         "nox_out_emission_rate_oz_access"="plnoxrto",
         "so2_out_emission_rate_access"="plso2rta",
         "co2_out_emission_rate_access"="plco2rta",
         "ch4_out_emission_rate_access"="plch4rta",
         "n2o_out_emission_rate_access"="pln2orta",
         "co2_equiv_out_emission_rate_access"="plc2erta",
         "hg_out_emission_rate_access"="plhgrta",
         "nox_in_emission_rate_access"="plnoxra",
         "nox_in_emission_rate_oz_access"="plnoxro",
         "so2_in_emission_rate_access"="plso2ra",
         "co2_in_emission_rate_access"="plco2ra",
         "ch4_in_emission_rate_access"="plch4ra",
         "n2o_in_emission_rate_access"="pln2ora",
         "co2_equiv_in_emission_rate_access"= "plc2era",
         "hg_in_emission_rate_access"="plhgra",
         "nox_combust_out_emission_rate_access"="plnoxcrt",
         "nox_combust_out_emission_rate_oz_access"="plnoxcro",
         "so2_combust_out_emission_rate_access"="plso2crt",
         "co2_combust_out_emission_rate_access"="plco2crt",
         "ch4_combust_out_emission_rate_access"="plch4crt",
         "n2o_combust_out_emission_rate_access"="pln2ocrt",
         "co2_equiv_combust_out_emission_rate_access"="plc2ecrt",
         "hg_combust_out_emission_rate_access"="plhgcrt",
         "unadj_nox_mass_access"="unnox",
         "unadj_nox_oz_access"="unnoxoz",
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
         "ch4_source_access"="unch4src",
         "n2o_source_access"="unn2osrc",
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
         "power_heat_ratio_access"="plhtrt", # check - already used above..
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
         "perc_ann_gen_non_combust_access"="plcnpr") %>% 
  mutate(plant_id = as.character(plant_id))

## 3. Compare Columns -------------------------
r_cols <- lapply(colnames(plant_r), FUN=gsub, pattern="_r$", replacement="") %>% unlist()
access_cols <- lapply(colnames(plant_access), FUN=gsub, pattern="_access$", replacement="") %>% unlist()

access_cols[!access_cols %in% r_cols]
# all access columns should appear in the r data
# Difference Note - r does not have a sequence variable yet
r_cols[!r_cols %in% access_cols]
# r can contain more columns
# Difference Note - access does not create source columns that are unadj
other_cols <- c(ch4_source, n2o_source, nox_source, nox_oz_source, co2_source, so2_source,
                heat_input_source & heat_input_oz_source)
# Difference Note - r splits plant_name into 
other_cols <- c(other_cols, plant_name_gen, plant_name_unit)
# Additional variables in R 
other_cols <- c(other_cols, num_gen_op num_units_op) # Remove in plant_file_create
other_cols <- c(other_cols, ann_gen) # used as denominator of perc_ann_gen columns
other_cols <- c(other_cols, ch4_ef, n2o_ef) # retains emissions factory - Remove in plant_file_create
# - fuel code - ??
# - combust_flag - ??
# - total_fuel_consumption_mmbtu - ??
# - co2_non_biomass - ??
# - heat_input_oz_season - ??
# - ann_gen_renew_nonhydro - ??
# - nominal_heat_rate - ??          
# - co2_combust_equiv_out_emission_rate - ??



## 4. Combine & Compare Values ------------------------------
# combine the two datasets
plant_comparison <- 
  plant_r %>% 
  full_join(plant_access, by = c("plant_id", 
                                "plant_id")) 

# Column by column checks -------------
# identify if there are any plants in R that are NOT in Access dataset
# anti_join() pulls out differences
check_diff_plant_r <- 
  plant_r %>% 
  anti_join(plant_access, by = c("plant_id", "plant_id")) %>% 
  filter(!is.na(plant_id))

if(nrow(check_diff_plant_r) > 0) {
  write_csv(check_diff_plant_r, paste0(save_dir, "check_diff_plant_r.csv")) }

# check if there are any plants in Access that are NOT in R dataset
check_diff_plant_access <- 
  plant_access %>% 
  anti_join(plant_r, by = c("plant_id", "plant_id")) %>% 
  filter(!is.na(plant_id))

if(nrow(check_diff_plant_access) > 0) {
  write_csv(check_diff_plant_access, paste0(save_dir, "check_diff_plant_access.csv")) }

# check if plant names match 
check_plant_names <- 
  plant_comparison %>% 
  filter(!(plant_name_r == plant_name_access)) %>% 
  select(plant_id, plant_name_r, plant_name_access) %>% distinct()

if(nrow(check_plant_names) > 0) {
  write_csv(check_plant_names, paste0(save_dir, "check_plant_names.csv")) }

# check if plant states match 
check_plant_state <- 
  plant_comparison %>% 
  filter(!(plant_state_r == plant_state_access)) %>% 
  select(plant_id, plant_state_r, plant_state_access) %>% distinct()

if(nrow(check_plant_state) > 0) {
  write_csv(check_plant_state, paste0(save_dir, "check_plant_state.csv")) }

# check if plant prime movers match
check_prime_mover <- 
  plant_comparison %>% 
  filter(!(prime_mover_r == prime_mover_access)) %>% 
  select(plant_id, plant_id, prime_mover_r, prime_mover_access)

if(nrow(check_prime_mover) > 0) {
  write_csv(check_prime_mover, paste0(save_dir, "check_prime_mover.csv")) }

# check operating status
check_operating_status <- 
  plant_comparison %>% 
  filter(!(operating_status_r == operating_status_access)) %>% 
  select(plant_id, plant_id, operating_status_r, operating_status_access)

if(nrow(check_operating_status) > 0) {
  write_csv(check_operating_status, paste0(save_dir, "check_operating_status.csv")) }

# check CAMD flag
check_camd_flag <- 
  plant_comparison %>% 
  filter(!(camd_flag_r == camd_flag_access)) %>% 
  select(plant_id, plant_id, camd_flag_r, camd_flag_access)

if(nrow(check_camd_flag) > 0) {
  write_csv(check_camd_flag, paste0(save_dir, "check_camd_flag.csv")) }

# check program code
check_program_code <- 
  plant_comparison %>% 
  separate_wider_delim(program_code_access, delim = ",", names = c("program_1_access", 
                                                                   "program_2_access", 
                                                                   "program_3_access", 
                                                                   "program_4_access", 
                                                                   "program_5_access", 
                                                                   "program_6_access"), 
                       too_few = "align_start", too_many = "error") %>% # fix delimiter to match R 
  plante("program_code_access", c(program_1_access, program_2_access, program_3_access, 
                                 program_4_access, program_5_access, program_6_access), 
        sep = ",", na.rm = TRUE) %>% 
  filter(!(program_code_r == program_code_access)) %>% 
  select(plant_id, plant_id, program_code_r, program_code_access)

if(nrow(check_program_code) > 0) {
  write_csv(check_program_code, paste0(save_dir, "check_program_code.csv")) }

# check boiler firing type
check_botfirty <- 
  plant_comparison %>% 
  filter(!(botfirty_r == botfirty_access)) %>% 
  select(plant_id, plant_id, botfirty_r, botfirty_access)

if(nrow(check_botfirty) > 0) {
  write_csv(check_botfirty, paste0(save_dir, "check_botfirty.csv")) }

# check number of generators
check_num_gens <- 
  plant_comparison %>% 
  filter(!(num_generators_r == num_generators_access)) %>% 
  select(plant_id, plant_id, num_generators_r, num_generators_access)

if(nrow(check_num_gens) > 0) {
  write_csv(check_num_gens, paste0(save_dir, "check_num_gens.csv")) }

# check primary fuel type
check_fuel_type <- 
  plant_comparison %>% 
  filter(!(primary_fuel_type_r == primary_fuel_type_access)) %>% 
  select(plant_id, plant_id, primary_fuel_type_r, primary_fuel_type_access)

if(nrow(check_fuel_type) > 0) {
  write_csv(check_fuel_type, paste0(save_dir, "check_fuel_type.csv")) }

# check operating hours
check_operating_hours <- 
  plant_comparison %>% 
  filter(!(operating_hours_r == operating_hours_access)) %>% 
  select(plant_id, plant_id, operating_hours_r, operating_hours_access)

if(nrow(check_operating_hours) > 0) {
  write_csv(check_operating_hours, paste0(save_dir, "check_operating_hours.csv")) }

# check SO2 controls
check_so2_controls <- 
  plant_comparison %>% 
  filter(!(so2_controls_r == so2_controls_access)) %>% 
  select(plant_id, plant_id, so2_controls_r, so2_controls_access)


if(nrow(check_so2_controls) > 0) {
  write_csv(check_so2_controls, paste0(save_dir, "check_so2_controls.csv")) }

# check NOx controls
check_nox_controls <- 
  plant_comparison %>% 
  filter(!(nox_controls_r == nox_controls_access)) %>% 
  select(plant_id, plant_id, nox_controls_r, nox_controls_access)

if(nrow(check_nox_controls) > 0) {
  write_csv(check_nox_controls, paste0(save_dir, "check_nox_controls.csv")) }

# check Hg controls flag
check_hg_flag <- 
  plant_comparison %>% 
  filter(!(hg_controls_flag_r == hg_controls_flag_access)) %>% 
  select(plant_id, plant_id, hg_controls_flag_r, hg_controls_flag_access)

if(nrow(check_hg_flag) > 0) {
  write_csv(check_hg_flag, paste0(save_dir, "check_hg_controls_flag.csv")) }

# check year online
check_year_online <- 
  plant_comparison %>% 
  filter(!(year_online_r == year_online_access)) %>% 
  select(plant_id, plant_id, year_online_r, year_online_access)

if(nrow(check_year_online) > 0) {
  write_csv(check_year_online, paste0(save_dir, "check_year_online.csv")) }

# Heat input checks ---------
# compare sums of heat input annual and ozone season
check_total_heat_input <- 
  plant_comparison %>% 
  summarize(sum_heat_input_r = sum(heat_input_r, na.rm = TRUE), 
            sum_heat_input_access = sum(heat_input_access, na.rm = TRUE), 
            sum_heat_input_oz_r = sum(heat_input_oz_r, na.rm = TRUE), 
            sum_heat_input_oz_access = sum(heat_input_oz_access, na.rm = TRUE)) %>% 
  select(contains("sum")) %>% 
  mutate(diff_heat_input = sum_heat_input_r - sum_heat_input_access, 
         diff_heat_input_oz = sum_heat_input_oz_r - sum_heat_input_oz_access)
  
if(!(check_total_heat_input$diff_heat_input == 0) | !(check_total_heat_input$diff_heat_input_oz == 0)) {
  write_csv(check_total_heat_input %>% select(sum_heat_input_r, sum_heat_input_access, diff_heat_input, 
                                        sum_heat_input_oz_r, sum_heat_input_oz_access, diff_heat_input_oz), 
            paste0(save_dir, "check_total_heat_input.csv")) }

# check heat input at plant level 
# annual heat input check 
check_heat_input_ann_plant <- 
  plant_comparison %>% 
  filter(!(heat_input_r == heat_input_access)) %>% 
  mutate(diff_heat_input = heat_input_r - heat_input_access) %>% 
  filter(diff_heat_input > 1 | diff_heat_input < -1) %>% 
  select(plant_id, plant_id, heat_input_r, heat_input_access, diff_heat_input)

if(nrow(check_heat_input_ann_plant) > 0) {
  write_csv(check_heat_input_ann_plant, paste0(save_dir, "check_heat_input_ann_plant.csv")) }

# ozone season heat input check 
check_heat_input_oz_plant <- 
  plant_comparison %>% 
  filter(!(heat_input_oz_r == heat_input_oz_access)) %>% 
  mutate(diff_heat_input_oz = heat_input_oz_r - heat_input_oz_access) %>% 
  filter(diff_heat_input_oz > 1 | diff_heat_input_oz < -1) %>% 
  select(plant_id, plant_id, heat_input_oz_r, heat_input_oz_access, diff_heat_input_oz)

if(nrow(check_heat_input_oz_plant) > 0) {
  write_csv(check_heat_input_oz_plant, paste0(save_dir, "check_heat_input_oz_plant.csv")) }

# check heat input sources
# annual heat input source
check_heat_input_ann_source <- 
  plant_comparison %>% 
  filter(!(heat_input_source_r == heat_input_source_access)) %>% 
  select(plant_id, plant_id, heat_input_r, heat_input_access, heat_input_source_r, heat_input_source_access)

if(nrow(check_heat_input_ann_source) > 0) {
  write_csv(check_heat_input_ann_source, paste0(save_dir, "check_heat_input_ann_source.csv")) }

# ozone heat input source 
check_heat_input_oz_source <- 
  plant_comparison %>% 
  filter(!(heat_input_oz_source_r == heat_input_oz_source_access)) %>% 
  select(plant_id, plant_id, heat_input_oz_r, heat_input_oz_access, heat_input_oz_source_r, heat_input_oz_source_access)

if(nrow(check_heat_input_oz_source) > 0) {
  write_csv(check_heat_input_oz_source, paste0(save_dir, "check_heat_input_oz_source.csv")) }

# Check emissions ------------
## Check NOx emissions -------------
check_total_nox_mass <- 
  plant_comparison %>% 
  summarize(sum_nox_mass_r = sum(nox_mass_r, na.rm = TRUE), 
            sum_nox_mass_access = sum(nox_mass_access, na.rm = TRUE), 
            sum_nox_oz_mass_r = sum(nox_oz_mass_r, na.rm = TRUE), 
            sum_nox_oz_mass_access = sum(nox_oz_mass_access, na.rm = TRUE)) %>% 
  select(contains("sum")) %>% 
  mutate(diff_nox_ann = sum_nox_mass_r - sum_nox_mass_access,
         diff_nox_oz = sum_nox_oz_mass_r - sum_nox_oz_mass_access)

if(!(check_total_nox_mass$diff_nox_ann == 0) | !(check_total_nox_mass$diff_nox_oz == 0)) {
  write_csv(check_total_nox_mass %>% select(sum_nox_mass_r, sum_nox_mass_access, diff_nox_ann, 
                                        sum_nox_oz_mass_r, sum_nox_oz_mass_access, diff_nox_oz), 
            paste0(save_dir, "check_total_nox_mass.csv")) }

# plant level NOx emissions
# annual NOx
check_nox_ann_plant <- 
  plant_comparison %>% 
  filter(!(nox_mass_r == nox_mass_access)) %>% 
  mutate(diff_nox_mass = nox_mass_r - nox_mass_access) %>% 
  filter(diff_nox_mass > 1 | diff_nox_mass < -1) %>% 
  select(plant_id, plant_id, nox_mass_r, nox_mass_access, nox_source_r, nox_source_access)

if(nrow(check_nox_ann_plant) > 0) {
  write_csv(check_nox_ann_plant, paste0(save_dir, "check_nox_ann_plant.csv")) }

# ozone season NOx
check_nox_oz_plant <- 
  plant_comparison %>% 
  filter(!(nox_oz_mass_r == nox_oz_mass_access)) %>% 
  mutate(diff_nox_oz_mass = nox_oz_mass_r - nox_oz_mass_access) %>% 
  filter(diff_nox_oz_mass > 1 | diff_nox_oz_mass < -1) %>% 
  select(plant_id, plant_id, nox_oz_mass_r, nox_oz_mass_access, nox_oz_source_r, 
         nox_oz_source_access)

if(nrow(check_nox_oz_plant) > 0) {
  write_csv(check_nox_oz_plant, paste0(save_dir, "check_nox_oz_plant.csv")) }

# check NOx sources 
# annual NOx source
check_nox_ann_source <- 
  plant_comparison %>% 
  filter(!(nox_source_r == nox_source_access)) %>% 
  select(plant_id, plant_id, nox_mass_r, nox_mass_access, nox_source_r, nox_source_access)

if(nrow(check_nox_ann_source) > 0) {
  write_csv(check_nox_ann_source, paste0(save_dir, "check_nox_ann_source.csv")) }

# ozone NOx source 
check_nox_oz_source <- 
  plant_comparison %>% 
  filter(!(nox_oz_source_r == nox_oz_source_access)) %>% 
  select(plant_id, plant_id, nox_oz_mass_r, nox_oz_mass_access, nox_oz_source_r, nox_oz_source_access)

if(nrow(check_nox_oz_source) > 0) {
  write_csv(check_nox_oz_source, paste0(save_dir, "check_nox_oz_source.csv")) }

## Check SO2 emissions -------------
# SO2 emissions mass 
check_total_so2_mass <- 
  plant_comparison %>% 
  summarize(sum_so2_mass_r = sum(so2_mass_r, na.rm = TRUE), 
            sum_so2_mass_access = sum(so2_mass_access, na.rm = TRUE)) %>% 
  select(contains("sum")) %>% 
  mutate(diff_so2 = sum_so2_mass_r - sum_so2_mass_access)

if(!(check_total_so2_mass$diff_so2 == 0) | !(check_total_so2_mass$diff_so2 == 0)) {
  write_csv(check_total_so2_mass %>% select(sum_so2_mass_r, sum_so2_mass_access, diff_so2), 
            paste0(save_dir, "check_total_so2_mass.csv")) }

# plant level SO2 emissions
check_so2_plant <- 
  plant_comparison %>% 
  filter(!(so2_mass_r == so2_mass_access)) %>% 
  mutate(diff_so2_mass = so2_mass_r - so2_mass_access) %>% 
  filter(diff_so2_mass > 1 | diff_so2_mass < -1) %>% 
  select(plant_id, plant_id, so2_mass_r, so2_mass_access, diff_so2_mass, 
         so2_source_r, so2_source_access)

if(nrow(check_so2_plant) > 0) {
  write_csv(check_so2_plant, paste0(save_dir, "check_so2_plant.csv")) }

# check SO2 source
check_so2_source <- 
  plant_comparison %>% 
  filter(!(so2_source_r == so2_source_access)) %>% 
  select(plant_id, plant_id, so2_mass_r, so2_mass_access, so2_source_r, so2_source_access)

if(nrow(check_so2_source) > 0) {
  write_csv(check_so2_source, paste0(save_dir, "check_so2_source.csv")) }

## Check CO2 emissions ----------
# CO2 emissions mass 
check_total_co2_mass <- 
  plant_comparison %>% 
  summarize(sum_co2_mass_r = sum(co2_mass_r, na.rm = TRUE), 
            sum_co2_mass_access = sum(co2_mass_access, na.rm = TRUE)) %>% 
  select(contains("sum")) %>% 
  mutate(diff_co2 = sum_co2_mass_r - sum_co2_mass_access)

if(!(check_total_co2_mass$diff_co2 == 0) | !(check_total_co2_mass$diff_co2 == 0)) {
  write_csv(check_total_co2_mass %>% select(sum_co2_mass_r, sum_co2_mass_access, diff_co2), 
            paste0(save_dir, "check_total_co2_mass.csv")) }

# plant level CO2 emissions
check_co2_plant <- 
  plant_comparison %>% 
  filter(!(co2_mass_r == co2_mass_access)) %>% 
  mutate(diff_co2_mass = co2_mass_r - co2_mass_access) %>% 
  filter(diff_co2_mass > 1 | diff_co2_mass < -1) %>% 
  select(plant_id, plant_id, co2_mass_r, co2_mass_access, diff_co2_mass, 
         co2_source_r, co2_source_access)

if(nrow(check_co2_plant) > 0) {
  write_csv(check_co2_plant, paste0(save_dir, "check_co2_plant.csv")) }

# check CO2 source
check_co2_source <- 
  plant_comparison %>% 
  filter(!(co2_source_r == co2_source_access)) %>% 
  select(plant_id, plant_id, co2_mass_r, co2_mass_access, co2_source_r, co2_source_access)

if(nrow(check_co2_source) > 0) {
  write_csv(check_co2_source, paste0(save_dir, "check_co2_source.csv")) }
