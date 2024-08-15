## -------------------------------
##
## Plant file QA 
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
library(here)
library(readr)
library(readxl)
library(stringr)

# set directory for saving files 
save_dir <- here("data/outputs/qa/plant_file_differences/")
# create it if it doesn't already exist
if(!dir.exists(save_dir)){ dir.create(save_dir,recursive = T)}

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
         "perc_ann_gen_non_combust_access"="plcnpr") 

## 3. Compare Columns -------------------------
# remove suffix from column names
r_cols <- lapply(colnames(plant_r), FUN=gsub, pattern="_r$", replacement="") %>% unlist()
access_cols <- lapply(colnames(plant_access), FUN=gsub, pattern="_access$", replacement="") %>% unlist()
# Check that all access columns appear in the r columns
stopifnot(length(access_cols[!access_cols %in% r_cols])==0)

# r contains more columns
# Difference Note - access does not create source columns that are unadj
other_cols <- c("ch4_source", "n2o_source", "nox_source", "nox_oz_source", "co2_source", "so2_source",
                "heat_input_source" , "heat_input_oz_source")

# Additional variables in R 
other_cols <- c(other_cols, "ann_gen") # used as denominator of perc_ann_gen columns
other_cols <- c(other_cols, "ch4_ef", "n2o_ef") # retains emissions factor - Remove in plant_file_create
other_cols <- c(other_cols, "fuel_code")  # fuel code - ??
other_cols <- c(other_cols, "combust_flag") # combust_flag - ??
other_cols <- c(other_cols, "total_fuel_consumption_mmbtu") # used to calculate uto  - Remove in plant_file_create
other_cols <- c(other_cols, "co2_non_biomass") # co2_non_biomass - ??
other_cols <- c(other_cols, "nominal_heat_rate") # set as power_heat_ratio when combust_flag =1 or =0.5 for CHP facilities        

# check that all additional columns have been accounted for
stopifnot(length(r_cols[!r_cols %in% access_cols & !r_cols %in% other_cols])==0)

## 4. Combine ------------------------------
# combine the two datasets
plant_comparison <- 
  plant_r %>%
  full_join(plant_access, by = c("plant_id" = "plant_id")) 
## 5. Compare Values ------------------
# a1. plant_id in R that are NOT in Access dataset ---------
check_diff_plant_r <- 
  plant_r %>% 
  filter(!is.na(plant_id)) %>%
  anti_join(plant_access, by = c("plant_id" = "plant_id"))
  

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
plant_compare <- function(x, data = plant_comparison, outdir = save_dir){
  r_name <- as.name(paste0(x,"_r"))
  access_name <- as.name(paste0(x,"_access"))
  comp <- data %>% 
    filter(!(!!r_name == !!access_name)) %>% 
    select(plant_id, !!r_name, !!access_name) %>% distinct()
  if(nrow(comp) > 0) {
    write_csv(comp, paste0(outdir, "check_", x, ".csv")) }
  return(comp)
}

# b. Loop over list of columns ----------
#  - exclude plant_id and other_cols by looking at access_cols
cols <- access_cols[access_cols!="plant_id"]

for(i in cols){
  x <- plant_compare(i)
  if(nrow(x)> 0){
    assign(paste0("check_", i), x)
    rm(x)
  }else{
    rm(x)
  }
}



## 6. Explain Difference ----------
# a. ann_gen by fuel type vars [16 vars]- differences explained by new raw EIA data?
    # ann_gen_biomass (354), ann_gen_coal (302), ann_gen_combust (1243), 
    # ann_gen_gas (1140), ann_gen_geothermal (13), ann_gen_hydro (308), 
    # ann_gen_non_combust (587), ann_gen_non_renew (1311), ann_gen_nuclear (9),
    # ann_gen_oil (999), ann_gen_other (187), ann_gen_other_ff (164), 
    # ann_gen_renew (919) ann_gen_renew_nonhydro (618), ann_gen_solar (232), 
    # ann_gen_wind
# b. ba_id & ba_name [2 vars] - 30 differences, mostly missing in R
    # investigate
# c. biomass_adj_flag - updated plant_file_create to change 1 to "Yes"
    # fixed?
# d. capfac - 58 differences, very close so look at differences in nameplate_capacity?
    # investigate
# e. CH4 variables [4 vars]
    # ch4_combust_out_emission_rate (44) - ?
    # ch4_in_emission_rate (16) - ?
    # ch4_mass (1) - plant_id 58186 - same as other mass variables?
    # ch4_out_emission_rate (44) - same as with combustion
# f. CHP variables [7 vars]- from different EIA data?
    # chp_ch4 (1) - plant_id 58186 - same as with N2O
    # chp_co2 (9)
    # chp_combust_heat_input (747)
    # chp_combust_heat_input_oz (721)
    # chp_n20 (1) -  plant_id 58186 - same as with CH4
    # chp_nox (14)
    # chp_so2 (1) - plant_id 57557 - different plant
# g. CO2 variables [4 vars]
    # co2_combust_out_emission_rate (15)
    # co2_in_emission_rate (11)
    # co2_mass (9)
    # co2_out_emission_rate (8)
# h. CO2 Equivalent variables [4 vars]
    # co2_equiv_combust_out_emission_rate (2932)
    # co2_equiv_in_emission_rate (888)
    # co2_equiv_out_emission_rate (2968)
    # co2_equivalent (3037)
# i. coal_flag - 4 differences No instead of Yes
    # investigate
# j. combust_heat_input (745) & combust_heat_input_oz (718) [2 vars]
    # investigate
# k. county - 22 differences seem to be the alaska renaming
    # investigate the static table
# l. elec_allocation - 60 differences
    # investigate
# m. generation_ann (299) & generation_oz (774) [2 vars]
    # investigate
# n. heat_input (135) & heat_input_oz (125) [2 vars]
    # investigate
# o. lat & lon [2 vars] - 10 differences - they start in the 2nd decimal place
    # investigate
# p. N2O variables [4 vars] - exact same counts as CH4
    # n2o_combust_out_emission_rate (44)
    # n2o_in_emission_rate (16)
    # n2o_mass (1) - plant_id 58186
    # n2o_out_emission_rate (44)
# q. nameplate_capacity - 537 differences
    # investigate
# r. NERC variables [3 vars]
    # nerc (11)
    # nerc_subregion (3)
    # nerc_subregion_name (3)
# s. nonbaseload - 1425 differences
    # investigate
# t. NOX variables [4 vars] 
    # nox_combust_out_emission_rate (308)
    # nox_in_emission_rate (29)
    # nox_mass (1425) - plant_id 58186
    # nox_out_emission_rate (308)
# u. num_gen & num_units [2 vars] - 1 difference in each, caused by different plants
    # investigate
# v. perc_ann_gen variables [16 vars] 
    # perc_ann_gen_biomass (648), perc_ann_gen_coal (313), perc_ann_gen_combust (3586),
    # perc_ann_gen_gas (2189), perc_ann_gen_geothermal (59), perc_ann_gen_hydro (1352),
    # perc_ann_gen_non_combust (10512), perc_ann_gen_non_renew (3278), perc_ann_gen_nuclear (56),
    # perc_ann_gen_oil (1429), perc_ann_gen_other (218), perc_ann_gen_other_ff (167),
    # perc_ann_gen_renew (7735), perc_ann_gen_renew_nonhydro (6390), perc_ann_gen_solar (4443),
    # perc_ann_gen_wind (1259)
# w. power_heat_ratio - 906 differences
    # investigate
# x. power_to_heat - 751 differences
    # investigate
# y. primary_fuel_type (58) & primary_fuel_category (53) [2 vars]
    # investigate
# z. sector - plant_id 55481
# aa. seq - produced based on sort order...
# ab. SO2 variables [4 vars]
    # so2_combust_out_emission_rate (9)
    # so2_in_emission_rate (3)
    # so2_mass (7) 
    # so2_out_emission_rate (6)
# ac. system_owner (1) & system_owner_id (12) - [2 vars]
    # investigate
# ad. unadjusted mass variables [3 vars]
    # unadj_ch4_mass (30)
    # unadj_n2o_mass (17)
    # unadj_so2_mass  (47)

mass_diff <- check_unadj_ch4_mass %>% full_join(check_unadj_n2o_mass)
# all 17 n2o differences coincide with ch4 differences
mass_diff <- mass_diff %>% full_join(check_unadj_so2_mass)
# all 47 so2 diffrences DO NOT coincide with the n2o & ch4 differences
x <- unit_file %>% filter(plant_id %in% check_unadj_so2_mass$plant_id) %>% 
  select(plant_id, so2_mass) %>% group_by(plant_id) %>% summarise(so2 = sum(so2_mass, na.rm = T))
y <- x %>% full_join(check_unadj_so2_mass)
# all differences come from rounding.. r version is rounding 5 to the nearest even integer instead of up
# fixed plant_file_create to use round_half_up from janitor package

# ae. unadjusted source variables [7 vars]
    # unadj_co2_source (2295)
    # unadj_heat_input_source (9368)
    # unadj_heat_input_oz_source (9404)
    # unadj_hg_source (185)
    # unadj_nox_oz_source (2189)
    # unadj_nox_source (2189)
    # unadj_so2_source (2149)
# af. unadjusted heat input variables [2 vars]
    # unadj_combust_heat_input (10)
    # unadj_combust_heat_input_oz (9)
chi <- check_unadj_combust_heat_input %>% full_join(check_unadj_combust_heat_input_oz)
# same 10 plants
# uses the same unit_file, so could be difference in list of combustion fuels?
# that would make sense since there are no differences in total heat_input and heat_input_oz


# ag. utility_id & utility_name [2 vars] - 6 differences - explained by coming from EIA-860

# ah. uto - 292 differences
    # investigate



