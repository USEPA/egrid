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
save_dir <- here("data/outputs/qa/plant_file_differences", Sys.Date(),"")
# create it if it doesn't already exist
if(!dir.exists(save_dir)){ dir.create(save_dir,recursive = T)}
do.call(file.remove, list(dir(save_dir, full.names = TRUE)))

## 1. R Version --------------
# load plant file R
plant_r <- read_rds("data/outputs/plant_file.RDS")

write.csv(plant_r, paste0(save_dir,"/plant_file_qa.csv"))
# add "_r" after each variable to easily identify dataset 
colnames(plant_r) <- paste0(colnames(plant_r), "_r")

plant_r <- plant_r %>% rename("plant_id" = "plant_id_r")

## 2. Access Version ---------------------
#excel_sheets("data/raw_data/eGRID_2021.xlsx")
#excel_sheets("data/outputs/qa/access_unit_file.xlsx")

plant_access <- read_excel("data/outputs/qa/eGRID2021 Plant file 9_19.xlsx", #sheet = "PLNT21", 
                               #skip = 1, 
                               guess_max = 4000) %>% janitor::clean_names() %>% 
  mutate(year = 2021, 
         plant_code = as.character(plant_code), 
         numunt = as.integer(numunt), 
         numgen = as.integer(numgen), 
         utility_id = as.character(utility_id), 
         transmission_or_distribution_system_owner_id = as.character(transmission_or_distribution_system_owner_id), 
         sequence_number = as.integer(sequence_number)) %>%
  rename("seqplt_access" = "sequence_number", # need to add sequnt to plant_r create script
         "year_access" = "year", 
         "plant_id" = "plant_code",
         "plant_name_access" = "plant_name", 
         "plant_state_access" = "state", 
         #"plant_id" = "plantid", 
         "system_owner_access" = "transmission_or_distribution_system_owner",
         "system_owner_id_access" = "transmission_or_distribution_system_owner_id",
         "utility_name_access" = "utility_name",
         "utility_id_access" = "utility_id",
         "sector_name_access" = "sector",
         "ba_name_access" = "balancing_authority_name",
         "ba_code_access" = "balancing_authority_code",
         "nerc_access" = "nerc_region",
         "egrid_subregion_access" = "subrgn",
         "egrid_subregion_name_access" = "srname",
         "isorto_access" = "isorto",
         "fips_state_code_access" = "fipsst",
         "fips_county_code_access" = "fipscnty",
         "county_access" = "county",
         "lat_access" = "latitude",
         "lon_access" = "longitude",
         "camd_flag_access" = "camdflag",
         "num_units_access" = "numunt", # REMOVE num_units_op
         "num_generators_access"= "numgen",   # REMOVE num_gen_op
         "primary_fuel_type_access" = "plprmfl",
         "primary_fuel_category_access"="plfuelct",
         "coal_flag_access"="coalflag",
         #"combust_access"
         "capfac_access" = "capfac",
         "nameplate_capacity_access" = "namepcap",
         "nonbaseload_access"="non_baseload",
         "biomass_adj_flag_access"="rmbmflag",
         "chp_flag_access"="chpflag",
         "useful_thermal_output_access"="usethrmo",
         "power_heat_ratio_access"="pwrtoht", # figure out difference with power_heat_ratio
         "elec_allocation_access"="elcalloc",
         "ps_flag_access"="psflag",
         "combust_heat_input_access"="combust_hti",
         "combust_heat_input_oz_access"="combust_htioz",
         "heat_input_access"="plhtian",
         "heat_input_oz_access"="plhtioz",
         "generation_ann_access"="plngenan", # "ann_gen_access" gets created for denominator when creating fuel %
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
         "nox_out_emission_rate_oz_access"="plnoxrto",
         "so2_out_emission_rate_access"="plso2rta",
         "co2_out_emission_rate_access"="plco2rta",
         "ch4_out_emission_rate_access"="plch4rta",
         "n2o_out_emission_rate_access"="pln2orta",
         "co2e_out_emission_rate_access"="plc2erta",
         "hg_out_emission_rate_access"="plhgrta",
         "nox_in_emission_rate_access"="plnoxra",
         "nox_in_emission_rate_oz_access"="plnoxro",
         "so2_in_emission_rate_access"="plso2ra",
         "co2_in_emission_rate_access"="plco2ra",
         "ch4_in_emission_rate_access"="plch4ra",
         "n2o_in_emission_rate_access"="pln2ora",
         "co2e_in_emission_rate_access"= "plc2era",
         "hg_in_emission_rate_access"="plhgra",
         "nox_combust_out_emission_rate_access"="plnoxcrt",
         "nox_combust_out_emission_rate_oz_access"="plnoxcro",
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
         "unadj_combust_heat_input_access"="un_combust_hti",
         "unadj_combust_heat_input_oz_access"="un_combust_htioz",
         "unadj_heat_input_access"="unhti",
         "unadj_heat_input_oz_access"="unhtioz",
         "unadj_nox_source_access"="unnox_source",
         "unadj_nox_oz_source_access"="unnoxoz_source",
         "unadj_so2_source_access"="unso2_source",
         "unadj_co2_source_access"="unco2_source",
         "unadj_ch4_source_access"="unch4_source",
         "unadj_n2o_source_access"="unn2o_source",
         "unadj_hg_source_access"="unhg_source",
         "unadj_heat_input_source_access"="unhti_source",
         "unadj_heat_input_oz_source_access"="unhtioz_source",
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
  read_csv("data/outputs/qa/generator_file_differences/plant_gen_difference_ids.csv") %>% 
  select(plant_id, source_diff) %>% distinct()

unit_diffs <- 
  read_csv("data/outputs/qa/unit_file_differences/plant_unit_difference_ids.csv") %>% 
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
stopifnot(length(access_cols[!access_cols %in% r_cols])==0)

# r contains more columns
# Difference Note - access labels the adjusted values as unadjusted and does not keep the true unadjusted values
# we assign 
other_cols <- c("unadj_nox_source", "unadj_nox_oz_source", "unadj_co2_source", "unadj_so2_source",
                "unadj_heat_input_source" , "unadj_heat_input_oz_source")
# missing n2o and ch4 since those come from EIA adjustment factors not the unit file
# Additional variables in R 
other_cols <- c(other_cols, "ann_gen") # used as denominator of perc_ann_gen columns - weird rounding 
# other_cols <- c(other_cols, "ch4_ef", "n2o_ef") # removed from plant file
other_cols <- c(other_cols, "fuel_code")  # fuel code - ??
other_cols <- c(other_cols, "combust_flag") # combust_flag - ??
other_cols <- c(other_cols, "total_fuel_consumption_mmbtu") # used to calculate uto  - Remove in plant_file_create
other_cols <- c(other_cols, "co2_non_biomass") # co2_non_biomass - used to adjust co2_mass in a weird way not done for others
other_cols <- c(other_cols, "nominal_heat_rate") # set as power_heat_ratio when combust_flag =1 or =0.5 for CHP facilities        

other_cols[!other_cols %in% r_cols]
# check that all additional columns have been accounted for
stopifnot(length(r_cols[!r_cols %in% access_cols & !r_cols %in% other_cols])==0)

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
plant_compare <- function(x, data = plant_comparison, outdir = save_dir){
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

# c. Summarise objects
obj_tab <- data.frame("object" = objects())
obj_tab <- obj_tab %>% filter(startsWith(object, "check"))
obj_tab$diffs <- lapply(obj_tab$object, FUN=function(x){nrow(get(x))}) %>% as.numeric()
#obj_tab$n_plants <- lapply(obj_tab$object, FUN=function(x){nrow(unique(get(x)[,"plant_id"]))})

helperfunc <- function(x){paste(unique(get(x)[,"plant_id"]))}
obj_tab$plants <- lapply(obj_tab$object, FUN=helperfunc) %>% as.character()
obj_tab$plants <- ifelse(nchar(obj_tab$plants)> 100, substr(obj_tab$plants,1,100), obj_tab$plants)

library(writexl)
write_xlsx(obj_tab,paste0( save_dir, "/differences_summary.xlsx"))

## 6. Explain Differences ----------
# a. ann_gen by fuel type vars [16 vars] - ? -------------------------
    # ann_gen_biomass (354), ann_gen_coal (302), ann_gen_combust (1243), 
    # ann_gen_gas (1140), ann_gen_geothermal (13), ann_gen_hydro (308), 
    # ann_gen_non_combust (587), ann_gen_non_renew (1311), ann_gen_nuclear (9),
    # ann_gen_oil (999), ann_gen_other (187), ann_gen_other_ff (164), 
    # ann_gen_renew (919) ann_gen_renew_nonhydro (618), ann_gen_solar (232), 
    # ann_gen_wind (46)

ann_gen_err <- check_ann_gen_biomass %>% full_join(check_ann_gen_coal) %>%
  full_join(check_ann_gen_combust) %>% full_join(check_ann_gen_gas) %>%
  full_join(check_ann_gen_geothermal) %>% full_join(check_ann_gen_hydro) %>%
  full_join(check_ann_gen_non_combust) %>% full_join(check_ann_gen_non_renew) %>%
  full_join(check_ann_gen_nuclear) %>% full_join(check_ann_gen_oil) %>%
  full_join(check_ann_gen_other) %>% full_join(check_ann_gen_other_ff) %>%
  full_join(check_ann_gen_renew) %>% full_join(check_ann_gen_renew_nonhydro) %>%
  full_join(check_ann_gen_solar) %>% full_join(check_ann_gen_wind) 
  

# b. ba_id & ba_name [2 vars] - 30 differences - Explained by EIA-860? --------
    # mostly missing in R
    ba <- check_ba_id %>% full_join(check_ba_name)
    # comes from EIA-860
# c. biomass_adj_flag - Resolved -----------------
    # updated plant_file_create to change 1 to "Yes"
# d. capfac - 58 differences - ? ---------------
    # values very close
    which(abs(check_capfac$capfac_access - check_capfac$capfac_r) > 1) # none
    which(abs(check_capfac$capfac_access - check_capfac$capfac_r) > .1) # 1
    which(abs(check_capfac$capfac_access - check_capfac$capfac_r) > .01) # 3
    which(abs(check_capfac$capfac_access - check_capfac$capfac_r) > .001) # 3
    which(abs(check_capfac$capfac_access - check_capfac$capfac_r) > .0001) # 4
    which(abs(check_capfac$capfac_access - check_capfac$capfac_r) > .00001) # 58
    # more differences appear in generation_ann & nameplate_capacity
    check_capfac$plant_id %in% check_generation_ann$plant_id %>% all
    check_capfac$plant_id %in% check_nameplate_capacity$plant_id %>% all
    # but neither account for all the plants...
    #  so look at differences in nameplate_capacity? 
# e. CH4 variables [4 vars] - Resolved --------------
    # ch4_combust_out_emission_rate (44) - ?
    # ch4_in_emission_rate (16) - ?
    # ch4_mass (1) - plant_id 58186 - same as other mass variables?
    # ch4_out_emission_rate (44) - same as with combustion
# f. CHP variables [7 vars] - ? ---------------
    # chp_ch4 (1) - plant_id 58186 - same as with N2O - Resolved
    # chp_co2 (9)
    # chp_combust_heat_input (747)
    # chp_combust_heat_input_oz (721)
    # chp_n20 (1) -  plant_id 58186 - same as with CH4 - Resolved
    # chp_nox (14) - Resolved
    # chp_so2 (1) - plant_id 57557 - different plant - Resolved
# g. CO2 variables [4 vars] - ? ---------
    # co2_combust_out_emission_rate (15)
    # co2_in_emission_rate (11)
    # co2_mass (9)
    # co2_out_emission_rate (8)
co2 <- check_co2_combust_out_emission_rate %>% full_join(check_co2_in_emission_rate)
co2 <- co2 %>% full_join(check_co2_mass)
co2 <- co2 %>% full_join(check_co2_out_emission_rate)

plant_comparison %>% select(co2_mass_r, co2_mass_access)
# what makes co2 different from other chemicals? No others have differences anymore..

# h. CO2 Equivalent variables [4 vars] - ? ---------------------
    # co2_equiv_combust_out_emission_rate (2963)
    # co2_equiv_in_emission_rate (897)
    # co2_equiv_out_emission_rate (3001)
    # co2_equivalent (3071)
# no differences in ch4 variables or n2o variables suggests they should all coincide with co2 vars but they don't
# differences increased from 2932, 888, 2968, & 3037

# i. coal_flag - 4 differences - Explained by EIA-923? --------------
    # No instead of Yes 
# none are a part of the primary fuel type recoding of EIA-923
check_coal_flag$plant_id %in% OTH_OG_recode$plant_id

# j. combust_heat_input (745) & combust_heat_input_oz (718) [2 vars] - ? -------------
    chti <- check_combust_heat_input %>% full_join(check_combust_heat_input_oz)
    # same 745 plants
    View(plant_comparison %>% filter(plant_id %in% chti$plant_id))
    # different list of combustables??
    # only 10 & 9 differences in the unadjusted values
    
# k. county - 22 differences - Resolved ------------------
    # alaska renaming
    
    
# l. elec_allocation - 60 differences - ? -----------------------
    which(abs(check_elec_allocation$elec_allocation_r - check_elec_allocation$elec_allocation_access) > .00001)
    # rounded to 6 digits. Differences start at 5 digits
    # calculated with generation_ann & uto
# m. generation_ann (299) & generation_oz (774) [2 vars] - ? -----------------

ids <- check_generation_ann$plant_id[which(abs(check_generation_ann$generation_ann_access - check_generation_ann$generation_ann_r) > 1)]
# of 299 only 12 are larger than 1
# should come straight fron generator file
x <- generator_file %>% filter(plant_id %in% check_generation_ann$plant_id) %>% group_by(plant_id, generator_id) %>% 
  summarise(ann_gen = sum(generation_ann, na.rm = T))
y <- generator_file %>% filter(plant_id %in% check_generation_ann$plant_id) %>% group_by(plant_id) %>% 
  summarise(ann_gen = sum(generation_ann, na.rm = T)) %>% mutate(plant_id = as.numeric(plant_id))

z <- y %>% full_join(check_generation_ann)
View(z %>% filter(plant_id %in% ids))
View(x %>% filter(plant_id %in% ids))

z1 <- generator_file %>% mutate(plant_id = as.numeric(plant_id)) %>% filter(plant_id %in% ids) %>% left_join(check_generation_ann)
# n. heat_input (135) & heat_input_oz (125) [2 vars] - ? -------------

    # investigate
# o. lat & lon [2 vars] - 10 differences - Explained by EIA-860? ----------------
    # they start in the 2nd decimal place

latlon <- check_lat %>% full_join( check_lon)
# p. N2O variables [4 vars] - exact same counts as CH4 - Resolved ---------------
    # n2o_combust_out_emission_rate (44)
    # n2o_in_emission_rate (16)
    # n2o_mass (1) - plant_id 58186
    # n2o_out_emission_rate (44)
# q. nameplate_capacity - 537 differences - ? --------------
    # never gets rounded
    which(abs(check_nameplate_capacity$nameplate_capacity_r - check_nameplate_capacity$nameplate_capacity_access) > 1) # 1
    which(abs(check_nameplate_capacity$nameplate_capacity_r - check_nameplate_capacity$nameplate_capacity_access) > 0.1) # 1
    which(abs(check_nameplate_capacity$nameplate_capacity_r - check_nameplate_capacity$nameplate_capacity_access) > 0.01) # 1
    which(abs(check_nameplate_capacity$nameplate_capacity_r - check_nameplate_capacity$nameplate_capacity_access) > 0.001) # 1
    which(abs(check_nameplate_capacity$nameplate_capacity_r - check_nameplate_capacity$nameplate_capacity_access) > 0.0001) # 1
    which(abs(check_nameplate_capacity$nameplate_capacity_r - check_nameplate_capacity$nameplate_capacity_access) > 0.00001) # 1
    which(abs(check_nameplate_capacity$nameplate_capacity_r - check_nameplate_capacity$nameplate_capacity_access) > 0.000001) # 1
    # only 1 difference at 6 decimal places
    check_nameplate_capacity[107,] # plant 3612
    # comes from generator file?
    x <- generator_file %>% filter(plant_id == 3612)
    sum(x$nameplate_capacity) # same as R
# r. NERC variables [3 vars]
    # nerc (11) - depends on EIA 860
    # nerc_subregion (3)
    # nerc_subregion_name (3)
nerc <- check_nerc %>% full_join(check_nerc_subregion)
nerc <- nerc %>% full_join(check_nerc_subregion_name)
    # nerc_subregion depends on numerous crosswalks
    # could be an ordering issue? check 3 plants in each crosswalk
    # BAX requires knowing the balancing authority
    nerc_ba <- plant_comparison %>% select(plant_id, ba_id_access, ba_id_r, ba_name_access, ba_name_r) %>% filter(plant_id %in% check_nerc_subregion$plant_id)
    # only 1 matched between r & access - 8066 which has ba_id PACE
    x <- BAX %>% filter(ba_id %in% nerc_ba$ba_id_access | ba_id %in% nerc_ba$ba_id_r)# according to BAX it should be NWPP but R selects RMPA
    # resolved this one case by adding triming of ba_name in BAX.
    # remaining differences from differing ba_id/ba_name? No since access nerc_subregions dont appear in BAX for any ba_id/name
    View(BAX %>% filter(nerc_subregion_new %in% check_nerc_subregion$nerc_subregion_access))
    View(BAtransX %>% filter(nerc_subregion_new %in% check_nerc_subregion$nerc_subregion_access))
    # they do appear in BAtransX with the correct ba_id, check the system_owner
    
    
# s. nonbaseload - 1425 differences - ? -------------------------
    # does not get rounded
    which(abs(check_nonbaseload$nonbaseload_access - check_nonbaseload$nonbaseload_r) > 0.1) # none
    which(abs(check_nonbaseload$nonbaseload_access - check_nonbaseload$nonbaseload_r) > 0.01) # 2
    length(which(abs(check_nonbaseload$nonbaseload_access - check_nonbaseload$nonbaseload_r) > 0.00001)) # 17
    length(which(abs(check_nonbaseload$nonbaseload_access - check_nonbaseload$nonbaseload_r) > 0.000001)) # 50
    # only 17 differences at the 5th decimal place and 50 at the 6th
    # based on capfac
    
# t. NOX variables [4 vars] - Resolved -----------------------
    # nox_combust_out_emission_rate (308)
    # nox_in_emission_rate (29)
    # nox_mass (1425) - plant_id 58186
    # nox_out_emission_rate (308)
# u. num_gen & num_units [2 vars] - 1 difference in each, caused by different plants ---------
    
View(generator_file %>% filter(plant_id %in% check_num_gen$plant_id))
# only 6 rows - R says 6, Access says 10
# gen id #1-#7 missing #4
which(generator_file %>% select(plant_id, generator_id) %>% duplicated)
# generator file has no duplicates

View(unit_file %>% filter(plant_id %in% check_num_units$plant_id))
# 10 rows for 6 unit_id, R says 6, access says 10
unit_file$plant_id[which(unit_file %>% select(plant_id, unit_id) %>% duplicated())]
# access seems to treat unit file as unique and just counts rows?
# only duplicates in the unit file are for this one plant

# v. perc_ann_gen variables [16 vars] ---------------
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
# y. primary_fuel_type (58) & primary_fuel_category (53) [2 vars] -----------
  x <- check_primary_fuel_type %>% full_join(check_primary_fuel_category)
  # same plants
  y <- unit_file %>% filter(plant_id %in% x$plant_id) %>% group_by(plant_id, primary_fuel_type) %>%
    summarise(hti = sum(heat_input, na.rm=T))
  y2 <- generator_file %>% filter(plant_id %in% x$plant_id) %>% group_by(plant_id,fuel_code) %>%
    summarise(npc = sum(nameplate_capacity, na.rm=T)) %>% mutate(plant_id = as.numeric(plant_id))
  z <- y %>% full_join(y2) %>% filter(fuel_code==primary_fuel_type) %>% unique
  # fixed bug that lowered the differences to 51 & 46 respectively
  # some of these could be from EIA-923 being used to pick between duplicates. 
  # however, many should be selected just from unit/gen files which should be identical
  # results of Z suggest r values are correct
  
# z. sector - plant_id 55481 - Explained by coming from EIA-860?---------------
# aa. seq - produced based on sort order... ------------
# ab. SO2 variables [4 vars] - 9 plants - Resolved ----------------
    # so2_combust_out_emission_rate (9) - Explained by setting negative values to 0, potential rounding diffs and poetenial different combustion fuels
    # so2_in_emission_rate (3) - Explained by setting negative values to 0 & a potential rounding diff
    # so2_mass (7) - Explained by setting negative values to 0
    # so2_out_emission_rate (6) - Explained by setting negative values to 0
#so2 <- check_so2_combust_out_emission_rate %>% full_join(check_so2_in_emission_rate)
#so2 <- so2 %>% full_join(check_so2_out_emission_rate)
#so2 <- so2 %>% full_join(check_so2_mass)

# ac. system_owner (1) & system_owner_id (12) [2 vars] - Explained by coming from EIA-860 ------
# ad. unadjusted mass variables [3 vars] Explained by rounding or coming from EIA-923 -------------
    # unadj_ch4_mass (30) - Explained by coming from EIA-923
    # unadj_n2o_mass (17) - Explained by coming from EIA-923
    # unadj_so2_mass  (47) - Explained by rounding differences
mass_diff <- check_unadj_ch4_mass %>% full_join(check_unadj_n2o_mass)
# all 17 n2o differences coincide with ch4 differences
mass_diff <- mass_diff %>% full_join(check_unadj_so2_mass)
# all 47 so2 diffrences DO NOT coincide with the n2o & ch4 differences
x <- unit_file %>% filter(plant_id %in% check_unadj_so2_mass$plant_id) %>% 
  select(plant_id, so2_mass) %>% group_by(plant_id) %>% summarise(so2 = sum(so2_mass, na.rm = T))
y <- x %>% full_join(check_unadj_so2_mass)
# all differences come from rounding.. r version is rounding 5 to the nearest even integer instead of up
# updated plant_file_create to use round_half_up from janitor package
# this created even more differences (94) so I reverted the change

# ae. unadjusted source variables [7 vars] - Explained by comparing to adjusted values (except HG) ------------
    # unadj_co2_source (2295)
      # compare to co2_source
    x <- plant_comparison %>% filter(co2_source_r != unadj_co2_source_access)
    if(nrow(x) ==0){ rm(check_unadj_co2_source, x) }
    # unadj_heat_input_source (9368)
    x <- plant_comparison %>% filter(heat_input_source_r != unadj_heat_input_source_access)
    if(nrow(x) ==0){ rm(check_unadj_heat_input_source, x)}
    # unadj_heat_input_oz_source (9404)
    x <- plant_comparison %>% filter(heat_input_oz_source_r != unadj_heat_input_oz_source_access)
    if(nrow(x) ==0){  rm(check_unadj_heat_input_oz_source, x) }
    # unadj_hg_source (185)
    # there is no hg_source_r, only the unadjusted variable
    plant_comparison$unadj_hg_source_r %>% unique
    plant_comparison$unadj_hg_source_access %>% unique
    unit_file$hg_source %>% unique
    # unadj_nox_oz_source (2189)
    x <- plant_comparison %>% filter(nox_oz_source_r != unadj_nox_oz_source_access)
    if(nrow(x) ==0){ rm(check_unadj_nox_oz_source, x)}
    # unadj_nox_source (2189)
    x <- plant_comparison %>% filter(nox_source_r != unadj_nox_source_access)
    if(nrow(x) ==0){ rm(check_unadj_nox_source, x)}
    # unadj_so2_source (2149)
    x <- plant_comparison %>% filter(so2_source_r != unadj_so2_source_access)
    if(nrow(x) ==0){ rm(check_unadj_so2_source, x)}
    
# af. unadjusted heat input variables [2 vars] - 10 plants unexplained -------------
    # unadj_combust_heat_input (10)
    # unadj_combust_heat_input_oz (9)
chi <- check_unadj_combust_heat_input %>% full_join(check_unadj_combust_heat_input_oz)
# same 10 plants
# uses the same unit_file, so could be difference in list of combustion fuels?
# that would make sense since there are no differences in total heat_input and heat_input_oz


# ag. utility_id & utility_name [2 vars] - 6 differences - Explained by coming from EIA-860 -----------
# ah. uto - 292 differences - Explained by coming from EIA-923 ------------
    # no rounding was done to uto in either dataset
    format(check_uto$uto_r, scientific = T)
    format(check_uto$uto_access, scientific = T)
    x <- check_uto %>% filter(abs(uto_r - uto_access) > .001)
    # only 15 differences when rounded to the 3rd decimal place
