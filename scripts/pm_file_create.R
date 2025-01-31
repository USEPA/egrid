## -------------------------------
##
## Name
## 
## Purpose: 
## 
## Purpose
##
## Additional notes
##
## Authors:  
##      Emma Russell, Abt Global
##
## -------------------------------

# Load Libraries ---------
library(dplyr)
library(readr)
library(readxl)

# Define eGRID year parameter ----------------

params <- list()
params$eGRID_year <- as.character(2023)
# if (exists("params")) {
#   if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
#     print("eGRID year parameter is already defined.") 
#   } else { # if params() is defined, but eGRID_year is not, define it here 
#     params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
#     params$eGRID_year <- as.character(params$eGRID_year) 
#   }
# } else { # if params() and eGRID_year are not defined, define them here
#   params <- list()
#   params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
#   params$eGRID_year <- as.character(params$eGRID_year)
# }

# Load necessary data --------------------

## EIA-923
if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) { 
  eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))
} else { 
  stop("eia_923_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

## NEI PM data
if(file.exists(glue::glue("data/raw_data/nei/{params$eGRID_year}/emis_sum_unit_25291.csv"))) { 
  pm_data_nei <- read_csv(glue::glue("data/raw_data/nei/{params$eGRID_year}/emis_sum_unit_25291.csv")) %>%
    janitor::clean_names()
} else { 
  stop("emis_sum_unit_25291.csv does not exist.")}
  # access and r both have 355919 observations

## NEI-EIA crosswalk
nei_eia_xwalk <- read_csv("data/static_tables/fac_conf_alt_id_25292.csv") %>% #, #col_types = "cccccccccccccccccccc") 
  janitor::clean_names()
  # both are 5310 observations

## eGRID production model data
unit_file <- read_xlsx(glue::glue("data/outputs/{params$eGRID_year}/unit_file_2021_access.xlsx")) %>%
  janitor::clean_names()
  # both have 24597 observations
  # both have 1819 missing htian values
#unit_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/unit_file.RDS"))

# Calculate PM data -------------
## Direct Match - NEI/EIA --------------
  # access queries 000-01b
  # match NEI (EIS codes) to eGRID (ORISPL codes)
  # using crosswalk (QAQPS)

# join pm data with matching crosswalk data (query 000)
# matching by facility and unit IDs
# left join keeps all pm records and any matching crosswalk records
# 3918 matching records from crosswalk
pm_unit_match <- # (matching 356369 observations)
  pm_data_nei %>%
  left_join(nei_eia_xwalk, by = join_by(eis_facility_id, eis_unit_id)) %>%
  select(oris_facility_code = left_side_of_alternate_id_to_become_oris_facility_code, oris_boiler_id = right_side_of_alternatve_id_to_become_oris_boiler_id, 
         pm25 = total_emissions, state = state.y,
         eis_facility_id, eis_unit_id, agency_unit_id)

# calculate sum of pm emissions for each facility ID and boiler ID combination (query 00)
 # 3906 observations of summed pm25
pm_sum_facility_boiler <-
  pm_unit_match %>%
  group_by(oris_facility_code, oris_boiler_id) %>%
  summarise(sum_pm25 = sum(pm25))

# match unit file units to pm25 sums (query 01)
# join by unit id and facility id
unit_pm_emissions <-
  unit_file %>%
  left_join(pm_sum_facility_boiler, by = join_by(unitid == oris_boiler_id, orispl == oris_facility_code)) %>%
  rename(pm25 = sum_pm25) %>%
  mutate(pm25_source = if_else(is.na(pm25), NA, "EPA/NEI"), eia_pm_control_efficiency = NA, botfirty = if_else(botfirty == "", NA, botfirty))
