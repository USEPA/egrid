## -------------------------------
##
## Generator file create 
## 
## Purpose: 
## 
## This file creates the generator file for eGRID. 
## This includes all operating generators for the specified eGRID data year
## 
## Authors:  
##      Sean Bock, Abt Global
##      Caroline Watson, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------


# Load libraries ----------

library(dplyr)
library(readr)
library(stringr)
library(glue)
library(readxl)

# check if parameters for eGRID data year need to be defined
# this is only necessary when running the script outside of egrid_master.qmd
# user will be prompted to input eGRID year in the console if params does not exist

if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.") 
  } else { # if params() is defined, but eGRID_year is not, define it here 
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year) 
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Load in necessary 923 and 860 files ----------

if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS"))) { # if file does not exist, stop code and print error
  eia_923 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS")) # read in all 923 files
} else { 
   stop("eia_923_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

if(file.exists(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS"))) { # if file does not exist, stop code and print error
  eia_860 <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS")) # read in all 860 files
} else { 
   stop("eia_860_clean.RDS does not exist. Run data_load_eia.R and data_clean_eia.R to obtain.")}

eia_923_gen <- eia_923$generator_data
eia_923_gen_fuel <- eia_923$generation_and_fuel_combined
eia_860_boiler <- eia_860$boiler_generator
eia_860_combined <- eia_860$combined %>% 
  select(plant_id, # keeping only necessary files for to streamline joins
         plant_name, 
         plant_state, 
         generator_id, 
         prime_mover, 
         status, 
         energy_source_1, 
         nameplate_capacity,
         planned_retirement_year,
         retirement_year,
         operating_year)

# Load crosswalks and static tables ------------

xwalk_fuel_codes <- # xwalk for specific changes made to certain generator fuel types
  read_csv("data/static_tables/og_oth_units_to_change_fuel_type.csv", 
           col_types = "cccccccc") %>% 
  select(plant_id, fuel_code) %>% distinct()

xwalk_eia_epa <- # xwalk for updating certain plants to EPA plant names and ids
  read_csv("data/static_tables/xwalk_oris_epa.csv", 
           col_types = "cccc") # all fields are characters

epa_plants_to_delete <- 
  read_csv("data/static_tables/epa_plants_to_delete.csv", 
           col_types = cols_only(`ORIS Code` = "c")) %>% 
  janitor::clean_names() %>% 
  rename("plant_id" = oris_code)

manual_corrections <- # manual corrections needed for generator file
  read_xlsx("data/static_tables/manual_corrections.xlsx", 
            sheet = "generator_file", 
            col_types = c("text", "text", "text", "text", "text"))

# Load EPA data to update plant names to EPA versions
if(file.exists(glue::glue("data/clean_data/epa/{params$eGRID_year}/epa_clean.RDS"))) { # if file does not exist, stop code and print error
  epa <- read_rds(glue::glue("data/clean_data/epa/{params$eGRID_year}/epa_clean.RDS")) %>% 
    select(plant_id, plant_name) %>% distinct()
} else { 
  stop("epa_clean.RDS does not exist. Run data_load_epa.R and data_clean_epa.R to obtain.")}

# Create lookup table for generator IDs with leading zeroes ------------
# some IDs in EIA-923 do not have leading zeroes, but should match to generators in EIA-860 that have leading zeroes
# we do this to match more generators between EIA-923 and EIA-860
# however, we want to maintain the generator IDs with leading zeroes once they are matched

plant_keep_leading_zeroes <- 
  manual_corrections %>% 
  filter(column_to_update == "keep_leading_zeroes") %>% # some plant IDs need to keep leading zeroes to avoid duplicates
  pull(plant_id)
  
eia_923_leading_zeroes <- 
  eia_923_gen %>% 
  filter(str_detect(generator_id, "^0+") & !(plant_id %in% plant_keep_leading_zeroes)) %>% 
  select(plant_id, generator_id) %>% 
  mutate(gen_id_clean = str_remove(generator_id, "^0+"), 
         id = paste0(plant_id, "_", gen_id_clean)) 

lookup_923_leading_zeroes <- with(eia_923_leading_zeroes, setNames(generator_id, id))
print(glue::glue("{length(lookup_923_leading_zeroes)} generator IDs have leading zeroes in EIA-923 Generator file. 
                 The leading zeroes are removed for matching purposes and replaced at the end of the script."))

eia_860_leading_zeroes <- 
  eia_860_combined %>% 
  filter(str_detect(generator_id, "^0+") & !(plant_id %in% plant_keep_leading_zeroes)) %>%  
  select(plant_id, generator_id) %>% 
  mutate(gen_id_clean = str_remove(generator_id, "^0+"),
         id = paste0(plant_id, "_", gen_id_clean))

lookup_860_leading_zeroes <- with(eia_860_leading_zeroes, setNames(generator_id, id))
print(glue::glue("{length(lookup_860_leading_zeroes)} generator IDs have leading zeroes in EIA-860 Combined file. 
                 The leading zeroes are removed for matching purposes and replaced at the end of the script."))


# Create modified dfs that will be used to calculate generation values ---------

gen_id_manual_corrections <- # update generator IDs
  manual_corrections %>% 
  filter(column_to_update == "generator_id" & is.na(prime_mover)) %>% 
  select(plant_id, generator_id, update)

gen_id_pm_corrections <- # update generator IDs for plant with duplicate prime movers
  manual_corrections %>% 
  filter(column_to_update == "generator_id" & !is.na(prime_mover)) %>% 
  select(plant_id, generator_id, prime_mover, update)

eia_923_gen_r <- 
  eia_923_gen %>% 
  left_join(gen_id_manual_corrections, by = c("plant_id", "generator_id")) %>% 
  left_join(gen_id_pm_corrections, by = c("plant_id", "generator_id", "prime_mover")) %>% 
  mutate(
    generator_id = if_else(!(plant_id %in% plant_keep_leading_zeroes), str_remove(generator_id, "^0+"), generator_id), # remove leading zeroes from generator IDs
    generator_id = if_else(!is.na(update.x), update.x, generator_id), 
    generator_id = if_else(!is.na(update.y), update.y, generator_id)) %>% # update generator IDs from manual_corrections)
  group_by(plant_id, generator_id, combined_heat_and_power_plant) %>% 
  summarize(across(contains("generation"), 
                   ~ sum(., na.rm = TRUE))) # sum generation for plants with duplicate prime movers

eia_923_gen_dups <- # check for duplicates in EIA-923 Generator File
  eia_923_gen_r %>% 
  group_by(plant_id, generator_id) %>% 
  mutate(n = n(),
         id = paste0(plant_id, "_", generator_id)) %>% 
  filter(n > 1, 
         combined_heat_and_power_plant == "Y") %>%  # default to generators with "Y" CHP plant flag
  ungroup()
  
eia_923_gen_r_2 <- 
  eia_923_gen_r %>% 
  filter(!(paste0(plant_id, "_", generator_id) %in% eia_923_gen_dups$id)) %>% # filter out duplicate plants 
  rbind(eia_923_gen_dups %>% select(-id, -n)) # add back in generators that were duplicated with correct non-duplicated row

eia_860_combined_r <- 
  eia_860_combined %>% 
  left_join(gen_id_manual_corrections, by = c("plant_id", "generator_id")) %>% 
  mutate(
    generator_id = if_else(!(plant_id %in% plant_keep_leading_zeroes), str_remove(generator_id, "^0+"), generator_id), # remove leading zeroes from generator IDs
    generator_id = if_else(!is.na(update), update, generator_id), # update generator IDs from manual_corrections
    retirement_year = if_else(is.na(retirement_year), planned_retirement_year, retirement_year)) %>% 
  select(-update)
    
eia_860_boiler_count <- # creating count of boilers for each generator
  eia_860_boiler %>% 
  group_by(plant_id, 
           generator_id) %>% 
  summarize(n_boilers = n()) %>% 
  ungroup()

# Determine generation ------------

## Generation from EIA-923 Generator file ------

ozone_months_gen <- 
                c("net_generation_may", # creating vector of ozone month generation columns for calculations
                  "net_generation_june", 
                  "net_generation_july", 
                  "net_generation_august",
                  "net_generation_september")

eia_gen_generation <-
  eia_860_combined_r %>% 
  left_join(eia_923_gen_r_2 %>% 
              select(plant_id, generator_id, starts_with("net"), combined_heat_and_power_plant), # keeping only necessary columns
            by = c("plant_id", "generator_id")) %>% 
  mutate(generation_oz = rowSums(pick(all_of(ozone_months_gen)), na.rm = TRUE),
         gen_data_source = if_else(is.na(net_generation_year_to_date), NA_character_, "EIA-923 Generator File"),
         generation_ann = net_generation_year_to_date)

# check how many generators are missing generation values
missing_gen_data <- 
  eia_gen_generation %>% 
  filter(is.na(gen_data_source))

print(glue::glue("{nrow(eia_gen_generation) - nrow(missing_gen_data)} generators updated with generation values from direct matches to EIA-923 Generator File data.
                 {nrow(missing_gen_data)} generators without generation values remain."))


## Distribute generation to plants not in EIA-923 Generator file -------

### We create a distributional proportion based on nameplate capacity for plant/prime movers that are not in the 
### EIA-923 Generator file and distribute the generation with proportion

# first calculate generation at plant/pm level for gen_fuel file 
### Generation from EIA-923 Generation and Fuel file at the plant/prime mover level ---------

ozone_months_gen_fuel <- 
  c("netgen_may",
    "netgen_june",
    "netgen_july",
    "netgen_august",
    "netgen_september")

eia_gen_fuel_generation_sum <- 
  eia_923_gen_fuel %>% 
  mutate(generation_oz = rowSums(pick(all_of(ozone_months_gen_fuel)), na.rm = TRUE)) %>% # summing generation across ozone months
  group_by(plant_id, prime_mover) %>% 
  summarize(tot_generation_oz_fuel = sum(generation_oz, na.rm = TRUE), # ozone months total
            tot_generation_ann_fuel = sum(net_generation_megawatthours, na.rm = TRUE)) %>% # annual total
  ungroup()

gen_distributed <- 
  eia_gen_generation %>% 
  group_by(plant_id, prime_mover) %>% 
  summarize(tot_generation_ann = sum(generation_ann, na.rm = TRUE), # summing generation at plant/pm level
            tot_generation_oz = sum(generation_oz, na.rm = TRUE)) %>% 
  ungroup() %>%
  left_join(eia_gen_fuel_generation_sum) %>% # joining with gen fuel file to compare totals
  mutate(generation_ann_diff = tot_generation_ann_fuel - tot_generation_ann, # calculating difference between gen and gen fuel file
         generation_oz_diff = tot_generation_oz_fuel - tot_generation_oz) %>% 
  select(plant_id, prime_mover, generation_ann_diff, generation_oz_diff) %>% 
  right_join(eia_gen_generation) %>% # joining back in other columns
  filter(is.na(gen_data_source)) %>% # filtering to only generators with missing source
  group_by(plant_id, prime_mover) %>%
  mutate(tot_nameplate_capacity = sum(nameplate_capacity),
         prop = if_else(tot_nameplate_capacity != 0, # creating proportion based on nameplate_capacity used to distribute generation across generators
                        nameplate_capacity / tot_nameplate_capacity, 
                        NA_real_)) %>% 
  ungroup() %>% 
  mutate(generation_ann = generation_ann_diff * prop, # multiplying differences by proportion value
         generation_oz = generation_oz_diff * prop,
         gen_data_source = "Distributed from EIA-923 Generation and Fuel") %>% 
  bind_rows(eia_gen_generation %>% filter(!is.na(gen_data_source))) # adding back 923 Generation source rows

# check how many generators are missing generation values
missing_gen_data_2 <- 
  gen_distributed %>% 
  filter(is.na(gen_data_source))

print(glue::glue("{nrow(gen_distributed) - (nrow(eia_gen_generation) - nrow(missing_gen_data))} generators updated with generation values by distributing generation by plant and prime mover from EIA-923 Generation and Fuel data.
                 {nrow(missing_gen_data_2)} generators without generation values remain."))


### Determine differences between EIA-923 Generator File and EIA-923 Generation and Fuel file, and identify and distribute large cases --------- 

eia_gen_genfuel_diff <- 
  gen_distributed %>% 
  group_by(plant_id, prime_mover) %>% 
  summarize(tot_generation_ann_gen = sum(generation_ann, na.rm = TRUE), # summing generation to plant/pm level to compare to gen_fuel file
            tot_generation_oz_gen = sum(generation_oz, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(eia_gen_fuel_generation_sum, by = c("plant_id", "prime_mover")) %>% # joining with gen_fuel file
  mutate(abs_diff_generation_ann = abs(tot_generation_ann_fuel - tot_generation_ann_gen), # calculating absolute differences between generation values
         abs_diff_generation_oz = abs(tot_generation_oz_fuel - tot_generation_oz_gen),
         perc_diff_generation_ann = if_else(abs_diff_generation_ann == 0, 0, 
                                            abs_diff_generation_ann / tot_generation_ann_fuel), # calculating the percentage of the difference over the fuel levels in gen_fuel file
         perc_diff_generation_oz = if_else(abs_diff_generation_oz == 0, 0, 
                                           abs_diff_generation_oz / tot_generation_oz_fuel),
         overwrite = if_else(perc_diff_generation_ann > 0.001, "overwrite", "EIA-923 Generator File")) %>% 
  filter(tot_generation_ann_fuel != 0)


## Where overwrite == overwrite, we distribute the the generation figures in the EIA-923 Gen and Fuel file and 
## create a DF of generators that have large differences between EIA-923 Generator file and EIA-923 Generation and Fuel file 
## and distribute the difference with EIA-923 Generation and Fuel File generation values.
## Note that generators incorrectly end up in overwrite if they share a plant prime mover id with another generator
## that is indeed in generator file.

# creating vector of column names that are essentials for modified dataframes. 
# These are used to reduce clutter in dfs before they are combined in final structure below. 
key_columns <- 
  c("plant_id", 
    "prime_mover",
    "generator_id",
    "generation_ann",
    "generation_oz",
    "gen_data_source")

gen_overwrite <- 
  eia_gen_genfuel_diff %>% 
  left_join(eia_860_combined_r %>% 
              select(plant_id, generator_id, prime_mover, nameplate_capacity)) %>%
  group_by(plant_id, prime_mover) %>% 
  mutate(tot_nameplate_capacity = sum(nameplate_capacity),
         prop = if_else(tot_nameplate_capacity != 0, # creating proportion based on nameplate_capacity used to distribute generation across generators
                        nameplate_capacity / tot_nameplate_capacity, 
                        NA_real_)) %>% 
  ungroup() %>% 
  filter(overwrite == "overwrite") %>% 
  mutate(generation_ann = tot_generation_ann_fuel * prop,
         generation_oz = tot_generation_oz_fuel * prop,
         gen_data_source = "Data from EIA-923 Generator File overwritten with distributed data from EIA-923 Generation and Fuel") %>% 
  select(any_of(key_columns), overwrite) %>%  # reducing columns for clarity and to facilitate QA
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id))  # creating unique idea to identify duplicates
  
print(glue::glue("{nrow(gen_overwrite)} generators have generation data overwritten from EIA-923 Generator file with distributed data from EIA-923 Generation and Fuel due to percent difference >0.1% between data sources."))

## December generation ------
# find plants in the EIA-923 Generator file that are using the same net generation amount in December and redistribute using GenFuel file 

december_netgen <- 
  gen_distributed %>% 
  mutate(generation_ann_dec_equal = if_else(net_generation_december != 0 & 
                                              net_generation_december == net_generation_year_to_date, 
                                            "yes", "no")) %>% # identifying cases where annual generation = december generation
  left_join(eia_gen_fuel_generation_sum) %>% ## pulling in Gen fuel data
  group_by(plant_id, prime_mover) %>%
  mutate(tot_nameplate_capacity = sum(nameplate_capacity, na.rm = TRUE),
         prop = if_else(tot_nameplate_capacity != 0, # creating proportion based on nameplate_capacity used to distribute generation across generators
                        nameplate_capacity / tot_nameplate_capacity, 
                        NA_real_)) %>% 
  ungroup() %>% 
  mutate(
    generation_oz = if_else(generation_ann_dec_equal == "yes", 
                            tot_generation_oz_fuel * prop, generation_oz),
    gen_data_source = "EIA-923 Generator File") %>% 
  filter(generation_ann_dec_equal == "yes") %>%
  select(any_of(key_columns), generation_ann_dec_equal) %>%  # keeping only necessary columns
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) # creating unique idea to identify duplicates
  
print(glue::glue("{nrow(december_netgen)} generators have generation data where generation data equals December generation."))


# Form generator file structure ------------

check_dup_ids <- 
  december_netgen %>% 
  filter(id_pm %in% gen_overwrite$id_pm) %>% 
  pull(id_pm)

# combine set of special cases
december_and_overwritten <- 
  bind_rows(
    december_netgen %>% filter(!(id_pm %in% check_dup_ids)), # if generator is in both december_netgen and gen_overwrite, default to gen_overwrite
    gen_overwrite) %>% 
  left_join(eia_gen_generation %>% # merging all columns back in
              select(-c(starts_with("generation"), gen_data_source)),
            by = c("plant_id", "generator_id", "prime_mover"))  

print(glue::glue("{length(check_dup_ids)} generators are in both december_netgen and gen_overwrite. We default to gen_overwrite."))
print(glue::glue("{nrow(december_and_overwritten)} generator generation data are either overwritten from EIA-923 Generator and Fuel or from December generation."))

# now combining all generators 

generators_combined <- 
  gen_distributed %>% 
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) %>%
  filter(!(id_pm %in% december_and_overwritten$id_pm)) %>%  #filtering out observations that are in modified df
  bind_rows(december_and_overwritten)

# check if the number of rows when combining gen_distributed and december_and_overwritten is correct
gen_dist_no_dec_overwritten <- 
  gen_distributed %>% 
  mutate(id_pm = paste0(plant_id, "_", prime_mover, "_", generator_id)) %>%
  filter(!(id_pm %in% december_and_overwritten$id_pm))

if(nrow(generators_combined) > (nrow(gen_dist_no_dec_overwritten) + nrow(december_and_overwritten))) { # check if there are any units with duplicate entries 
  print(glue::glue("There are {nrow(gen_dist_no_dec_overwritten)} generators that are not overwritten or use December generation data. 
                   There are {nrow(december_and_overwritten)} generators. The dataframe with all generators {nrow(generators_combined)} generators."))
  
  check_dupe_ids <- 
    generators_combined %>%
    count(plant_id, generator_id, sort =  TRUE) %>% 
    filter(n > 1) %>% 
    mutate(plant_gen = glue::glue("Plant :{plant_id}, Generator: {generator_id}")) %>% 
    pull(plant_unit) %>% 
    str_c(., collapse = "\n")
  
  stop(glue::glue("There are more rows than there should be in the generators_combined dataframe. There are multiple rows for the following units: {\n dupe_ids}.\n Check for possible sources of duplicate generator_ids."))
} else{
  print("The number of rows in generators_combined matches the sum of generators that are overwritten, generators that use December generation, and all other generators.")
}

# Final modifications to generator file -----------

# creating lookup tables based on xwalk to use with recode() 
#lookup_fuel_codes <- with(xwalk_fuel_codes, setNames(fuel_code, id))

lookup_eia_id_epa_id <- with(xwalk_eia_epa, setNames(epa_plant_id, eia_plant_id))
lookup_epa_id_name <- with(xwalk_eia_epa, setNames(epa_plant_name, epa_plant_id))

generators_edits <- 
  generators_combined %>% 
  left_join(xwalk_fuel_codes %>% rename(fuel_code_update = fuel_code), by = c("plant_id")) %>% 
  mutate(id = paste0(plant_id, "_", generator_id), 
         fuel_code = if_else(plant_id %in% xwalk_fuel_codes$plant_id & energy_source_1 %in% c("OG", "OTH"), fuel_code_update, energy_source_1),
         generator_id = recode(id, !!!lookup_860_leading_zeroes, .default = generator_id), # updating generator ID to add back in leading zeroes
         generator_id = recode(id, !!!lookup_923_leading_zeroes, .default = generator_id), # updating generator ID to add back in leading zeroes
         plant_id = recode(plant_id, !!!lookup_eia_id_epa_id), # updating plant_id to corresponding EPA IDs with lookup table
         plant_name = recode(plant_id, !!!lookup_epa_id_name, .default = plant_name), # updating plant_name for specific plant_ids with lookup table
         gen_data_source = if_else(is.na(generation_ann), NA_character_, gen_data_source), # updating generation source to missing if annual generation is missing
         year = params$eGRID_year,
         capfact = if_else(nameplate_capacity != 0, generation_ann / (nameplate_capacity * 8760), 0)) %>%  # calculating capacity factor
  left_join(eia_860_boiler_count) %>% 
  rows_update(epa, by = c("plant_id"), unmatched = "ignore") %>% 
  rows_delete(epa_plants_to_delete, by = c("plant_id"), unmatched = "ignore")

# creating named vector of final variable order and variable name included in generator file
final_vars <-
    c("SEQGEN" = "seqgen",
      "YEAR" = "year",
      "PSTATABB" = "plant_state",
      "PNAME" = "plant_name",
      "ORISPL" = "plant_id",
      "GENID" = "generator_id",
      "NUMBLR" = "n_boilers",
      "GENSTAT" = "status",
      "PRMVR" =  "prime_mover",
      "FUELG1" = "fuel_code",
      "NAMEPCAP" = "nameplate_capacity",
      "CFACT" = "capfact",
      "GENNTAN" = "generation_ann",
      "GENNTOZ" = "generation_oz",
      "GENERSRC" = "gen_data_source",
      "GENYRONL" = "operating_year",
      "GENYRRET" = "retirement_year")

generators_formatted <-
  generators_edits %>%
  arrange(plant_state, plant_name, fuel_code) %>% 
  mutate(seqgen = row_number(),
         across(c("capfact", "generation_ann", "generation_oz"), ~ round(.x, 3))) %>% 
  select(as_tibble(final_vars)$value) %>% # keeping columns with tidy names since the rename is done in the final formatting script
  drop_na(plant_id, generator_id)

# Export generator file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
} else {
   dir.create("data/outputs")
}
 
if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
  print(glue::glue("Folder output/{params$eGRID_year} already exists."))
} else {
   dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
}

print(glue::glue("Saving generator file to folder data/outputs/{params$eGRID_year}"))

write_rds(generators_formatted, glue::glue("data/outputs/{params$eGRID_year}/generator_file.RDS"))
  

# check if file is successfully written to folder 
if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/generator_file.RDS"))){
  print(glue::glue("File generator_file.RDS successfully written to folder data/outputs/{params$eGRID_year}"))
} else {
   print("File generator_file.RDS failed to write to folder.")
}  

  