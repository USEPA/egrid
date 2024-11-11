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


# Libraries 
library(dplyr)
library(readr)
library(stringr)
library(glue)

# Define params for eGRID data year
# This is only necessary when running script outside of the eGRID_master.qmd document

params <- list()
params$eGRID_year <- "2021"

# Load in necessary 923 and 860 files ----------

eia_923_files <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_923_clean.RDS")) # read in all 923 files
eia_860_files <- read_rds(glue::glue("data/clean_data/eia/{params$eGRID_year}/eia_860_clean.RDS")) # read in all 860 files

eia_923_gen <- eia_923_files$generator_data
eia_923_gen_fuel <- eia_923_files$generation_and_fuel_combined
eia_860_boiler <- eia_860_files$boiler_generator
eia_860_combined <- eia_860_files$combined %>% 
  select(plant_id, # keeping only necessary files for to streamline joins
         plant_name, 
         plant_state, 
         generator_id, 
         prime_mover, 
         status, 
         energy_source_1, 
         nameplate_capacity,
         retirement_year,
         operating_year)


# Create modified dfs that will be used to calculate generation values ---------

eia_923_gen_r <- 
  eia_923_gen %>% 
    mutate(generator_id = case_when( # modifying some ids to match 860 files.
      plant_id == 55168 & generator_id == 1 ~ "0001",
      plant_id == 55210 & generator_id == 1 ~ "0001",
      plant_id == 55239 & generator_id == 1 ~ "0001",
      plant_id == 55479 & generator_id == 1 ~ "0001",
      plant_id == 56298 & generator_id == 1 ~ "0001",
      plant_id == 56319 & generator_id == 1 ~ "0001",
      plant_id == 55596 & generator_id == 1 ~ "0001",
      plant_id == 54464 & generator_id == 1 ~ "0001",
      plant_id == 54464 & generator_id == 2 ~ "0002",
      plant_id == 55168 & generator_id == 2 ~ "0002",
      plant_id == 55210 & generator_id == 2 ~ "0002",
      plant_id == 55239 & generator_id == 2 ~ "0002",
      plant_id == 56298 & generator_id == 2 ~ "0002",
      plant_id == 55596 & generator_id == 2 ~ "0002",
      plant_id == 54464 & generator_id == 3 ~ "0003",
      plant_id == 55168 & generator_id == 3 ~ "0003",
      plant_id == 55239 & generator_id == 3 ~ "0003",
      plant_id == 56298 & generator_id == 3 ~ "0003",
      plant_id == 55596 & generator_id == 3 ~ "0003",
      plant_id == 54464 & generator_id == 4 ~ "0004",
      plant_id == 55239 & generator_id == 4 ~ "0004",
      plant_id == 55596 & generator_id == 4 ~ "0004",
      plant_id == 54464 & generator_id == 5 ~ "0005",
      plant_id == 55596 & generator_id == 5 ~ "0005",
      plant_id == 664 & generator_id == "8.1999999999999993" ~ "8.2",
      plant_id == 7512 & generator_id == 1 ~ "01", 
      plant_id == 7512 & generator_id == 2 ~ "02", 
      plant_id == 7512 & generator_id == 3 ~ "03", 
      TRUE ~ generator_id  # Default value if no condition matches
      ) 
    )

eia_860_combined_r <- 
  eia_860_combined %>% 
  mutate(generator_id = case_when( # updating generator IDs for plant that gets combined under ORIS 3612
    plant_id == 7512 & generator_id == 1 ~ "01", 
    plant_id == 7512 & generator_id == 2 ~ "02", 
    plant_id == 7512 & generator_id == 3 ~ "03", 
    TRUE ~ generator_id  # Default value if no condition matches
  ))
    
eia_860_boiler_count <- # creating count of boilers for each generator
  eia_860_boiler %>% 
  group_by(plant_id, 
           generator_id) %>% 
  summarize(n_boilers = n())

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
  left_join(eia_923_gen_r %>% 
              select(plant_id, generator_id, starts_with("net")), # keeping only necessary columns
            by = c("plant_id", "generator_id")) %>% 
  mutate(generation_oz = rowSums(pick(all_of(ozone_months_gen)), na.rm = TRUE),
         gen_data_source = if_else(is.na(net_generation_year_to_date), NA_character_, "EIA-923 Generator File"),
         generation_ann = net_generation_year_to_date)


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
  ungroup() %>% 
  group_by(plant_id, prime_mover) %>% 
  summarize(tot_generation_oz_fuel = sum(generation_oz, na.rm = TRUE), # ozone months total
            tot_generation_ann_fuel = sum(net_generation_megawatthours, na.rm = TRUE)) # annual total

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
         prop = nameplate_capacity/tot_nameplate_capacity) %>% # creating proportion based on nameplate_capacity used to distribute generation across generators
  ungroup() %>% 
  mutate(generation_ann = generation_ann_diff * prop, # multiplying differences by proportion value
         generation_oz = generation_oz_diff * prop,
         gen_data_source = "Distributed from EIA-923 Generation and Fuel") %>% 
  bind_rows(eia_gen_generation %>% filter(!is.na(gen_data_source))) # adding back 923 Generation source rows


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
         perc_diff_generation_ann = if_else(abs_diff_generation_ann == 0, 0, abs_diff_generation_ann/tot_generation_ann_fuel), # calculating the percentage of the difference over the fuel levels in gen_fuel file
         perc_diff_generation_oz = if_else(abs_diff_generation_oz == 0, 0, abs_diff_generation_oz/tot_generation_oz_fuel),
         overwrite = if_else(perc_diff_generation_ann > 0.001, "overwrite", "EIA-923 Generator File"))


## Where overwrite == overwrite, we distribute the the generation figures in the EIA-923 Gen and Fuel file and 
## create a DF of generators that have large differences between EIA-923 Generator file and EIA-923 Generation and Fuel file 
## and distribute the difference with EIA-923 Generation and Fuel File generation values.
## Note that generators incorrectly end up in overwrite if they share a plant prime mover id with another generator
## that is indeed in generator file.


key_columns <- # creating vector of column names that are essentials for modified dataframes. These are used to reduce clutter in dfs before they are combined in final structure below
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
  mutate(tot_nameplate_cap = sum(nameplate_capacity),
         prop = nameplate_capacity/tot_nameplate_cap) %>% 
  ungroup() %>% 
  filter(overwrite == "overwrite") %>% 
  mutate(generation_ann = tot_generation_ann_fuel * prop,
         generation_oz = tot_generation_oz_fuel * prop,
         gen_data_source = "Data from EIA-923 Generator File overwritten with distributed data from EIA-923 Generation and Fuel") %>% 
  select(any_of(key_columns), overwrite) # reducing columns for clarity and to facilitate QA


## December generation ------
# find plants in the EIA-923 Generator file that are using the same net generation amount in December and redistribute using GenFuel file 

december_netgen <- 
  gen_distributed %>% 
  mutate(generation_ann_dec_equal = if_else(net_generation_december != 0 & net_generation_december == net_generation_year_to_date, "yes", "no")) %>% # identifying cases where annual generation = december generation
  left_join(eia_gen_fuel_generation_sum) %>% ## pulling in Gen fuel data
  group_by(plant_id, prime_mover) %>%
  mutate(tot_nameplate_cap = sum(nameplate_capacity),
         prop = nameplate_capacity/tot_nameplate_cap) %>% 
  ungroup() %>% 
  mutate(
    generation_oz = if_else(generation_ann_dec_equal == "yes", 
                            tot_generation_oz_fuel * prop, generation_oz),
    gen_data_source = "EIA-923 Generator File") %>% 
  filter(generation_ann_dec_equal == "yes") %>%
  select(any_of(key_columns), generation_ann_dec_equal) # keeping only necessary columns


# Form generator file structure ------------

# combine set of special cases
december_and_overwritten <- 
  bind_rows(
    december_netgen,
    gen_overwrite
  ) %>% 
  left_join(eia_gen_generation %>% # merging all columns back in
              select(-c(starts_with("generation"), gen_data_source)),
            by = c("plant_id", "generator_id", "prime_mover")) %>% 
  mutate(id = paste0(plant_id,prime_mover,generator_id)) # creating unique id to identify duplicates

# now combining all generators 

generators_combined <- 
  gen_distributed %>% 
  mutate(id = paste0(plant_id,prime_mover,generator_id)) %>%
  filter(!id %in% december_and_overwritten$id) %>%  #filtering out observations that are in modified df
  bind_rows(december_and_overwritten)


# Final modifications to generator file -----------

xwalk_fuel_codes <- # this is xwalk for specific changes made to certain generator xwalks
  read_csv("data/static_tables/xwalk_fuel_type.csv") %>% 
  mutate(id = paste0(plant_id, prime_mover, unit_id)) %>% # creating id to facilitate join
  select(id, fuel_code)

# creating lookup tables based on xwalk to use with recode() 
lookup_fuel_codes <- with(xwalk_fuel_codes, setNames(fuel_code, id))

xwalk_eia_camd <- # xwalk for updating certain plants to camd plant names and ids
  read_csv("data/static_tables/xwalk_oris_camd.csv") %>%
  mutate(across(everything(), ~ as.character(.x))) # changing all columns to character values


lookup_eia_id_camd_id <- with(xwalk_eia_camd, setNames(camd_plant_id, eia_plant_id))
lookup_camd_id_name <- with(xwalk_eia_camd, setNames(camd_plant_name, camd_plant_id))
  

generators_edits <- 
  generators_combined %>% 
  mutate(fuel_code = recode(id, !!!lookup_fuel_codes, .default = energy_source_1), # creating fuel_code based on lookup table and energy_source_1 if not in lookup table. recode() essentially matches on id, then replaces with key value  
         plant_id = recode(plant_id, !!!lookup_eia_id_camd_id), # updating plant_id to corresponding camd ids with lookup table
         plant_name = recode(plant_id, !!!lookup_camd_id_name, .default = plant_name), # updating plant_name for specific plant_ids with lookup table
         gen_data_source = if_else(is.na(generation_ann), NA_character_, gen_data_source), # updating generation source to missing if annual generation is missing
         year = params$eGRID_year,
         capfact = generation_ann/(nameplate_capacity * 8760)) %>%  # calculating capacity factor
  left_join(eia_860_boiler_count)

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
  group_by(plant_id, generator_id, fuel_code) %>% 
  slice(1) %>% # Some generators are duplicated due to combined_heat_and_power_plant field and associated differences in generatation value. This is short-term fix until confirmation on how to handle.
  ungroup() %>%
  arrange(plant_state, plant_name) %>% 
  mutate(seqgen = row_number(),
         across(c("capfact", "generation_ann", "generation_oz"), ~ round(.x, 3))) %>% 
  select(as_tibble(final_vars)$value) # keeping columns with tidy names for QA steps


# generator_file <- # creating version with final generator file names for eGRID. (Note: This may be done differently when create final excel file)
#   generators_formatted %>% 
#   rename(all_of(final_vars))


# Export generator file -----------

if(dir.exists("data/outputs")) {
  print("Folder output already exists.")
}else{
  dir.create("data/outputs")
}

if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
  print("Folder output already exists.")
}else{
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

  
