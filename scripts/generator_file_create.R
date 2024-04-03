
# Libraries 
library(dplyr)
library(readr)
library(stringr)
library(glue)


# Load in 923 and 860 files ----------

eia_923_gen <- read_rds("data/clean_data/eia/eia_923_gen_clean.RDS")

eia_923_gen_fuel <- read_rds("data/clean_data/eia/eia_923_gen_fuel_clean.RDS")

eia_860_boiler <- read_rds("data/clean_data/eia/eia_860_boil_gen_clean.RDS")

eia_860_combined <- read_rds("data/clean_data/eia/eia_860_combined_clean.RDS")


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
      TRUE ~ generator_id  # Default value if no condition matches
      ) 
    )
    
eia_860_boiler_count <- # creating count of boilers for each generator
  eia_860_boiler %>% 
  group_by(plant_id, 
           generator_id) %>% 
  summarize(n_boilers = n())

# Determine generation ------------

## Generation from 923 generator file ---


ozone_months_gen <- 
                c("net_generation_may", # creating vector of ozone month generation columns for calculations
                  "net_generation_june", 
                  "net_generation_july", 
                  "net_generation_august",
                  "net_generation_september")


eia_gen_generation <-
  eia_860_combined %>% 
  left_join(eia_923_gen_r) %>% # joins on plant_id, plant_name, plant_state, generator_id, prime_mover, and sector_name
  mutate(generation_oz = rowSums(pick(all_of(ozone_months)), na.rm = TRUE),
         gen_data_source = if_else(is.na(net_generation_year_to_date), NA_character_, "EIA-923 Generator File"))



## Distribute generation to plants not in generator file ----

### We create a distributional proportion based on nameplate capacity for plant/prime movers that are not in the 
### Generator file and distribute the generation with proportion

# first calculate generation at plant/pm level for gen_fuel file 
## Generation from 923 gen and fuel file at the plant/prime mover level ---

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
  summarize(tot_generation_oz_fuel = sum(generation_oz), # ozone months total
            tot_generation_ann_fuel = sum(net_generation_megawatthours)) # annual total

gen_distributed <- 
  eia_gen_generation %>% 
  left_join(eia_gen_fuel_generation_sum) %>%
  filter(is.na(gen_data_source)) %>%
  group_by(plant_id, prime_mover) %>% 
  mutate(tot_generation_ann = sum(net_generation_year_to_date, na.rm = TRUE),
         tot_generation_oz = sum(generation_oz),
         generation_ann_diff = tot_generation_ann_fuel - tot_generation_ann,
         generation_oz_diff = tot_generation_oz_fuel - tot_generation_oz) %>% 
  ungroup() %>% 
  mutate(tot_nameplate_capacity = sum(nameplate_capacity),
         prop = nameplate_capacity/tot_nameplate_capacity) %>% 
  ungroup() %>% 
  mutate(generation_ann = generation_ann_diff * prop,
         generation_oz = generation_oz_diff * prop,
         gen_data_source = "Distributed from 923 Generation and Fuel") %>% 
  bind_rows(eia_gen_generation %>% filter(!is.na(gen_data_source))) # adding back 923 Generation source rows


### determine differences between 923 Generator File and 923 Generation and Fuel file and identify and  distribute large cases --------- 
eia_gen_genfuel_diff <- 
  gen_distributed %>% 
  group_by(plant_id, prime_mover) %>% 
  summarize(tot_generation_ann_gen = sum(net_generation_year_to_date, na.rm = TRUE), # summing generation to plant/pm level to compare to gen_fuel file
            tot_generation_oz_gen = sum(generation_oz, na.rm = TRUE)) %>% 
  left_join(eia_gen_fuel_generation_sum) %>% # joing with gen_fuel file
  mutate(abs_diff_generation_ann = abs(tot_generation_ann_fuel - tot_generation_ann_gen), # calculating absolute differences between generation values
         abs_diff_generation_oz = abs(tot_generation_oz_fuel - tot_generation_oz_gen),
         perc_diff_generation_ann = if_else(abs_diff_generation_ann == 0, 0, abs_diff_generation_ann/tot_generation_ann_fuel), # calculating the percentage of the difference over the fuel levels in gen_fuel file
         perc_diff_generation_oz = if_else(abs_diff_generation_oz == 0, 0, abs_diff_generation_oz/tot_generation_oz_fuel),
         overwrite = if_else(perc_diff_generation_ann > .001, "overwrite", "EIA-923 Generator File"))

## Where overwrite == overwrite, we distribute the the generation figures in the Gen and Fuel file and 
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
  left_join(eia_860_combined %>% 
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

# find plants in the EIA-923 Generator file that are using the same net generation amount in December and redistribute using GenFuel file

december_netgen <- 
  gen_distributed %>% 
  mutate(generation_ann_dec_equal = if_else(net_generation_december != 0 & net_generation_december == net_generation_year_to_date, "yes", "no")) %>% # identifying cases where annual generation = december generation
  left_join(eia_gen_fuel_generation_sum) %>% ## pulling in Gen fuel data
  group_by(plant_id, prime_mover) %>%
  mutate(tot_nameplate_cap = sum(nameplate_capacity),
         proportion = nameplate_capacity/tot_nameplate_cap) %>% 
  ungroup() %>% 
  mutate(
    generation_oz = if_else(generation_ann_dec_equal == "yes", tot_generation_oz_fuel * prop, generation_oz),
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
  bind_rows(december_and_overwritten$id)

