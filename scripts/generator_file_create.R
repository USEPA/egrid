
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
      TRUE ~ NA_character_  # Default value if no condition matches
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
  group_by(plant_id, prime_mover) %>% 
  mutate(tot_generation_ann = sum(net_generation_year_to_date, na.rm = TRUE),
         tot_generation_oz = sum(generation_oz),
         generation_ann_diff = tot_generation_ann_fuel - tot_generation_ann,
         generation_oz_diff = tot_generation_oz_fuel - tot_generation_oz) %>% 
  ungroup() %>% 
  filter(is.na(gen_data_source)) %>% 
  mutate(tot_nameplate_capacity = sum(nameplate_capacity),
         prop = nameplate_capacity/tot_nameplate_capacity) %>% 
  ungroup() %>% 
  mutate(generation_ann = generation_ann_diff * prop,
         generation_oz = generation_oz_diff * prop,
         gen_data_source = "Distributed from 923 Generation and Fuel")

    