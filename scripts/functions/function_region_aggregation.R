## -------------------------------
##
## Aggregation file create function
## 
## Purpose: 
## 
## This file creates a function for the aggregation files for eGRID. 
##
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------


region_aggregation <- function(region, region_cols) {
  
  #' region_aggregation
  #' 
  #' Function to create region aggregation files. 
  #' 
  #' @param region Region for data to be aggregated to, as a string
  #' @param region_cols Relevant region columns from the plant file, as a vector
  #' @return .RDS file of aggregated region
  #' @examples
  #' region_aggregation("state", c(fips_state_code, state)) # Aggregates the plant file to the state regional level
  
  require(dplyr)
  
  # Load and clean necessary data ------
  
  # columns to keep from plant file
  
  columns_to_keep <- 
    c("year", 
      "state" = "plant_state",
      "fips_state_code",
      "plant_name",
      "plant_id",
      "nerc",
      "nerc_name", 
      "subregion" = "egrid_subregion",
      "subregion_name" = "egrid_subregion_name",
      "ba_name",
      "ba_code",
      "primary_fuel_type",
      "primary_fuel_category",
      "nameplate_capacity",
      "capfac",
      "nonbaseload",
      "combust_heat_input",
      "combust_heat_input_oz",
      "heat_input",
      "heat_input_oz",
      "generation_ann",
      "generation_oz",
      "generation_nonbaseload",
      "nox_mass",
      "nox_oz_mass",
      "so2_mass",
      "co2_mass",
      "ch4_mass",
      "n2o_mass",
      "co2e_mass",
      "hg_mass",
      "ann_gen_coal",
      "ann_gen_oil",
      "ann_gen_gas",
      "ann_gen_nuclear",
      "ann_gen_hydro",
      "ann_gen_biomass",
      "ann_gen_wind",
      "ann_gen_solar",
      "ann_gen_geothermal",
      "ann_gen_other_ff",
      "ann_gen_other",
      "ann_gen_non_renew",
      "ann_gen_renew",
      "ann_gen_non_renew_other", 
      "ann_gen_renew_nonhydro",
      "ann_gen_combust",
      "ann_gen_non_combust",
      "ann_gen_non_combust_other"
    )
  
  # read in NERC names to add to plant file
  
  nerc_names <- read_csv("data/static_tables/nerc_region_names.csv") %>% janitor::clean_names()
  
  # read in plant file 
  
  plant_file <- 
    read_rds(glue::glue("data/outputs/{params$eGRID_year}/plant_file.RDS")) %>% 
    left_join(nerc_names, by = c("nerc")) %>% 
    select(any_of(columns_to_keep))
  
  
  # factor plant_fuel_category to final output order
  plant_file$primary_fuel_category <- factor(plant_file$primary_fuel_category, 
                                             levels = c("COAL", 
                                                        "OIL", 
                                                        "GAS",
                                                        "NUCLEAR", 
                                                        "HYDRO", 
                                                        "BIOMASS", 
                                                        "WIND", 
                                                        "SOLAR", 
                                                        "GEOTHERMAL", 
                                                        "OFSL", 
                                                        "OTHF"))
  
  
  if(as.character(region) != "us") { # US level does not have any relevant regional columns, so we remove that for US version
  
    # aggregate plant file to region level ------
    
    # sum capacity, generation, emissions mass to region level 
    
    region_agg <- 
      plant_file %>% 
      group_by(pick({{ region_cols }})) %>% 
      summarize(across(.cols = c("nameplate_capacity", 
                                 contains("heat_input"), 
                                 "generation_ann", 
                                 "generation_oz", 
                                 "generation_nonbaseload",
                                 contains("_mass"), 
                                 contains("ann_gen")), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      mutate(region_hg_mass = "--") %>% 
      ungroup()
  
    
    ## Calculate emission rates ------
    
    ### Output emission rates (lb/MWh) -----
    
    region_output_rates <- 
      region_agg %>% 
      mutate(
        # calculate output emissions rates (lb/MWh)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_generation_ann != 0, 2000 * . / region_generation_ann, NA_real_),
               .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
        region_nox_oz_output_rate = if_else(region_generation_oz != 0, 
                                            2000 * region_nox_oz_mass / region_generation_oz, NA_real_),
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_generation_ann != 0, . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate"),
        region_hg_output_rate = "--") %>% 
      relocate(region_nox_oz_output_rate, .after = region_nox_output_rate) %>%  
      relocate(region_co2e_output_rate, .after = region_n2o_output_rate) %>% 
      select({{ region_cols }}, contains("rate"))

    
    ### Input emission rates (lb/MMBtu) -----
    
    region_input_rates <- 
      region_agg %>% 
      mutate(
        # calculate input emission rates (lb/MMBtu)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_combust_heat_input != 0, 2000 * . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
        region_nox_oz_input_rate = if_else(region_combust_heat_input_oz != 0, 
                                           2000 * region_nox_oz_mass / region_combust_heat_input_oz, NA_real_),  
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_combust_heat_input != 0, . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
        region_hg_input_rate = "--") %>% 
      relocate(region_nox_oz_input_rate, .after = region_nox_input_rate) %>%  
      relocate(region_co2e_input_rate, .after = region_n2o_input_rate) %>% 
      select({{ region_cols }}, contains("rate"))
    
    
    ### Combustion emission rates (lb/MWh) -----
    
    region_combustion_rates <- 
      region_agg %>% 
      mutate(
        # calculate combustion emissions rates (lb/MWh)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_ann_gen_combust != 0, 2000 * . / region_ann_gen_combust, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
        region_nox_oz_combustion_rate = if_else(region_generation_oz != 0 & 
                                                  region_ann_gen_combust != 0 &
                                                  region_generation_ann != 0, 
                                                2000 * region_nox_oz_mass / 
          (region_ann_gen_combust * (region_generation_oz / region_generation_ann)), NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_ann_gen_combust != 0, . / region_ann_gen_combust, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
        region_hg_combustion_rate = "--") %>% 
      relocate(region_nox_oz_combustion_rate, .after = region_nox_combustion_rate) %>%  
      relocate(region_co2e_combustion_rate, .after = region_n2o_combustion_rate) %>% 
      select({{ region_cols }}, contains("rate")) 
    
    
    ### Fuel type output emission rates (lb/MWh) and input emission rates (lb/MMBtu)  -----
    
    # calculate emission rates by fossil fuel types 
    
    fossil_fuels <- c("COAL", 
                      "OIL",
                      "GAS", 
                      "OSFL")
    
    region_fuel_rates <-
      plant_file %>% 
      group_by(pick({{ region_cols }}), primary_fuel_category) %>% 
      filter(primary_fuel_category %in% fossil_fuels, 
             !primary_fuel_category == 'OSFL') %>% # do not include other fossil in individual fuel rate calculations
      summarize(across(.cols = c(contains("heat_input"), 
                                 "generation_ann", 
                                 "generation_oz", 
                                 contains("mass"), 
                                 "ann_gen_coal", 
                                 "ann_gen_oil", 
                                 "ann_gen_gas"), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      mutate(
        # output emission rates (lb/MWh)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_generation_ann != 0, 2000 * . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
        region_nox_oz_output_rate = if_else(region_generation_oz != 0, 
                                            2000 * region_nox_oz_mass / region_generation_oz, NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_generation_ann != 0, . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate"),
        
        # input emission rates (lb/MMBtu)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_combust_heat_input != 0, 2000 * . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
        region_nox_oz_input_rate = if_else(region_combust_heat_input_oz != 0, 
                                           2000 * region_nox_oz_mass / region_combust_heat_input_oz, NA_real_),  
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_combust_heat_input != 0, . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
      select({{ region_cols }}, primary_fuel_category, contains("rate")) %>% 
      relocate(region_nox_oz_output_rate, .after = region_nox_output_rate) %>% 
      relocate(region_co2e_output_rate, .after = region_n2o_output_rate) %>% 
      relocate(region_nox_oz_input_rate, .after = region_nox_input_rate) %>% 
      relocate(region_co2e_input_rate, .after = region_n2o_input_rate) %>% 
      arrange(primary_fuel_category) %>% 
      pivot_wider(names_from = primary_fuel_category, 
                  values_from = contains("rate")) %>% 
      janitor::clean_names() %>% 
      mutate(across(where(is.numeric), ~ replace_na(., 0)), 
             region_hg_output_rate_coal = "--", 
             region_hg_output_rate_fossil = "--", 
             region_hg_input_rate_coal = "--", 
             region_hg_input_rate_fossil = "--") %>% 
      ungroup()
    
    
    # calculate all fossil fuel output and input emission rates 
    
    region_fossil_rates <-
      plant_file %>% 
      group_by(pick({{ region_cols }})) %>% 
      filter(primary_fuel_category %in% fossil_fuels) %>% 
      summarize(across(.cols = c(contains("heat_input"), 
                                 "generation_ann", 
                                 "generation_oz", 
                                 contains("mass"), 
                                 "ann_gen_coal", 
                                 "ann_gen_oil", 
                                 "ann_gen_gas", 
                                 "ann_gen_other_ff"), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      mutate(# output emission rates (lb/MWh)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_generation_ann != 0, 2000 * . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
        region_nox_oz_output_rate_fossil = if_else(region_generation_oz != 0, 
                                                   2000 * region_nox_oz_mass / region_generation_oz, NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_generation_ann != 0, . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
        
        # input emission rates (lb/MMBtu)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_combust_heat_input != 0, 2000 * . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
        region_nox_oz_input_rate_fossil = if_else(region_combust_heat_input_oz != 0, 
                                                  2000 * region_nox_oz_mass / region_combust_heat_input_oz, 
                                                  NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_combust_heat_input != 0, . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
        across(where(is.numeric), ~ replace_na(., 0))) %>% 
      relocate(region_co2e_input_rate_fossil, .after = region_n2o_input_rate_fossil) %>%  
      relocate(region_nox_oz_input_rate_fossil, .after = region_nox_input_rate_fossil) %>% 
      relocate(region_nox_oz_output_rate_fossil, .after = region_nox_output_rate_fossil) %>% 
      relocate(region_co2e_output_rate_fossil, .after = region_n2o_output_rate_fossil) %>% 
      select({{ region_cols }}, contains("rate")) %>% 
      ungroup()
    
    
    
    ### Non-baseload output emission rates (lb/MWh) -----
    
    region_nonbaseload_rates <- 
      plant_file %>% 
      group_by(pick({{ region_cols }})) %>% 
      summarize(across(.cols = c("generation_ann", 
                                 "generation_oz", 
                                 contains("mass")), 
                       .fns = ~ sum(. * nonbaseload, na.rm = TRUE), 
                       .names = "region_{.col}")) %>% 
      mutate(across(.cols = c("region_nox_mass",  
                              "region_so2_mass", 
                              "region_co2_mass", 
                              "region_co2e_mass"), 
                    .fns = ~ if_else(region_generation_ann != 0, 2000 * . / region_generation_ann, NA_real_), 
                    .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
             region_nox_oz_output_rate_nonbaseload = if_else(region_generation_oz != 0, 
                                                             2000 * region_nox_oz_mass / region_generation_oz, 
                                                             NA_real_), 
             across(.cols = c("region_ch4_mass", 
                              "region_n2o_mass"),
                    .fns = ~ if_else(region_generation_ann != 0, . / region_generation_ann, NA_real_), 
                    .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
             region_hg_output_rate_nonbaseload = "--") %>% 
      relocate(region_nox_oz_output_rate_nonbaseload, .after = region_nox_output_rate_nonbaseload) %>% 
      relocate(region_co2e_output_rate_nonbaseload, .after = region_n2o_output_rate_nonbaseload) %>%
      select({{ region_cols }}, contains("rate")) %>% 
      ungroup()
    
    
    ## Calculate net generation and resource mix -----
    
    ### Generation by fuel category (MWh) -----
    
    region_gen <- 
      region_agg %>% 
      select({{ region_cols }}, contains("gen"))
    
    region_gen_2 <- 
      region_gen %>% 
      select(-region_generation_ann, -region_generation_oz, -region_generation_nonbaseload) # remove duplicate columns for final formatting
    
    ### Resource mix by fuel category (%) -----
    
    region_resource_mix <- 
      region_gen %>% 
      select(-region_generation_oz, -region_generation_nonbaseload) %>%   
      mutate(across(.cols = -c({{ region_cols }}, "region_generation_ann"), 
                    .fns = ~ if_else(region_generation_ann != 0, 
                                     . / region_generation_ann, NA_real_), # convert to percentage 
                    .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
      select({{ region_cols }}, contains("resource_mix"))
    
    
    ### Nonbaseload generation (MWh) -----
    
    region_nonbaseload_gen <- 
      plant_file %>% 
      group_by(pick({{ region_cols }}), primary_fuel_category) %>% 
      summarize(region_nonbaseload_gen = sum(generation_ann * nonbaseload, na.rm = TRUE)) %>% 
      arrange(primary_fuel_category) %>% 
      pivot_wider(names_from = primary_fuel_category, 
                  values_from = region_nonbaseload_gen, 
                  names_prefix = "region_nonbaseload_gen_") %>% 
      janitor::clean_names() %>% 
      mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% 
      ungroup() %>% 
      select({{ region_cols }}, contains("nonbaseload_gen"))
    
    
    ### Nonbaseload resource mix (%) -----
    
    region_nonbaseload_resource_mix <- 
      region_nonbaseload_gen %>% 
      mutate(region_nonbaseload_gen = rowSums(pick(contains("nonbaseload"))), 
             across(.cols = -c({{ region_cols }}, "region_nonbaseload_gen"), 
                    .fns = ~ if_else(region_nonbaseload_gen != 0, . / region_nonbaseload_gen, NA_real_), 
                    .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
      select({{ region_cols }}, contains("resource_mix"))
    
    
    ## Create final data frame -----
    
    ### Join necessary data -----
    
    region_merged <- 
      region_agg %>% select(-contains("ann_gen")) %>% 
      left_join(region_output_rates) %>% # output emission rates
      left_join(region_input_rates) %>% # input emission rates
      left_join(region_combustion_rates) %>% # combustion emission rates
      left_join(region_fuel_rates) %>% # output and input emission rates by fuel type
      left_join(region_fossil_rates) %>% # output and input emission rates for all fossil fuels
      left_join(region_nonbaseload_rates) %>% # output emission rates for nonbaseload generation
      left_join(region_gen_2) %>% # generation by fuel category
      left_join(region_resource_mix) %>% # resource mix by fuel category
      left_join(region_nonbaseload_gen) %>% # nonbaseload generation by fuel category
      left_join(region_nonbaseload_resource_mix) %>% 
      mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
             across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
      select(-contains("gen_na"),
             -contains("mix_na")) %>% # remove unnecessary columns 
      drop_na({{ region_cols }})
    
    
    ### Round data ----
    
    n2o_ch4_fuel_rates <- c("n2o_input_rate_coal", 
                            "n2o_input_rate_oil", 
                            "n2o_input_rate_gas", 
                            "n2o_input_rate_fossil", 
                            "n2o_output_rate_coal", 
                            "n2o_output_rate_oil", 
                            "n2o_output_rate_gas", 
                            "n2o_output_rate_fossil", 
                            "ch4_input_rate_coal", 
                            "ch4_input_rate_oil", 
                            "ch4_input_rate_gas", 
                            "ch4_input_rate_fossil", 
                            "ch4_output_rate_coal", 
                            "ch4_output_rate_oil", 
                            "ch4_output_rate_gas", 
                            "ch4_output_rate_fossil")
    
    region_rounded <- 
      region_merged %>% 
      mutate(across(c(where(is.numeric), -contains(n2o_ch4_fuel_rates)), 
                    \(x) round(x, 3)), # round to three decimals
             across(contains(n2o_ch4_fuel_rates), 
                    \(x) round(x, 4))) # round N2O and CH4 fuel specific rates to 4 decimal points
    
    
    ### Format to regional output -------
    
    region_formatted <- 
      region_rounded %>% 
      mutate(year = params$eGRID_year) %>% # set year as params$eGRID_year
      relocate(year) %>% # relocate to left most column 
      relocate(region_nox_output_rate_fossil, .after = region_nox_output_rate_gas) %>% 
      relocate(region_nox_oz_output_rate_fossil, .after = region_nox_oz_output_rate_gas) %>% 
      relocate(region_so2_output_rate_fossil, .after = region_so2_output_rate_gas) %>% 
      relocate(region_co2_output_rate_fossil, .after = region_co2_output_rate_gas) %>% 
      relocate(region_co2e_output_rate_fossil, .after = region_co2e_output_rate_gas) %>% 
      relocate(region_ch4_output_rate_fossil, .after = region_ch4_output_rate_gas) %>% 
      relocate(region_n2o_output_rate_fossil, .after = region_n2o_output_rate_gas) %>% 
      relocate(region_hg_output_rate_coal, .after = region_co2e_output_rate_fossil) %>% 
      relocate(region_hg_output_rate_fossil, .after = region_hg_output_rate_coal) %>% 
      relocate(region_nox_input_rate_fossil, .after = region_nox_input_rate_gas) %>% 
      relocate(region_nox_oz_input_rate_fossil, .after = region_nox_oz_input_rate_gas) %>% 
      relocate(region_so2_input_rate_fossil, .after = region_so2_input_rate_gas) %>% 
      relocate(region_co2_input_rate_fossil, .after = region_co2_input_rate_gas) %>% 
      relocate(region_co2e_input_rate_fossil, .after = region_co2e_input_rate_gas) %>% 
      relocate(region_ch4_input_rate_fossil, .after = region_ch4_input_rate_gas) %>% 
      relocate(region_n2o_input_rate_fossil, .after = region_n2o_input_rate_gas) %>% 
      relocate(region_hg_output_rate_coal, .after = region_co2e_output_rate_fossil) %>% 
      relocate(region_hg_input_rate_fossil, .after = region_hg_input_rate_coal) %>% 
      mutate(across(.cols = starts_with("region_"), # replace region with specific region name
                    .fns = ~ ., 
                    .names = "{str_replace(.col, 'region', {region})}")) %>% 
      select(-starts_with("region_"))
    
  
    
    ### Export region aggregation file -----------
    
    if(dir.exists("data/outputs")) {
      print("Folder output already exists.")
    } else {
      dir.create("data/outputs")
    }
    
    if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
      print("Folder output already exists.")
    } else {
      dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
    }
    
    print(glue::glue("Saving {region} aggregation file to folder data/outputs/{params$eGRID_year}"))
    
    write_rds(region_formatted, glue::glue("data/outputs//{params$eGRID_year}/{region}_aggregation.RDS")) 
    
  } else {

    ### aggregate plant file to US level ------
    
    # sum capacity, generation, emissions mass to US level 
    
    region_agg <- 
      plant_file %>% 
      summarize(across(.cols = c("nameplate_capacity", 
                                 contains("heat_input"), 
                                 "generation_ann", 
                                 "generation_oz", 
                                 "generation_nonbaseload",
                                 contains("_mass")), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      mutate(region_hg_mass = "--") %>% 
      ungroup() %>% 
      distinct()
    
    
    ## Calculate emission rates ------
    
    #### Output emission rates (lb/MWh) -----
    
    region_output_rates <- 
      region_agg %>% 
      mutate(# calculating output emissions rates (lb/MWh)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_generation_ann != 0, 2000 * . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
        region_nox_oz_output_rate = if_else(region_generation_oz != 0, 
                                            2000 * region_nox_oz_mass / region_generation_oz, NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_generation_ann != 0, . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate"),
        region_hg_output_rate = "--") %>% 
      relocate(region_nox_oz_output_rate, .after = region_nox_output_rate) %>%  
      relocate(region_co2e_output_rate, .after = region_n2o_output_rate) %>% 
      select(contains("rate")) %>% 
      distinct()
    
    
    #### Input emission rates (lb/MMBtu) -----
    
    region_input_rates <- 
      region_agg %>% 
      mutate(# calculating input emission rates (lb/MMBtu)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_combust_heat_input != 0, 2000 * . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
        region_nox_oz_input_rate = if_else(region_combust_heat_input_oz != 0, 
                                           2000 * region_nox_oz_mass / region_combust_heat_input_oz, NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_combust_heat_input != 0, . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
        region_hg_input_rate = "--") %>% 
      relocate(region_nox_oz_input_rate, .after = region_nox_input_rate) %>%  
      relocate(region_co2e_input_rate, .after = region_n2o_input_rate) %>% 
      select(contains("rate")) %>% 
      distinct()
    
    
    #### Combustion emission rates (lb/MWh) -----
    
    region_combustion_rates <- 
      plant_file %>% 
      summarize(region_combustion_gen = sum(ann_gen_combust, na.rm = TRUE)) %>% 
      cbind(plant_file, region_agg) %>% 
      mutate(# calculating combustion emissions rates (lb/MWh)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_combustion_gen != 0, 2000 * . / region_combustion_gen, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
        region_nox_oz_combustion_rate = if_else(region_combustion_gen != 0 & 
                                                  region_generation_oz != 0 & 
                                                  region_generation_ann != 0, 
                                                2000 * region_nox_oz_mass / 
          (region_combustion_gen * (region_generation_oz / region_generation_ann)), NA_real_),  
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_generation_ann != 0, . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_combustion_rate"), 
        region_hg_combustion_rate = "--") %>% 
      relocate(region_nox_oz_combustion_rate, .after = region_nox_combustion_rate) %>%  
      relocate(region_co2e_combustion_rate, .after = region_n2o_combustion_rate) %>% 
      select(contains("rate")) %>% 
      distinct()
    
    
    #### Fuel type output emission rates (lb/MWh) and input emission rates (lb/MMBtu)  -----
    
    # calculate emission rates by fossil fuel types 
    
    fossil_fuels <- c("COAL", 
                      "OIL",
                      "GAS", 
                      "OSFL")
    
    region_fuel_rates <-
      plant_file %>% 
      group_by(primary_fuel_category) %>% 
      filter(primary_fuel_category %in% fossil_fuels, 
             !primary_fuel_category == 'OSFL') %>% # do not include other fossil in individual fuel rate calculations
      summarize(across(.cols = c(contains("heat_input"), 
                                 "generation_ann", 
                                 "generation_oz", 
                                 contains("mass"), 
                                 "ann_gen_coal", 
                                 "ann_gen_oil", 
                                 "ann_gen_gas"), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      mutate(# output emission rates (lb/MWh)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_generation_ann != 0, 2000 * . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate"), 
        region_nox_oz_output_rate = if_else(region_generation_oz != 0, 
                                            2000 * region_nox_oz_mass / region_generation_oz, NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_generation_ann != 0, . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate"),
        
        # input emission rates (lb/MMBtu)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_combust_heat_input != 0, 2000 * . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate"), 
        region_nox_oz_input_rate = if_else(region_combust_heat_input_oz != 0, 
                                           2000 * region_nox_oz_mass / region_combust_heat_input_oz, NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_combust_heat_input != 0, . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate")) %>% 
      select(primary_fuel_category, contains("rate")) %>% 
      relocate(region_nox_oz_output_rate, .after = region_nox_output_rate) %>% 
      relocate(region_co2e_output_rate, .after = region_n2o_output_rate) %>% 
      relocate(region_nox_oz_input_rate, .after = region_nox_input_rate) %>% 
      relocate(region_co2e_input_rate, .after = region_n2o_input_rate) %>% 
      arrange(primary_fuel_category) %>% 
      pivot_wider(names_from = primary_fuel_category, 
                  values_from = contains("rate")) %>% 
      janitor::clean_names() %>% 
      mutate(across(where(is.numeric), ~ replace_na(., 0)), 
             region_hg_output_rate_coal = "--", 
             region_hg_output_rate_fossil = "--", 
             region_hg_input_rate_coal = "--", 
             region_hg_input_rate_fossil = "--") %>% 
      distinct()
    
    
    # calculate all fossil fuel output and input emission rates 
    
    region_fossil_rates <-
      plant_file %>% 
      filter(primary_fuel_category %in% fossil_fuels) %>% 
      summarize(across(.cols = c(contains("heat_input"), 
                                 "generation_ann", 
                                 "generation_oz", 
                                 contains("mass"), 
                                 "ann_gen_coal", 
                                 "ann_gen_oil", 
                                 "ann_gen_gas", 
                                 "ann_gen_other_ff"), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      mutate(# output emission rates (lb/MWh)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_generation_ann != 0, 2000 * . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"), 
        region_nox_oz_output_rate_fossil = if_else(region_generation_oz != 0, 
                                                   2000 * region_nox_oz_mass / region_generation_oz, NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_generation_ann != 0, . / region_generation_ann, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_output_rate_fossil"),
        
        # input emission rates (lb/MMBtu)
        across(.cols = c("region_nox_mass",  
                         "region_so2_mass", 
                         "region_co2_mass", 
                         "region_co2e_mass"), 
               .fns = ~ if_else(region_combust_heat_input != 0, 2000 * . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
        region_nox_oz_input_rate_fossil = if_else(region_combust_heat_input_oz != 0, 
                                                  2000 * region_nox_oz_mass / region_combust_heat_input_oz, 
                                                  NA_real_), 
        across(.cols = c("region_ch4_mass", 
                         "region_n2o_mass"),
               .fns = ~ if_else(region_combust_heat_input != 0, . / region_combust_heat_input, NA_real_), 
               .names = "{str_replace(.col, '_mass', '')}_input_rate_fossil"), 
        across(where(is.numeric), ~ replace_na(., 0))) %>% 
      relocate(region_co2e_input_rate_fossil, .after = region_n2o_input_rate_fossil) %>%  
      relocate(region_nox_oz_input_rate_fossil, .after = region_nox_input_rate_fossil) %>% 
      relocate(region_nox_oz_output_rate_fossil, .after = region_nox_output_rate_fossil) %>% 
      relocate(region_co2e_output_rate_fossil, .after = region_n2o_output_rate_fossil) %>% 
      select(contains("rate")) %>% 
      distinct()
    
    
    
    #### Non-baseload output emission rates (lb/MWh) -----
    
    region_nonbaseload_rates <- 
      plant_file %>% 
      summarize(across(.cols = c("generation_ann", 
                                 "generation_oz", 
                                 contains("mass")), 
                       .fns = ~ sum(. * nonbaseload, na.rm = TRUE), 
                       .names = "region_{.col}")) %>% 
      mutate(across(.cols = c("region_nox_mass",  
                              "region_so2_mass", 
                              "region_co2_mass", 
                              "region_co2e_mass"), 
                    .fns = ~ if_else(region_generation_ann != 0, 2000 * . / region_generation_ann, NA_real_), 
                    .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"), 
             region_nox_oz_output_rate_nonbaseload = if_else(region_generation_oz != 0, 
                                                             2000 * region_nox_oz_mass / region_generation_oz, 
                                                             NA_real_), 
             across(.cols = c("region_ch4_mass", 
                              "region_n2o_mass"),
                    .fns = ~ if_else(region_generation_ann != 0, . / region_generation_ann, NA_real_), 
                    .names = "{str_replace(.col, '_mass', '')}_output_rate_nonbaseload"),
             region_hg_output_rate_nonbaseload = "--") %>% 
      relocate(region_nox_oz_output_rate_nonbaseload, .after = region_nox_output_rate_nonbaseload) %>% 
      relocate(region_co2e_output_rate_nonbaseload, .after = region_n2o_output_rate_nonbaseload) %>%
      select(contains("rate")) %>% 
      distinct()
    
    
    ## Calculate net generation and resource mix -----
    
    #### Generation by fuel category (MWh) -----
    
    region_gen <- 
      plant_file %>% 
      summarize(across(.cols = c(contains("gen")), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      distinct()
    
    region_gen_2 <- 
      region_gen %>% 
      select(-region_generation_ann, -region_generation_oz, -region_generation_nonbaseload) # remove duplicate columns for final formatting
    
    #### Resource mix by fuel category (%) -----
    
    region_resource_mix <- 
      region_gen %>% 
      select(-region_generation_oz, -region_generation_nonbaseload) %>%   
      mutate(across(.cols = -c("region_generation_ann"), 
                    .fns = ~ if_else(region_generation_ann != 0, 
                                     . / region_generation_ann, NA_real_), # convert to percentage 
                    .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
      select(contains("resource_mix")) %>% 
      distinct()
    
    
    #### Nonbaseload generation (MWh) -----
    
    region_nonbaseload_gen <- 
      plant_file %>% 
      group_by(primary_fuel_category) %>% 
      summarize(region_nonbaseload_gen = sum(generation_ann * nonbaseload, na.rm = TRUE)) %>% 
      arrange(primary_fuel_category) %>% 
      pivot_wider(names_from = primary_fuel_category, 
                  values_from = region_nonbaseload_gen, 
                  names_prefix = "region_nonbaseload_gen_") %>% 
      janitor::clean_names() %>% 
      mutate(across(where(is.numeric), ~ replace_na(., 0))) 
    
    
    #### Nonbaseload resource mix (%) -----
    
    region_nonbaseload_resource_mix <- 
      region_nonbaseload_gen %>% 
      mutate(region_nonbaseload_gen = rowSums(pick(contains("nonbaseload"))), 
             across(.cols = -c("region_nonbaseload_gen"), 
                    .fns = ~ if_else(region_nonbaseload_gen != 0, . / region_nonbaseload_gen, NA_real_), 
                    .names = "{str_replace(.col, 'gen', 'resource_mix')}")) %>% 
      select(contains("resource_mix")) %>% 
      distinct()
    
    
    ## Create final data frame -----
    
    #### Join necessary data -----
    
    region_merged <- 
      cbind(region_agg, 
            region_output_rates, # output emission rates
            region_input_rates, # input emission rates
            region_combustion_rates, # combustion emission rates
            region_fuel_rates, # output and input emission rates by fuel type
            region_fossil_rates, # output and input emission rates for all fossil fuels
            region_nonbaseload_rates, # output emission rates for nonbaseload generation
            region_gen_2, # generation by fuel category
            region_resource_mix, # resource mix by fuel category
            region_nonbaseload_gen, # nonbaseload generation by fuel category
            region_nonbaseload_resource_mix) %>% # nonbaseload resource mix
      mutate(across(contains("Hg"), ~replace_na(., "--")), # fill NAs in Hg with "--"
             across(where(is.numeric), ~replace_na(., 0))) %>% # fill NAs with 0
      select(-contains("gen_na"),
             -contains("mix_na")) # remove unnecessary columns
    
    
    #### Round data ----
    
    n2o_ch4_fuel_rates <- c("n2o_input_rate_coal", 
                            "n2o_input_rate_oil", 
                            "n2o_input_rate_gas", 
                            "n2o_input_rate_fossil", 
                            "n2o_output_rate_coal", 
                            "n2o_output_rate_oil", 
                            "n2o_output_rate_gas", 
                            "n2o_output_rate_fossil", 
                            "ch4_input_rate_coal", 
                            "ch4_input_rate_oil", 
                            "ch4_input_rate_gas", 
                            "ch4_input_rate_fossil", 
                            "ch4_output_rate_coal", 
                            "ch4_output_rate_oil", 
                            "ch4_output_rate_gas", 
                            "ch4_output_rate_fossil")
    
    region_rounded <- 
      region_merged %>% 
      mutate(across(c(where(is.numeric), -contains(n2o_ch4_fuel_rates)), 
                    \(x) round(x, 3)), # round to three decimals
             across(contains(n2o_ch4_fuel_rates), 
                    \(x) round(x, 4))) # round N2O and CH4 fuel specific rates to 4 decimal points
    
    
    #### Format to regional output -------
    
    region_formatted <- 
      region_rounded %>% 
      mutate(year = params$eGRID_year) %>% # set year as params$eGRID_year
      relocate(year) %>% # relocate to left most column 
      relocate(region_nox_output_rate_fossil, .after = region_nox_output_rate_gas) %>% 
      relocate(region_nox_oz_output_rate_fossil, .after = region_nox_oz_output_rate_gas) %>% 
      relocate(region_so2_output_rate_fossil, .after = region_so2_output_rate_gas) %>% 
      relocate(region_co2_output_rate_fossil, .after = region_co2_output_rate_gas) %>% 
      relocate(region_co2e_output_rate_fossil, .after = region_co2e_output_rate_gas) %>% 
      relocate(region_ch4_output_rate_fossil, .after = region_ch4_output_rate_gas) %>% 
      relocate(region_n2o_output_rate_fossil, .after = region_n2o_output_rate_gas) %>% 
      relocate(region_hg_output_rate_coal, .after = region_co2e_output_rate_fossil) %>% 
      relocate(region_hg_output_rate_fossil, .after = region_hg_output_rate_coal) %>% 
      relocate(region_nox_input_rate_fossil, .after = region_nox_input_rate_gas) %>% 
      relocate(region_nox_oz_input_rate_fossil, .after = region_nox_oz_input_rate_gas) %>% 
      relocate(region_so2_input_rate_fossil, .after = region_so2_input_rate_gas) %>% 
      relocate(region_co2_input_rate_fossil, .after = region_co2_input_rate_gas) %>% 
      relocate(region_co2e_input_rate_fossil, .after = region_co2e_input_rate_gas) %>% 
      relocate(region_ch4_input_rate_fossil, .after = region_ch4_input_rate_gas) %>% 
      relocate(region_n2o_input_rate_fossil, .after = region_n2o_input_rate_gas) %>% 
      relocate(region_hg_output_rate_coal, .after = region_co2e_output_rate_fossil) %>% 
      relocate(region_hg_input_rate_fossil, .after = region_hg_input_rate_coal) %>% 
      mutate(across(.cols = starts_with("region"), 
                    .fns = ~ .,
                    .names = "{str_replace(.col, 'region', {region})}")) %>% 
      select(-starts_with("region"))
    
    
    ## Export region aggregation file -----------
    
    if(dir.exists("data/outputs")) {
      print("Folder output already exists.")
    } else {
      dir.create("data/outputs")
    }
    
    if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
      print("Folder output already exists.")
    } else {
      dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
    }
    
    print(glue::glue("Saving {region} aggregation file to folder data/outputs/{params$eGRID_year}"))
    
    write_rds(region_formatted, glue::glue("data/outputs/{params$eGRID_year}/{region}_aggregation.RDS"))
    
  }
  
}
  
  
