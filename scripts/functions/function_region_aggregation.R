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
  
  if (params$temporal_res == "annual") { 
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
        "coal_netgen",
        "oil_netgen",
        "gas_netgen",
        "nuclear_netgen",
        "hydro_netgen",
        "biomass_netgen",
        "wind_netgen",
        "solar_netgen",
        "geothermal_netgen",
        "other_ff_netgen",
        "other_netgen",
        "nonrenew_netgen",
        "renew_netgen",
        "nonrenew_other_netgen", 
        "renew_nonhydro_netgen",
        "combust_netgen",
        "noncombust_netgen",
        "noncombust_other_netgen"
      )} 
  if (params$temporal_res == "monthly") { 
    monthly_vars <- 
      c("capfac", 
        "nonbaseload", 
        "useful_thermal_output", 
        "power_heat_ratio", 
        "elec_allocation", 
        "combust_heat_input", 
        "heat_input", 
        "generation", 
        "generation_nonbaseload",
        "nox_mass", 
        "so2_mass", 
        "co2_mass", 
        "ch4_mass", 
        "n2o_mass", 
        "co2e_mass", 
        "hg_mass", 
        "coal_netgen", 
        "oil_netgen", 
        "gas_netgen", 
        "nuclear_netgen", 
        "hydro_netgen", 
        "biomass_netgen", 
        "wind_netgen", 
        "solar_netgen", 
        "geothermal_netgen", 
        "other_ff_netgen", 
        "other_netgen", 
        "nonrenew_netgen", 
        "renew_netgen", 
        "nonrenew_other_netgen", 
        "renew_nonhydro_netgen", 
        "combust_netgen", 
        "noncombust_netgen", 
        "noncombust_other_netgen")
    
    for (x in monthly_vars) { 
      assign(paste0(x, "_monthly"), paste0(x, "_", tolower(month.name)))}
    
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
        capfac_monthly, 
        "nonbaseload",
        nonbaseload_monthly, 
        "combust_heat_input",
        "combust_heat_input_oz",
        combust_heat_input_monthly, 
        "heat_input",
        "heat_input_oz",
        heat_input_monthly, 
        "generation_ann",
        "generation_oz",
        generation_monthly, 
        "generation_nonbaseload",
        generation_nonbaseload_monthly, 
        "nox_mass",
        "nox_oz_mass",
        nox_mass_monthly, 
        "so2_mass",
        so2_mass_monthly, 
        "co2_mass",
        co2_mass_monthly, 
        "ch4_mass",
        ch4_mass_monthly, 
        "n2o_mass",
        n2o_mass_monthly, 
        "co2e_mass",
        co2e_mass_monthly, 
        "hg_mass",
        hg_mass_monthly, 
        "coal_netgen",
        coal_netgen_monthly, 
        "oil_netgen",
        oil_netgen_monthly, 
        "gas_netgen",
        gas_netgen_monthly, 
        "nuclear_netgen",
        nuclear_netgen_monthly, 
        "hydro_netgen",
        hydro_netgen_monthly, 
        "biomass_netgen",
        biomass_netgen_monthly, 
        "wind_netgen",
        wind_netgen_monthly, 
        "solar_netgen",
        solar_netgen_monthly, 
        "geothermal_netgen",
        geothermal_netgen_monthly, 
        "other_ff_netgen",
        other_ff_netgen_monthly, 
        "other_netgen",
        other_netgen_monthly, 
        "nonrenew_netgen",
        nonrenew_netgen_monthly, 
        "renew_netgen",
        renew_netgen_monthly, 
        "nonrenew_other_netgen", 
        nonrenew_other_netgen_monthly, 
        "renew_nonhydro_netgen",
        renew_nonhydro_netgen_monthly, 
        "combust_netgen",
        combust_netgen_monthly, 
        "noncombust_netgen",
        noncombust_netgen_monthly, 
        "noncombust_other_netgen", 
        noncombust_other_netgen_monthly
      )} 
  
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
  
    # Aggregate plant file to region level -----------
    
    # sum capacity, generation, emissions mass to region level 
    
    region_agg <- 
      plant_file %>% 
      group_by(pick({{ region_cols }})) %>% 
      summarize(across(.cols = c("nameplate_capacity", 
                                 contains("heat_input"), 
                                 starts_with("generation"),
                                 contains("_mass"), 
                                 contains("netgen")), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      mutate(across(.cols = starts_with("region_hg_mass"), 
                    .fns = ~ "--")) %>% 
      ungroup()
  
    
    ## Calculate emission rates ------
    
    ### Output emission rates (lb/MWh) -----
    
    region_output_rates <- 
      region_agg %>% 
      rename("region_generation" = region_generation_ann) %>% 
      mutate(
        # calculate output emissions rates (lb/MWh)
        # output rates for NOx, SO2, CO2, and CO2e
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                    "so2_mass" = "generation", 
                                                                    "co2_mass" = "generation", 
                                                                    "co2e_mass" = "generation"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                                "so2_mass" = "generation", 
                                                                                "co2_mass" = "generation", 
                                                                                "co2e_mass" = "generation"))), 
                                NA_real_),
               .names = "{str_replace(.col, '_mass', '_output_rate')}"), 
        # output rate for NOx ozone
        region_nox_oz_output_rate = if_else(region_generation_oz != 0, 
                                            2000 * region_nox_oz_mass / region_generation_oz, NA_real_),
        # output rates for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                    "n2o_mass" = "generation"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                         "n2o_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_output_rate')}"),
        # assign "--" to Hg output rates
        across(.cols = starts_with("region_hg_mass"), 
               .fns = ~ "--", 
               .names = "{str_replace(.col, '_mass', '_output_rate')}")) %>% 
      select({{ region_cols }}, contains("rate"))

    
    ### Input emission rates (lb/MMBtu) -----
    
    region_input_rates <- 
      region_agg %>% 
      mutate(
        # calculate input emission rates (lb/MMBtu)
        # input rates for NOx, SO2, CO2, and CO2e
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                    "so2_mass" = "combust_heat_input", 
                                                                    "co2_mass" = "combust_heat_input", 
                                                                    "co2e_mass" = "combust_heat_input"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                               "so2_mass" = "combust_heat_input", 
                                                                               "co2_mass" = "combust_heat_input", 
                                                                               "co2e_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_input_rate')}"), 
        # input rate for NOx ozone 
        region_nox_oz_input_rate = if_else(region_combust_heat_input_oz != 0, 
                                           2000 * region_nox_oz_mass / region_combust_heat_input_oz, NA_real_),  
        # input rates for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                    "n2o_mass" = "combust_heat_input"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                         "n2o_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_input_rate')}"), 
        # assign "--" for Hg input rate
        across(.cols = starts_with("region_hg_mass"), 
               .fns = ~ "--", 
               .names = "{str_replace(.col, '_mass', '_input_rate')}")) %>% 
      select({{ region_cols }}, contains("rate"))
    
    
    ### Combustion emission rates (lb/MWh) -----
    
    region_combustion_rates <- 
      region_agg %>% 
      rename("region_generation" = region_generation_ann) %>% 
      mutate(
        # calculate combustion emissions rates (lb/MWh)
        # combustion rates for NOx, SO2, CO2, and CO2e 
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "combust_netgen", 
                                                                    "so2_mass" = "combust_netgen", 
                                                                    "co2_mass" = "combust_netgen", 
                                                                    "co2e_mass" = "combust_netgen"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "combust_netgen", 
                                                                                "so2_mass" = "combust_netgen", 
                                                                                "co2_mass" = "combust_netgen", 
                                                                                "co2e_mass" = "combust_netgen"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_combustion_rate')}"), 
        # combustion rate for NOx ozone
        region_nox_oz_combustion_rate = if_else(region_generation_oz != 0 & 
                                                  region_combust_netgen != 0 &
                                                  region_generation != 0, 
                                                2000 * region_nox_oz_mass / 
          (region_combust_netgen * (region_generation_oz / region_generation)), NA_real_), 
        # combustion rate for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "combust_netgen", 
                                                                    "n2o_mass" = "combust_netgen"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "combust_netgen", 
                                                                         "n2o_mass" = "combust_netgen"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_combustion_rate')}"), 
        # assign "--" to Hg combustion rate 
        across(.cols = starts_with("region_hg_mass"), 
               .fns = ~ "--", 
               .names = "{str_replace(.col, '_mass', '_combustion_rate')}")) %>% 
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
                                 starts_with("generation"), 
                                 -contains("nonbaseload"), 
                                 contains("_mass"), 
                                 starts_with("coal_netgen"), 
                                 starts_with("oil_netgen"), 
                                 starts_with("gas_netgen")), 
                       .fns = ~ sum(.x, na.rm = TRUE))) %>% 
      rename("generation" = generation_ann) %>% 
      mutate(
        # fuel specific output emission rates (lb/MWh)
        # fuel specific output rates for NOx, SO2, CO2, and CO2e
        across(.cols = c(starts_with("nox_mass"),  
                         starts_with("so2_mass"), 
                         starts_with("co2_mass"), 
                         starts_with("co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                    "so2_mass" = "generation", 
                                                                    "co2_mass" = "generation", 
                                                                    "co2e_mass" = "generation"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                                "so2_mass" = "generation", 
                                                                                "co2_mass" = "generation", 
                                                                                "co2e_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_output_rate')}"), 
        # fuel specific output rates for NOx ozone
        nox_oz_output_rate = if_else(generation_oz != 0, 
                                     2000 * nox_oz_mass / generation_oz, NA_real_), 
        # fuel specific output rates for CH4 and N2O
        across(.cols = c(starts_with("ch4_mass"), 
                         starts_with("n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                    "n2o_mass" = "generation"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                         "n2o_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_output_rate')}"),
        
        # fuel specific input emission rates (lb/MMBtu)
        # fuel specific input ratees for NOx, SO2, CO2, CO2e
        across(.cols = c(starts_with("nox_mass"),  
                         starts_with("so2_mass"), 
                         starts_with("co2_mass"), 
                         starts_with("co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                    "so2_mass" = "combust_heat_input", 
                                                                    "co2_mass" = "combust_heat_input", 
                                                                    "co2e_mass" = "combust_heat_input"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                                "so2_mass" = "combust_heat_input", 
                                                                                "co2_mass" = "combust_heat_input", 
                                                                                "co2e_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_input_rate')}"), 
        # fuel specific input rate for NOx ozone
        nox_oz_input_rate = if_else(combust_heat_input_oz != 0, 
                                    2000 * nox_oz_mass / combust_heat_input_oz, NA_real_),  
        # fuel specific input rates for CH4 and N2O 
        across(.cols = c(starts_with("ch4_mass"), 
                         starts_with("n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                    "n2o_mass" = "combust_heat_input"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                         "n2o_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_input_rate')}")) %>% 
      select({{ region_cols }}, primary_fuel_category, contains("rate"), contains("hg")) %>% 
      arrange(primary_fuel_category) %>% 
      pivot_wider(names_from = primary_fuel_category, # create columns for each fuel 
                  values_from = contains("rate"),
                  names_glue = "region_{primary_fuel_category}_{.value}") %>% 
      janitor::clean_names() %>% 
      mutate(across(where(is.numeric), ~ replace_na(.x, 0)), 
             # assign Hg coal and fossil output and input emissions rates
             # setting region Hg coal output rate to "--" 
             across(.cols = starts_with("hg_mass"), 
                    .fns = ~ "--", 
                    .names = "region_coal_{str_replace_all(.col, '_mass', '_output_rate')}"),
             # setting region Hg fossil output rate to "--" 
             across(.cols = starts_with("hg_mass"), 
                    .fns = ~ "--", 
                    .names = "region_fossil_{str_replace(.col, '_mass', '_output_rate')}"),
             # setting region Hg coal input rate to "--"  
             across(.cols = starts_with("hg_mass"), 
                    .fns = ~ "--", 
                    .names = "region_coal_{str_replace(.col, '_mass', '_input_rate')}"),
             # setting region Hg fossil input rate to "--" 
             across(.cols = starts_with("hg_mass"), 
                    .fns = ~ "--", 
                    .names = "region_fossil_{str_replace(.col, '_mass', '_input_rate')}")) %>% 
      ungroup() %>% 
      select({{ region_cols }}, contains("rate"))
    
    
    # calculate all fossil fuel output and input emission rates 
    
    region_fossil_rates <-
      plant_file %>% 
      group_by(pick({{ region_cols }})) %>% 
      filter(primary_fuel_category %in% fossil_fuels) %>% 
      summarize(across(.cols = c(starts_with("combust_heat_input"), 
                                 starts_with("generation"), -contains("nonbaseload"), 
                                 contains("mass")), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      rename("region_generation" = region_generation_ann) %>% 
      mutate(# output emission rates (lb/MWh)
        # fossil output rates for NOx, SO2, CO2, CO2e 
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                    "so2_mass" = "generation", 
                                                                    "co2_mass" = "generation", 
                                                                    "co2e_mass" = "generation"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                                "so2_mass" = "generation", 
                                                                                "co2_mass" = "generation", 
                                                                                "co2e_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace_all(.col, c('region' = 'region_fossil', '_mass' = '_output_rate'))}"), 
        # fossil output rate for NOx ozone
        region_fossil_nox_oz_output_rate = if_else(region_generation_oz != 0, 
                                                   2000 * region_nox_oz_mass / region_generation_oz, NA_real_),
        # fossil output rates for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                    "n2o_mass" = "generation"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                         "n2o_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace_all(.col, c('region' = 'region_fossil', '_mass' = '_output_rate'))}"),
        
        # input emission rates (lb/MMBtu)
        # fossil input emission rates for NOx, SO2, CO2, CO2e
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                    "so2_mass" = "combust_heat_input", 
                                                                    "co2_mass" = "combust_heat_input", 
                                                                    "co2e_mass" = "combust_heat_input"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                                "so2_mass" = "combust_heat_input", 
                                                                                "co2_mass" = "combust_heat_input", 
                                                                                "co2e_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace_all(.col, c('region' = 'region_fossil', '_mass' = '_input_rate'))}"), 
        # fossil input emission rates for NOx ozone 
        region_fossil_nox_oz_input_rate = if_else(region_combust_heat_input_oz != 0, 
                                                  2000 * region_nox_oz_mass / region_combust_heat_input_oz, 
                                                  NA_real_), 
        # fossil input emission rates for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                    "n2o_mass" = "combust_heat_input"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                         "n2o_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace_all(.col, c('region' = 'region_fossil', '_mass' = '_input_rate'))}"), 
        across(where(is.numeric), ~ replace_na(.x, 0))) %>% 
      select({{ region_cols }}, contains("rate")) %>% 
      ungroup()
    
    
    ### Nonbaseload output emission rates (lb/MWh) -----
    
    region_nonbaseload_rates <- 
      plant_file %>% 
      group_by(pick({{ region_cols }})) %>% 
      summarize(across(.cols = c(contains("_mass")), 
                       .fns = ~ sum(.x * get(str_replace_all(cur_column(), c("nox_mass" = "nonbaseload", 
                                                                             "nox_oz_mass" = "nonbaseload", 
                                                                             "so2_mass" = "nonbaseload", 
                                                                             "co2_mass" = "nonbaseload", 
                                                                             "ch4_mass" = "nonbaseload", 
                                                                             "n2o_mass" = "nonbaseload", 
                                                                             "co2e_mass" = "nonbaseload"))), na.rm = TRUE), 
                       .names = "region_{.col}"), 
                across(.cols = starts_with("generation_nonbaseload"), 
                       .fns = ~ sum(.x, na.rm = TRUE), 
                       .names = "region_{.col}"), 
                region_generation_oz_nonbaseload = sum(nonbaseload * generation_oz, na.rm = TRUE)) %>% 
      mutate(# nonbaseload output rate for NOx, SO2, CO2, CO2e 
             across(.cols = c(starts_with("region_nox_mass"),  
                              starts_with("region_so2_mass"), 
                              starts_with("region_co2_mass"), 
                              starts_with("region_co2e_mass")), 
                    .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "generation_nonbaseload", 
                                                                         "so2_mass" = "generation_nonbaseload", 
                                                                         "co2_mass" = "generation_nonbaseload", 
                                                                         "co2e_mass" = "generation_nonbaseload"))) != 0, 
                                     2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "generation_nonbaseload", 
                                                                                     "so2_mass" = "generation_nonbaseload", 
                                                                                     "co2_mass" = "generation_nonbaseload", 
                                                                                     "co2e_mass" = "generation_nonbaseload"))), 
                                     NA_real_), 
                    .names = "{str_replace(.col, '_mass', '_output_rate_nonbaseload')}"), 
             # nonbaseload output rate for ozone NOx 
             region_nox_oz_output_rate_nonbaseload = if_else(region_generation_oz_nonbaseload != 0, 
                                                             2000 * region_nox_oz_mass / region_generation_oz_nonbaseload, 
                                                             NA_real_), 
             # nonbaseload output rate for CH4 and N2O 
             across(.cols = c(starts_with("region_ch4_mass"), 
                              starts_with("region_n2o_mass")),
                    .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "generation_nonbaseload", 
                                                                         "n2o_mass" = "generation_nonbaseload"))) != 0, 
                                     .x / get(str_replace_all(cur_column(), c("ch4_mass" = "generation_nonbaseload", 
                                                                              "n2o_mass" = "generation_nonbaseload"))), 
                                     NA_real_), 
                    .names = "{str_replace(.col, '_mass', '_output_rate_nonbaseload')}"),
             # assign "--" to nonbaseload Hg output rate 
             across(.cols = starts_with("region_hg_mass"), 
                    .fns = ~ "--", 
                    .names = "{str_replace(.col, '_mass', '_output_rate_nonbaseload')}")) %>% 
      select({{ region_cols }}, contains("rate")) %>% 
      ungroup()
    
    
    ## Calculate net generation and resource mix -----
    
    ### Generation by fuel category (MWh) -----
    
    region_gen <- 
      region_agg %>% 
      select({{ region_cols }}, contains("gen")) 
    
    region_gen_2 <- 
      region_gen %>% 
      select(-starts_with("region_generation")) # remove duplicate columns for final formatting
    
    ### Resource mix by fuel category (%) -----
    
    region_resource_mix <- 
      region_gen %>% 
      select(-region_generation_oz, -starts_with("region_generation_nonbaseload")) %>% 
      rename("region_generation" = region_generation_ann) %>% 
      mutate(#calculate resource mix percentage for each fuel type
             across(.cols = contains("netgen"), 
                    .fns = ~ if_else(get(str_replace_all(cur_column(), c("coal_netgen" = "generation", 
                                                                         "oil_netgen" = "generation", 
                                                                         "gas_netgen" = "generation", 
                                                                         "nuclear_netgen" = "generation",
                                                                         "biomass_netgen" = "generation", 
                                                                         "wind_netgen" = "generation", 
                                                                         "solar_netgen" = "generation",
                                                                         "geothermal_netgen" = "generation",
                                                                         "other_ff_netgen" = "generation",
                                                                         "nonrenew_other_netgen" = "generation",
                                                                         "nonrenew_netgen" = "generation",
                                                                         "renew_netgen" = "generation",
                                                                         "renew_nonhydro_netgen" = "generation",
                                                                         "noncombust_netgen" = "generation",
                                                                         "noncombust_other_netgen" = "generation",
                                                                         "combust_netgen" = "generation",
                                                                         "other_netgen" = "generation",
                                                                         "hydro_netgen" = "generation"))) != 0, 
                                     .x / get(str_replace_all(cur_column(), c("coal_netgen" = "generation", 
                                                                              "oil_netgen" = "generation", 
                                                                              "gas_netgen" = "generation", 
                                                                              "nuclear_netgen" = "generation",
                                                                              "biomass_netgen" = "generation", 
                                                                              "wind_netgen" = "generation", 
                                                                              "solar_netgen" = "generation",
                                                                              "geothermal_netgen" = "generation",
                                                                              "other_ff_netgen" = "generation",
                                                                              "nonrenew_other_netgen" = "generation",
                                                                              "nonrenew_netgen" = "generation",
                                                                              "renew_netgen" = "generation",
                                                                              "renew_nonhydro_netgen" = "generation",
                                                                              "noncombust_netgen" = "generation",
                                                                              "noncombust_other_netgen" = "generation",
                                                                              "combust_netgen" = "generation",
                                                                              "other_netgen" = "generation",
                                                                              "hydro_netgen" = "generation"))), 
                                     NA_real_), # convert to percentage 
                    .names = "{str_replace(.col, 'netgen', 'resource_mix')}")) %>% 
      select({{ region_cols }}, contains("resource_mix"))
    
    
    ### Nonbaseload generation (MWh) -----
    
    region_nonbaseload_gen <- 
      plant_file %>% 
      group_by(pick({{ region_cols }}), primary_fuel_category) %>% 
      summarize(across(.cols = starts_with("generation_nonbaseload"), 
                       .fns = ~ sum(.x, na.rm = TRUE))) %>% 
      arrange(primary_fuel_category) %>% 
      pivot_wider(names_from = primary_fuel_category, 
                  values_from = starts_with("generation_nonbaseload"),
                  names_glue = "region_{primary_fuel_category}_{.value}") %>% 
      janitor::clean_names() %>% 
      mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>% 
      ungroup() %>% 
      select({{ region_cols }}, contains("generation_nonbaseload"))
    
    ### Nonbaseload resource mix (%) -----
    
    region_nonbaseload_resource_mix <- 
      region_nonbaseload_gen %>% 
      left_join(region_agg %>% select({{ region_cols }}, contains("generation_nonbaseload"))) %>% 
      mutate(# calculate nonbaseload resource mix for each fuel type 
             across(.cols = -any_of(c({{ region_cols }}, 
                                    "region_generation_nonbaseload", 
                                    paste0("region_generation_nonbaseload_", tolower(month.name)))), 
                    .fns = ~ if_else(get(str_replace_all(cur_column(), c("_coal" = "", 
                                                                         "_oil" = "", 
                                                                         "_gas" = "", 
                                                                         "_nuclear" = "",
                                                                         "_hydro" = "", 
                                                                         "_biomass" = "", 
                                                                         "_wind" = "", 
                                                                         "_solar" = "",
                                                                         "_geothermal" = "",
                                                                         "_other_ff" = "",
                                                                         "_other" = "",
                                                                         "_nonrenew" = "",
                                                                         "_renew" = "",
                                                                         "_nonrenew_other" = "",
                                                                         "_renew_nonhydro" = "",
                                                                         "_combust" = "",
                                                                         "_noncombust" = "",
                                                                         "_noncombust_other" = ""))) != 0, 
                                     .x / get(str_replace_all(cur_column(), c("_coal" = "", 
                                                                              "_oil" = "", 
                                                                              "_gas" = "", 
                                                                              "_nuclear" = "",
                                                                              "_hydro" = "", 
                                                                              "_biomass" = "", 
                                                                              "_wind" = "", 
                                                                              "_solar" = "",
                                                                              "_geothermal" = "",
                                                                              "_other_ff" = "",
                                                                              "_other" = "",
                                                                              "_nonrenew" = "",
                                                                              "_renew" = "",
                                                                              "_nonrenew_other" = "",
                                                                              "_renew_nonhydro" = "",
                                                                              "_combust" = "",
                                                                              "_noncombust" = "",
                                                                              "_noncombust_other" = ""))), 
                                     NA_real_), 
                    .names = "{str_replace(.col, 'generation', 'resource_mix')}")) %>% 
      select({{ region_cols }}, contains("resource_mix"))
    
    
    ## Create final data frame -----
    
    ### Join necessary data -----
    
    region_merged <- 
      region_agg %>% select(-contains("netgen")) %>% 
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
      mutate(across(contains("Hg"), ~ replace_na(.x, "--")), # fill NAs in Hg with "--"
             across(where(is.numeric), ~ replace_na(.x, 0))) %>% # fill NAs with 0
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
                    ~ round(.x, 3)), # round to three decimals
             across(contains(n2o_ch4_fuel_rates), 
                    ~ round(.x, 4))) # round N2O and CH4 fuel specific rates to 4 decimal points
    
    
    ### Format to regional output -------
    if (params$temporal_res == "annual") { 
      final_vars <- get(glue::glue("{region}_nonmetric"))}
    if (params$temporal_res == "monthly") { 
      final_vars <- get(glue::glue("{region}_nonmetric_monthly"))}
      
    region_formatted <- 
      region_rounded %>% 
      mutate(year = params$eGRID_year,
             across(.cols = starts_with("region_"), # replace region with specific region name
                    .fns = ~ .x, 
                    .names = "{str_replace(.col, 'region', {region})}")) %>% # set year as params$eGRID_year
      select(as_tibble(final_vars)$value) # order final vars with snake_case_names

  
    ### Export region aggregation file -----------
    
    if(dir.exists("data/outputs")) {
      print("Folder outputs already exists.")
    } else {
      dir.create("data/outputs")
    }
    
    if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
      print(glue::glue("Folder outputs/{params$eGRID_year} already exists."))
    } else {
      dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
    }
    
    print(glue::glue("Saving {toupper(region)} aggregation file to folder data/outputs/{params$eGRID_year}"))
    
    write_rds(region_formatted, glue::glue("data/outputs/{params$eGRID_year}/{region}_aggregation.RDS")) 
    
  } else {

    # Aggregate plant file to region level -----------
    
    # sum capacity, generation, emissions mass to region level 
    
    region_agg <- 
      plant_file %>% 
      summarize(across(.cols = c("nameplate_capacity", 
                                 contains("heat_input"), 
                                 starts_with("generation"),
                                 contains("_mass"), 
                                 contains("netgen")), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      mutate(across(.cols = starts_with("region_hg_mass"), 
                    .fns = ~ "--")) %>% 
      ungroup()
    
    
    ## Calculate emission rates ------
    
    ### Output emission rates (lb/MWh) -----
    
    region_output_rates <- 
      region_agg %>% 
      rename("region_generation" = region_generation_ann) %>% 
      mutate(
        # calculate output emissions rates (lb/MWh)
        # output rates for NOx, SO2, CO2, and CO2e
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                    "so2_mass" = "generation", 
                                                                    "co2_mass" = "generation", 
                                                                    "co2e_mass" = "generation"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                                "so2_mass" = "generation", 
                                                                                "co2_mass" = "generation", 
                                                                                "co2e_mass" = "generation"))), 
                                NA_real_),
               .names = "{str_replace(.col, '_mass', '_output_rate')}"), 
        # output rate for NOx ozone
        region_nox_oz_output_rate = if_else(region_generation_oz != 0, 
                                            2000 * region_nox_oz_mass / region_generation_oz, NA_real_),
        # output rates for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                    "n2o_mass" = "generation"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                         "n2o_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_output_rate')}"),
        # assign "--" to Hg output rates
        across(.cols = starts_with("region_hg_mass"), 
               .fns = ~ "--", 
               .names = "{str_replace(.col, '_mass', '_output_rate')}")) %>% 
      select(contains("rate"))
    
    
    ### Input emission rates (lb/MMBtu) -----
    
    region_input_rates <- 
      region_agg %>% 
      mutate(
        # calculate input emission rates (lb/MMBtu)
        # input rates for NOx, SO2, CO2, and CO2e
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                    "so2_mass" = "combust_heat_input", 
                                                                    "co2_mass" = "combust_heat_input", 
                                                                    "co2e_mass" = "combust_heat_input"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                                "so2_mass" = "combust_heat_input", 
                                                                                "co2_mass" = "combust_heat_input", 
                                                                                "co2e_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_input_rate')}"), 
        # input rate for NOx ozone 
        region_nox_oz_input_rate = if_else(region_combust_heat_input_oz != 0, 
                                           2000 * region_nox_oz_mass / region_combust_heat_input_oz, NA_real_),  
        # input rates for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                    "n2o_mass" = "combust_heat_input"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                         "n2o_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_input_rate')}"), 
        # assign "--" for Hg input rate
        across(.cols = starts_with("region_hg_mass"), 
               .fns = ~ "--", 
               .names = "{str_replace(.col, '_mass', '_input_rate')}")) %>% 
      select(contains("rate"))
    
    
    ### Combustion emission rates (lb/MWh) -----
    
    region_combustion_rates <- 
      region_agg %>% 
      rename("region_generation" = region_generation_ann) %>% 
      mutate(
        # calculate combustion emissions rates (lb/MWh)
        # combustion rates for NOx, SO2, CO2, and CO2e 
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "combust_netgen", 
                                                                    "so2_mass" = "combust_netgen", 
                                                                    "co2_mass" = "combust_netgen", 
                                                                    "co2e_mass" = "combust_netgen"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "combust_netgen", 
                                                                                "so2_mass" = "combust_netgen", 
                                                                                "co2_mass" = "combust_netgen", 
                                                                                "co2e_mass" = "combust_netgen"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_combustion_rate')}"), 
        # combustion rate for NOx ozone
        region_nox_oz_combustion_rate = if_else(region_generation_oz != 0 & 
                                                  region_combust_netgen != 0 &
                                                  region_generation != 0, 
                                                2000 * region_nox_oz_mass / 
                                                  (region_combust_netgen * (region_generation_oz / region_generation)), NA_real_), 
        # combustion rate for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "combust_netgen", 
                                                                    "n2o_mass" = "combust_netgen"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "combust_netgen", 
                                                                         "n2o_mass" = "combust_netgen"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_combustion_rate')}"), 
        # assign "--" to Hg combustion rate 
        across(.cols = starts_with("region_hg_mass"), 
               .fns = ~ "--", 
               .names = "{str_replace(.col, '_mass', '_combustion_rate')}")) %>% 
      select(contains("rate")) 
    
    
    ### Fuel type output emission rates (lb/MWh) and input emission rates (lb/MMBtu)  -----
    
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
                                 starts_with("generation"), 
                                 -contains("nonbaseload"), 
                                 contains("mass"), 
                                 starts_with("coal_netgen"), 
                                 starts_with("oil_netgen"), 
                                 starts_with("gas_netgen")), 
                       .fns = ~ sum(.x, na.rm = TRUE))) %>% 
      rename("generation" = generation_ann) %>% 
      mutate(
        # fuel specific output emission rates (lb/MWh)
        # fuel specific output rates for NOx, SO2, CO2, and CO2e
        across(.cols = c(starts_with("nox_mass"),  
                         starts_with("so2_mass"), 
                         starts_with("co2_mass"), 
                         starts_with("co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                    "so2_mass" = "generation", 
                                                                    "co2_mass" = "generation", 
                                                                    "co2e_mass" = "generation"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                                "so2_mass" = "generation", 
                                                                                "co2_mass" = "generation", 
                                                                                "co2e_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_output_rate')}"), 
        # fuel specific output rates for NOx ozone
        nox_oz_output_rate = if_else(generation_oz != 0, 
                                     2000 * nox_oz_mass / generation_oz, NA_real_), 
        # fuel specific output rates for CH4 and N2O
        across(.cols = c(starts_with("ch4_mass"), 
                         starts_with("n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                    "n2o_mass" = "generation"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                         "n2o_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_output_rate')}"),
        
        # fuel specific input emission rates (lb/MMBtu)
        # fuel specific input ratees for NOx, SO2, CO2, CO2e
        across(.cols = c(starts_with("nox_mass"),  
                         starts_with("so2_mass"), 
                         starts_with("co2_mass"), 
                         starts_with("co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                    "so2_mass" = "combust_heat_input", 
                                                                    "co2_mass" = "combust_heat_input", 
                                                                    "co2e_mass" = "combust_heat_input"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                                "so2_mass" = "combust_heat_input", 
                                                                                "co2_mass" = "combust_heat_input", 
                                                                                "co2e_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_input_rate')}"), 
        # fuel specific input rate for NOx ozone
        nox_oz_input_rate = if_else(combust_heat_input_oz != 0, 
                                    2000 * nox_oz_mass / combust_heat_input_oz, NA_real_),  
        # fuel specific input rates for CH4 and N2O 
        across(.cols = c(starts_with("ch4_mass"), 
                         starts_with("n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                    "n2o_mass" = "combust_heat_input"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                         "n2o_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_input_rate')}")) %>% 
      select(primary_fuel_category, contains("rate"), contains("hg")) %>% 
      arrange(primary_fuel_category) %>% 
      pivot_wider(# create columns for each fuel 
                  names_from = primary_fuel_category, 
                  values_from = contains("rate"),
                  names_glue = "region_{primary_fuel_category}_{.value}") %>% 
      janitor::clean_names() %>% 
      mutate(across(where(is.numeric), ~ replace_na(.x, 0)), 
             # assign Hg coal and fossil output and input emissions rates
             # setting region Hg coal output rate to "--" 
             across(.cols = starts_with("hg_mass"), 
                    .fns = ~ "--", 
                    .names = "region_coal_{str_replace_all(.col, '_mass', '_output_rate')}"),
             # setting region Hg fossil output rate to "--" 
             across(.cols = starts_with("hg_mass"), 
                    .fns = ~ "--", 
                    .names = "region_fossil_{str_replace(.col, '_mass', '_output_rate')}"),
             # setting region Hg coal input rate to "--"  
             across(.cols = starts_with("hg_mass"), 
                    .fns = ~ "--", 
                    .names = "region_coal_{str_replace(.col, '_mass', '_input_rate')}"),
             # setting region Hg fossil input rate to "--" 
             across(.cols = starts_with("hg_mass"), 
                    .fns = ~ "--", 
                    .names = "region_fossil_{str_replace(.col, '_mass', '_input_rate')}")) %>% 
      ungroup() %>% 
      select(contains("rate"))
    
    
    # calculate all fossil fuel output and input emission rates 
    
    region_fossil_rates <-
      plant_file %>% 
      filter(primary_fuel_category %in% fossil_fuels) %>% 
      summarize(across(.cols = c(starts_with("combust_heat_input"), 
                                 starts_with("generation"), -contains("nonbaseload"), 
                                 contains("mass")), 
                       .fns = ~ sum(.x, na.rm = TRUE),
                       .names = "region_{.col}")) %>% 
      rename("region_generation" = region_generation_ann) %>% 
      mutate(# output emission rates (lb/MWh)
        # fossil output rates for NOx, SO2, CO2, CO2e 
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                    "so2_mass" = "generation", 
                                                                    "co2_mass" = "generation", 
                                                                    "co2e_mass" = "generation"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "generation", 
                                                                                "so2_mass" = "generation", 
                                                                                "co2_mass" = "generation", 
                                                                                "co2e_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace_all(.col, c('region' = 'region_fossil', '_mass' = '_output_rate'))}"), 
        # fossil output rate for NOx ozone
        region_fossil_nox_oz_output_rate = if_else(region_generation_oz != 0, 
                                                   2000 * region_nox_oz_mass / region_generation_oz, NA_real_),
        # fossil output rates for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                    "n2o_mass" = "generation"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "generation", 
                                                                         "n2o_mass" = "generation"))), 
                                NA_real_), 
               .names = "{str_replace_all(.col, c('region' = 'region_fossil', '_mass' = '_output_rate'))}"),
        
        # input emission rates (lb/MMBtu)
        # fossil input emission rates for NOx, SO2, CO2, CO2e
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                    "so2_mass" = "combust_heat_input", 
                                                                    "co2_mass" = "combust_heat_input", 
                                                                    "co2e_mass" = "combust_heat_input"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "combust_heat_input", 
                                                                                "so2_mass" = "combust_heat_input", 
                                                                                "co2_mass" = "combust_heat_input", 
                                                                                "co2e_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace_all(.col, c('region' = 'region_fossil', '_mass' = '_input_rate'))}"), 
        # fossil input emission rates for NOx ozone 
        region_fossil_nox_oz_input_rate = if_else(region_combust_heat_input_oz != 0, 
                                                  2000 * region_nox_oz_mass / region_combust_heat_input_oz, 
                                                  NA_real_), 
        # fossil input emission rates for CH4 and N2O
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                    "n2o_mass" = "combust_heat_input"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "combust_heat_input", 
                                                                         "n2o_mass" = "combust_heat_input"))), 
                                NA_real_), 
               .names = "{str_replace_all(.col, c('region' = 'region_fossil', '_mass' = '_input_rate'))}"), 
        across(where(is.numeric), ~ replace_na(.x, 0))) %>% 
      select(contains("rate")) %>% 
      ungroup()
    
    
    ### Nonbaseload output emission rates (lb/MWh) -----
    
    region_nonbaseload_rates <- 
      plant_file %>% 
      summarize(across(.cols = c(contains("_mass")), 
                       .fns = ~ sum(.x * get(str_replace_all(cur_column(), c("nox_mass" = "nonbaseload", 
                                                                             "nox_oz_mass" = "nonbaseload", 
                                                                             "so2_mass" = "nonbaseload", 
                                                                             "co2_mass" = "nonbaseload", 
                                                                             "ch4_mass" = "nonbaseload", 
                                                                             "n2o_mass" = "nonbaseload", 
                                                                             "co2e_mass" = "nonbaseload"))), na.rm = TRUE), 
                       .names = "region_{.col}"), 
                across(.cols = starts_with("generation_nonbaseload"), 
                       .fns = ~ sum(.x, na.rm = TRUE), 
                       .names = "region_{.col}"), 
                region_generation_oz_nonbaseload = sum(nonbaseload * generation_oz, na.rm = TRUE)) %>% 
      mutate(# nonbaseload output rate for NOx, SO2, CO2, CO2e 
        across(.cols = c(starts_with("region_nox_mass"),  
                         starts_with("region_so2_mass"), 
                         starts_with("region_co2_mass"), 
                         starts_with("region_co2e_mass")), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("nox_mass" = "generation_nonbaseload", 
                                                                    "so2_mass" = "generation_nonbaseload", 
                                                                    "co2_mass" = "generation_nonbaseload", 
                                                                    "co2e_mass" = "generation_nonbaseload"))) != 0, 
                                2000 * .x / get(str_replace_all(cur_column(), c("nox_mass" = "generation_nonbaseload", 
                                                                                "so2_mass" = "generation_nonbaseload", 
                                                                                "co2_mass" = "generation_nonbaseload", 
                                                                                "co2e_mass" = "generation_nonbaseload"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_output_rate_nonbaseload')}"), 
        # nonbaseload output rate for ozone NOx 
        region_nox_oz_output_rate_nonbaseload = if_else(region_generation_oz_nonbaseload != 0, 
                                                        2000 * region_nox_oz_mass / region_generation_oz_nonbaseload, 
                                                        NA_real_), 
        # nonbaseload output rate for CH4 and N2O 
        across(.cols = c(starts_with("region_ch4_mass"), 
                         starts_with("region_n2o_mass")),
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("ch4_mass" = "generation_nonbaseload", 
                                                                    "n2o_mass" = "generation_nonbaseload"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("ch4_mass" = "generation_nonbaseload", 
                                                                         "n2o_mass" = "generation_nonbaseload"))), 
                                NA_real_), 
               .names = "{str_replace(.col, '_mass', '_output_rate_nonbaseload')}"),
        # assign "--" to nonbaseload Hg output rate 
        across(.cols = starts_with("region_hg_mass"), 
               .fns = ~ "--", 
               .names = "{str_replace(.col, '_mass', '_output_rate_nonbaseload')}")) %>% 
      select(contains("rate")) %>% 
      ungroup()
    
    
    ## Calculate net generation and resource mix -----
    
    ### Generation by fuel category (MWh) -----
    
    region_gen <- 
      region_agg %>% 
      select(contains("gen")) 
    
    region_gen_2 <- 
      region_gen %>% 
      select(-starts_with("region_generation")) # remove duplicate columns for final formatting
    
    ### Resource mix by fuel category (%) -----
    
    region_resource_mix <- 
      region_gen %>% 
      select(-region_generation_oz, -starts_with("region_generation_nonbaseload")) %>% 
      rename("region_generation" = region_generation_ann) %>% 
      mutate(#calculate resource mix percentage for each fuel type
        across(.cols = contains("netgen"), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("coal_netgen" = "generation", 
                                                                    "oil_netgen" = "generation", 
                                                                    "gas_netgen" = "generation", 
                                                                    "nuclear_netgen" = "generation",
                                                                    "biomass_netgen" = "generation", 
                                                                    "wind_netgen" = "generation", 
                                                                    "solar_netgen" = "generation",
                                                                    "geothermal_netgen" = "generation",
                                                                    "other_ff_netgen" = "generation",
                                                                    "nonrenew_other_netgen" = "generation",
                                                                    "nonrenew_netgen" = "generation",
                                                                    "renew_netgen" = "generation",
                                                                    "renew_nonhydro_netgen" = "generation",
                                                                    "noncombust_netgen" = "generation",
                                                                    "noncombust_other_netgen" = "generation",
                                                                    "combust_netgen" = "generation",
                                                                    "other_netgen" = "generation",
                                                                    "hydro_netgen" = "generation"))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("coal_netgen" = "generation", 
                                                                         "oil_netgen" = "generation", 
                                                                         "gas_netgen" = "generation", 
                                                                         "nuclear_netgen" = "generation",
                                                                         "biomass_netgen" = "generation", 
                                                                         "wind_netgen" = "generation", 
                                                                         "solar_netgen" = "generation",
                                                                         "geothermal_netgen" = "generation",
                                                                         "other_ff_netgen" = "generation",
                                                                         "nonrenew_other_netgen" = "generation",
                                                                         "nonrenew_netgen" = "generation",
                                                                         "renew_netgen" = "generation",
                                                                         "renew_nonhydro_netgen" = "generation",
                                                                         "noncombust_netgen" = "generation",
                                                                         "noncombust_other_netgen" = "generation",
                                                                         "combust_netgen" = "generation",
                                                                         "other_netgen" = "generation",
                                                                         "hydro_netgen" = "generation"))), 
                                NA_real_), # convert to percentage 
               .names = "{str_replace(.col, 'netgen', 'resource_mix')}")) %>% 
      select(contains("resource_mix"))
    
    
    ### Nonbaseload generation (MWh) -----
    
    region_nonbaseload_gen <- 
      plant_file %>% 
      group_by(primary_fuel_category) %>% 
      summarize(across(.cols = starts_with("generation_nonbaseload"), 
                       .fns = ~ sum(.x, na.rm = TRUE))) %>% 
      arrange(primary_fuel_category) %>% 
      pivot_wider(names_from = primary_fuel_category, 
                  values_from = starts_with("generation_nonbaseload"),
                  names_glue = "region_{primary_fuel_category}_{.value}") %>% 
      janitor::clean_names() %>% 
      mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>% 
      ungroup() %>% 
      select(contains("generation_nonbaseload"))
    
    ### Nonbaseload resource mix (%) -----
    
    region_nonbaseload_resource_mix <- 
      cbind(region_nonbaseload_gen, 
            region_agg %>% select(starts_with("region_generation_nonbaseload"))) %>% 
      mutate(# calculate nonbaseload resource mix for each fuel type 
        across(.cols = -c(region_generation_nonbaseload, 
                          paste0("region_generation_nonbaseload_", tolower(month.name))), 
               .fns = ~ if_else(get(str_replace_all(cur_column(), c("_coal" = "", 
                                                                    "_oil" = "", 
                                                                    "_gas" = "", 
                                                                    "_nuclear" = "",
                                                                    "_hydro" = "", 
                                                                    "_biomass" = "", 
                                                                    "_wind" = "", 
                                                                    "_solar" = "",
                                                                    "_geothermal" = "",
                                                                    "_other_ff" = "",
                                                                    "_other" = "",
                                                                    "_nonrenew" = "",
                                                                    "_renew" = "",
                                                                    "_nonrenew_other" = "",
                                                                    "_renew_nonhydro" = "",
                                                                    "_combust" = "",
                                                                    "_noncombust" = "",
                                                                    "_noncombust_other" = ""))) != 0, 
                                .x / get(str_replace_all(cur_column(), c("_coal" = "", 
                                                                         "_oil" = "", 
                                                                         "_gas" = "", 
                                                                         "_nuclear" = "",
                                                                         "_hydro" = "", 
                                                                         "_biomass" = "", 
                                                                         "_wind" = "", 
                                                                         "_solar" = "",
                                                                         "_geothermal" = "",
                                                                         "_other_ff" = "",
                                                                         "_other" = "",
                                                                         "_nonrenew" = "",
                                                                         "_renew" = "",
                                                                         "_nonrenew_other" = "",
                                                                         "_renew_nonhydro" = "",
                                                                         "_combust" = "",
                                                                         "_noncombust" = "",
                                                                         "_noncombust_other" = ""))), 
                                NA_real_), 
               .names = "{str_replace(.col, 'generation', 'resource_mix')}")) %>% 
      select(contains("resource_mix"))
    
    
    ## Create final data frame -----
    
    ### Join necessary data -----
    
    region_merged <- 
      cbind(region_agg %>% select(-contains("netgen")),
            region_output_rates, # output emission rates
            region_input_rates, # input emission rates
            region_combustion_rates, # combustion emission rates
            region_fuel_rates, # output and input emission rates by fuel type
            region_fossil_rates, # output and input emission rates for all fossil fuels
            region_nonbaseload_rates, # output emission rates for nonbaseload generation
            region_gen_2, # generation by fuel category
            region_resource_mix, # resource mix by fuel category
            region_nonbaseload_gen, # nonbaesload generation by fuel cateogry
            region_nonbaseload_resource_mix) %>% # nonbaseload resource mix by fuel category
      mutate(across(contains("Hg"), ~ replace_na(.x, "--")), # fill NAs in Hg with "--"
             across(where(is.numeric), ~ replace_na(.x, 0))) # fill NAs with 0

    
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
                    ~ round(.x, 3)), # round to three decimals
             across(contains(n2o_ch4_fuel_rates), 
                    ~ round(.x, 4))) # round N2O and CH4 fuel specific rates to 4 decimal points
    
    
    ### Format to regional output -------
    if (params$temporal_res == "annual") { 
      final_vars <- get(glue::glue("{region}_nonmetric"))}
    if (params$temporal_res == "monthly") { 
      final_vars <- get(glue::glue("{region}_nonmetric_monthly"))}
    
    region_formatted <- 
      region_rounded %>% 
      mutate(year = params$eGRID_year,
             across(.cols = starts_with("region_"), # replace region with specific region name
                    .fns = ~ .x, 
                    .names = "{str_replace(.col, 'region', {region})}")) %>% # set year as params$eGRID_year
      select(as_tibble(final_vars)$value) # order final vars with snake_case_names
    
    
    ### Export region aggregation file -----------
    
    if(dir.exists("data/outputs")) {
      print("Folder outputs already exists.")
    } else {
      dir.create("data/outputs")
    }
    
    if(dir.exists(glue::glue("data/outputs/{params$eGRID_year}"))) {
      print(glue::glue("Folder outputs/{params$eGRID_year} already exists."))
    } else {
      dir.create(glue::glue("data/outputs/{params$eGRID_year}"))
    }
    
    print(glue::glue("Saving {toupper(region)} aggregation file to folder data/outputs/{params$eGRID_year}"))
    
    write_rds(region_formatted, glue::glue("data/outputs/{params$eGRID_year}/{region}_aggregation.RDS")) 
  }   
  
}
  
  
