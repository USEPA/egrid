
gen_rename <- function(df, alt = FALSE){
  clean_names <- # renaming column names to match naming conventions used in other files. This can be removed once new unit file is completed
      c("seq" = "SEQUENCE NUMBER", 
        "year" = "YEAR",
        "plant_state" = "STATE", 
        "plant_name" = "PLANT NAME",
        "plant_id" = "PLANT CODE",
        "generator_id" = "GENERATOR ID",
        "prime_mover" = "PRIME MOVER",
        "n_boilers" = "NUMBLR",
        "status" = "STATUS",
        "fuel_code" = "ENERGY SOURCE 1"  ,      
        "nameplate_capacity"  = "NAMEPLATE CAPACITY (MW)",
        "cfact" = "CFACT",
        "generation_ann" = "NETGEN",    
        "generation_oz" = "NETGENOZ",     
        "gen_data_source"   = "DATASOURCE",
        "operating_year" = "OPERATING YEAR",
        "retirement_year" = "RETIREMENT YEAR"
      )
  
  
  colnames(df) <- toupper(colnames(df))
  if(!"YEAR" %in% colnames(df)){
    df <- df %>% mutate(YEAR=2021)
  }
  
  df <- df %>%
    rename(all_of(clean_names))
  
}