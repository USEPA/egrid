
unit_rename <- function(df, alt = FALSE){
  clean_names <- # renaming column names to match naming conventions used in other files. This can be removed once new unit file is completed
    c("seq" = "SEQUNT",
      "year" = "YEAR",
      "plant_state" = "PSTATABB",
      "plant_name" = "PNAME",
      "plant_id" = "ORISPL",
      "unit_id" = "UNITID",
      "prime_mover" = "PRMVR",
      "operating_status" = "UNTOPST",
      "camd_flag" = "CAMDFLAG",
      "program_code" = "PRGCODE",
      "botfirty" = "BOTFIRTY",
      "n_generators" = "NUMGEN",
      "primary_fuel_type" = "FUELU1",
      "operating_hours" = "HRSOP",
      "heat_input" = "HTIAN",
      "heat_input_oz" = "HTIOZ",
      "nox_mass" = "NOXAN",
      "nox_oz" = "NOXOZ",
      "so2_mass" = "SO2AN",
      "co2_mass" = "CO2AN",
      "hg_mass" = "HGAN",
      "heat_input_source" = "HTIANSRC",
      "heat_input_oz_source" = "HTIOZSRC",
      "nox_source" = "NOXANSRC",
      "nox_oz_source" = "NOXOZSRC",
      "so2_source" = "SO2SRC",
      "co2_source" = "CO2SRC",
      "hg_source" = "HGSRC",
      "so2_controls" = "SO2CTLDV",
      "nox_controls" = "NOXCTLDV",
      "hg_controls" = "HGCTLDV",
      "year_online" = "UNTYRONL")
  
  if(alt){
    clean_names <- # renaming column names to match naming conventions used in other files. This can be removed once new unit file is completed
      c("seq" = "SEQUENCE NUMBER", # "SEQUNT",
        "year" = "YEAR",
        "plant_state" = "STATE", # "PSTATABB",
        "plant_name" = "FACILITY NAME",#"PNAME",
        "plant_id" = "ORIS CODE", #"ORISPL",
        "unit_id" = "UNIT ID",#"UNITID",
        "prime_mover" = "PRIMEMOVER",#"PRMVR",
        "operating_status" = "OP STATUS" ,#"UNTOPST",
        "camd_flag" = "CAMDFLAG",
        "program_code" = "PROGRAM",#"PRGCODE",
        "botfirty" = "BOTFIRTY",
        "n_generators" = "NUMGEN",
        "primary_fuel_type" = "FUEL TYPE (PRIMARY)",#"FUELU1",
        "operating_hours" = "ANNUAL SUM OP TIME",#"HRSOP",
        "heat_input" = "ANNUAL HEAT INPUT",#"HTIAN",
        "heat_input_oz" = "OZ SEAS HEAT INPUT", # "HTIOZ",
        "nox_mass" = "ANNUAL NOX MASS",
        "nox_oz" = "OZ SEAS NOX MASS",
        "so2_mass" =      "ANNUAL SO2 MASS" ,
        "co2_mass" =      "ANNUAL CO2 MASS" ,
        "hg_mass" = "MERCURY MASS (LBS)"  ,
        "heat_input_source" =  "HEAT INPUT SOURCE",
        "heat_input_oz_source" =     "HEAT INPUT OZ SOURCE",
        "nox_source" =  "NOX SOURCE"           ,
        "nox_oz_source" = "NOX OZ SOURCE"      ,
        "so2_source" =   "SO2 SOURCE"          ,
        "co2_source" = "CO2 SOURCE",
        "hg_source" =            "HG SOURCE"    ,
        "so2_controls" =         "SO2 CONTROLS"   ,
        "nox_controls" =       "NOX CONTROLS"     ,
        "hg_controls" = "HGCTLDV",
        "year_online" = "YEAR ONLINE")
    
  }
  
  colnames(df) <- toupper(colnames(df))
  if(!"YEAR" %in% colnames(df)){
    df <- df %>% mutate(YEAR=2021)
  }
  
  df <- df %>%
    rename(all_of(clean_names))
  
}