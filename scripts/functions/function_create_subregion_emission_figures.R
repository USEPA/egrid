## -------------------------------
## 
## Create subregion emissions figures
##
## Purpose: 
## 
## This function plots PM2.5, NH3, or VOC subregion emissions data
## and saves figures for the current eGRID year and previous years
## to be used in the final .xlsx sheet of data
##
## Resulting figures are saved in the following folder:
## "data/2b_pm_nh3_voc/static_tables/formatting/"
##
## Images are included in the final .xlsx files for emisisons data
## using the final_formatting_pm_nh3_voc.R script
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

create_subregion_emissions_figures <- function(emission_type) {
  
  #' create_subregion_emissions_figures
  #' 
  #' Function to plot pm2.5, nh3, or voc subregion annual generation, emissions, 
  #' and output rates data
  #' 
  #' @param emission_type Emission type to produce plots for - either
  #'                      "pm25", "nh3", or "voc"
  #' 
  #' @return Saved figures of subregion annual generation, emissions, and output rates 
  #' for each year from 2018 to current eGRID year - saved in 
  #' "data/2b_pm_nh3_voc/static_tables/formatting/"
  #'         
  #' @examples 
  #' # Create PM2.5 plots
  #' pm25_annual_generation <- plot_subregion_emissions(emission_type = "pm25")
  
  # Require Libraries ---------
  require(ggplot2)
  require(readr)
  require(readxl)
  require(scales)
  
  # Create Nested Functions ------
  
  ## Plotting function -----
  plot_subregion_emissions <- function(ydata,
                                       fill_color,
                                       ylabel,
                                       annotate_label,
                                       ylabel_min,
                                       ylabel_max,
                                       ylabel_int,
                                       yaxis_max) {
    
    plot <- ggplot(subregion_file, aes(x = subregion, y = get(ydata))) +
      geom_col(fill = fill_color, 
               color = "black", 
               width = 0.35, 
               linewidth = 0.23) +
      labs( x = "", 
            y = ylabel,
            title = year) +
      annotate("rect", 
               xmin = 21.35, 
               xmax = 27.1, 
               ymin = yaxis_max - ((ylabel_max - ylabel_min) / 6), 
               ymax = yaxis_max, 
               alpha = 1, 
               fill = "#E7E6E6", 
               color = "black", 
               linewidth = 0.23) +
      annotate("text", 
               x = 24.225, 
               y = yaxis_max - ((ylabel_max - ylabel_min) / 12), 
               label = annotate_label, 
               size = 3, 
               fontface = "bold") +
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(color = NA),
            panel.grid.major.y = element_line(color = "#D9D9D9", size = 0.35, linetype = 1),
            axis.ticks = element_line(linewidth = 0),
            axis.text.x = element_text(angle = 48, vjust = 1.3, hjust=1, color = "#595959", size = 7.8),
            axis.text.y = element_text(color = "#595959", size = 7.8),
            axis.title.y = element_text(color = "#595959", size = 9, vjust = 1.3),
            plot.title = element_text(hjust = 0.5, vjust = -85), 
            plot.margin=grid::unit(c(-5, 3.75 ,0, 6), "mm")) +
      scale_y_continuous(limits = c(0, yaxis_max), breaks = seq(ylabel_min, ylabel_max, ylabel_int),  labels = label_comma())
    
    return(plot)}
  
  ## Saving function -----
  save_fig <- function(plot_name) {
    ggsave(glue::glue("{save_dir}{emission_type}_{deparse(substitute(plot_name))}_{year}.png"), plot = plot_name, width = 8.5, height = 3.2, units = "in")
  }
  
  # Define save directory -----
  save_dir <- glue::glue("data/2b_pm_nh3_voc/static_tables/formatting/")
  
  # Define emission abbreviation for data import -----
  if(emission_type == "pm25") {
    emission_abbrev <- "pm"
  } else {
    emission_abbrev <- emission_type 
  }
  
  # Specify unique plot formatting -----
  
  emission_label <- c("pm25" = "PM", "nh3" = "NH", "voc" = "VOC")
  emission_subscript <- c("pm25" = 2.5, "nh3" = 3, "voc" = "")
  ylabel_max_emissions = c("pm25" = 8E4, "nh3" = 1E4, "voc" = 1E4)
  ylabel_int_emissions = c("pm25" = 1E4, "nh3" = 1E3, "voc" = 1E3)
  yaxis_max_emissions = c("pm25" = 8.65E4, "nh3" = 1.065E4, "voc" = 1.065E4)
  ylabel_max_rate = c("pm25" = 8E4, "nh3" = 0.3, "voc" = 0.45)
  ylabel_int_rate = c("pm25" = 1E4, "nh3" = 0.05, "voc" = 0.05)
  yaxis_max_rate = c("pm25" = 8.65E4, "nh3" = 0.3065, "voc" = 0.4565)
  
  # Define Data Years to Produce Plots -----
  
  # Convert eGRID year to numeric
  year_numeric <- as.numeric(params$eGRID_year)
  
  # Create list of data years (2018 - eGRID_year)
  years <- seq(year_numeric, 2018, -1)
  
  # Loop Through Data Years and Produce Plots -----
  for(year in years) {
    
    ## Check for the presence of preexisting files -----
    test_missing <- 0
    test_strings <- c(glue::glue("{emission_type}_emissions_{year}.png"),
                      glue::glue("{emission_type}_annual_generation_{year}.png"),
                      glue::glue("{emission_type}_rate_{year}.png"))
    
    for (file in test_strings) {
      # if emission_type, emission_plot type, year, if any of these don't exist:
      if (!file.exists(paste0(save_dir, file))) {
        test_missing <- test_missing + 1
      }
    }
    
    if (test_missing > 0) {
      print(glue::glue("Missing Figures - Producing Plots for {toupper(emission_type)}"))
      
      ## Load Subregion Data -----
      # collect subregion data from excel sheet for previous years
      if(year < params$eGRID_year) {
        # import excel subregion data
        emission_prev <- read_xlsx(glue::glue("data/2b_pm_nh3_voc/outputs/{year_numeric - 1}/eGRID{year_prev}_{emission_abbrev}emissions.xlsx"),
                                   skip = 1,
                                   sheet = glue::glue("{year} {toupper(emission_abbrev)} Subregion-level Data")) %>%
          # remove U.S. row if present
          filter(SUBRGN != "U.S.")
        
        # Define new data column names
        load("data/1_production_model/static_tables/name_matches.Rdata")
        colnames_new <- setNames(c(paste0(emission_type, "_ann"),
                                   paste0(emission_type, "_output_rate")),
                                 c(paste0("SR", toupper(emission_type), "AN"), 
                                   paste0("SR", toupper(emission_type), "RTA")))
        
        colnames <- c(subregion_nonmetric[names(subregion_nonmetric) %in% colnames(emission_prev)],
                      colnames_new[names(colnames_new) %in% colnames(emission_prev)])
        
        # Rename data columns
        subregion_file <-
          emission_prev %>%
          rename(!!!setNames(lapply(names(colnames), sym), colnames))
        
        # collect subregion data from .RDS file for current year
      } else {
        if(file.exists(glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/subregion_aggregation_{emission_type}.RDS"))) {
          subregion_file <- read_rds(glue::glue("data/2b_pm_nh3_voc/outputs/{params$eGRID_year}/subregion_aggregation_{emission_type}.RDS"))
        } else {
          stop(glue::glue("subregion_aggregation_{emission_type}.RDS does not exist. Run region_aggregation_create_pm_nh3_voc.R to obtain."))
        }
      }
      
      ## Plot and Save Subregion Emissions Data -----
      
      ### Annual Generation -----
      annual_generation <- plot_subregion_emissions(ydata = "subregion_generation_ann",
                                                    fill_color = "#5B9BD5",
                                                    ylabel = "Annual Generation (MWh)",
                                                    annotate_label = "Generation",
                                                    ylabel_min = 0,
                                                    ylabel_max = 6E8,
                                                    ylabel_int = 1E8,
                                                    yaxis_max = 6.65E8)
      save_fig(annual_generation)
      
      ### Emissions -----
      
      emissions <- plot_subregion_emissions(ydata = glue::glue("{emission_type}_ann"),
                                            fill_color = "#FF0000",
                                            ylabel = bquote(.(emission_label[emission_type])[.(emission_subscript[emission_type])] ~ "Emissions (short tons)"),
                                            annotate_label = bquote(.(emission_label[emission_type])[.(emission_subscript[emission_type])] ~ "Emissions"),
                                            ylabel_min = 0,
                                            ylabel_max = ylabel_max_emissions[emission_type],
                                            ylabel_int = ylabel_int_emissions[emission_type],
                                            yaxis_max = yaxis_max_emissions[emission_type])
      save_fig(emissions)
      
      ### Emission Rates -----
      
      rate <- plot_subregion_emissions(ydata = glue::glue("{emission_type}_output_rate"),
                                       fill_color = "#70AD47",
                                       ylabel = bquote(.(emission_label[emission_type])[.(emission_subscript[emission_type])] ~ "Emission Rates (lb/MWh)"),
                                       annotate_label = bquote(.(emission_label[emission_type])[.(emission_subscript[emission_type])] ~ "Emission Rates"),
                                       ylabel_min = 0,
                                       ylabel_max = ylabel_max_rate[emission_type],
                                       ylabel_int = ylabel_int_rate[emission_type],
                                       yaxis_max = yaxis_max_rate[emission_type])
      save_fig(rate)
    } else {
      print("No Missing Figures - Skipping Plot Production")
    }
  }
}
