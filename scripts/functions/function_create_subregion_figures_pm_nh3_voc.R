## -------------------------------
## 
## Create subregion emissions figures function
##
## Purpose: 
## 
## This function plots PM2.5, NH3, or VOC subregion emissions data
## and saves figures for the current eGRID year and previous years
## to be used in the final .xlsx sheet of data
##
## Resulting figures are saved in the following folder:
## "data/2a_pm_nh3_voc/static_tables/formatting/"
##
## Images are included in the final .xlsx files for emissions data
## using the final_formatting_pm_nh3_voc.R script
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

create_subregion_emission_figures <- function(wb,
                                              emission_type,
                                              skip_if_exists = TRUE) {
  
  #' create_subregion_emission_figures
  #' 
  #' Function to plot PM2.5, NH3, or VOC subregion annual generation, emissions, 
  #' and output rates data
  #' 
  #' @param wb Workbook with subregion emissions data to load in and plot
  #' @param emission_type Emission type to produce plots for - either
  #'                      "pm", "nh3", or "voc"
  #' @param skip_if_exists If TRUE, checks the presence of figures and skips
  #'                       plot production if figure already exists.
  #'                       If FALSE, all plots will be reproduced and resaved
  #' 
  #' @return Saved figures of subregion annual generation, emissions, and output rates 
  #' for each year from 2018 to current eGRID year - saved in 
  #' "data/2a_pm_nh3_voc/static_tables/formatting/"
  #'         
  #' @examples 
  #' # Create PM2.5 plots and override previously-saved plots
  #' pm_annual_generation <- create_subregion_emission_figures(wb = wb, emission_type = "pm", skip_if_exists = FALSE)
                                                            
  # Require libraries ---------
  require(ggbreak)
  require(ggplot2)
  require(openxlsx)
  require(readr)
  require(readxl)
  require(scales)
  
  # Create nested functions ------
  
  ## Plotting function -----
  plot_subregion_emissions <- function(ydata,
                                       fill_color,
                                       ylabel,
                                       annotate_label,
                                       ylabel_min,
                                       ylabel_max,
                                       ylabel_int,
                                       yaxis_max) {
    
    # Split axis for VOC emissions data
    if (ydata %in% c("voc_ann", "voc_output_rate")) {
      
      # define split axis parameters
      scale_params <- list(
        # y-axis lower break
        break_min = c(voc_ann = 1.3e4, voc_output_rate = 0.2), 
        # y-axis upper break
        break_max = c(voc_ann = 3.4e4, voc_output_rate = 0.6), 
        # proportion of upper axis to lower axis
        proportion = c(voc_ann = 0.2, voc_output_rate = 0.3), 
        # lower bound for title label rectangle
        annot_min = c(voc_ann = 1e4, voc_output_rate = 0.61),
        # upper bound for title label rectangle
        annot_max = c(voc_ann = 1.3e4, voc_output_rate = 0.68),
        # center location for title label text
        annot_text = c(voc_ann = 1.15e4, voc_output_rate = 0.645)
        )
        
      # Plot VOC emissions data
      plot <- ggplot(subregion_file, aes(x = subregion, y = get(ydata))) +
        # bar chart of values
        geom_col(fill = fill_color, 
                 color = "black", 
                 width = 0.35, 
                 linewidth = 0.23) +
        # create y-axis with break
        scale_y_continuous(limits = c(0, yaxis_max), breaks = seq(ylabel_min, ylabel_max, ylabel_int),  labels = label_comma()) + 
          scale_y_break(c(scale_params$break_min[ydata], scale_params$break_max[ydata]), scales = scale_params$proportion[ydata]) +
        # define axis labels and title
          labs(x = "", 
               y = ylabel,
               title = year) +
        # create rectangular annotation with emissions type and data type
          annotate("rect",
                   xmin = 21.35,
                   xmax = 27.1,
                   ymin = scale_params$annot_min[ydata],
                   ymax = scale_params$annot_max[ydata],
                   alpha = 1,
                   fill = "#E7E6E6",
                   color = "black",
                   linewidth = 0.23) +
        # add text to annotation
          annotate("text",
                   x = 24.225,
                   y = scale_params$annot_text[ydata],
                   label = annotate_label,
                   size = 3,
                   fontface = "bold") +
          theme(panel.background = element_rect(fill = NA),
                # remove x-axis grids
                panel.grid.major.x = element_line(color = NA),
                # set y-axis grid size and color
                panel.grid.major.y = element_line(color = "#D9D9D9", size = 0.35, linetype = 1),
                # remove axis ticks
                axis.ticks = element_line(linewidth = 0),
                # tilt x-axis text and set color
                axis.text.x = element_text(angle = 48, vjust = 1.3, hjust=1, color = "#595959", size = 7.8),
                # set y-axis size and color
                axis.text.y = element_text(color = "#595959", size = 7.8),
                # tilt y-axis title and move to center
                axis.title.y = element_text(color = "#595959", size = 9, angle = 90, vjust = -1, hjust = 0.6),
                # remove righthand y-axis labels
                axis.title.y.right = element_blank(),
                axis.text.y.right = element_blank(),
                axis.ticks.y.right = element_blank(),
                # set plot title location and size
                plot.title = element_text(hjust = 0.5, vjust = -80, size = 12))
      
    # Plot all other emission variables
    } else {
      plot <- ggplot(subregion_file, aes(x = subregion, y = get(ydata))) +
      # bar chart of values
      geom_col(fill = fill_color, 
               color = "black", 
               width = 0.35, 
               linewidth = 0.23) +
      # define axis labels and title
      labs(x = "", 
           y = ylabel,
           title = year) +
      # create rectangular annotation with emissions type and data type
      annotate("rect", 
               xmin = 21.35, 
               xmax = 27.1, 
               ymin = yaxis_max - ((ylabel_max - ylabel_min) / 6), 
               ymax = yaxis_max, 
               alpha = 1, 
               fill = "#E7E6E6", 
               color = "black", 
               linewidth = 0.23) +
      # add text to annotation
      annotate("text", 
               x = 24.225, 
               y =  yaxis_max - ((ylabel_max - ylabel_min) / 12), 
               label = annotate_label, 
               size = 3, 
               fontface = "bold") +
      theme(panel.background = element_rect(fill = NA),
            # remove x-axis grids
            panel.grid.major.x = element_line(color = NA),
            # set y-axis grid size and color
            panel.grid.major.y = element_line(color = "#D9D9D9", size = 0.35, linetype = 1),
            # remove axis ticks
            axis.ticks = element_line(linewidth = 0),
            # tilt x-axis text and set color
            axis.text.x = element_text(angle = 48, vjust = 1.3, hjust=1, color = "#595959", size = 7.8),
            # set y-axis size and color
            axis.text.y = element_text(color = "#595959", size = 7.8),
            # tilt y-axis title and move to center
            axis.title.y = element_text(color = "#595959", size = 9, vjust = 1.3),
            # set plot title location and size
            plot.title = element_text(hjust = 0.5, vjust = -85), 
            # set margins around plotting
            plot.margin=grid::unit(c(-5, 3.75 ,0, 6), "mm")) +
      # create continuous yaxis
      scale_y_continuous(limits = c(0, yaxis_max), breaks = seq(ylabel_min, ylabel_max, ylabel_int),  labels = label_comma())
    }
    return(plot)
  }
  
  ## Saving function -----
  # save figure using plot variable name
  save_fig <- function(plot_name) {
    ggsave(glue::glue("{save_dir}{emission_type}_{deparse(substitute(plot_name))}_{year}.png"), plot = plot_name, width = 8.5, height = 3.2, units = "in")
  }
  
  # Define save directory -----
  save_dir <- glue::glue("data/2a_pm_nh3_voc/static_tables/formatting/")
  
  # Define emission abbreviation for data import -----
  if(emission_type == "pm") {
    emission_label <- "pm25"
  } else {
    emission_label <- emission_type 
  }
  
  # Specify unique plot formatting -----
  
  plot_params <- list(
    # emissions label for title
    label = c(pm = "PM", nh3 = "NH", voc = "VOC"),
    # subscripts for title
    subscript = c(pm = 2.5, nh3 = 3, voc = ""),
    # yaxis label intervals for emissions plot
    ylabel_int_emissions = c(pm = 1e4, nh3 = 1e3, voc = 2e3),
    # yaxis label maximum value for emissions plot
    ylabel_max_emissions = c(pm = 7.5e4, nh3 = 6e3, voc = 3.4e4),
    # yaxis maximum value for emissions plot
    yaxis_max_emissions = c(pm = 8e4, nh3 = 6e3, voc = 3.5e4),
    # yaxis label intervals for rate plot
    ylabel_int_rate = c(pm = 0.1, nh3 = 0.02, voc = 0.05),
    # yaxis label maximum value for rate plot
    ylabel_max_rate = c(pm = 1.0, nh3 = 0.18, voc = 0.65),
    # yaxis maximum value for rate plot
    yaxis_max_rate = c(pm = 1.05, nh3 = 0.2, voc = 0.68)
  )

  # Define Data Years to Produce Plots -----
  
  # convert eGRID year to numeric
  year_numeric <- as.numeric(params$eGRID_year)
  
  # create list of data years (2018 - eGRID_year)
  years <- seq(year_numeric, 2018, -1)

  # Loop Through Data Years and Produce Plots -----
  for(year in years) {
    
    ## Check for the presence of preexisting files -----
    
    # set file count
    test_missing <- 0
    
    if (skip_if_exists == TRUE) {
      # check for all three plots for each emissions type
      test_strings <- c(glue::glue("{emission_type}_emissions_{year}.png"),
                        glue::glue("{emission_type}_annual_generation_{year}.png"),
                        glue::glue("{emission_type}_rate_{year}.png"))
      
      for (file in test_strings) {
        if (!file.exists(paste0(save_dir, file))) {
          # if any of the files don't exist, set counter to 1
          test_missing <- test_missing + 1
        }
      }
    }
    
    # produce plots if skip_if_exists == FALSE or files don't already exist
    if (test_missing > 0 | skip_if_exists == FALSE) {
      print(glue::glue("Producing Plots for {toupper(emission_type)} {year}"))
      
      ## Load Subregion Data -----
      # collect subregion data from loaded workbook
      emission_prev <- readWorkbook(wb, 
                                     sheet = glue::glue("{year} {toupper(emission_type)} Subregion-level Data"),
                                     startRow = 2,
                                     colNames = TRUE) %>%
        # remove U.S. row if present
        filter(SUBRGN != "U.S.") %>%
        # set column types
        mutate(across(c(YEAR, SUBRGN, SRNAME), as.character),
               across(c(SRNGENAN, paste0("SR", toupper(emission_label), "AN"), paste0("SR", toupper(emission_label), "RTA")), as.numeric))
               
      # # collect subregion data from excel sheet for previous years
      # if(year < params$eGRID_year) {
      #   emission_prev <- read_xlsx(glue::glue("data/2a_pm_nh3_voc/inputs/pm_nh3_voc_historic/{year_numeric - 1}/eGRID{year_numeric - 1}_{emission_type}emissions.xlsx"),
      #                              skip = 1,
      #                              sheet = glue::glue("{year} {toupper(emission_type)} Subregion-level Data")) %>%
      #     # remove U.S. row if present
      #     filter(SUBRGN != "U.S.")
        
      # define new data column names
      base::load("data/1_production_model/static_tables/name_matches.Rdata")
      colnames_new <- setNames(c(paste0(emission_label, "_ann"),
                                 paste0(emission_label, "_output_rate")),
                               c(paste0("SR", toupper(emission_label), "AN"), 
                                 paste0("SR", toupper(emission_label), "RTA")))
      
      # reassign column names
      colnames <- c(subregion_nonmetric[names(subregion_nonmetric) %in% colnames(emission_prev)],
                    colnames_new[names(colnames_new) %in% colnames(emission_prev)])
      # rename subregion file column names
      subregion_file <-
        emission_prev %>%
        rename(!!!setNames(lapply(names(colnames), sym), colnames))
        
      #   # collect subregion data from .RDS file for current year
      # } else {
      #   if(file.exists(glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/subregion_aggregation_{emission_type}.RDS"))) {
      #     subregion_file <- read_rds(glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/subregion_aggregation_{emission_type}.RDS"))
      #   } else {
      #     stop(glue::glue("subregion_aggregation_{emission_type}.RDS does not exist. Run region_aggregation_create_pm_nh3_voc.R to obtain."))
      #   }
      # }
      
      ## Plot and Save Subregion Emissions Data -----
      
      ### Annual Generation -----
      
      annual_generation <- plot_subregion_emissions(ydata = "subregion_generation_ann",
                                                    fill_color = "#5B9BD5",
                                                    ylabel = bquote(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Annual Generation (MWh)"),
                                                    annotate_label =  bquote(bold(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Generation")),
                                                    ylabel_min = 0,
                                                    ylabel_max = 6E8,
                                                    ylabel_int = 1E8,
                                                    yaxis_max = 6.65E8)
      save_fig(annual_generation)
      
      ### Emissions -----
      
      emissions <- plot_subregion_emissions(ydata = glue::glue("{emission_label}_ann"),
                                            fill_color = "#FF0000",
                                            ylabel = bquote(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Emissions (short tons)"),
                                            annotate_label = bquote(bold(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Emissions")),
                                            ylabel_min = 0,
                                            ylabel_max = plot_params$ylabel_max_emissions[emission_type],
                                            ylabel_int = plot_params$ylabel_int_emissions[emission_type],
                                            yaxis_max = plot_params$yaxis_max_emissions[emission_type])
      save_fig(emissions)
      
      ### Emission Rates -----
      
      rate <- plot_subregion_emissions(ydata = glue::glue("{emission_label}_output_rate"),
                                       fill_color = "#70AD47",
                                       ylabel = bquote(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Emission Rates (lb/MWh)"),
                                       annotate_label = bquote(bold(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Emission Rates")),
                                       ylabel_min = 0,
                                       ylabel_max = plot_params$ylabel_max_rate[emission_type],
                                       ylabel_int = plot_params$ylabel_int_rate[emission_type],
                                       yaxis_max = plot_params$yaxis_max_rate[emission_type])
      save_fig(rate)
    } else {
      print(glue::glue("Skipping Plot Production for {toupper(emission_type)} {year} - Figures Already Exist"))
    }
  }
}
