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
## Resulting figures are temporarily stored in the following folder:
## "data/2a_pm_nh3_voc/static_tables/formatting/"
##
## Note: Figures will be reproduced each year for the purpose
##       of including them in the final formatted .xlsx document
##       using the final_formatting_pm_nh3_voc.R script
##
## Authors:
##      Emma Russell, Abt Global
##
## -------------------------------

create_subregion_emission_figures <- function(wb,
                                              emission_type) {
  
  #' create_subregion_emission_figures
  #' 
  #' Function to plot PM2.5, NH3, or VOC subregion annual generation, emissions, 
  #' and output rates data
  #' 
  #' @param wb Workbook with subregion emissions data to load in and plot
  #' @param emission_type Emission type to produce plots for - either
  #'                      "pm", "nh3", or "voc"
  #' 
  #' @return Saved figures of subregion annual generation, emissions, and output rates 
  #' for each year from 2018 to current eGRID year - stored in 
  #' "data/2a_pm_nh3_voc/static_tables/formatting/"
  #'         
  #' @examples 
  #' # Create PM2.5 plots
  #' pm_annual_generation <- create_subregion_emission_figures(wb = wb, emission_type = "pm")
                                                            
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
    if (emission_type == "voc" & ydata %in% c("emission_ann", "emission_output_rate")) {
      
      # define split axis parameters
      scale_params <- list(
        # y-axis lower break
        break_min = c(emission_ann = 1.3e4, emission_output_rate = 0.2), 
        # y-axis upper break
        break_max = c(emission_ann = 3.4e4, emission_output_rate = 0.6), 
        # proportion of upper axis to lower axis
        proportion = c(emission_ann = 0.2, emission_output_rate = 0.3), 
        # lower bound for title label rectangle
        annot_min = c(emission_ann = 1e4, emission_output_rate = 0.61),
        # upper bound for title label rectangle
        annot_max = c(emission_ann = 1.3e4, emission_output_rate = 0.68),
        # center location for title label text
        annot_text = c(emission_ann = 1.15e4, emission_output_rate = 0.645)
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
    
    print(glue::glue("Producing Plots for {toupper(emission_type)} {year}"))
    
    ## Load Subregion Data -----
    # collect subregion data from loaded workbook
    subregion_file <- readWorkbook(wb, 
                                   sheet = glue::glue("{year} {toupper(emission_type)} Subregion-level Data"),
                                   startRow = 2,
                                   colNames = TRUE) %>%
      janitor::clean_names() %>%
      # replace emission label with emission for universal computation
      rename_with(~gsub(emission_label, "emission", .)) %>%
      # rename data to snake case
      rename(subregion = subrgn,
             subregion_name = srname,
             subregion_generation_ann = srngenan,
             emission_ann = sremissionan,
             emission_output_rate = sremissionrta) %>%
      # set column types
      mutate(across(c(year, subregion, subregion_name), as.character),
             across(c(subregion_generation_ann, emission_ann, emission_output_rate), as.numeric)) %>%
      # remove U.S. row if present
      filter(subregion != "U.S.")

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
    
    emissions <- plot_subregion_emissions(ydata = "emission_ann",
                                          fill_color = "#FF0000",
                                          ylabel = bquote(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Emissions (short tons)"),
                                          annotate_label = bquote(bold(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Emissions")),
                                          ylabel_min = 0,
                                          ylabel_max = plot_params$ylabel_max_emissions[emission_type],
                                          ylabel_int = plot_params$ylabel_int_emissions[emission_type],
                                          yaxis_max = plot_params$yaxis_max_emissions[emission_type])
    save_fig(emissions)
    
    ### Emission Rates -----
    
    rate <- plot_subregion_emissions(ydata = "emission_output_rate",
                                     fill_color = "#70AD47",
                                     ylabel = bquote(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Emission Rates (lb/MWh)"),
                                     annotate_label = bquote(bold(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ "Emission Rates")),
                                     ylabel_min = 0,
                                     ylabel_max = plot_params$ylabel_max_rate[emission_type],
                                     ylabel_int = plot_params$ylabel_int_rate[emission_type],
                                     yaxis_max = plot_params$yaxis_max_rate[emission_type])
    save_fig(rate)
  }
}

