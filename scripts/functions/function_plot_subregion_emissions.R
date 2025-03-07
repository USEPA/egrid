## -------------------------------
## 
## Plot subregion emissions
##
## Purpose: 
## 
## This function plots PM2.5, NH3, or VOC subregion emissions data
## to be used in the final .xlsx sheet of data
## 
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

plot_subregion_emissions <- function(emission_type,
                                     ydata,
                                     fill_color,
                                     ylabel,
                                     annotate_label,
                                     ylabel_min,
                                     ylabel_max,
                                     ylabel_int,
                                     yaxis_max) {
  
  #' plot_subregion_emissions
  #' 
  #' Function to plot pm2.5, nh3, or voc subregion emissions data
  #' 
  #' @param emission_type Emission type to be calculated - either
  #'                      "pm25", "nh3", or "voc"
  #' @param ydata Data column name to be plotted on y axis - 
  #'              must be a column name of subregion dataset
  #' @param fill_color HEX code of bar plot fill color as character
  #' @param ylabel Y-axis label for plot as character
  #' @param annotate_label Plot title to be added as annotation as character
  #' @param ylabel_min Lowest tick label on y-axis as numeric
  #' @param ylabel_max Highest tick label on y-axis as numeric
  #' @param ylabel_int Y-axis tick interval as numeric
  #' @param yaxis_max Y-axis maximum value as numeric
  #' 
  #' @return Plot of desired emission type and data column
  #'         
  #' @examples 
  #' # Create PM2.5 emissions plot
  #' pm25_annual_generation <- plot_subregion_emissions(emission_type = "pm25",
  #'                           ydata = "pm25_tons",
  #'                           fill_color = "#FF0000",
  #'                           ylabel = expression("PM"[2.5] ~ " Emissions (short tons)"),
  #'                           annotate_label = expression(bold("PM"[2.5] ~ " Emissions")),
  #'                           ylabel_min = 0,
  #'                           ylabel_max = 8E4,
  #'                           ylabel_int = 1E4,
  #'                           yaxis_max = 8.65E4))
  #' 
  
  # Require Libraries ---------
  require(ggplot2)
  require(readr)
  require(scales)
  
  # Load subregion data -----
  if(file.exists(glue::glue("data/outputs/{params$eGRID_year}/subregion_aggregation_{emission_type}.RDS"))) {
    subregion_file <- read_rds(glue::glue("data/outputs/{params$eGRID_year}/subregion_aggregation_{emission_type}.RDS")) #%>%
  } else {
    stop(glue::glue("subregion_aggregation_{emission_type}.RDS does not exist. Run region_aggregation_create_pm_nh3_voc.R to obtain."))
  }
  
  # Plot data -----
  plot <- ggplot(subregion_file, aes(x = subregion, y = get(ydata))) +
    geom_col(fill = fill_color, 
             color = "black", 
             width = 0.35, 
             linewidth = 0.23) +
    labs( x = "", 
          y = ylabel) +
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
          plot.margin=grid::unit(c(-5, 3.75 ,1, 6), "mm")) +
    scale_y_continuous(limits = c(0, yaxis_max), breaks = seq(ylabel_min, ylabel_max, ylabel_int),  labels = label_comma())
  
  return(plot)
}
