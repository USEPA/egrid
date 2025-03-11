## -------------------------------
##
## Plot Subregion Emissions
##
## Purpose: 
## 
## This file plots PM2.5, NH3, and VOC subregion data using the function
## function_plot_subregion_emissions.R 
##
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

# Load Libraries ---------
library(ggplot2)
library(readr)
library(scales)

# Define eGRID year parameter ----------------
# define parameter year if no one is currently assigned using prompted user input
if (exists("params")) {
  if ("eGRID_year" %in% names(params)) { # if params() and params$eGRID_year exist, do not re-define
    print("eGRID year parameter is already defined.")
  } else { # if params() is defined, but eGRID_year is not, define it here
    params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
    params$eGRID_year <- as.character(params$eGRID_year)
  }
} else { # if params() and eGRID_year are not defined, define them here
  params <- list()
  params$eGRID_year <- readline(prompt = "Input eGRID_year: ")
  params$eGRID_year <- as.character(params$eGRID_year)
}

# Set function source -----
source("scripts/functions/function_plot_subregion_emissions.R")

# Set save directory and save function --------
save_dir <- "data/static_tables/formatting/"

save_fig <- function(plot_name) {
  ggsave(paste0(save_dir, deparse(substitute(plot_name)), ".png"), plot = plot_name, width = 8.5, height = 3.2, units = "in")
}

# Plot PM2.5 -----
## Annual Generation ------
pm25_annual_generation <- plot_subregion_emissions(emission_type = "pm25",
                                  ydata = "subregion_generation_ann",
                                  fill_color = "#5B9BD5",
                                  ylabel = "Annual Generation (MWh)",
                                  annotate_label = "Generation",
                                  ylabel_min = 0,
                                  ylabel_max = 6E8,
                                  ylabel_int = 1E8,
                                  yaxis_max = 6.65E8)
pm25_annual_generation
save_fig(pm25_annual_generation)

## Emissions -----
pm25_emissions <- plot_subregion_emissions(emission_type = "pm25",
                              ydata = "pm25_tons",
                              fill_color = "#FF0000",
                              ylabel = expression("PM"[2.5] ~ "Emissions (short tons)"),
                              annotate_label = expression(bold("PM"[2.5] ~ "Emissions")),
                              ylabel_min = 0,
                              ylabel_max = 8E4,
                              ylabel_int = 1E4,
                              yaxis_max = 8.65E4)
pm25_emissions
save_fig(pm25_emissions)

## Emission Rates -----
pm25_rate <- plot_subregion_emissions(emission_type = "pm25",
                         ydata = "pm25_rate",
                         fill_color = "#70AD47",
                         ylabel = expression("PM"[2.5] ~ "Emission Rates (lb/MWh)"),
                         annotate_label = expression(bold("PM"[2.5] ~ "Emission Rates")),
                         ylabel_min = 0,
                         ylabel_max = 1.2,
                         ylabel_int = 0.2,
                         yaxis_max = 1.265)
pm25_rate
save_fig(pm25_rate)

# Plot NH3 -----
## Annual Generation ------
nh3_annual_generation <- plot_subregion_emissions(emission_type = "nh3",
                                   ydata = "subregion_generation_ann",
                                   fill_color =  "#4472C4",
                                   ylabel = "Annual Generation (MWh)",
                                   annotate_label = "Generation",
                                   ylabel_min = 0,
                                   ylabel_max = 6E8,
                                   ylabel_int = 1E8,
                                   yaxis_max = 6.65E8)
nh3_annual_generation
save_fig(nh3_annual_generation)

## Emissions -----
nh3_emissions <- plot_subregion_emissions(emission_type = "nh3",
                           ydata = "nh3_tons",
                           fill_color = "#70AD47",
                           ylabel = expression("NH"[3] ~ "Emissions (short tons)"),
                           annotate_label = expression(bold("NH"[3] ~ "Emissions")),
                           ylabel_min = 0,
                           ylabel_max = 1E4,
                           ylabel_int = 1E3,
                           yaxis_max = 1.065E4)
nh3_emissions
save_fig(nh3_emissions)

## Emission Rates -----
nh3_rate <- plot_subregion_emissions(emission_type = "nh3",
                      ydata = "nh3_rate",
                      fill_color = "#FF0000",
                      ylabel = expression("NH"[3] ~ "Emission Rates (lb/MWh)"),
                      annotate_label = expression(bold("NH"[3] ~ "Emission Rates")),
                      ylabel_min = 0,
                      ylabel_max = 0.3,
                      ylabel_int = 0.05,
                      yaxis_max = 0.3065)
nh3_rate
save_fig(nh3_rate)

# Plot VOC -----
## Annual Generation ------
voc_annual_generation <- plot_subregion_emissions(emission_type = "voc",
                                  ydata = "subregion_generation_ann",
                                  fill_color = "#4472C4",
                                  ylabel = "Annual Generation (MWh)",
                                  annotate_label = "Generation",
                                  ylabel_min = 0,
                                  ylabel_max = 6E8,
                                  ylabel_int = 1E8,
                                  yaxis_max = 6.65E8)
voc_annual_generation
save_fig(voc_annual_generation)

## Emissions -----
voc_emissions <- plot_subregion_emissions(emission_type = "voc",
                          ydata = "voc_tons",
                          fill_color = "#70AD47",
                          ylabel = "VOC Emissions (short tons)",
                          annotate_label = "VOC Emissions",
                          ylabel_min = 0,
                          ylabel_max = 1E4,
                          ylabel_int = 1E3,
                          yaxis_max = 1.065E4)
voc_emissions
save_fig(voc_emissions)

## Emission Rates -----
voc_rate <- plot_subregion_emissions(emission_type = "voc",
                     ydata = "voc_rate",
                     fill_color = "#FF0000",
                     ylabel = expression("VOC Emission Rates (lb/MWh)"),
                     annotate_label = expression(bold("VOC Emission Rates")),
                     ylabel_min = 0,
                     ylabel_max = 0.45,
                     ylabel_int = 0.05,
                     yaxis_max = 0.4565)
voc_rate
save_fig(voc_rate)
