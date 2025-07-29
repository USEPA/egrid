## -------------------------------
## 
## Create combined subregion emissions figures
##
## Purpose: 
## 
## This script plots PM2.5, NH3, or VOC subregion emissions data
## across all years of data for internal QA purposes for the following:
##    1) Generation
##    2) Emissions
##    3) Output Rates
##
## Resulting figures are saved in the following folder:
## "data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}/subregion_combined_plots/"
##
## Additional notes
##
##      Emma Russell, Abt Global
##
## -------------------------------

# Load libraries ---------
library(dplyr)
library(ggbreak)
library(ggplot2)
library(readr)
library(readxl)
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

# Produce graphs for all emission types -----
plot_subregion_data <- function(emission_type, save_dir) {
  
  #' plot_subregion_data
  #' 
  #' Function to plot PM2.5, NH3, or VOC subregion annual generation, emissions, and output rates data combined across all years of data
  #' 
  #' @param emission_type Emission type to produce plots for - either
  #'                      "pm", "nh3", or "voc"
  #' @param save_dir Directory to save figures produced
  #' 
  #' @return Saved figures of subregion combined plots for years 2018-current eGRID year
  #'         
  #' @examples 
  #' # Create PM2.5 plots for generation, emissions, and rates
  #' plot_subregion_data(emission_type = "pm", save_dir = glue::glue( "data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}/plots"))
  
  ## Assign emission type formatting for headers
  if(emission_type == "pm") {
    emission_label <- "pm25"
  } else {
    emission_label <- emission_type
  }
  
  ## Load emissions final formatted subregion data ------
  
  # define excel sheet path
  emissions_path <- glue::glue("data/2a_pm_nh3_voc/outputs/{params$eGRID_year}/eGRID{params$eGRID_year}_{emission_type}emissions.xlsx")
  
  # collect sheet names
  sheet_names <- c(excel_sheets(emissions_path))
  
  # select subregion data sheets
  subregion_sheets <- sheet_names[grepl("Subregion", sheet_names)]

  # load in subregion data
  subregion_data_list <- lapply(subregion_sheets, function(X) {
    df <- read_excel(emissions_path, sheet = X, skip = 1) %>%
      janitor::clean_names() %>%
      mutate(year = as.character(year))
    return(df)
  })
  
  # combine all years of data
  combined_data <- bind_rows(subregion_data_list) %>%
    # remove U.S. row if present
    filter(subrgn != "U.S.")
  
  ## Specify y data-specific plotting parameters -----
  
  # data to plot, main title, and ylabel
  ydata_params <- list(
    data = c(generation = "srngenan",
             emissions = glue::glue("sr{emission_label}an"),
             rate = glue::glue("sr{emission_label}rta")),
    title = c(generation = "Annual Generation",
              emissions = "Emissions",
              rate = "Output Rate"),
    ylabel = c(generation = "Annual Generation (MWh)",
               emissions = "Emissions (short tons)",
               rate = "Emission Rates (lb/MWh)")
  )
  
  # create list to store plots for combined PDF
  plots <- list()
  
  for (ydata in c("generation", "emissions", "rate")) {
    
    ## Specify yaxis parameters depending on ydata -----
    if (ydata == "generation") {
      ylabel_max = 6E8
      ylabel_int = 1E8
      yaxis_max = 6.2E8
    } else {
      ylabel_int <- unname(plot_params[[paste0("ylabel_int_", ydata)]][emission_type])
      ylabel_max <- unname(plot_params[[paste0("ylabel_max_", ydata)]][emission_type])
      yaxis_max <- unname(plot_params[[paste0("yaxis_max_", ydata)]][emission_type])
    }
  
    # set legend position
    legendy = 0.8
    if (emission_type == "nh3" & ydata == "emissions") {
      legendx = 0.08
    } else {
      legendx = 0.95
    }

    ## Plot data -----
    plot <- ggplot(combined_data, aes(x = subrgn, y = .data[[ydata_params$data[ydata]]], fill = year)) +
      geom_bar(stat = "identity", color = "black", position = "dodge", size = 0.1) +
      scale_fill_brewer(palette = "Dark2") +
      labs(title = bquote(.(plot_params$label[emission_type])[.(plot_params$subscript[emission_type])] ~ .(ydata_params$title[ydata])), x = "Year", y = ydata_params$ylabel[ydata]) +
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(color = NA),
            panel.grid.major.y = element_line(color = "#D9D9D9", size = 0.35, linetype = 1),
            axis.ticks = element_line(linewidth = 0),
            axis.text.x = element_text(angle = 48, vjust = 1.3, hjust=1, color = "#595959", size = 7.8),
            axis.text.y = element_text(color = "#595959", size = 7.8),
            axis.title.y = element_text(color = "#595959", size = 9, vjust = 5),
            axis.title.x = element_text(color = "#595959", size = 9, vjust = 5),
            plot.title = element_text(size = 12, hjust = 0.5, vjust = -7),
            plot.margin=grid::unit(c(-5, 3.75 ,0, 6), "mm"),
            legend.position = c(legendx, legendy),
            legend.title.align = 0.5,
            legend.key.size = unit(0.4, "cm")) +
      scale_y_continuous(limits = c(0, yaxis_max), breaks = seq(0, ylabel_max, ylabel_int),  labels = label_comma())
    
    # add plot to storage list
    plots[[ydata]] <- plot
  
    # ## Save plot -----
    # Check if folder to store raw data exists, if not - create it
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }
    ggsave(glue::glue("{save_dir}{emission_type}_{ydata}.png"), plot = plot, width = 8.5, height = 3.2, units = "in")
    print(glue::glue("{toupper(emission_type)} {ydata} plot saved successfully."))
  }
  
  # create combined figure for all plots of emissions type
  combined <- plots[["generation"]] / plots[["emissions"]] / plots[["rate"]]
  ggsave(glue::glue("{save_dir}{emission_type}_all.pdf"), combined, width = 8.5, height = 10)
    }
  
# Define emissions-specific plotting parameters ------
plot_params <- list(
  label = c(pm = "PM", nh3 = "NH", voc = "VOC"),
  subscript = c(pm = 2.5, nh3 = 3, voc = ""),
  ylabel_int_emissions = c(pm = 1e4, nh3 = 1e3, voc = 5e3),
  ylabel_max_emissions = c(pm = 7e4, nh3 = 5e3, voc = 3.5e4),
  yaxis_max_emissions = c(pm = 7.2e4, nh3 = 5.5e3, voc = 3.6e4),
  ylabel_int_rate = c(pm = 0.1, nh3 = 0.02, voc = 0.1),
  ylabel_max_rate = c(pm = 1.0, nh3 = 0.18, voc = 0.65),
  yaxis_max_rate = c(pm = 1.05, nh3 = 0.19, voc = 0.68)
)

# Define save directory -----
save_dir <- glue::glue("data/2a_pm_nh3_voc/outputs/qa/{params$eGRID_year}/subregion_combined_plots/")

# Produce plots for all emission types
for (emission_type in c("pm", "nh3", "voc")) {
  plot_subregion_data(emission_type, save_dir)
}
