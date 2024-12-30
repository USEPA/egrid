## -------------------------------
##
## Region summary table and plots
## 
## Purpose: 
## 
## This file creates functions to create summary tables and plots for subregions and states. 
##
## Authors:  
##      Teagan Goforth, Abt Global
##
## -------------------------------

## egrid subregion aggregation

create_cap_table_subregion <- function(subregion_abbr, years) {
  
  table_cap_gen <- 
    subregion_cap_gen %>% 
    filter(subregion == subregion_abbr) %>% 
    knitr::kable(digits = 0,
                 format.args = list(big.mark = ","),
                 col.names = gsub("[_]", " ", names(subregion_cap_gen))) 
  
  return(table_cap_gen)}
  
create_resource_table_subregion <- function(subregion_abbr, years) {
  
  table_resource_mix <-  
    subregion_resource_mix_wider %>% 
    filter(subregion == subregion_abbr) %>%  
    knitr::kable(digits = c(0, 0, 0, 0, 0, 0, 1),
                 format.args = list(big.mark = ","), 
                 col.names = gsub("[_]", " ", names(subregion_resource_mix_wider))) 
  
  return(table_resource_mix)}

create_resource_plot_subregion <- function(subregion_abbr, years) {
  
  region_filter <- 
    subregion_resource_mix %>% 
    filter(subregion == subregion_abbr, 
           year %in% years)
  
  gen_plot <- 
    ggplot(region_filter, aes(y = reorder(energy_source, desc(energy_source)),
                              x = generation, fill = year)) + 
    geom_bar(position = "dodge", 
             stat = "identity") +
    ggtitle("Net Generation") +
    ylab("Energy source") + 
    xlab("Net Generation (MWh)")
  
  return(plotly::ggplotly(gen_plot) %>%
           plotly::layout(legend = list(orientation = "h", x = 0.35, y = 1.1)))}

## state level aggregation 

create_cap_table_state <- function(state_abbr, years) {
  
  table_cap_gen <- 
    state_cap_gen %>% 
    filter(state == state_abbr) %>% 
    knitr::kable(digits = 0,
                 format.args = list(big.mark = ","),
                 col.names = gsub("[_]", " ", names(state_cap_gen)))
  
  return(table_cap_gen)}

create_resource_table_state <- function(state_abbr, years) {
  
  table_resource_mix <-  
    state_resource_mix_wider %>% 
    filter(state == state_abbr) %>%  
    knitr::kable(digits = c(0, 0, 0, 0, 0, 0, 1),
                 format.args = list(big.mark = ","),
                 col.names = gsub("[_]", " ", names(state_resource_mix_wider)))
  
  return(table_resource_mix)}

create_resource_plot_state <- function(state_abbr, years) {
  
  region_filter <- 
    state_resource_mix %>% 
    filter(state == state_abbr, 
           year %in% years)
  
  gen_plot <- 
    ggplot(region_filter, aes(y = reorder(energy_source, desc(energy_source)),
                              x = generation, fill = year)) + 
    geom_bar(position = "dodge", 
             stat = "identity") +
    ggtitle("Net Generation") +
    ylab("Energy source") + 
    xlab("Net Generation (MWh)")
  
  return(plotly::ggplotly(gen_plot) %>% 
           plotly::layout(legend = list(orientation = "h", x = 0.35, y = 1.1)))}

