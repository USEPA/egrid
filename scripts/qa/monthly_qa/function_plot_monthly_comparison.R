


plot_monthly_comparison <- function(monthly_df, col, title) {
  
  #' plot_monthly_comparison
  #' 
  #' Create plots that compare annual to monthly values. 
  #' 
  #' @param monthly_df Dataframe of monthly vs. annual data
  #' @param col Column in dataframe that is plotted in monthly_df
  #' @param title Title of figure

  plot <- 
    monthly_df %>% 
    ggplot(aes(x = month, y = {{ col }}, fill = month)) +
    geom_bar(stat = "identity") + 
    ggtitle(title) + 
    xlab("Time period") + 
    ylab(title) + 
    scale_fill_manual(values = c("deepskyblue3", "deepskyblue3", "deepskyblue3", "deepskyblue3", 
                                 "deepskyblue3", "deepskyblue3", "deepskyblue3", "deepskyblue3", 
                                 "deepskyblue3", "deepskyblue3", "deepskyblue3", "deepskyblue3", 
                                 "firebrick3")) + 
    theme(legend.position = "none") 
  
  return(plot)

}
