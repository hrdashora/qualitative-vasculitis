hist <- function(crop.output){

  tnt <- crop.output[[2]]
  
  num.hist <- tnt %>% group_by(LVV_Type) %>% summarise(Median = median(Elapsed_Time),
                                                       Q1 = quantile(Elapsed_Time, probs = 0.25),
                                                       Q2 = quantile(Elapsed_Time, probs = 0.75))
  
  # Plot Histogram
  plot.hist <- ggplot(data = tnt, aes(x = tnt$Elapsed_Time, fill = tnt$LVV_Type)) +
    geom_histogram(binwidth = 100, alpha = 0.5, boundary = 0, closed ="right", position = "identity") +
    xlab("Time (days)") + ylab("Counts") + labs(fill = "Subtype") +
    scale_x_continuous(breaks = seq(100, 2000, by = 100)) +
    scale_y_continuous(breaks = seq(0, 10, by = 1)) +
    geom_vline(data = num.hist, aes(xintercept = Median, color = LVV_Type),
               linetype = "dashed", size = 0.5, show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          panel.grid.minor = element_blank()) +
    ggtitle("Elapsed Time Between Imaging Studies of Patient Cohort")
  
  return(plot.hist)
}