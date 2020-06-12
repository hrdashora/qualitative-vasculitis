heatmap <- function(crop.output){
  
  names.arg <- c("AscAo"="Ascend Aorta", "AorArch"="Aortic Arch", "DescAo"="Descend Aorta","AbdAo"="Abdom. Aorta",
                 "Riliac"="R. Iliac","Liliac"="L. Iliac","Rfem"="R. Femoral","Lfem"="L. Femoral",
                 "RCA"="R. Carotid","LCA"="L. Carotid","Innom"="Innominate","Rsub"="R. Subclavian","Lsub"="L. Subclavian",
                 "Rax"="R. Axillary","Lax"="L. Axillary","Rvert"="R. Vertebral","Lvert"="L. Vertebral")
 
   # Import the "patient-by-region" matrix from a previous function output
  map <- crop.output[[1]]
  tnt <- crop.output[[2]]
  join <- inner_join(map,tnt,by="MRN")
  # Order/Manipulate data
  rsum <- rowSums(abs(select(join,-c(MRN, Elapsed_Time, LVV_Type))), na.rm = TRUE) #calculate the row sum to estimate the total vascular damage in each patient
  map.new <- arrange(join, LVV_Type, desc(rsum))
  melt.map <- melt(select(map.new,-c(Elapsed_Time, LVV_Type)), id=c("MRN")) #change the "patient-by-region" matrix from wide-form to long-form DataFrame
  melt.map$num <- as.character(group_indices(melt.map, MRN)) #assign patient numbers based on MRN
  melt.map$value <- as.factor(melt.map$value) #convert numeric vascular damage score score to factor
  ord <- unique(group_indices(melt.map, MRN))
  
  # Plot heatmap
  plot.map <- ggplot(data = melt.map, aes(x = factor(num, levels = as.character(ord)), y = variable)) +
    geom_raster(aes(fill = value), na.rm = T) +
    scale_y_discrete(labels=names.arg) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          title = element_text(size = 12)
    ) +
    scale_fill_manual(values = c("-3" = "#D73027","-2" = "#FC8D59","-1" = "#FEE090",
                                 "0" = "#E0F3F8","2" = "#91BFDB","3" = "#4575B4"),
                      name = "Status of \nArterial Damage",
                      labels = c("-3"="New Lesion","-2"="Increased Severity of Damange","-1"="No Change in Damange",
                                 "0"="Territory Not Involved","2"="Decreased Severity of Damange","3"="Resolved Lesion")) +
    ggtitle("Longitudinal Damage Heatmap of Patients' Arterial Territories")
  
  return(plot.map)
}