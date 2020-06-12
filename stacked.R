stacked <- function(crop.output){
  
  map <- crop.output[[1]]
  
  names.arg <- c("AscAo"="Ascend Aorta", "AorArch"="Aortic Arch", "DescAo"="Descend Aorta","AbdAo"="Abdom. Aorta",
                 "Riliac"="R. Iliac","Liliac"="L. Iliac","Rfem"="R. Femoral","Lfem"="L. Femoral",
                 "RCA"="R. Carotid","LCA"="L. Carotid","Innom"="Innominate","Rsub"="R. Subclavian","Lsub"="L. Subclavian",
                 "Rax"="R. Axillary","Lax"="L. Axillary","Rvert"="R. Vertebral","Lvert"="L. Vertebral")
  
  scores.arg <- c("-3"="New Lesion","-2"="Increased Severity of Damage","-1"="No Change in Damage",
                  "2"="Decreased Severity of Damage","3"="Resolved Lesion")
  
  # Stacked Bar Graph
  map.melt <- melt(map, id = "MRN")
  map.cast <- acast(map.melt, value ~ variable, length)
  #map.cast[1,] <- colSums(map.cast[1:2,])
  map.cast.stack <- map.cast[!rownames(map.cast) %in% c("0","NA"), ]
  colnames(map.cast.stack) <- names.arg
  stack <- map.cast.stack
  
  # if (is.matrix(map.cast.stack) == FALSE && length(region) < 2){
  #   map.cast.stack <- (as.matrix(map.cast.stack))
  #   colnames(map.cast.stack) <- region
  # } else if (is.matrix(map.cast.stack) == FALSE && length(region) >= 2){
  #   map.cast.stack <- t(as.matrix(map.cast.stack))
  #   colnames(map.cast.stack) <- region
  # }
  # rownames(map.cast.stack) <- rownames(map.cast)[!rownames(map.cast) %in% c("0","NA")]
  #rownames(map.cast.stack)[1] <- "-3 & -2"
  
  # map.new <- as.data.frame(map.new)
  # map.new$MRN <- row.names(map.new)
  # row.names(map.new) <- NULL
  # map.new$type <- subtype[,2]
  
  # Plot Stacked Bar Graph
  stack.melt <- melt(stack)
  stack.melt$Var1 <- as.factor(stack.melt$Var1)
  plot.stack <- ggplot(data = stack.melt, aes(x = Var2, y = value, fill = Var1)) +
    geom_col() +
    scale_fill_manual(values = c("-3" = "#D73027","-2" = "#FC8D59","-1" = "#FEE090",
                                 "2" = "#91BFDB","3" = "#4575B4"),
                      labels = c("-3"="New Lesion","-2"="Increased Severity of Damage","-1"="No Change in Damage",
                                 "2"="Decreased Severity of Damage","3"="Resolved Lesion")) +
    labs(fill = "Status of \nArterial Damage") +
    ylab("Counts") +
    xlab("Arterial Territory") +
    ggtitle("Change in Arterial Damage \nOver Time in Large-Vessel Vasculitis") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          title = element_text(size = 12))
  
  rownames(stack) <- scores.arg
  data.stack <- stack
  
  output <- list(plot.stack, data.stack)
  
  return(output)
}