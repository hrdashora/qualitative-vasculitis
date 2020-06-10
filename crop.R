library(dplyr)
library(tibble)

crop <- function(loop.output, subtype = c("GCA","TAK"), region = "ALL"){
  
  map <- loop.output[[1]]
  tnt <- loop.output[[2]]
  
  # Convert GUI inputs
  options.regions <- c("AscAo","AorArch","DescAo","AbdAo",
                       "Riliac","Liliac","Rfem","Lfem",
                       "RCA","LCA","Innom","Rsub","Lsub",
                       "Rax","Lax","Rvert","Lvert")
  if ("ALL" %in% region){
    region <- options.regions
  }
  
  # Subtype/Region Cropping
  tnt.new <- tnt[tnt$LVV_Type %in% subtype,]
  if (length(region) < 2){ # Prevent errors on single region select
    map.new <- as.matrix(map[rownames(map) %in% tnt.new$MRN, colnames(map) %in% region])
    colnames(map.new) <- region
  } else{
    map.new <- map[rownames(map) %in% tnt.new$MRN, colnames(map) %in% region]
  }
  
  map.new <- as.data.frame(map.new)
  map.new <- tibble::rownames_to_column(map.new, "MRN")
  
  output <- list(map.new, tnt.new)
  return(output)
  
}