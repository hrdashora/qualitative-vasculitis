# This function imports a PETVAS CSV file, 

overlay <- function(filepath){
  f <- filepath
  input <- read.csv(f, header = TRUE)
}

# 
# 
# # PET OVERLAY -------------------------------------------------------------
# 
# 
# # Logistic Regression Model with OUTCOME: Damange and PREDICTOR: PET Activity
# #f <- " " # filepath for PETVAS spreadsheet
# #read.csv(f)
# #source("MatchingFunction.R")
# 
# 
# intersection <- intersect(matchlist, as.numeric(rownames(heat)))
# matchheat <- heat[as.character(intersection),]
# 
# # new region
# x <- patients[,3][!is.na(patients[,3])]
# y <- regions[,3][!is.na(regions[,3])]
# newregioncoord <- cbind(x,y)
# newregion_activecount <- sum(as.numeric(na.omit(petvas[newregioncoord])) > 2)
# # worse region
# x <- patients[,4][!is.na(patients[,4])]
# y <- regions[,4][!is.na(regions[,4])]
# worseregioncoord <- cbind(x,y)
# worse_activecount <- sum(as.numeric(na.omit(petvas[worseregioncoord])) > 2)
# # improve region
# x <- c(patients[,5][!is.na(patients[,5])],
#        patients[,6][!is.na(patients[,6])])
# y <- c(regions[,5][!is.na(regions[,5])],
#        regions[,6][!is.na(regions[,6])])
# improveregioncoord <- cbind(x,y)
# improve_activecount <- sum(as.numeric(na.omit(petvas[improveregioncoord])) > 2)
# # static region
# x <- patients[,2][!is.na(patients[,2])]
# y <- regions[,2][!is.na(regions[,2])]
# staticregioncoord <- cbind(x,y)
# static_activecount <- sum(as.numeric(na.omit(petvas[staticregioncoord])) > 2)
# 
# 
# worsearea <- matchheat == 3
# improvearea <- matchheat == 4
# staticarea <- matchheat == 2 
# 
# worseindex <- which(worsearea, arr.ind = TRUE, useNames = FALSE)
# x <- rownames(worsearea)[worseindex[,1]]
# y <- colnames(worsearea)[worseindex[,2]]
# coord <- cbind(x,y)
# worsepet <- na.omit(petvas[coord])
# worsemean <- mean(as.numeric(worsepet))
# 
# improveindex <- which(improvearea, arr.ind = TRUE, useNames = FALSE)
# x <- rownames(improvearea)[improveindex[,1]]
# y <- colnames(improvearea)[improveindex[,2]]
# coord <- cbind(x,y)
# improvepet <- na.omit(petvas[coord])
# improvemean <- mean(as.numeric(improvepet))
# 
# staticindex <- which(staticarea, arr.ind = TRUE, useNames = FALSE)
# x <- rownames(staticarea)[staticindex[,1]]
# y <- colnames(staticarea)[staticindex[,2]]
# coord <- cbind(x,y)
# staticpet <- na.omit(petvas[coord])
# staticmean <- mean(as.numeric(staticpet))
# 
# worsepairs <- matrix(data = NA, nrow = length(worsepet), ncol = 2)
# colnames(worsepairs) <- c("Predictor","Outcome")
# worsepairs[,1] <- as.numeric(worsepet)
# worsepairs[,2] <- "Worse"
# #worsepairs[,2] <- factor(worsepairs[,2], levels = c("Worse","Static","Improve"))
# 
# improvepairs <- matrix(data = NA, nrow = length(improvepet), ncol = 2)
# colnames(improvepairs) <- c("Predictor","Outcome")
# improvepairs[,1] <- as.numeric(improvepet)
# improvepairs[,2] <- "Improve"
# #improvepairs[,2] <- factor(improvepairs[,2], levels = c("Worse","Static","Improve"))
# 
# staticpairs <- matrix(data = NA, nrow = length(staticpet), ncol = 2)
# colnames(staticpairs) <- c("Predictor","Outcome")
# staticpairs[,1] <- as.numeric(staticpet)
# staticpairs[,2] <- "Static"
# #staticpairs[,2] <- factor(staticpairs[,2], levels = c("Worse","Static","Improve"))
