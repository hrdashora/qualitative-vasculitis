# This function is interates over all the angiographic regions in a CSV file

main_loop <- function(filepath){
  
  # Read tabular data
  f <- filepath
  input <- read.csv(f, stringsAsFactors = FALSE) #import data
  regions <- c("AscAo","AorArch","DescAo","AbdAo","Riliac","Liliac","Rfem","Lfem",
                   "RCA","LCA","Innom","Rsub","Lsub","Rax","Lax","Rvert","Lvert") 
  input$Scan_Date <- as.Date(input$Scan_Date, format = "%m/%d/%y") #convert to date format
  
  # Initialize variables and matrices 
  patient.count <- integer(1) #total number of patients that have been analyzed
  patient.ids <- unique(input[,1])
  map.damage <- matrix(data = NA, nrow = length(patient.ids), ncol = length(regions)) #create empty matrix
  colnames(map.damage) <- regions
  rownames(map.damage) <- patient.ids
  time <- as.data.frame(matrix(data = NA, nrow = length(patient.ids), ncol = 1))
  rownames(time) <- patient.ids
  colnames(time) <- "Elapsed_Time"
  type <- unique(input[,1:2])
  
  # Sort the data frame
  patient.data <- input %>% 
    arrange(MRN, Scan_Date)
  
  # Loop to select region
  for (k in 1:17){
    target <- regions[k]
    region.data <- patient.data %>%
      select(1:3, starts_with(target))
    n <- dim(region.data)[1]/2
    # Loop to select patient
    for (j in 1:(n)){
      cropped.data <- region.data %>%
        filter(MRN == patient.ids[j])
      output <- scoring(cropped.data)
      score <- output[[1]]
      delta <- output[[2]]
      map.damage[j,k] <- score
      time[j,1] <- delta
    }
  } 
  
  timeandtype <- time
  timeandtype$LVV_Type <- type$LVV_Type
  timeandtype <- tibble::rownames_to_column(timeandtype, "MRN")
  
  
  output <- list(map.damage,timeandtype)
  return(output)
}


