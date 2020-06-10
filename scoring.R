# 'Static Lesion': 0 -> 0 (score = 0), 1 -> 1S (score = -1)
# 'Worse Lesion': 0 -> 1 (score = -3), 1 -> 1W (score = -2)
# 'Improve Lesion': 1 -> 0 (score = 3), 1 -> 1B (score = 2)

scoring <- function(df){
  # Initialize variables
  A_score <- numeric(1)
  S_score <- numeric(1)
  O_score <- numeric(1)
  
  # Score aneurysm
  A <- df %>% select(ends_with("Aneurysm"))
  if(length(A) > 0){
    Post <- A[2,1]
    Pre <- A[1,1]
    if (is.na(Pre) == FALSE && is.na(Post) == FALSE){
      if(Pre == "1"){
        if(Post == "1S"){
          A_score <- -1
        }else if(Post == "1B"){
          A_score <- 2
        }else if(Post == "1W"){
          A_score <- -2
        }else if(Post == "0"){
          A_score <- 3
        }
      }else if(Pre == "0"){
        if(Post == "1"){
          A_score <- -3
        }else if(Post == "0"){
          A_score <- 0
        }
      }
    }else { # NA 
      A_score <- NA
    }
  }else {
    A_score <- NA
  }
  
  # Score stenosis
  S <- df %>% select(ends_with("Stenosis"))
  if(length(S) > 0){
    Post <- S[2,1]
    Pre <- S[1,1]
    if (is.na(Pre) == FALSE && is.na(Post) == FALSE){
      if(Pre == "1"){
        if(Post == "1S"){
          S_score <- -1
        }else if(Post == "1B"){
          S_score <- 2
        }else if(Post == "1W"){
          S_score <- -2
        }else if(Post == "0"){
          S_score <- 3
          }
      }else if(Pre == "0"){
        if(Post == "1"){
          S_score <- -3
        }else if(Post == "0"){
          S_score <- 0
        }
      }
    }else {
      S_score <- NA
    }
  }else {
    S_score <- NA
  }
  
  # Score occlusion
  O <- df %>% select(ends_with("Occlusion"))
  if(length(O) > 0){
    Post <- O[2,1]
    Pre <- O[1,1]
    if (is.na(Pre) == FALSE && is.na(Post) == FALSE){
      if(Pre == "1"){
        if(Post == "1S"){
          O_score <- -1
        }else if(Post == "1B"){
          O_score <- 2
        }else if(Post == "1W"){
          O_score <- -2
        }else if(Post == "0"){
          O_score <- 3
        }
      }else if(Pre == "0"){
        if(Post == "1"){
          O_score <- -3
        }else if(Post == "0"){
          O_score <- 0
        }
      }
    }else {
      O_score <- NA
    }
  }else {
    O_score <- NA
  }
  
  # Overall score
  combo <- c(O_score, S_score, A_score) # Order priotiritizes occlusion for tiebreaks
  score <- combo[which.max(abs(combo))]
  if (length(score) == 0){ # Account for NA instances
    score <- NA
  }
 
  # Elapsed time
  E <- df$Scan_Date
  delta <- E[2] - E[1]
  
  # Output values
  output <- list(score,delta)
  return(output)
}