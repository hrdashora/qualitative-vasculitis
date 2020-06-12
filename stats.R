stats <- function(crop.output){
  
  map <- crop.output[[1]]
  
  # Lesion counts
  static.offtarget <- which(map == 0, arr.ind = TRUE) # 0 -> 0 (score = 0)
  static.offtarget.count <- dim(static.offtarget)[1] # total count of lesion type
  static.offtarget.id <- length(unique(static.offtarget[,"row"])) # number of unique patients with lesion type
  
  static.ontarget <- which(map == -1, arr.ind = TRUE) # 1 -> 1S (score = -1)
  static.ontarget.count <- dim(static.ontarget)[1]
  static.ontarget.id <- length(unique(static.ontarget[,"row"]))
  
  damage.new <- which(map == -3, arr.ind = TRUE) # 0 -> 1 (score = -3)
  damage.new.count <- dim(damage.new)[1]
  damage.new.id <- length(unique(damage.new[,"row"]))
  
  damage.worse <- which(map == -2, arr.ind = TRUE) # 1 -> 1W (score = -2)
  damage.worse.count <- dim(damage.worse)[1]
  damage.worse.id <- length(unique(damage.worse[,"row"]))
  
  damage.heal <- which(map == 3, arr.ind = TRUE) # 1 -> 0 (score = 3)
  damage.heal.count <- dim(damage.heal)[1]
  damage.heal.id <- length(unique(damage.heal[,"row"]))
  
  damage.better <- which(map == 2, arr.ind = TRUE) # 1 -> 1B (score = 2)
  damage.better.count <- dim(damage.better)[1]
  damage.better.id <- length(unique(damage.better[,"row"]))
  
  total <- which(map == 0 | map == -1 | map == -3 | map == -2 | map == 3 | map == 2, arr.ind = TRUE)
  total.count <- dim(total)[1]
  total.id <- length(unique(total[,"row"]))
  
  # Baseline counts
  baseline.active <- which(map == -1 | map == -2 | map == 3 | map == 2, arr.ind = TRUE)
  baseline.active.count <- dim(baseline.active)[1]
  baseline.active.id <- length(unique(baseline.active[,"row"]))
  
  baseline.inactive <- which(map == 0 | map == -3 , arr.ind = TRUE)
  baseline.inactive.count <- dim(baseline.inactive)[1]
  baseline.inactive.id <- length(unique(baseline.inactive[,"row"]))
  
  baseline.improvement <- which(map == 3 | map == 2, arr.ind = TRUE)
  baseline.improvement.count <- dim(baseline.improvement)[1]
  baseline.improvement.id <- length(unique(baseline.improvement[,"row"]))
  
  mixed.positive <- which((map == 3 | map == 2), arr.ind = TRUE)
  mixed.negative <- which((map == -3 | map == -2), arr.ind = TRUE)
  mixed.positive.id <- unique(mixed.positive[,"row"])
  mixed.negative.id <- unique(mixed.negative[,"row"])
  mixed.intersect.id <- length(intersect(mixed.positive.id,mixed.negative.id))
  
  lesion.counts <- cbind(c(static.offtarget.count,
                           static.ontarget.count,
                           damage.new.count,
                           damage.worse.count,
                           damage.heal.count,
                           damage.better.count,
                           total.count),
                         c(static.offtarget.id,
                           static.ontarget.id,
                           damage.new.id,
                           damage.worse.id,
                           damage.heal.id,
                           damage.better.id,
                           total.id))
  
  baseline.counts <- cbind(c(baseline.active.count,
                             baseline.inactive.count,
                             baseline.improvement.count),
                           c(baseline.active.id,
                             baseline.inactive.id,
                             baseline.improvement.id))
  
  # Output
  output <- list(lesion.counts, baseline.counts)
  return(output)
}