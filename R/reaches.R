
getReachDeviations <- function(exp, trialtype) {
  
  demographics <- read.csv(sprintf('data/exp%d/demographics.csv', exp), stringsAsFactors = FALSE)
  
  all_devs <- c()
  
  for (ppid in demographics$participant) {
    
    df <- read.csv( sprintf('data/exp%d/%s/%s.csv', exp, ppid, trialtype), stringsAsFactors = FALSE )
    
    trials <- unique(df$trial)
    
    for (trial in trials) {
      # print(trial)
      trial_df <- df[df$trial == trial, ]
      # print(nrow(trial_df))
      
      dev <- getReachDeviation(trial_df)
      
      all_devs <- rbind(all_devs, dev)
      
    }
    
  }
  
  all_devs <- as.data.frame(all_devs, stringsAsFactors = FALSE)
  
  write.csv(all_devs, sprintf('data/exp%d/%s_reachdeviations.csv', exp, trialtype), row.names = FALSE)
  
}

getReachDeviation <- function(df,distance=2.5) {
  
  
  target <- df$targetangle_deg[1]
  
  X <- df$handx_cm
  Y <- df$handy_cm
  
  # at 1/4 the target distance
  # target distance was 10 cm
  # 2.5 cm
  
  distances <- sqrt(X^2 + Y^2)
  idx <- which(distances > distance)[1]
  
  
  
  # print(idx)
  x <- X[idx]
  y <- Y[idx]
  
  
  fd <- abs(distances[idx]   - distance)
  cd <- abs(distances[idx-1] - distance)
  
  cprop <- cd / (cd + fd)
  fprop <- fd / (cd + fd)
  
  # print(c(cprop, fprop, cprop + fprop)) # should be 1
  
  x <- (X[idx-1] * fprop) + (X[idx] * cprop)
  y <- (Y[idx-1] * fprop) + (Y[idx] * cprop)
  
  # print(sqrt(x^2 + y^2))
  
  th <- (-1*target/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # rotate the coordinates, add the origin back in
  norm_sample <- matrix(data=c(x,y),ncol=2) %*% R
  
  # print(norm_sample)
  
  reachdev <- (atan2(norm_sample[2], norm_sample[1]) / pi) * 180
  
  return(c('participant' = df$participant[1],
           'trial' = df$trial[1],
           'block' = df$block[1],
           'set' = df$set[1],
           'targetangle_deg'=target,
           'rotation_deg'=df$rotation_deg[1],
           'reachdeviation_deg'=reachdev))
  
}