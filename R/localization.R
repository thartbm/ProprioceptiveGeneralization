
getLocalizationDeviations <- function(exp) {
  
  demographics <- read.csv(sprintf('data/exp%d/demographics.csv', exp), stringsAsFactors = FALSE)
  
  all_loc_devs <- NA
  
  for (ppid in demographics$participant) {
    # print(ppid)
    df <- read.csv( sprintf('data/exp%d/%s/localization.csv', exp, ppid), stringsAsFactors = FALSE )
    
    df$locdev_deg <- NA
    
    for (target in unique(df$targetangle_deg)) {
      
      target_idx <- which(df$targetangle_deg == target)
      
      th <- (-1*target/180) * pi
      R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
      
      x <- df$tapx_cm[target_idx]
      y <- df$tapy_cm[target_idx]
      norm_sample <- matrix(data=c(x,y),ncol=2) %*% R
      
      # print(norm_sample)
      
      locdev <- (atan2(norm_sample[,2], norm_sample[,1]) / pi) * 180
      # print(locdev)
      df$locdev_deg[target_idx] <- locdev

    }
    
    if (is.data.frame(all_loc_devs)) {
      all_loc_devs <- rbind(all_loc_devs, df)
    } else {
      all_loc_devs <- df
    }
    
  }
  
  write.csv(all_loc_devs, sprintf('data/exp%d/localization_deviations.csv', exp), row.names=FALSE)
  
}