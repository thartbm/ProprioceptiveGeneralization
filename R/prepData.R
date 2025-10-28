# maybe this should be outside of the function libraries?

installReach <- function() {
  
  # library('remotes')
  ip <- installed.packages()
  if ('Reach' %in% ip[,'Package']) {
    if (ip[which(ip[,'Package'] == 'Reach'),'Version'] < "2023.12.17") {
      remotes::install_github('thartbm/Reach')
    }
  } else {
    remotes::install_github('thartbm/Reach')
  }
  
}

# get data from https://osf.io/kxmjs/

getOSFdata <- function(experiments=c(2,3)) {
  
  Reach::downloadOSFdata(
    repository = 'kxmjs',
    filelist   = list('data'=sprintf('exp_%d.zip', experiments)),
    folder     = 'data/ORG',
    overwrite  = TRUE,
    unzip      = TRUE,
    removezips = TRUE,
    wait = 40
  )
  
}


# exp X / exp 1 -----

processExpX <- function(rotations=c(45)) {
  
  demographics <- read.csv('data/ORG/exp_X/demographics.csv', stringsAsFactors = FALSE)
  
  demographics <- demographics[!is.na(demographics$age), ]
  
  pp_data <- c()
  
  for (ppid in demographics$participant) {
    
    if (dir.exists(sprintf('data/ORG/exp_X/r45/%s', ppid))) {
      
      pp_data <- c(pp_data, ppid)
      
    }
    
  }
  
  demographics <- demographics[demographics$participant %in% pp_data, ]
  
  dir.create('data/exp1', showWarnings = FALSE)
  
  write.csv(demographics, 'data/exp1/demographics.csv', row.names = FALSE)
  
  
  for (ppid in demographics$participant) {
    
    # copy the files:
    file.copy(
      from = sprintf('data/ORG/exp_X/r45/%s', ppid),
      to   = 'data/exp1/',
      recursive = TRUE
    )
    
    # correct the rotation columns for training and nocursor:
    for (trialtype in c('training','nocursor')) {
      df <- read.csv( sprintf('data/exp1/%s/%s.csv', ppid, trialtype), stringsAsFactors = FALSE )
      df$rotation_deg[which(df$block < 12)] <- 0
      write.csv( df, sprintf('data/exp1/%s/%s.csv', ppid, trialtype), row.names = FALSE )
    }

  }
  
}


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
  # distance = 2.5 cm
  
  distances <- sqrt(X^2 + Y^2)
  idx <- which(distances > distance)[1]
  
  x <- X[idx]
  y <- Y[idx]
  
  # # this interpolates a point usually just before 'distance'
  # # the method is too simple
  # fd <- abs(distances[idx]   - distance)
  # cd <- abs(distances[idx-1] - distance)
  # 
  # fw <- cd / (cd + fd)
  # # cw <- fd / (cd + fd)
  # 
  # x <- X[idx-1] + (diff(X[c(idx-1,idx)]) * fw)
  # y <- Y[idx-1] + (diff(Y[c(idx-1,idx)]) * fw)
  # 
  # # fcx <- X[idx] - (diff(X[c(idx-1,idx)]) * cw)
  # # fcy <- Y[idx] - (diff(Y[c(idx-1,idx)]) * cw)
  # # 
  # # x <- (cfx + fcx) / 2
  # # y <- (cfy + fcy) / 2
  # 
  # # print(sqrt(x^2 + y^2))
  
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