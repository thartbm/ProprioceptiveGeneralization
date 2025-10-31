# maybe this should be outside of the function libraries?

# packages -----

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

# unclean data -----

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


# data processing -----



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

getReachDeviation <- function(df,cutoff=2.5) {
  
  
  # X <- df$handx_cm
  # Y <- df$handy_cm
  
  # at 1/4 the target distance
  # target distance was 10 cm
  # distance = 2.5 cm
  
  alldistances <- sqrt(df$handx_cm^2 + df$handy_cm^2)
  idx <- which(alldistances > cutoff)[1]
  
  # sample before AND after the cutoff distance
  # x <- df$handx_cm[c(idx-1,idx)]
  # y <- df$handy_cm[c(idx-1,idx)]
  
  # deviations from cutoff distance
  # d1 <- abs(alldistances[idx-1] - cutoff)
  # d2 <- abs(alldistances[idx]   - cutoff)
  
  d <- abs(alldistances[c(idx-1,idx)] - cutoff)
  
  # weights are inverted relative deviations from cutoff distance
  # w1 <- d2 / (d1 + d2)
  # w2 <- d1 / (d1 + d2)
  
  w <- c(d[2], d[1]) / sum(d)
  
  # create rotation matrix that inverts the target direction:
  target <- df$targetangle_deg[1]
  th <- (-1*target/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  
  # rotate the coordinates such that the target is at 0 degrees:
  norm_sample <- matrix(data=c(df$handx_cm[c(idx-1,idx)],df$handy_cm[c(idx-1,idx)]),ncol=2) %*% R
  
  # get the reach deviation for both sample points:
  # bothreachdevs <- (atan2(norm_sample[,2], norm_sample[,1]) / pi) * 180
  
  # print(bothreachdevs)
  # reachdev <- (bothreachdevs[1] * w1) + (bothreachdevs[2] * w2)
  reachdev <- sum(((atan2(norm_sample[,2], norm_sample[,1]) / pi) * 180) * w)
  # print(reachdev)
  
  # ds <- sqrt(norm_sample[,1]^2 + norm_sample[,2]^2)
  # print(ds)
  # print(ds[1] * w1 + ds[2] * w2) # exactly my test distance of 2.5 (apart from rounding issues)
  
  
  return(
    c(
      'participant' = df$participant[1],
      'trial' = df$trial[1],
      'block' = df$block[1],
      'set' = df$set[1],
      'targetangle_deg'=target,
      'rotation_deg'=df$rotation_deg[1],
      'reachdeviation_deg'=reachdev)
  )
  
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


# clean & process exp X / exp 1 -----

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
  
  getReachDeviations(exp=1, trialtype='training')
  getReachDeviations(exp=1, trialtype='nocursor')
  getLocalizationDeviations(exp=1)
  
}







# clean & process exp Y / exp 2 -----

processExpY <- function() {
  
  demographics <- read.csv('data/ORG/exp_Y/demographics.csv', stringsAsFactors = FALSE)
  
  participants <- c()
  
  # check if we have data for all these participants:
  for (ppid in demographics$participant) {
    if (dir.exists(sprintf('data/ORG/exp_Y/%sd/', ppid))) {
      participants <- c(participants, ppid)
    }
  }
  
  # make new directory for exp 2 data:
  dir.create('data/exp2', showWarnings = FALSE)
  
  # save demographics for these participants only:
  demographics <- demographics[demographics$participant %in% participants, ]
  write.csv(demographics, 'data/exp2/demographics.csv', row.names = FALSE)
  
  # also save drawing data for these participants only:
  for (workspace in c('left','right')) {
    drawings <- read.csv( sprintf('data/ORG/exp_Y/drawing_%s.csv', workspace), stringsAsFactors = FALSE )
    drawings <- drawings[drawings$participant %in% participants, ]
  
    write.csv(drawings, sprintf('data/exp2/drawing_%s.csv', workspace), row.names = FALSE)
  }
  
  # now copy the behavioral data, and add reach deviations etc.
  
  for (ppid in participants) {
    
    # copy the files:
    file.copy(
      from = sprintf('data/ORG/exp_Y/%sd/', ppid),
      to   = 'data/exp2/',
      recursive = TRUE
    )
    
    # # correct the rotation columns for training and nocursor:
    # for (trialtype in c('training','nocursor')) {
    #   df <- read.csv( sprintf('data/exp2/%s/%s.csv', ppid, trialtype), stringsAsFactors = FALSE )
    #   # df$rotation_deg[which(df$block < 12)] <- 0
    #   write.csv( df, sprintf('data/exp2/%s/%s.csv', ppid, trialtype), row.names = FALSE )
    # }
    
  }
  
  
  
}