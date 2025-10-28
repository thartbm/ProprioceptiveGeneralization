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
    folder     = 'data',
    overwrite  = TRUE,
    unzip      = TRUE,
    removezips = TRUE,
    wait = 40
  )
  
}

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
