

# get data from https://osf.io/kxmjs/

getOSFdata <- function() {
  
  Reach::downloadOSFdata(
    repository = 'kxmjs',
    filelist   = list('data'='exp_2.zip', 'exp_1.zip'),
    folder     = 'data',
    overwrite  = TRUE,
    unzip      = FALSE,
    removezips = FALSE,
    wait = 40
  )
  
}


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