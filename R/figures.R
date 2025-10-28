
plotTrainingReachesExp1 <- function() {
  
  df <- read.csv('data/exp1/training_reachdeviations.csv', stringsAsFactors = FALSE)
  
  participants <- unique(df$participant)
  
  # normalize & baseline:
  
  for (ppid in participants) {
    
    # participant data;
    ppdf <- df[which(df$participant == ppid),]
    
    # print(ppid)
    # baseline <- mean(ppdf$reachdeviation_deg[which(ppdf$block %in% c(6,7,8,9,10,11))], na.rm=TRUE)
    # print(baseline)
    
    # baseline on second half of aligned phase:
    ppdf$reachdeviation_deg <- ppdf$reachdeviation_deg - median(ppdf$reachdeviation_deg[which(ppdf$block %in% c(6,7,8,9,10,11))], na.rm=TRUE)
    
    # normalize and put back in main data frame:
    if (ppdf$rotation_deg[nrow(ppdf)] > 0) {
      df$reachdeviation_deg[which(df$participant == ppid)] <- -1 * ppdf$reachdeviation_deg
    }

  }
  
  aggRD <- aggregate(reachdeviation_deg ~ trial, data=df, FUN=mean)
  
  plot(aggRD, type='l')
  lines( x = c(1,385,385,767),
         y = c(0,0,45,45),
         col='#FF9999')  # 385 -- 767
  
}


plotImplicitGeneralization <- function() {
  
  df <- read.csv('data/exp1/nocursor_reachdeviations.csv', stringsAsFactors = FALSE)
  
  # SPLIT BY ROTATION DIRECTION!
  
  df$direction <- ''
  
  for (ppid in unique(df$participant)) {
    idx <- which(df$participant == ppid)
    if (mean(df$rotation_deg[idx]) > 0) {
      df$direction[idx] <- 'ccw'
      df$reachdeviation_deg[idx] <- -1 * df$reachdeviation_deg[idx]
    } else {
      df$direction[idx] <- 'cw'
    }
    
  }
  
  
  for (direction in c('cw','ccw')) {
    
    plot(NULL,
         xlim=c(10,170),ylim=c(-5,50),
         main='',xlab='target [deg]',ylab='reach aftereffect [deg]',
         bty='n',ax=F)
    
    lines(x=c(10,170),y=c(0,0),col='#CCCCCC',lty=2)
    lines(x=c(10,170),y=c(45,45),col='#CCCCCC',lty=2)
    lines(x=rep(90-(45/2),2),y=c(0,45),col='#CCCCCC')
    lines(x=rep(90+(45/2),2),y=c(0,45),col='#CCCCCC')
    lines(x=rep(90,2),y=c(0,45),col='#CCCCCC',lty=2)
    
    ddf <- df[which(df$direction == direction),]
    
    aligned <- aggregate(reachdeviation_deg ~ targetangle_deg, data = ddf[which(df$block %in% c(2:11)),], FUN=mean, na.rm=TRUE)
    
    rotated <- aggregate(reachdeviation_deg ~ targetangle_deg, data = ddf[which(df$block %in% c(14:23)),], FUN=mean, na.rm=TRUE)
    
    target <- aligned$targetangle_deg
    reachaftereffect <- rotated$reachdeviation_deg - aligned$reachdeviation_deg
    
    implicit_generalization <- data.frame(target=target, reachaftereffect=reachaftereffect)
    
    print(implicit_generalization)
    lines( x = implicit_generalization$target,
           y = implicit_generalization$reachaftereffect,
           col=c('ccw'='orange','cw'='turquoise')[direction])
    
    axis(side=1,at=c(15,40,65,90,115,140,165))
    axis(side=2,at=c(0,15,30,45))
    
    text(x=90+(c(-1,1)*(45/2)),
         y=c(55,55),
         labels = list('cw'=c('training\ntarget','actual\nhand'),
                       'ccw'=c('actual\nhand','training\ntarget'))[[direction]],
         xpd=TRUE)
    
  }
  
}