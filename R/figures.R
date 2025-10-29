
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
  
  
  layout(mat=matrix(c(1:2),ncol=1))
  
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
  
  locdf <- read.csv('data/exp1/localization_deviations.csv', stringsAsFactors = FALSE)
  
  for (direction in c('cw','ccw')) {
    
    col <- c('ccw'='orange','cw'='turquoise')[direction]
    
    legend_labels <- c()
    legend_colors <- c()
    legend_lty    <- c()
    
    plot(NULL,
         xlim=c(170,10),ylim=c(-5,50),
         main='',xlab='target [deg]',ylab='reach aftereffect [deg]',
         bty='n',ax=F)
    
    lines(x=c(10,170),y=c(0,0),col='#CCCCCC',lty=2)
    lines(x=c(10,170),y=c(45,45),col='#CCCCCC',lty=2)
    lines(x=rep(90-(45/2),2),y=c(0,45),col='#CCCCCC')
    lines(x=rep(90+(45/2),2),y=c(0,45),col='#CCCCCC')
    lines(x=rep(90,2),y=c(0,45),col='#CCCCCC',lty=2)
    
    ddf <- df[which(df$direction == direction),]
    
    aligned_all <- aggregate(reachdeviation_deg ~ targetangle_deg * participant, data = ddf[which(df$block %in% c(2:11)),], FUN=median, na.rm=TRUE)
    rotated_all <- aggregate(reachdeviation_deg ~ targetangle_deg * participant, data = ddf[which(df$block %in% c(14:23)),], FUN=median, na.rm=TRUE)
    
    # baseline the rotated data per participant:
    participants <- unique(rotated_all$participant)
    for (ppid in participants) {
      for (target in unique(rotated_all$targetangle_deg)) {
        al_idx <- which(aligned_all$participant == ppid & aligned_all$targetangle_deg == target)
        target_bias <- median(aligned_all$reachdeviation_deg[al_idx], na.rm=TRUE)
        ro_idx <- which(rotated_all$participant == ppid & rotated_all$targetangle_deg == target)
        rotated_all$reachdeviation_deg[ro_idx] <- rotated_all$reachdeviation_deg[ro_idx] - target_bias
      }
    }
    
    # confidence intervals:
    CI <- aggregate(reachdeviation_deg ~ targetangle_deg,
                      data = rotated_all,
                      FUN = Reach::getConfidenceInterval,
                      method='b')

    lo <- CI$reachdeviation_deg[,1]
    hi <- CI$reachdeviation_deg[,2]


    polygon( x = c( CI$targetangle_deg, rev(CI$targetangle_deg)),
             y = c( lo, rev(hi)),
             border=NA,
             col = Reach::colorAlpha(col=col, alpha=34),
             xpd=TRUE)
    
    # aligned <- aggregate(reachdeviation_deg ~ targetangle_deg, data = aligned_all, FUN=mean, na.rm=TRUE)
    rotated <- aggregate(reachdeviation_deg ~ targetangle_deg, data = rotated_all, FUN=mean, na.rm=TRUE)    
    
    # print(implicit_generalization)
    lines( x = rotated$targetangle_deg,
           y = rotated$reachdeviation_deg,
           col=col)
    
    legend_labels <- c(legend_labels, 'aftereffects')
    legend_colors <- c(legend_colors, col)
    legend_lty    <- c(legend_lty, 1)
    
    # add hand localization generalization curve
    
    dlocdf <- locdf[which(locdf$participant %in% unique(ddf$participant)),]
    
    if (direction == 'cw') {
      dlocdf$locdev_deg <- -1 * dlocdf$locdev_deg
    }
    
    loc_aligned_all <- aggregate(locdev_deg ~ targetangle_deg * participant, data = dlocdf[which(dlocdf$block %in% c(2:11)),], FUN=median, na.rm=TRUE)
    loc_rotated_all <- aggregate(locdev_deg ~ targetangle_deg * participant, data = dlocdf[which(dlocdf$block %in% c(14:23)),], FUN=median, na.rm=TRUE)
    
    # baseline the rotated data per participant:
    participants <- unique(loc_rotated_all$participant)
    for (ppid in participants) {
      for (target in unique(loc_rotated_all$targetangle_deg)) {
        al_idx <- which(loc_aligned_all$participant == ppid & loc_aligned_all$targetangle_deg == target)
        target_bias <- median(loc_aligned_all$locdev_deg[al_idx], na.rm=TRUE)
        ro_idx <- which(loc_rotated_all$participant == ppid & loc_rotated_all$targetangle_deg == target)
        loc_rotated_all$locdev_deg[ro_idx] <- loc_rotated_all$locdev_deg[ro_idx] - target_bias
      }
    }
    
    col <- c('ccw'='red','cw'='blue')[direction]
    
    # print confidence intervals:
    loc_CI <- aggregate(locdev_deg ~ targetangle_deg,
                      data = loc_rotated_all,
                      FUN = Reach::getConfidenceInterval,
                      method='b')
    
    loc_lo <- loc_CI$locdev_deg[,1]
    loc_hi <- loc_CI$locdev_deg[,2]
    
    polygon( x = c( loc_CI$targetangle_deg, rev(loc_CI$targetangle_deg)),
             y = c( loc_lo, rev(loc_hi)),
             border=NA,
             col = Reach::colorAlpha(col=col, alpha=17),
             xpd=TRUE)
    
    loc_rotated <- aggregate(locdev_deg ~ targetangle_deg, data = loc_rotated_all, FUN=mean, na.rm=TRUE)
    lines( x = loc_rotated$targetangle_deg,
           y = loc_rotated$locdev_deg,
           col=col,
           lty=3)
    
    legend_labels <- c(legend_labels, 'hand localization shift')
    legend_colors <- c(legend_colors, col)
    legend_lty    <- c(legend_lty, 3)
    
    axis(side=1,at=c(15,40,65,90,115,140,165))
    axis(side=2,at=c(0,15,30,45))
    
    text(x=90+(c(-1,1)*(45/2)),
         y=c(55,55),
         labels = list('cw'=c('cursor/\ntarget','ideal\nhand'),
                       'ccw'=c('ideal\nhand','cursor/\ntarget'))[[direction]],
         xpd=TRUE)
    
    legend( x = c('cw'=165,'ccw'=60)[direction],
            y = 45,
            legend = legend_labels,
            col = legend_colors,
            lty = legend_lty,
            bty = 'n',
            cex=0.8)
    
  }
  
}