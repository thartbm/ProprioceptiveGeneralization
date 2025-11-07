
# setupFigureFile <- function(target='inline',width=8,height=6,dpi=300,filename) {
#   
#   if (target == 'pdf') {
#     pdf(file   = filename, 
#         width  = width, 
#         height = height)
#   }
#   if (target == 'svg') {
#     svglite::svglite( filename = filename,
#                       width = width,
#                       height = height,
#                       fix_text_size = FALSE) 
#     # fix_text_size messes up figures on my machine... 
#     # maybe it's better on yours?
#   }
#   if (target == 'png') {
#     png( filename = filename,
#          width = width*dpi,
#          height = height*dpi,
#          res = dpi
#     )
#   }
#   if (target == 'tiff') {
#     tiff( filename = filename,
#           compression = 'lzw',
#           width = width*dpi,
#           height = height*dpi,
#           res = dpi
#     )
#   }
# }

# plotTrainingReachesExp1 <- function() {
#   
#   df <- read.csv('data/exp1/training_reachdeviations.csv', stringsAsFactors = FALSE)
#   
#   participants <- unique(df$participant)
#   
#   # normalize & baseline:
#   
#   for (ppid in participants) {
#     
#     # participant data;
#     ppdf <- df[which(df$participant == ppid),]
#     
#     # print(ppid)
#     # baseline <- mean(ppdf$reachdeviation_deg[which(ppdf$block %in% c(6,7,8,9,10,11))], na.rm=TRUE)
#     # print(baseline)
#     
#     # baseline on second half of aligned phase:
#     ppdf$reachdeviation_deg <- ppdf$reachdeviation_deg - median(ppdf$reachdeviation_deg[which(ppdf$block %in% c(6,7,8,9,10,11))], na.rm=TRUE)
#     
#     # normalize and put back in main data frame:
#     if (ppdf$rotation_deg[nrow(ppdf)] > 0) {
#       df$reachdeviation_deg[which(df$participant == ppid)] <- -1 * ppdf$reachdeviation_deg
#     }
# 
#   }
#   
#   aggRD <- aggregate(reachdeviation_deg ~ trial, data=df, FUN=mean)
#   
#   plot(aggRD, type='l')
#   lines( x = c(1,385,385,767),
#          y = c(0,0,45,45),
#          col='#FF9999')  # 385 -- 767
#   
# }

getColors <- function() {
  
  cols.op <- c(rgb(255, 147, 41,  255, max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  255, max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 255, max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 255, max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 255, max = 255)) # blue:   164, 255, 68
  
  cols.tr <- c(rgb(255, 147, 41,  32,  max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  32,  max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 32,  max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 32,  max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 32,  max = 255)) # blue:   164, 255, 68
  
  cols <- list()
  cols$op <- cols.op
  cols$tr <- cols.tr
  
  return(cols)
  
}


fig2_exp1 <- function(target='inline') {
  
  Reach::setupFigureFile( target   = target,
                          width    = 4.5,
                          height   = 6,
                          dpi      = 300,
                          filename = 'doc/figures/fig2_exp1' )
  
  layout(mat=matrix(c(1:3),ncol=1))
  
  par(mar=c(4.5,4,0.5,0.5))
  
  
  df <- read.csv('data/exp1/training_deviations.csv', stringsAsFactors = FALSE)
  
  df$direction <- ''
  
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
      df$direction[which(df$participant == ppid)] <- 'ccw'
    } else {
      df$direction[which(df$participant == ppid)] <- 'cw'
    }
    
  }
  
  # aggRD <- aggregate(reachdeviation_deg ~ trial, data=df, FUN=mean)
  # 
  # plot(aggRD, type='l')
  # lines( x = c(1,385,385,767),
  #        y = c(0,0,45,45),
  #        col='#FF9999')  # 385 -- 767
  
  Ntab <- aggregate(direction ~ participant, data=df, FUN=head, n=1)
  Ns <- table(Ntab$direction)
  
  plot( NULL, NULL,
        xlim=c(0,769), ylim=c(-10,50),
        xlab='trial', ylab='reach deviation [deg]', main='',
        bty='n', ax=F)
  
  lines( x = c(0,385,385,767),
         y = c(0,0,45,45),
         col='#000000')  # 385 -- 767
  
  for (direction in c('cw','ccw')) {
    
    col <- c('ccw'='orange','cw'='turquoise')[direction]
    
    ddf <- df[which(df$direction == direction),]
    
    CI <- aggregate(reachdeviation_deg ~ trial,
                    data = ddf,
                    FUN = Reach::getConfidenceInterval,
                    method='b')
    
    lo <- CI$reachdeviation_deg[,1]
    hi <- CI$reachdeviation_deg[,2]
    
    
    polygon( x = c( CI$trial, rev(CI$trial)),
             y = c( lo, rev(hi)),
             border=NA,
             col = Reach::colorAlpha(col=col, alpha=34),
             xpd=TRUE)
    
    aggRD <- aggregate(reachdeviation_deg ~ trial, data=ddf, FUN=mean)
    
    lines( x = aggRD$trial,
           y = aggRD$reachdeviation_deg,
           col=col)
    
  }
  
  axis(side=1,at=c(1,seq(96,769,by=96)),cex.axis=0.8)
  axis(side=2,at=c(0,15,30,45),cex.axis=0.8)
  
  legend( x = 100,
          y = 50,
          legend = sprintf('%s (N=%d)',c('ccw','cw'),Ns),
          col=c('orange','turquoise'),
          bty='n',
          lty=1)
  
  ## implicit measures / generalization
  
  ## first two aligned blocks are skipped for baselining
  ## first two rotated blocks are skipped to saturate adaptation (more or less)
  
  
  df <- read.csv('data/exp1/nocursor_deviations.csv', stringsAsFactors = FALSE)
  
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
         xlim=c(170,10),ylim=c(-15,60),
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
      for (targetang in unique(rotated_all$targetangle_deg)) {
        al_idx <- which(aligned_all$participant == ppid & aligned_all$targetangle_deg == targetang)
        target_bias <- median(aligned_all$reachdeviation_deg[al_idx], na.rm=TRUE)
        ro_idx <- which(rotated_all$participant == ppid & rotated_all$targetangle_deg == targetang)
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
    
    # implicit generalization peak locations:
    
    gdf <- rotated_all
    names(gdf) <- c('x', 'ID', 'y')
    bs <- bootstrapSplineInterpolation( df         = gdf,
                                        bootstraps = 5000,
                                        spar       = 0.25, 
                                        npoints    = 1501,
                                        # xout=seq(min(df$x),max(df$x), length.out=npoints),
                                        # nknots=6
          )
    
    # print(dim(bs$y))
    peaks <- bs$x[apply(bs$y, MARGIN=2, FUN=which.max)]
    CI <- quantile(peaks, probs=c(0.025,0.5,0.975))
    # print(CI)
    
    polygon( x = c( CI[1], CI[3], CI[3], CI[1]),
             y = c( -2.5, -2.5, -7.5, -7.5),
             border=NA,
             col = Reach::colorAlpha(col=col, alpha=34),
             xpd=TRUE)
    lines( x = c( CI[2], CI[2]),
           y = c(-2.5, -7.5),
           col = col)
    
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
      for (targetang in unique(loc_rotated_all$targetangle_deg)) {
        al_idx <- which(loc_aligned_all$participant == ppid & loc_aligned_all$targetangle_deg == targetang)
        target_bias <- median(loc_aligned_all$locdev_deg[al_idx], na.rm=TRUE)
        ro_idx <- which(loc_rotated_all$participant == ppid & loc_rotated_all$targetangle_deg == targetang)
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
    
    
    gdf <- loc_rotated_all
    names(gdf) <- c('x', 'ID', 'y')
    bs <- bootstrapSplineInterpolation( df         = gdf,
                                        bootstraps = 5000,
                                        spar       = 0.25, 
                                        npoints    = 1501,
                                        # xout=seq(min(df$x),max(df$x), length.out=npoints),
                                        # nknots=6
    )
    
    # print(dim(bs$y))
    peaks <- bs$x[apply(bs$y, MARGIN=2, FUN=which.max)]
    CI <- quantile(peaks, probs=c(0.025,0.5,0.975))
    # print(CI)
    
    polygon( x = c( CI[1], CI[3], CI[3], CI[1]),
             y = c( -10, -10, -15, -15),
             border=NA,
             col = Reach::colorAlpha(col=col, alpha=34),
             xpd=TRUE)
    lines( x = c( CI[2], CI[2]),
           y = c(-10, -15),
           col = col)
    
    
    legend_labels <- c(legend_labels, 'localization shift')
    legend_colors <- c(legend_colors, col)
    legend_lty    <- c(legend_lty, 3)
    
    axis(side=1,at=c(15,40,65,90,115,140,165),cex.axis=0.8)
    axis(side=2,at=c(0,15,30,45),cex.axis=0.8)
    
    text(x=90+(c(-1,1)*(45/2)),
         y=c(55,55),
         labels = list('cw'=c('cursor/\ntarget','ideal\nhand'),
                       'ccw'=c('ideal\nhand','cursor/\ntarget'))[[direction]],
         xpd=TRUE)
    
    legend( x      = c('cw'=165,'ccw'=60)[direction],
            y      = 45,
            legend = legend_labels,
            col    = legend_colors,
            lty    = legend_lty,
            bty    = 'n',
            cex    = 0.8 )
    
  }
  
  if (target %in% c('pdf','svg','png','tiff')) {
    # cat('need to turn off the device\n')
    dev.off()
  }
  
}

fig3_exp2 <- function(target='inline') {
  
  Reach::setupFigureFile( target   = target,
                          width    = 4.5,
                          height   = 6,
                          dpi      = 300,
                          filename = 'doc/figures/fig3_exp2' )
  
  colors <- getColors()
  
  layout(mat=matrix(c(1:3),ncol=1))
  
  par(mar=c(4.5,4,0.5,0.5))
  
  tdf <- getNormalizedDeviations(exp=2, trialtype='training')
  
  # print(length(unique(tdf$participant)))
  
  plot(NULL,NULL,
       xlim=c(0,769), ylim=c(-10,60),
       xlab='trial', ylab='reach deviation [deg]', main='',
       bty='n', ax=F)
  
  
  lines( x = c(0, 193, 193, 289, 289, 385, 385, 481, 481, 577, 577, 768),
         y = c(0,   0,  10,  10,  20,  20,  30,  30,  40,  40,  50,  50),
         col = '#999999')
  
  
  # check last 4 blocks for learners / non-learners:
  b50tdf <- tdf[which(tdf$block>19),]
  asymptotes <- aggregate(reachdeviation_deg ~ participant, data = b50tdf, FUN=median, na.rm=FALSE)
  # print(asymptotes)
  print(length(asymptotes$participant))
  okpp <- asymptotes$participant[which(asymptotes$reachdeviation_deg > 30)]
  print(length(okpp))
  tdf <- tdf[which(tdf$participant %in% okpp),]
  
  for (rotation in  c(0,10,20,30,40,50)) {
    
    rdf <- tdf[which(tdf$rotation_deg == rotation),]
    
    if (rotation == 0) {
      col.op <- '#00000099'
      col.tr <- '#00000020'
    } else {
      col.op <- colors$op[rotation/10]
      col.tr <- colors$tr[rotation/10]
    }
    
    CI <- aggregate(reachdeviation_deg ~ trial, data=rdf, FUN=Reach::getConfidenceInterval)
    polygon( x = c(CI$trial, rev(CI$trial)),
             y = c(CI$reachdeviation_deg[,1], rev(CI$reachdeviation_deg[,2])),
             col = col.tr,
             border = NA
             )
    
    avg <- aggregate(reachdeviation_deg ~ trial, data=rdf, FUN=mean)
    lines(avg$trial, avg$reachdeviation_deg, col=col.op)
    
  }
  
  axis(side = 1, at=c(1,seq(96,769,96)))
  axis(side = 2, at=c(0,10,20,30,40,50),las=2)
  
  # no-cursors
  plot(NULL,NULL,
       xlim=c(-60,110), ylim=c(-10,60),
       xlab='relative target [deg]', ylab='reach aftereffects [deg]', main='',
       bty='n', ax=F)
  
  lines(x = c(-60,110),
        y = c(0,0),
        col='#999999')
  lines(x = c(-60,110),
        y = c(50,50),
        col='#999999')
  lines(x = c(0,0),
        y = c(0,50),
        col='#999999', lty=2)
  lines(x = c(50,50),
        y = c(0,50),
        col='#999999', lty=2)
  
  
    
  ndf <- getNormalizedDeviations(exp=2, trialtype='nocursor')
  ndf <- ndf[which(ndf$participant %in% okpp),]
  
  for (rotation in  c(10,20,30,40,50)) {
    
    col.op <- colors$op[rotation/10]
    col.tr <- colors$tr[rotation/10]
    
    rdf <- ndf[which(ndf$rotation_deg == rotation),]
    blocks <- unique(rdf$block)
    if (length(blocks) == 3) {
      rdf <- rdf[which(rdf$block > min(blocks)),]
    }
    if (length(blocks) == 6) {
      rdf <- rdf[which(rdf$block > (min(blocks) + 1)),]
    }
    
    bsdf <- rdf[,c('reltargetangle_deg','participant','reachdeviation_deg')]
    names(bsdf) <- c('x', 'ID', 'y')
    bs <- bootstrapSplineInterpolation( df         = bsdf,
                                        bootstraps = 5000,
                                        spar       = 0.05, 
                                        npoints    = 1601,
                                        # xout=seq(min(df$x),max(df$x), length.out=npoints),
                                        nknots=5
    )
    
    # print(dim(bs$y))
    peaks <- bs$x[apply(bs$y, MARGIN=2, FUN=which.max)]
    CI <- quantile(peaks, probs=c(0.025,0.5,0.975))
    # print(CI)

    polygon( x = c( CI[1], CI[3], CI[3], CI[1]),
             y = c( 0, 0, 1.5, 1.5) + 51 + ((rotation-10)/5),
             border=NA,
             col = col.tr,
             xpd=TRUE)
    lines( x = c( CI[2], CI[2]),
           y = c( 0, 1.5) + 51 + ((rotation-10)/5),
           col = col.op)
    
    CI <- apply(bs$y, MARGIN=1, FUN=quantile, probs=c(0.025, 0.975))
    polygon( x = c(bs$x, rev(bs$x)),
             y = c(CI[1,], rev(CI[2,])),
             col = col.tr,
             border = NA)
    
    lines( x = bs$x,
           y = apply(bs$y, MARGIN=1, FUN=mean),
           col = col.op)

  }
  
  axis(side = 1, at = c(-55, -30,  -5,  15,  35,  55,  80, 105))
  axis(side = 2, at=c(0,10,20,30,40,50),las=2)
  
  text(x=c(0,50),
       y=c(-5.2,-5.2),
       labels = c('cursor/\ntarget','ideal\nhand'),
       xpd=TRUE)
  
  # hand localization
  plot(NULL,NULL,
       xlim=c(-60,110), ylim=c(-5,30),
       xlab='relative target [deg]', ylab='localization shift [deg]', main='',
       bty='n', ax=F)
  
  lines(x = c(-60,110),
        y = c(0,0),
        col='#999999')
  lines(x = c(-60,110),
        y = c(25,25),
        col='#999999')
  lines(x = c(0,0),
        y = c(0,25),
        col='#999999', lty=2)
  lines(x = c(50,50),
        y = c(0,25),
        col='#999999', lty=2)
  
  
  
  ldf <- getNormalizedDeviations(exp=2, trialtype='localization')
  ldf <- ldf[which(ldf$participant %in% okpp),]
  
  
  for (rotation in  c(10,20,30,40,50)) {
    
    col.op <- colors$op[rotation/10]
    col.tr <- colors$tr[rotation/10]
    
    rdf <- ldf[which(ndf$rotation_deg == rotation),]
    blocks <- unique(rdf$block)
    if (length(blocks) == 3) {
      rdf <- rdf[which(rdf$block > min(blocks)),]
    }
    if (length(blocks) == 6) {
      rdf <- rdf[which(rdf$block > (min(blocks) + 1)),]
    }
    
    bsdf <- rdf[,c('reltargetangle_deg','participant','locdev_deg')]
    names(bsdf) <- c('x', 'ID', 'y')
    bs <- bootstrapSplineInterpolation( df         = bsdf,
                                        bootstraps = 5000,
                                        spar       = 0.05,
                                        npoints    = 1601,
                                        # xout=seq(min(df$x),max(df$x), length.out=npoints),
                                        nknots=5
    )
    
    # print(dim(bs$y))
    peaks <- bs$x[apply(bs$y, MARGIN=2, FUN=which.max)]
    CI <- quantile(peaks, probs=c(0.025,0.5,0.975))
    # print(CI)
    
    polygon( x = c( CI[1], CI[3], CI[3], CI[1]),
             y = c( 0, 0, 0.75, 0.75) + 25.5 + ((rotation-10)/10),
             border=NA,
             col = col.tr,
             xpd=TRUE)
    lines( x = c( CI[2], CI[2]),
           y = c( 0, 0.75) + 25.5 + ((rotation-10)/10),
           col = col.op)
    
    CI <- apply(bs$y, MARGIN=1, FUN=quantile, probs=c(0.025, 0.975))
    polygon( x = c(bs$x, rev(bs$x)),
             y = c(CI[1,], rev(CI[2,])),
             col = col.tr,
             border = NA)
    
    lines( x = bs$x,
           y = apply(bs$y, MARGIN=1, FUN=mean),
           col = col.op)
    
  }
  
  axis(side = 1, at = c(-55, -30,  -5,  15,  35,  55,  80, 105))
  axis(side = 2, at=c(0,5,10,15,20,25),las=2)
  
  text(x=c(0,50),
       y=c(-2.6,-2.6),
       labels = c('cursor/\ntarget','ideal\nhand'),
       xpd=TRUE)
  
  if (target %in% c('pdf','svg','png','tiff')) {
    # cat('need to turn off the device\n')
    dev.off()
  }
  
}