
bootstrapSplineInterpolation <- function( df,
                                          bootstraps=1000,
                                          spar=0.25, 
                                          npoints=100,
                                          xout=seq(min(df$x),max(df$x), length.out=npoints),
                                          nknots=6
                                          ) {
  
  # df has a column with ID (participant), x (angle/direction), and y (reach deviation / response)
  # we bootstrap the IDs
  
  # make a matrix with IDs in columns, x in rows, and y in the cells:
  shortdf <- aggregate( y ~ x + ID, data=my_df, FUN=mean )
  data.matrix <- reshape2::dcast( shortdf, x ~ ID, value.var = 'y' )

  xvals <- data.matrix$x
  dm <- as.matrix( data.matrix[,-1] )  # remove x column
  
  # bootstrap the columns (IDs) using `sample()`
  boot_dm <- array( dm[, sample( 1:ncol(dm), 
                                 size=(bootstraps * ncol(dm)), 
                                 replace=TRUE ) ], 
                    dim=c(length(xvals),nrow(dm),bootstraps))
  
  # average across participants in each sample:
  boot_dm <- apply(boot_dm, MARGIN=c(1,3), FUN=mean, na.rm=TRUE)
  
  
  a <- apply ( X         = boot_dm,
               MARGIN    = 2, 
               FUN       = splineInterpolation,
               xvals  = xvals,
               spar   = spar,
               xout   = xout,
               nknots = nknots
               )
  
  return(list('m'=a, 'x'=xout))
  # spl <- stats::smooth.spline( x = df$x, 
  #                              y = df$y, 
  #                              spar = spar, 
  #                              keep.data=F )
  # 
  # predict.smooth.spline(spl, xout)$y
  
}

splineInterpolation <- function(y, xvals,
                                spar=0.8, 
                                xout=seq(min(x),max(x), length.out=100),
                                nknots=5) {
  
  # print(xvals)
  # print(y)
  
  spl <- stats::smooth.spline( x = xvals,
                               y = y,
                               spar = spar,
                               nknots=nknots,
                               )

  # predict.smooth.spline(spl, xout)$y
  
  # out <- predict(spl, xout)$y
  # print(out)
  
  out <- stats::predict(spl, xout)$y
  # print(out)
  
  return(out)
  
}