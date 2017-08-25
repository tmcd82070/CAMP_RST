
fitSpline <- function(covarString,df,eff.ind.inside,tmp.df,dist){

  # covarString <- covarString   #covarString,df,eff.ind.inside,tmp.df
  # df <- df
  # eff.ind.inside <- eff.ind.inside
  # tmp.df <- tmp.df
  # dist <- "quasibinomial"
  
  #   ---- Record the dates we actually do the spline.  Tricky because bd2 is the mapped set of dates.
  #   ---- I specifically use tz = "UTC" to get out of daylight savings madness.  
  s.beg <- as.POSIXct(strftime(min(df[eff.ind.inside,]$batchDate2),tz="UTC"),format="%Y-%m-%d",tz="UTC")
  s.end <- as.POSIXct(strftime(max(df[eff.ind.inside,]$batchDate2),tz="UTC"),format="%Y-%m-%d",tz="UTC")
  
  
  
  
  
  #   ---- At least one efficiency trial "inside" for this trap.  Fit a "null" model.  
  fit <- glm( as.formula(paste0("nCaught / nReleased ~ ",covarString)), family=dist, data=tmp.df, weights=tmp.df$nReleased ) 
  fit.AIC <- AIC(fit)
  cat(paste0("Considering different temporal splines:  \n"))
  cat(paste("df= ", 1, ", conv= ", fit$converged, " bound= ", fit$boundary, " AIC= ", round(fit.AIC, 4), "\n"))
  
  cur.df <- 3
  repeat{

    
    cur.bspl <- bs( df$batchDate2[eff.ind.inside], df=cur.df)#, knots= )
    tmp.bs <- cur.bspl[!is.na(df$efficiency[eff.ind.inside]),]
    
    #   ---- Fit updated model.
    if(covarString == ""){  # We could NOW have a blank covarString.  Note the lack of the '+' below.
      cur.fit <- glm( as.formula(paste0("nCaught / nReleased ~ tmp.bs ",covarString)), family=dist, data=tmp.df, weights=tmp.df$nReleased )
    } else {
      cur.fit <- glm( as.formula(paste0("nCaught / nReleased ~ tmp.bs + ",covarString)), family=dist, data=tmp.df, weights=tmp.df$nReleased )
    }
    
  cur.AIC <- AIC(cur.fit)
    
  cat(paste("df= ", cur.df, ", conv= ", cur.fit$converged, " bound= ", cur.fit$boundary, " AIC= ", round(cur.AIC, 4), "\n"))
    
    if( !cur.fit$converged | cur.fit$boundary | cur.df > max.df.spline | cur.AIC > (fit.AIC - 2) ){
      break
    } else {
      fit <- cur.fit
      fit.AIC <- cur.AIC
      bspl <- cur.bspl
      tmp.bs <- tmp.bs
      cur.df <- cur.df + 1
      disp <- sum(residuals(cur.fit, type="pearson")^2)/cur.fit$df.residual
    }
  }
  ans <- list(fit=fit,fit.AIC=fit.AIC,bspl=bspl,tmp.bs=tmp.bs,cur.df=cur.df,disp=disp,s.beg=s.beg,s.end=s.end)
  return(ans)
}