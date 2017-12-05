overDphi <- function(model,family="binomial",type){
  
  # model <- fit
  # family <- "binomial"
  # type <- "pearson"
  
  resids <- residuals(model,type)

  #   ---- For binomial overdispersion, there are not very many efficiency trials.
  #   ---- So, I do not want to toss the top 2.5% (or other) residuals in magintude, 
  #   ---- like I did for the catches.  However, we need to ensure that there are 
  #   ---- not some very very large residuals.  Thus, any greater than a cut off 
  #   ---- will be eliminated.
  if(family == "binomial"){
    toss.ind <- (abs(resids) > 8)
    resids <- resids[!toss.ind]
    disp <- sum( resids*resids ) / (model$df.residual - sum(toss.ind))
    if( disp < 1.0 ){
      disp <- 1.0
    }
  } else if(family == "poisson"){   # Not currently coded inside this function.
    
    #   ---- We must cut down the over-dispersion parameter to avoid obvious
    #   ---- a-typical residuals.  My approach is to toss the largest and smallest
    #   ---- 20% of residuals, then compute overdispersion.
    qrds <- quantile( resids, p=c(.2, .8))
    toss.ind <- (resids < qrds[1]) | (qrds[2] < resids)
    resids <- resids[!toss.ind]
    disp <- sum( resids*resids ) / (model$df.residual - sum(toss.ind))
    if( disp < 1.0 ){
      disp <- 1.0
    }
    
    #   ---- Uncomment the following line to include all residuals in overdispersion.
    # disp <- sum(residuals(model, type="pearson")^2) / model$df.residual
  }
  
  #   ---- Visually examine the residuals.  
  # plot(predict(fit,type="response"), residuals(fit, type))
    
  return(disp)
}