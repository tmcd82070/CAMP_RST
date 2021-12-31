#' @export
#' 
#' @title overDphi
#'   
#' @description For a set of covariates for which estimates have been obtained
#'   on non-continguous dates, expand recorded data values to fill in all dates,
#'   so as to create a step-like function.
#'   
#' @param model A \code{glm} object resulting from a call to function
#'   \code{glm}.  Typically either a catch Poisson model or efficiency binomial
#'   model.
#'   
#' @param family The family of exponential distributions.  Currently allows
#'   either of \code{family="binomial"} or \code{family="poisson"}.
#'   
#' @param type The type of residual to calculate.  Currently allows only
#'   \code{"pearson"}.
#'   
#' @return An estimate of the overdispersion \eqn{\phi} statistic from the
#'   fitting of either a \code{family="binomial"} or \code{family="poisson"}
#'   generlized linear model.
#'   
#' @details The \code{overDphi} function excludes Pearson residuals larger in
#'   magnitude of 8 in the case of binomial fits.  In the case of Poisson fits,
#'   only the residuals falling within the 20% and 80% percentiles are retained.
#'   
#'   Estimates of the overdispersion less than one are set to one.  
#'   
#' @author WEST Inc.
#'   
#' @examples
#' \dontrun{
#' disp <- overDphi(model=fit,
#'                  family="binomial",
#'                  type="pearson")
#' }
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