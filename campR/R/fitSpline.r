#' @export
#' 
#' @title fitSpline
#'   
#' @description Fit a temporal spline with a given set of covariates; i.e., fit
#'   a generalized additive model (GAM).
#'   
#' @param covarString A character string of length one housing all covariates to
#'   be considered, with all covariates collapsed via \code{" + "}.
#'   
#' @param df The data frame for a specific \code{TrapPositionID} containing 
#'   efficiency-trial information and covariates, if available, at the time of 
#'   fitting enhanced efficiency trials in \code{eff_model.r} (or 
#'   \code{F.efficiency.model }).
#'   
#' @param eff.ind.inside A vector of length equal to the number of rows in data 
#'   frame \code{df} set equal to \code{TRUE} when the \code{df} 
#'   \code{batchDate} is inside the data range specified by 
#'   \code{eff.inside.dates}.
#'   
#' @param tmp.df The reduced data frame originating from \code{df} containing 
#'   only efficiency-trial dates; i.e., those with non-\code{NA} 
#'   \code{nReleased} values.
#'   
#' @param dist The distributional family used in calls to function \code{glm}. 
#'   Almost always set to \code{"binomial"}.
#'   
#' @param max.df The highest number of degrees of freedom to be used in the 
#'   fitting of temporal-dimension splines.  Set by \code{max.df.spline}, which 
#'   is \code{4} in function \code{eff.models}.
#'   
#' @param eff.inside.dates A temporal \code{POSIX} vector of length two 
#'   indicating the first and last \code{batchDate} efficiency trials.
#'   
#' @return A list containing several different objects.  
#' 
#' \describe{
#'   \item{fit}{The \code{glm}-type object resulting from the fit.}
#'   \item{fit.AIC}{The AIC of \code{fit}.}
#'   \item{bspl}{The matrix of ALL \code{batchDate}s within \code{eff.inside.dates} of the final evaluated model. }
#'   \item{tmp.bs}{The matrix of REDUCED \code{batchDates}s from \code{bspl} tied to efficiency-trial dates.}
#'   \item{cur.df}{The final number of degrees-of-freedom used in the final temporal spline fit.}
#'   \item{disp}{}
#'   \item{s.beg}{The first date housed within the 1960-spline paradigm variable \code{batchDate2}.}
#'   \item{s.end}{The last date housed within the 1960-spline paradigm variable \code{batchDate2}.}
#'   \item{cur.bspl}{The matrix of ALL \code{batchDate}s within \code{eff.inside.dates} of \code{fit} model. }
#' }
#' 
#' @details Note that returned object \code{bspl} is not the same as 
#'   \code{cur.bspl}.  In order to stop, the loop that evaluates temporal 
#'   splines must first evaluate the next model.  In the case that the next 
#'   model fails to be better than the current model, the current model is the 
#'   winner. Object \code{cur.bspl} is the basis matrix associated with the 
#'   winning current model, while \code{bspl} is the basis maxtri associated 
#'   with that (non-winning) next model.
#'   
#'   The overdispersion parameter is fit in the traditional way, i.e., via
#'   Pearson residuals.
#'   
#' @examples  
#' \dontrun{
#' fit <- fitSpline(covarString,
#'                  df,
#'                  eff.ind.inside,
#'                  tmp.df,
#'                  dist,
#'                  max.df,
#'                  eff.inside.dates)
#' }
fitSpline <- function(covarString,df,eff.ind.inside,tmp.df,dist,max.df,eff.inside.dates){

  # covarString <- covarString   #covarString,df,eff.ind.inside,tmp.df
  # df <- df
  # eff.ind.inside <- eff.ind.inside
  # tmp.df <- tmp.df
  # dist <- "binomial"
  # max.df <- max.df.spline
  # eff.inside.dates <- eff.inside.dates
  
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
    
    if( !cur.fit$converged | cur.fit$boundary | cur.df > max.df | cur.AIC > (fit.AIC - 2) ){
      if(cur.df == 3){
        
        #   ---- If we're here, we have a model with no temporal component.  
        fit <- fit
        fit.AIC <- fit.AIC
        bspl <- matrix( 1, length(df$batchDate2[eff.ind.inside]), 1)#cur.bspl
        
        attr(bspl,"intercept") <- FALSE
        attr(bspl,"Boundary.knots") <- as.numeric(eff.inside.dates)
        attr(bspl,"knots") <- numeric(0)
        
        tmp.bs <- tmp.bs
        cur.df <- NA
        disp <- sum(residuals(fit, type="pearson")^2)/fit$df.residual
      }
      break
    } else {
      fit <- cur.fit
      fit.AIC <- cur.AIC
      bspl <- cur.bspl
      tmp.bs <- tmp.bs
      cur.df <- cur.df + 1
      disp <- sum(residuals(cur.fit, type="pearson")^2)/cur.fit$df.residual
      
      #   ---- Make sure we have a bs fit for the winning model, and not the next evaluated one. 
      cur.bsplF <- cur.bspl
    }
  }
  
  ans <- list(fit=fit,fit.AIC=fit.AIC,bspl=bspl,tmp.bs=tmp.bs,cur.df=cur.df,disp=disp,s.beg=s.beg,s.end=s.end,cur.bspl=cur.bsplF)
  return(ans)
}