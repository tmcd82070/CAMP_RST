#' @export
#' 
#' @title covarPlot
#'   
#' @description For a trap, plot efficiency against an environmental covariate 
#'   and the covariate against time.
#'   
#' @param covar A unit-length character specifying the covariate of interest
#'   with its associated unit, e.g., \code{turbidity_ntu}.
#'   
#' @param df A data frame containing at least numerator and denominator
#'   efficiency information, i.e., variables \code{nCaught} and
#'   \code{nReleased}.
#'   
#' @param dbCovar A data frame containing queried data from a CAMP database for 
#'   one environmental covariate.
#'   
#' @param trap A unit-length numeric corresponding to a \code{trapPositionID}.
#'   
#' @param eff.ind.inside A logical vector identifying which visit dates
#'   correspond to dates within a fishing period, as defined by when efficiency
#'   trials took place.
#'  
#' @return Plots to screen, as described in the description.  
#'  
#' @details The function is intended to be called as part of a larger script.  
#' As such, the function itself doesn't include any \code{png} statements or 
#' \code{dev.off()} of the same.  
#'  
#' @seealso \code{estCovar}
#'  
#' @author WEST Inc.
#'  
#' @examples
#' \dontrun{
#' covarPlot("turbidity_ntu",obs.eff.df,dbTurb,57004,eff.ind.inside)
#'}

covarPlot <- function(covar,df,dbCovar,trap,eff.ind.inside,bsplBegDt){
  
  # covar <- "turbidity_ntu"
  # df <- obs.eff.df
  # dbCovar <- dbTurb
  # trap <- 57004
  # eff.ind.inside <- eff.ind.inside
  
  #   ---- We give the function all the data, so restrict to the trap.  
  df <- df[df$TrapPositionID == trap,]
  effdf <- df[!is.na(df$efficiency),]
  
  #   ---- Plot of efficiency versus covariate.
  if(covar %in% names(effdf)){
    plot(effdf[,covar],100*effdf$efficiency,type="p",pch=19,xlab=NA,ylab='Efficiency (%)',main=paste0(covar," at ",effdf$TrapPositionID[1]))
  } else {
    plot(1,1)
  }
  
  #   ---- Plot of variable 3 versus time with smoother. Currently assumes 1 entry in the list df.covar.
  if(covar %in% names(df)){
    
    bsplBegDt$mon + 1
    
    #   ---- Find the range with respect to the data we use.
    xS <- df$batchDate
    yS <- df[,covar]

    #   ---- Find the range with respect to the data recorded.  
    xD <- dbCovar[dbCovar$subSiteID == trap,]$measureTime
    yD <- dbCovar[dbCovar$subSiteID == trap,3]
    
    xm <- min(xD[!is.na(xD)])
    xM <- max(xD[!is.na(xD)])
    ym <- min(yS[!is.na(yS)])
    yM <- max(yS[!is.na(yS)])

    plot(xD,yD,xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(xm,xM),ylim=c(ym,yM),type="p",cex=0.25,pch=19,main=paste0(covar," at ",df$TrapPositionID[1]))
    par(new=TRUE)
    plot(xS[eff.ind.inside],yS[eff.ind.inside],xlab='Date',ylab=covar,xlim=c(xm,xM),ylim=c(ym,yM),type="l",col="red")

  } else {
    plot(1,1)
  }
  
}