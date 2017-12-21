#' @export
#' 
#' @title F.est.efficiency.enh
#'   
#' @description Estimate trap efficiency for every sample period, per trap.
#'   
#' @param release.df A data frame produced by \code{F.get.release.data}. 
#'   Contains information on releases and recaptures.  This data frame has one 
#'   line per release trial per trap, with trap identified via variable 
#'   \code{TrapPositionID}.
#'   
#' @param batchDate A POSIX-formatted vector of dates.
#'   
#' @param df.spline The default degrees of freedom to use in the estimation of 
#'   splines. Default is 4 (1 internal knot).
#'   
#' @param plot A logical indicating if efficiencies are to be plotted over time,
#'   per trap.
#'   
#' @param plot.file The name to which a graph of efficiency is to be output, if 
#'   \code{plot=TRUE}.
#'   
#' @return Results from running the enhanced efficiency model fitting process 
#'   include \code{csv}s of efficiency trials missing a covariate, for each
#'   \code{TrapPositionID} at a \code{subSiteID}.  This prevents these
#'   efficiency trials from inclusion in the model fitting process.
#'   
#'   Each \code{TrapPositionID} also results in a series of plots depicting the
#'   fitted temporal spline, at each point of the backwards-fitting process. 
#'   These could number many, depending on the number of covariates available
#'   for possible exlcusion.
#'   
#'   Each \code{trapPositionID} also outputs a \code{png} containing model
#'   fitting information, including plots of efficiency versus each considered
#'   covariate, along with plotted temporal trends of each covariate against
#'   time.  Additional plots include the final fitted temporal spline (along
#'   with an prediction "curve" derived from the available data), as well as a
#'   final "plot" depicting model summary statistics, obtained via the
#'   \code{summary} function against the logistic efficiency-trial model fits.
#'   
#'   Note that no passage estimates result from the fitting of enhanced
#'   efficiency models.  This is because bootstrapping does not occur, but also
#'   because estimation of passage is not the goal of the model fitting process.
#'   Use the regular function sequence; i.e., functions without the
#'   \code{".enh"} to estimation passage for one-year intervals of interest.
#'   
#' @section Five programs make up the specialized procedure for fitting enhanced
#'   efficiency models.  This means actually compiling the data of efficiency 
#'   trials obtained over several years, and then fitting a generalized additive
#'   model (GAM) to those data.  All five programs have suffixes of 
#'   \code{".enh"}, and originated with the program versions without the suffix.
#'   As such, they are very similar to the originals.
#'   
#'   The first, \code{run_passage.enh} corrals the fitting.  It is different 
#'   from \code{run_passage} in that all of the passage summary that is usually 
#'   created has been suppressed.  This is because there is no need to 
#'   bootstrap, once enhanced efficiency models have been obtained.
#'   
#'   The second, \code{get_release_data.enh} modifies the obtaining of release 
#'   data, so as to obtain astrological data and mean fork-length.  It is very 
#'   similar to its originator.
#'   
#'   The third, \code{est_passage.enh}, corrals the data from 
#'   \code{get_release_data.enh} for use in \code{est_efficiency.enh}.  It too 
#'   should be very similar to its originator.
#'   
#'   The fourth, \code{est_efficiency.enh}, ensures the calculation of weighted 
#'   averages for the three efficiency-trial covariates have to do with mean 
#'   fork-lengths, and percent of fishing performed at night, or while the moon 
#'   is up.  It also emulates closely its originator.
#'   
#'   Finally, the fifth, \code{eff_model.enh}, fits the enhanced efficiency 
#'   models.  It follows a backwards selection procedure, allowing for both 
#'   variable covariate selection, as well as variable temporal spline 
#'   complexity.  It creates graphical output, for each trap, so as to provide 
#'   further hypothesis generation.
#' 
#' @seealso \code{run_passage.enh}, \code{get_release_data},
#'   \code{est_passage.enh}, \code{est_efficiency.enh}, \code{eff_model.enh}
#'   
#' @author WEST Inc.
#'   
#' @examples
#' \dontrun{
#' #   ---- Estimate the efficiency.  
#' theEff <- F.est.efficiency(release.df,batchDate,df.spline=4,plot=TRUE,plots.file=NA)
#' }

F.est.efficiency.enh <- function( release.df, batchDate, df.spline=4, plot=TRUE, plot.file=NA ){

  # release.df <- release.df.enh
  # batchDate <- bd.enh
  # df.spline <- 4
  # plot <- TRUE
  # plot.file <- file.root
  
  time.zone <- get("time.zone", envir=.GlobalEnv)
  forEffPlots <- attr(release.df,"forEffPlots")

  #   ---- Fix up the data frame.
  rel.df <- release.df[,c("releaseID","ReleaseDate","nReleased","HrsToFirstVisitAfter",
  												"HrsToLastVisitAfter","trapPositionID","meanRecapTime","Recaps",'beg.date','end.date',
  												"allMoonMins","meanMoonProp",   # added this line for enhanced models for collapsing on batchDate.
  												"allNightMins","meanNightProp", # added this line for enhanced models for collapsing on batchDate.
  												"allfl","meanForkLength")]      # added this line for enhanced models for collapsing on batchDate.
  rel.df$batchDate <- rep(NA, nrow(rel.df))
  names(rel.df)[ names(rel.df) == "meanRecapTime" ] <- "EndTime"

  #   ---- The meanRecapTime is NA if they did not catch anything from a release.
  #   ---- This is different from the check on line 36, where there was no catch
  #   ---- over ALL releases.  In this NA case, replace any NA meanEndTimes with 
  #   ---- releaseTime plus mean of HrsToFirstVisitAfter and HrsToLastVisitAfter. 
  #   ---- This will assign a batch date.  
  ind <- is.na( rel.df$EndTime )
  rel.df$EndTime[ind] <- rel.df$ReleaseDate[ind] + (rel.df$HrsToFirstVisitAfter[ind] + rel.df$HrsToLastVisitAfter[ind]) / 2 

  #   ---- Assign batch date to efficiency trials based on meanEndTime (really weighted meanVisitTime).
  rel.df <- F.assign.batch.date( rel.df )
  rel.df$batchDate.str <- format(rel.df$batchDate,"%Y-%m-%d")

  
  rel.df[rel.df$releaseID %in% c(276,277),]
  
  #   ---- Sum by batch dates.  This combines release and catches over trials that occured close in time.  For enhanced
  #   ---- efficiency models, need to collapse prop of moon and night and forklength as well over batchDate.
  ind <- list( TrapPositionID=rel.df$trapPositionID,batchDate=rel.df$batchDate.str )
  nReleased <- tapply( rel.df$nReleased,ind, sum )
  nCaught   <- tapply( rel.df$Recaps,ind, sum )
  
  #lapply(split(truc, truc$x), function(z) weighted.mean(z$y, z$w)) 
  
  bdMeanNightProp  <- sapply( split(rel.df, ind) ,function(z) weighted.mean(z$meanNightProp,z$allNightMins) )
  bdMeanMoonProp   <- sapply( split(rel.df, ind) ,function(z) weighted.mean(z$meanMoonProp,z$allMoonMins) )
  bdMeanForkLength <- sapply( split(rel.df, ind) ,function(z) weighted.mean(z$meanForkLength,z$allfl) )

  eff.est <- cbind( expand.grid( TrapPositionID=dimnames(nReleased)[[1]],batchDate=dimnames(nReleased)[[2]]), 
                nReleased=c(nReleased),nCaught=c(nCaught),bdMeanNightProp=c(bdMeanNightProp),
                bdMeanMoonProp=c(bdMeanMoonProp),bdMeanForkLength=c(bdMeanForkLength) )
  eff.est$batchDate <- as.character(eff.est$batchDate)

  #   ================== done with data manipulations ===========================
  #   ---- Compute efficiency.
  eff.est$nReleased[ eff.est$nReleased <= 0] <- NA  # don't think this can happen, but just in case.
  eff.est$efficiency <- (eff.est$nCaught)/(eff.est$nReleased)  # eff$efficiency not used in computation, but is plotted. 
  eff.est <- eff.est[ !is.na(eff.est$efficiency), ]

  #   ---- Figure out which days have efficiency data.
  bd <- expand.grid(TrapPositionID=sort(unique(eff.est$TrapPositionID)),batchDate=format(batchDate,"%Y-%m-%d"),stringsAsFactors=F)
  eff <- merge( eff.est, bd, by=c("TrapPositionID","batchDate"),all.y=T)
  eff$batchDate <- as.POSIXct( eff$batchDate, format="%Y-%m-%d",tz=time.zone )

  #   ---- Assign attributes for plotting.  
  ind <- !duplicated(release.df$TrapPosition)
  attr(eff,"subsites") <- data.frame(subSiteName=as.character(release.df$TrapPosition[ind]),subSiteID=release.df$trapPositionID[ind],stringsAsFactors=F)
  attr(eff, "site.name") <- release.df$siteName[1]

  #   ---- If there are missing days, impute them.  
  missing.days <- is.na(eff$efficiency)
  attr(eff,"forEffPlots") <- forEffPlots
  if( any(missing.days) ){
    
    #   ---- Run specific efficiency model, based on input. 
    eff.and.fits <- suppressWarnings(F.efficiency.model.enh( eff, plot=plot, max.df.spline=df.spline, plot.file=plot.file ))
    
  } else {
    eff.and.fits <- list(eff=eff, fits=NULL, X=NULL, obs.data=eff.est)
    attr(eff.and.fits, "out.fn.list") <- NULL
  }

  eff.and.fits

}
