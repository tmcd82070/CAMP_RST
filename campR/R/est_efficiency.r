#' @export
#' 
#' @title F.est.efficiency
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
#' @param method The method by which efficiency is estimated.  By default always
#'   equal to 3. See Details.
#'   
#' @param df.spline The default degrees of freedom to use in the estimation of
#'   splines. By default always equal to 3. Only works when \code{method=3}.
#'   
#' @param plot A logical indicating if efficiencies are to be plotted over time,
#'   per trap.
#'   
#' @param plot.file The name to which a graph of efficiency is to be output, if 
#'   \code{plot=TRUE}.
#'   
#' @return A data frame containing fishing intervals and associated capture
#'   efficiency, along with variable \code{gam.estimated}.  Variable
#'   \code{gam.estimated} is \code{Yes} if efficiency for that interval was
#'   estimated by the GAM model (\code{method=3}), rather than being empirical
#'   (\code{method=1}).
#'   
#' @details Function \code{F.est.efficiency} first checks to ensure that at 
#'   least one fish, out of what could be many efficiency trials, was caught. 
#'   The lack of any caught fish prevents any estimation of total passage, given
#'   the role that efficiency plays as a denominator quantity.
#'   
#'   Generally, fish released as part of an efficiency trial arrive in traps 
#'   over the course of several days.  Function \code{F.est.efficiency} 
#'   condenses what may be a large temporal spread by calculating the mean 
#'   recapture time of all caught fish.  In the case that a release trial 
#'   resulted in no caught fish, the recorded \code{releaseTime}, plus the mean 
#'   of the recorded \code{HrsToFirstVisitAfter} and \code{HrsToLastVisitAfter},
#'   ensures the subsequent assigning of a \code{batchDate}.  Note that 
#'   variables \code{HrsToFirstVisitAfter} and \code{HrsToLastVisitAfter} are 
#'   used as helper variables to derive a batch date when the \code{meanEndTime}
#'   variable is \code{NA}.
#'   
#'   Function \code{F.assign.batch.date} collapses mean recapture time, which is
#'   mesured to the nearest minute, to \code{batchDate}, a simple calendar date.
#'   
#'   Fishing instances during which traps utilized half-cones are recorded via 
#'   variable \code{HalfCone}.  During these instances, the number of captured 
#'   fish, variable \code{Recaps}, is multiplied by 2, where the value 2 is
#'   specified via use of the \code{halfConeMulti} variable in function
#'   \code{GlobalVars}.  The expansion by two happens on the raw catch, and not
#'   the mean recapture.  In this way, the number recorded in variable
#'   \code{Recaps} may not be twice the number recorded in variable
#'   \code{oldRecaps}.
#'   
#'   Note that the run season sample period is a vector of length 2 of dates, 
#'   housing the beginning and ending of the run.  These are stored as an 
#'   attribute of the \code{release.df} data frame.
#' 
#' @seealso \code{F.get.release.data}, \code{F.assign.batch.date}
#'   
#' @author Trent McDonald (tmcdonald@west-inc.com)   
#'   
#' @examples
#' \dontrun{
#' #   ---- Estimate the efficiency.  
#' theEff <- F.est.efficiency(release.df,batchDate,method=3,df.spline=3,plot=TRUE,plots.file=NA)
#' }

F.est.efficiency <- function( release.df, batchDate, method=3, df.spline=3, plot=TRUE, plot.file=NA ){
  
  # release.df <- release.df
  # batchDate <- bd
  # method <- 3
  # df.spline <- 3
  # plot.file <- file.root

  #   ---- Check that we actually caught released fish.  If not, cannot do estimate;
  #   ---- denom of passage estimate will be zero.
  if( sum(release.df$Recaps, na.rm=T) <= 0 ){
    warning("NO RELEASE FISH CAUGHT.  Zero efficiency.")
    return(data.frame(no.fish=NA))
  }

  time.zone <- get("time.zone", env=.GlobalEnv)

  #   ---- Fix up the data frame.
  rel.df <- release.df[,c("releaseID","ReleaseDate","nReleased","HrsToFirstVisitAfter","HrsToLastVisitAfter","trapPositionID","meanRecapTime","Recaps",'beg.date','end.date')]
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

  #   ---- Sum by batch dates.  This combines release and catches over trials that occured close in time.
  ind <- list( TrapPositionID=rel.df$trapPositionID,batchDate=rel.df$batchDate.str )
  nReleased <- tapply( rel.df$nReleased,ind, sum )
  nCaught   <- tapply( rel.df$Recaps,ind, sum )

  eff.est <- cbind( expand.grid( TrapPositionID=dimnames(nReleased)[[1]],batchDate=dimnames(nReleased)[[2]]), 
                nReleased=c(nReleased),nCaught=c(nCaught) )
  eff.est$batchDate <- as.character(eff.est$batchDate)

  #   ================== done with data manipulations ===========================
  #   ---- Compute efficiency.
  eff.est$efficiency <- (eff.est$nCaught + 1)/(eff.est$nReleased + 1)
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
  if( any(missing.days) ){
    eff.and.fits <- suppressWarnings(F.efficiency.model( eff, plot=plot, method=method, max.df.spline=df.spline, plot.file=plot.file ))
  } else {
    eff.and.fits <- list(eff=eff, fits=NULL, X=NULL)
    attr(eff.and.fits, "out.fn.list") <- NULL
  }

  eff.and.fits

}
