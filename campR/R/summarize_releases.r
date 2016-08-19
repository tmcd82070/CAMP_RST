#' @export F.summarize.releases
#'   
#' @title F.summarize.releases
#'   
#' @description Summarize trap efficiency for every release.
#'   
#' @param  release.df A data frame read by function \code{F.get.release.data}
#'   containing information on releases.  Run season is a vector of
#'   dates for the start and end of a run, and is stored as an attribute of the
#'   data frame.
#'   
#' @details The multiplicative expansion of caught fish due to half-cone 
#'   operations utilizes the value of the global variable \code{halfConeMulti}, 
#'   set in function \code{GlobalVars}. This value is currently set to \code{2}.
#'   
#' @return A data frame containing a summary of unique combinations of traps
#'   (via \code{trapPosition}) and releases (via \code{releaseID}).
#'   
#' @author WEST Inc.
#'   
#' @seealso \code{F.get.release.data}
#'   
#' @examples
#' \dontrun{
#' #   ---- Summarize releases for provided data frame release.df. 
#' df <- F.summarize.releases(release.df)
#' }
F.summarize.releases <- function( release.df ){

  # release.df <- release.df
  
  #   ---- Obtain halfcone multiplier from the global environment. 
  halfConeMulti <- get("halfConeMulti",envir=.GlobalEnv)
  
  #   ---- Summarize by releaseID.  This summarizes over the traps at a site.  Visits have 
  #   ---- already been collapsed by the time we get here.

  #   ---- Do NOT summarize by releaseID.  
  nRcap    <- release.df$Recaps
  nRelease <- release.df$nReleased
  trapPos  <- as.character(release.df$TrapPosition)
  ReleaseID<- release.df$releaseID
  Include  <- as.character(release.df$IncludeTest)
  comment  <- gsub("\r\n\r\n", ". ", as.character(release.df$ReleaseComments))
  dt       <- release.df$ReleaseDate
  mean.vis   <- release.df$meanRecapTime
  mean.atLarge   <- release.df$meanTimeAtLargeHrs
  nFishRecaps <- ifelse(release.df$HalfCone == 'Yes',release.df$Recaps / halfConeMulti,release.df$Recaps)
  nHalfConeRecaps <- release.df$Recaps - nFishRecaps
  HalfCone <- release.df$HalfCone
  
  class( dt ) <- class( release.df$meanRecapTime )
  class( mean.vis ) <- class( release.df$meanRecapTime )
  
  ans <- data.frame(TrapPosition=trapPos, releaseTime=dt, ReleaseID=ReleaseID,  nReleased=nRelease, HalfCone=HalfCone,
      nFishRecaps=nFishRecaps,nHalfConeRecaps=nHalfConeRecaps,nRecaps=nRcap, meanRecapTime=mean.vis, meanTimeAtLargeHrs = mean.atLarge,
      IncludeTest=Include, ReleaseComment=comment )
  ans <- ans[ order(ans$releaseTime), ]
  
  ans
}
