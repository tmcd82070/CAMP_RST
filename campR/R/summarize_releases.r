#' @export F.summarize.releases
#' 
#' @title F.summarize.releases
#' 
#' @description
#' 
#'    Summarize trap efficiency for every release.  This function
#'    produces a table of information that is used by other routines.
#' 
#'    Input:
#'    release.df = data frame read by F.get.release.data().  Contains
#'        information on releases, marks, etc.  Run season is a vector
#'        of dates for beginning and ending of run, stored as an attribute of the data frame.
#' 
#' 
#' 
#' 
#' @param  release.df  <describe argument>
#' 
#' @details <other comments found in file>
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' \dontrun{
#' <insert examples>
#' 
#' }
F.summarize.releases <- function( release.df ){
#
#   Summarize trap efficiency for every release.  This function
#   produces a table of information that is used by other routines.
#
#   Input:
#   release.df = data frame read by F.get.release.data().  Contains
#       information on releases, marks, etc.  Run season is a vector
#       of dates for beginning and ending of run, stored as an attribute of the data frame.
#



#   ---- Summarize by releaseID.  This summarizes over the traps at a site.  Visits have already been collapsed by the time we get here.
# nRcap    <- tapply( release.df$Recaps, release.df$releaseID, sum, na.rm=T )
# nRelease <- tapply( release.df$nReleased, release.df$releaseID, function(x){ x[1] } )
# trapPos  <- tapply( as.character(release.df$TrapPosition), release.df$releaseID, function(x){ x[1] })
# ReleaseID<- tapply( release.df$releaseID, release.df$releaseID, function(x){ x[1] } )
# Include  <- tapply( as.character(release.df$IncludeTest), release.df$releaseID, function(x){ x[1] } )
# comment  <- gsub("\r\n\r\n", ". ", tapply( as.character(release.df$ReleaseComments), release.df$releaseID, function(x){ x[1] } ))
# dt       <- tapply( release.df$ReleaseDate, release.df$releaseID, function(x){ x[1] })
# mean.vis   <- tapply( release.df$meanRecapTime, release.df$releaseID, function(x){ mean(x, na.rm=T) })
# mean.atLarge   <- tapply( release.df$meanTimeAtLargeHrs, release.df$releaseID, function(x){ mean(x, na.rm=T) })



#   ---- DO NOT Summarize by releaseID.  
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
