#' @export F.est.efficiency
#' 
#' @title F.est.efficiency
#' 
#' @description
#' 
#'    Estimate trap efficiency for every sample period. 
#' 
#'    Input:
#'    release.df = data frame produced by F.get.release.data().  Contains
#'        information on releases, and recaptures. This data frame has one line per release trial 
#'        per trap (TrapPositionID).
#' 
#'    Note: Run season is a vector length 2 of dates for beginning and 
#'    ending of run, stored as an attribut of the data frame.
#' 
#'    Output:
#'    A data frame containing interval and capture efficiency and
#'    $gam.estimated.  $gam.estimated is 'Yes' if efficiency for that interval was estimated
#'    by the GAM model, rather than being empirical.
#' 
#'     batchDate <- bd
#'       method <- 3
#'       df.spline <- 3
#'       plot.file <- file.root
#' 
#' 
#'    ---- Check that we actually caught released fish.  If not, 0 efficiency, cannot do estimate.
#' 
#' @param  release.df <describe argument>
#' @param  batchDate <describe argument>
#' @param  method=1 <describe argument>
#' @param  df.spline=4 <describe argument>
#' @param  plot=TRUE <describe argument>
#' @param  plot.file=NA  <describe argument>
#' 
#' @details <other comments found in file>
#'    Replace any NA meanEndTimes with releaseTime plus mean of HrsToFirstVisitAfter and HrsToLastVisitAfter . This will assign a batch date.  
#'    ---- Assign batch date to efficiency trials based on meanEndTime (really weighted meanVisitTime)
#'        Sum by batch dates.  This combines release and catches over trials that occured close together in time
#'    ================== done with data manipulations ===========================
#'        Compute efficiency
#' cat("All observed efficiency trial data...\n")
#' print(eff.est)
#'    ---- Figure out which days have efficiency data.
#'    Assign attributes for plotting
#' cat("((((((((((((In est_efficiency.r))))))))))))\n")
#' print(eff)
#' print(bd)
#' readline()
#'    ---- If there are missing days, imput them
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' <insert examples>
#' 
F.est.efficiency <- function( release.df, batchDate, method=1, df.spline=4, plot=TRUE, plot.file=NA ){
#
#   Estimate trap efficiency for every sample period. 
#
#   Input:
#   release.df = data frame produced by F.get.release.data().  Contains
#       information on releases, and recaptures. This data frame has one line per release trial 
#       per trap (TrapPositionID).
#
#   Note: Run season is a vector length 2 of dates for beginning and 
#   ending of run, stored as an attribut of the data frame.
#
#   Output:
#   A data frame containing interval and capture efficiency and
#   $gam.estimated.  $gam.estimated is 'Yes' if efficiency for that interval was estimated
#   by the GAM model, rather than being empirical.
#
#    batchDate <- bd
#      method <- 3
#      df.spline <- 3
#      plot.file <- file.root


#   ---- Check that we actually caught released fish.  If not, 0 efficiency, cannot do estimate.
if( sum(release.df$Recaps, na.rm=T) <= 0 ){
    warning("NO RELEASE FISH CAUGHT.  Zero efficiency.")
    return(data.frame(no.fish=NA))
}

time.zone <- get("time.zone", env=.GlobalEnv)

#   ---- Fix up the data frame
rel.df <- release.df[,c("releaseID", "ReleaseDate", "nReleased",  "HrsToFirstVisitAfter", "HrsToLastVisitAfter", "trapPositionID", "meanRecapTime", "Recaps",'beg.date','end.date')]
rel.df$batchDate <- rep(NA, nrow(rel.df))
names(rel.df)[ names(rel.df) == "meanRecapTime" ] <- "EndTime"


#   ---- meanRecapTime is NA if they did not catch anything from a release. 
#   Replace any NA meanEndTimes with releaseTime plus mean of HrsToFirstVisitAfter and HrsToLastVisitAfter . This will assign a batch date.  
ind <- is.na( rel.df$EndTime )
rel.df$EndTime[ind] <- rel.df$ReleaseDate[ind] +  (rel.df$HrsToFirstVisitAfter[ind] + rel.df$HrsToLastVisitAfter[ind]) / 2 



#   ---- Assign batch date to efficiency trials based on meanEndTime (really weighted meanVisitTime)
rel.df <- F.assign.batch.date( rel.df )


rel.df$batchDate.str <- format(rel.df$batchDate, "%Y-%m-%d")



#       Sum by batch dates.  This combines release and catches over trials that occured close together in time
ind <- list( TrapPositionID=rel.df$trapPositionID, batchDate=rel.df$batchDate.str )
nReleased <- tapply( rel.df$nReleased, ind, sum )
nCaught   <- tapply( rel.df$Recaps, ind, sum )

eff.est <- cbind( expand.grid( TrapPositionID=dimnames(nReleased)[[1]], batchDate=dimnames(nReleased)[[2]]), 
                nReleased=c(nReleased), nCaught=c(nCaught) )
eff.est$batchDate <- as.character(eff.est$batchDate)

#   ================== done with data manipulations ===========================

#       Compute efficiency
eff.est$efficiency <- (eff.est$nCaught+1)/(eff.est$nReleased+1)

#cat("All observed efficiency trial data...\n")
#print(eff.est)

eff.est <- eff.est[ !is.na(eff.est$efficiency), ]


#   ---- Figure out which days have efficiency data.
bd <- expand.grid(TrapPositionID=sort(unique(eff.est$TrapPositionID)), batchDate=format(batchDate, "%Y-%m-%d"), stringsAsFactors=F)


eff <- merge( eff.est, bd, by=c("TrapPositionID", "batchDate"), all.y=T)
eff$batchDate <- as.POSIXct( eff$batchDate, format="%Y-%m-%d", tz=time.zone )

#   Assign attributes for plotting
ind <- !duplicated(release.df$TrapPosition)
attr(eff,"subsites") <- data.frame(subSiteName=as.character(release.df$TrapPosition[ind]), subSiteID=release.df$trapPositionID[ind], stringsAsFactors=F)
attr(eff, "site.name") <- release.df$siteName[1]

#cat("((((((((((((In est_efficiency.r))))))))))))\n")
#print(eff)
#print(bd)
#readline()



#   ---- If there are missing days, imput them
missing.days <- is.na(eff$efficiency)
if( any(missing.days) ){
    eff.and.fits <- suppressWarnings(F.efficiency.model( eff, plot=plot, method=method, max.df.spline=df.spline, plot.file=plot.file ))
} else {
    eff.and.fits <- list(eff=eff, fits=NULL, X=NULL)
    attr(eff.and.fits, "out.fn.list") <- NULL
}


eff.and.fits

}
