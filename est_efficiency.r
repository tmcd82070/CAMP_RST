F.est.efficiency <- function( release.df, batchDate, method=1, df.spline=4, plot=TRUE, plot.file=NA ){
#
#   Estimate trap efficiency for every sample period. 
#
#   Input:
#   release.df = data frame produced by F.get.release.data().  Contains
#       information on releases, and recaptures. This data frame has one line per release trial 
#       per trap (trapPositionID).
#
#   Note: Run season is a vector length 2 of dates for beginning and 
#   ending of run, stored as an attribut of the data frame.
#
#   Output:
#   A data frame containing interval and capture efficiency and
#   $gam.estimated.  $gam.estimated is 'Yes' if efficiency for that interval was estimated
#   by the GAM model, rather than being empirical.
#


#   ---- Check that we actually caught released fish.  If not, 0 efficiency, cannot do estimate.
if( sum(release.df$n, na.rm=T) <= 0 ){
    warning("NO RELEASE FISH CAUGHT.  Zero efficiency.")
    return(data.frame(no.fish=NA))
}

#   ---- Assign batch date to efficiency trials

#print(release.df[1:10,])

rel.df <- release.df[,c("releaseID", "releaseTime", "nReleased", "testDays",    "trapPositionID", "n", "meanRecapTime")]
rel.df$batchDate <- rep(NA, nrow(rel.df))
names(rel.df)[ names(rel.df) == "meanRecapTime" ] <- "sampleEnd"


#        Replace any missing meanVisitTimes with 1/2 the testDays.  Mean visit time is missing if they did not catch anything from a release. 
ind <- is.na( rel.df$sampleEnd )
rel.df$sampleEnd[ind] <- rel.df$releaseTime[ind] + (rel.df$testDays[ind] * 24 * 60 * 60) / 2


#       Assign batch date
rel.df <- F.assign.batch.date( rel.df )

rel.df$batchDate.str <- format(rel.df$batchDate, "%Y-%m-%d")

#print("hello")
#print(rel.df)

#       Sum by batch dates.  This combines release and catches over trials that occured close together in time
ind <- list( trapPositionID=rel.df$trapPositionID, batchDate=rel.df$batchDate.str )
nReleased <- tapply( rel.df$nReleased, ind, sum )
nCaught   <- tapply( rel.df$n, ind, sum )

eff.est <- cbind( expand.grid( trapPositionID=dimnames(nReleased)[[1]], batchDate=dimnames(nReleased)[[2]]), 
                nReleased=c(nReleased), nCaught=c(nCaught) )
eff.est$batchDate <- as.character(eff.est$batchDate)


#       Compute efficiency
eff.est$efficiency <- (eff.est$nCaught+1)/(eff.est$nReleased+1)

#cat("All observed efficiency trial data...\n")
#print(eff.est)

eff.est <- eff.est[ !is.na(eff.est$efficiency), ]


#   ---- Figure out which days have efficiency data.
bd <- expand.grid(trapPositionID=sort(unique(eff.est$trapPositionID)), batchDate=format(batchDate, "%Y-%m-%d"), stringsAsFactors=F)


eff <- merge( eff.est, bd, by=c("trapPositionID", "batchDate"), all.y=T)
#eff <- eff[ order( eff$batchDate), ]
#print(eff)
eff$batchDate <- as.POSIXct( eff$batchDate, format="%Y-%m-%d", tz="America/Los_Angeles" )

#   Assign attributes for plotting
attr(eff, "site.abbr") <- attr(release.df, "site.abbr")
attr(eff, "run.name") <- attr(release.df, "run.name")
attr(eff, "species.name") <- attr(release.df, "species.name")


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
