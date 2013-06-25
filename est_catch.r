F.est.catch <- function( catch.df, plot=TRUE, plot.file="raw_catch.pdf" ){
#
#   Estimate capture for every day of the season.
#   This assumes that only species and life stages we want are
#   in catch.df.  I.e., that appropriate filtering has already been done.
#
#   Input:
#   catch.df = data frame resulting from call to F.get.catch.data.  Run season is
#       an attribute of this data frame
#
#   Output:
#   A data frame containing $batchDay and $catch
#

#   ---- Establish the days on which estimates need to be produced.
#        Times in run season must be same as time in batchDate each day. 
#        batchDate must occur at same time every day.  Can be missing days, but always at same time when present.
run.season <- attr(catch.df, "run.season")
bd.time <- format(catch.df$batchDate[1], "%H:%M:%S")
start.season <- as.POSIXct( paste(format(run.season$start, "%Y-%m-%d"), bd.time, format="%Y-%m-%d %H:%M:%S" ))
end.season   <- as.POSIXct( paste(format(run.season$end,   "%Y-%m-%d"), bd.time, format="%Y-%m-%d %H:%M:%S" ))




#   ---- Fill in the gaps for individual traps
df <- NULL
u.traps <- unique( catch.df$trapPositionID )
catch.fits <- X.miss <- Gaps <- bDates.miss <- vector("list", length(u.traps))   # lists to contain thing to save for bootstrapping 
names(catch.fits) <- u.traps
#catch.df <<- catch.df   # save a copy for debugging
for( trap in u.traps ){
    cat(paste("==== Catch model for trapPositionID", trap, "========\n" ))

    df2 <- catch.df[catch.df$trapPositionID == trap,]

    #   Impute a value for the gaps
    #   When df2 comes back from F.catch.model, it has extra lines in it.  One extra line for each 24 hour period in the 
    #   gaps that were bigger than max.ok.gap. If gap = 3 days, there will be 3 extra lines.
    #   sampleStart and sampleEnd for each of the new lines are defined so that no gap appears  now.  Variable
    #   'gamEstimated' is true for these periods. Batch date is assigned based on sampleEnd, as usual. 
    #   On return, there is a value or imputed value for each day from start of season to end of season.
    
    df.and.fit <- suppressWarnings( F.catch.model( df2 ) )  # df.and.fit is list of length 2, $df2 contains data frame, $fit contains model 

    df <- rbind( df, df.and.fit$df2)
    catch.fits[[which(trap==u.traps)]] <- df.and.fit$fit
    X.miss[[which(trap==u.traps)]] <- df.and.fit$X.for.missings
    Gaps[[which(trap==u.traps)]] <- df.and.fit$gaps
    bDates.miss[[which(trap==u.traps)]] <- df.and.fit$batchDate.for.missings

#    tmp.df <- df
#    readline()
}

#   Should probably save df into the Access data base for storage and potential use by others. 


#   ---- Now that there are no gaps, sum over all traps operating on a batch day, and all checks that occurred on a batch day.
ind <- list( trapPositionID=df$trapPositionID, batchDate=format(df$batchDate, "%Y-%m-%d") )
est.catch <- tapply( df$n.tot, ind, sum )
p.imputed <- tapply( df$gamEstimated, ind, mean )

est.catch <- cbind( expand.grid( trapPositionID=dimnames(est.catch)[[1]], batchDate=dimnames(est.catch)[[2]]), 
                catch=c(est.catch), imputed.catch=c(p.imputed) )
est.catch$batchDate <- as.POSIXct( as.character(est.catch$batchDate), "%Y-%m-%d", tz=attr(catch.df$batchDate, "tz"))                

#tmp.est.catch <<- est.catch
#readline()

#   If a trap runs without checks for 48 hours say, it runs over two batch dates.  When this happens, 
#   the above 5 statements put in a NA for catch on the day it skipped.  The real (non-imputed) catch 
#   for these days is 0.  Replace these NA's with zeros. 
ind <- is.na(est.catch$catch)
est.catch$catch[ ind ] <- 0
est.catch$imputed.catch[ ind ] <- 0


#   Assign attributes for plotting
attr(est.catch, "site.abbr") <- attr(catch.df, "site.abbr")
attr(est.catch, "run.name") <- attr(catch.df, "run.name")
attr(est.catch, "species.name") <- attr(catch.df, "species.name")

#   Make a plot if called for (I don't use parameter 'plot', apparently.
if( !is.na(plot.file) ) {
    out.fn <- F.plot.catch.model( est.catch, file=plot.file )
} else {
    out.fn <- NULL
}

cat("Catch estimation complete...\n")


ans <- list(catch=est.catch, fits=catch.fits, X.miss=X.miss, gaps=Gaps, bDates.miss=bDates.miss)

attr(ans, "out.fn.list") <- out.fn

ans

}
