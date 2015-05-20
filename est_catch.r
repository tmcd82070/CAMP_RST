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

time.zone <- get("time.zone", env=.GlobalEnv )

#   ---- Establish the days on which estimates need to be produced.
#        Times in run season must be same as time in batchDate each day. 
#        batchDate must occur at same time every day.  Can be missing days, but always at same time when present.
#start.season <- min(catch.df$batchDate)
#end.season   <- max(catch.df$batchDate)




#   ---- Fill in the gaps for individual traps
df <- NULL
true.imp <- NULL
u.traps <- unique( catch.df$trapPositionID )
catch.fits <- X.miss <- Gaps <- bDates.miss <- vector("list", length(u.traps))   # lists to contain thing to save for bootstrapping 
names(catch.fits) <- u.traps
# catch.dff <<- catch.df   # save a copy for debugging


for( trap in u.traps ){
    cat(paste("==== Catch model for trapPositionID", trap, "========\n" ))

    df2 <- catch.df[catch.df$trapPositionID == trap,]

    #   Impute a value for the gaps
    #   When df2 comes back from F.catch.model, it has extra lines in it.  One extra line for each 24 hour period in the 
    #   gaps that were bigger than max.ok.gap. If gap = 3 days, there will be 3 extra lines.
    #   sampleStart and sampleEnd for each of the new lines are defined so that no gap appears  now.  Variable
    #   'gamEstimated' is true for these periods. Batch date is assigned based on sampleEnd, as usual. 
    #   On return, there is a value or imputed value for each day from start of season to end of season.
    
    
    df.and.fit <- suppressWarnings( F.catch.model( df2 ) )  # df.and.fit is list of, $df2 contains data frame, $fit contains model, etc
#     jason.df.and.fit <<- df.and.fit
    df <- rbind( df, df.and.fit$df2)
    
   
    catch.fits[[which(trap==u.traps)]] <- df.and.fit$fit
    X.miss[[which(trap==u.traps)]] <- df.and.fit$X.for.missings
    Gaps[[which(trap==u.traps)]] <- df.and.fit$gaps
    bDates.miss[[which(trap==u.traps)]] <- df.and.fit$batchDate.for.missings

    true.imp <- rbind(true.imp,df.and.fit$true.imp)
#    print(df.and.fit)
#    cat("in est_catch (hit return):")
#    readline()
}
# true.imp <<- true.imp
cat("in est_catch.r  DF")
print( tapply(df$batchDate, df$trapPositionID, range) )
cat("-------\n")


#   Should probably save df into the Access data base for storage and potential use by others. 


#   ---- Now that there are no gaps, sum within traps operating on a batch day, and all checks that occurred on a batch day.
ind <- list( batchDate=format(df$batchDate, "%Y-%m-%d"), trapPositionID=df$trapPositionID  )
est.catch <- tapply( df$n.tot, ind, sum )
p.imputed <- tapply( df$gamEstimated, ind, mean )


tmp.est.catch <- est.catch   # save a copy of est.catch for counting traps later


#   ---- Un-matrix the results and put into a data frame

est.catch <- cbind( expand.grid( batchDate=dimnames(est.catch)[[1]], trapPositionID=dimnames(est.catch)[[2]]), 
                catch=c(est.catch), imputed.catch=c(p.imputed) )
est.catch$batchDate <- as.POSIXct( as.character(est.catch$batchDate), "%Y-%m-%d", tz=time.zone)                

#   ---- Now remove times before and after a trap started and stopped.  The above tapply's using ind inserted NA for days when one trap was not fishing but others where. 
#        There are no gaps between the start and stop of a trap in a season, but each trap could operated over a different part of the season.
#        If you want to leave all the days in, comment the following code out.
trapSeasons <- tapply(est.catch$catch, est.catch$trapPositionID, function(x){ 
            seas <- which(!is.na(x))
            f <- min(seas)
            l <- max(seas)
            ans <- rep(FALSE,length(x))
            ans[f:l] <- TRUE
            ans
        }) 

trapSeasons <- unlist(trapSeasons)

est.catch <- est.catch[ trapSeasons, ] 


#   ---- Before we un-matrix-ized est.catch, we saved a copy so we could compute number of traps operating each day because it is easier.
#        But, gaps are filled.  Take back out the gaps when counting.
trapSeasons <- matrix( trapSeasons, nrow=nrow(tmp.est.catch), ncol=ncol(tmp.est.catch) )
for( i in 1:length(bDates.miss) ){
    tmp.est.catch[ trapSeasons[,i] & is.na(tmp.est.catch[,i]), i ] <- 0  # Add days when trap was operating but no trap visit.  I.e., trap operated for >24 hours without a check.  Must do this first, before next line.
    tmp.est.catch[dimnames(tmp.est.catch)[[1]] %in% format(bDates.miss[[i]]), i] <- NA   #  blank out the gaps.
}
trapsOperating <- apply(tmp.est.catch, 1, function(x){sum(!is.na(x))} )
trapsOperating <- data.frame( batchDate=names(trapsOperating), nTrapsOperating=trapsOperating, stringsAsFactors=F )
trapsOperating$batchDate <- as.POSIXct( as.character(trapsOperating$batchDate), "%Y-%m-%d", tz=time.zone)                

#tmp.est.catch <<- tmp.est.catch
#tmp.trapsOperating <<- trapsOperating

#readline()

#   If a trap runs without checks for 48 hours say, it runs over two batch dates.  When this happens, 
#   the above statements result in a NA for catch on the day it skipped.  The real (non-imputed) catch 
#   for these days is 0.  Replace these NA's with zeros.  (But, perhaps these lines should be tossed...)
ind <- is.na(est.catch$catch)
est.catch$catch[ ind ] <- 0
est.catch$imputed.catch[ ind ] <- 0


#   Assign attributes for plotting
u.ss.rows <- !duplicated(catch.df$trapPositionID)
u.ss <- catch.df$trapPositionID[u.ss.rows]
u.ss.name <- catch.df$TrapPosition[u.ss.rows]
ord <- order( u.ss )
u.ss <- u.ss[ ord ]
u.ss.name <- u.ss.name[ ord ]

#   Make a plot if called for (I don't use parameter 'plot', apparently.
# jason.est.catch <<- est.catch


# jason add:  merge in TrapPosition so that graphical output can show text labels instead of computer code numbers.
# est.catch <- merge(jason.est.catch,unique(catch.dff[,c('trapPositionID','TrapPosition')]),by=c('trapPositionID'),all.x=TRUE)  delete later
est.catch <- merge(est.catch,unique(catch.df[,c('trapPositionID','TrapPosition')]),by=c('trapPositionID'),all.x=TRUE)
attr(est.catch, "site.name") <- catch.df$siteName[1]
attr(est.catch, "subsites") <- data.frame(subSiteID = u.ss, subSiteName=u.ss.name)
attr(est.catch, "run.name") <- run.name#catch.df$FinalRun[1]
attr(est.catch, "life.stage" ) <- catch.df$lifeStage[1]
attr(est.catch, "species.name") <- "Chinook Salmon"

if( !is.na(plot.file) ) {
    out.fn <- F.plot.catch.model( est.catch, file=plot.file )
} else {
    out.fn <- NULL
}

cat("Catch estimation complete...\n")




ans <- list(catch=est.catch, fits=catch.fits, X.miss=X.miss, gaps=Gaps, bDates.miss=bDates.miss, trapsOperating=trapsOperating, true.imp=true.imp)

attr(ans, "out.fn.list") <- out.fn

ans

}
