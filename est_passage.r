F.est.passage <- function( catch.df, release.df, summarize.by, file.root, ci ){
#
#   Compute passage estimates.
#
#   Input:
#   catch.df = data frame resulting from call to F.get.catch.data.
#   release.df = data frame resulting from call to F.get.release.data.  Contains info on efficiency.
#   summarize.by = string specifying how to sum passage estimates.  valid values
#       are "day", "week", "month", "year".
#   file.root = root of file name for graphics files
#  
#
#   Output,
#   A data frame containing date, passage estimate, and SE of passage estimate.
#

f.banner <- function( x ){

    cat("\n")
    cat(paste(rep("=",50), collapse="")); 
    cat(x); 
    cat(paste(rep("=",50), collapse="")); 
    cat("\n")
}

f.banner(" F.est.passage - START ")

#   This keeps track of the files produced
out.fn.list <- NULL

#   retrieve the progress bar
usepb <- exists( "progbar", where=.GlobalEnv )

#   ------------------------------------------------------------------
#   Estimate capture for every day of season.  Return value is
#   data frame with columns $batchDate and $catch.  
#   By default, this produces one graph in a pdf.  Turn this off with plot=F in call.
catch.and.fits <- F.est.catch( catch.df, plot=TRUE, plot.file=file.root )
if(usepb){
    tmp <- getWinProgressBar(progbar)
    setWinProgressBar(progbar, (2*tmp + 1)/3 )
}
catch <- catch.and.fits$catch

out.fn.list <- c(out.fn.list, attr(catch.and.fits, "out.fn.list"))

#catch.fits <- catch.and.fits$fits  # fits are needed for variance computation
#print(catch[1:20,])

#   ------------------------------------------------------------------
#   Estimate trap efficiency for every batchDate of season.  Return value is
#   data frame with columns $batchDate and $eff.  
#   If plot=T, this produces a graph in a pdf.
f.banner(" Efficiency estimation ")
bd <- sort( unique(catch$batchDate) )
eff.and.fits <- F.est.efficiency( release.df, bd, method=3, df=3, plot=TRUE, plot.file=file.root )
if(usepb){
    tmp <- getWinProgressBar(progbar)
    setWinProgressBar(progbar, (2*tmp + 1)/3 )
}
efficiency <- eff.and.fits$eff

out.fn.list <- c(out.fn.list, attr(eff.and.fits, "out.fn.list"))


if( all(is.na(efficiency[1,])) ){
    #   Something is wrong with efficiency data. Make an empty efficiency data frame
    efficiency <- data.frame( trapPositionID=catch$trapPositionID, batchDate=catch$batchDate, efficiency=rep(NA, nrow(catch)))
    warning("Zero efficiency")
}
    

#   could do this n <- data.base( catch, efficiency=efficiency$efficiency, gam.estimated.eff=efficiency$gam.estimated ) 
#   to produce a data frame of values that go into estimator, one line per batchDate

#   ------------------------------------------------------------------
#   Now, estimate passage
if( any(ind <- !is.na(efficiency$efficiency) & (efficiency$efficiency <= 0)) ){    # shouldn't happen that efficiency <= 0, but just in case.  This also gives us a way to exclude days (just set efficiency <= 0)
    efficiency$efficiency[ind] <- NA
}

#   First merge catch and efficiency data frames

catch$batchDay <- format(catch$batchDate, "%Y-%m-%d")
catch$trapPositionID <- as.character(catch$trapPositionID)
efficiency$batchDay <- format(efficiency$batchDate, "%Y-%m-%d")
efficiency$trapPositionID <- as.character(efficiency$trapPositionID)
efficiency <- efficiency[,names(efficiency) != "batchDate"]  # drop POSIX date from efficiency

cat("First 20 rows of CATCH...\n")
print(catch[1:20,])
cat("First 20 rows of EFFICIENCY...\n")
print(efficiency[1:20,])

grand.df <- merge( catch, efficiency, by=c("trapPositionID", "batchDay"), all=T)


#   The passage estimator
grand.df$passage <- rep(NA, nrow(grand.df))
grand.df$passage <- grand.df$catch / grand.df$efficiency
grand.df$passage <- round(grand.df$passage,1)   # round final passage estimate here so different summaries sum to the same number.


#   Save grand.df to .GlobalEnv (for debuggin) and write it out to a csv file
grand.df <<- grand.df
cat("grand.df stored in .GlobalEnv")

if( !is.na(file.root) ){
    out.fn <- paste(file.root, "_baseTable.csv", sep="")
    write.table( grand.df, file=out.fn, sep=",", row.names=FALSE, col.names=TRUE)
    out.fn.list <- c(out.fn.list, out.fn)
}

# ====== Passage estimates are done by day.  Compute variance and summarize ====================================================================================================
f.banner(paste(" Bootstrapping, if called for, and summarizing by", summarize.by))

#   Because the summarization (to weeks, years, etc.) needs to go on in the bootstrapping routine, 
#   it is easier to do it all there. 
#   Even if bootstraps are not called for, F.bootstrap averages over traps (if multiple present) and 
#   summarizes by 'summarize.by'.

n <- F.bootstrap.passage( grand.df, catch.and.fits$fits, catch.and.fits$X.miss, catch.and.fits$gaps,
                catch.and.fits$bDates.miss, eff.and.fits$fits, eff.and.fits$X, eff.and.fits$ind.inside, summarize.by, 100, ci )
if(usepb){
    tmp <- getWinProgressBar(progbar)
    setWinProgressBar(progbar, tmp + (1-tmp)*.9 )
}

#   Do I need the following?
#if( summarize.by == "day" ){
#    #   Add in some extra columns that don't apply otherwise.
#    n$catch <- c(tapply( grand.df$catch, index, sum, na.rm=T ))
#    n$pct.imputed.eff <- c(tapply(as.numeric(grand.df$imputed.eff), index, mean, na.rm=T ))
#    n$efficiency <- c(tapply(grand.df$efficiency, index, mean, na.rm=T ))
#}




#   ---- Summarize auxillary information about catch

index.aux <- F.summarize.index( catch.df$batchDate, summarize.by )

#   Mean Forklength
num <- catch.df$mean.fl * catch.df$n.tot
num <- tapply( num, index.aux, sum, na.rm=T )


#   SD of Forklength
num.sd <- (catch.df$sd.fl * catch.df$sd.fl) * (catch.df$n.tot - 1)    # this is sum of squares
num.sd <- tapply( num.sd, index.aux, sum, na.rm=T )


#   n
den <- tapply( catch.df$n.tot, index.aux, sum, na.rm=T)

#   Mean and SD computations
aux.fl <- ifelse( den > 0, num / den, NA )
aux.sd <- ifelse( den > 1, sqrt(num.sd / (den-1)), NA )


#   Amount of time sampled
num <- as.numeric( catch.df$sampleLengthHrs )
aux.hrs <- tapply( num, index.aux, sum, na.rm=T )   # this is hrs actually sampled during the 'index' period

#den <- rep( 24, length(batchDate.filled) )
#den <- tapply( den, index.aux2, sum, na.rm=T )  # this is total hours in 'index' period
#
#   Note: I will leave the commented out code that computes amount of time in each index period.  The reason 
#   I commented it out is that 'den' may have more rows than num.  i.e., catch.df$batchDate may have fewer rows than batchDate.filled.
#   This makes 'den' difficult to merge back in to 'num', but it could be done.

     
aux<-data.frame( s.by=dimnames(aux.fl)[[1]], 
    nForkLenMM=c(den),
    meanForkLenMM=c(aux.fl), 
    sdForkLenMM=c(aux.sd),
    sampleLengthHrs=c(aux.hrs),
    stringsAsFactors=F, row.names=NULL )


#   ---- Merge 'n' and 'aux' information together    
n <- merge(n,aux, by="s.by", all.x=T)   


n$sampleLengthDays <- n$sampleLengthHrs / 24

tzn <- attr( grand.df$batchDate, "tz" )
tz.offset <- as.numeric(as.POSIXct(0, origin="1970-01-01", tz=tzn))
n$date <- as.POSIXct( n$date-tz.offset, origin="1970-01-01", tz=tzn )  # I think this only works west of GMT (North America).  East of GMT, it may be 12 hours off. UNTESTED east of GMT





#   Put the final data frame together
names(n)[names(n) == "s.by"] <- summarize.by

attr(n, "taxonID" ) <- attr(catch.df,"taxonID")
attr(n, "species.name") <- attr(catch.df, "species.name")
attr(n, "siteID" ) <- attr(catch.df,"siteID")
attr(n, "site.name") <- attr(catch.df, "site.name")
attr(n, "site.abbr") <- attr(catch.df, "site.abbr")  
attr(n, "runID") <- attr(catch.df, "runID")
attr(n, "run.name") <- attr(catch.df, "run.name")
attr(n, "year") <- attr(catch.df, "year")
attr(n, "run.season") <- attr(catch.df, "run.season")
attr(n, "summarized.by") <- summarize.by
attr(n, "out.fn.list") <- out.fn.list

f.banner(" F.est.passage - COMPLETE ")

n

}
