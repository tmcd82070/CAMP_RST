F.assign.batch.date <- function( df ){
#
#   Assign batch data to records.
#
# df <- catch                      # df <- nvCatch2

cuttime <- get( "samplePeriodCutTime", env=.GlobalEnv )
midtime <- "00:00:00"   # this is the time of day assigned to batchDates.  Could be half way between cut times or (cuttime - 12*60*60).

time.zone <- get( "time.zone", env=.GlobalEnv )


#   A sequence of dates at cuttime every day
min.day <- as.POSIXlt( min(df$EndTime) - 24*60*60, format="%Y-%m-%d %H:%M:%S", tz=time.zone)
max.day <- as.POSIXlt( max(df$EndTime) + 2*24*60*60, format="%Y-%m-%d %H:%M:%S", tz=time.zone)
#min.day <- min(df$EndTime) - 24*60*60
#max.day <- max(df$EndTime) + 2*24*60*60
cut.seq <- seq( min.day, max.day, by=24*60*60 )
cut.day <- format( cut.seq, "%Y-%m-%d" )
cut.seq <- as.POSIXct( paste( cut.day, cuttime ), format="%Y-%m-%d %H:%M:%S", tz=time.zone)


#   Bin the sampleEnd's to cut.seq
ind <- cut( df$EndTime, cut.seq, labels=FALSE )


#   Establish when the cut time "wraps" to the next day.  I.e., at some point, as cut time increases from 0, you
#   stop calling the sample from the night before, and attribute it to the current night.  This time is set in 
#   wrap.POSIX.
cut.POSIX <- as.POSIXct( paste("1970-1-1", cuttime, format="%Y-%m-%d %H:%M:%S" ))
wrap.POSIX <- as.POSIXct("1970-1-1 06:00:00", format="%Y-%m-%d %H:%M:%S" )

 
if( cut.POSIX < wrap.POSIX ){
    add.day <- 0
} else {
    add.day <- 1
}
    
#   Compute batchDate    
bDate <- as.POSIXct( paste( format(cut.seq[ind] + add.day*24*60*60, "%Y-%m-%d"), midtime), format="%Y-%m-%d %H:%M:%S", tz=time.zone)


##   If we allow biologists to over ride batchDate, uncomment the following code
##   Figure out which batchDates are missing and replace just those.
#miss <- is.na(df$batchDate)
#df$batchDate[miss] <- bDate[miss]

#   Add batch date to data frame
if( "SampleDate" %in% names(df) ){
    #   overwrite with batchDate
    df$SampleDate <- bDate
    names(df)[names(df) == "SampleDate" ] <- "batchDate"
} else {
    df$batchDate <- bDate
}

df
}
