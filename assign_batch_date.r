F.assign.batch.date <- function( df ){
#
#   Assign batch data to records where batch date is missing.  If biologists change default batch date,
#   it will appear in df.  Don't change it when it is present.
#

cuttime <- get( "samplePeriodCutTime", env=.GlobalEnv )
midtime <- "00:00:00"   # this is the time of day assigned to batchDates.  Could be half way between cut times or (cuttime - 12*60*60).



#   A sequence of dates at cuttime every day
min.day <- min(df$sampleEnd)-24*60*60
max.day <- max(df$sampleEnd)+ 2*24*60*60
cut.seq <- seq( min.day, max.day, by=24*60*60 )
cut.day <- format( cut.seq, "%Y-%m-%d" )
cut.seq <- as.POSIXct( paste( cut.day, cuttime ), format="%Y-%m-%d %H:%M:%S", tz=attr(df$sampleEnd,"tzone"))

#   Bin the sampleEnd's to cut.seq
ind <- cut( df$sampleEnd, cut.seq, labels=FALSE )


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
bDate <- as.POSIXct( paste( format(cut.seq[ind] + add.day*24*60*60, "%Y-%m-%d"), midtime), format="%Y-%m-%d %H:%M:%S", tz=attr(df$sampleEnd,"tzone"))


##   If we allow biologists to over ride batchDate, uncomment the following code
##   Figure out which batchDates are missing and replace just those.
#miss <- is.na(df$batchDate)
#df$batchDate[miss] <- bDate[miss]

#   Otherwise, just overwrite batchDate
df$batchDate <- bDate

df
}
