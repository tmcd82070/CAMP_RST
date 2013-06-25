F.assign.sample.period <- function( df ){
#
#   Assign the start and stop of sample periods.
#


tmp.df <<- df

#   First, sort
df <- df[order(df$trapPositionID, df$visitTime),]

period.strt <- NULL
period.end <- NULL
u.traps <- unique( df$trapPositionID )
for( trap in u.traps ){
    n <- sum(df$trapPositionID == trap)
    df2 <- df[df$trapPositionID == trap,]
    
    samp.per.strt <- rep(NA, n)
    samp.per.end <- rep(NA, n)
    
    #   Toss out records until the first time they processed fish at this trap
    frst <- 0
    for( i in 1:n ){
        frst <- i
        if( df2$visitTypeID[i] == 1 | df2$fishProcessedID[i] == 1 ) break
    }
    if( frst >= n ) next   # no processing ever.
    
    #   Process visits in time order
    prevstart <- df2$visitTime2[frst] 
    for( i in frst:n ){
        if( df2$visitTypeID[i] == 1){
            samp.per.strt[i] <- NA
            samp.per.end[i] <- NA
            prevstart <- df2$visitTime2[i]
        } else if(df2$visitTypeID[i] == 2 | df2$visitTypeID[i] == 3){
            samp.per.strt[i] <- prevstart
            samp.per.end[i] <- df2$visitTime[i]
            prevstart <- df2$visitTime2[i]
        } else if(df2$visitTypeID[i] == 4){
            samp.per.strt[i] <- prevstart
            samp.per.end[i] <- df2$visitTime[i]
            prevstart <- NA 
        } 
    }
    
    period.strt <- c(period.strt, samp.per.strt)
    period.end <- c(period.end, samp.per.end)
    
}


df$sampleStart <- period.strt
df$sampleEnd <- period.end

class(df$sampleStart) <- c("POSIXct", "POSIXt")
class(df$sampleEnd) <- c("POSIXct", "POSIXt")

attr(df$sampleStart, "tzone") <- "America/Los_Angeles"
attr(df$sampleEnd, "tzone") <- "America/Los_Angeles"

df$sampleLengthHrs <- difftime( df$sampleEnd, df$sampleStart, units="hours" )

df

}
        
