F.summarize.index <- function( dt, summarize.by ){
#
#   return a list suitable for calling tapply which summarizes a vector by 'summarize.by'.
#
#   dt = a POSIXct date vector
#   summarize.by = a string equal to "week", "month", "year", or "day"
#

        

f.week <- function( x ){
    #   A utility to compute julian weeks 
    #   x = vector of POSIXct's

    yr <- format( x, "%Y" )

    jday <- as.numeric(format( x, "%j" ))
    wk <- trunc((jday - 1) / 7) + 1
    wk <- formatC(wk, width=2, flag="0")  # to make sure sort order is correct
    
    wk <- paste(yr, "-", wk, sep="")
    wk
}

#   ---- Construct index
if( summarize.by == "week" ){
    index <- list(s.by=f.week( dt ) )
} else if( summarize.by == "month" ){
    index <- list(s.by= format( dt, "%Y-%m" ))
} else if( summarize.by == "year" ){
    #   Because we checked, and min.date and max.date are less than 365 days appart, for annual numbers
    #   we just sum everything.  This is necessary because most winter runs occur in two calender years.
    year.of.mean.date <- mean( dt, na.rm=T )
    tzn <- attr( dt, "tzone" )
    tz.offset <- as.numeric(as.POSIXct(0, origin="1970-01-01", tz=tzn))
    year.of.mean.date <- as.POSIXct( year.of.mean.date-tz.offset, origin="1970-01-01", tz=tzn )  # I think this only works west of GMT (North America).
    year.of.mean.date <- format(year.of.mean.date, "%Y")
    index <- list(s.by= rep( year.of.mean.date, length(dt) ))
} else { # summarize by day.  
    index <- list(s.by= format( dt, "%Y-%m-%d" ))
}


index

}
