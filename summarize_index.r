F.summarize.index <- function( dt, summarize.by ){
#
#   return a list suitable for calling tapply which summarizes a vector by 'summarize.by'.
#
#   dt = a POSIXct date vector
#   summarize.by = a string equal to "week", "month", "year", or "day"
#


#   ---- A utility function to deal with week 0
f.week <- function( x ){
    #   x = vector of POSIXct's
    # Week 0 contains Jan 1.  Week 1 is first full week of the year.  Make week 0 part of last week of previous year
    wk <- format( x, "%W" )
    yr <- format( x, "%Y" )
    yr[ wk == "00" ] <- yr[ wk == max(as.numeric(wk)) ][1]  # previous year for week zeros.  Can't have more than 3 years in data.  
                                                            # This is true because we restrict max.date-min.date<365
    wk[ wk == "00" ] <- max( as.numeric(wk) )   # make week 0 part of last week of year previous
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
