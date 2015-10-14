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

#     jday <- as.numeric(format( x, "%j" ))   # for non-leap years, the count doesnt skip a day on 2/29.  we need it to do so for consistency over years.
    
    # ------------------------------------------------
    myYear = as.POSIXlt(x)$year + 1900

    leap <- rep(NA,length(myYear))
    for(i in 1:length(myYear)){
      if(myYear[i] %% 4 != 0){                         # wikipedia article on leap year.
        leap[i] <- 0
      } else if (myYear[i] %% 100 != 0){
        leap[i] <- 1
      } else if (myYear[i] %% 400 != 0){
        leap[i] <- 0
      } else {
        leap[i] <- 1
      }
    }

    tmp.jday <- rep(NA,length(myYear))
    for(i in 1:length(myYear)){
      if(leap[i] == 1){  # leap year -- feb 29th included.
        tmp.jday[i] <- as.numeric(format(x[i], "%j"))
      } else {           # not a leap year -- feb 29th not included. adjust.
        if(as.numeric(format(x[i], "%j")) >= 60 ){            # 60 = feb 29th on leap years. 61 = mar 1st on leap years.
          tmp.jday[i] <- as.numeric(format(x[i], "%j")) + 1     # this pushes march 1st for non-leap to day 61 instead of day 60.                 
        } else {
          tmp.jday[i] <- as.numeric(format(x[i], "%j"))
        }
      }
    }
    # ------------------------------------------------

    wk <- trunc((tmp.jday - 1) / 7) + 1
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
