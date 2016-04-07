#' @title Average passage over all traps at a site, then sum by summarize.by.
#'   
#' @param df The data frame containing days which are to be summarized via  
#'   the temporal unit specified by \code{summarize.by}.
#' @param summarize.by The time unit into which summary occurs, i.e., one of 
#' \code{day},\code{week},\code{month},\code{year}.
#' 
#' @return The dataframe fed to function \code{F.summarize.passage} must have a 
#'   column \code{batchDate}, including the day fished, and a column 
#'   \code{imputed.catch}, containing the percentage of the total count of the
#'   Total Estimated Catch obtained via imputation.  Additionally, the 
#'   dataframe must contain estimated passage.  See Details.  
#'   
#'   Function \code{F.summarize.passage} first averages the passage on a per-day basis 
#'   over all traps running on that day.  It then summarizes via the temporal units 
#'   specified by \code{summarize.by}. See Examples.
#'   
#'   By default, passage estimation involving lifestage always takes place over 
#'   a year.
#'   
#' @section: Details:  The Total Estimated Catch is the sum of Assigned,
#' Unassigned, Half-Cone Adjusted, and Imputed Fish.  It is also the numerator
#' utilized in the estimation of passage, where the denominator is the estimated
#' efficiency.

#' @seealso
#' 
#' @aliases
#' 
#' @examples
#' 
#' # Fetch Julian week labels from a local CAMP.mdb.
#' # These are necessary for summaries by week.  
#' db.file <- "C:/CAMP Folder/CAMP.mdb"                       
#' db <- get( "db.file", env=.GlobalEnv )
#' ch <- odbcConnectAccess(db)
#' the.dates <- sqlFetch( ch, "Dates" )
#' close(ch) 
#'  
#' # Create a data set of trapping.  
#' beg <- strptime("2014-01-24",format="%F")
#' df <- data.frame(trapVisitID=c(seq(1234,length.out=10)),
#'                  trapPositionID=c(rep(10001,10),rep(10002,10)),
#'                  batchDate=rep(seq(beg,by=60*60*24,length.out=10),2),
#'                  imputed.catch=c(0,0,0,0.1,0,0,1.0,1.0,0,0,0,0,0,0.1,0,0,0,0,0,1),
#'                  passage=c(11,11,14,16,12,10,23,18,17,18,3,5,2,7,5,4,1,3,1,1))
#' df <- df[order(df$batchDate,df$trapPositionID),]
#' 
#' # Summarize by different time frames. 
#' df.day <- F.summarize.passage( df, "day" )
#' df.week <- F.summarize.passage( df, "week" )
#' df.month <- F.summarize.passage( df, "month" )
#' df.year <- F.summarize.passage( df, "year" )
#' 
#' @export






F.summarize.index <- function( dt, summarize.by ){
#
#   return a list suitable for calling tapply which summarizes a vector by 'summarize.by'.
#
#   dt = a POSIXct date vector
#   summarize.by = a string equal to "week", "month", "year", or "day"
#
# dt <- catch.df$batchDate


  f.week <- function( x ){
    the.dates$week <- paste0(the.dates$year,'-',formatC(the.dates$julianWeek, width=2, flag="0"))
    the.dates <- unique(the.dates)
    the.dates$uniqueDate <- as.Date(the.dates$uniqueDate)
  
    dtDF <- data.frame(uniqueDate=as.Date(dt))
    dtDF$R_ID <- seq(1,nrow(dtDF),1)
    test <- merge(dtDF,the.dates,by=c('uniqueDate'),all.x=TRUE)
    test <- test[order(test$R_ID),]
    ans <- test$week
    ans
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
