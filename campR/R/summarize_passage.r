#' @export
#' 
#' @title F.summarize.passage
#' 
#' @description Average passage over all traps at a site, then sum by the 
#' temporal unit specified by variable \code{summarize.by}.
#'   
#' @param df The data frame containing days which are to be summarized via the 
#'   temporal unit specified by \code{summarize.by}.
#' @param summarize.by A string time unit into which summary occurs, i.e., one 
#'   of "\code{day}", "\code{week}", "\code{month}", or "\code{year}".
#'   
#' @return A data frame containing summarized passage, per the time unit 
#'   specified via \code{summarize.by}.
#'   
#' @details The dataframe fed to function \code{F.summarize.passage} must have a
#'   day-fished POSIX column \code{batchDate}, formatted via the ISO 8601 date
#'   format (\code{\%F}), and a column \code{imputed.catch}, containing the
#'   percentage of the total count of the Total Estimated Catch obtained via
#'   imputation.  Note that this date format requires a four-digit year.  
#'   Additionally, the dataframe must contain estimated passage 
#'   via variable \code{passage}.
#'   
#'   Function \code{F.summarize.passage} first averages the passage on a per-day
#'   basis over all traps running on that day.  It then summarizes over those
#'   days via the temporal units specified by \code{summarize.by}. See Examples.
#'   
#'   By default, passage estimation involving life stage always takes place over 
#'   a year.
#'   
#'   The Total Estimated Catch is the sum of Assigned, Unassigned, Half-Cone 
#'   Adjusted, and Imputed Fish.  It is also the numerator utilized in the 
#'   estimation of passage, where the denominator is the estimated efficiency.
#'   
#' @examples
#' \dontrun{
#' #   ---- Create a data set of trapping.
#' beg <- strptime("2014-01-24",format="%F")
#' df <- data.frame(trapVisitID=c(seq(1234,length.out=10)),
#'                  trapPositionID=c(rep(12345,10),rep(98765,10)),
#'                  batchDate=rep(seq(beg,by=60*60*24,length.out=10),2),
#'                  imputed.catch=c(0,0,0,0.1,0,0,1.0,1.0,0,0,0,0,0,0.1,0,0,0,0,0,1),
#'                  passage=c(11,11,14,16,12,10,23,18,17,18,3,5,2,7,5,4,1,3,1,1))
#' df <- df[order(df$batchDate,df$trapPositionID),]
#' 
#' #   ---- Summarize by different time frames.
#' df.day <- F.summarize.passage( df, "day" )
#' df.month <- F.summarize.passage( df, "month" )
#' df.year <- F.summarize.passage( df, "year" )
#' 
#'   ---- Need Julian weeks to compute this particular example.  
#' df.week <- F.summarize.passage( df, "week" )
#' 
#' }
F.summarize.passage <- function( df, summarize.by ){
  
  # df <- grand.df
  # summarize.by <- sum.by
  
  #   ----- First, average over traps -----
  index <- list(batchDate=format( df$batchDate, "%Y-%m-%d" ))

  n <- c(tapply( df$passage, index, FUN=mean, na.rm=T ))
  dt <- c(tapply( df$batchDate, index, FUN=function(x, narm){ min(x, na.rm=narm) }, narm=T ))
  class(dt) <- class(df$batchDate)  # the tapply above strips class information.  make this back into a POSIX date
  attr(dt, "tzone") <- attr(df$batchDate, "tzone")   
  
  #   ---- Push this along for bootstrapping by week.  
  attr(dt, "JDates") <- attr(df, "JDates") 
  p.imp <- c(tapply( df$imputed.catch, index, FUN=mean, na.rm=T ))   # imputed.catch is the var with % per day.
  
  #   ----- Now, summarize passage by time period -----   
  index <- F.summarize.index( dt, summarize.by )
  n <- c(tapply( n, index, FUN=sum, na.rm=T ))
  dt <- c(tapply( dt, index, FUN=function(x, narm){ min(x, na.rm=narm) }, narm=T ))
  p.imp <- c(tapply( p.imp, index, FUN=mean, na.rm=T ))
  class(dt) <- class(df$batchDate)

  #   ----- Put catch and aux information into data frame -----
  n <- data.frame( s.by=names(n),
    passage=n,
    date=dt,
    pct.imputed.catch=p.imp,
    stringsAsFactors=F, row.names=NULL)

  n
}
