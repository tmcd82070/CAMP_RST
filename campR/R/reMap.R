#' @export
#' 
#' @title reMap
#'   
#' @description Map 1959-1960 efficiency-trial \code{batchDate}s back to the 
#'   present, as defined via \code{min.date} and \code{max.date}.
#'   
#' @param df Usually a \code{c0} dataframe housing the basis matrix used to fit 
#'   an underlying enhanced efficiency model.  Thus, rows are unique 
#'   (\code{batchDate2} in the 1959-1960 paradigm), while columns are dimensions
#'   associated with the underlying b-spline matrix.
#'   
#' @param bd A character vector of length one identifying the name of the 
#'   variable holding \code{batchDate2} to be mapped back (remapped) to the 
#'   present.
#'   
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#'   
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @param strt.dt The remapped start date associated with the current trap's 
#'   minimum (earliest) spline date.
#'   
#' @param end.dt The remapped end date associated with the current trap's 
#'   maximum (latest) spline date.
#'   
#' @return A list with two dataframes.  The first, \code{c0} contains the 
#'   remapped \code{batchDate}s (so originating \code{batchDate2}, remapped to 
#'   \code{batchDate}), for the originaing b-spline basis matrix, while the 
#'   second, \code{allDates} also contains remapped \code{batchDate}s for all 
#'   days of interest.
#'   
#'   Dataframe \code{allDates} is used as the originating left-table dataframe 
#'   against which all other covariate dataframes are merged.
#'   
#' @author WEST Inc.
#'   
#' @examples 
#' \dontrun{
#' reMap(df,bd,min.date,max.date,strt.dt,end.dt)
#' }
reMap <- function(df,bd,min.date,max.date,strt.dt,end.dt){
  
  # df <- c0
  # bd <- "batchDate2"
  # min.date <- min.date
  # max.date <- max.date
  # strt.dt <- strt.dt
  # end.dt <- end.dt
  
  #   ---- Get time.zone from global environment.
  time.zone <- get("time.zone",envir=.GlobalEnv)
  
  #   ---- Build a bridge to map 1960 batchDate2 to whatever regular batchDate we have.  leap year ok?
  bd2.lt <- as.POSIXlt(df[,bd])
  
  #   ---- This gets tricky.  If batchDate2 has a 2-29, but our real dates don't, we have a problem.
  checkLeapDay <- as.POSIXlt(seq.POSIXt(as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone),
                                        as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone),
                                        by="1 DSTday"))
  
  checkLeapDay2 <- as.POSIXlt(seq.POSIXt(as.POSIXct(strt.dt,format="%Y-%m-%d",tz=time.zone),
                                         as.POSIXct(end.dt,format="%Y-%m-%d",tz=time.zone),
                                         by="1 DSTday"))
  
  #   ---- Check if we should have leap day in the map back to the min.date max.date range.
  if( sum(checkLeapDay$mon == 1 & checkLeapDay$mday == 29) == 0 ){
    
    #   ---- Get rid of 2/29 -- don't need it.
    bd2.lt <- bd2.lt[!(bd2.lt$mon + 1 == 2 & bd2.lt$mday == 29)]   
    df <- df[!(as.POSIXlt(df[,bd])$mon + 1 == 2 & as.POSIXlt(df[,bd])$mday == 29),]
    
    checkLeapDay2 <- as.POSIXct(sort(checkLeapDay2[!(checkLeapDay2$mon + 1 == 2 & checkLeapDay2$mday == 29),]))
    
  }
  
  #   ---- If the minimum year of bd2.lt is 1960, we've wrapped around to 1960, after starting
  #   ---- in 1959.  If the minimum year of bs2.lt is 1959, we haven't wrapped around.  
  thebd2Year <- min(bd2.lt$year)
  
  #   ---- By design, the min.date and max.date span at most one year.  Add exactly the 
  #   ---- number of years between 1960 and the min(c0$batchDate$year) to the dates in bd2.lt.  
  #yearDiff <- min(as.POSIXlt(tmp.df$batchDate)$year) - thebd2Year   # Assumes all batchDates after 1960.
  yearDiff <- min(as.POSIXlt(df$batchDate)$year) - thebd2Year   # Assumes all batchDates after 1960.
  
  #   ---- Similar to the above with the strt.dt and end.dt, we need to be careful with the remap. 
  df <- df[order(df[,bd]),]
  
  #   ---- Make sure this batchDate is ct and not lt.
  if(class(checkLeapDay2)[1] == "POSIXlt"){
    df$batchDate <- as.POSIXct(checkLeapDay2)   #seq.POSIXt(strt.dt,end.dt,by="1 DSTday")
  }
  
  #   ---- Allow for all days in the spline enh eff trial period. 
  allDates <- data.frame(batchDate=seq(as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone),
                                       as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone),by="1 DSTday"))
  
  #   ---- The documentation indicates that allDates$batchDate should have tz="UTC", because I set it 
  #   ---- in the from of the seq call.  I suspect that my then placing it in a data.frame loses the 
  #   ---- tz specification, which is forcing it as "PDT".  Force it to "UTC"...again.
  allDates$batchDate <- as.POSIXct(allDates$batchDate,format="%Y-%m-%d",tz=time.zone)
  
  ans <- list(c0=df,allDates=allDates)
  
}  
  