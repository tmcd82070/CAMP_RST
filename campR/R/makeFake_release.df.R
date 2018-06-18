#' @export
#'
#' @title makeFake_release.df
#'
#' @description Create a fake \code{release.df} data frame for use when no
#'   trials exist within the time period specified via \code{min.date} and
#'   \code{max.date}.
#'   
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#'   
#' @param max.date The end date for data to include.  Same format as
#'   \code{min.date}.
#'   
#' @param visit.df 
#'
#' @return A data frame with the number of rows equal to the number of days
#'   between \code{min.date} and \code{max.date}, inclusive.
#'
#' @details Days produced depend on the \code{seq.POSIXt} function with
#'   \code{by="1 DSTday"}.
#'
#' @seealso \code{F.get.release.data}
makeFake_release.df <- function(min.date,max.date,visit.df){

  # min.date <- "2016-03-02"
  # max.date <- "2016-04-01"
  
  pos <- 1
  envir <- as.environment(pos)
  time.zone <- get("time.zone",envir=.GlobalEnv)
  
  # ReleaseDates <- as.POSIXlt(seq.POSIXt(as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone),
  #                                       as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone),
  #                                       by="1 DSTday"))
  # ReleaseDates <- as.POSIXlt(seq.POSIXt(as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone),
  #                                       as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone),
  #                                       by="1 DSTday"))
  
  
  
  #   ---- Be sure to make a fake eff trial on a day within the range covered by our enh eff spline days.  
  
  #   ---- Get the temporal spline basis matrix goods.  
  here <- paste0(find.package("campR"),"/enhEffStats")  
  .tmpDataEnv <- new.env(parent=emptyenv()) 
  
  traps <- unique(visit.df$trapPositionID)
  release.df <- NULL
  for(trap in traps){
    load(paste0(here,"/",site,"_",trap,"_splineDays.RData"),envir=.tmpDataEnv)
    splineDays <- .tmpDataEnv$splineDays
  
    #   ---- Remap back to the present.  
    reMap2list <- reMap2(min.date,max.date,splineDays)
    strt.dt <- reMap2list$strt.dt
    end.dt <- reMap2list$end.dt
  
    #   ---- We need to make a fake efficiency trial on a day inside BOTH strt.dt and end.dt (days on 
    #   ---- which we fit an enh eff spline) AND min.date and max.date.  
    min.max.dates  <- as.POSIXlt(seq.POSIXt(as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone),
                                            as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone),by="1 DSTday"))
    strt.end.dates <- as.POSIXlt(seq.POSIXt(as.POSIXct(strt.dt ,format="%Y-%m-%d",tz=time.zone),
                                            as.POSIXct(end.dt  ,format="%Y-%m-%d",tz=time.zone),by="1 DSTday"))
  
    #   ---- Check for non-zero intersection.  If so, use last date.  Don't use first, because the 
    #   ---- 4 AM construct pushes the first date back one day -- this could end up being outside 
    #   ---- the true date range we care about.  If empty intersection...we can't do anything?  
    AintB <- intersect(min.max.dates,strt.end.dates)
    
    if(length(AintB) > 0){
    
      release.df.trap <- data.frame(projectDescriptionID=0,
                                    releaseID=0,
                                    IncludeTest=NA,
                                    IncludeCatch=NA,
                                    ReleaseDate=AintB[length(AintB)],
                                    siteID=site,
                                    siteName=NA,
                                    trapPositionID=trap,
                                    TrapPosition=visit.df[visit.df$trapPositionID == trap,]$TrapPosition[1],
                                    sampleGear=NA,
                                    HalfCone=NA,
                                    nReleased=1000,
                                    Recaps=0,
                                    ReleaseComments=NA,
                                    visitTime=.POSIXct(NA),
                                    visitTime2=.POSIXct(NA),
                                    visitTypeID=0,
                                    wmForkLength=0, 
                                    nForkLength=0,
                                    StartTime=.POSIXct(NA),
                                    EndTime=.POSIXct(NA),
                                    JasonSampleMinutes=difftime(.POSIXct(NA),.POSIXct(NA)),
                                    uniqueDate=0,
                                    sunMinutes=0,
                                    sunProp=0,
                                    moonMinutes=0,
                                    moonProp=0,
                                    nightMinutes=0,
                                    nightProp=0,
                                    halfConeAdj=0,    
                                    oldRecaps=0,
                                    allNightMins=0,
                                    allMoonMins=0,
                                    allSampleMins=0,
                                    allfl=0,
                                    allNfl=0,        
                                    HrsToFirstVisitAfter=0,
                                    HrsToLastVisitAfter=0,
                                    meanRecapTime=.POSIXct(NA),
                                    meanTimeAtLargeHrs=0,
                                    meanNightProp=0,
                                    meanMoonProp=0,       
                                    meanForkLength=0)
      release.df <- rbind(release.df,release.df.trap)
      
      #   ---- Clean up workspace.  
      rm(splineDays,reMap2list,strt.dt,end.dt,min.max.dates,strt.end.dates,AintB,release.df.trap)
    } else {
      cat(paste0("No intersection between min.date and max.date and enh.eff spline strt.dt and end.dt.\n"))
      cat(paste0("To estimate passage, choose a min.date and max.date that spans at least one day in \n"))
      cat(paste0("the enh.eff spline strt.dt and end.dt range for trap ",trap,".\n"))
    }
  }
  return(release.df)
}  