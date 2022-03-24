#' @export
#'   
#' @title F.weekly.effort - Plot fishing and non-fishing effort.
#'   
#' @description Compute weekly "Fishing" and 
#'   "Not fishing" hours, then plot.
#'   
#' @inheritParams F.size.by.date
#' 
#' @return A \code{png} graphical display and \code{csv} of underlying data, for
#'   each unique trap with fishing data between the specified \code{min.date}
#'   and \code{max.date}. An additional \code{png} and \code{csv} is output 
#'   summarizing all traps together.
#'   
#' @details Dates provided by variables \code{min.date} and \code{max.date} 
#'   cannot span more than 366 days.  Additionally, the code is only set up to 
#'   calculate fishing with a provided \code{taxon} of \code{161980}, i.e., 
#'   Chinook Salmon.
#'   
#'   Function \code{F.weekly.effort} utilizes the Build Report Criteria Release 
#'   and Sample Period query series to first identify appropriate fishing 
#'   instances between \code{min.date} and \code{max.date}.  Series Weekly
#'   Effort then massages the fishing time data into the format necessary for 
#'   output.  See section Structured Query Language (SQL) Queries in
#'   \code{F.run.sqlFile} for more information on query series.
#'   
#'   Allocation of time spent fishing is to one of three possibilities. 
#'   The first tabulates time during which a trap was not fishing; i.e., the
#'   trap was out of the water.  The second and third summarize time for which
#'   the trap was deployed but for which fishing was unsuccessful or successful,
#'   respectively.  Variable \code{includeCatchID} determines whether or not a 
#'   fishing instance (identifiable via a unique \code{trapVisitID}) was 
#'   successful or not.
#'   
#'   All vertical bars in the graph cover seven calendar 
#'   days, or 168 hours, with the exception of Julian week 53, which covers 
#'   December 30th to December 31st in leap years, and December 31st alone in 
#'   non-leap years. In this case, the "week" spans only 2 or 1 day(s), 
#'   respectively.  Gray coloring is used to flesh out the remaining 5 or 6 
#'   days, respectively, for this special week.  In this way, all weeks cover a 
#'   7-day period within the bar-chart graphical output.
#'   
#'   The output graph and \code{csv}s  for summary fishing over
#'   all unique traps between the provided \code{min.date} and \code{max.date}
#'   calculate total minutes on a per-day basis.  Thus, if three traps fished on
#'   one day, but only two the next, the program utilizes \eqn{1440*3=4320}
#'   total minutes for the first day, but only \eqn{1440*2=2880} total minutes
#'   for the second. Note there are \eqn{1440 = 24*60} minutes in one day.
#'   
#' @seealso \code{F.run.sqlFile}
#'   
#' @inheritSection F.size.by.date Author
#'   
#' @examples
#' \dontrun{
#' #   ---- Estimate the weekly effort on the American River for all 
#' #   ---- inclusive traps, from Jan. 16, 2013 through June 8, 2013.  
#' site <- 57000
#' taxon <- 161980
#' min.date <- "2016-01-16"
#' max.date <- "2013-06-08"
#' output.file <- "American River"
#' F.weekly.effort(site,taxon,min.date,max.date,output.file)
#' }
F.weekly.effort <- function( site, taxon, min.date, max.date, output.file ){
  
  #   ---- A constant used in this routine
  minPerDay <- 24*60
  
  #   ---- Make sure we have all temp tables.
  tableChecker()
  
  #   ---- Check that times are less than 1 year apart.
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  dt.len <- difftime(end.dt, strt.dt, units="days")
  dt.len.min <- difftime(end.dt, strt.dt, units="mins")
  if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")
  
  #   ---- Check that taxon is Chinook salmon.  
  if( taxon != 161980 ) stop("Cannot specify any species other than Chinook salmon, code 161980.")
  
  #   ---- Start a progress bar.
  progbar <- winProgressBar( "Weekly effort estimate", label=paste0("Reading data and accounting for the ",dt.len.min[1]," minutes your time range specified." ), width=500 )
  
  # Utilize this construction to avoid NOTEs about assigning variables to the 
  # .GlobalEnv when running devtools::check().  
  pos <- 1
  envir <- as.environment(pos)
  assign("progbar",progbar,envir=envir)
  
  nvisits <- F.buildReportCriteria( site, min.date, max.date )
  
  if( nvisits == 0 ){
    warning("Your criteria returned no trapVisit table records.")
    return()
  }
  
  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv ) 
  ch <- RODBC::odbcConnectAccess(db)
  
  #   ---- Develop the hours fished and TempSamplingSummary table.
  F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon ) 
  
  #   ---- Generate the daily allocation of minutes, so they sum to 1440.
  F.run.sqlFile( ch, "QryWeeklyEffort.sql" ) 
  
  #   ---- Fetch the result.
  newDF <- RODBC::sqlFetch( ch, "TempEffortSummary_b" )
  F.sql.error.check(newDF)
  
  #   ---- Set time zone here so it propagates through all calculations
  newDF <- newDF %>% 
    dplyr::mutate(EffortDate = as.POSIXct(format(EffortDate), tz = get("time.zone", envir = .GlobalEnv)))
    
  #   ---- Obtain Julian dates so days can be mapped to specialized Julian weeks. 
  # JDates <- RODBC::sqlFetch( ch, "Dates" )
  Site <- RODBC::sqlFetch( ch, "Site")
  close(ch) 
  
  setWinProgressBar( get("progbar",envir=.GlobalEnv), getWinProgressBar(get("progbar",envir=.GlobalEnv))*.7 + .3 , label="Formatting results." )
  
  #   ---- Given data, compile per-trap, per-day summaries of the three different
  #   ---- types of fishing.  
  eff.df2 <- newDF %>% 
    dplyr::select(Position, Year, JWeek, EffortDate, FishingEffort, Minutes) %>% 
    dplyr::group_by(Position, Year, JWeek, EffortDate, FishingEffort) %>% 
    dplyr::summarise(Minutes = sum(Minutes), .groups = 'drop') %>% 
    dplyr::mutate(EffortID = dplyr::case_when(
      FishingEffort == "Excluded" ~ 2,
      FishingEffort == "Included" ~ 1,
      FishingEffort == "Not fishing" ~ 3,
      TRUE ~ NA_real_)) %>% 
    dplyr::select(-FishingEffort) %>% 
    dplyr::arrange(Position, EffortDate, EffortID) %>% 
    dplyr::rename(Date = EffortDate)
      
  eff.dfWide <- eff.df2 %>% 
    tidyr::pivot_wider(id_cols = c("Position", "Year", "JWeek", "Date"), 
                       names_from = EffortID, 
                       names_prefix = "Minutes.", 
                       values_from = Minutes, 
                       values_fill = 0) %>% 
    dplyr::select(Position, Year, JWeek, Date, Minutes.1, Minutes.2, Minutes.3) %>% 
    dplyr::mutate(Total = Minutes.1 + Minutes.2 + Minutes.3, 
                  Diff = Total - minPerDay)
  

  #   ---- Set up data frame of dates and time possibilities.  Get min and max over all 
  #   ---- traps so all resulting data frames are the same size.
  minDate <- min( eff.dfWide$Date )
  maxDate <- max( eff.dfWide$Date )
  
  #   ---- Get helpful stuff for looping over traps.
  traps <- unique(eff.dfWide$Position)
  nTraps <- length(traps)
  
  #   ---- Clean up the Julian week information.
  theDates <- data.frame(Date=seq(minDate,maxDate,by="days")) %>% 
    dplyr::mutate(JWeek = as.numeric(format(Date, "%V")), 
                  mon = format(Date, "%b"), 
                  day = format(Date, "%d")) %>% 
    dplyr::group_by(JWeek) %>% 
    dplyr::summarise(julianWeekLabel = paste(dplyr::first(mon), dplyr::first(day), 
                                             "-", 
                                             dplyr::last(mon), dplyr::last(day)))

  #   ---- Check for Julian week 53, pull it out, make an indicator, and then get
  #   ---- the number of days in it. 
  JWeekChecker <- unique(theDates[theDates$JWeek == 53,c("JWeek","julianWeekLabel")])
  J53 <- as.numeric(any(theDates$JWeek == 53))
  if(J53 == 1){
    J53nDays <- ifelse(nchar(as.character(JWeekChecker$julianWeekLabel)) == 7,1,2)
  } else {
    J53nDays <- 0
  }
  
  #   ---- Make nice data frames for output csv and also prep for plotting.
  #   ---- Note that the last entry in the df.Day list is for all traps combined.
  df.Day <- vector("list",nTraps + 1)
  the.sum <- NULL
  for(i in 1:nTraps){
    df.Day[[i]] <- theDates %>% 
      dplyr::mutate(Position = traps[i]) %>% 
      dplyr::left_join(eff.dfWide, by = c("Position", "JWeek"))
    
    # df.Day[[i]] <- merge(theDates,eff.dfWide[eff.dfWide$Position == traps[i],],by=c('Date','Year','Position','JWeek'),all.x=TRUE)
    df.Day[[i]][is.na(df.Day[[i]])] <- 0
    
    df.Day[[i]] <- df.Day[[i]] %>% 
      dplyr::mutate(DataPresent = ifelse(Minutes.1 > 0 | Minutes.2 > 0,1,0),
                    Minutes.3 = ifelse(Total == 0, minPerDay, Minutes.3),
                    Total = ifelse(Total == 0, minPerDay, Total))  

    if(i == 1){
      the.sum <- df.Day[[i]] %>% dplyr::select(dplyr::matches("Minutes"), Total, Diff, DataPresent)
    } else {
      the.sum <- the.sum + (df.Day[[i]] %>% dplyr::select(dplyr::matches("Minutes"), Total, Diff, DataPresent))
    }
    
  }
  
  #   ---- Combine all trap data into an overall data frame.
  #   When summing, do not rely on the Total column because of missingness.
  the.sum <- tibble::tibble(the.sum) %>%   
    dplyr::mutate(Minutes.1 = ifelse(DataPresent > 0, Minutes.1 / DataPresent, 0),
                  Minutes.2 = ifelse(DataPresent > 0, Minutes.2 / DataPresent, 0),
                  Minutes.3 = ifelse(DataPresent > 0, minPerDay - Minutes.1 - Minutes.2, minPerDay), 
                  Total = Minutes.1 + Minutes.2 + Minutes.3, 
                  Diff = round(Total - minPerDay, 3))
  df.Day[[nTraps + 1]] <- dplyr::bind_cols((df.Day[[1]] %>%  dplyr::select(JWeek, julianWeekLabel, Position, Year, Date)),the.sum)
  df.Day[[nTraps + 1]]$Position <- 'AllTraps'
  
  #   ---- Get site label.
  siteLabel <- Site %>% 
    dplyr::filter(siteID == site) %>% 
    dplyr::pull(siteName)
  
  setWinProgressBar( get("progbar",envir=.GlobalEnv), .7 , label="Creating trap-specific plots." )
  
  # for each trap (and over all traps)
  
  if(nTraps == 1){
    stopHere <- nTraps
  } else {
    stopHere <- nTraps + 1
  }
  out.fn.roots <- NULL
  for(i in 1:stopHere){
    
    eff.df3 <- df.Day[[i]] %>% 
      dplyr::group_by(Position, JWeek) %>% 
      dplyr::summarise(Year = dplyr::first(Year), 
                       julianWeekLabel = dplyr::first(julianWeekLabel),
                       Minutes.1 = sum(Minutes.1), 
                       Minutes.2 = sum(Minutes.2), 
                       Minutes.3 = sum(Minutes.3), 
                       Total = sum(Total), 
                       Diff = sum(Diff), 
                       .groups = "drop") %>% 
      dplyr::mutate(Effort1h = Minutes.1 / 60,
                    Effort2h = Minutes.2 / 60,
                    Effort3h = Minutes.3 / 60) %>% 
      dplyr::arrange(Position, Year, JWeek)

    #   ---- Make plotting matrix.
    test <- t(eff.df3[,c('JWeek','Effort1h','Effort2h','Effort3h')])                                                                  
    test[ is.nan(test) ] <- 0
    colnames(test) <- test[1,]
    test <- test[-1,]
  
    #   ---- Set up for plotting.  
    theCols <- c("blue","red","white")
    theLegd <- c("Fishing successful","Fishing unsuccessful","Trap not fished")
    
    #   ---- Check for week 53.  
    if(J53 == 1){
      getIt <- rep(0,ncol(test))
      getIt[colnames(test) == 53] <- (7 - J53nDays)*1440 / 60
      test <- rbind(test,getIt)
      theCols <- c("blue","red","white","gray")
      theLegd <- c("Fishing successful","Fishing unsuccessful","Trap not fished","Calendar end")
    }
    
    out.fn <- paste(output.file, paste0("_EffortPlot_",eff.df3$Position[1],".png"), sep="")
    tryCatch({png(filename=out.fn,width=7,height=7,units="in",res=1000)}, error=function(x){png(filename=out.fn)})
    
    #   ---- Layout plotting area. 
    z <- layout( matrix(c(1,2,3), ncol=1), heights=c(0.1,0.45,0.45), widths=1)

    par(mar = c(0.2,0.2,0.2,0.2)); 
    plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE); u <- par("usr");     
    text(u[4]-0.07*(u[4]-u[3]),u[1] +   3*(u[2]-u[1])/5,adj=c(1,NA),siteLabel,cex=2.25)
    text(u[4]-0.07*(u[4]-u[3]),u[1] + 1.5*(u[2]-u[1])/5,adj=c(1,NA),paste0("Weekly Effort from ",minDate,' through ',maxDate,": ",eff.df3$Position[1]),cex=1.0)
  
    par(mar=c(1.0,6,0,2))
    
    #   ---- Fins where to cut bars and make top plot matrix and bottom plot matrix.
    mid.time <- round((1+ncol(test))/2)
    eff1.bars <- test[,1:mid.time]
    eff2.bars <- test[,(mid.time+1):ncol(test)]
    
    #   ---- Set plot area. 
    wk1.bars <- eff.df3$JWeek[1:mid.time]
    wk1.Labels <- eff.df3$julianWeekLabel[1:mid.time]
    yr1.bars <- eff.df3$Year[1:mid.time]
    
    wk2.bars <- eff.df3$JWeek[(mid.time+1):ncol(test)]
    wk2.Labels <- eff.df3$julianWeekLabel[(mid.time+1):ncol(test)]
    yr2.bars <- eff.df3$Year[(mid.time+1):ncol(test)]
    
    yText <- ifelse(i == (nTraps + 1),"Weighted average hours per Julian week","Number of hours per week")
    
    #   ---- Actually make the top plot. 
    barplot(eff1.bars, 
            space=0, 
            col=theCols, 
            legend.text=theLegd, 
            args.legend=list(x="top", horiz=T, bty="n"), 
            ylab="", 
            xlab="", 
            ylim=c(-60,7*24*1.15), 
            yaxt = "n", 
            xaxt = "n")
  
    #   ---- Format top plot. 
    mtext( side=2, text=yText, line=3, cex=0.7, at=c(NULL,84) )
    axis(2, at=seq(0, 7*24, by=24), cex.axis=0.8)
    text(x=c(1:length(wk1.bars)) - 0.5, y = -31, labels=wk1.Labels, cex=0.7, srt = 90)
  
    #   ---- Actually make the bottom plot. 
    par(mar=c(1.0,6,0,2))
    barplot(eff2.bars, 
            space=0, 
            col=theCols, 
            legend.text=theLegd, 
            args.legend=list(x="top", horiz=T, bty="n"), 
            ylab="", 
            xlab="", 
            ylim=c(-60,7*24*1.15),
            yaxt = "n", 
            xaxt = "n")
    
    #   ---- Format the bottom plot.  
    mtext( side=2, text=yText, line=3, cex=0.7, at=c(NULL,84) )    
    axis(2, at=seq(0, 7*24, by=24), cex.axis=0.8)
    text(x=c(1:length(wk2.bars)) - 0.5, y = -31, labels=wk2.Labels, cex=0.7, srt = 90)
    
    dev.off()
    
    #   ---- Output csv of minutes for the ith trap.
    out.week.table <- paste(output.file, "_EffortSummaryTable_",eff.df3$Position[1],".csv", sep="")

    #   ---- Format output. 
    eff.df3Print <- eff.df3 %>% 
      dplyr::rename(FishSuccess = Minutes.1, 
                    FishUnsuccess = Minutes.2, 
                    TrapNotFished = Minutes.3) %>% 
    dplyr::select('julianWeekLabel','Year','FishSuccess','FishUnsuccess','TrapNotFished','Total','Diff') 
      
    rs <- paste0(minDate,' through ',maxDate)
  
    sink(out.week.table)
    cat(paste("Site=,", siteLabel, "\n", sep=""))
    cat(paste("Site ID=,", eff.df3$Position[1], "\n", sep=""))
    cat(paste("Species ID=,", 161980, "\n", sep=""))
    cat(paste("Summarized by= week\n", sep=""))
    cat(paste("Dates included=,", rs, "\n", sep=""))
    cat(paste("Note:  All time units in minutes.\n"))  
  
    cat("\n")
    cat("\n")
    sink()
    suppressWarnings(write.table( eff.df3Print, file=out.week.table, sep=",", append=TRUE, row.names=FALSE, col.names=TRUE))
  
    out.fn.roots <- c(out.fn.roots,out.week.table,out.fn)
  }
  
  tableDeleter()
  
  #   ---- Send messages back to the interface.  
  cat("SUCCESS - F.weekly.effort\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<No RData saved>", "\n\n"))
  cat(paste("Number of files created in working directory =", 2*stopHere, "\n"))
  for(i in 1:length(out.fn.roots)){
    cat(paste(out.fn.roots[i], "\n", sep=""))
  }
  cat("\n")
  
  setWinProgressBar( get("progbar",envir=.GlobalEnv), 1 , label="SUCCESS" )
  close(progbar)
  
  invisible(eff.df3Print)

}