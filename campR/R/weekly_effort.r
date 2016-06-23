#' @export F.weekly.effort
#'   
#' @title F.weekly.effort
#'   
#' @description Compute weekly effort, in terms of time spent "Fishing" versus
#'   "Not fishing."
#'   
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#' @param output.file A text string indicating a prefix to append to all output.
#'   
#' @return A png graphical display and csv of underlying data, for each unique 
#'   trap with fishing data between the specified \code{min.date} and
#'   \code{max.date}. An additional png and csv is output summarizing all traps
#'   together.
#'   
#' @details Dates provided by variables \code{min.date} and \code{max.date} 
#'   cannot span more than 366 days.  Additionally, the code is only set up to
#'   calculate fishing with a provided \code{taxon} of \code{161980}, i.e., 
#'   Chinook salmon.
#'   
#'   Function \code{F.weekly.effort} utilizes the Build Report Criteria Release 
#'   and Sample Period query series to first identify appropriate fishing 
#'   instances via variables \code{min.date} and \code{max.date}.  Series Weekly
#'   Effort then massages the fishing time data into the format necessary for 
#'   output.  See \code{F.run.sqlFile} for more information on queries.
#'   
#'   Allocation of time spent fishing is to one of either three possibilities.  The first 
#'   tabulates time during which a trap was not fishing; i.e., the trap was out 
#'   of the water.  The second and third summarize time for which the trap was 
#'   deployed but for which fishing was unsuccessful or successful, 
#'   respectively.  Variable \code{includeCatchID} determines whether or not a 
#'   fishing instance (identifiable via a unique \code{trapVisitID}) is 
#'   successful or not.
#'   
#'   All vertical bars in the resulting graphical display cover seven calendar 
#'   days, or 168 hours, with the exception of Julian week 53, which covers 
#'   December 30th to December 31st in leap years, and December 31st alone in 
#'   non-leap years. In this case, the "week" spans only 2 or 1 day(s), 
#'   respectively.  Gray coloring is used to flush out the remaining 5 or 6 
#'   days, respectively, for this special week.  In this way, all weeks cover a 
#'   7-day period within the bar-chart graphical output.
#'   
#'   Resulting bar chart pngs and csvs for summary fishing over all unique traps
#'   between the provided \code{min.date} and \code{max.date} calculate total 
#'   minutes on a per-day basis.  Thus, if three traps fished on one day, but 
#'   only two the next, the program utilizes \eqn{1440*3=4320} total minutes for
#'   the first day, but only \eqn{1440*2=2880} total minutes for the second. 
#'   Note that \eqn{1440 = 24*60} minutes for one day.
#'   
#' @seealso \code{\link{F.run.sqlFile}}
#'   
#' @author WEST Inc.
#'   
#' @examples
#' \dontrun{
#' #   ---- Estimate the weekly effort on the American River for all 
#' #   ---- inclusive traps, from Jan. 16, 2013 through June 8, 2013.  
#' F.weekly.effort(57000,161980,"2013-01-16","2013-06-08","American River")
#' }
F.weekly.effort <- function( site, taxon, min.date, max.date, output.file ){
  
  # site <- 57000
  # taxon <- 161980
  # min.date <- "2013-01-16"
  # max.date <- "2013-06-08"
  # output.file <- 

  #   ---- Check that times are less than 1 year apart.
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  dt.len <- difftime(end.dt, strt.dt, units="days")
  dt.len.min <- difftime(end.dt, strt.dt, units="mins")
  if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")
  
  #   ---- Start a progress bar.
  progbar <<- winProgressBar( "Weekly effort estimate", label=paste0("Reading data and accounting for the ",dt.len.min[1]," minutes your time range specified." ), width=500 )
  
  
  nvisits <- F.buildReportCriteria( site, min.date, max.date )
  
  if( nvisits == 0 ){
    warning("Your criteria returned no trapVisit table records.")
    return()
  }
  
  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv ) 
  ch <- odbcConnectAccess(db)
  
  #   ---- Develop the hours fished and TempSamplingSummary table.
  F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon ) 
  
  #   ---- Generate the daily allocation of minutes, so they sum to 1440.
  F.run.sqlFile( ch, "QryWeeklyEffort.sql" ) 
  
  #   ---- Fetch the result.
  newDF <- sqlFetch( ch, "TempEffortSummary_b" )
  
  #   ---- Obtain Julian dates so days can be mapped to specialized Julian weeks. 
  JDates <- sqlFetch( ch, "Dates" )
  Site <- sqlFetch( ch, "Site")
  F.sql.error.check(newDF)
  close(ch) 
  
  setWinProgressBar( get("progbar",envir=.GlobalEnv), getWinProgressBar(get("progbar",envir=.GlobalEnv))*.7 + .3 , label="Formatting results." )
  
  #   ---- Given data, compile per-trap, per-day summaries of the three different
  #   ---- types of fishing.  
  eff.df <- newDF[,c('Position','Year','JWeek','EffortDate','FishingEffort','Minutes')]
  eff.df2 <- aggregate(eff.df$Minutes, list(Position=eff.df$Position, Year=eff.df$Year, JWeek=eff.df$JWeek, Date=eff.df$EffortDate, preEffortID=eff.df$FishingEffort), sum)
  
  eff.df2$EffortID <- ifelse(eff.df2$preEffortID == 'Excluded',2,ifelse(eff.df2$preEffortID == 'Included',1,3))
  eff.df2$preEffortID <- NULL                                                        
  eff.df2 <- eff.df2[order(eff.df2$Position,eff.df2$Date,eff.df2$EffortID),]
  names(eff.df2)[names(eff.df2) == 'x'] <- 'Minutes'
  
  eff.dfWide <- reshape(eff.df2, v.names="Minutes", timevar="EffortID", idvar=c("Position", "Year", "JWeek", "Date"), direction="wide")
  eff.dfWide[is.na(eff.dfWide)] <- 0
  
  #   ---- Check to make sure minutes.1 minutes.2 minutes.3 all exist, and if not, insert.
  if(!("Minutes.1" %in% names(eff.dfWide))){eff.dfWide$Minutes.1 <- 0}
  if(!("Minutes.2" %in% names(eff.dfWide))){eff.dfWide$Minutes.2 <- 0}
  if(!("Minutes.3" %in% names(eff.dfWide))){eff.dfWide$Minutes.3 <- 0}
  
  #   ---- Sort columns for what ease of presentation.
  eff.dfWide <- eff.dfWide[,c('Position','Year','JWeek','Date','Minutes.1','Minutes.2','Minutes.3')]    
  
  eff.dfWide$Total <- eff.dfWide$Minutes.1 + eff.dfWide$Minutes.2 + eff.dfWide$Minutes.3
  eff.dfWide$Diff <- eff.dfWide$Total - 1440
  
  eff.dfWide <- eff.dfWide[,c('Position','Year','JWeek','Date','Minutes.1','Minutes.2','Minutes.3','Total','Diff')]
  eff.dfWide$Date <- as.POSIXct(as.character(eff.dfWide$Date), tz=get("time.zone",envir=.GlobalEnv), format="%Y-%m-%d")
  eff.dfWide$Date <- as.Date(eff.dfWide$Date)
  
  #   ---- Set up data frame of dates and time possibilities.  Get min and max over all 
  #   ---- traps so all resulting data frames are the same size.
  minDate <- min( eff.dfWide$Date )
  maxDate <- max( eff.dfWide$Date )
  
  #   ---- Get helpful stuff for looping over traps.
  traps <- as.character(droplevels(unique(eff.dfWide$Position) ))
  nTraps <- length(traps)
  
  #   ---- Clean up the Julian week information.
  theDates <- data.frame(Date=as.Date(seq(minDate,maxDate,by="days")))
  JDates$Year <- as.numeric(format(JDates$uniqueDate,"%Y"))
  JDates$Date <- as.Date(JDates$uniqueDate)
  theDates <- merge(theDates,JDates[,c('Date','julianWeek','Year','julianWeekLabel')],by=c('Date'))
  names(theDates)[names(theDates) == 'julianWeek'] <- 'JWeek'
  
  #   ---- Check for Julian week 53, pull it out, make an indicator, and then get
  #   ---- the number of days in it. 
  JWeekChecker <- unique(theDates[theDates$JWeek == 53,c("JWeek","julianWeekLabel")])
  J53 <- ifelse(nrow(JWeekChecker) > 0,1,0)
  J53nDays <- ifelse(J53 == 1,ifelse(nchar(as.character(droplevels(JWeekChecker$julianWeekLabel))) == 7,1,2),0)
  
  #   ---- Make nice data frames for output csv and also prep for plotting.
  #   ---- Note that the last entry in the df.Day list is for all traps combined.
  df.Day <- vector("list",nTraps + 1)
  the.sum <- NULL
  for(i in 1:nTraps){
    theDates$Position <- traps[i]
    df.Day[[i]] <- merge(theDates,eff.dfWide[eff.dfWide$Position == traps[i],],by=c('Date','Year','Position','JWeek'),all.x=TRUE)
    df.Day[[i]][is.na(df.Day[[i]])] <- 0
    df.Day[[i]]$DataPresent <- ifelse(df.Day[[i]]$Minutes.1 > 0 | df.Day[[i]]$Minutes.2 > 0,1,0)
    if(i == 1){
      the.sum <- df.Day[[1]][,c(6:11)]
    } else {
      the.sum <- the.sum + df.Day[[i]][,c(6:11)]
    }
    for(j in 1:nrow(df.Day[[i]])){
      if(df.Day[[i]]$Total[j] == 0){
        df.Day[[i]]$Total[j] <- 1440
        df.Day[[i]]$Minutes.3[j] <- 1440
      }
    }
  }
  
  #   ---- Combine all trap data into an overall data frame.
  df.Day[[nTraps + 1]] <- cbind(theDates,the.sum)
  df.Day[[nTraps + 1]]$Position <- 'All Traps'
  for(i in 1:nrow(df.Day[[nTraps + 1]])){
    if(df.Day[[nTraps + 1]]$DataPresent[i] > 0){
      df.Day[[nTraps + 1]]$Minutes.3[i] <- ((1440 * df.Day[[nTraps + 1]]$DataPresent[i]) - (df.Day[[nTraps + 1]]$Minutes.1[i] + df.Day[[nTraps + 1]]$Minutes.2[i] )) / df.Day[[nTraps + 1]]$DataPresent[i]
      df.Day[[nTraps + 1]]$Minutes.1[i] <- df.Day[[nTraps + 1]]$Minutes.1[i] / df.Day[[nTraps + 1]]$DataPresent[i]
      df.Day[[nTraps + 1]]$Minutes.2[i] <- df.Day[[nTraps + 1]]$Minutes.2[i] / df.Day[[nTraps + 1]]$DataPresent[i]   
    } else {
      df.Day[[nTraps + 1]]$Minutes.3[i] <- 1440
    }
  }
  df.Day[[nTraps + 1]]$Total <- df.Day[[nTraps + 1]]$Minutes.1 + df.Day[[nTraps + 1]]$Minutes.2 + df.Day[[nTraps + 1]]$Minutes.3
  df.Day[[nTraps + 1]]$Diff <- round(df.Day[[nTraps + 1]]$Total - 1440,3)
  
  #   ---- Get site label.
  siteLabel <- as.character(droplevels(Site[Site$siteID == site,]$siteName))
  
  setWinProgressBar( progbar, .7 , label="Creating trap-specific plots." )
  
  # for each trap (and over all traps)
  
  if(nTraps == 1){
    stopHere <- nTraps
  } else {
    stopHere <- nTraps + 1
  }
  out.fn.roots <- NULL
  for(i in 1:stopHere){
    
    eff.df3 <- aggregate(data.frame(Minutes.1=df.Day[[i]]$Minutes.1,Minutes.2=df.Day[[i]]$Minutes.2,Minutes.3=df.Day[[i]]$Minutes.3,Total=df.Day[[i]]$Total,Diff=df.Day[[i]]$Diff), list(Position=df.Day[[i]]$Position, Year=df.Day[[i]]$Year, JWeek=df.Day[[1]]$JWeek, Label=df.Day[[1]]$julianWeekLabel), sum)
    eff.df3 <- eff.df3[order(eff.df3$Position,eff.df3$Year,eff.df3$JWeek),]
    
    #   ---- Convert to hours.
    eff.df3$Effort1h <- eff.df3$Minutes.1 / 60                                                                                        
    eff.df3$Effort2h <- eff.df3$Minutes.2 / 60
    eff.df3$Effort3h <- eff.df3$Minutes.3 / 60
    
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
    
    out.fn <- paste(output.file, paste0(" Effort - ",eff.df3$Position[1],".png"), sep="")
    tryCatch({png(filename=out.fn,width=7,height=7,units="in",res=1000)}, error=function(x){png(filename=out.fn)})
    
    #   ---- Layout plotting area. 
    z <- layout( matrix(c(1,2,3), ncol=1), heights=c(0.1,0.45,0.45), widths=1)
    layout.show(z)
  
    par(mar = c(0.2,0.2,0.2,0.2)); plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE); u <- par("usr");     
    text(u[4]-0.07*(u[4]-u[3]),u[1] +   3*(u[2]-u[1])/5,adj=c(1,NA),siteLabel,cex=2.25)
    text(u[4]-0.07*(u[4]-u[3]),u[1] + 1.5*(u[2]-u[1])/5,adj=c(1,NA),paste0("Weekly Effort from ",minDate,' through ',maxDate,": ",eff.df3$Position[1]),cex=1.0)
  
    par(mar=c(1.0,6,0,2))
    
    #   ---- Fins where to cut bars and make top plot matrix and bottom plot matrix.
    mid.time <- round((1+ncol(test))/2)
    eff1.bars <- test[,1:mid.time]
    eff2.bars <- test[,(mid.time+1):ncol(test)]
    
    #   ---- Set plot area. 
    wk1.bars <- eff.df3$JWeek[1:mid.time]
    wk2.bars <- eff.df3$JWeek[(mid.time+1):ncol(test)]
    yr1.bars <- eff.df3$Year[1:mid.time]
    yr2.bars <- eff.df3$Year[(mid.time+1):ncol(test)]
  
    yText <- ifelse(i == (nTraps + 1),"Weighted average hours per Julian week","Number of hours per Julian week")
    
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
    wk1.Labels <- unique(as.character(droplevels(theDates[paste0(theDates$JWeek,theDates$Year) %in% paste0(wk1.bars,yr1.bars),]$julianWeekLabel)))                           # find where to place wk labels                              # place wk labels
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
    wk2.Labels <- unique(as.character(droplevels(theDates[paste0(theDates$JWeek,theDates$Year) %in% paste0(wk2.bars,yr2.bars),]$julianWeekLabel)))                           # find where to place wk labels                              # place wk labels
    text(x=c(1:length(wk2.bars)) - 0.5, y = -31, labels=wk2.Labels, cex=0.7, srt = 90)
    
    dev.off()
    
    #   ---- Output csv of minutes for the ith trap.
    out.week.table <- paste(output.file, " Effort Summary Table - ",eff.df3$Position[1],".csv", sep="")

    #   ---- Format output. 
    eff.df3Print <- eff.df3[,!(names(eff.df3) %in% c('Position','JWeek'))]
    names(eff.df3Print)[names(eff.df3Print) == 'Minutes.1'] <- 'FishSuccess'
    names(eff.df3Print)[names(eff.df3Print) == 'Minutes.2'] <- 'FishUnsuccess'
    names(eff.df3Print)[names(eff.df3Print) == 'Minutes.3'] <- 'TrapNotFished'
    eff.df3Print <- eff.df3Print[,c('Label','Year','FishSuccess','FishUnsuccess','TrapNotFished','Total','Diff')]
  
    rs <- paste0(minDate,' through ',maxDate)
  
    sink(out.week.table)
    cat(paste("Site=,", siteLabel, "\n", sep=""))
    cat(paste("Site ID=,", eff.df3$Position[1], "\n", sep=""))
    cat(paste("Species ID=,", 161980, "\n", sep=""))
    cat(paste("Summarized by=week\n", sep=""))
    cat(paste("Dates included=,", rs, "\n", sep=""))
    cat(paste("Note:  All time units in minutes.\n"))  
  
    cat("\n")
    cat("\n")
    sink()
    suppressWarnings(write.table( eff.df3Print, file=out.week.table, sep=",", append=TRUE, row.names=FALSE, col.names=TRUE))
  
    out.fn.roots <- c(out.fn.roots,out.week.table,out.fn)
    print(cat(out.fn.roots))
  }
  
  #   ---- Send messages back to the interface.  
  cat("SUCCESS - F.weekly.effort\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<No RData saved>", "\n\n"))
  cat(paste("Number of files created in working directory =", 2*stopHere, "\n"))
  for(i in 1:length(out.fn.roots)){
    cat(paste(out.fn.roots[i], "\n", sep=""))
  }
  cat("\n")
  
  setWinProgressBar( progbar, 1 , label="SUCCESS" )
  close(progbar)
  
  invisible(eff.df3Print)

}