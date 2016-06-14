#' @export F.weekly.effort
#' 
#' @title F.weekly.effort
#' 
#' @description
#' 
#'    compute weekly effort.  Effort is number of days/hours that trap was fishing.
#' 
#'    site = site to do summary for
#'    min.date = minimum date to include
#'    max.date = maximum date to include
#'    output.file = root name of output file(s)
#' 
#'    ---- Check that times are less than 1 year apart
#' 
#' @param  site <describe argument>
#' @param  taxon <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date <describe argument>
#' @param  output.file  <describe argument>
#' 
#' @details <other comments found in file>
#'   === update 5/23/2015 === requires new connie-query. ===================================
#'    Open ODBC channel
#'    *****
#'    This SQL file develops the hours fished and TempSamplingSummary table 
#'    *****
#'    This SQL generates the daily allocation of minutes, so they sum to 1440
#'    Now, fetch the result 
#'  manipulate data
#'  check to make sure minutes.1 minutes.2 minutes.3 all exist, and if not, insert.
#'  set up df of dates and time possibilities.  get min and max over all traps so all resulting dfs are the same size.
#'  get helpful stuff for looping about traps
#'  clean up the julian week info 
#'  check for julian week 53
#'  make nice dfs for csv and for prep for plotting
#'  combine all the traps into an overall df
#'  the.sum <- NULL
#'  for(i in 1:nTraps){
#'    if(i == 1){
#'      the.sum <- df.Day[[1]][,c(6:10)]
#'    } else {
#'      the.sum <- the.sum + df.Day[[i]][,c(6:10)]
#'    }
#'  }  
#'  df.Day[[nTraps + 1]]$nTrapsDay <- (df.Day[[nTraps + 1]]$Total - df.Day[[nTraps + 1]]$Diff) / 1440
#'  get site label
#'  for each trap (and over all traps)
#'  check for week 53
#'    Compute locations for, and labels of, months'
#'    Lower plot
#'    Compute locations for, and labels of, months'
#'  output csv of minutes for trap i
#'    out.week.table2 <- paste(output.file, " Daily Effort Summary Table - ",eff.df3$Position[1],".csv", sep="")  
#'    write.table( df.Day[[i]], file=out.week.table2, sep=",", row.names=FALSE, col.names=TRUE)
#' cat(eff.df3Print)
#'    ---- Send messages back to the interface
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' <insert examples>
#' 
F.weekly.effort <- function( site, taxon, min.date, max.date, output.file ){
#
#   compute weekly effort.  Effort is number of days/hours that trap was fishing.
#
#   site = site to do summary for
#   min.date = minimum date to include
#   max.date = maximum date to include
#   output.file = root name of output file(s)

#   ---- Check that times are less than 1 year apart
strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
dt.len <- difftime(end.dt, strt.dt, units="days")
dt.len.min <- difftime(end.dt, strt.dt, units="mins")
if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")
  
#   ---- Start a progress bar
progbar <<- winProgressBar( "Weekly effort estimate", label=paste0("Reading data and accounting for the ",dt.len.min[1]," minutes your time range specified." ), width=500 )

nvisits <- F.buildReportCriteria( site, min.date, max.date )

if( nvisits == 0 ){
  warning("Your criteria returned no trapVisit table records.")
  return()
}

#  === update 5/23/2015 === requires new connie-query. ===================================
#   Open ODBC channel
db <- get( "db.file", envir=.GlobalEnv ) 
ch <- odbcConnectAccess(db)

#   *****
#   This SQL file develops the hours fished and TempSamplingSummary table 
F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon ) 

#   *****
#   This SQL generates the daily allocation of minutes, so they sum to 1440
F.run.sqlFile( ch, "QryWeeklyEffort.sql" ) 

#   Now, fetch the result 
newDF <- sqlFetch( ch, "TempEffortSummary_b" )

JDates <- sqlFetch( ch, "Dates" )
Site <- sqlFetch( ch, "Site")
F.sql.error.check(newDF)
close(ch) 

setWinProgressBar( progbar, getWinProgressBar(progbar)*.7 + .3 , label="Formatting results." )

# manipulate data
eff.df <- newDF[,c('Position','Year','JWeek','EffortDate','FishingEffort','Minutes')]
eff.df2 <- aggregate(eff.df$Minutes, list(Position=eff.df$Position, Year=eff.df$Year, JWeek=eff.df$JWeek, Date=eff.df$EffortDate, preEffortID=eff.df$FishingEffort), sum)

eff.df2$EffortID <- ifelse(eff.df2$preEffortID == 'Excluded',2,ifelse(eff.df2$preEffortID == 'Included',1,3))
eff.df2$preEffortID <- NULL                                                                                            # no longer needed
eff.df2 <- eff.df2[order(eff.df2$Position,eff.df2$Date,eff.df2$EffortID),]
names(eff.df2)[names(eff.df2) == 'x'] <- 'Minutes'

eff.dfWide <- reshape(eff.df2, v.names="Minutes", timevar="EffortID", idvar=c("Position", "Year", "JWeek", "Date"), direction="wide")
eff.dfWide[is.na(eff.dfWide)] <- 0

# check to make sure minutes.1 minutes.2 minutes.3 all exist, and if not, insert.
if(!("Minutes.1" %in% names(eff.dfWide))){eff.dfWide$Minutes.1 <- 0}
if(!("Minutes.2" %in% names(eff.dfWide))){eff.dfWide$Minutes.2 <- 0}
if(!("Minutes.3" %in% names(eff.dfWide))){eff.dfWide$Minutes.3 <- 0}

eff.dfWide <- eff.dfWide[,c('Position','Year','JWeek','Date','Minutes.1','Minutes.2','Minutes.3')]     # sort cols

eff.dfWide$Total <- eff.dfWide$Minutes.1 + eff.dfWide$Minutes.2 + eff.dfWide$Minutes.3
eff.dfWide$Diff <- eff.dfWide$Total - 1440

eff.dfWide <- eff.dfWide[,c('Position','Year','JWeek','Date','Minutes.1','Minutes.2','Minutes.3','Total','Diff')]
eff.dfWide$Date <- as.POSIXct(as.character(eff.dfWide$Date), tz=time.zone, format="%Y-%m-%d")
eff.dfWide$Date <- as.Date(eff.dfWide$Date)

# set up df of dates and time possibilities.  get min and max over all traps so all resulting dfs are the same size.
minDate <- min( eff.dfWide$Date )
maxDate <- max( eff.dfWide$Date )

# get helpful stuff for looping about traps
traps <- as.character(droplevels(unique(eff.dfWide$Position) ))
nTraps <- length(traps)

# clean up the julian week info 
theDates <- data.frame(Date=as.Date(seq(minDate,maxDate,by="days")))
JDates$Year <- as.numeric(format(JDates$uniqueDate,"%Y"))
JDates$Date <- as.Date(JDates$uniqueDate)
theDates <- merge(theDates,JDates[,c('Date','julianWeek','Year','julianWeekLabel')],by=c('Date'))
names(theDates)[names(theDates) == 'julianWeek'] <- 'JWeek'

# check for julian week 53
JWeekChecker <- unique(theDates[theDates$JWeek == 53,c("JWeek","julianWeekLabel")])                                        # pull out week 53 info
J53 <- ifelse(nrow(JWeekChecker) > 0,1,0)                                                                                  # indicator for julian week 53
J53nDays <- ifelse(J53 == 1,ifelse(nchar(as.character(droplevels(JWeekChecker$julianWeekLabel))) == 7,1,2),0)              # get number of days in week 53

# make nice dfs for csv and for prep for plotting
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

# combine all the traps into an overall df
# the.sum <- NULL
# for(i in 1:nTraps){
#   if(i == 1){
#     the.sum <- df.Day[[1]][,c(6:10)]
#   } else {
#     the.sum <- the.sum + df.Day[[i]][,c(6:10)]
#   }
# }  
df.Day[[nTraps + 1]] <- cbind(theDates,the.sum)
df.Day[[nTraps + 1]]$Position <- 'All Traps'
# df.Day[[nTraps + 1]]$nTrapsDay <- (df.Day[[nTraps + 1]]$Total - df.Day[[nTraps + 1]]$Diff) / 1440
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

# get site label
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
  eff.df3$Effort1h <- eff.df3$Minutes.1 / 60                                                                                        # convert to hours
  eff.df3$Effort2h <- eff.df3$Minutes.2 / 60                                                                                        # convert to hours
  eff.df3$Effort3h <- eff.df3$Minutes.3 / 60                                                                                        # convert to hours
  test <- t(eff.df3[,c('JWeek','Effort1h','Effort2h','Effort3h')])                                                                  # make plotting matrix
  test[ is.nan(test) ] <- 0                                                                                                         # put in 0s
  colnames(test) <- test[1,]                                                                                                        # put jweek as col headers
  test <- test[-1,]                                                                                                                 # drop jweek row in matrix

  theCols <- c("blue","red","white")
  theLegd <- c("Fishing successful","Fishing unsuccessful","Trap not fished")
  # check for week 53
  if(J53 == 1){
    getIt <- rep(0,ncol(test))
    getIt[colnames(test) == 53] <- (7 - J53nDays)*1440 / 60
    test <- rbind(test,getIt)
    theCols <- c("blue","red","white","gray")
    theLegd <- c("Fishing successful","Fishing unsuccessful","Trap not fished","Calendar end")
  }
  
  out.fn <- paste(output.file, paste0(" Effort - ",eff.df3$Position[1],".png"), sep="")
  tryCatch({png(filename=out.fn,width=7,height=7,units="in",res=1000)}, error=function(x){png(filename=out.fn)})
  z <- layout( matrix(c(1,2,3), ncol=1), height=c(0.1,0.45,0.45), widths=1)                                                           # layout plot area
  layout.show(z)                                                                                                                    # check plot area

  par(mar = c(0.2,0.2,0.2,0.2)); plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE); u <- par("usr");                             # blank area for title                                             # zero out margins, make empty plot, get bounding box     
  text(u[4]-0.07*(u[4]-u[3]),u[1] +   3*(u[2]-u[1])/5,adj=c(1,NA),siteLabel,cex=2.25)                                                # place site label
  text(u[4]-0.07*(u[4]-u[3]),u[1] + 1.5*(u[2]-u[1])/5,adj=c(1,NA),paste0("Weekly Effort from ",minDate,' through ',maxDate,": ",eff.df3$Position[1]),cex=1.0)            # place trap

  par(mar=c(1.0,6,0,2))                                                                                                             # set plot area
  
  mid.time <- round((1+ncol(test))/2)                                                                                               # find where to cut bars
  eff1.bars <- test[,1:mid.time]                                                                                                    # make top plot matrix
  eff2.bars <- test[,(mid.time+1):ncol(test)]                                                                                       # make bot plot matrix
  
  wk1.bars <- eff.df3$JWeek[1:mid.time]                                                                                             # set plot area
  wk2.bars <- eff.df3$JWeek[(mid.time+1):ncol(test)]                                                                                # set plot area
  
  yr1.bars <- eff.df3$Year[1:mid.time]                                                                                              # set plot area
  yr2.bars <- eff.df3$Year[(mid.time+1):ncol(test)]                                                                                 # set plot area

  yText <- ifelse(i == (nTraps + 1),"Weighted average hours per Julian week","Number of hours per Julian week")
  
  barplot(eff1.bars, 
          space=0, 
          col=theCols, 
          legend.text=theLegd, 
          args.legend=list(x="top", horiz=T, bty="n"), 
          ylab="", 
          xlab="", 
          ylim=c(-60,7*24*1.15), 
          yaxt = "n", 
          xaxt = "n")                                                                                                               # make top box plot

  mtext( side=2, text=yText, line=3, cex=0.7, at=c(NULL,84) )                                                                            # add y-axis label
  axis(2, at=seq(0, 7*24, by=24), cex.axis=0.8)                                                                                                   # add y-axis ticks
  
  #   Compute locations for, and labels of, months'
  wk1.Labels <- unique(as.character(droplevels(theDates[paste0(theDates$JWeek,theDates$Year) %in% paste0(wk1.bars,yr1.bars),]$julianWeekLabel)))                           # find where to place wk labels                              # place wk labels
  text(x=c(1:length(wk1.bars)) - 0.5, y = -31, labels=wk1.Labels, cex=0.7, srt = 90)

  #   Lower plot
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
  
  mtext( side=2, text=yText, line=3, cex=0.7, at=c(NULL,84) )    
  axis(2, at=seq(0, 7*24, by=24), cex.axis=0.8)

  #   Compute locations for, and labels of, months'
  wk2.Labels <- unique(as.character(droplevels(theDates[paste0(theDates$JWeek,theDates$Year) %in% paste0(wk2.bars,yr2.bars),]$julianWeekLabel)))                           # find where to place wk labels                              # place wk labels
  text(x=c(1:length(wk2.bars)) - 0.5, y = -31, labels=wk2.Labels, cex=0.7, srt = 90)
  
  dev.off()
  
  # output csv of minutes for trap i
  out.week.table <- paste(output.file, " Effort Summary Table - ",eff.df3$Position[1],".csv", sep="")
#   out.week.table2 <- paste(output.file, " Daily Effort Summary Table - ",eff.df3$Position[1],".csv", sep="")  
#   write.table( df.Day[[i]], file=out.week.table2, sep=",", row.names=FALSE, col.names=TRUE)
  
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
  #cat(eff.df3Print)
  cat("\n")
  sink()
  suppressWarnings(write.table( eff.df3Print, file=out.week.table, sep=",", append=TRUE, row.names=FALSE, col.names=TRUE))

  out.fn.roots <- c(out.fn.roots,out.week.table,out.fn)
  print(cat(out.fn.roots))
}

#   ---- Send messages back to the interface
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
  
