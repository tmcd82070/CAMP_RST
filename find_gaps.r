find_gaps <- function( river, site, taxon, min.date, max.date ){
  
#   river <- "American River"
#   site <- 57000
#   taxon <- 161980
#   min.date <- '1980-01-01'
#   max.date <- '2016-03-02'

  if(river == ''){
    db.file <- db.file1
  } else if(river == 'Sacramento River'){
    db.file <- db.file2
  } else if(river == 'American River'){
    db.file <- db.file3
  } else if(river == ''){
    db.file <- db.file4
  } else if(river == 'Feather River'){
    db.file <- db.file5
  } else if(river == 'Stanislaus River'){
    db.file <- db.file6
  } else if(river == 'Old American Test'){
    db.file <- db.file7
  } else if(river == 'Mokelumne River'){
    db.file <- db.file8
    #   } else if(river == "Knight's Landing"){
    #     db.file <- db.file9
  } else if(river == "Knight's Landing"){
    db.file <- db.fileA
  }
  
  db.file <<- db.file
  cat(paste0(db.file,"\n"))
  
  #   Check that times are less than 1 year apart
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  run.season <- data.frame( start=strt.dt, end=end.dt )

  nvisits <- F.buildReportCriteria( site, min.date, max.date )
  
  if( nvisits == 0 ){
    warning("Your criteria returned no trapVisit table records.")
    return()
  }
  
  db <- get( "db.file", env=.GlobalEnv ) 
  ch <- odbcConnectAccess(db)  
    F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )               #   This SQL file develops the hours fished and TempSamplingSummary table 
    F.run.sqlFile( ch, "QryNotFishing.sql" )                                #   This SQL generates times when the traps were not fishing
    F.run.sqlFile( ch, "QryUnmarkedByRunLifestage.sql", R.TAXON=taxon )     #   This SQL generates unmarked fish by run and life stage
    catch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final" )              #   Now, fetch the result 
    F.sql.error.check(catch) 
  close(ch) 
  
  if(nrow(catch) == 0){
    warning("Your criteria returned no catch records.  Check to make sure valid Fishing occurred within your date range.")
    stop
  }
  
  catch$river <- river
  catch$site <- site
  
  catch
  
}

ame57000 <- find_gaps("American River"  ,  57000, 161980, '1980-01-01', '2016-03-02')
fea3000  <- find_gaps("Feather River"   ,   3000, 161980, '1980-01-01', '2016-03-02')
fea52000 <- find_gaps("Feather River"   ,  52000, 161980, '1980-01-01', '2016-03-02')
fea5000  <- find_gaps("Feather River"   ,   5000, 161980, '1980-01-01', '2016-03-02')
fea4000  <- find_gaps("Feather River"   ,   4000, 161980, '1980-01-01', '2016-03-02')
fea2000  <- find_gaps("Feather River"   ,   2000, 161980, '1980-01-01', '2016-03-02')
fea6000  <- find_gaps("Feather River"   ,   6000, 161980, '1980-01-01', '2016-03-02')
sac42000 <- find_gaps("Sacramento River",  42000, 161980, '1980-01-01', '2016-03-02')
sta1000  <- find_gaps("Stanislaus River",   1000, 161980, '1980-01-01', '2016-03-02')
mok34000 <- find_gaps("Mokelumne River" ,  34000, 161980, '1980-01-01', '2016-03-02')
kni63000 <- find_gaps("Knight's Landing",  63000, 161980, '1980-01-01', '2016-03-02')

gaps <- rbind(ame57000,fea3000,fea52000,fea5000,fea4000,fea2000,fea6000,sac42000,sta1000,mok34000,kni63000)



table(gaps$TrapStatus)

gapsT <- gaps[gaps$TrapStatus == "Not fishing",]
gapsT$SampleHours <- gapsT$SampleMinutes / 60
gapsT$SampleDays <- gapsT$SampleMinutes / 60 / 24

gapsT <- gapsT[,c('trapPositionID','TrapPosition','SampleDate','StartTime','EndTime','SampleMinutes','SampleHours','SampleDays','siteID','siteName','river')]

gapsT <- gapsT[order(gapsT$SampleDays, decreasing=TRUE),]

gapsT365 <- gapsT[gapsT$SampleDays <= 365,]

gapsT365 <- gapsT365[order(gapsT365$river,gapsT365$siteID,gapsT365$trapPositionID,gapsT365$SampleDate,gapsT365$SampleMinutes),]





traps <- unique(gapsT365$trapPositionID)#[c(1:6)]

png("C:/Users/jmitchell/Desktop/theGaps_DO_NOT_PRINT.png",units="in",width=24,height=120,res=300)
par(mfrow=c(length(traps),6))
for(i in 1:length(traps)){
  
  trap <- traps[i]
  
  # -------- set it up ------------
  df <- gapsT365[gapsT365$trapPositionID == trap,]
  the95 <- quantile(df$SampleMinutes,c(0.95))


  river <- df[1,]$river
  siteName <- df[1,]$siteName
  TrapPosition <- df[1,]$TrapPosition

  dist1 <- df$SampleMinutes
  dist2 <- ecdf(df$SampleDays)
  if(nrow(df) >= 10){
    dist3  <- df[df$trapPositionID == trap & df$SampleMinutes <= the95,]$SampleMinutes
    dist3b <- df[df$trapPositionID == trap & df$SampleMinutes <= 15840,]$SampleMinutes
    dist4  <- ecdf(df[df$trapPositionID == trap & df$SampleMinutes <= the95,]$SampleDays)
  } else {
    dist4 <- dist3 <- "Insufficient Data"
  }
  
  # -------- make the plot --------
  
  plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE,xlab="",ylab=""); u <- par("usr")                                                                        # zero out margins, make empty plot, get bounding box
  text(1,u[3] + 1.1*(u[4]-u[3])/2,river,cex=1)
  text(1,u[3] + 1.0*(u[4]-u[3])/2,siteName,cex=1)
  text(1,u[3] + 0.9*(u[4]-u[3])/2,TrapPosition,cex=1)
  box()
  
  h1 <- hist(dist1,main="Histogram -- All Data",xlab="Minutes",xaxt="n")
  axis(1,at=h1$breaks,formatC(h1$breaks, digits = 0, format = "f",big.mark=",") ) 
  plot(dist2, xaxt="n",verticals = TRUE, main="EDF",col.points = "blue",col.hor = "red", col.vert = "bisque",xlab="Days")
  axis(1,at=pretty(seq(0,max(df$SampleDays)),length.out=10),formatC(pretty(seq(0,max(df$SampleDays)),length.out=10), digits = 0, format = "f",big.mark=",") ) 
  
  if(class(dist3)[1] == "character" | class(dist4)[1] == "character"){
    plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE,xlab="",ylab=""); u <- par("usr")                                                                        # zero out margins, make empty plot, get bounding box
    text(1,u[3] + 1.1*(u[4]-u[3])/2,paste0("Only N=",nrow(df)," points."),cex=1)
    box()
    
    plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE,xlab="",ylab=""); u <- par("usr")                                                                        # zero out margins, make empty plot, get bounding box
    text(1,u[3] + 1.1*(u[4]-u[3])/2,paste0("Only N=",nrow(df)," points."),cex=1)
    box()
    
    plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE,xlab="",ylab=""); u <- par("usr")                                                                        # zero out margins, make empty plot, get bounding box
    text(1,u[3] + 1.1*(u[4]-u[3])/2,paste0("Only N=",nrow(df)," points."),cex=1)
    box()
  } else {
    plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE,xlab="",ylab="")
    par(new=TRUE)
    h <- hist(dist3b,breaks=seq(0,15840,length.out=11*4+1),col="lightgray",xaxt="n",main="Histogram -- 0 to 15,840, with Bin Size = 360 Mins",xlab="Minutes")
    axis(side=1, at=seq(0,15840,length.out=11*2+1), labels=seq(0,15840,length.out=11*2+1))
    par(new=TRUE)
    for(i in 1:11){
      drawEm <- seq(0,15840,length.out=11+1)
      #abline(v=drawEm[i],col="blue")
      segments(drawEm[i],0,drawEm[i],max(h$counts),col="blue",lwd=2)
      text(drawEm[i]+400,max(h$counts),drawEm[i]/1440 + 1,cex=1.5,pos=4)
    }
    

    hist(dist3,breaks=length(dist3)/5,main="Histogram -- 0 to 95th Percentile",xlab="Minutes")
    plot(dist4, xaxt="n",verticals = TRUE, main="EDF -- 0 to 95th Percentile",col.points = "blue",col.hor = "red", col.vert = "bisque",xlab="Days")
    axis(1,at=pretty(seq(0,max(df[df$trapPositionID == trap & df$SampleDays <= the95/60/24,]$SampleDays)),length.out=10),formatC(pretty(seq(0,max(df[df$trapPositionID == trap & df$SampleDays <= the95/60/24,]$SampleDays)),length.out=10), digits = 0, format = "f",big.mark=",") ) 
  }
}
dev.off()
par(mfrow=c(1,1))






