

#' @param obs.eff.df A data frame with at least variables \code{batchDate} and 
#'   \code{efficiency}, where \code{efficiency} is \code{NA} for all days 
#'   requiring an estimate.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#' document me.


getTogetherCovarData <- function(obs.eff.df,min.date,max.date){
  
  # obs.eff.df <- obs.eff.df
  
  #   ---- JASON SETS THIS UP BASED ON UPDATED PACKAGE.
  #   ---- We assemble all the unique ourSiteIDs we need for this run. 
  TestingPlatform <- "CAMP_RST20161212-campR1.0.0"
  luSubSiteID <- read.csv(paste0("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",TestingPlatform,"/R/library/EnvCovDBpostgres/helperfiles/luSubSiteID.csv"))
  xwalk <- luSubSiteID[luSubSiteID$subSiteID %in% attr(obs.eff.df,"subsites")$subSiteID,]
  uniqueOurSiteIDsToQuery <- unique(na.omit(c(xwalk$ourSiteIDChoice1,xwalk$ourSiteIDChoice2)))
  
  df <- vector("list",length(uniqueOurSiteIDsToQuery))
  m1 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  flow (cfs)
  m2 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  temperature (C)
  m3 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  turbidity
  
  
  #   ---- Need to set an initial value for variable covar.  This holds the building string of 
  #   ---- a model statement.  Don't need to worry about if all the values for all efficiency 
  #   ---- trials are actually present -- we deal with that possibility below.  So... just add
  #   ---- the simple text statement.  
  obs.eff.df$covar <- "bdMeanNightProp + bdMeanMoonProp + bdMeanForkLength"
  
  for(ii in 1:length(uniqueOurSiteIDsToQuery)){
    
    #   ---- Get covariate data of interest.  
    oursitevar <- uniqueOurSiteIDsToQuery[ii]   
    minEffDate <- as.character(min(obs.eff.df$batchDate))
    maxEffDate <- as.character(max(obs.eff.df$batchDate))
    
    #   ---- See if we can get anything on tbld.  This is good for local runs.
    #ch <- odbcConnect("PostgreSQL30", uid = 'jmitchell', pwd = 'jmitchell')
    #library(DBI)
    #library(RPostgres)
    
    #   ---- Query the PostgreSQL database for information on temperature and flow.  
    ch <- dbConnect(Postgres(),
                    dbname='EnvCovDB',
                    host='streamdata.west-inc.com',
                    port=5432,
                    user="jmitchell",
                    password="G:hbtr@RPH5M.")
    res <- dbSendQuery(ch,paste0("SELECT COUNT(oursiteid) FROM tbld WHERE ('",min.date,"' <= date AND date <= '",max.date,"') AND oursiteid = ",oursitevar," GROUP BY oursiteid;"))
    nGood <- dbFetch(res)
    dbClearResult(res)
    dbDisconnect(ch)
    
    if(nrow(nGood) > 0){
      df[[ii]] <- queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="D",plot=FALSE)
      df[[ii]]$date <- strptime(df[[ii]]$date,format="%Y-%m-%d",tz=time.zone)
    } else {
      df[[ii]] <- queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="U",plot=FALSE)
      
      if(sum(!is.na(df[[ii]]$flow_cfs)) > 0 & (sum(df[[ii]]$flow_cfs <= -9997 & !is.na(df[[ii]]$flow_cfs)) > 0) ){
        df[[ii]][df[[ii]]$flow_cfs <= -9997 & !is.na(df[[ii]]$flow_cfs),]$flow_cfs <- NA
      }
      if(sum(!is.na(df[[ii]]$temp_c)) > 0 & (sum((df[[ii]]$temp_c >= 100 | df[[ii]]$temp_c <= 0) & !is.na(df[[ii]]$temp_c)) < 0)){
        df[[ii]][(df[[ii]]$temp_c >= 100 | df[[ii]]$temp_c <= 0) & !is.na(df[[ii]]$temp_c),]$temp_c <- NA
      }
    }  
    
    #   ---- Compile the good dates for each metric. 
    min.date.flow <- suppressWarnings(min(df[[ii]][!is.na(df[[ii]]$flow_cfs),]$date))
    max.date.flow <- suppressWarnings(max(df[[ii]][!is.na(df[[ii]]$flow_cfs),]$date))
    min.date.temp <- suppressWarnings(min(df[[ii]][!is.na(df[[ii]]$temp_c),]$date))
    max.date.temp <- suppressWarnings(max(df[[ii]][!is.na(df[[ii]]$temp_c),]$date))
    
    #   ---- Query this river's Access database for information recorded at the trap.  For now, we only use this 
    #   ---- for turbidity.  I name objects that respect this.  
    
    if(ii == 1){
      
      #   ---- 11/20/2017.  Update to run Connie's cleaning query.
      db <- get( "db.file", envir=.GlobalEnv )
      ch <- odbcConnectAccess(db)
      
      #   ---- Develop the TempReportCriteria_TrapVisit table.
      F.buildReportCriteria( site, min.date, max.date )   # was min.date2, max.date2.  matter?
    
      #   ---- Run the clean-up query.
      F.run.sqlFile( ch, "QryCleanEnvCov.sql")#, min.date2, max.date2 )
      
      #   ---- Now, fetch the result.  
      dbCov <- sqlFetch( ch, "EnvDataRaw_Standardized" )
      
      close(ch)
      
      #   ---- Make a dataframe for what we have.  
      dbDisc <- getCAMPEnvCov(dbCov,"discharge","dischargeUnitID",12)
      dbDpcm <- getCAMPEnvCov(dbCov,"waterDepth","waterDepthUnitID",3)
      dbATpF <- getCAMPEnvCov(dbCov,"airTemp","airTempUnitID",19)
      dbTurb <- getCAMPEnvCov(dbCov,"turbidity","turbidityUnitID",20)
      dbWVel <- getCAMPEnvCov(dbCov,"waterVel","waterVelUnitID",8)
      dbWTpC <- getCAMPEnvCov(dbCov,"waterTemp","waterTempUnitID",18) 
      dbLite <- getCAMPEnvCov(dbCov,"lightPenetration","lightPenetrationUnitID",3)
      #dbDOxy <- getCAMPEnvCov(dbCov,"dissolvedOxygen","dissolvedOxygenUnitID",36)         
      #dbCond <- getCAMPEnvCov(dbCov,"conductivity","conductivityUnitID",36)
      #dbBaro <- getCAMPEnvCov(dbCov,"barometer","barometerUnitID",33)    
      #dbWeat <- getCAMPEnvCov(dbCov,"weather",NA,NA)
      
      
      #   ---- Put all database covariates into a list for easier processing.
      dbCovar <- list(dbDisc,dbDpcm,dbATpF,dbTurb,dbWVel,dbWTpC,dbLite)#,dbDOxy,dbCond,dbBaro,dbWeat)
      
      #   ---- Collapse all the UnitIDs we have. 
      dfUnitIDs <- NULL
      for(i in 1:length(dbCovar)){
        l <- length(attr(dbCovar[[i]],"uniqueUnitID"))
        if(l > 0){
          dfUnitIDs.i <- data.frame("site"=rep(site,l),"covar"=rep(attr(dbCovar[[i]],"cov"),l),"UnitID"=attr(dbCovar[[i]],"uniqueUnitID"))
          dfUnitIDs <- rbind(dfUnitIDs,dfUnitIDs.i)
        }
      }
      
      #   ---- Compile unique UnitIDs per covar. 
      dfUnitIDs <- unique(dfUnitIDs)
      rownames(dfUnitIDs) <- NULL
      
      dfUnitIDs$test <- NA
      for(i in 1:nrow(dfUnitIDs)){
        if(i == 1){
          dfUnitIDs[i,]$test <- dfUnitIDs[i,]$UnitID
        } else if(dfUnitIDs[i,]$covar != dfUnitIDs[i - 1,]$covar){
          dfUnitIDs[i,]$test <- paste0(dfUnitIDs[i,]$UnitID," ")
        } else {
          dfUnitIDs[i,]$test <- paste0(dfUnitIDs[i - 1,]$test,dfUnitIDs[i,]$UnitID,sep=" ")
        }
      }
      
      dfUnitIDs <- aggregate(dfUnitIDs,list(dfUnitIDs$covar),function(x) tail(x,1))
      dfUnitIDs$Group.1 <- dfUnitIDs$UnitID <- dfUnitIDs$site <- NULL
      
      # #   ---- Read in how to map unstandardized weather values to standardized values.  Put this in as data.frame...eventually. 
      # weaMap <- read.csv("//LAR-FILE-SRV/Data/PSMFC_CampRST/felipe products/variables/weather/weatherLookupMapped20170720.csv")
      # #weaKey <- weaMap[1:4,c("PrecipLevel","PrecipLevelText")]
      # dbWeat <- merge(dbWeat,weaMap[,c("weather","precipLevel")],by=c("weather"),all.x=TRUE)
      # dbWeat <- dbWeat[,c("subSiteID","measureDate","precipLevel")]
      # names(dbWeat)[names(dbWeat) == "precipLevel"] <- "precipLevel"
    }
    
    #   ---- Fit a simple smoothing spline and predict.  First for temp and flow from the EnvCovDB.   
    
    covar <- NULL
    dontDo <- FALSE
    if(sum(!is.na(df[[ii]]$flow_cfs)) > 0){
      m1[[ii]] <- smooth.spline(df[[ii]][!is.na(df[[ii]]$flow_cfs),]$date,df[[ii]][!is.na(df[[ii]]$flow_cfs),]$flow_cfs,cv=TRUE)
      
      if("covar" %in% names(obs.eff.df)){
        if(is.na(obs.eff.df$covar[1])){
          obs.eff.df$covar <- paste0("flow_cfs")
        } else if(!("flow_cfs" %in% names(obs.eff.df))) {
          obs.eff.df$covar <- paste0(obs.eff.df$covar," + flow_cfs")
        } else {
          dontDo <- TRUE       #  <---- In this case, we already have this covariate from a previous ii run.  
        }
      } else {
        obs.eff.df$covar <- "flow_cfs"
      }
      
      if(dontDo == FALSE){
        obs.eff.df$flow_cfs <- NA
      
        #   ---- Not the best, but works with two possible IDChoices.  
        if(ii == 1){
          obs.eff.df[obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice1 == oursitevar,]$subSiteID,]$flow_cfs <- predict(m1[[ii]],as.numeric(obs.eff.df$batchDate))$y       
        } else if(ii == 2){
          obs.eff.df[obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice2 == oursitevar,]$subSiteID,]$flow_cfs <- predict(m1[[ii]],as.numeric(obs.eff.df$batchDate))$y        
        }
        
        #df[[ii]]$pred_flow_cfs <- predict(m1[[ii]])$y
        df[[ii]]$pred_flow_cfs <- predict(m1[[ii]],x=as.numeric(df[[ii]]$date))$y
        
        #    ---- See if we have any predicted values outside the range for which we have data.
        if(sum(df[[ii]]$date < min.date.flow | df[[ii]]$date > max.date.flow) > 0){
          df[[ii]][df[[ii]]$date < min.date.flow | df[[ii]]$date > max.date.flow,]$pred_flow_cfs <- NA
        }
        
        #   ---- Build a dataframe like the CAMP covariates.  If we're running against the unit table, we have no statistic.  
        #   ---- For flow, call it 450L, for temp, 451L.
        if(!(nrow(nGood) > 0)){
          dbFlPG <- data.frame(subSiteID=NA,measureDate=df[[ii]]$date,flow_cfs=df[[ii]]$flow_cfs,flow_cfsUnitID=rep(450L,nrow(df[[ii]])))
        } else {
          dbFlPG <- data.frame(subSiteID=NA,measureDate=df[[ii]]$date,flow_cfs=df[[ii]]$flow_cfs,flow_cfsUnitID=df[[ii]]$flow_statistic)
        }
        
        #    ---- See if we have any predicted values outside the range for which we have data.  Off by a day..?  Daylight savings?  So buffer.
        if(sum(obs.eff.df$batchDate + 60*60 < min.date.flow | obs.eff.df$batchDate - 60*60 > max.date.flow) > 0){
          obs.eff.df[obs.eff.df$batchDate + 60*60 < min.date.flow | obs.eff.df$batchDate - 60*60 > max.date.flow,]$flow_cfs <- NA
        }
      }
    }
    
    dontDo <- FALSE
    if(sum(!is.na(df[[ii]]$temp_c)) > 0){
      m2[[ii]] <- smooth.spline(as.numeric(df[[ii]][!is.na(df[[ii]]$temp_c),]$date),df[[ii]][!is.na(df[[ii]]$temp_c),]$temp_c,cv=TRUE)
      
      if("covar" %in% names(obs.eff.df)){
        if(is.na(obs.eff.df$covar[1])){
          obs.eff.df$covar <- paste0("temp_c")
        } else if(!("temp_c" %in% names(obs.eff.df))) {
          obs.eff.df$covar <- paste0(obs.eff.df$covar," + temp_c")
        } else {
          dontDo <- TRUE       #  <---- In this case, we already have this covariate from a previous ii run.  
        }
      } else {
        obs.eff.df$covar <- "temp_c"
      }
      
      if(dontDo == FALSE){
        obs.eff.df$temp_c <- NA
        if(ii == 1){
          obs.eff.df[obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice1 == oursitevar,]$subSiteID,]$temp_c <- predict(m2[[ii]],as.numeric(obs.eff.df$batchDate))$y       
        } else if(ii == 2){
          obs.eff.df[obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice2 == oursitevar,]$subSiteID,]$temp_c <- predict(m2[[ii]],as.numeric(obs.eff.df$batchDate))$y       
        }
        
        #df[[ii]]$pred_temp_c <- predict(m2[[iii]])$y
        df[[ii]]$pred_temp_c <- predict(m2[[ii]],x=as.numeric(df[[ii]]$date))$y
        
        #    ---- See if we have any predicted values outside the range for which we have data.
        if(sum(df[[ii]]$date < min.date.temp | df[[ii]]$date > max.date.temp) > 0){
          df[[ii]][df[[ii]]$date < min.date.temp | df[[ii]]$date > max.date.temp,]$pred_temp_c <- NA
        }
        
        #   ---- Build a dataframe like the CAMP covariates.  If we're running against the unit table, we have no statistic.  
        #   ---- For flow, call it 450L, for temp, 451L.  Recall that oursitevar >= 80 means EnvCovDB from unit table is used.
        if(!(nrow(nGood) > 0)){
          dbTpPG <- data.frame(subSiteID=NA,measureDate=df[[ii]]$date,temp_c=df[[ii]]$temp_c,temp_cUnitID=451L)       
        } else {
          dbTpPG <- data.frame(subSiteID=NA,measureDate=df[[ii]]$date,temp_c=df[[ii]]$temp_c,temp_cUnitID=df[[ii]]$temp_statistic)       
        }
        
        #   ---- See if we have any predicted values outside the range for which we have data.  Off by a day..?  Daylight savings?  So buffer.
        if(sum(obs.eff.df$batchDate + 60*60 < min.date.temp | obs.eff.df$batchDate - 60*60 > max.date.temp) > 0){
          obs.eff.df[obs.eff.df$batchDate + 60*60 < min.date.temp | obs.eff.df$batchDate - 60*60 > max.date.temp,]$temp_c <- NA
        }
      }
    }
    
    #   ---- Next for data from the CAMP db, which could be collected per subSiteID.  Reduce to the set of subsiteIDs in this run.  This 
    #   ---- is necessary for the Feather, which has many sites that branch into individual subSiteIDs.  Do this for each covar.
    
    #dbCov <- dbCovar[[ii]]
    
    #   ---- Now, bring in smoothing-spline estimated values.  All Units summarized in SQL QryCleanEnvCov.sql.
    if(ii == 1){
      obs.eff.df <- estCovar(dbDisc,"discharge_cfs",1,traps,obs.eff.df,xwalk,oursitevar)
      obs.eff.df <- estCovar(dbDpcm,"waterDepth_cm",1,traps,obs.eff.df,xwalk,oursitevar)
      obs.eff.df <- estCovar(dbATpF,"airTemp_F",1,traps,obs.eff.df,xwalk,oursitevar)
      obs.eff.df <- estCovar(dbTurb,"turbidity_ntu",1,traps,obs.eff.df,xwalk,oursitevar)
      obs.eff.df <- estCovar(dbWVel,"waterVel_fts",1,traps,obs.eff.df,xwalk,oursitevar)
      obs.eff.df <- estCovar(dbWTpC,"waterTemp_C",1,traps,obs.eff.df,xwalk,oursitevar)
      obs.eff.df <- estCovar(dbLite,"lightPenetration_cm",1,traps,obs.eff.df,xwalk,oursitevar)
      #obs.eff.df <- estCovar(dbDOxy,"dissolvedOxygen_mgL",1,traps,obs.eff.df,xwalk,oursitevar)        
      #obs.eff.df <- estCovar(dbCond,"conductivity_mgL",1,traps,obs.eff.df,xwalk,oursitevar)
      #obs.eff.df <- estCovar(dbBaro,"barometer_inHg",1,traps,obs.eff.df,xwalk,oursitevar)    
      #obs.eff.df <- estCovar(dbWeat,"precipLevel_qual",2,traps,obs.eff.df,xwalk,oursitevar)
    }
    
    obs.eff.df <- obs.eff.df[order(obs.eff.df$TrapPositionID,obs.eff.df$batchDate),]
    
  }
  
  return(list(obs.eff.df=obs.eff.df,dbDisc=dbDisc,dbDpcm=dbDpcm,dbATpF=dbATpF,dbTurb=dbTurb,dbWVel=dbWVel,dbWTpC=dbWTpC,dbLite=dbLite,dbFlPG=dbFlPG,dbTpPG=dbTpPG))
}