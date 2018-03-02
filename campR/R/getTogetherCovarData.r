#' @export
#' 
#' @title getTogetherCovarData
#'   
#' @description Put together available covariate data from both the external
#'   Environmental Covariate Database, as well as CAMP-collected environmental
#'   variables.
#'   
#' @param obs.eff.df A data frame with at least variables \code{batchDate} and
#'   \code{efficiency}, where \code{efficiency} is \code{NA} for all days 
#'   requiring an estimate.
#'   
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#'   
#' @param max.date The end date for data to include.  Same format as 
#' \code{min.date}.
#' 
#' @param traps A set of traps for which efficiency data are available.  
#' 
#' @param useEnhEff A logical indicating if function \code{getTogetherCovarData}
#'   should use query dates based on provided \code{min.date} and
#'   \code{max.date} (\code{TRUE}), or efficency time frames (\code{FALSE}).
#'   
#' @return Several items, all housed within a list.  The main piece of output is 
#' data frame \code{obs.eff.df}, which contains the same number of rows as the 
#' \code{obs.eff.df} submitted to the function, but with several additional 
#' columns, due to appended covariates.  
#' 
#' \describe{
#'   \item{obs.eff.df}{Same as submitted to the function, but with covariate columns added.}
#'   \item{dbDisc}{Available CAMP discharge data.}
#'   \item{dbDpcm}{Available CAMP water-depth data.}
#'   \item{dbATpF}{Available CAMP air-temperature data.}
#'   \item{dbTurb}{Available CAMP turbidity data.}
#'   \item{dbWVel}{Available CAMP water-velocity data.}
#'   \item{dbWTpC}{Available CAMP water-temperature data.}
#'   \item{dbLite}{Available CAMP light-penetration discharge data.}
#'   \item{dbFlPG}{Available Environmental Covariate Database discharge data.}
#'   \item{dbTpPG}{Available Environemtnal Covariate Database water temperature data.}
#' }
#'   
#' @details Function \code{getTogetherCovarData} appends covariate information 
#'   for each unique \code{batchDate} and \code{TrapPositionID} combination 
#'   avaiable in data frame \code{obs.eff.df}.
#'   
#'   Environmental covariate data are queried from the WEST-supported database
#'   created for housing USGS and CDEC data at sites important to the CAMP
#'   program.
#'   
#'   Other covariates derive directly from data stored within a particular
#'   river's CAMP mdb database.  Prior to use in efficiency models, data are
#'   first converted to standardized units via the \code{QryCleanEnvCov} SQL
#'   query sequence in function \code{getCAMPEnvCov}, which is called once 
#'   for each covariate.    
#'   
#'   Following querying, all covariates are then fit with a smoothing spline via
#'   function \code{estCovar}, with penalization parameter \eqn{\lambda}
#'   selected via cross-validation. Smoothing occurs to ensure easy fitting of
#'   efficiency models, whose temporal resolution is that of a \code{batchDate}.
#'   
#' @seealso \code{getCAMPEnvCov}, \code{estCovar}
#' 
#' @references Hastie, T., Tibshirani, R., and Friedman, J. 2009.  The Elements
#'   of Statistical Learning.  2nd Edition.  Springer, New York, New York.
#'   
#' @examples  
#' \dontrun{
#' ans <- getTogetherCovarData(obs.eff.df,
#'                             min.date,
#'                             max.date,
#'                             traps,
#'                             useEnhEff)
#' }
getTogetherCovarData <- function(obs.eff.df,min.date,max.date,traps,useEnhEff){
  
  # obs.eff.df <- obs.eff.df
  # min.date <- min.date2
  # max.date <- max.date2
  # traps <- traps
  # useEnhEff <- TRUE
  
  #   ---- Obtain necessary variables from the global environment.  
  time.zone <- get("time.zone",envir=.GlobalEnv)
  
  #   ---- Get from dataframe. 
  site <- attr(obs.eff.df,"site")
  
  #   ---- JASON SETS THIS UP BASED ON UPDATED PACKAGE.
  #   ---- We assemble all the unique ourSiteIDs we need for this run. 
  #TestingPlatform <- "CAMP_RST20161212-campR1.0.0"
  #luSubSiteID <- read.csv(paste0("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",TestingPlatform,"/R/library/EnvCovDBpostgres/helperfiles/luSubSiteID.csv"))
  luSubSiteID <- read.csv(paste0(find.package("EnvCovDBpostgres"),"/helperFiles/luSubSiteID.csv"))
  xwalk <- luSubSiteID[luSubSiteID$subSiteID %in% attr(obs.eff.df,"subsites")$subSiteID,]
  uniqueOurSiteIDsToQuery <- unique(na.omit(c(xwalk$ourSiteIDChoice1,xwalk$ourSiteIDChoice2,xwalk$ourSiteIDChoice3)))
  
  df <- vector("list",length(uniqueOurSiteIDsToQuery))
  m1 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  flow (cfs)
  m2 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  temperature (C)
  m3 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  turbidity
  
  
  #   ---- Need to set an initial value for variable covar.  This holds the building string of 
  #   ---- a model statement.  Don't need to worry about if all the values for all efficiency 
  #   ---- trials are actually present -- we deal with that possibility below.  So... just add
  #   ---- the simple text statement.  But if we're calling this function for real passage 
  #   ---- estimation, we don't want this.  
  #if( sum(c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength") %in% names(obs.eff.df)) == 3 ){
    obs.eff.df$covar <- "bdMeanNightProp + bdMeanMoonProp + bdMeanForkLength"
  #}
  
  for(ii in 1:length(uniqueOurSiteIDsToQuery)){
    
    #   ---- Get covariate data of interest.  
    oursitevar <- uniqueOurSiteIDsToQuery[ii]   
    
    if(useEnhEff == TRUE){
      
      #   ---- Use these when constructing passage estimates.  Buff out a month each way.
      minEffDate <- as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone) - 90*24*60*60
      maxEffDate <- as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone) + 90*24*60*60
    } else {
      
      #   ---- Use these when building enhanced efficiency trials.  
      minEffDate <- as.character(min(obs.eff.df$batchDate))
      maxEffDate <- as.character(max(obs.eff.df$batchDate))
    }
    
    #   ---- See if we can get anything on tbld.  This is good for local runs.
    #ch <- odbcConnect("PostgreSQL30", uid = 'jmitchell', pwd = 'jmitchell')
    #library(DBI)
    #library(RPostgres)
    
    #   ---- Query the PostgreSQL database for information on temperature and flow.  
    ch <- dbConnect(RPostgres::Postgres(),    
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
      df[[ii]] <- EnvCovDBpostgres::queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="D",plot=FALSE)
      df[[ii]]$date <- strptime(df[[ii]]$date,format="%Y-%m-%d",tz=time.zone)
    } else {
      df[[ii]] <- EnvCovDBpostgres::queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="U",plot=FALSE)
      
      if(sum(!is.na(df[[ii]]$flow_cfs)) > 0 & (sum(df[[ii]]$flow_cfs <= -9997 & !is.na(df[[ii]]$flow_cfs)) > 0) ){
        df[[ii]][df[[ii]]$flow_cfs <= -9997 & !is.na(df[[ii]]$flow_cfs),]$flow_cfs <- NA
      }
      if(sum(!is.na(df[[ii]]$temp_c)) > 0){
        
        #   ---- This is mostly due to weird CDEC data.  
        
        #   ---- If we have temps less than -17.8F, then the Celsius value was 0.00.  Chuck these -- they're probably bad values. 
        #   ---- The value of 37.7778 corresponds to 100F.  
        if( any( (df[[ii]][!is.na(df[[ii]]$temp_c),]$temp_c >= 37.7778) | (df[[ii]][!is.na(df[[ii]]$temp_c),]$temp_c <= -17.8) ) ){
          df[[ii]][!is.na(df[[ii]]$temp_c) & ( df[[ii]]$temp_c >= 37.7778 | df[[ii]]$temp_c <= -17.8 ),]$temp_c <- NA
        }
        
        #   ---- They seem to use 32.0 as a substitution value as well.  Weird.  Get rid of those.  
        if( any( (df[[ii]][!is.na(df[[ii]]$temp_c),]$temp_c == 0.0) ) ){
          df[[ii]][!is.na(df[[ii]]$temp_c) & df[[ii]]$temp_c == 0.0,]$temp_c <- NA
        }
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
      cat(paste0("2:  site is ",site,".\n"))
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
      if(substr(obs.eff.df$TrapPositionID[1],1,2) != "42"){
        dbTurb <- getCAMPEnvCov(dbCov,"turbidity","turbidityUnitID",20)
      }
      dbWVel <- getCAMPEnvCov(dbCov,"waterVel","waterVelUnitID",8)
      dbWTpC <- getCAMPEnvCov(dbCov,"waterTemp","waterTempUnitID",18) 
      dbLite <- getCAMPEnvCov(dbCov,"lightPenetration","lightPenetrationUnitID",3)
      #dbDOxy <- getCAMPEnvCov(dbCov,"dissolvedOxygen","dissolvedOxygenUnitID",36)         
      #dbCond <- getCAMPEnvCov(dbCov,"conductivity","conductivityUnitID",36)
      #dbBaro <- getCAMPEnvCov(dbCov,"barometer","barometerUnitID",33)    
      #dbWeat <- getCAMPEnvCov(dbCov,"weather",NA,NA)
      
      #   ---- Put all database covariates into a list for easier processing.
      if(substr(obs.eff.df$TrapPositionID[1],1,2) != "42"){
        dbCovar <- list(dbDisc,dbDpcm,dbATpF,dbTurb,dbWVel,dbWTpC,dbLite)#,dbDOxy,dbCond,dbBaro,dbWeat)
      } else {
        dbCovar <- list(dbDisc,dbDpcm,dbATpF,dbWVel,dbWTpC,dbLite)#,dbDOxy,dbCond,dbBaro,dbWeat)
      }
      
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
    } else if(!exists("dbFlPG")){
      dbFlPG <- NULL
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
    } else if(!exists("dbTpPG")){
      dbTpPG <- NULL
    }
    
    #   ---- Next for data from the CAMP db, which could be collected per subSiteID.  Reduce to the set of subsiteIDs in this run.  This 
    #   ---- is necessary for the Feather, which has many sites that branch into individual subSiteIDs.  Do this for each covar.
    
    #dbCov <- dbCovar[[ii]]
    
    #   ---- Now, bring in smoothing-spline estimated values.  All Units summarized in SQL QryCleanEnvCov.sql.
    if(ii == 1){
      obs.eff.df <- estCovar(dbDisc,"discharge_cfs",1,traps,obs.eff.df,xwalk,oursitevar)
      obs.eff.df <- estCovar(dbDpcm,"waterDepth_cm",1,traps,obs.eff.df,xwalk,oursitevar)
      obs.eff.df <- estCovar(dbATpF,"airTemp_F",1,traps,obs.eff.df,xwalk,oursitevar)
      if(substr(obs.eff.df$TrapPositionID[1],1,2) != "42"){
        obs.eff.df <- estCovar(dbTurb,"turbidity_ntu",1,traps,obs.eff.df,xwalk,oursitevar)
      }
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

    
  #   ---- In the case of the RBDD, swap out flow_cfs for percQ.  Note empty dbperQ has Date
  #   ---- instead of POSIXct.  Don't think this matters.  
  dbPerQ <- data.frame(subSiteID=integer(),measureDate=as.Date(character()),percQ=numeric(),percQUnitID=integer(),stringsAsFactors=FALSE)
  if( site == 42000 ){
    dbPerQ <- percQ(hrflow=df[[2]])
    
    #   ---- The obs.eff.df batchDate is off by 8 hours; i.e., it's 8 hours earlier than what it should be.  This could be due 
    #   ---- to not setting tz = "UTC", which maybe up to now hasn't mattered.  I do not know how DST messes with this "8"-hour 
    #   ---- difference.  To avoid that headache, merge on a Date type instead.  
    obs.eff.df$tmpDate <- as.Date(obs.eff.df$batchDate)
    obs.eff.df$subSiteID <- as.numeric(levels(obs.eff.df$TrapPositionID))[obs.eff.df$TrapPositionID]
    
    dbPerQ2 <- dbPerQ
    names(dbPerQ)[names(dbPerQ) == "measureDate"] <- "batchDate"
    dbPerQ$tmpDate <- as.Date(dbPerQ$batchDate)

    obs.eff.df <- merge(obs.eff.df,dbPerQ[,c("tmpDate","subSiteID","percQ")],by=c("tmpDate","subSiteID"),all.x=TRUE)
    obs.eff.df$tmpDate <- obs.eff.df$subSiteID <- NULL
    
    #   ---- Put these back.
    names(dbPerQ)[names(dbPerQ) == "batchDate"] <- "measureDate"
    names(dbPerQ)[names(dbPerQ) == "TrapPositionID"] <- "subSiteID"
    
    #   ---- Adjust the covar variable in obs.eff.df.  Note that covar should have flow_cfs, because the RBDD should always 
    #   ---- have flow.  Can this break if they request a super short min.date and max.date?
    flowPresent <- grepl("flow_cfs",obs.eff.df$covar,fixed=TRUE)
    obs.eff.df[flowPresent,]$covar <- gsub("flow_cfs","percQ",obs.eff.df$covar[flowPresent],fixed=TRUE)
    
    #   ---- Always true?  Remove flow_cfs.  
    if( "flow_cfs" %in% names(obs.eff.df) ){
      obs.eff.df$flow_cfs <- NULL
    }
    
  }
  
  if(substr(obs.eff.df$TrapPositionID[1],1,2) == "42"){
    return(list(obs.eff.df=obs.eff.df,dbDisc=dbDisc,dbDpcm=dbDpcm,dbATpF=dbATpF,              dbWVel=dbWVel,dbWTpC=dbWTpC,dbLite=dbLite,dbFlPG=dbFlPG,dbTpPG=dbTpPG,dbPerQ=dbPerQ))
  } else {
    return(list(obs.eff.df=obs.eff.df,dbDisc=dbDisc,dbDpcm=dbDpcm,dbATpF=dbATpF,dbTurb=dbTurb,dbWVel=dbWVel,dbWTpC=dbWTpC,dbLite=dbLite,dbFlPG=dbFlPG,dbTpPG=dbTpPG,dbPerQ=dbPerQ))
  }
}





