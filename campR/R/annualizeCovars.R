




# estimate annual covariates.  annual as defined by the big looper theExcel.

annualizeCovars <- function(site,min.date,max.date,season){
  
  # site <- site
  # min.date <- min.date
  # max.date <- max.date
  # season <- theSeason

  #   ---- Connect to database. 
  ch <- odbcConnectAccess(db.file)
  
  #   ---- Develop the TempReportCriteria_TrapVisit table.
  F.buildReportCriteria( site, min.date, max.date )
    
  #   ---- Run the clean-up query.
  F.run.sqlFile( ch, "QryCleanEnvCov.sql" )
    
  #   ---- Now, fetch the result.  
  dbCov <- sqlFetch( ch, "EnvDataRaw_Standardized" )
  
  #   ---- While we're in here, get mapping if sites to subSites.
  sitesXwalk <- sqlQuery( ch, "SELECT siteID, subSiteID FROM SubSite;" )
    
  close(ch)
    
  #   ---- Calculate the average for each variable from CAMP mdbs.  
  avg <- data.frame(site=site,
                    Season=theSeason,
                    dischargeMean=mean(na.omit(dbCov$discharge)),
                    waterDepthMean=mean(na.omit(dbCov$waterDepth)),
                    waterVelMean=mean(na.omit(dbCov$waterVel)),
                    airTempMean=mean(na.omit(dbCov$airTemp)),
                    waterTempMean=mean(na.omit(dbCov$waterTemp)),
                    lightPenetrationMean=mean(na.omit(dbCov$lightPenetration)),
                    turbidityMean=mean(na.omit(dbCov$turbidity)),
                    dissolvedOxygenMean=mean(na.omit(dbCov$dissolvedOxygen)),
                    conductivityMean=mean(na.omit(dbCov$conductivity)),
                    baromenterMean=mean(na.omit(dbCov$barometer)))
  
  
  
  #   ---- We assemble all the unique ourSiteIDs we need for this run.  In this application, I assume that 
  #   ---- I can query once (via information on a subSiteID), as being representative for the site in question. 
  #   ---- This works because in luSubSiteID, ourSiteIDChoice1, etc. does not vary within a particular site.  
  luSubSiteID <- read.csv(paste0(find.package("EnvCovDBpostgres"),"/helperFiles/luSubSiteID.csv"))
  xwalk <- luSubSiteID[luSubSiteID$subSiteID %in% sitesXwalk$subSiteID,]  
  uniqueOurSiteIDsToQuery <- unique(na.omit(c(xwalk$ourSiteIDChoice1,xwalk$ourSiteIDChoice2,xwalk$ourSiteIDChoice3)))
  
  df <- vector("list",length(uniqueOurSiteIDsToQuery))
  m1 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  flow (cfs)
  m2 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  temperature (C)
  
  #   ---- This is nearly verbatim of code in function getTogetherCovarData.  
  for(ii in 1:length(uniqueOurSiteIDsToQuery)){
    
    #   ---- Get covariate data of interest.  
    oursitevar <- uniqueOurSiteIDsToQuery[ii]   
    
    #   ---- Use these when building enhanced efficiency trials.  
    minEffDate <- as.character(min.date)
    maxEffDate <- as.character(max.date)
    
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
    
    #   ---- If we have flow this go-around, calculate its mean.  
    if(!is.na(min.date.flow) & !is.na(max.date.flow)){
      m1[[ii]] <- mean(na.omit(df[[ii]]$flow_cfs))
    }
    
    #   ---- If we have temp this go-around, calculate its mean.  
    if(!is.na(min.date.temp) & !is.na(max.date.temp)){
      m2[[ii]] <- mean(na.omit(df[[ii]]$temp_c))
    }
    
  }

  #   ---- Now that we're done querying the EnvCovDBpostgres, take as 'the average,' the value from
  #   ---- the lowest list number; e.g., take m1[[1]] over m2[[2]], if both exist.  As of March 2018,
  #   ---- there could be up to three distinct calculated values, because the xwalk above allows up
  #   ---- to three distinct station queries, per subSiteID.
  ii.flow <- 1
  repeat{
    if(!is.null(m1[[ii.flow]]) | ii.flow == length(uniqueOurSiteIDsToQuery)){
      dischargeEnvCovMean <- m1[[ii.flow]]
      break()
    } else {
      ii.flow <- ii.flow + 1
    }
  }
  
  #   ---- Make sure we have something in the case we have no good flow.  
  if(is.null(dischargeEnvCovMean)){
    dischargeEnvCovMean <- NaN
  }
  
  ii.temp <- 1
  repeat{
    if(!is.null(m2[[ii.temp]]) | ii.temp == length(uniqueOurSiteIDsToQuery)){
      tempEnvCovMean <- m2[[ii.temp]]
      break()
    } else {
      ii.temp <- ii.temp + 1
    }
  }
  
  #   ---- Make sure we have something in the case we have no good temp.  
  if(is.null(tempEnvCovMean)){
    tempEnvCovMean <- NaN
  }
  
  avg2 <- data.frame(site=site,
                     Season=theSeason,
                     dischargeEnvCovMean=dischargeEnvCovMean,
                     tempEnvCovMean=tempEnvCovMean)
    
    
  all <- merge(avg,avg2,by=c("site","Season"))  
    
  #   ---- Calculate an estimated mean percent Q, for this trap.  Should probably only work 
  #   ---- on the RBDD.  
  
  #   ----  water vel     * area *  half-cone * multipliers for adjustments
  #   ----                         (assume no)  
  q_d <- all$waterVelMean * 24.6 *      1      / 43650 * 86400 
    
  #   ----  daily total stream Q * multipliers for adjustments
  Q_D <- all$dischargeEnvCovMean * 86400 / 43560
  
  #   ---- Calculate percent Q.
  all$percQ <- q_d / Q_D
  
  
  return(all)
}