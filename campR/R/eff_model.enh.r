#' @export
#' 
#' @title F.efficiency.model
#'   
#' @description Estimate the efficiency model from a data frame containing 
#' efficiency trials.
#'   
#' @param obs.eff.df A data frame with at least variables \code{batchDate} and 
#'   \code{efficiency}, where \code{efficiency} is \code{NA} for all days 
#'   requiring an estimate.
#' @param plot A logical indicating if raw efficiencies and the model(s)
#' should be plotted.
#' @param max.df.spline The maximum degrees of freedom allowed for splines.
#' @param plot.file The name of the file prefix under which output is to be 
#'   saved.  Set to \code{NA} to plot to the plot window.
#'   
#' @return A data frame with all observed and imputed \code{efficiency} values, 
#'   where variable \code{gam.estimated} identifies days with imputed values.
#' 
#' @section Efficiency model method:
#'
#'   
#'   \itemize{ 
#'   \item{Less than \code{eff.min.spline.samp.size} trials : 
#'   A "weighted average constant model with bias correction."  This model 
#'   uses constant efficiency over the season, and estimates it 
#'   using a ratio-of-means bias-corrected ("ROM+1") average. For each 
#'   trap, estimated efficiency is \deqn{\frac{(\sum nCaught)
#'   + 1}{(\sum nReleased) + 1}{(sum(nCaught) + 1) / (sum(nReleased) + 1)}}.  Values
#'   for \code{nCaught} and \code{nReleased} come from function
#'   \code{F.get.releases}. }
#'   
#'   
#'   \item{\code{eff.min.spline.samp.size} trials or more : A "B-spline model." 
#'   This model starts by estimating a constant logistic regression where
#'   recaptures (i.e., \code{nCaught}) is the number of "successes" and releases
#'   (i.e.,  \code{nReleased}) is number of "trials". Assuming this constant
#'   model is successful, the method estimates a series of increasingly complex
#'   b-spline logistic regression models until AIC is minimized or model
#'   estimation fails (failure to converge or estimates at boundary). B-spline
#'   models, in general, divide the date range into intervals by adding 'knots'.
#'   Between 'knots', b-spline models fit cubic polynomials in a way that 
#'   connects smoothly at knots (refer to b-spline methods for details).
#'   
#'   The first (lowest order) b-spline model fitted contains zero knots and
#'   therefore estimates a cubic model. Assuming that model was successful and
#'   that AIC improved relative to the constant model, the method adds one knot
#'   at the median date and re-estimates. If that model was successful and AIC
#'   improved relative to the previous model, the method adds another knot at 
#'   the (1/(knots+1))-th quantiles of date and re-estimates.  The method 
#'   containues to add knots until one or more of the following conditions
#'   happen: (1) AIC does not improve, (2) estimation fails somehow, or (3) the
#'   maximum number of knots (i.e., \code{max.df.spline-3}) is fitted.
#'   
#'   Using the default value of \code{max.df.spline}, the efficiency model is
#'   either constant (intercept-only), cubic, or b-spline with one interval
#'   knot.
#'   
#'   When the best logistic regression model is constant (intercept-only), 
#'   estimated efficiency is the ratio-of-means estimator WITHOUT the "+1" bias
#'   correction.  With many efficiency trial, the "+1" bias correction is tiny
#'   and inconsequential. The exact efficiency model used at each subsite is
#'   listed in the campR log file.
#'   
#'   The \eqn{\beta}s from the final logistic regression are saved for use in 
#'   bootstrapping by function \code{F.boostrap.passage}.  Modeled efficiencies 
#'   are used for all days, even if a particular day contained an efficiency
#'   trial.
#'   
#'   All dates outside the efficiency trial season use the mean of estimates
#'   within the season.  This means the efficiency model can vary within a
#'   season, but is always constant before the first and after the last
#'   efficiency trial.}
#'   
#'   }
#'   
#' @seealso \code{F.get.releases}, \code{F.bootstrap.passage}
#' 
#' @author WEST Inc.
#' 
#' @examples 
#' \dontrun{
#' #   ---- Fit an efficiency model for each unique trapPositionID 
#' #   ---- in data frame obs.eff.df.  
#' F.enh.efficiency.model( obs.eff.df, plot=T, max.df.spline=4, plot.file=NA)
#' }
F.efficiency.model.enh <- function( obs.eff.df, plot=T, max.df.spline=4, plot.file=NA ){
  
  # obs.eff.df <- eff
  # plot <- plot
  # max.df.spline <- 4
  # plot.file <- plot.file
  
  option <- 2
  
  
  
  #   ---- Read in table of fishing seasons.  These are derived from taking the minimum 
  #   ---- start period of fishing, based on month and date, for all fishing seasons 
  #   ---- recorded in "theExcel" used for Big-Looping.  Similar logic applies for the
  #   ---- end period of fishing, where the maximum is used.  Doug and Connie provided
  #   ---- these dates, and should serve as seasons to "bracket" efficiency trials 
  #   ---- for modeling.  
  
  #   ---- I want to use POSIX for these, so in "theExcel," I code these dates with 
  #   ---- respect to Jan 1, 1970.  I need a year for POSIX.  So, for example, the 
  #   ---- American, over year 2013-2016, has an earliest fishing start date of 
  #   ---- 12/31, with the latest end to fishing on 7/1.  I want to code these dates
  #   ---- via 1970, so I put in 12/31/1969 and 7/1/1970 for these.  So, it is 
  #   ---- important to note that fishing seasons that straddle 12/31 will have 
  #   ---- periods that start in 1969, but end in 1970.  Otherwise, start and end 
  #   ---- fishing dates will be coded via year 1970.  
  
  #   ---- Need to make this go to the package inst folder, not the working directory.
  theExcel <- read.csv("L:/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/helperCode/theExcel.csv",stringsAsFactors=FALSE)
  bsplBegDt <- as.POSIXlt(strptime(theExcel[theExcel$siteID == site,]$minOverall[1],format="%m/%d/%Y",tz=time.zone),format="%Y-%m-%d",tz=time.zone)
  bsplEndDt <- as.POSIXlt(strptime(theExcel[theExcel$siteID == site,]$maxOverall[1],format="%m/%d/%Y",tz=time.zone),format="%Y-%m-%d",tz=time.zone)
  
  
  
  ans <- NULL
  traps <- sort( unique(obs.eff.df$TrapPositionID))
  
  fits <- all.X <- all.ind.inside <- all.dts <- obs.data <- eff.type <- vector("list", length(traps))
  names(fits) <- traps
  names(all.X) <- traps
  names(all.dts) <- traps
  names(all.ind.inside) <- traps
  names(obs.data) <- traps
  names(eff.type) <- traps
  
  # 	---- If number of trials at a trap less than this number, 
  #        assume constant and use ROM+1 estimator
  eff.min.spline.samp.size <- get("eff.min.spline.samp.size", pos=.GlobalEnv)
  sql.code.dir.pg <- get("sql.code.dir.pg", pos=.GlobalEnv)
  
  #   ---- Query for covariate data.  
  #require(EnvCovDBpostgres)
  
  #envCov <- odbcConnect("PostgreSQL30",uid="jmitchell",pwd="jmitchell")  # 32-bit.
  #tbldemo <- sqlQuery(envCov,"SELECT * FROM tbldemo")
  #close(envCov)
  
  #   ---- We assemble all the unique ourSiteIDs we need for this run. 
  TestingPlatform <- "CAMP_RST20161212-campR1.0.0"
  luSubSiteID <- read.csv(paste0("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",TestingPlatform,"/R/library/EnvCovDBpostgres/helperfiles/luSubSiteID.csv"))
  xwalk <- luSubSiteID[luSubSiteID$subSiteID %in% attr(obs.eff.df,"subsites")$subSiteID,]
  uniqueOurSiteIDsToQuery <- unique(c(xwalk$ourSiteIDChoice1))#,xwalk$ourSiteIDChoice2))
  
  df <- vector("list",length(uniqueOurSiteIDsToQuery))
  m1 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  flow (cfs)
  m2 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  temperature (C)
  m3 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  turbidity
  
  
  #   ---- Need to set an initial value for variable covar.  This holds the building string of 
  #   ---- a model statement.  Don't need to worry about if all the values for all efficiency 
  #   ---- trials are actually present -- we deal with that possibility below.  So... just add
  #   ---- the simple text statement.  
  
  obs.eff.df$covar <- "bdMeanNightProp + bdMeanMoonProp + bdMeanForkLength"
  
  # #   ---- Make summaries so we can see which sites and metrics span which time periods.  
  # ch <- odbcConnect("PostgreSQL30",uid="jmitchell",pwd="jmitchell")
  # tblsummarydnold <- sqlQuery(ch,"SELECT count(*) FROM tblsummaryd")
  # tblsummaryunold <- sqlQuery(ch,"SELECT count(*) FROM tblsummaryu")
  # buildnumber <- as.integer(sqlQuery(ch,"SELECT max(buildnumber) FROM tbllog"))
  # close(ch)
  # listsum <- makeSummaries(uid="jmitchell",pwd="jmitchell")
  # tblsummaryd <- listsum[[1]]
  # tblsummaryu <- listsum[[2]]
  
  # min.date <- min.date7
  # max.date <- max.date7
  # oursitevar <- oursitevar7
  # 
  # min.date <- "1957-01-01"
  # max.date <- "1958-12-31"
  # oursitevar <- 10
  
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
    #nGood <- sqlQuery(ch,paste0("SELECT COUNT(oursiteid) FROM tbld WHERE ('",min.date,"' <= date AND date <= '",max.date,"') AND oursiteid = ",oursitevar," GROUP BY oursiteid;"))
    res <- dbSendQuery(ch,paste0("SELECT COUNT(oursiteid) FROM tbld WHERE ('",min.date,"' <= date AND date <= '",max.date,"') AND oursiteid = ",oursitevar," GROUP BY oursiteid;"))
    nGood <- dbFetch(res)
    dbClearResult(res)
    #close(ch)
    dbDisconnect(ch)
    
    if(nrow(nGood) > 0){
      df[[ii]] <- queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="D",plot=FALSE)
      df[[ii]]$date <- strptime(df[[ii]]$date,format="%Y-%m-%d",tz=time.zone)
    } else {
      df[[ii]] <- queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="U",plot=FALSE)
      #df[[ii]]$date <- head(strptime(df[[ii]]$date,format="%Y-%m-%d %H:%M:%s",tz=time.zone))
      
      if(sum(!is.na(df[[ii]]$flow_cfs)) > 0 & (sum(df[[ii]]$flow_cfs <= -9997 & !is.na(df[[ii]]$flow_cfs)) > 0) ){
        df[[ii]][df[[ii]]$flow_cfs <= -9997 & !is.na(df[[ii]]$flow_cfs),]$flow_cfs <- NA
      }
      if(sum(!is.na(df[[ii]]$temp_c)) > 0 & (sum((df[[ii]]$temp_c >= 100 | df[[ii]]$temp_c <= 0) & !is.na(df[[ii]]$temp_c)) < 0)){
        df[[ii]][(df[[ii]]$temp_c >= 100 | df[[ii]]$temp_c <= 0) & !is.na(df[[ii]]$temp_c),]$temp_c <- NA
      }
      #dev.off();plot(df[[ii]]$date,df[[ii]]$flow_cfs,type="l",col="red",xlab="Date",ylab="Flow(cfs)",main="Flow (cfs)")
      #dev.off();plot(df[[ii]]$date,df[[ii]]$pred_temp_c,type="l",col="red",xlab="Date",ylab="Flow(cfs)",main="Flow (cfs)")
    }  
      
    #   ---- Compile the good dates for each metric. 
    min.date.flow <- suppressWarnings(min(df[[ii]][!is.na(df[[ii]]$flow_cfs),]$date))
    max.date.flow <- suppressWarnings(max(df[[ii]][!is.na(df[[ii]]$flow_cfs),]$date))
    min.date.temp <- suppressWarnings(min(df[[ii]][!is.na(df[[ii]]$temp_c),]$date))
    max.date.temp <- suppressWarnings(max(df[[ii]][!is.na(df[[ii]]$temp_c),]$date))
    
    #   ---- Query this river's Access database for information recorded at the trap.  For now, we only use this 
    #   ---- for turbidity.  I name objects that respect this.  
    db <- get( "db.file", envir=.GlobalEnv )
    ch <- odbcConnectAccess(db)
      dbDisc <- getCAMPEnvCov(ch,"discharge","dischargeUnitID",12)                     # <--- not sure on the UnitID.  could be 13 too?
      dbDpcm <- getCAMPEnvCov(ch,"waterDepth","waterDepthUnitID",3)                    # <--- depth in cm (American)
      dbDpft <- getCAMPEnvCov(ch,"waterDepth","waterDepthUnitID",6)                    # <--- depth in feet (RBDD)
      dbATpC <- getCAMPEnvCov(ch,"airTemp","airTempUnitID",18)
      dbATpF <- getCAMPEnvCov(ch,"airTemp","airTempUnitID",19)
      dbTurb <- getCAMPEnvCov(ch,"turbidity","turbidityUnitID",20)
      dbWVel <- getCAMPEnvCov(ch,"waterVel","waterVelUnitID",8)
      dbWTpC <- getCAMPEnvCov(ch,"waterTemp","waterTempUnitID",18)                     # <--- water temp in C (American)
      dbWTmF <- getCAMPEnvCov(ch,"waterTemp","waterTempUnitID",19)                     # <--- water temp in F (RBDD)
      dbLite <- getCAMPEnvCov(ch,"lightPenetration","lightPenetrationUnitID",20)       # <--- not sure on the UnitID.
      dbDOxy <- getCAMPEnvCov(ch,"dissolvedOxygen","dissolvedOxygenUnitID",36)         
      dbCond <- getCAMPEnvCov(ch,"conductivity","conductivityUnitID",36)               # <--- not sure on the UnitID.
      dbBaro <- getCAMPEnvCov(ch,"barometer","barometerUnitID",33)                     # <--- not sure on the UnitID.    
      dbWeat <- getCAMPEnvCov(ch,"weather",NA,NA)
    close(ch)
    
    #   ---- Put all database covariates into a list for easier processing.
    dbCovar <- list(dbDisc,dbDpcm,dbDpft,dbATpC,dbATpF,dbTurb,dbWVel,dbWTpC,dbWTmF,dbLite,dbDOxy,dbCond,dbBaro,dbWeat)

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
    
    #   ---- Read in how to map unstandardized weather values to standardized values.  Put this in as data.frame...eventually. 
    weaMap <- read.csv("//LAR-FILE-SRV/Data/PSMFC_CampRST/felipe products/variables/weather/weatherLookupMapped20170720.csv")
    #weaKey <- weaMap[1:4,c("PrecipLevel","PrecipLevelText")]
    dbWeat <- merge(dbWeat,weaMap[,c("weather","precipLevel")],by=c("weather"),all.x=TRUE)
    dbWeat <- dbWeat[,c("subSiteID","measureTime","precipLevel")]
    names(dbWeat)[names(dbWeat) == "precipLevel"] <- "precipLevel"
    
    #   ---- Fit a simple smoothing spline and predict.  First for temp and flow from the EnvCovDB.    
    covar <- NULL
    if(sum(!is.na(df[[ii]]$flow_cfs)) > 0){
      m1[[ii]] <- smooth.spline(df[[ii]][!is.na(df[[ii]]$flow_cfs),]$date,df[[ii]][!is.na(df[[ii]]$flow_cfs),]$flow_cfs,cv=TRUE)
      
      if("covar" %in% names(obs.eff.df)){
        if(is.na(obs.eff.df$covar[1])){
          obs.eff.df$covar <- paste0("flow_cfs")
        } else {
          obs.eff.df$covar <- paste0(obs.eff.df$covar," + flow_cfs")
        }
      } else {
        obs.eff.df$covar <- "flow_cfs"
      }
      
      obs.eff.df$flow_cfs <- NA
      obs.eff.df[obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice1 == oursitevar,]$subSiteID,]$flow_cfs <- predict(m1[[ii]],as.numeric(obs.eff.df$batchDate))$y
      #df[[ii]]$pred_flow_cfs <- predict(m1[[ii]])$y
      df[[ii]]$pred_flow_cfs <- predict(m1[[ii]],x=as.numeric(df[[ii]]$date))$y
      
      #    ---- See if we have any predicted values outside the range for which we have data.
      if(sum(df[[ii]]$date < min.date.flow | df[[ii]]$date > max.date.flow) > 0){
        df[[ii]][df[[ii]]$date < min.date.flow | df[[ii]]$date > max.date.flow,]$pred_flow_cfs <- NA
      }
      
      #   ---- Build a dataframe like the CAMP covariates.  If we're running against the unit table, we have no statistic.  
      #   ---- For flow, call it 450L, for temp, 451L.
      if(!(nrow(nGood) > 0)){
        dbFlPG <- data.frame(subSiteID=NA,measureTime=df[[ii]]$date,flow_cfs=df[[ii]]$flow_cfs,flow_cfsUnitID=rep(450L,nrow(df[[ii]])))
      } else {
        dbFlPG <- data.frame(subSiteID=NA,measureTime=df[[ii]]$date,flow_cfs=df[[ii]]$flow_cfs,flow_cfsUnitID=df[[ii]]$flow_statistic)
      }
      
      #    ---- See if we have any predicted values outside the range for which we have data.  Off by a day..?  Daylight savings?  So buffer.
      if(sum(obs.eff.df$batchDate + 60*60 < min.date.flow | obs.eff.df$batchDate - 60*60 > max.date.flow) > 0){
        obs.eff.df[obs.eff.df$batchDate + 60*60 < min.date.flow | obs.eff.df$batchDate - 60*60 > max.date.flow,]$flow_cfs <- NA
      }
    }
    if(sum(!is.na(df[[ii]]$temp_c)) > 0){
      m2[[ii]] <- smooth.spline(as.numeric(df[[ii]][!is.na(df[[ii]]$temp_c),]$date),df[[ii]][!is.na(df[[ii]]$temp_c),]$temp_c,cv=TRUE)
      
      if("covar" %in% names(obs.eff.df)){
        if(is.na(obs.eff.df$covar[1])){
          obs.eff.df$covar <- paste0("temp_c")
        } else {
          obs.eff.df$covar <- paste0(obs.eff.df$covar," + temp_c")
        }
      } else {
        obs.eff.df$covar <- "temp_c"
      }

      obs.eff.df$temp_c <- NA
      obs.eff.df[obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice1 == oursitevar,]$subSiteID,]$temp_c <- predict(m2[[ii]],as.numeric(obs.eff.df$batchDate))$y
      #df[[ii]]$pred_temp_c <- predict(m2[[iii]])$y
      df[[ii]]$pred_temp_c <- predict(m2[[ii]],x=as.numeric(df[[ii]]$date))$y
      
      #    ---- See if we have any predicted values outside the range for which we have data.
      if(sum(df[[ii]]$date < min.date.temp | df[[ii]]$date > max.date.temp) > 0){
        df[[ii]][df[[ii]]$date < min.date.temp | df[[ii]]$date > max.date.temp,]$pred_temp_c <- NA
      }
      
      #   ---- Build a dataframe like the CAMP covariates.  If we're running against the unit table, we have no statistic.  
      #   ---- For flow, call it 450L, for temp, 451L.  Recall that oursitevar >= 80 means EnvCovDB from unit table is used.
      if(!(nrow(nGood) > 0)){
        dbTpPG <- data.frame(subSiteID=NA,measureTime=df[[ii]]$date,temp_c=df[[ii]]$temp_c,temp_cUnitID=451L)       
      } else {
        dbTpPG <- data.frame(subSiteID=NA,measureTime=df[[ii]]$date,temp_c=df[[ii]]$temp_c,temp_cUnitID=df[[ii]]$temp_statistic)       
      }

      #   ---- See if we have any predicted values outside the range for which we have data.  Off by a day..?  Daylight savings?  So buffer.
      if(sum(obs.eff.df$batchDate + 60*60 < min.date.temp | obs.eff.df$batchDate - 60*60 > max.date.temp) > 0){
        obs.eff.df[obs.eff.df$batchDate + 60*60 < min.date.temp | obs.eff.df$batchDate - 60*60 > max.date.temp,]$temp_c <- NA
      }
    }
    
    #   ---- Next for data from the CAMP db, which could be collected per subSiteID.  Reduce to the set of subsiteIDs in this run.  This 
    #   ---- is necessary for the Feather, which has many sites that branch into individual subSiteIDs.  Do this for each covar.
    
    #dbCov <- dbCovar[[ii]]
    
    #   ---- Now, bring in smoothing-spline estimated values.
    
    obs.eff.df <- estCovar(dbDisc,"discharge_cfs",1,traps,obs.eff.df,xwalk,oursitevar)                   # <--- not sure on the UnitID.  could be 13 too?
    obs.eff.df <- estCovar(dbDpcm,"waterDepth_cm",1,traps,obs.eff.df,xwalk,oursitevar)                   # <--- depth in cm (American)
    obs.eff.df <- estCovar(dbDpft,"waterDepth_ft",1,traps,obs.eff.df,xwalk,oursitevar)                   # <--- depth in feet (RBDD)
    obs.eff.df <- estCovar(dbATpC,"airTemp_C",1,traps,obs.eff.df,xwalk,oursitevar)
    obs.eff.df <- estCovar(dbATpF,"airTemp_F",1,traps,obs.eff.df,xwalk,oursitevar)
    obs.eff.df <- estCovar(dbTurb,"turbidity_ntu",1,traps,obs.eff.df,xwalk,oursitevar)
    obs.eff.df <- estCovar(dbWVel,"waterVel_fts",1,traps,obs.eff.df,xwalk,oursitevar)
    obs.eff.df <- estCovar(dbWTpC,"waterTemp_C",1,traps,obs.eff.df,xwalk,oursitevar)                     # <--- water temp in C (American)
    obs.eff.df <- estCovar(dbWTmF,"waterTemp_F",1,traps,obs.eff.df,xwalk,oursitevar)                     # <--- water temp in F (RBDD)
    obs.eff.df <- estCovar(dbLite,"lightPenetration_ntu",1,traps,obs.eff.df,xwalk,oursitevar)            # <--- not sure on the UnitID.
    obs.eff.df <- estCovar(dbDOxy,"dissolvedOxygen_mgL",1,traps,obs.eff.df,xwalk,oursitevar)        
    obs.eff.df <- estCovar(dbCond,"conductivity_mgL",1,traps,obs.eff.df,xwalk,oursitevar)                # <--- not sure on the UnitID.
    obs.eff.df <- estCovar(dbBaro,"barometer_inHg",1,traps,obs.eff.df,xwalk,oursitevar)                  # <--- not sure on the UnitID.    
    obs.eff.df <- estCovar(dbWeat,"precipLevel_qual",2,traps,obs.eff.df,xwalk,oursitevar)
  
    obs.eff.df <- obs.eff.df[order(obs.eff.df$TrapPositionID,obs.eff.df$batchDate),]

  }
  
  #   ---- Look at how the full models work out with respect to different traps.  
  table(obs.eff.df[!is.na(obs.eff.df$efficiency),]$covar,obs.eff.df[!is.na(obs.eff.df$efficiency),]$TrapPositionID)

   
  
  
  
  # plot(obs.eff.df[!is.na(obs.eff.df$efficiency),]$temp_c,
  #      obs.eff.df[!is.na(obs.eff.df$efficiency),]$waterTemp_C,
  #      col=c("red","orange","green","blue","black")[as.factor(obs.eff.df[!is.na(obs.eff.df$efficiency),]$TrapPositionID)],pch=19)
  # lines(c(-50000,50000),c(-50000,50000),col="black")
  # 
  # cor(obs.eff.df[!is.na(obs.eff.df$efficiency) & obs.eff.df$TrapPositionID == "57001",]$temp_c,
  #     obs.eff.df[!is.na(obs.eff.df$efficiency) & obs.eff.df$TrapPositionID == "57001",]$waterTemp_C)
  # cor(obs.eff.df[!is.na(obs.eff.df$efficiency) & obs.eff.df$TrapPositionID == "57004",]$temp_c,
  #     obs.eff.df[!is.na(obs.eff.df$efficiency) & obs.eff.df$TrapPositionID == "57004",]$waterTemp_C)
  
  varSummary <- NULL
  possibleVars <- c("(Intercept)","bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength","flow_cfs","temp_c","discharge_cfs","waterDepth_cm","waterDepth_ft","airTemp_C","airTemp_F","turbidity_ntu","waterVel_fts","waterTemp_C","waterTemp_F","lightPenetration_ntu","dissolvedOxygen_mgL","conductivity_mgL","barometer_inHg","precipLevel_qual")

  #   ---- Estimate a model for efficiency for each trap in obs.eff.df.
  for( trap in traps ){
    
    df <- obs.eff.df[ is.na(obs.eff.df$TrapPositionID) | (obs.eff.df$TrapPositionID == trap), ]
    ind <- !is.na(df$efficiency)
 
    initialVars <- c("(Intercept)",names(df)[!(names(df) %in% c("batchDate","nReleased","nCaught","efficiency","covar","batchDate2","fishDay"))])
    initialVarsNum <- c(2,as.integer(possibleVars %in% initialVars))
    
    # JASON PLAYS AROUND AND ENSURES A DATE REPEATS OVER TWO YEARS.
    #df[!is.na(df$efficiency),]$batchDate[15] <- df[!is.na(df$efficiency),]$batchDate[1]
    
    
    #   ---- Define these so we can model on (basically) fishing day of the season.  Do this with respect to 
    #   ---- the year 1970 paradigm defined above.  We really only care about the number of days since the
    #   ---- minimum start of fishing, over all years, so the year is immaterial.  Use 1970, or maybe 1969, 
    #   ---- so we always keep this fact in mind.  Not sure this will always work... Need to check.  
    sign <- as.numeric(strftime(df$batchDate,format="%j")) - as.numeric(strftime(bsplBegDt,format="%j"))
    df$fishDay <- ifelse(sign == 0,0,
                  ifelse(sign  < 0,sign + 365,as.numeric(strftime(df$batchDate,format="%j")))) 
    #df$fishPeriod <- as.factor(as.POSIXlt(df$batchDate)$year + 1900)
    
    #firstYear <- min(as.POSIXlt(df$batchDate)$year + 1900)
    firstYear <- min(bsplBegDt$year + 1900)
    df$batchDate2 <- ifelse(sign >= 0,as.POSIXct(paste0(firstYear,"-",as.POSIXlt(df$batchDate)$mon + 1,"-",as.POSIXlt(df$batchDate)$mday),format="%Y-%m-%d",tz=time.zone),
                     ifelse(sign < 0,as.POSIXct(paste0(firstYear + 1,"-",as.POSIXlt(df$batchDate)$mon + 1,"-",as.POSIXlt(df$batchDate)$mday),format="%Y-%m-%d",tz=time.zone),
                     NA))
    
    #   ---- If we have a batchDate on 2/29, we have a problem, because 1970 wasn't a leap year.  Note that I could have 
    #   ---- chosen 
    df$batchDate2 <- as.POSIXct(df$batchDate2,format="%Y-%m-%d",tz="America/Los_Angeles",origin="1970-01-01 00:00:00 UTC")
      
    #df$batchDate2 <- as.POSIXct(paste0(firstYear,"-",as.POSIXlt(df$batchDate)$mon + 1,"-",as.POSIXlt(df$batchDate)$mday),format="%Y-%m-%d",tz=time.zone)

    
    #   ---- Find the "season", which is between first and last trials
    #   ---- We look for 'inside' with respect to our 1969-1970 mapped year of trials.  
    strt.dt <- bsplBegDt #min( df$batchDate[ind], na.rm=T )  # Earliest date with an efficiency trial
    end.dt  <- bsplEndDt #max( df$batchDate[ind], na.rm=T )  # Latest date with efficiency trial
    ind.inside <- (strt.dt <= df$batchDate2) & (df$batchDate2 <= end.dt)
    inside.dates <- c(strt.dt, end.dt)
    all.ind.inside[[trap]] <- inside.dates  # save season dates for bootstrapping
    
    #   ---- The fitting data frame
    tmp.df <- df[ind & ind.inside,]
    m.i <- sum(ind & ind.inside)
    
    #   ---- With the inclusion of all CAMP covariates, the probability increases by a lot that we don't have all 
    #   ---- the values over all time.  Chuck those that don't have at least ... 90% of the data rows, given a 
    #   ---- trap.  Note that this considers NA WITHIN a column.  If we delete, we have to update tmp.df$covar.
    # write.csv(tmp.df,"L:/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Outputs/Mokelumne River--Golf RST Main Site/2006/Enhanced_Eff_Get_Betas/tmp.df.csv",row.names=FALSE)
    atLeast <- floor(0.90*m.i) + 1
    vars <- c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength",colnames(tmp.df[,(which(colnames(tmp.df) == "covar") + 1):ncol(tmp.df)]))
    vars <- vars[!(vars %in% c("fishDay","batchDate2"))]
    for(i in 1:length(vars)){
      if(!(sum(!is.na(tmp.df[,vars[i]])) >= atLeast)){
        cat(paste0("Trap ",trap," variable ",vars[i]," has only ",sum(!is.na(tmp.df[,vars[i]]))," points but needs ",atLeast," for inclusion.  Deleting.\n"))
        tmp.df[,vars[i]] <- NULL
        
        #   ---- Have to now update the "+" situation.  Removed variable could be leading, in the middle, or 
        #   ---- trailing.  So, consider all these.  
        tmp.df$covar <- gsub(paste0(" + ",vars[i]),"",tmp.df$covar,fixed=TRUE)  # middle or trailing -- remove leading " + "
        tmp.df$covar <- gsub(vars[i],"",tmp.df$covar,fixed=TRUE)                # leading -- remove var[i] alone
      } else {
        cat(paste0("Trap ",trap," variable ",vars[i]," has ",sum(!is.na(tmp.df[,vars[i]]))," points and only need ",atLeast," for inclusion.  Keeping.\n"))
      }
    }
    
    #   ---- See if we have any missing covars --- assumes all other values are not NA.  Note that this considers 
    #   ---- NA OVER columns.  
    tmp.df$allCovars <- apply(tmp.df,1,function(x) as.numeric(!(sum(is.na(x)) > 0)))
    
    #   ---- Could be that we have efficiency trials performed at times for which we have no covariate information.
    #   ---- Delete out these trials in favor of data rows with all data present -- for now. Output when we do this.
    #   ---- Also identify the trials we delete so that the basis matrix gets the right number of rows (valid trials).
    #   ---- I do this via NA in efficiency, since that is the criterion used in fitSpline.R.  
    dataDeficient <- tmp.df[tmp.df$allCovars == 0,]
    cat( paste0("Trap ",trap," has ",nrow(dataDeficient)," rows of data-covariate deficient, out of ",nrow(tmp.df)," originally present.\n") )
    write.csv(dataDeficient,paste0(plot.file,"-",trap,"dataDeficient.csv"),row.names=FALSE)
    if(length(tmp.df[tmp.df$allCovars == 0,]$batchDate) > 0){
      df[df$batchDate %in% tmp.df[tmp.df$allCovars == 0,]$batchDate,]$efficiency <- NA
    }
    tmp.df <- tmp.df[tmp.df$allCovars == 1,]
    tmp.df$allCovars <- NULL     # This has served its purpose.  
    
    #   ---- We defined the start and end for the fishing season.  Need this to know when we need an efficiency estimate.
    #   ---- But we need to find the actual temporal range of efficiency dates, so the boundary points of the spline are
    #   ---- appropriate for efficiency effort.  Otherwise, the boundary points in the efficiency spline uses the 
    #   ---- overall min and max of fishing, which isn't correct.  Also, we may have just thrown out "good" efficiency 
    #   ---- trials that are "bad" because they lack data on a possible covariate.  If this trial was on the edge of the 
    #   ---- efficiency 1959-1960-time-period construct, this is a problem.  
    eff.strt.dt <- min(tmp.df$batchDate2)
    eff.end.dt <- max(tmp.df$batchDate2)
    eff.ind.inside <- (eff.strt.dt <= df$batchDate2) & (df$batchDate2 <= eff.end.dt)
    eff.inside.dates <- c(eff.strt.dt,eff.end.dt)
    
    #   ---- At this point, have cleaned up the this trap's efficiency data.  NOW, define fish-start days, so that we can
    #   ---- model via the number of days from the start of fishing.  This puts efficiency years "on top of each other."  
    #   ---- I *could* have deleted, because of bad covariates, the *true* start of the fishing period for a certain 
    #   ---- year.  I'm okay with this I think.  
    
    
    # par(mfrow=c(1,2))
    # plot(tmp.df$batchDate,tmp.df$efficiency,col=c("red","green","blue")[as.factor(tmp.df$fishPeriod)],pch=19,cex=3)
    # plot(tmp.df$batchDate2,tmp.df$efficiency,col=c("red","green","blue")[as.factor(tmp.df$fishPeriod)],pch=19,cex=3)
    # par(mfrow=c(1,1))
    
    
    if( m.i == 0 ){
      
      #   ---- No efficiency trials at this trap.
      cat( paste("NO EFFICIENCY TRIALS FOR TRAP", trap, "\n") )
      cat( paste("Catches at this trap will not be included in production estimates.\n"))
      fits[[trap]] <- NA
      all.X[[trap]] <- NA
      df$efficiency <- NA
      obs.data[[trap]] <- NA
      eff.type[[trap]] <- 1
      
    } else if( (m.i < eff.min.spline.samp.size) | (sum(tmp.df$nCaught) == 0) ){
      
      #   ---- Take simple average of efficiency trials:  constant over season.
      cat("Fewer than 10 trials found or no recaptures.  'ROM+1' estimator used.\n")
      
      #   ---- This is MOR, or mean of ratios.  
      #obs.mean <- mean(obs)  
      #cat(paste("MOR efficiency= ", obs.mean, "\n"))
      
      #   ---- This is ROM = Ratio of means, with bias correction.  
      obs.mean <- (sum(tmp.df$nCaught)+1) / (sum(tmp.df$nReleased)+1)   
      cat(paste("'ROM+1' efficiency= ", obs.mean, "\n"))
      cat(paste0("\n\n\n"))
      
      #   ---- If want to use ROM for missing efficiencies only, uncomment the next line.  
      #df$efficiency[!ind] <- obs.mean
      
      #   ---- If, however, you want to use ROM for all days, missing or not, uncomment the next line. 
      #   ---- Fit a model here so we have dispersion statistic, beta, and covar matrix for use in
      #   ---- bootstrapping later.  
      fits[[trap]] <- glm(nCaught / nReleased ~ 1, family=binomial, data=tmp.df, weights=tmp.df$nReleased )
      df$efficiency <- obs.mean
      obs.data[[trap]] <- tmp.df
      eff.type[[trap]] <- 2
      
      #   ---- Make a design matrix for ease in calculating predictions.  Used in bootstrapping.
      #   ---- Very simple design matrix in this case, since we're only fitting an intercept.  
      if( length(coef(fits[[trap]])) == 1 ){
        #pred <- matrix( coef(fit), sum(ind.inside), 1 )
        X <- matrix( 1, length(df$batchDate2[eff.ind.inside]), 1)#sum(eff.ind.inside), 1)
      }
      
      #   ---- Plot the spline.  Note this reproduces the pred calculation just above.  I like having 
      #   ---- all of that in the function from start to finish, although it's clear it's not much. 
      #   ---- For this mean-only model, I have to finagle X to look like bs-function output. 
      attr(X,"intercept") <- FALSE
      attr(X,"Boundary.knots") <- as.numeric(eff.inside.dates)
      attr(X,"knots") <- numeric(0)
      bspl <- X
      png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",0,"mt.png"),width=7,height=7,units="in",res=600)
      model.info <- plot.bs.spline(bspl,fits[[trap]],bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
      dev.off()
      
      #   ---- Save X, and the dates at which we predict, for bootstrapping.
      all.X[[trap]] <- X   
      all.dts[[trap]] <- df$batchDate[ind.inside] 
      
    } else {    
      
      #   ---- There are enough observations to estimate B-spline model.
      covarString <- "1"
      m0 <- fitSpline(covarString,df,eff.ind.inside,tmp.df,dist="binomial",max.df.spline)
      
      cat("\nTemporal-only efficiency model for trap: ", trap, "\n")
      print(summary(m0$fit, disp=sum(residuals(m0$fit, type="pearson")^2)/m0$fit$df.residual)  )
        
      #   ---- Plot the spline.  
      png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",0,"mt.png"),width=7,height=7,units="in",res=600)
      model.info <- plot.bs.spline(m0$bspl,m0$fit,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
      dev.off()       
        
    }   
      
    
  
  # #   ---- At this point, we have a temporal spline.  We now consider covariates.  Identify them.
  # covarMat <- as.matrix(tmp.df[,strsplit(tmp.df$covar[1]," + ",fixed=TRUE)[[1]]])
  # 
  # #   ---- If we have only one covariate, make sure the vector (not matrix) is named for clarity in models.
  # if(dim(covarMat)[2] == 1){
  #   colnames(covarMat) <- tmp.df$covar[1]
  # }
  # covar <- tmp.df$covar[1]
  # cat(paste0("\n\n++++++ Starting backwards selection of confounding covariates: ",gsub(" + ",", ",covar,fixed=TRUE),".\n"))
  # 
  # #   ---- See if our smoothing splines lead to non-sensical estimates; i.e., negative values for 
  # #   ---- either of flow (cfs) or turbidity (ntu).  But not temperature!  Others?
  # if("temp_c" %in% colnames(covarMat)){
  #   
  #   theNames <- colnames(covarMat)
  #   theNames <- theNames[theNames != "temp_c"]
  #   
  #   #   ---- Without temp_c.  I assume temp_c is the only var for which negatives are permissable.  
  #   covarMat <- cbind(apply(as.matrix(covarMat[,colnames(covarMat) != "temp_c"]),2,function(x) ifelse(x < 0,0,x)),as.matrix(covarMat[,colnames(covarMat) == "temp_c"]))
  #   colnames(covarMat) <- c(theNames,"temp_c")
  # } else {
  #   covarMat <- apply(covarMat,2,function(x) ifelse(x < 0,0,x))
  # } 
  
  
  
    
    #   ---- Clean up the duplicate variable situation. 
    
    #   ---- If we have water temperature from both EnvCovDB and CAMP, keep EnvCovDB.
    one <- sum(initialVars %in% "temp_c")
    two <- as.numeric(sum(initialVars %in% c("waterTemp_C","waterTemp_F")) >= 1)
    interimVars1 <- initialVars
    interimVars1Num <- c(3,as.numeric(possibleVars %in% interimVars1))
    
    #   ---- If both conditions are true, we have both vars.  Get rid of CAMP.  
    if( (one == two) & (one == 1) ){
      interimVars1 <- interimVars1[!(interimVars1 %in% c("waterTemp_C","waterTemp_F"))]
      interimVars1Num <- c(3,as.numeric(possibleVars %in% interimVars1))
      
      #   ---- Have to now update the "+" situation.  Removed var could be leading, middle, or trailing. 
      covarString <- gsub(paste0(" + ","waterTemp_C"),"",covarString,fixed=TRUE)  # middle or trailing -- remove leading " + "
      covarString <- gsub("waterTemp_C","",covarString,fixed=TRUE)                # leading -- remove var[i] alone
      cat("I have removed variable waterTemp_C because temp_c is already present.\n\n")
      
      covarString <- gsub(paste0(" + ","waterTemp_F"),"",covarString,fixed=TRUE)  # middle or trailing -- remove leading " + "
      covarString <- gsub("waterTemp_F","",covarString,fixed=TRUE)                # leading -- remove var[i] alone
      cat("I have removed variable waterTemp_F because temp_c is already present.\n\n")
    }
  
  
    #   ---- If we have water flow from both EnvCovDB and CAMP, keep EnvCovDB.
    one <- sum(initialVars %in% "flow_cfs")
    two <- as.numeric(sum(initialVars %in% c("discharge_cfs")) >= 1)
    interimVars2 <- initialVars
    interimVars2Num <- c(4,as.numeric(possibleVars %in% interimVars2))
    
    #   ---- If both conditions are true, we have both vars.  Get rid of CAMP.  
    if( (one == two) & (one == 1) ){
      interimVars2 <- interimVars2[!(interimVars2 %in% c("discharge_cfs"))]
      interimVars2Num <- c(4,as.numeric(possibleVars %in% interimVars2))
      
      #   ---- Have to now update the "+" situation.  Removed var could be leading, middle, or trailing. 
      covarString <- gsub(paste0(" + ","discharge_cfs"),"",covarString,fixed=TRUE)  # middle or trailing -- remove leading " + "
      covarString <- gsub("discharge_cfs","",covarString,fixed=TRUE)                # leading -- remove var[i] alone
      cat("I have removed variable discharge_cfs because flow_cfs is already present.\n\n")
    }
      
    #   ---- Fit the full model with all possible covariates that made it.  Object fit.tmp.bs is from the last kept run.
    method <- "likeType3SS"    # "pVal"    
    distn <- "binomial"   # "binomial"
    pCutOff <- 0.10
    covarString <- tmp.df$covar[1]
    
    #   ---- Save a record of the variables we care about for plotting below.  
    covarStringPlot <- covarString
    
    #   ---- If we only ended up with an intercept-only model, skip straight to the end.
    if( !(m.i < eff.min.spline.samp.size) | (sum(tmp.df$nCaught) == 0) ){
        

      m0 <- fitSpline(covarString,df,eff.ind.inside,tmp.df,distn,max.df.spline)
      fit0 <- m0$fit
      tmp.bs <- m0$bspl[!is.na(df$efficiency[eff.ind.inside]),]
      disp <- m0$disp
      new.bspl <- m0$bspl
      
      #   ---- Send results to console.  Note that this applies the value of disp in the summary.  
      #   ---- In other words, this is 'quasibinomial' by definition, but in a backwards way.  
      cat("\n\n\nTemporal & initial covariate efficiency model for trap: ", trap, "\n")
      print(summary(m0$fit, disp=sum(residuals(m0$fit, type="pearson")^2)/m0$fit$df.residual))
      
      #   ---- Plot the spline.  
      png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",1,"m0.png"),width=7,height=7,units="in",res=600)
      model.info <- plot.bs.spline(m0$bspl,m0$fit,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
      dev.off()  
      
      

          
      model.i <- 2
      repeat{
            
        #   ---- Identify the worst-performing variable.  I use the highest p-value.  
        if(method == "pVal"){
          theHighestp <- max(coefficients(summary(fit0))[row.names(coefficients(summary(fit0))) != "(Intercept)" & !(sapply(row.names(coefficients(summary(fit0))),function(x) grepl("fit.tmp",x,fixed=TRUE))),4])
          varToTestForDeletion <- row.names(coefficients(summary(fit0)))[coefficients(summary(fit0))[,4] == theHighestp]
            
        } else if(method == "likeType3SS"){
              
          #   ---- We need to calculate a X2 test for each variable, one-by-one.  
          thePossibleCovarString <- as.data.frame(coefficients(summary(fit0)))[row.names(coefficients(summary(fit0))) != "(Intercept)" & !(sapply(row.names(coefficients(summary(fit0))),function(x) grepl("tmp.bs",x,fixed=TRUE))),]
          temporalSplinePresent <- ifelse(sum(grepl("tmp.bs",row.names(coefficients(summary(fit0))))) > 0,1,0) 
          
          #   ---- Loop through, creating a formula string with the ith row's coefficient deleted. 
          checkThisMany <- nrow(thePossibleCovarString)
          holdEm <- vector("list",checkThisMany)
          df.anova <- NULL
          for(v in 1:checkThisMany){
                
            if( checkThisMany == 1 ){
              vthCovarString <- paste0("nCaught / nReleased ~ tmp.bs")
              if(temporalSplinePresent == 0){
                vthCovarString <- paste0("nCaught / nReleased ~ 1")
              }
            } else {
              vthCovarString <- paste0("nCaught / nReleased ~ tmp.bs + ",paste0(rownames(thePossibleCovarString)[-v],collapse=" + "))
              if(temporalSplinePresent == 0){   # Can this ever happen?
                vthCovarString <- paste0("nCaught / nReleased ~ 1 + ",paste0(rownames(thePossibleCovarString)[-v],collapse=" + "))
              }
            }
            holdEm[[v]] <- glm( as.formula(vthCovarString), family=distn, data=tmp.df, weights=tmp.df$nReleased )
                
            #   ---- Assuming all variables to be tested are of 1 degree of freedom, could just find the set that results in the lowest
            #   ---- residual deviance.  But, we should make it smarter for the eventual factors we could have.  
            tmp.anova <- as.data.frame(anova(holdEm[[v]]))
            if(nrow(tmp.anova) == 1){
              rownames(tmp.anova) <- rownames(thePossibleCovarString)[v]
            }
            tmp.anova$testingCovarDF <- anova(fit0)[row.names(anova(fit0)) == rownames(thePossibleCovarString)[v],]$Df
            tmp.anova$model <- v
            tmp.anova$testingCovar <- rownames(thePossibleCovarString)[v]
            df.anova <- rbind(df.anova,tmp.anova)
          }
          df.anova$biggerModelDev <- anova(fit0)$`Resid. Dev`[checkThisMany + 1 + temporalSplinePresent]   # + 1 Int + 1 spline
          df.chi <- aggregate(df.anova,list(df.anova$testingCovar),function(x) tail(x,1))
          df.chi$Group.1 <- df.chi$`Resid. Df` <- NULL
          df.chi$ChiSq <- df.chi$`Resid. Dev` - df.chi$biggerModelDev
          #df.chi$pVal <- pchisq(df.chi$ChiSq/summary(fit0)$dispersion,df.chi$testingCovarDF,lower.tail=FALSE)
          df.chi$pVal <- pchisq(df.chi$ChiSq/disp,df.chi$testingCovarDF,lower.tail=FALSE)
          
          # #   ---- Tinkering with deviance -- normal models.
          # samp <- NULL
          # for(i in 1:10000){
          #   N <- 10
          #   e <- rnorm(N,0,100)
          #   beta0 <- 1
          #   beta1 <- 2
          #   x <- seq(0,N - 1,1)
          #   y <- beta0 + beta1*x + e
          #   df <- data.frame(y=y,x=x)
          #   mF <- glm(y ~ 1 + x,data=df,family=gaussian)
          #   summary(mF)
          #   
          #   #   ---- Test for significance of beta1 via likelihood ratio test statistic.  Need reduced model.
          #   mR <- glm(y ~ 1,data=df,family=gaussian)
          #   
          #   deltaDev <- (mR$deviance - mF$deviance)/summary(mF)$dispersion  # <- this is distributed (exactly b/c of normal) X^2(1).
          #   lp <- pchisq(deltaDev,1,lower.tail=FALSE)     # <- s/b approximate (exact?) to wald z in summary(mF).
          #   
          #   #   ---- Compare explicitly the two p-values.
          #   tp <- coefficients(summary(mF))[2,4]   # t-based pvalue
          #   samp <- rbind(samp,data.frame(tp=tp,lp=lp,sample=i))
          # }
          # 
          # hist(samp$tp-samp$lp)
          
          #   ---- Now, identify the one with the highest Chi-square p-value.  
          theHighestp <- max(df.chi$pVal) 
          varToTestForDeletion <- df.chi[df.chi$pVal == theHighestp,]$testingCovar
          
        }
          
        if(theHighestp > pCutOff & covarString != ""){
            
          cat(paste0("Variable ",varToTestForDeletion," has a p-value of ",round(theHighestp,4),", which is greater than ",pCutOff,".  Removing.\n\n"))
              
          #   ---- Have to now update the "+" situation.  Removed var could be leading, middle, or trailing. If this is the last covariate, 
          #   ---- we have nothing left.  
          if(checkThisMany == 1){
            covarString <- ""
          } else {
            covarString <- gsub(paste0(" + ",varToTestForDeletion),"",covarString,fixed=TRUE)  # middle or trailing -- remove leading " + "
            covarString <- gsub(paste0(varToTestForDeletion," + "),"",covarString,fixed=TRUE)  # leading -- remove var[i] alone and next " + "
          }
          
          #   ---- Fit updated model.  
          if(covarString == ""){  # We could NOW have a blank covarString.  Note the lack of the '+' below. 
            if(temporalSplinePresent == 1){
              fit1 <- glm( as.formula(paste0("nCaught / nReleased ~ tmp.bs ",covarString)), family=distn, data=tmp.df, weights=tmp.df$nReleased ) 
            } else if(temporalSplinePresent == 0){
              fit1 <- glm( as.formula(paste0("nCaught / nReleased ~ ",covarString)), family=distn, data=tmp.df, weights=tmp.df$nReleased ) 
            }
          } else {
            if(temporalSplinePresent == 1){   # Assumes this was calculated above in evaluation of covariate.
              fit1 <- glm( as.formula(paste0("nCaught / nReleased ~ tmp.bs + ",covarString)), family=distn, data=tmp.df, weights=tmp.df$nReleased ) 
            } else if(temporalSplinePresent == 0){
              fit1 <- glm( as.formula(paste0("nCaught / nReleased ~ ",covarString)), family=distn, data=tmp.df, weights=tmp.df$nReleased ) 
            }
          }
          
          #   ---- Output visual impact of variable deletion.  Object new.bspl possibly has a bigger spline from below. 
          png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",model.i,"mc.png"),width=7,height=7,units="in",res=600)
          model.info <- plot.bs.spline(new.bspl,fit1,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
          dev.off()
             
          #   ---- We've now removed a covariate.  Reconsider the temporal spline.
          fit0 <- fit1    
          
          if(covarString != ""){
          
            
            m2 <- fitSpline(covarString,df,eff.ind.inside,tmp.df,distn,max.df.spline)
            fit2 <- m2$fit
            tmp.bs <- m2$bspl[!is.na(df$efficiency[eff.ind.inside]),]
            disp <- m2$disp
            
            #   ---- Send results to console.  Note that this applies the value of disp in the summary.  
            #   ---- In other words, this is 'quasibinomial' by definition, but in a backwards way.  
            cat("\nFinal Efficiency model for trap: ", trap, "\n")
            print(summary(m2$fit, disp=sum(residuals(m2$fit, type="pearson")^2)/m2$fit$df.residual))
            
            #   ---- Plot the spline.  
            png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",model.i,"mt.png"),width=7,height=7,units="in",res=600)
            model.info <- plot.bs.spline(m2$bspl,m2$fit,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
            dev.off()  
        
            #   ---- Report.  fit1 is from the covariate fit, while fit2 is from the subsequent temporal spline fit.
            before.bs.Len <- length(coef(fit1)[substr(names(coef(fit1)),1,6) == "tmp.bs"])
            after.bs.Len <- length(coef(fit2)[substr(names(coef(fit2)),1,6) == "tmp.bs"])
            cat(paste0("After last variable removable, I went from a ",before.bs.Len," spline to a ",after.bs.Len," spline.\n\n"))
            if(after.bs.Len != before.bs.Len){
              new.bspl <- m2$bspl
            } 
            fit0 <- fit2
          } else if(covarString == ""){
            
            #   ---- If we are here, we are back to the temporal spline-only model.  No covariate works.  
            covarString <- "1"
            m0 <- fitSpline(covarString,df,eff.ind.inside,tmp.df,dist="binomial",max.df.spline)
            
            cat("\nTemporal-only (after consideration of covariates) efficiency model for trap: ", trap, "\n")
            print(summary(m0$fit, disp=sum(residuals(m0$fit, type="pearson")^2)/m0$fit$df.residual)  )
            
            #   ---- Plot the spline.  
            png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",model.i + 1,"mc.png"),width=7,height=7,units="in",res=600)
            model.info <- plot.bs.spline(m0$bspl,m0$fit,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
            dev.off() 
            break
          }
          
        } else {
          cat(paste0("Variable ",varToTestForDeletion," has a p-value of ",round(theHighestp,8),", which is less than ",pCutOff,".  Keeping and exiting.\n\n"))
          cat("Final model identified.\n")
          break
        }
        model.i <- model.i + 1
        varToTestForDeletion <- NULL
      }
    } else {
      #   ---- Intercept-only model.  Just report what we already have.  
      fit0 <- fits[[trap]]
    } 
          
    cat(paste0("The final fit with covariates and spline together is:  \n"))
    print(summary(fit0, disp=sum(residuals(fit0, type="pearson")^2)/fit0$df.residual))
    cat(paste0("\n\n\n"))
          
    fit <- fit0
        
    #   ---- Summarize which variables for this subSiteID made it into the final model.  
    finalVars <- names(fit$coefficients)[!(names(fit$coefficients) %in% c(names(fit$coefficients)[c(grepl("tmp",names(fit$coefficients)))]))]
    finalVarsNum <- c(5,as.integer(possibleVars %in% finalVars))
        
        
    #   ---- Make a design matrix for ease in calculating predictions.
    plotCovars <- unlist(strsplit(covarStringPlot," + ",fixed=TRUE)[[1]])
        
    if( length(coef(fit)) <= 1 ){
      pred <- matrix( coef(fit), sum(eff.ind.inside), 1 )
      X <- matrix( 1, length(df$batchDate2[eff.ind.inside]), 1)#sum(eff.ind.inside), 1)
      nCovars <- 0
    } else {
      #X <- cbind( 1, bspl )
      #pred <- X %*% coef(fit)
      X <- model.info$X
      pred <- log(model.info$pred/(1 - model.info$pred))
      nCovars <- length(plotCovars)
    }
      
    #   ---- Save X, and the dates at which we predict, for bootstrapping.
    all.X[[trap]] <- X   
    all.dts[[trap]] <- df$batchDate[ind.inside]   
        
    #   ---- Standard logistic prediction equation.  
    #   ---- "Pred" is all efficiencies for dates between min and max of trials.
    pred <- 1 / (1 + exp(-pred))  
        
      
    #   ---- Make a lookup vector.  
    dfs <- c("dbMoon","dbNite","dbFLen","dbFlPG","dbTpPG","dbDisc","dbDpcm","dbDpft","dbATpC","dbATpF","dbTurb","dbWVel","dbWTpC","dbWTmF","dbLite","dbDOxy","dbCond","dbBaro","dbWeat")
    covars <- c("bdMeanMoonProp","bdMeanNightProp","bdMeanForkLength","flow_cfs","temp_c","discharge_cfs","waterDepth_cm","waterDepth_ft","airTemp_C","airTemp_F","turbidity_ntu","waterVel_fts","waterTemp_C","waterTemp_F","lightPenetration_ntu","dissolvedOxygen_mgL","conductivity_mgL","barometer_inHg","precipLevel_qual")
    covarlu <- dfs
    names(covarlu) <-covars
        
    #   ---- The postgres flow and temp data are trap independent, unlike the CAMP data.  So, tie the data
    #   ---- to the trap in the way the covar plotting function expects.
    if(exists("dbFlPG")) dbFlPG$subSiteID <- trap
    if(exists("dbTpPG")) dbTpPG$subSiteID <- trap
        
    tt <- forEffPlots[forEffPlots$trapPositionID == trap,]
    dbMoon <- data.frame(subSiteID=tt$trapPositionID,measureTime=tt$EndTime,moonProp=tt$moonProp,moonPropUnitID=-99)
    dbNite <- data.frame(subSiteID=tt$trapPositionID,measureTime=tt$EndTime,nightProp=tt$nightProp,nightPropUnitID=-99)
    dbFLen <- data.frame(subSiteID=tt$trapPositionID,measureTime=tt$EndTime,wmForkLength=tt$wmForkLength,wmForkLengthUnitID=-99)
        
    DF <- model.info[[3]]
    DF <- DF[!duplicated(DF$batchDate2),]
    DF <- DF[order(DF$batchDate2),]
        
        
    png(paste0(plot.file,"-",trap,".png"),res=400,width=4*(nCovars + 1),height=6,units="in")
          
      par(mfcol=c(2,nCovars + 1))
          
      #   ---- Plot covariate-specific stuff.  
      if(nCovars > 0){
        for(cc in 1:nCovars){
          covarPlot(plotCovars[cc],obs.eff.df,get(covarlu[plotCovars[cc]]),trap,eff.ind.inside,bsplBegDt)
        }
      }
   
      #   ---- Get a plot of observed versus a prediction curve.  
      yM <- max(tmp.df$nCaught / tmp.df$nReleased)
      plot(tmp.df$batchDate2,tmp.df$nCaught / tmp.df$nReleased,type="p",pch=19,col="red",ylim=c(0,yM),xaxt='n',yaxt='n',xlab=NA,ylab=NA,cex=(tmp.df$nReleased)/mean((tmp.df$nReleased)))
      par(new=TRUE)
      plot(DF$batchDate2,DF$p,type="l",pch=19,col="blue",ylim=c(0,yM),xlab="Date",ylab='Efficiency (0.0 - 1.0)',main=attr(df,"site.name"))
      legend("topright",c("Observed","Predicted"),pch=c(19,NA),lwd=c(NA,1),col=c("red","blue"))
          
      #   ---- 'Plot' some statistics of interest.
      O <- capture.output(summary(fit))
          
      plot(1,col="white")
      for(i in 1:length(O)){
        text(0.6,0.7 + 0.7/length(O)*(length(O) - i),O[i], pos=4, family="mono",cex=0.6)
      }
          
    dev.off()
    par(mfcol=c(1,1))
        
        
    #   ---- JASON TURNS OFF FOR ENHANCED MODEL BETA OBTAINMENT.        
    # #   ---- If you want to use observed efficiency on days when efficiency trials were run, uncomment.
    # #miss.eff.inside <- ind.inside & !ind  # missing efficiencies inside first and last trials, sized same as df
    # #miss.eff <- miss.eff.inside[ind.inside]      # missing efficiencies inside first and last trials, sized same as pred
    # #df$efficiency[miss.eff.inside] <- pred[miss.eff]
    # 
    # #   ---- If, however, you want to use the modeled efficiency for all days, even when a trial was done, use these. 
    # df$efficiency[ind.inside] <- pred
    # 
    # #   ---- Use the mean of spline estimates for all dates outside efficiency trial season.  
    # mean.p <- mean(pred, na.rm=T)
    # df$efficiency[!ind.inside] <- mean.p
        
    #   ---- Save the fit for bootstrapping.
    fits[[trap]] <- fit  
        
    #   ---- If you want to use observed efficiency on days when efficiency trials were run, uncomment.
    #miss.eff.inside <- ind.inside & !ind  # missing efficiencies inside first and last trials, sized same as df
    #miss.eff <- miss.eff.inside[ind.inside]      # missing efficiencies inside first and last trials, sized same as pred
    #df$efficiency[miss.eff.inside] <- pred[miss.eff]
        
    #   ---- If, however, you want to use the modeled efficiency for all days, even when a trial was done, use these. 
    df$efficiency[eff.ind.inside] <- pred
        
    #   ---- Use the mean of spline estimates for all dates outside efficiency trial season.  
    mean.p <- mean(pred, na.rm=T)
    df$efficiency[!eff.ind.inside] <- mean.p
        
    #   ---- Save the fit for bootstrapping.
    fits[[trap]] <- fit  
        
    #   ---- Save the raw efficiency data.  
    obs.data[[trap]] <- tmp.df
    eff.type[[trap]] <- 5
    
    #   ---- Uncomment the following line if using imputed value for all days.  Otherwise, comment it out, 
    #   ---- and imputed.eff will tell which are observed.  With the following uncommented, you can find 
    #   ---- efficiency trials in grand.df with !is.na(grand.df$nReleased).
    ind <- rep(F, nrow(df))   
      
    df$imputed.eff <- factor( !ind, levels=c(T,F), labels=c("Yes", "No"))
    df$trapPositionID <- trap
      
    ans <- rbind(ans, df)
    
    #   ---- Summarize the betas in the final model.  
    finalBetas <- coefficients(fit)
    finalBetas <- finalBetas[!(grepl("tmp.bs",names(finalBetas),fixed=TRUE))]
    
    finalBetasNum <- possibleVars %in% names(finalBetas)
    finalBetasNum[finalBetasNum == TRUE] <- finalBetas
    finalLogOddsNum <- c(7,exp(finalBetasNum))
    finalBetasNum <- c(6,finalBetasNum)

    #   ---- Compile which variables entered.  
    v <- data.frame(trap,rbind(initialVarsNum,interimVars1Num,interimVars2Num,finalVarsNum,finalBetasNum,finalLogOddsNum))
    v$threshold <- atLeast
    v$available <- m.i
    varSummary <- rbind(varSummary,v)

  }
  
  attr(ans,"subsites") <- attr(obs.eff.df, "subsites")
  attr(ans,"site.name") <- attr(obs.eff.df, "site.name")
  
  #   ---- Clean up varSummary for output.  
  colnames(varSummary) <- c("subsiteID","numStage",possibleVars,"threshold","available")
  varSummary$Stage <- NA
  varSummary[varSummary$numStage == 2,]$Stage <- "Initial"
  varSummary[varSummary$numStage == 3,]$Stage <- "Interim (Temp)"
  varSummary[varSummary$numStage == 4,]$Stage <- "Interim (Flow)"
  varSummary[varSummary$numStage == 5,]$Stage <- "Final"
  varSummary[varSummary$numStage == 6,]$Stage <- "Final Model Betas"
  varSummary[varSummary$numStage == 7,]$Stage <- "Final Model Odds Ratios"
  varSummary$numStage <- NULL
  rownames(varSummary) <- NULL
  
  #   ---- Export these data in a special spot, ready to go for more R processing.
  here <- "L:/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Outputs/Holding/"
  save(varSummary,file=paste0(here,"varSummary_",site,".RData"))
  
  
  # #   ---- Make a plot if called for.
  # if( !is.na(plot.file) ) {
  #   out.fn <- F.plot.eff.model( ans, plot.file )
  # } else {
  #   out.fn <- NULL
  # }
  
  cat("Observed efficiency data used in efficiency models.\n")
  print(obs.data)
  cat("\n")
  
  #ans <- list(eff=ans, fits=fits, X=all.X, ind.inside=all.ind.inside, X.dates=all.dts, obs.data=obs.data, eff.type=eff.type)
  #attr(ans, "out.fn.list") <- out.fn
  
  save("fits",file=paste0(plot.file,".RData"))
  #ans
  
}
