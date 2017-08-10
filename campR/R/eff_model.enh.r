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
      df[[ii]] <- queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="D",plot=TRUE)
      df[[ii]]$date <- strptime(df[[ii]]$date,format="%Y-%m-%d",tz=time.zone)
    } else {
      df[[ii]] <- queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="U",plot=TRUE)
      #df[[ii]]$date <- head(strptime(df[[ii]]$date,format="%Y-%m-%d %H:%M:%s",tz=time.zone))
      
      if(sum(!is.na(df[[ii]]$flow_cfs)) > 0){
        df[[ii]][df[[ii]]$flow_cfs <= -9997 & !is.na(df[[ii]]$flow_cfs),]$flow_cfs <- NA
      }
      if(sum(!is.na(df[[ii]]$temp_c)) > 0){
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
      
    source("L:/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/R/getCAMPEnvCov.R")
    source("L:/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/R/estCovar.R")
    source("L:/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/R/plot.bs.spline.R")
    
    #   ---- Query this river's Access database for information recorded at the trap.  For now, we only use this 
    #   ---- for turbidity.  I name objects that respect this.  
    db <- get( "db.file", envir=.GlobalEnv )
    ch <- odbcConnectAccess(db)
      dbDisc <- getCAMPEnvCov("discharge","dischargeUnitID",12)                     # <--- not sure on the UnitID.  could be 13 too?
      dbDpcm <- getCAMPEnvCov("waterDepth","waterDepthUnitID",3)                    # <--- depth in cm (American)
      dbDpft <- getCAMPEnvCov("waterDepth","waterDepthUnitID",6)                    # <--- depth in feet (RBDD)
      dbATpC <- getCAMPEnvCov("airTemp","airTempUnitID",18)
      dbATpF <- getCAMPEnvCov("airTemp","airTempUnitID",19)
      dbTurb <- getCAMPEnvCov("turbidity","turbidityUnitID",20)
      dbWVel <- getCAMPEnvCov("waterVel","waterVelUnitID",8)
      dbWTpC <- getCAMPEnvCov("waterTemp","waterTempUnitID",18)                     # <--- water temp in C (American)
      dbWTmF <- getCAMPEnvCov("waterTemp","waterTempUnitID",19)                     # <--- water temp in F (RBDD)
      dbLite <- getCAMPEnvCov("lightPenetration","lightPenetration",20)             # <--- not sure on the UnitID.
      dbDOxy <- getCAMPEnvCov("dissolvedOxygen","dissolvedOxygenUnitID",36)         
      dbCond <- getCAMPEnvCov("conductivity","conductivityUnitID",36)               # <--- not sure on the UnitID.
      dbBaro <- getCAMPEnvCov("barometer","barometerUnitID",33)                     # <--- not sure on the UnitID.    
      dbWeat <- getCAMPEnvCov("weather",NA,NA)
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
      obs.eff.df$covar <- "flow_cfs"
      obs.eff.df$flow_cfs <- NA
      obs.eff.df[obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice1 == oursitevar,]$subSiteID,]$flow_cfs <- predict(m1[[ii]],as.numeric(obs.eff.df$batchDate))$y
      #df[[ii]]$pred_flow_cfs <- predict(m1[[ii]])$y
      df[[ii]]$pred_flow_cfs <- predict(m1[[ii]],x=as.numeric(df[[ii]]$date))$y
      
      #    ---- See if we have any predicted values outside the range for which we have data.
      if(sum(df[[ii]]$date < min.date.flow | df[[ii]]$date > max.date.flow) > 0){
        df[[ii]][df[[ii]]$date < min.date.flow | df[[ii]]$date > max.date.flow,]$pred_flow_cfs <- NA
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
      
      #    ---- See if we have any predicted values outside the range for which we have data.  Off by a day..?  Daylight savings?  So buffer.
      if(sum(obs.eff.df$batchDate + 60*60 < min.date.temp | obs.eff.df$batchDate - 60*60 > max.date.temp) > 0){
        obs.eff.df[obs.eff.df$batchDate + 60*60 < min.date.temp | obs.eff.df$batchDate - 60*60 > max.date.temp,]$temp_c <- NA
      }
    }
    
    #   ---- Next for data from the CAMP db, which could be collected per subSiteID.  Reduce to the set of subsiteIDs in this run.  This 
    #   ---- is necessary for the Feather, which has many sites that branch into individual subSiteIDs.  Do this for each covar.
    
    #dbCov <- dbCovar[[ii]]
    
    #   ---- Now, bring in smoothing-spline estimated values.  
    
    obs.eff.df <- estCovar(dbDisc,"discharge_cfs",1,traps,obs.eff.df)                   # <--- not sure on the UnitID.  could be 13 too?
    obs.eff.df <- estCovar(dbDpcm,"waterDepth_cm",1,traps,obs.eff.df)                   # <--- depth in cm (American)
    obs.eff.df <- estCovar(dbDpft,"waterDepth_ft",1,traps,obs.eff.df)                   # <--- depth in feet (RBDD)
    obs.eff.df <- estCovar(dbATpC,"airTemp_C",1,traps,obs.eff.df)
    obs.eff.df <- estCovar(dbATpF,"airTemp_F",1,traps,obs.eff.df)
    obs.eff.df <- estCovar(dbTurb,"turbidity_ntu",1,traps,obs.eff.df)
    obs.eff.df <- estCovar(dbWVel,"waterVel",1,traps,obs.eff.df)
    obs.eff.df <- estCovar(dbWTpC,"waterTemp_C",1,traps,obs.eff.df)                     # <--- water temp in C (American)
    obs.eff.df <- estCovar(dbWTmF,"waterTemp_F",1,traps,obs.eff.df)                     # <--- water temp in F (RBDD)
    obs.eff.df <- estCovar(dbLite,"lightPenetration_ntu",1,traps,obs.eff.df)            # <--- not sure on the UnitID.
    obs.eff.df <- estCovar(dbDOxy,"dissolvedOxygen_mgL",1,traps,obs.eff.df)        
    obs.eff.df <- estCovar(dbCond,"conductivity_mgL",1,traps,obs.eff.df)                # <--- not sure on the UnitID.
    obs.eff.df <- estCovar(dbBaro,"barometer_inHg",1,traps,obs.eff.df)                  # <--- not sure on the UnitID.    
    obs.eff.df <- estCovar(dbWeat,"precipLevel_qual",2,traps,obs.eff.df)
  
    obs.eff.df <- obs.eff.df[order(obs.eff.df$TrapPositionID,obs.eff.df$batchDate),]

  }
  
  #   ---- Look at how the full models work out with respect to different traps.  
  table(obs.eff.df[!is.na(obs.eff.df$efficiency),]$covar,obs.eff.df[!is.na(obs.eff.df$efficiency),]$TrapPositionID)

  
  
  
  #   ---- Preserve for use in making plots below.  
  #df.covar <- df                  # <--- The whole list.
  #df.covar.turb <- allTurb        # <--- A data frame.
 

  
  #   ---- Estimate a model for efficiency for each trap in obs.eff.df.
  for( trap in traps ){
    
    df <- obs.eff.df[ is.na(obs.eff.df$TrapPositionID) | (obs.eff.df$TrapPositionID == trap), ]
    ind <- !is.na(df$efficiency)
    
    
    
    
    # JASON PLAYS AROUND AND ENSURES A DATE REPEATS OVER TWO YEARS.
    df[!is.na(df$efficiency),]$batchDate[15] <- df[!is.na(df$efficiency),]$batchDate[1]
    
    
    
    
    
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
    
    #   ---- We defined the start and end for the fishing season.  Need this to know when we need an efficiency estimate.
    #   ---- But we need to find the actual temporal range of efficiency dates, so the boundary points of the spline are
    #   ---- appropriate for efficiency effort.  Otherwise, the boundary points in the efficiency spline uses the 
    #   ---- overall min and max of fishing, which isn't correct.  
    eff.strt.dt <- min(tmp.df$batchDate2)
    eff.end.dt <- max(tmp.df$batchDate2)
    eff.ind.inside <- (eff.strt.dt <= df$batchDate2) & (df$batchDate2 <= eff.end.dt)
    eff.inside.dates <- c(eff.strt.dt,eff.end.dt)
    
    #   ---- With the inclusion of all CAMP covariates, the probability increases by a lot that we don't have all 
    #   ---- the values over all time.  Chuck those that don't have at least ... 90% of the data rows, given a 
    #   ---- trap.  Note that this considers NA WITHIN a column.  If we delete, we have to update tmp.df$covar.
    atLeast <- floor(0.90*m.i) + 1
    vars <- unlist(strsplit(tmp.df$covar[1]," + ",fixed=TRUE)[[1]])
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
    goodInd <- tmp.df$allCovars            #  <---- Apply this vector to tmp.bs below.
    dataDeficient <- tmp.df[tmp.df$allCovars == 0,]
    cat( paste0("Trap ",trap," has ",nrow(dataDeficient)," rows of data covariate deficient, out of ",nrow(tmp.df)," originally present.\n") )
    write.csv(dataDeficient,paste0(plot.file,"-",trap,"dataDeficient.csv"),row.names=FALSE)
    tmp.df <- tmp.df[tmp.df$allCovars == 1,]
    tmp.df$allCovars <- NULL     # This has served its purpose.  
    
    
    
    #   ---- At this point, have cleaned up the this trap's efficiency data.  NOW, define fish-start days, so that we can
    #   ---- model via the number of days from the start of fishing.  This puts efficiency years "on top of each other."  
    #   ---- I *could* have deleted, because of bad covariates, the *true* start of the fishing period for a certain 
    #   ---- year.  I'm okay with this I think.  
    
    
    par(mfrow=c(1,2))
    plot(tmp.df$batchDate,tmp.df$efficiency,col=c("red","green","blue")[as.factor(tmp.df$fishPeriod)],pch=19,cex=3)
    plot(tmp.df$batchDate2,tmp.df$efficiency,col=c("red","green","blue")[as.factor(tmp.df$fishPeriod)],pch=19,cex=3)
    par(mfrow=c(1,1))
    
    
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
        X <- matrix( 1, sum(ind.inside), 1)
      }
      
      #   ---- Save X, and the dates at which we predict, for bootstrapping.
      all.X[[trap]] <- X   
      all.dts[[trap]] <- df$batchDate[ind.inside] 
      
    } else {    
      
      #   ---- There are enough observations to estimate B-spline model.
      
      #   ---- Fit glm model, increasing degress of freedom, until minimize AIC or something goes wrong.  
      cat(paste("\n\n++++++Spline model fitting for trap:", trap, "\n Trials are:"))
      print(tmp.df)
      
      #   ---- At least one efficiency trial "inside" for this trap.
      #   ---- Fit a null model.  
      fit <- glm( nCaught / nReleased ~ 1, family=binomial, data=tmp.df, weights=tmp.df$nReleased ) 
      fit.AIC <- AIC(fit)
      
      cat(paste("df= ", 1, ", conv= ", fit$converged, " bound= ", fit$boundary, " AIC= ", round(fit.AIC, 4), "\n"))
      
      if( !fit$converged | fit$boundary ){
        
        #   ---- Something went wrong with the constant model. 
        #		---- I don't think this can actually happen because m.i > 10 and sum(nCaught) > 0, 
        #   ---- but I'm adding this clause just in case (maybe something is missing).
        #		---- In this case, use ROM+1 estimator.
        cat("Constant (intercept-only) logistic model for efficiency failed. Using 'ROM+1' estimator. ")
        obs.mean <- (sum(tmp.df$nCaught)+1) / (sum(tmp.df$nReleased)+1)   
        cat(paste("'ROM+1' efficiency= ", obs.mean, "\n"))
        
        df$efficiency <- obs.mean
        
        fits[[trap]] <- data.frame(nCaught=df$nCaught[ind], nReleased=df$nReleased[ind])
        eff.type[[trap]] <- 3
        
      } else {
        
        #		---- Fit increasingly complex models. 
        #				 Note, we skip the quadratic:
        #					df = 3 means cubic model (no internal knots)
        #				  df = 4 means cubic spline w/ 1 internal knot at median
        #					df = 5 means cubic spline w/ 2 internal knots at 0.33 and 0.66 of range
        #					etc.
        #				 Subtract 3 from df to get number of internal knots.
        
        cur.df <- 3
        repeat{
          
          #cur.bspl <- bs( df$batchDate[ind.inside], df=cur.df )
          #tmp.bs <- cur.bspl[!is.na(df$efficiency[ind.inside]),]
          
          #   ---- Note:  ind.inside for these enhanced efficiency models should be the julian dates
          #   ---- inside the min and max fishing days...not what it's been historically.  We are not
          #   ---- applying to catch data, so these intercept pre- and post- fits don't matter.  
          
          #   ---- Note I use a unique here...we expect batchDate duplicates when reducing from many 
          #   ---- years' data to our 1969-1970 year.
          cur.bspl <- bs( df$batchDate2[eff.ind.inside], df=cur.df )
          tmp.bs <- cur.bspl[!is.na(df$efficiency[eff.ind.inside]),]
          
          cur.fit <- glm( nCaught / nReleased ~ tmp.bs, family=binomial, data=tmp.df, weights=tmp.df$nReleased )   
          cur.AIC <- AIC(cur.fit)
          
          cat(paste("df= ", cur.df, ", conv= ", cur.fit$converged, " bound= ", cur.fit$boundary, " AIC= ", round(cur.AIC, 4), "\n"))
          
          if( !cur.fit$converged | cur.fit$boundary | cur.df > max.df.spline | cur.AIC > (fit.AIC - 2) ){
            break
          } else {
            fit <- cur.fit
            fit.AIC <- cur.AIC
            bspl <- cur.bspl
            cur.df <- cur.df + 1
          }
        }
        
        cat("\nFinal Efficiency model for trap: ", trap, "\n")
        print(summary(fit, disp=sum(residuals(fit, type="pearson")^2)/fit$df.residual))
        
        plot.bs.spline(bspl,fit,bsplBegDt,bsplEndDt,tmp.df)
        
        #   ---- Make a design matrix for ease in calculating predictions.
        if( length(coef(fit)) <= 1 ){
          pred <- matrix( coef(fit), sum(ind.inside), 1 )
          X <- matrix( 1, sum(ind.inside), 1)
        } else {
          X <- cbind( 1, bspl )
          pred <- X %*% coef(fit)
        }
        
        #   ---- Save X, and the dates at which we predict, for bootstrapping.
        all.X[[trap]] <- X   
        all.dts[[trap]] <- df$batchDate[ind.inside]   
        
        #   ---- Standard logistic prediction equation.  
        #   ---- "Pred" is all efficiencies for dates between min and max of trials.
        pred <- 1 / (1 + exp(-pred))  
        
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
        
      } 

      #   ---- Save the raw efficiency data.  
      obs.data[[trap]] <- tmp.df
      eff.type[[trap]] <- 5
      
      
      
      
      #   ---- We now need to fit an updated spline, based on covariates.  
      
      
      
      
      
    }
    
    #   ---- Uncomment the following line if using imputed value for all days.  Otherwise, comment it out, 
    #   ---- and imputed.eff will tell which are observed.  With the following uncommented, you can find 
    #   ---- efficiency trials in grand.df with !is.na(grand.df$nReleased).
    ind <- rep(F, nrow(df))   
    
    df$imputed.eff <- factor( !ind, levels=c(T,F), labels=c("Yes", "No"))
    df$trapPositionID <- trap
    
    ans <- rbind(ans, df)
  
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #   ---- There are enough observations to estimate B-spline model -- with covariates!
  
  #   ---- Fit glm model, increase degrees of freedom, until minimize AIC or something goes wrong.  
  cat(paste("\n\n++++++Spline model fitting for trap:", trap, "\n Trials are:"))
  print(tmp.df)
  
  
  
  
  
  
  covarMat <- as.matrix(tmp.df[,strsplit(tmp.df$covar[1]," + ",fixed=TRUE)[[1]]])
  
  #   ---- If we have only one covariate, make sure the vector (not matrix) is named for clarity in models.
  if(dim(covarMat)[2] == 1){
    colnames(covarMat) <- tmp.df$covar[1]
  }
  covar <- tmp.df$covar[1]
  cat(paste0("\n\n++++++ ...and covariates include",covar,".\n"))
  
  #   ---- See if our smoothing splines lead to non-sensical estimates; i.e., negative values for 
  #   ---- either of flow (cfs) or turbidity (ntu).  But not temperature!  Others?
  if("temp_c" %in% colnames(covarMat)){
    
    theNames <- colnames(covarMat)
    theNames <- theNames[theNames != "temp_c"]
    
    #   ---- Without temp_c.  I assume temp_c is the only var for which negatives are permissable.  
    covarMat <- cbind(apply(as.matrix(covarMat[,colnames(covarMat) != "temp_c"]),2,function(x) ifelse(x < 0,0,x)),as.matrix(covarMat[,colnames(covarMat) == "temp_c"]))
    colnames(covarMat) <- c(theNames,"temp_c")
  } else {
    covarMat <- apply(covarMat,2,function(x) ifelse(x < 0,0,x))
  }
  
  #   ---- At least one efficiency trial "inside" for this trap.
  #   ---- Fit a null model (with our covariates).  
  fit <- glm( nCaught / nReleased ~ 1 + covarMat, family=binomial, data=tmp.df, weights=tmp.df$nReleased ) 
  fit.AIC <- AIC(fit)
  
  cat(paste("df= ", 1, ", conv= ", fit$converged, " bound= ", fit$boundary, " AIC= ", round(fit.AIC, 4), "\n"))
  
  if( !fit$converged | fit$boundary ){
    
    #   ---- Something went wrong with the constant model. 
    #		---- I don't think this can actually happen because m.i > 10 and sum(nCaught) > 0, 
    #   ---- but I'm adding this clause just in case (maybe something is missing).
    #		---- In this case, use ROM+1 estimator.
    cat("Constant (intercept-only) logistic model for efficiency failed. Using 'ROM+1' estimator. ")
    obs.mean <- (sum(tmp.df$nCaught)+1) / (sum(tmp.df$nReleased)+1)   
    cat(paste("'ROM+1' efficiency= ", obs.mean, "\n"))
    
    df$efficiency <- obs.mean
    
    fits[[trap]] <- data.frame(nCaught=df$nCaught[ind], nReleased=df$nReleased[ind])
    eff.type[[trap]] <- 3
    
  } else {
    
    
    
    #   ---- Figure out covariates.  
    fit <- 
      
      
      
      
      
      
      #		---- Fit increasingly complex models. 
      #				 Note, we skip the quadratic:
      #					df = 3 means cubic model (no internal knots)
      #				  df = 4 means cubic spline w/ 1 internal knot at median
      #					df = 5 means cubic spline w/ 2 internal knots at 0.33 and 0.66 of range
    #					etc.
    #				 Subtract 3 from df to get number of internal knots.
    
    cur.df <- 3
    repeat{
      
      #cur.bspl <- bs( df$batchDate[ind.inside], df=cur.df )
      #tmp.bs <- cur.bspl[!is.na(df$efficiency[ind.inside]),]
      #tmp.bs <- tmp.bs[as.logical(goodInd),]     # <--- Keep rows with all good covariates.
      
      cur.bspl <- bs( df$fishDay[ind.inside], df=cur.df )
      tmp.bs <- cur.bspl[!is.na(df$efficiency[ind.inside]),]
      tmp.bs <- tmp.bs[as.logical(goodInd),]     # <--- Keep rows with all good covariates.  tmp.df already reduced.
      
      cur.fit <- glm( nCaught / nReleased ~ covarMat + tmp.bs, family=binomial, data=tmp.df, weights=tmp.df$nReleased )   
      cur.AIC <- AIC(cur.fit)
      
      cat(paste("df= ", cur.df, ", conv= ", cur.fit$converged, " bound= ", cur.fit$boundary, " AIC= ", round(cur.AIC, 4), "\n"))
      
      if( !cur.fit$converged | cur.fit$boundary | cur.df > max.df.spline | cur.AIC > (fit.AIC - 2) ){
        #bspl <- cur.bspl
        break
      } else {
        fit <- cur.fit
        fit.AIC <- cur.AIC
        bspl <- cur.bspl
        cur.df <- cur.df + 1
      }
    }
    
    cat("\nFinal Efficiency model for trap: ", trap, "\n")
    print(summary(fit, disp=sum(residuals(fit, type="pearson")^2)/fit$df.residual))
    
    #   ---- Make a design matrix for ease in calculating predictions.
    if( length(coef(fit)) <= 1 ){
      pred <- matrix( coef(fit), sum(ind.inside), 1 )
      X <- matrix( 1, sum(ind.inside), 1)
    } else {
      pred <- predict(fit)
      
      
      # pred2 <- predict(fit,newdata=seq(min(fit$data$batchDate),max(fit$data$batchDate),by=60*60*24))
      # pred2 <- predict(fit,newdata=as.data.frame(rep(1,nrow(tmp.bs)),tmp.bs))
      # pred2 <- predict(fit,newdata=cur.bspl)
      # 
      # #pdat <- data.frame(x = seq(min(fit$data$batchDate),max(fit$data$batchDate),by=60*60*24))
      # ## predict for new `x`
      # newX <- seq(min(fit$data$batchDate),max(fit$data$batchDate),by=60*60*24)
      # 
      # 
      # bs(newX,cur.df)
      # 
      # 
      # 
      # nX <- length(newX)
      # pdat <- transform(pdat,yhat=predict(fit,newdata=data.frame(covarMatflow_cfs=rep(0.1,nX),covarMatturbidity_ntu=rep(0.1,nX),batchDate=newX))#cbind(covarMatflow_cfs=rep(0.1,nX),covarMatturbidity_ntu=rep(0.1,nX),x=newX)))
      # 
      
      
      
      #X <- cbind( 1, bspl ) #cbind( 1, bspl, flow_cfs, temp_c )
      #pred <- X %*% coef(fit)
    }
    
    #   ---- JASON TURNS OFF FOR ENHANCED MODEL BETA OBTAINMENT.
    # #   ---- Save X, and the dates at which we predict, for bootstrapping.
    # all.X[[trap]] <- X   
    # all.dts[[trap]] <- df$batchDate[ind.inside]   
    
    #   ---- Standard logistic prediction equation.  
    #   ---- "Pred" is all efficiencies for dates between min and max of trials.
    pred <- 1 / (1 + exp(-pred))  
    
    #   ---- Make a very boring plot to assess goodness-of-fit of prediction. 
    png(paste0(plot.file,"-",trap,".png"),res=400,width=16,height=6,units="in")
    par(mfcol=c(2,4))
    
    #   ---- Plot of efficiency versus variable 1. 
    if("flow_cfs" %in% names(tmp.df)){
      plot(tmp.df$flow_cfs,tmp.df$nCaught / tmp.df$nReleased,type="p",pch=19,col=c("pink","red")[as.factor(tmp.df$allCovars)],xlab="Flow cfs",ylab='Efficiency (0.0 - 1.0)',main=paste0("Flow cfs at ",attr(df,"site.name")))
    } else {
      plot(1,1)
    }
    
    #   ---- Plot of variable 1 versus time with smoother. Currently assumes 1 entry in the list df.covar.
    if("flow_cfs" %in% names(tmp.df)){
      ym <- min(df.covar[[1]][!is.na(df.covar[[1]]$flow_cfs),]$flow_cfs)
      yM <- max(df.covar[[1]][!is.na(df.covar[[1]]$flow_cfs),]$flow_cfs) 
      plot(df.covar[[1]]$date,df.covar[[1]]$flow_cfs,xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(ym,yM),type="p",cex=0.5,pch=19,col="black",main=paste0("Qry Flow cfs at ",attr(df,"site.name")))
      par(new=TRUE)
      plot(df.covar[[1]]$date,df.covar[[1]]$pred_flow_cfs,xlab='Date',ylab='Flow (cfs)',ylim=c(ym,yM),col="red",type="l")
    } else {
      plot(1,1)
    }
    
    #   ---- Plot of efficiency versus variable 2.
    if("temp_c" %in% names(tmp.df)){
      plot(tmp.df$temp_c,tmp.df$nCaught / tmp.df$nReleased,type="p",pch=19,col=c("lightblue","blue")[as.factor(tmp.df$allCovars)],xlab="Temperature C",ylab='Efficiency (0.0 - 1.0)',main=paste0("Temp C at ",attr(df,"site.name")))
    } else {
      plot(1,1)
    }
    
    #   ---- Plot of variable 2 versus time with smoother. Currently assumes 1 entry in the list df.covar.
    if("temp_c" %in% names(tmp.df)){
      ym <- min(df.covar[[1]][!is.na(df.covar[[1]]$temp_c),]$temp_c)
      yM <- max(df.covar[[1]][!is.na(df.covar[[1]]$temp_c),]$temp_c) 
      plot(df.covar[[1]]$date,df.covar[[1]]$temp_c,xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(ym,yM),type="p",cex=0.5,pch=19,col="black",main=paste0("Qry Temp C at ",attr(df,"site.name")))
      par(new=TRUE)
      plot(df.covar[[1]]$date,df.covar[[1]]$pred_temp_c,xlab='Date',ylab='Temp (C)',ylim=c(ym,yM),col="red",type="l")
    } else {
      plot(1,1)
    }
    
    #   ---- Plot of efficiency versus variable 3.
    if("turbidity_ntu" %in% names(tmp.df)){
      plot(tmp.df$turbidity_ntu,tmp.df$nCaught / tmp.df$nReleased,type="p",pch=19,col=c("lightblue","blue")[as.factor(tmp.df$allCovars)],xlab="Turbidity NTU",ylab='Efficiency (0.0 - 1.0)',main=paste0("Turbidity ntu at ",attr(df,"site.name")))
    } else {
      plot(1,1)
    }
    
    #   ---- Plot of variable 3 versus time with smoother. Currently assumes 1 entry in the list df.covar.
    if("turbidity_ntu" %in% names(tmp.df)){
      xm <- min(df.covar.turb[!is.na(df.covar.turb$turbidity),]$measureTime)
      xM <- max(df.covar.turb[!is.na(df.covar.turb$turbidity),]$measureTime) 
      ym <- min(df.covar.turb[!is.na(df.covar.turb$turbidity),]$turbidity)
      yM <- 50#max(df.covar.turb[!is.na(df.covar.turb$turbidity),]$turbidity) 
      theJJ <- unique(dbTurb$subSiteID)   # i coded for all traps, but really just 
      theJJ <- theJJ[theJJ == trap]       # want the one here.
      col <- c("red","orange","green","blue","purple","brown","black")
      
      for(jj in 1:length(theJJ)){
        jTurb <- theJJ[jj]
        if(jj > 1){
          par(new=TRUE)
        } 
        plot(df.covar.turb[df.covar.turb$subSiteID == jTurb,]$measureTime,df.covar.turb[df.covar.turb$subSiteID == jTurb,]$turbidity,xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(xm,xM),ylim=c(ym,yM),type="p",cex=0.25,pch=19,col="black",main=paste0("Qry Turbidity NTU at ",attr(df,"site.name")))
        par(new=TRUE)
        plot(df.covar.turb[df.covar.turb$subSiteID == jTurb,]$measureTime,df.covar.turb[df.covar.turb$subSiteID == jTurb,]$pred_turbidity_ntu,xlab='Date',ylab='Turbidity (NTU)',xlim=c(xm,xM),ylim=c(ym,yM),col=col[jj],type="l")
      }
    } else {
      plot(1,1)
    }
    
    #   ---- Get a plot of observed versus a prediction curve.  
    yM <- max(tmp.df$nCaught / tmp.df$nReleased)
    plot(tmp.df$batchDate,tmp.df$nCaught / tmp.df$nReleased,type="p",pch=19,col="red",ylim=c(0,yM),xaxt='n',yaxt='n',xlab=NA,ylab=NA)
    par(new=TRUE)
    plot(tmp.df$batchDate,pred,type="l",pch=19,col="blue",ylim=c(0,yM),xlab="Date",ylab='Efficiency (0.0 - 1.0)',main=attr(df,"site.name"))
    legend("topright",c("Observed","Predicted"),pch=c(19,NA),lwd=c(NA,1),col=c("red","blue"))
    
    #   ---- 'Plot' some statistics of interest.
    #plot(1,xaxt="n",yaxt="n",type="n",xlab="",ylab="",xlim=c(0, 10),ylim=c(0, 10))
    X <- capture.output(summary(fit))
    
    plot(1,col="white")
    for(i in 1:length(X)){
      text(0.6,0.7 + 0.7/length(X)*(length(X) - i),X[i], pos=4, family="mono",cex=0.6)
    }
    
    
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
    
  } 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  attr(ans,"subsites") <- attr(obs.eff.df, "subsites")
  attr(ans,"site.name") <- attr(obs.eff.df, "site.name")
  
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
