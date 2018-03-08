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
#'   
#' @param plot A logical indicating if raw efficiencies and the model(s)
#' should be plotted.
#' 
#' @param max.df.spline The maximum degrees of freedom allowed for splines.
#' 
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
#' @seealso \code{F.get.releases}, \code{F.bootstrap.passage}, \code{reMap},
#'   \code{reMap2}
#'   
#' @author WEST Inc.
#' 
#' @examples 
#' \dontrun{
#' #   ---- Fit an efficiency model for each unique trapPositionID 
#' #   ---- in data frame obs.eff.df.  
#' F.efficiency.model( obs.eff.df, plot=T, max.df.spline=4, plot.file=NA)
#' }
F.efficiency.model <- function( obs.eff.df, plot=T, max.df.spline=4, plot.file=NA ){
  
  # obs.eff.df <- eff
  # plot <- plot
  # max.df.spline <- 4
  # plot.file <- plot.file

  #save.image("C:/Users/jmitchell/Desktop/save/rbdd.RData")
  
  ans <- NULL
  
  useEnhEff <- attr(obs.eff.df,"useEnhEff")
  min.date <- attr(obs.eff.df,"min.date")
  max.date <- attr(obs.eff.df,"max.date")
  site <- attr(obs.eff.df,"site")
  subsites <- attr(obs.eff.df,"subsites")
  site.name <- attr(obs.eff.df,"site.name")
  catch.subsites <- attr(obs.eff.df,"catch.subsites")
  
  # ---- If we are using enhanced efficiency trials, we should always have an efficiency model,
  # ---- unless a trap is new for the year defined by original min.date and max.date.  Define
  # ---- traps vector appropriately so the looping works appropriately.
  if(useEnhEff == TRUE){
    traps <- catch.subsites
  } else {
    traps <- as.character(droplevels(sort(unique(obs.eff.df$TrapPositionID))))
  }
  
  fits <- all.X <- all.ind.inside <- all.dts <- obs.data <- eff.type <- vector("list", length(traps))
  names(fits) <- traps
  names(all.X) <- traps
  names(all.dts) <- traps
  names(all.ind.inside) <- traps
  names(obs.data) <- traps
  names(eff.type) <- traps
  
  #   ---- Obtain necessary variables from the global environment.  
  time.zone <- get("time.zone",envir=.GlobalEnv)
  
  #   ---- Set this up here, so it can evaluated in the if below when useEnhEff == FALSE.  
  doOldEff <- rep(TRUE,length(traps))
  names(doOldEff) <- traps
  
  #   ---- Decide if we're going to use enhanced efficiency.  
  if(useEnhEff == TRUE){
    
    #   ---- Get stuff we need to fit the enhanced efficiency models.  
    
    #   ---- 1.  We know traps from immediately above.
    #load("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/data/betas.rda")
    data(betas,envir=environment())
    betas <- betas[betas$subsiteID %in% traps,]
    
    #   ---- Get together covariates.  We need to query for days before and after the first and last 
    #   ---- eff trial date, so we need to make the obs.eff.df "bigger."  
    big.obs.eff.df <- F.assign.batch.date(data.frame(EndTime=seq(as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone) - 90*24*60*60,
                                                                 as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone) + 90*24*60*60,by="2 hours")))
    big.obs.eff.df$EndTime <- NULL
    big.obs.eff.df <- data.frame(batchDate=unique(big.obs.eff.df$batchDate))
    all.Dates <- expand.grid(TrapPositionID=traps,batchDate=big.obs.eff.df$batchDate)
    
    all.Dates <- all.Dates[!(all.Dates$batchDate %in% obs.eff.df$batchDate),]
    otherCols <- names(obs.eff.df)[!(names(obs.eff.df) %in% c("TrapPositionID","batchDate"))]
    all.Dates[otherCols] <- NA
    
    #   ---- It could be that all.Dates has more traps than obs.eff.df, if we have fish in catch 
    #   ---- from a trap on which there were no eff trials this year.  Find these traps.   
    extraTraps <- unique(all.Dates$TrapPositionID)[!(unique(all.Dates$TrapPositionID) %in% unique(obs.eff.df$TrapPositionID))]
    
    #   ---- If we have extraTraps, add in rows to obs.eff.df, so the rbind below creates the
    #   ---- same number of rows for each trap, as expected. 
    if(length(extraTraps) > 0){
      if(is.factor(obs.eff.df$TrapPositionID)){
        theFirst <- as.character(droplevels(obs.eff.df$TrapPositionID[1]))
      } else {
        theFirst <- obs.eff.df$TrapPositionID[1]
      }
      for(i in 1:length(extraTraps)){
        extra.obs.eff.df <- data.frame(TrapPositionID=extraTraps[i],
                                       batchDate=obs.eff.df[obs.eff.df$TrapPositionID == theFirst,]$batchDate,
                                       nReleased=NA,
                                       nCaught=NA,
                                       bdMeanNightProp=NA,
                                       bdMeanMoonProp=NA,
                                       bdMeanForkLength=NA,
                                       efficiency=NA)
        obs.eff.df <- rbind(obs.eff.df,extra.obs.eff.df)
      }
    }
  
    #   ---- Combine the original obs.eff.df from eff with all.Dates, so we have bigger date ranges
    #   ---- for use with splines.  
    obs.eff.df <- rbind(obs.eff.df,all.Dates)
    obs.eff.df <- obs.eff.df[order(obs.eff.df$TrapPositionID,obs.eff.df$batchDate),]
    
    #   ---- Get covariate data on bigger obs.eff.df.  
    stuff <- getTogetherCovarData(obs.eff.df,min.date,max.date,traps,useEnhEff=TRUE)

    #   ---- Unpack 'stuff' so that we have the dbCovar dataframes available for plotting below.
    obs.eff.df <- stuff$obs.eff.df
    obs.eff.df$covar <- NULL         # This only hold meaning when building enhanced eff models.  
    dbDisc <- stuff$dbDisc
    dbDpcm <- stuff$dbDpcm
    dbATpF <- stuff$dbATpF
    dbTurb <- stuff$dbTurb
    dbWVel <- stuff$dbWVel
    dbWTpC <- stuff$dbWTpC
    dbLite <- stuff$dbLite
    dbFlPG <- stuff$dbFlPG
    dbTpPG <- stuff$dbTpPG
    dbPerQ <- stuff$dbPerQ
    
    #   ---- Create a means to identify traps for which we end up lacking data.  
    doOldEff <- rep(FALSE,length(traps))
    names(doOldEff) <- traps
    
  
    #   ---- Run over individual traps.
    for(trap in traps){
    
      #   ---- Objects created in non-efficiency models -- need something for bootstrapping.  
      df <- obs.eff.df[ is.na(obs.eff.df$TrapPositionID) | (obs.eff.df$TrapPositionID == trap), ]
      ind <- !is.na(df$efficiency)
      
      #   ---- Get the temporal spline basis matrix goods.  
      #here <- "L:/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/enhEffStats"  # for testing before you have the package.
      here <- paste0(find.package("campR"),"/enhEffStats")  # <- for when you have a package.
      
      .tmpDataEnv <- new.env(parent=emptyenv()) # not exported
      
      #   ---- If a trap is new this year, we won't have enhanced efficiency prior-fit info.  
      #   ---- See if we have that stuff waiting around. 
      if(file.exists(paste0(here,"/",site,"_",trap,"_splineCoef.RData"))){
        load(paste0(here,"/",site,"_",trap,"_splineCoef.RData"),envir=.tmpDataEnv)
        load(paste0(here,"/",site,"_",trap,"_splineDays.RData"),envir=.tmpDataEnv)
        load(paste0(here,"/",site,"_",trap,"_splineBegD.RData"),envir=.tmpDataEnv)
        load(paste0(here,"/",site,"_",trap,"_splineEndD.RData"),envir=.tmpDataEnv)
        load(paste0(here,"/",site,"_",trap,"_fit.RData"),envir=.tmpDataEnv)
      
        splineCoef <- .tmpDataEnv$splineCoef
        splineDays <- .tmpDataEnv$splineDays
        splineBegD <- .tmpDataEnv$splineBegD
        splineEndD <- .tmpDataEnv$splineEndD
        fit <- .tmpDataEnv$fit
      
        #   ---- Stuff we just loaded.  
        #splineDays ...came from... df$batchDate2[eff.ind.inside]
        #splineCoef ...came from... fit$coefficients[grepl("tmp",names(fit$coefficients))]
        #splineBegD ...came from... bsplBegDt
        #splineEndD ...came from... bsplEndDt
        #fit        ...came from... fit
        
        #   ---- Remap back to the present.  
        reMap2list <- reMap2(min.date,max.date,splineDays)
        strt.dt <- reMap2list$strt.dt
        end.dt <- reMap2list$end.dt
        
        #   ---- By design, can only have the two spline 59/60 60/60 paradigms coded above.  
        
        #   ---- Identify dates for which we have splined information.  
        # ind.inside <- (strt.dt <= df$batchDate) & (df$batchDate <= end.dt)
        # inside.dates <- c(strt.dt, end.dt)
        # all.ind.inside[[trap]] <- inside.dates  # save season dates for bootstrapping
        min.date.p <- as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone)
        max.date.p <- as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone)
        ind.inside <- (max(min.date.p,as.POSIXct(strt.dt)) <= df$batchDate) & (df$batchDate <= min(max.date.p,as.POSIXct(end.dt)))
        inside.dates <- c(max(min.date.p,as.POSIXct(strt.dt)), min(max.date.p,as.POSIXct(end.dt)))
        all.ind.inside[[trap]] <- inside.dates  # save season dates for bootstrapping
        
        #  ---- The fitting data frame
        tmp.df <- df[ind & ind.inside,]
        m.i <- sum(ind & ind.inside)
        
        #   ---- 4. Find out which covariates this trap cares about.  
        thisB1 <- betas[betas$subsiteID == trap,]                                                               # Restrict to trap.
        thisB2 <- thisB1[,names(thisB1)[sapply(thisB1,function(x) ifelse(is.numeric(x),sum(x),x)) != "0"]]      # Restrict to non-zero covariates.
        
        #   ---- Get the Intercept.  
        covarI <- as.numeric(thisB2[names(thisB2) == "(Intercept)"])
        
        #   ---- Reduce down to a dataframe.  Handle the contingency when the "dataframe" is of length one, 
        #   ---- and thus drops down to a vector.  
        if(sum(!(names(thisB2) %in% c("subsiteID","(Intercept)","threshold","available","Stage"))) > 1){
          thisB3 <- thisB2[,!(names(thisB2) %in% c("subsiteID","(Intercept)","threshold","available","Stage"))] # Restrict to covariates.
        } else {
          theName <- names(thisB2)[!(names(thisB2) %in% c("subsiteID","(Intercept)","threshold","available","Stage"))] 
          thisB3 <- data.frame(thisB2[,!(names(thisB2) %in% c("subsiteID","(Intercept)","threshold","available","Stage"))])
          names(thisB3) <- theName
        }
        
        covarB <- thisB3
        if(length(names(covarB)) > 0){
          cat(paste0("\n\nEnhanced efficiency model for trap ",trap," seeks recorded data on covariates ",paste0(names(covarB),collapse=", "),".\n"))
        } else {
          cat(paste0("\n\nEnhanced efficiency model for trap ",trap," seeks no recorded data -- it's an intercept-only model.\n"))
        }
        
        #   ---- Check to make sure we have the data we need to fit enhanced efficiency trials.  Not necessary
        #   ---- if we have an intercept-only model.  
        if(length(covarB) > 0){
          checkCovarValidity <- checkValidCovars(df,tmp.df,min.date,max.date,covarB)
          df <- checkCovarValidity$df
          doEnhEff <- checkCovarValidity$doEnhEff
        } else {
          doEnhEff <- TRUE
        }
      } else{
        
        #   ---- If we are here, we have a new trap with no previous enhanced efficiency model.  If there were efficiency trials
        #   ---- this year, we could at least try to fit an eff model the old way.  So tell it to do just that.  
        doEnhEff <- FALSE 
      }
      
      #   ---- If doEnhEff == FALSE, I exit the loop for this trap.  I'll run regular eff below.
      if(doEnhEff == FALSE){
        doOldEff[names(doOldEff) == trap] <- TRUE
      } else {

        #   ---- We good to go.  
      
        #   ---- I have identified variables I care about.  Get them together, for all batchDates.
        covarC <- c("discharge_cfs","waterDepth_cm","airTemp_F","turbidity_ntu","waterVel_fts","waterTemp_C","lightPenetration_ntu")#,"dissolvedOxygen_mgL","conductivity_mgL","barometer_inHg","precipLevel_qual")
        covarE <- c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength")
      
        #   ---- See if we have any temporal spline tmp bits.  
        if(sum(grepl("tmp",names(splineCoef))) > 0){
        
          #   ---- Build the basis matrix.  This does includes the intercept.  
          N <- nrow(bs(splineDays, df=length(splineCoef)))
          tB <- c(covarI,splineCoef)
          timeX <- cbind(rep(1,N),bs( splineDays, df=length(splineCoef))) 
          timeDF <- data.frame(timeX=timeX,batchDate2=splineDays)
          names(timeDF)[names(timeDF) == "timeX.V1"] <- "Intercept"
          c0 <- timeDF[!duplicated(timeDF$batchDate2),]
    
        } else {
          
          #   ---- If we're here, the temporal spline was fit as a simple intercept.  We still 
          #   ---- need to be sure we grab the 1960 batchDate2 dates, so do that here. 
          tB <- c(covarI)
          timeDF <- data.frame(Intercept=rep(1,length(splineDays)),batchDate2=splineDays)
          c0 <- timeDF[!duplicated(timeDF$batchDate2),]
          c0 <- c0[order(c0$batchDate2),]
        }
      
        #   ---- Get the environmental covariate data.  
        if(sum(c("temp_c","flow_cfs") %in% names(covarB)) > 0){
          c1 <- df[,c("batchDate",names(covarB)[names(covarB) %in% c("temp_c","flow_cfs")])]
          c1 <- c1[,c("batchDate",names(covarB)[names(covarB) %in% c("temp_c","flow_cfs")])]
          
          #   ---- The times are off, probably because c1 originates from obs.eff.df, which has been used with dates recorded
          #   ---- in the CAMP mdb.  These dates don't record time zones, so POSIX gets tricky.  
          c1$batchDate <-  as.POSIXct(strptime(c1$batchDate,format="%Y-%m-%d",tz=time.zone),format="%Y-%m-%d",tz=time.zone)
        } else {
          c1 <- NULL
        }
        
        #   ---- Get the CAMP environmental covariate data.  
        if(sum(c("waterDepth_cm","turbidity_ntu","waterVel_fts","waterTemp_C") %in% names(covarB)[names(covarB) %in% covarC]) > 0){
          c2 <- df[,c("batchDate",names(covarB)[names(covarB) %in% covarC])]
        } else {
          c2 <- NULL
        }
           
        #   ---- Get the percent-Q covariate data.
        if(sum(c("percQ") %in% names(covarB)) > 0){
          #do stuff
          c3 <- df[,c("batchDate",names(covarB)[names(covarB) %in% "percQ"])]
        } else {
          c3 <- NULL
        }
        
        #   ---- Get the e-trial data.  
        if(sum(c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength") %in% names(covarB)) > 0){
          
          #   ---- Variable batchDate doesn't include all dates, since releases average over days.  Fill in the missing 
          #   ---- dates.  This creates a step function ish for meanNightProp, meanMoonProp, and meanForkLength.  
          c4 <- stepper(tmp.df[,c("batchDate","TrapPositionID","bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength")],
                        c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength"),
                        min.date,
                        max.date)
          
          #   ---- Function stepper designed to run over non-unique TrapPositionIDs.  Get rid of this variable. 
          c4$TrapPositionID <- NULL
          
          #   ---- I like that the stepper function renames variables with a "Step" suffix, but that is not useful here.
          #   ---- So, rename those variables to get rid of that suffix.  
          names(c4) <- sapply(names(c4),function(x) gsub("Step","",x))
          c4 <- c4[,c("batchDate",names(covarB)[names(covarB) %in% c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength")])]
        } else {
          c4 <- NULL
        } 
        
        #   ---- Map back to current time frame.  
        reMapped <- reMap(c0,"batchDate2",min.date,max.date,tmp.df,strt.dt,end.dt)
        c0 <- reMapped$c0
        allDates <- reMapped$allDates
        
        #   ---- We use allDates as the backbone for bringing in all the different pieces. 
                            allDates <- merge(allDates,c0   ,by=c("batchDate"),all.x=TRUE)
        if(!is.null(c1   )) allDates <- merge(allDates,c1   ,by=c("batchDate"),all.x=TRUE)
        if(!is.null(c2   )) allDates <- merge(allDates,c2   ,by=c("batchDate"),all.x=TRUE)
        if(!is.null(c3   )) allDates <- merge(allDates,c3   ,by=c("batchDate"),all.x=TRUE)
        if(!is.null(c4   )) allDates <- merge(allDates,c4   ,by=c("batchDate"),all.x=TRUE)                   
                            
        #   ---- We used the enhanced efficiency dates batchDate2 to drive this.  If, in the result of 
        #   ---- allDates here, batchDate2 is NA, this means the user put in a set of dates in the 
        #   ---- Platform that span outside the real range of e-trials encapsulated by batchDate2. 
        #   ---- Put in a boring intercept estimate for these, so we report something.  
        if(sum(is.na(allDates$batchDate2)) > 0){
          allDates[is.na(allDates$batchDate2),]$Intercept <- 1
        }
        
        #   ---- If we're outside the splineDays for this trap (strt.dt and end.dt), we just use the
        #   ---- intercept estimate.  We don't know what happens outside the enh eff temporal range.
        allDates$inSplineDays <- 0
        allDates[(allDates$batchDate) < strt.dt | (allDates$batchDate > end.dt),names(allDates)[!(names(allDates) %in% c("batchDate","batchDate2","Intercept"))]  ] <- 0
        allDates[(allDates$batchDate) >= strt.dt & (allDates$batchDate <= end.dt),]$inSplineDays <- 1                  
  
        #   ---- MAKE THE EFF ESTIMATE.  Don't forget that tB includes the intercept already.  
        X <- as.matrix(allDates[allDates$inSplineDays == 1,names(allDates)[!(names(allDates) %in% c("batchDate","batchDate2","inSplineDays"))]],ncol=(1 + length(splineCoef) + length(covarB)))
        B <- as.matrix(unlist(c(tB,covarB[colnames(X)[colnames(X) %in% names(covarB)]])),ncol=1)  # <--- Make sure B in same order as cols in X.
        
        #   ---- Save X, and the dates at which we predict, for bootstrapping.
        all.X[[trap]] <- X     
        all.dts[[trap]] <- df$batchDate[ind.inside]  # Can't use sort(unique(splineDays)) here because it has Feb 29th, by construction.  
        fits[[trap]] <- fit #B
        
        pred <- 1 / (1 + exp(-1*X %*% B))
        #plot(allDates$batchDate,allDates$pred)
  
        #   ---- Add in predicted values for enh eff spline days.  
        df[ind.inside,]$efficiency <- pred
        
        #   ---- Use the mean of spline estimates for all dates outside efficiency trial season.  
        mean.p <- mean(pred, na.rm=T)
        df$efficiency[!ind.inside] <- mean.p
        
        #   ---- We added in fake days before and after the start of e-trials, so as to query for env covars
        #   ---- correctly.  Reduce df back to what it should be, based on min.date and max.date. 
        orig.dates <- data.frame(batchDate=seq(as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone),
                                               as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone),by="1 DSTday"))
        df <- df[df$batchDate %in% orig.dates$batchDate,]
        
        #   ---- We are done with the covariates, so get rid of them so that subsequent code doesn't get hung
        #   ---- up on them.  
        df <- df[,c("TrapPositionID","batchDate","nReleased","nCaught","efficiency")]
        
        #   ---- Save the fit for bootstrapping.
        #fits[[trap]] <- fit  
        
        #   ---- Save the raw efficiency data.  
        obs.data[[trap]] <- tmp.df  
        eff.type[[trap]] <- 5
        
        #   ---- Uncomment the following line if using imputed value for all days.  Otherwise, comment it out, 
        #   ---- and imputed.eff will tell which are observed.  With the following uncommented, you can find 
        #   ---- efficiency trials in grand.df with !is.na(grand.df$nReleased).
        ind <- rep(F, nrow(df))   
        
        df$imputed.eff <- factor( !ind, levels=c(T,F), labels=c("Yes", "No"))
        df$enhanced.eff <- rep("Yes",nrow(df))
        df$trapPositionID <- trap
        #plot(df$batchDate,df$efficiency)
        ans <- rbind(ans, df)
        #plot(ans[ans$TrapPositionID == "42010",]$batchDate,ans[ans$TrapPositionID == "42010",]$efficiency)
      }
      
      # par(mfrow=c(5,1))
      # for(trap in traps){
      #   x <- ans[ans$TrapPositionID == trap,]$batchDate
      #   y <- ans[ans$TrapPositionID == trap,]$efficiency
      #   plot(x,y,type="b")
      # }
      # par(mfrow=c(1,1))
    }
  } 
  
  if( useEnhEff == FALSE | sum(doOldEff) > 0 ){
    
    #   ---- Just do it the old way.  

    #   ---- If sum(doOldEff) > 0, then we have at least one trap for which we lack the requisite data from attempt to do enh eff models.
    #   ---- Redefine vector traps to only include those for which the old way is necessary. Of course,
    #   ---- only do this redefining if we really hoped for Enh Eff.  
    if( useEnhEff == TRUE ){
      oldTraps <- traps
      traps <- traps[doOldEff]
    }
 
    # 	---- If number of trials at a trap less than this number, 
    #        assume constant and use ROM+1 estimator
    eff.min.spline.samp.size <- get("eff.min.spline.samp.size", pos=.GlobalEnv)
    
    #   ---- Estimate a model for efficiency for each trap in obs.eff.df.
    for( trap in traps ){
  
      
      #   ---- No efficiency trials at this trap.  BUT!  If we have an enhanced efficiency model at 
      #   ---- this trap, but lack a covariate's data, we can at least do it the old way, on the 1960 paradigm
      #   ---- batchDate2, and at least bust out the temporal spline (with no covariates).  We use the old 
      #   ---- data to refit the spline -- we do NOT use the temporal spline fit with the covariates.  
      if(file.exists(paste0(here,"/",site,"_",trap,"_fit.RData"))){
        
        #   ---- Get the current trap enh eff fit back in memory.  
        load(paste0(here,"/",site,"_",trap,"_fit.RData"),envir=.tmpDataEnv)
        load(paste0(here,"/",site,"_",trap,"_splineDays.RData"),envir=.tmpDataEnv)
        # load(paste0(here,"/",site,"_",trap,"_splineCoef.RData"),envir=.tmpDataEnv)
        
        splineDays <- .tmpDataEnv$splineDays
        # splineCoef <- .tmpDataEnv$splineCoef
        fit <- .tmpDataEnv$fit
        
        tmp.df <- fit$data
        
        #   ---- Could have the same month-day represent more than one e-trial, over more than one year. 
        #   ---- Collapse these down to help with a merge below. 
        nReleased <- aggregate(tmp.df$nReleased,list(batchDate2=tmp.df$batchDate2),sum)
        names(nReleased)[names(nReleased) == "x"] <- "nReleased"
        nCaught <- aggregate(tmp.df$nCaught,list(batchDate2=tmp.df$batchDate2),sum)
        names(nCaught)[names(nCaught) == "x"] <- "nCaught"
        tmp.df <- merge(nReleased,nCaught,by=c("batchDate2"))
        
        #   ---- Pluck off batchDate2.  
        bd2 <- tmp.df$batchDate2
        bd2 <- bd2[!is.na(bd2)]
        
        #   ---- Remap back to the current time frame we care about.  
        reMap2list <- reMap2(min.date,max.date,splineDays)
        strt.tmp.dt <- reMap2list$strt.dt
        end.tmp.dt <- reMap2list$end.dt

        #   ---- Build up a crosswalk between batchDate and batchDate2.  
        date.seq.bd  <- data.frame(batchDate=seq(strt.tmp.dt,end.tmp.dt,by="1 DSTday"))
        date.seq.bd2 <- data.frame(batchDate2=seq(min(tmp.df$batchDate2),max(tmp.df$batchDate2),by="1 DSTday"))
        
        #   ---- Make sure we agree on 2/29.  Default batchDate2 seq always has this, because it's 1960.
        if(any(as.POSIXlt(date.seq.bd2$batchDate2)$mon == 1 & as.POSIXlt(date.seq.bd2$batchDate2)$mday == 29)){
          date.seq.bd2 <- date.seq.bd2[!(as.POSIXlt(date.seq.bd2$batchDate2)$mon == 1 & as.POSIXlt(date.seq.bd2$batchDate2)$mday == 29),]
          date.seq.bd2 <- data.frame(batchDate2=date.seq.bd2)
        }
        
        #   ---- Make the batchDate crosswalk.  
        bdxwalk <- data.frame(date.seq.bd,date.seq.bd2)
        
        #   ---- And now, disguise the data frame like the old efficiency code expects it. 
        tmp.df <- merge(bdxwalk,tmp.df,by=c("batchDate2"),all.x=TRUE)
        tmp.df$TrapPositionID <- trap
        tmp.df$efficiency <- tmp.df$nCaught / tmp.df$nReleased
        tmp.df <- tmp.df[,c("TrapPositionID","batchDate","nReleased","nCaught","efficiency")]
        
        #   ---- We need the dates from the original obs.eff.df for this trap, so it matches the other traps'
        #   ---- temporal sequence.  
        obs.eff.df.df <- obs.eff.df[obs.eff.df$TrapPositionID == trap,]
        df <- merge(obs.eff.df.df[,!(names(obs.eff.df.df) %in% c("TrapPositionID","nReleased","nCaught","efficiency"))],
                    tmp.df,
                    by=c("batchDate"),
                    all.x=TRUE)
        
        #   ---- In this part of the code, we don't care about covariates.  
        df <- df[,c("TrapPositionID","batchDate","nReleased","nCaught","efficiency")]
        df$TrapPositionID <- trap

      } else {
        
        #   ---- We have a trap that is new this year -- no enhanced efficiency model exists.  
        df <- obs.eff.df[ is.na(obs.eff.df$TrapPositionID) | (obs.eff.df$TrapPositionID == trap), ]
      }
      
      ind <- !is.na(df$efficiency)
        
      #  ---- Find the "season", which is between first and last trials
      strt.dt <- suppressWarnings(min( df$batchDate[ind], na.rm=T ))  # Earliest date with an efficiency trial
      end.dt  <- suppressWarnings(max( df$batchDate[ind], na.rm=T ))  # Latest date with efficiency trial
      ind.inside <- (strt.dt <= df$batchDate) & (df$batchDate <= end.dt)
      inside.dates <- c(strt.dt, end.dt)
      all.ind.inside[[trap]] <- inside.dates  # save season dates for bootstrapping
      
      #   ---- Dataframe df possibly too big here, since we buffed it out up top temporally, so as to induce
      #   ---- better fitting splines.  Knock it back down. 
      min.date.p <- as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone)
      max.date.p <- as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone)
    
  		#  ---- The fitting data frame
      tmp.df <- df[ind & ind.inside,]
      m.i <- sum(ind & ind.inside)
      
      if( m.i == 0 ){
          
      	cat( paste("NO EFFICIENCY TRIALS FOR TRAP", trap, ".\n") )
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
      	cat(paste("\n\n++++++Spline model fitting (no covariates) for trap:", trap, "\n Trials are:"))
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
  	             
  	        cur.bspl <- bs( df$batchDate[ind.inside], df=cur.df )
  	        tmp.bs <- cur.bspl[!is.na(df$efficiency[ind.inside]),]
  	    
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
  	      df$efficiency[ind.inside] <- pred
  	
  	      #   ---- Use the mean of spline estimates for all dates outside efficiency trial season.  
  	      mean.p <- mean(pred, na.rm=T)
  	      df$efficiency[!ind.inside] <- mean.p
  	      
  	      #   ---- Reduce down to the set we know we need.  We upped temporally for splining way above.  
  	      df <- df[df$batchDate >= min.date.p & df$batchDate <= max.date.p,]
  	
  	      #   ---- Save the fit for bootstrapping.
  	      fits[[trap]] <- fit  
  	      
        } 
        
        #   ---- Save the raw efficiency data.  
        obs.data[[trap]] <- tmp.df
        eff.type[[trap]] <- 4
      }
      
      #   ---- Uncomment the following line if using imputed value for all days.  Otherwise, comment it out, 
      #   ---- and imputed.eff will tell which are observed.  With the following uncommented, you can find 
      #   ---- efficiency trials in grand.df with !is.na(grand.df$nReleased).
      ind <- rep(F, nrow(df))   
      
      df$imputed.eff <- factor( !ind, levels=c(T,F), labels=c("Yes", "No"))
      df$trapPositionID <- trap
      
      df$enhanced.eff <- rep("No",nrow(df))
      df <- df[,c("TrapPositionID","batchDate","nReleased","nCaught","efficiency","imputed.eff","enhanced.eff","trapPositionID")]
      
      ans <- rbind(ans, df)
    }
  }

  
  
  #   ---- With enhanced efficiency models, could have traps in ans with NA entries.  This happens because
  #   ---- we didn't have the covariates this year to fit a trap's enh eff model, and there were no efficiency
  #   ---- trials at that trap.  But, we have catch at that trap.  Make sure we have the trap location's pretty 
  #   ---- name by directly throwing unique values in ans against the originating CAMP database. 
  db <- get( "db.file", envir=.GlobalEnv )
  ch <- odbcConnectAccess(db)
  newSubsites <- sqlQuery(ch, "SELECT DISTINCT subSiteName, subSiteID FROM SubSite;")             
  F.sql.error.check(newSubsites)
  close(ch)
  newSubsites <- newSubsites[newSubsites$subSiteID %in% catch.subsites,]
  if(is.factor(newSubsites$subSiteName)){
    newSubsites$subSiteName <- as.character(droplevels(newSubsites$subSiteName))
  }
  
  attr(ans,"subsites") <- newSubsites
  attr(ans,"site.name") <- site.name
  attr(ans,"eff.type") <- eff.type

  #   ---- Make a plot if called for.  
  if( !is.na(plot.file) ) {
    out.fn <- F.plot.eff.model( ans, plot.file )
  } else {
    out.fn <- NULL
  }
  
  cat("Observed efficiency data used in efficiency models.\n")
  print(obs.data)
  cat("\n")
  
  ans <- list(eff=ans, fits=fits, X=all.X, ind.inside=all.ind.inside, X.dates=all.dts, obs.data=obs.data, eff.type=eff.type)
  attr(ans, "out.fn.list") <- out.fn

  ans

}
