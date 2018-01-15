#' @export
#' 
#' @title F.efficiency.model.enh
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
#' @return Results from running the enhanced efficiency model fitting process 
#'   include \code{csv}s of efficiency trials missing a covariate, for each
#'   \code{TrapPositionID} at a \code{subSiteID}.  This prevents these
#'   efficiency trials from inclusion in the model fitting process.
#'   
#'   Each \code{TrapPositionID} also results in a series of plots depicting the
#'   fitted temporal spline, at each point of the backwards-fitting process. 
#'   These could number many, depending on the number of covariates available
#'   for possible exlcusion.
#'   
#'   Each \code{trapPositionID} also outputs a \code{png} containing model
#'   fitting information, including plots of efficiency versus each considered
#'   covariate, along with plotted temporal trends of each covariate against
#'   time.  Additional plots include the final fitted temporal spline (along
#'   with an prediction "curve" derived from the available data), as well as a
#'   final "plot" depicting model summary statistics, obtained via the
#'   \code{summary} function against the logistic efficiency-trial model fits.
#'   
#'   Note that no passage estimates result from the fitting of enhanced
#'   efficiency models.  This is because bootstrapping does not occur, but also
#'   because estimation of passage is not the goal of the model fitting process.
#'   Use the regular function sequence; i.e., functions without the
#'   \code{".enh"} to estimation passage for one-year intervals of interest.
#' 
#' @section Functions Used to Fit Enhanced Efficiency Models: 
#'   Five programs make up the specialized procedure for fitting enhanced
#'   efficiency models.  This means actually compiling the data of efficiency 
#'   trials obtained over several years, and then fitting a generalized additive
#'   model (GAM) to those data.  All five programs have suffixes of 
#'   \code{".enh"}, and originated with the program versions without the suffix.
#'   As such, they are very similar to the originals.
#'   
#'   The first, \code{run_passage.enh} corrals the fitting.  It is different 
#'   from \code{run_passage} in that all of the passage summary that is usually 
#'   created has been suppressed.  This is because there is no need to 
#'   bootstrap, once enhanced efficiency models have been obtained.
#'   
#'   The second, \code{get_release_data.enh} modifies the obtaining of release 
#'   data, so as to obtain astrological data and mean fork-length.  It is very 
#'   similar to its originator.
#'   
#'   The third, \code{est_passage.enh}, corrals the data from 
#'   \code{get_release_data.enh} for use in \code{est_efficiency.enh}.  It too 
#'   should be very similar to its originator.
#'   
#'   The fourth, \code{est_efficiency.enh}, ensures the calculation of weighted 
#'   averages for the three efficiency-trial covariates have to do with mean 
#'   fork-lengths, and percent of fishing performed at night, or while the moon 
#'   is up.  It also emulates closely its originator.
#'   
#'   Finally, the fifth, \code{eff_model.enh}, fits the enhanced efficiency 
#'   models.  It follows a backwards selection procedure, allowing for both 
#'   variable covariate selection, as well as variable temporal spline 
#'   complexity.  It creates graphical output, for each trap, so as to provide 
#'   further hypothesis generation.
#' 
#' @seealso \code{run_passage.enh}, \code{get_release_data},
#'   \code{est_passage.enh}, \code{est_efficiency.enh}, \code{eff_model.enh}
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

  #save.image("L:/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Outputs/Holding/RBDDNew.RData")
  #load("L:/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Outputs/Holding/RBDDNew.RData")
  
  option <- 2
  forEffPlots <- attr(obs.eff.df,"forEffPlots")
  site <- attr(obs.eff.df,"site")
  
  cat(paste0("1:  site is ",site,".\n"))
  
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
  
  #   ---- Query for covariates.  This is a big function!
  buildingEnhEff <- TRUE     # Make this very simple for now.  
  if(buildingEnhEff == TRUE){
    stuff <- getTogetherCovarData(obs.eff.df,min.date=min.date2,max.date=max.date2,traps,useEnhEff=FALSE)
  } else {
    stuff <- getTogetherCovarData(obs.eff.df,min.date,max.date,traps,useEnhEff=FALSE)
  }
  
  #   ---- Unpack 'stuff' so that we have the dbCovar dataframes available for plotting below.
  obs.eff.df <- stuff$obs.eff.df
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

  #   ---- Look at how the full models work out with respect to different traps.  
  table(obs.eff.df[!is.na(obs.eff.df$efficiency),]$covar,obs.eff.df[!is.na(obs.eff.df$efficiency),]$TrapPositionID)
  
  # #sql.code.dir.pg <- file.path(find.package("EnvCovDBpostgres"),"sql")
  # save.image("L:/Jason/saveObjs/rbdd.RData")
  # load("L:/Jason/saveObjs/rbdd.RData")
  # require(campR)
  
  
  varSummary <- NULL
  possibleVars <- c("(Intercept)","bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength","flow_cfs","temp_c","discharge_cfs","waterDepth_cm","waterDepth_ft","airTemp_C","airTemp_F","turbidity_ntu","waterVel_fts","waterTemp_C","waterTemp_F","lightPenetration_ntu","dissolvedOxygen_mgL","conductivity_mgL","barometer_inHg","precipLevel_qual","percQ")

  #   ---- Estimate a model for efficiency for each trap in obs.eff.df.
  for( trap in traps ){
    
  
    #   ---- 5. Reduce to the trials this trap cares about.  We do this to apply the 90% rule.  
    reducedETrials <- reduceETrials(obs.eff.df,possibleVars,bsplBegDt,bsplEndDt,trap,all.ind.inside)
    
    #   ---- Pull out the goodies.  
    df <- reducedETrials$df
    tmp.df <- reducedETrials$tmp.df
    m.i <- reducedETrials$m.i
    initialVars <- reducedETrials$initialVars
    initialVarsNum <- reducedETrials$initialVarsNum
    all.ind.inside <- reducedETrials$all.ind.inside
    
    #   ---- See if we have to deal with any covariates with missing data rows.  
    return <- checkMissingCovars(tmp.df,m.i,df,trap,plot.file)
    
    df <- return$df
    tmp.df <- return$tmp.df
    m.i <- return$m.i
    dataDeficient <- return$dataDeficient
    atLeast <- return$atLeast
    
    #   ---- We defined the start and end for the fishing season.  Need this to know when we need an efficiency estimate.
    #   ---- But we need to find the actual temporal range of efficiency dates, so the boundary points of the spline are
    #   ---- appropriate for efficiency effort.  Otherwise, the boundary points in the efficiency spline uses the 
    #   ---- overall min and max of fishing, which isn't correct.  Also, we may have just thrown out "good" efficiency 
    #   ---- trials that are "bad" because they lack data on a possible covariate.  If this trial was on the edge of the 
    #   ---- efficiency 1959-1960-time-period construct, this is a problem.  
    eff.strt.dt <- min(tmp.df$batchDate2)
    eff.end.dt <- max(tmp.df$batchDate2)
    
    #   ---- The long criteria here turns 'off' those trials lacking a covariate value from inclusion.  
    eff.ind.inside <- (eff.strt.dt <= df$batchDate2) & (df$batchDate2 <= eff.end.dt) # & 
                      #!(df$batchDate2 %in% dataDeficient$batchDate2 & df$nReleased %in% dataDeficient$nReleased & df$nCaught %in% dataDeficient$nCaught)
    eff.inside.dates <- c(eff.strt.dt,eff.end.dt)
    
    
    
    #   ---- Very rarely, we will have all of a series of trap at a subsiteID return zero fish.  This screws with the 
    #   ---- logistic regression of efficiency.  In this case, bump up the nCaught to 1 for the trial with the biggest
    #   ---- number of nReleased fish.  This situation happened on trap 52004 of the Feather, with its trials on 
    #   ---- 2017-01-26, 2017-01-31, and 2017-02-01.  
    if(sum(tmp.df$nCaught) == 0){
      message("There were no nCaught fish over the three e-trial traps on ",paste0(tmp.df$batchDate,collapse=", ")," for subSiteID = TrapPositionID = ",trap,
              ".  Subbing in a '1' for nCaught for the trial with the largest number of nReleased fish.\n")
      tmp.df <- tmp.df[order(tmp.df$nReleased,decreasing=TRUE),]
      tmp.df[1,]$nCaught <- 1
    }
    
    
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
      model.info <- plotbsSpline(bspl,fits[[trap]],bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
      dev.off()
      
      #   ---- Save X, and the dates at which we predict, for bootstrapping.
      all.X[[trap]] <- X   
      #all.dts[[trap]] <- df$batchDate[ind.inside] 
      
    } else {    
      
      #   ---- There are enough observations to estimate B-spline model.
      covarString <- "1"
      m0 <- fitSpline(covarString,df,eff.ind.inside,tmp.df,dist="binomial",max.df.spline,eff.inside.dates)
      
      cat("\nTemporal-only efficiency model for trap: ", trap, "\n")
      print(summary(m0$fit, disp=sum(residuals(m0$fit, type="pearson")^2)/m0$fit$df.residual))
        
      #   ---- Plot the spline.  
      png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",0,"mt.png"),width=7,height=7,units="in",res=600)
      model.info <- plotbsSpline(m0$bspl,m0$fit,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
      dev.off()       
        
    }   
      
    #   ---- Fit a backwards-selection enhanced-efficiency model for this trap.  
    theFit <- backEnhFit(tmp.df,df,initialVars,possibleVars,m.i,eff.ind.inside,max.df.spline,eff.inside.dates,trap,model.info,fits,plot.file,option,bsplBegDt,bsplEndDt)
    
    #   ---- Pull out the model stuff.  
    fit <- theFit$fit
    model.info <- theFit$model.info
    fits <- theFit$fits                                  # i think this works.
    covarStringPlot <- theFit$covarStringPlot
    interimVars1Num <- theFit$interimVars1Num
    interimVars2Num <- theFit$interimVars2Num
    
    #   ---- Run this again to get the cur.bspl from the final run.  It has the basis of ALL DATES, and not 
    #   ---- just those tied to an efficiency trial.  We need ALL DATES for predicting.  
    #X_t <- fitSpline(covarString,df,eff.ind.inside,tmp.df,dist="binomial",max.df.spline,eff.inside.dates)$cur.bspl
    
    # I THINK THIS IS ALL WE NEED TO SAVE.
    splineDays <- df$batchDate2[eff.ind.inside]
    splineCoef <- fit$coefficients[grepl("tmp",names(fit$coefficients))]
    splineBegD <- bsplBegDt
    splineEndD <- bsplEndDt

    #   ---- Check to make sure dim(splineCoef) = dim(X_t)[2]
    #if(length(splineCoef) == dim(X_t)[2]){message("Dimension of spline beta vector equals dimension of temporal basis column space.\n")}
    
    #   ---- TEMPORARY FILE SAVE.  
    holding <- paste0("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Outputs/Holding/")
    #save(splineCoef,splineDays,splineBegD,splineEndD,fit,file=holding)

    
    save(splineCoef,file=paste0(holding,site,"_",trap,"_splineCoef.RData"),compress="xz",compression_level=9)
    save(splineDays,file=paste0(holding,site,"_",trap,"_splineDays.RData"),compress="xz",compression_level=9)
    save(splineBegD,file=paste0(holding,site,"_",trap,"_splineBegD.RData"),compress="xz",compression_level=9)
    save(splineEndD,file=paste0(holding,site,"_",trap,"_splineEndD.RData"),compress="xz",compression_level=9)
    save(fit,file=paste0(holding,site,"_",trap,"_fit.RData"),compress="xz",compression_level=9)
  
    
    
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
    #all.dts[[trap]] <- df$batchDate[ind.inside]   11/27/2017 locked in a function cause i dont push it out.  do i need this?
        
    #   ---- Standard logistic prediction equation.  
    #   ---- "Pred" is all efficiencies for dates between min and max of trials.
    pred <- 1 / (1 + exp(-pred))  
        
      
    #   ---- Make a lookup vector.  
    dfs <- c("dbMoon","dbNite","dbFLen","dbFlPG","dbTpPG","dbDisc","dbDpcm","dbDpft","dbATpC","dbATpF","dbTurb","dbWVel","dbWTpC","dbWTmF","dbLite","dbDOxy","dbCond","dbBaro","dbWeat","dbPerQ")
    covars <- c("bdMeanMoonProp","bdMeanNightProp","bdMeanForkLength","flow_cfs","temp_c","discharge_cfs","waterDepth_cm","waterDepth_ft","airTemp_C","airTemp_F","turbidity_ntu","waterVel_fts","waterTemp_C","waterTemp_F","lightPenetration_ntu","dissolvedOxygen_mgL","conductivity_mgL","barometer_inHg","precipLevel_qual","percQ")
    covarlu <- dfs
    names(covarlu) <- covars
        
    #   ---- The postgres flow and temp data are trap independent, unlike the CAMP data.  So, tie the data
    #   ---- to the trap in the way the covar plotting function expects.
    if(exists("dbFlPG")) dbFlPG$subSiteID <- trap
    if(exists("dbTpPG")) dbTpPG$subSiteID <- trap
        
    tt <- forEffPlots[forEffPlots$trapPositionID == trap,]
    dbMoon <- data.frame(subSiteID=tt$trapPositionID,measureDate=tt$EndTime,moonProp=tt$moonProp,moonPropUnitID=-99)
    dbNite <- data.frame(subSiteID=tt$trapPositionID,measureDate=tt$EndTime,nightProp=tt$nightProp,nightPropUnitID=-99)
    dbFLen <- data.frame(subSiteID=tt$trapPositionID,measureDate=tt$EndTime,wmForkLength=tt$wmForkLength,wmForkLengthUnitID=-99)
        
    DF <- model.info[[3]]
    DF <- DF[!duplicated(DF$batchDate2),]
    DF <- DF[order(DF$batchDate2),]
        
        
    png(paste0(plot.file,"-",trap,".png"),res=400,width=4*(nCovars + 1),height=6,units="in")
          
      par(mfcol=c(2,nCovars + 1))
          
      #   ---- Plot covariate-specific stuff.  
      if(nCovars > 0){
        for(cc in 1:nCovars){
          covarPlot(plotCovars[cc],obs.eff.df,get(covarlu[plotCovars[cc]]),trap,eff.ind.inside,bsplBegDt,fit)
        }
      }
   
      #   ---- Get a plot of observed versus a prediction curve.  
      yM <- max(tmp.df$nCaught / tmp.df$nReleased)
      plot(tmp.df$batchDate2,100*(tmp.df$nCaught / tmp.df$nReleased),type="p",pch=19,col="red",ylim=c(0,100*yM),xaxt='n',yaxt='n',xlab=NA,ylab=NA,cex=(tmp.df$nReleased)/mean((tmp.df$nReleased)))
      par(new=TRUE)
      plot(DF$batchDate2,100*DF$p,type="l",pch=19,col="blue",ylim=c(0,100*yM),xlab="Date",ylab='Efficiency (%)',main=attr(df,"site.name"))
      legend("topright",c("Observed","Predicted"),pch=c(19,NA),lwd=c(NA,1),col=c("red","blue"))
          
      #   ---- 'Plot' some statistics of interest.
      O <- capture.output(summary(fit))
          
      plot(1,col="white")
      for(i in 1:length(O)){
        text(0.6,0.7 + 0.7/length(O)*(length(O) - i),O[i], pos=4, family="mono",cex=0.6)
      }
          
    dev.off()
    par(mfcol=c(1,1))
        
    
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
  save(varSummary,file=paste0(holding,"/varSummary_",site,".RData"))
  
  cat("Observed efficiency data used in efficiency models.\n")
  print(obs.data)
  cat("\n")
 
  save("fits",file=paste0(plot.file,".RData"))

}
