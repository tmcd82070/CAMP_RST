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
#' F.efficiency.model( obs.eff.df, plot=T, max.df.spline=4, plot.file=NA)
#' }
F.efficiency.model <- function( obs.eff.df, plot=T, max.df.spline=4, plot.file=NA ){
  
  # obs.eff.df <- eff
  # plot <- plot
  # max.df.spline <- 4
  # plot.file <- plot.file

  ans <- NULL
  traps <- sort( unique(obs.eff.df$TrapPositionID))

  fits <- all.X <- all.ind.inside <- all.dts <- obs.data <- eff.type <- vector("list", length(traps))
  names(fits) <- traps
  names(all.X) <- traps
  names(all.dts) <- traps
  names(all.ind.inside) <- traps
  names(obs.data) <- traps
  names(eff.type) <- traps
  
  
  
  #   ---- Decide if we're going to use enhanced efficiency.  
  if(useEnEff == TRUE){
    
    #   ---- Get stuff we need to fit the enhanced efficiency models.  
    
    #   ---- 1.  We know traps from immediately above.
    data(betas)
    betas <- betas[betas$subsiteID %in% traps,]
    
    #   ----    It knows which days to run on because it pulls off a min.date and max.date from obs.eff.df.  
    obs.eff.df <- getTogetherCovarData(obs.eff.df)
    
    #   ---- Run over individual traps.
    for(trap in traps){
    
      
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
      cat(paste0("Enhanced efficiency model for trap ",trap," seeks recorded data on covariates ",paste0(names(covarB),collapse=", "),".\n"))
      #names(covarB)[names(covarB) == "waterDepth_ft"] <- "waterDepth_cm"
      
      
      
      #   ---- I have identified variables I care about.  Get them together, for all batchDates.
      
      #   ---- Get the temporal spline basis matrix.  
      load(paste0("L:/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Outputs/Holding/","splineSummary_57000.RData"))
      
      #   ---- Stuff we just loaded.  
      splineDays <- df$batchDate2[eff.ind.inside]
      splineCoef <- fit$coefficients[grepl("tmp",names(fit$coefficients))]
      splineBegD <- bsplBegDt
      splineEndD <- bsplEndDt
      
      if(sum(grepl("tmp",names(fit$coefficients))) > 0){
      
        #   ---- Build the basis matrix.  This does includes the intercept.  
        N <- nrow(bs( splineDays, df=length(splineCoef)))
        timextB <- cbind(rep(1,N),bs( splineDays, df=length(splineCoef))) %*% c(covarI,splineCoef)
        timeDF <- data.frame(timextB=timextB,batchDate2=splineDays)
        c0 <- timeDF[!duplicated(timeDF$batchDate2),]
        
      } else {
        
        #   ---- If we're here, the temporal spline was fit as a simple intercept.  We still 
        #   ---- need to be sure we grab the 1960 batchDate2 dates, so do that here. 
        c0 <- data.frame(timextB=rep(covarI,length(splineDays)),batchDate2=splineDays)
      }
    
      #   ---- Get the environmental covariate data.  
      if(sum(c("temp_c","flow_cfs") %in% names(covarB)) > 0){
        #do stuff
        #c1 <- thedf
      } else {
        c1 <- NULL
      }
      
      #   ---- Get the CAMP environmental covariate data.  
      if(sum(c("waterDepth_cm","turbidity_ntu","waterVel_fts","waterTemp_C") %in% names(covarB)) > 0){
        #do stuff
        #c2 <- thedf
      } else {
        c2 <- NULL
      }
         
      #   ---- Get the percent-Q covariate data.
      if(sum(c("percQ_") %in% names(covarB)) > 0){
        #do stuff
        #c3 <- thedf
      } else {
        c3 <- NULL
      }
      
      #   ---- Get the e-trial data.  
      if(sum(c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength") %in% names(covarB)) > 0){
        
        #   ---- If bdMeanNightProp or bdMeanMoonProp.
        
        #   ---- If bdMeanForkLength.
        if(sum(c("bdMeanForkLength") %in% names(covarB)) > 0){
          
          #   ---- Compile a daily mean forklength from catch.  I take all fish (including not randomly sampled).
          catch.t <- catch.df[catch.df$oldtrapPositionID == trap & 
                              catch.df$TrapStatus == "Fishing" & 
                              catch.df$assignedCatch > 0,
                              c("batchDate","forkLength","assignedCatch")]   # what about gaps in fishing?
        
          fl <- by(catch.t[,c("forkLength","assignedCatch")],list(batchDate=catch.t$batchDate),function(x) weighted.mean(x$forkLength,x$assignedCatch))
          fl <- data.frame(vapply(fl,unlist,unlist(fl[[1]])))
          fl$batchDate <- rownames(fl)
          rownames(fl) <- NULL
          names(fl) <- c("bdMeanForkLength","batchDate")
          fl$batchDate <- as.POSIXct(fl$batchDate,format="%Y-%m-%d",tz="UTC")
          
          #   ---- Fit a smoothing spline based on available dates.
          ss <- smooth.spline(fl$batchDate,fl$bdMeanForkLength,cv=TRUE)
          
          #   ---- Compile all inclusive dates.  
          bb <- data.frame(batchDate=strptime(seq(min(fl$batchDate),max(fl$batchDate),by="day"),format="%Y-%m-%d",tz="UTC"))  # daylight savings issue?
          bb$spline <- predict(ss,as.numeric(bb$batchDate))$y  
          
          #   ---- Get together daily values of forklength.  We have historically used spline values of 
          #   ---- efficiency on days we have real e-trial values, so use spline values on all days here too.
          c4.fl <- merge(bb,fl,by=c("batchDate"),all.x=TRUE)

          #   ---- Swap out the real value with the splined value. 
          c4.fl$bdMeanForkLength <- NULL
          names(c4.fl)[names(c4.fl) == "spline"] <- "bdMeanForkLength"
          
          #   ---- Testing:  take a look and make sure this appears to work.  
          #plot(dayfl$batchDate,dayfl$bdMeanForkLength,type="l",col="black",xlim=c(min(dayfl$batchDate),max(dayfl$batchDate)),ylim=c(0,100))
          #par(new=TRUE)
          #plot(dayfl$batchDate,dayfl$spline,type="l",col="red",xlim=c(min(dayfl$batchDate),max(dayfl$batchDate)),ylim=c(0,100))
        } else {
          c4.fl <- NULL
        }
        
        if(sum(c("bdMeanNightProp","bdMeanMoonProp") %in% names(covarB)) > 0){
          # do stuff
        } else {
          c4.ms <- NULL    # ms stands for moon sun. 
        }
        
        #   ---- Bring in the c4.fl and cf.ms down below so we have the master set of batchDates we care about.
        
      }
      
      
      #   ---- Build a bridge to map 1960 batchDate2 to whatever regular batchDate we have.  leap year ok?
      bd2.lt <- as.POSIXlt(c0$batchDate2)
      c0$batchDate <- ISOdate(substr(min.date,1,4),bd2.lt$mon + 1,bd2.lt$mday,0,tz="UTC")
      
      #   ---- Allow for all days in between min.date and max.date, which are provided by user. 
      allDates <- data.frame(batchDate=seq(as.POSIXct(min.date,format="%Y-%m-%d",tz="UTC"),
                                           as.POSIXct(max.date,format="%Y-%m-%d",tz="UTC"),by="1 day"))
      
      #   ---- We use allDates as the backbone for bringing in all the different pieces. 
                          allDates <- merge(allDates,c0   ,by=c("batchDate"),all.x=TRUE)
      if(!is.null(c1   )) allDates <- merge(allDates,c1   ,by=c("batchDate"),all.x=TRUE)
      if(!is.null(c2   )) allDates <- merge(allDates,c2   ,by=c("batchDate"),all.x=TRUE)
      if(!is.null(c3   )) allDates <- merge(allDates,c3   ,by=c("batchDate"),all.x=TRUE)
      if(!is.null(c4.fl)) allDates <- merge(allDates,c4.fl,by=c("batchDate"),all.x=TRUE)
      if(!is.null(c4.ms)) allDates <- merge(allDates,c4.ms,by=c("batchDate"),all.x=TRUE)
      
      #   ---- We used the enhanced efficiency dates batchDate2 to drive this.  If, in the result of 
      #   ---- allDates here, batchDate2 is NA, this means the use put in a set of dates in the 
      #   ---- Platform that span outside the real range of e-trials encapsulated by batchDate2. 
      #   ---- Put in a boring intercept estimate for these, so we report something.  
      allDates[is.na(allDates$batchDate2),]$timextB <- covarI     
      
      #   ---- And if we have no temporal spline, we don't want to apply xTBeta for those dates 
      #   ---- either.  So NA out the covariate values.  
      allDates[is.na(allDates$batchDate2),names(covarB)] <- 0
                          
      #   ---- MAKE THE ESTIMATE.  Don't forget that timxtB includes the intercept already.  
      X <- as.matrix(allDates[,names(allDates)[!(names(allDates) %in% c("batchDate","batchDate2"))]],ncol=2)
      B <- as.matrix(c(1,as.matrix(covarB)),ncol=1)   # is this a row with covarB more than 1?
      
      e <- X %*% B
      pred <- 1 / (1 + exp(-1*e))
      allDates$pred <- pred
      #plot(allDates$batchDate,allDates$pred)
      
      
      #   ---- Save the raw efficiency data.  
      obs.data[[trap]] <- tmp.df   # something different?
      eff.type[[trap]] <- 5
      
    }
  } else {
    
    #   ---- Just do it the old way.  
  
 
    # 	---- If number of trials at a trap less than this number, 
    #        assume constant and use ROM+1 estimator
    eff.min.spline.samp.size <- get("eff.min.spline.samp.size", pos=.GlobalEnv)
    
    #   ---- Estimate a model for efficiency for each trap in obs.eff.df.
    for( trap in traps ){
  
      df <- obs.eff.df[ is.na(obs.eff.df$TrapPositionID) | (obs.eff.df$TrapPositionID == trap), ]
      ind <- !is.na(df$efficiency)
  
      #  ---- Find the "season", which is between first and last trials
      strt.dt <- min( df$batchDate[ind], na.rm=T )  # Earliest date with an efficiency trial
      end.dt  <- max( df$batchDate[ind], na.rm=T )  # Latest date with efficiency trial
      ind.inside <- (strt.dt <= df$batchDate) & (df$batchDate <= end.dt)
      inside.dates <- c(strt.dt, end.dt)
      all.ind.inside[[trap]] <- inside.dates  # save season dates for bootstrapping
      
  		#  ---- The fitting data frame
      tmp.df <- df[ind & ind.inside,]
      m.i <- sum(ind & ind.inside)
      
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
      
      ans <- rbind(ans, df)
  }

  attr(ans,"subsites") <- attr(obs.eff.df, "subsites")
  attr(ans,"site.name") <- attr(obs.eff.df, "site.name")

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
