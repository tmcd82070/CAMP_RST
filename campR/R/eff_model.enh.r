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
  

  
  oursitevar <- ifelse(site %in% c(57000),16,NA)      # 17?
    
  #   ---- Get covariate data of interest.  
  df <- queryEnvCovDB("jmitchell","jmitchell",min.date2,max.date2,oursitevar,type="D",plot=TRUE)
  
  df$date <- strptime(df$date,format="%Y-%m-%d",tz=time.zone)
  
  #df1 <- df[!is.na(df$flow_cfs),]
  
  #   ---- Fit a simple smoothing spline and predict.    
  covar <- NULL
  if(sum(!is.na(df$flow_cfs)) > 0){
    m1 <- smooth.spline(df[!is.na(df$flow_cfs),]$date,df[!is.na(df$flow_cfs),]$flow_cfs,cv=TRUE)
    covar <- "flow_cfs"
    obs.eff.df$flow_cfs <- predict(m1,as.numeric(obs.eff.df$batchDate))$y
  }
  if(sum(!is.na(df$flow_cfs)) > 0){
    m2 <- smooth.spline(df[!is.na(df$temp_c),]$date,df[!is.na(df$temp_c),]$temp_c,cv=TRUE)
    covar <- c(covar,"temp_c")
    obs.eff.df$temp_c <- predict(m2,as.numeric(obs.eff.df$batchDate))$y
  }
  
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
      
      #   ---- There are enough observations to estimate B-spline model -- with covariates!
      
      #   ---- Fit glm model, increasing degress of freedom, until minimize AIC or something goes wrong.  
      cat(paste("\n\n++++++Spline model fitting for trap:", trap, "\n Trials are:"))
      print(tmp.df)
      
      covar <- paste0(covar,collapse=" + ")
      cat(paste("\n\n++++++ ...and covariates are",covar,".\n"))
      
      #   ---- At least one efficiency trial "inside" for this trap.
      #   ---- Fit a null model (with our covariates).  
      fit <- glm( nCaught / nReleased ~ 1 + flow_cfs*temp_c, family=binomial, data=tmp.df, weights=tmp.df$nReleased ) 
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
          
          cur.fit <- glm( nCaught / nReleased ~ tmp.bs + flow_cfs*temp_c, family=binomial, data=tmp.df, weights=tmp.df$nReleased )   
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
        png(paste0(plot.file,"-",trap,".png"),res=400,width=6,height=6,units="in")
        par(mfrow=c(2,2))
        
          #   ---- Plot of efficiency versus variable 1. 
          if(sum(!is.na(tmp.df$flow_cfs)) > 0){
            plot(tmp.df$flow_cfs,tmp.df$nCaught / tmp.df$nReleased,type="p",pch=19,col="red",xlab="Flow cfs",ylab='Efficiency (0.0 - 1.0)',main=paste0("Flow cfs at ",attr(df,"site.name")))
          } else {
            plot(1,1)
          }
        
          #   ---- Plot of efficiency versus variable 2.
          if(sum(!is.na(tmp.df$temp_c)) > 0){
            plot(tmp.df$temp_c,tmp.df$nCaught / tmp.df$nReleased,type="p",pch=19,col="blue",xlab="Temperature C",ylab='Efficiency (0.0 - 1.0)',main=paste0("Temp C at ",attr(df,"site.name")))
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
          plot(1,xaxt="n",yaxt="n",type="n",xlab="",ylab="",xlim=c(0, 10),ylim=c(0, 10))
          text(8,3,exp(1000*coef(fit)[names(coef(fit)) == "flow_cfs"])
          coef(fit)[names(coef(fit)) == "temp_c"]
          
          glm(nCaught/nReleased ~ flow_cfs,family=binomial,data=tmp.df,weights=tmp.df$nReleased)   
          
          
          
        par(mfrow=c(1,1))
        dev.off()
      
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
      
      #   ---- Save the raw efficiency data.  
      obs.data[[trap]] <- tmp.df
      eff.type[[trap]] <- 5
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
