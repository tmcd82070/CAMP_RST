#' @export
#' 
#' @title F.bootstrap.passage
#'   
#' @description Bootstrap or Monte-Carlo simulate data sufficient to compute 
#'   confidence intervals for passage.
#'   
#' @param grand.df A data frame containing both daily estimated passage and 
#'   efficiency, for each trap.
#' @param catch.fits A list of Poisson fitted \code{glm} objects for each trap,
#'   possibly with basis spline covariates, used to impute missing catches.
#' @param catch.Xmiss A list containing a spline basis matrix of imputed days 
#'   where catch is missing for each trap.
#' @param catch.gapLens A list, for each trap, containing a numeric vector of
#'   hours of time spent \code{"Not fishing"}, originating from variable
#'   \code{TrapStatus} in original catch data queries.  All values necessarily
#'   have entries less than 24.
#' @param catch.bDates.miss A list containing a POSIX vector of \code{"Not fishing"} 
#'   \code{batchDate}s for missing catches, for each trap.  Necessary because 
#'   one \code{batchDate} may have two (or more) periods with no fishing.
#' @param eff.fits A list of binomial logistic regression fitted objects used to
#'   compute efficiency.  One per trap.
#' @param eff.X A list containing the basis matrix associated with each 
#'   efficiency-spline model for each trap.  These matrices originate from use
#'   of function \code{bs} in function \code{F.efficiency.model}.
#' @param eff.ind.inside A list containing the first and last day of trapping 
#'   for each trap.
#' @param eff.X.dates A list containing the dates for which missing efficiency 
#'   must be estimated, for each trap.
#' @param eff.X.obs.data A list containing the raw observed efficiency data used
#'   to fit efficiency models and estimate bias-corrected efficiencies.
#' @param eff.type A list containing the type of efficiency model utilized for 
#'   each trap.  See \code{eff_model.r} for details.  
#' @param sum.by A text string indicating the temporal unit over which daily 
#'   estimated catch is to be summarized.  Can be one of \code{day}, 
#'   \code{week}, \code{month}, \code{year}.
#' @param R An integer specifying the number of Monte Carlo iterations to do.
#' @param ci A logical indicating if 95\% bootstrapped confidence intervals 
#'   should be estimated along with passage estimates.
#'   
#' @return A data frame containing 95\% bias-adjusted confidence intervals for 
#'   all unique temporal units summarized via specification of \code{sum.by}.
#'   
#' @details In order to bootstrap the estimated passage for a particular trap, 
#'   random realizations of passage must be generated.  Variability in passage 
#'   can originate from two sources:  imputed catch and imputed efficiency. 
#'   Imputed catch originates from periods of \code{"Not fishing"} in excess of two 
#'   hours, while imputed efficiency results from days between the first and 
#'   last day of a recorded efficiency trial.  Any one day may lead to several 
#'   instances of imputed catch, but at most, only one instance of imputed 
#'   efficiency.  Since days of operation varies over different traps, the 
#'   imputation periods vary as well.
#'   
#'   Bootstrapping of each of catch and efficiency is organized via matrices of 
#'   dimension \eqn{\code{nrow(grand.df)} \times \code{R}}, where rows hold unique 
#'   trapping instances, and the columns the bootstrapping replicates. Because 
#'   efficiency is only estimated on a per-day basis, but multiple trapping 
#'   instances can take place on any one day, catch data are summarized per day 
#'   following initial bootstrap sampling, with corresponding multiple intra-day
#'   replicates summed.  Imputed values within each of the resulting daily catch
#'   and efficiency matrices thus contribute to underlying stochastic 
#'   variability.
#'   
#'   For each trap following sampling completion, the catch matrix is divided by
#'   the efficiency matrix, where the \eqn{(i,j)}th entry of the resulting 
#'   passage sampling matrix corresponds to the \eqn{j}th passage replicate of 
#'   the \eqn{i}th day.  These daily passage estimates, over each replicate, are
#'   then summarized via function \code{summarize.passage} over the temporal 
#'   unit specified via \code{sum.by}.  In this way, \code{R} samples for each 
#'   unique temporal time unit within the date range of \code{grand.df} are 
#'   obtained.
#'   
#'   Given the \code{R} replicates for each unique time period, 95\% 
#'   bias-corrected confidence intervals are obtained.  These confidence 
#'   intervals correct for non-symmetric passage replicates.
#'   
#' @section Variance Matrices: Catch models are fit via a Poisson generalized 
#'   linear model.  Often, these models are overdispersed, with a large Pearson 
#'   overdispersion parameter, relative to one.  Catch, however, often has a 
#'   much higher-than-expected variance, due to seasonal fish pulses.  To 
#'   account for outliers in this case, the largest and smallest 20% of Pearson 
#'   residuals are removed, and the dispersion statistic recalculated.    If 
#'   instead, the dispersion statistic is less than one, it is set to one.
#'   
#'   The modified overdispersion statistic is then multiplied by the 
#'   variance-covariance matrix of the original model-fit;  in this way, 
#'   standard errors are recalculated via a modified quasililelihood approach.
#'   
#'   Efficiency models are fit via a binomial generalized linear model.  As a 
#'   discrete model, these also can be overdispersed.  However, the efficiency 
#'   trial data are generally sparse.  As a result, instead of removing the top 
#'   proportion of residuals greater than some percentile magnitude, those 
#'   greater than an absolute cut-off are removed instead.  Here, any Pearson 
#'   residual with an absolute value greater than 8 are removed.  Following the 
#'   removal of all extreme residuals, the resulting overdispersion is then 
#'   calculated, and then applied to the variance-covariance of the original 
#'   binomial fit.  The resulting dispersion statistic is set to one in case it 
#'   calculates as less than one.  Traps with one efficiency trial also have 
#'   overdispersions set to one.  Similar to the variance adjustment applied to 
#'   catch, this is a modified quasilikelihood approach.  
#'   
#'   In the case when there are less than ten observed efficiency trials for one
#'   trap, a bias-corrected efficiency is calculated in lieu of a model fit. 
#'   This efficiency is calculated simply as the sum of the \code{nCaught} fish 
#'   plus one, divided by the sum of the \code{nReleased} fish plus one. The 
#'   plus-one manipulation prevents the direct estimation of variance via a 
#'   formal generalized linear (or additive) model.  In this case, bootstrap 
#'   samples originate from a multivariate distribution with mean equal to the 
#'   bias-corrected efficiency, and a variance equal to the traditional 
#'   generalized linear model (glm) variance, but with back-transformed
#'   model-derived estimates of observed efficiencies replaced with their
#'   bias-corrected equivalents. See McCulloch and Searle (2001), or any other
#'   mathetmatical treatment of the generalized linear model, for details on the
#'   glm variance.
#'   
#' @section Random Realizations: Catch fit models are utilized to generate
#'   random realizations of catch for each individual trap.  To do this, the 
#'   \code{rmvnorm} function randomly samples from a multivariate normal 
#'   distribution, with column dimension equal to the number of \eqn{\beta} 
#'   coefficients utilized in that trap's catch model.  The \code{rmvnorm} uses 
#'   the vector of model coefficients for the mean and the the modified 
#'   quasilikelihood variance-covariance matrix for the variance. To emphasize 
#'   calculation speed, the Cholesky matrix decomposition is used to calculate 
#'   the variance matrix root.  Due to Poisson catch models utilizing a log 
#'   link, betas and variances are on the log scale.
#'   
#'   Each of the \code{R} multivariate-normal samples is then utilized to create
#'   a new prediction for missing catches, with each expanded by the log of the
#'   trap down-time, i.e., trap down-time is an offset. Resulting predictions
#'   for imputed catch are then exponentiated, and then combined with the
#'   observed catch to create a full day-based temporal fishing record for each
#'   trap.
#'   
#' @references Manly, B. F. J.  Randomization, Bootstrap and Monte Carlo Methods
#'   in Biology, Third Edition, 2006.  Chapman and Hall/CRC.
#'   
#'   McCulloch, C. E. and Searle, S. R.  Generalized, Linear, and Mixed Models,
#'   2001. Wiley Interscience.
#'   
#' @seealso \code{F.est.catch}, \code{F.est.eff}, \code{F.summarize.passage}, \code{F.efficiency.model}
#'   
#' @author WEST Inc.
#'   
#' @examples 
#' \dontrun{
#' grand.df <- grand.df
#' catch.fits <- catch.and.fits$fits
#' catch.Xmiss <- catch.and.fits$X.miss
#' catch.gapLens <- catch.and.fits$gaps
#' catch.bDates.miss <- catch.and.fits$bDates.miss
#' eff.fits <- eff.and.fits$fits
#' eff.X <- eff.and.fits$X
#' eff.ind.inside <- eff.and.fits$ind.inside
#' eff.X.dates <- eff.and.fits$X.dates
#' sum.by <- summarize.by
#' R <- 100
#' ci=T <- ci
#' df <- F.bootstrap.passage(grand.df,catch.fits,catch.Xmiss,catch.gapLens,
#'   catch.bDates.miss,eff.fits,eff.X,eff.ind.inside,eff.X.dates,
#'   sum.by,R,ci=T)
#' }
F.bootstrap.passage <- function( grand.df, catch.fits, catch.Xmiss, catch.gapLens, catch.bDates.miss, eff.fits, eff.X, eff.ind.inside, eff.X.dates, eff.X.obs.data, eff.type, sum.by, R, ci=T ){

  # grand.df <- grand.df
  # catch.fits <- catch.and.fits$fits
  # catch.Xmiss <- catch.and.fits$X.miss
  # catch.gapLens <- catch.and.fits$gaps
  # catch.bDates.miss <- catch.and.fits$bDates.miss
  # eff.fits <- eff.and.fits$fits
  # eff.X <- eff.and.fits$X
  # eff.ind.inside <- eff.and.fits$ind.inside
  # eff.X.dates <- eff.and.fits$X.dates
  # eff.X.obs.data <- eff.and.fits$obs.data
  # eff.type <- eff.and.fits$eff.type
  # sum.by <- summarize.by
  # R <- 100
  # ci <- T


  #   ---- Set the confidence level of the intervals.
  conf <- 0.95   
  
  #   ---- Get Julian weeks for this timeframe.  
  if(sum.by == 'week'){
    
    #   ---- Obtain information from the global environment.  
    min.date <- get("min.date",envir=.GlobalEnv)
    max.date <- get("max.date",envir=.GlobalEnv)
    
    #   ---- Obtain Julian dates so days can be mapped to specialized Julian weeks. 
    db <- get( "db.file", envir=.GlobalEnv ) 
    ch <- odbcConnectAccess(db)
    JDates <- sqlFetch( ch, "Dates" )
    close(ch) 
    
    #   ---- Clean up the Julian week information.
    theDates <- data.frame(Date=as.Date(seq(as.Date(attr(grand.df,"min.date"),format="%Y-%m-%d"),as.Date(attr(grand.df,"max.date"),format="%Y-%m-%d"),by="days")))
    JDates$Year <- as.numeric(format(JDates$uniqueDate,"%Y"))
    JDates$Date <- as.Date(JDates$uniqueDate,format="%Y-%m-%d")
    theDates <- merge(theDates,JDates[,c('Date','julianWeek','Year','julianWeekLabel')],by=c('Date'))
    names(theDates)[names(theDates) == 'julianWeek'] <- 'JWeek'
    
    jDates <- JDates[as.Date(JDates$uniqueDate) >= min.date & as.Date(JDates$uniqueDate) <= max.date,]
    jDates <- jDates[,c('uniqueDate','year','julianWeek','julianWeekLabel')]
    
    #jDates <- subset(JDates, as.Date(uniqueDate) >= min.date & as.Date(uniqueDate) <= max.date,c(uniqueDate,year,julianWeek,julianWeekLabel))
    
    attr(grand.df,"JDates") <- jDates
  }

  #   ---- Compute THE estimate.  F.summarize.passage first averages over traps,
  #   ---- then sums by sum.by.  When this returns, n.orig is a data frame with columns
  #   ---- s.by, passage, date, and pct.imputed.catch.
  n.orig <- F.summarize.passage( grand.df, sum.by )
  cat("back from summarize\n")

  #   ---- If confidence intervals are called for, do them; otherwise return.
  n.len <- nrow(n.orig)
  n.grand.df <- nrow(grand.df)
  if( !ci ){
      na <- rep(NA, n.len)
      ans <- data.frame(l = na, u= na)
  } else {
    n.traps <- length(catch.fits)

    #   ---- These giant matrices will hold bootstrap iterations.
    c.pred <- matrix( grand.df$totalEstimatedCatch, nrow=nrow(grand.df), ncol=R )      
    e.pred <- matrix( grand.df$efficiency, nrow=nrow(grand.df), ncol=R )

    cat(paste("n.traps=", n.traps, "\n"))
    cat(paste("n.len=", n.len, "\n"))

    #   ---- Main iteration loop (over traps).
    for(trap in 1:n.traps){

      trapID <- names(catch.fits)[trap]
      trap.ind <- grand.df$trapPositionID == trapID
      
      cat(paste("trap=", trapID, "\n" ))

      #   ---- Generate random realization of catch, where required, i.e., for 
      #   ---- days (time periods) requiring imputation.  
      ind <- which(trapID == names(catch.fits))
      if( length(ind) > 0 ){
        c.fit <- catch.fits[[ind]]
        X <- catch.Xmiss[[ind]]
        gaps <- catch.gapLens[[ind]]
        bd.miss <- catch.bDates.miss[[trap]]

        cat("in bootstrap_passage.r (hit return)...")

        if( all(!is.na(bd.miss)) ){
          
          #   ---- We have some gaps.
          
          #   ---- Estimate an overdispersion parameter.  
          disp <- overDphi(model=c.fit,family="poisson",type="pearson")

          #   ---- Function vcov returns unscaled variance-covariance matrix, so scale by overdispersion.
          sig <- disp * vcov( c.fit )  

          cat(paste("...Poisson over-dispersion in catch model for trap ", trapID, " = ", disp, "\n"))
          cat("in bootstrap_passage.r (hit return)...")

          #   ---- Coefficients.
          beta <- coef( c.fit )

          #   ---- If there is only one coefficient, then number of non-zero catches is <= 10.
          #   ---- It is possible for number of non-zero catches to be 0 (they never caught anything)
          #   ---- In the latter case, coefficient is large negative and causes estimates to blow.
          #   ---- Trap this situation.  The correct estimate in this case is 0.
          #   ---- Not sure the trapped scenario can still happen, given that we now get rid of 
          #   ---- precedent and antecedent zeros.  We specifically exclude zero-catch traps from 
          #   ---- being estimated.  
          if( (length(beta) == 1) & (beta[1] < -10)){
            rbeta <- matrix( -10, nrow=R, ncol=1 )
          } else {
            #   ---- Generate random coefficients.
            rbeta <- rmvnorm(n=R, mean=beta, sigma=sig, method="chol")  # R random realizations of the beta vector. rbeta is R X (n coef)
          }

          #   ---- Predict catches using random coefficients.
          #   ---- When computed, pred is a matrix where each column is a random realization of model 
          #   ---- predictions for missing catches. There are R columns (sets of predictions).

          pred <- X %*% t(rbeta) + log(gaps)
          pred <- exp(pred)                          #   ---- Pred is nrow(X) X R.

          #   ---- But remember, it is possible for a gap to be small, say 3 hours, and the resulting gap be
          #   ---- on the same batch date as another.  So, we need to sum over batch dates.  Do this for every column.
          pred <- apply( pred, 2, function(x,bd){tapply(x,bd,sum)}, bd=bd.miss )
          pred <- matrix( unlist(pred), nrow=length(unique(bd.miss)), ncol=R )

          #   ---- Make catch matrix the correct size by including the observed counts
          ind.mat <- matrix( trap.ind & (grand.df$imputed.catch > 0), nrow=n.grand.df, ncol=R )
                
          #   ---- Replaces imputed values with other realizations contained in pred.  This is 
          #   ---- nrow(grand.df) X R, or (n.batch.days*n.traps) X R.  
          c.pred[ind.mat] <- pred   
        }
      }


      #   ---- Generate random realizations of efficiency.  We utilize the non-deciamal trapID, since we never fit 
      #   ---- efficiency on the decimal traps alone.  This maps decimal traps of catch to non-decimal traps of 
      #   ---- efficiency.  
      ind <- which( as.character(round(as.numeric(trapID),0)) == names(eff.fits) )
      if( length(ind) > 0 ){

        e.fit <- eff.fits[[ind]]
        e.X <- eff.X[[ind]]
        eff.obs.data <- eff.X.obs.data[[ind]]
        e.type <- eff.type[[ind]]
            
        #   ---- The e.X design matrix has columns in an order that was convenient in eff_model.r.  This 
        #   ---- order needs to be checked, to ensure alignment with the order in e.fit.  It is easier to
        #   ---- manipulate the column order in e.X (one matrix), than all the stuff in e.fit (a list).  
        if(ncol(e.X) > 1){
          timeVar <- sort(colnames(e.X)[grepl("time",colnames(e.X),fixed=TRUE)])  
          fit.Vars <- names(coef(e.fit))
          notTimeVar <- fit.Vars[!(grepl("tmp.bs",fit.Vars,fixed=TRUE))]
          notTimeVar <- notTimeVar[notTimeVar != "(Intercept)"]
          thisOrder <- c("Intercept",timeVar,notTimeVar)
          e.X <- e.X[,thisOrder]
        }
      
        #   ---- This is a 2-vector of the first and last efficiency trials.
        e.ind <- eff.ind.inside[[ind]]  
            
        #   ---- This is an indicator for days inside the efficiency season.
        e.ind <- (e.ind[1] <= grand.df$batchDate) & (grand.df$batchDate <= e.ind[2]) & trap.ind 
            
        #   ---- The vector of dates inside efficiency season.  This is used to 
        #   ---- line up with catches because catch seasons vary.
        e.dts <- eff.X.dates[[ind]] 

        #   ---- Check if there are no efficiency trials.  Not sure what this is enh eff framework.  
        if( (!is.list(e.fit) & e.type != 5) | length(e.fit) == 0 | (e.type == 5 & length(e.fit) == 0) ){
          e.pred <- matrix( NA, nrow(c.pred), ncol(c.pred) )
        } else {

          #   ---- Variance matrix,
          #   ---- Check if less than eff.min.spline.samp.size.  
          if( e.fit$df.residual == 0 | nrow(e.fit$data) < 10 ){
            disp <- 1
          } else {
            
            #   ---- Estimate an overdispersion parameter. 
            disp <- overDphi(model=e.fit,family="binomial",type="pearson")

          }

          cat(paste("...Binomial over-dispersion in efficiency model for trap ", as.character(round(as.numeric(trapID),0)), " = ", disp, "\n"))

          #   ---- Function vcov returns unscaled variance-covariance matrix. 
          #   ---- Scale by overdispersion.
          sig <- disp * vcov( e.fit )   

          #   ---- Coefficients.
          beta <- coef( e.fit )
         
          #   ---- Generate R random coefficients of the beta vector. 
          #   ---- Addition of bias-corrected efficiency complicates things. 
          if( length(coef(e.fit)) == 1 ){
            
            #   ---- Reproduce the variance with our bias-corrected estimate of the mean efficiency. 
            #   ---- Construct, via the general expression for a generalized linear model's variance,
            #   ---- the number needed, assuming a logit link.  McCulloch, C.E. & Searle, S. R. 
            #   ---- Generalized, Linear, and Mixed Models, p. 147.  Note we also use an adjusted
            #   ---- mean that incorporates the bias. 
            
#             #   ---- Example from the above book, p. 147.
#             egWeight <- c(5,5,5,5,5,5,5,5,5,5)
#             egY <- c(0,0,2,2,3,4,5,5,5,5)
#             egConc <- c(1/128,1/64,1/32,1/16,1/8,1/4,1/2,1,2,4)
#             
#             #   ---- CAMP RST style.
#             eg <- glm(egY / egWeight ~ 1 + log(egConc), family=binomial, weights=egWeight )
#             vcov(eg)
#             
#             #   ---- Emulate our intercept-only model.
#             eg2 <- glm(egY / egWeight ~ 1, family=binomial, weights=egWeight )
#             vcov(eg2)            
#             
#             #   ---- Manipulate the dispersion. 
#             resids <- residuals(eg2, type="pearson")
#             toss.ind <- (abs(resids) > 8)
#             resids <- resids[!toss.ind]
#             disp <- sum( resids*resids ) / (e.fit$df.residual - sum(toss.ind))
#             if( disp < 1.0 ){
#               disp <- 1.0
#             }
#             
#             #   ---- reproduce our below.
#             X <- rep(1,length(egWeight))
#             p <- (sum(egY) + 0) / (sum(egWeight) + 0) 
#             w <- egWeight*rep(p*(1 - p),length(X))
#             
#             diagonal <- matrix(rep(0,length(X)*length(X)),length(X),length(X))
#             for(i in 1:length(X)){
#               diagonal[i,i] <- w[i]
#             }
#             
#             #   ---- This "matrix" is 1x1 here by design...only doing this for intercept-only models.
#             sig <- as.matrix(disp*( solve(t(X) %*% diagonal %*% X) ))
            
            if(e.type == 5){
              
              #   ---- Do the bias adjustment based on the data that went into the enh eff estimation.  
              X <- rep(1,length(e.X))
              p <- (sum(e.fit$data$nCaught) + 1) / (sum(e.fit$data$nReleased) + 1)
              w <- e.fit$data$nReleased*rep(p*(1 - p),length(X))
              
              diagonal <- matrix(rep(0,length(X)*length(X)),length(X),length(X))
              for(i in 1:length(X)){
                diagonal[i,i] <- w[i]
              }
              
              #   ---- This "matrix" is 1x1 here by design...only doing this for intercept-only models.
              sig <- as.matrix(disp*( solve(t(X) %*% diagonal %*% X) ))
              rbeta <- rmvnorm(n=R, mean=log(p/(1-p)),sigma=sig,method="chol")

            
            } else {
              X <- rep(1,length(eff.obs.data$nCaught))
              p <- (sum(eff.obs.data$nCaught) + 1) / (sum(eff.obs.data$nReleased) + 1)
              w <- eff.obs.data$nReleased*rep(p*(1 - p),length(X))
              
              diagonal <- matrix(rep(0,length(X)*length(X)),length(X),length(X))
              for(i in 1:length(X)){
                diagonal[i,i] <- w[i]
              }
              
              #   ---- This "matrix" is 1x1 here by design...only doing this for intercept-only models.
              sig <- as.matrix(disp*( solve(t(X) %*% diagonal %*% X) ))
              rbeta <- rmvnorm(n=R, mean=log(p/(1-p)),sigma=sig,method="chol")
            }
#             #   ---- Bootstrap on the observed efficiency trials.
#             bsDF <- lapply(1:100000, function(x) eff.obs.data[sample(seq(1:length(eff.obs.data$nReleased)),replace=TRUE),])
#             bsp <- sapply(bsDF, function(x) (sum(x$nCaught) + 1) / (sum(x$nReleased) + 1))
#             
#             hist(bsp)
#             bspMean <- mean(bsp)
#             bspMedian <- median(bsp)
#             bspVar <- var(bsp)*this is wrong
            
          } else {
            rbeta <- rmvnorm(n=R, mean=beta, sigma=sig, method="chol")  
          }
          
          #   ---- Predict efficiency using random coefficients
          #   ---- When computed, pred is a matrix where each column is a random realization of model predictions
          #   ---- for missing catches. There are R columns (sets of predictions).
          pred <- (e.X %*% t(rbeta))
          pred <- 1 / (1 + exp(-pred))                      # Pred is nrow(e.X) X R.

          #   ---- Use mean-predicted efficiency for times outside first and last trials.  
          ind.mat <- matrix( trap.ind, nrow=n.grand.df, ncol=R )
          e.means <- matrix( colMeans( pred ), byrow=T, nrow=sum(trap.ind), ncol=R )

          #   ---- This is complicated, but we have to line up the catch dates with 
          #   ---- the efficiency dates.  Because length of seasons vary, this is necessary.
          df.c <- data.frame(batchDate=format(grand.df$batchDate[trap.ind]), in.catch = TRUE, stringsAsFactors=FALSE )
          df.e <- data.frame(batchDate=format(e.dts), in.eff = TRUE, stringsAsFactors=FALSE )

          df.ce <- merge( df.c, df.e, all.x=TRUE )
          df.ec <- merge( df.e, df.c, all.x=TRUE )

          df.ec$in.catch[ is.na(df.ec$in.catch) ] <- FALSE
          df.ce$in.eff[ is.na(df.ce$in.eff) ] <- FALSE

          #   ---- Predictions that are in the catch data set,
          pred <- pred[ df.ec$in.catch, ]   
                
          #   ---- Predictions inside the season, on the right dates,  
          e.means[ df.ce$in.eff ] <- pred  
          
          #   ---- Assign mean outside of eff.ind.inside[[trap]], and efficiency model inside season.
          e.pred[ind.mat] <- e.means    
        }
      }
      cat("...BS complete\n")
    }

    assign("c.pred", c.pred, pos=.GlobalEnv)
    assign("e.pred", e.pred, pos=.GlobalEnv)

    #   ---- Estimate passage.
    #   ---- Matrices c.pred and e.pred are the same size, so just divide.
    test <- ifelse(grand.df$imputed.catch > 0 & grand.df$imputed.catch < 1,grand.df$totalEstimatedCatch - grand.df$imputedCatch,0)
    c.pred <- apply(c.pred,2,function(x) x + test)
    c.pred2 <- c.pred / e.pred    # Re-use the c.pred matrix to save space

    
    grand.df[as.Date(grand.df$batchDate) == "2013-04-12",]
    
    c.pred2[c(79,229,300),]
    e.pred[c(79,229,300),]
    
    hist(apply(c.pred2[c(79,229,300),],2,mean))
    
    hist(c.pred2[c(300),])
    mean(c.pred2[c(300),])
    #   ---- Now, average over traps
    #   ---- At this point, c.pred is a (n.batch.day*n.trap) X R matrix, with each cell containing the passage estimated
    #   ---- at a particular trap at the site for a particular batch day for a particular iteration of the bootstrap.
    #   ---- Row dimension of list items corresponds to (batch days x trap), columns correspond to iterations.
    #   ---- We now need to average the cells over the traps, and summarize by time.  Do this by calling 
    #   ---- F.summarize.passage on each column.

    #   ---- Get Julian week information in the case of weeks.  Only adds a couple of seconds, so don't check if 
    #   ---- the temporal summary is by week;  just do it. 
    db <- get( "db.file", envir=.GlobalEnv ) 
    ch <- odbcConnectAccess(db)
    JDates <- sqlFetch( ch, "Dates" )
    close(ch) 
    
    JDates$uniqueDate <- as.Date(JDates$uniqueDate,format="%Y-%m-%d")
    JDates <- JDates[JDates$uniqueDate >= attr(grand.df,"min.date") & JDates$uniqueDate <= attr(grand.df,"max.date"),]
    
    #   ---- Internal function to summarize catch by s.by by applying
    #   ---- F.summarize to every column of pass.
    f.sumize.pass <- function(p, s.by, bd){#}, imp.catch){
      
      #   p <- c.pred
      df <- data.frame( batchDate=bd, passage=p, imputed.catch=1 )

      #   ---- Attach JDates if we need to bootstrap by week.  
      attr(df,"JDates") <- JDates
      n <- F.summarize.passage( df, s.by )
      n$passage
    }
    pass <- apply( c.pred2, 2, f.sumize.pass, s.by=sum.by, bd=grand.df$batchDate)
    pass <- matrix( unlist(pass), n.len, R )

    #   ---- Compute bias corrected bootstrap CIs by applying 
    #   ---- f.bias.bs.ci to every row of pass to get bootstrap intervals.
    f.bias.acc.ci <- function( x, alpha, x.orig ){
      p <- mean( x > x.orig, na.rm=TRUE)
      z.0 <- qnorm( 1 - p )
      z.alpha <- qnorm( 1 - (alpha/2))
      p.L <- pnorm( 2*z.0 - z.alpha )
      p.H <- pnorm( 2*z.0 + z.alpha )
      ci <- quantile( x[ !is.na(x) & (x < Inf) ], p=c(p.L, p.H) )
      ci
    }
    ans <- apply( pass, 1, f.bias.acc.ci, alpha=(1-conf), x.orig=n.orig )
    ans <- as.data.frame(t(matrix( unlist(ans), 2, n.len )))

  }

  #   ---- Append lower and upper end points and return
  names(ans) <- paste0( c("lower.", "upper."), conf*100 )
  ans <- data.frame( n.orig, ans, stringsAsFactors=F )

  ans

}
