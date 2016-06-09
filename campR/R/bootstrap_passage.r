#' @export
#' 
#' @title F.bootstrap.passage
#'   
#' @description Bootstrap or Monte-Carlo simulate data sufficient to compute
#'   confidence intervals for passage.
#'   
#' @param grand.df A data frame containing both daily estimated passage and 
#'   efficiency, for each trap.
#' @param catch.fits A list of Poisson fitted objects, possibly with basis 
#'   spline covariates, used to imput missing catches, for each trap.
#' @param catch.Xmiss A list containing a spline basis matrix of imputed days 
#'   where catch is missing for each trap.
#' @param catch.gapLens A list containing a numeric vector of hours of "Not 
#'   fishing" for "Not fishing" days, necessarily with all entries less than 24,
#'   for each trap.
#' @param catch.bDates.miss A list containing a POSIX vector of "Not fishing" 
#'   \code{batchDate}s for missing catches, for each trap.  Necessary because 
#'   one \code{batchDate} may have two (or more) gaps.
#' @param eff.fits A list of binomial logistic regression fitted objects used to
#'   compute efficiency.  One per trap.
#' @param eff.X NEED TO CHECK. A list containing a numeric vector of days 
#'   lacking an efficiency trial, and for which efficiency must be estimated,
#'   for each trap.
#' @param eff.ind.inside NEED TO CHECK. A list containing the first and last day
#'   of a sequence of efficiency trials, for each trap.
#' @param eff.X.dates NEED TO CHECK. A list xxxx, for each trap.
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
#'   Imputed catch originates from periods of 'Not fishing' in excess of two 
#'   hours, while imputed efficiency results from days between the first and 
#'   last day of a recorded efficiency trial.  Any one day may lead to several 
#'   instances of imputed catch, but at most, only one instance of imputed 
#'   efficiency.  Since days of operation varies over different traps, the 
#'   imputation periods vary as well.
#'   
#'   Bootstrapping of each of catch and efficiency is organized via matrices of 
#'   dimension \eqn{\code{nrow(grand.df)} \times R}, where rows hold unique 
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
#' @section Variance Matrices: 
#' Catch models are fit via a Poisson generalized 
#' linear model.  Often, these models are overdispersed, with a large Pearson 
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
#' @section Random Realizations: 
#' Catch fit models are utilized to generate random
#'   realizations of catch for each individual trap.  To do this, the 
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
#'   a new prediction for missing catches, with each modified (word choice?) by 
#'   the log of the trap down-time, i.e., trap down-time is an offset. (i'm 
#'   specifically avoiding the word 'gap,' since that has a different definition
#'   now.)  Resulting predictions for imputed catch are then exponentiated, and 
#'   then combined with the observed catch to create a full day-based temporal 
#'   fishing record for each trap.  (In the case that no imputation was required
#'   for catch, I don't think that the numbers vary.  How is this correct, i.e.,
#'   how do we capture variability in catch?)
#'   
#'   (Also, the dimension utilized in the \code{rmvnorm} function assumes a 
#'   certain dimensionality of the underlying model, e.g., say a fourth-degree
#'   model was used for days between date A and date B, and maybe a third-degree
#'   model was used between date B and date C.  Couldn't it be the case that a
#'   random realization of catch would result in a fifth-degree model bewteen
#'   date A and date B, say, and a quadratic between date B and date C?  Are we 
#'   missing some of the variability in catch by assuming that the dimension of 
#'   the beta vector from the catch model is non-varying?)
#'   
#' @seealso \code{F.est.catch}, \code{F.est.eff}, \code{summarize.passage}
#' 
#' @author Trent McDonald (tmcdonald@west-inc.com)
#' 
#' @examples 
#' 
F.bootstrap.passage <- function( grand.df, catch.fits, catch.Xmiss, catch.gapLens, catch.bDates.miss, eff.fits, eff.X, eff.ind.inside, eff.X.dates, sum.by, R, ci=T ){

  # grand.df <- grand.df
  # catch.fits <- catch.and.fits$fits
  # catch.Xmiss <- catch.and.fits$X.miss
  # catch.gapLens <- catch.and.fits$gaps
  # catch.bDates.miss <- catch.and.fits$bDates.miss
  # eff.fits <- eff.and.fits$fits
  # eff.X <- eff.and.fits$X
  # eff.ind.inside <- eff.and.fits$ind.inside
  # eff.X.dates <- eff.and.fits$X.dates
  # sum.by <- summarize.by
  # R <- 100
  # ci=T <- ci

  library(mvtnorm)

  #   ---- Set the confidence level of the intervals.
  conf <- 0.95   

  #   ---- Get Julian weeks for this timeframe.  
  if(sum.by == 'week'){
    jDates <- subset(the.Jdates, as.Date(uniqueDate) >= min.date & as.Date(uniqueDate) <= max.date,c(uniqueDate,year,julianWeek,julianWeekLabel))
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

          #   ---- Variance matrix.
          #   ---- We must cut down the over-dispersion parameter to avoid obvious
          #   ---- a-typical residuals.  My approach is to toss the largest and smallest 
          #   ---- 20% of residuals, then compute overdispersion.
          resids <- residuals(c.fit, type="pearson")
          qrds <- quantile( resids, p=c(.2, .8))
          toss.ind <- (resids < qrds[1]) | (qrds[2] < resids)
          resids <- resids[!toss.ind]
          disp <- sum( resids*resids ) / (c.fit$df.residual - sum(toss.ind))
          if( disp < 1.0 ){
            disp <- 1.0
          }

          #   ---- Visually examine the residuals.  
          # plot(predict(c.fit,type="response"), residuals(c.fit, type="pearson"))

          #   ---- Uncomment the following line to include all residuals in overdispersion.
          # disp <- sum(residuals(c.fit, type="pearson")^2) / c.fit$df.residual

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
          if( (length(beta) == 1) & (beta[1] < -2)){
            rbeta <- matrix( -2, nrow=R, ncol=1 )
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


      #   ---- Generate random realizations of efficiency.  
      ind <- which(trapID == names(eff.fits))
      if( length(ind) > 0 ){

        e.fit <- eff.fits[[ind]]
        e.X <- eff.X[[ind]]
            
        #   ---- This is a 2-vector of the first and last efficiency trials.
        e.ind <- eff.ind.inside[[ind]]  
            
        #   ---- This is an indicator for days inside the efficiency season.
        e.ind <- (e.ind[1] <= grand.df$batchDate) & (grand.df$batchDate <= e.ind[2]) & trap.ind 
            
        #   ---- The vector of dates inside efficiency season.  This is used to 
        #   ---- line up with catches because catch seasons vary.
        e.dts <- eff.X.dates[[ind]] 

        #   ---- Check if there are no efficiency trials.  
        if( !is.list(e.fit) | length(e.fit) == 0 ){
          e.pred <- matrix( NA, nrow(c.pred), ncol(c.pred) )
        } else {

          #   ---- Variance matrix,
          #   ---- Check if only one efficiency trial at this trap.
          if( e.fit$df.residual == 0 ){
            disp <- 1
          } else {
            resids <- residuals(e.fit, type="pearson")

            # qrds <- quantile( resids, p=c(.025, .975))
            # toss.ind <- (resids < qrds[1]) | (qrds[2] < resids)

            #   ---- For binomial overdispersion, there are not very many efficiency trials.
            #   ---- So, I do not want to toss the top 2.5% (or other) residuals in magintude, 
            #   ---- like I did for the catches.  However, we need to ensure that there are 
            #   ---- not some very very large residuals.  Thus, any greater than a cut off 
            #   ---- will be eliminated.
            toss.ind <- (abs(resids) > 8)
            resids <- resids[!toss.ind]
            disp <- sum( resids*resids ) / (e.fit$df.residual - sum(toss.ind))
            if( disp < 1.0 ){
              disp <- 1.0
            }
          }

          cat(paste("...Binomial over-dispersion in efficiency model for trap ", trapID, " = ", disp, "\n"))

          #   ---- Function vcov returns unscaled variance-covariance matrix. 
          #   ---- Scale by overdispersion.
          sig <- disp * vcov( e.fit )   

          #   ---- Coefficients.
          beta <- coef( e.fit )

          #   ---- Generate R random coefficients of the beta vector. 
          rbeta <- rmvnorm(n=R, mean=beta, sigma=sig, method="chol")  

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
    c.pred <- c.pred / e.pred    # Re-use the c.pred matrix to save space

    #   ---- Now, average over traps
    #   ---- At this point, c.pred is a (n.batch.day*n.trap) X R matrix, with each cell containing the passage estimated
    #   ---- at a particular trap at the site for a particular batch day for a particular iteration of the bootstrap.
    #   ---- Row dimension of list items corresponds to (batch days x trap), columns correspond to iterations.
    #   ---- We now need to average the cells over the traps, and summarize by time.  Do this by calling 
    #   ---- F.summarize.passage on each column.

    #   ---- Internal function to summarize catch by s.by by applying
    #   ---- F.summarize to every column of pass.
    f.sumize.pass <- function(p, s.by, bd){#}, imp.catch){
      df <- data.frame( batchDate=bd, passage=p, imputed.catch=1 )  
      n <- F.summarize.passage( df, s.by )
      n$passage
    }
    pass <- apply( c.pred, 2, f.sumize.pass, s.by=sum.by, bd=grand.df$batchDate)
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
