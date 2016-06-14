#' @export
#' 
#' @title F.efficiency.model
#'   
#' @description Compute and estimate efficiency for all missing combinations of
#'   traps and days;  i.e., "impute" a value for efficiency when missing.
#'   
#' @param obs.eff.df A data frame with at least variables \code{batchDate} and 
#'   \code{efficiency}, where \code{efficiency} is \code{NA} for all days 
#'   requiring an estimate.
#' @param plot A logical indicating if resulting efficiency should be plotted.
#' @param method A scalar specifying the type of extrapolation to do. 
#'   \code{method = 1} takes average of entire season. \code{method = 2} uses 
#'   earliest observed efficiency for each unique interval between distinct
#'   efficiency trials; i.e., a so called "step-function" constant model. 
#'   \code{method = 3} is a B-spline model with up to \code{max.df.spline}
#'   degrees of freedom.
#' @param max.df.spline The maximum degrees of freedom allowed for splines.
#' @param plot.file The name of the file prefix under which output is to be 
#'   saved.  Set to NA to plot to the Plot window.
#'   
#' @return A data frame with all observed and imputed \code{efficiency} values, 
#'   where variable \code{gam.estimated} identifies days with imputed values.
#' 
#' 
#' 
#' @section Efficiency Methodologies:
#'
#' Selection of \code{method} allows for efficiency estimation to vary, based on
#' need.
#'   
#'   \itemize{ 
#'   \item{\code{method=1} : A "Seasonal-mean model".  This means that a
#'   ratio-of-means bias-corrected global efficiency is calculated, for each 
#'   trap, where the efficiency is estimated via \deqn{\frac{nCaught
#'   + 1}{nReleased + 1}}{(nCaught + 1) / (nReleased + 1).}  Values
#'   for variables \code{nCaught} and \code{nReleased} originate via function
#'   \code{F.get.releases} and the querying of an underlying Access database. 
#'   The resulting ratio-of-means efficiency is then applied to all trapping
#'   days, regardless if data were missing or not.}
#'   
#'   \item{\code{method=2} : A "Constant model."  This means that this model
#'   utilizes a step function to estimate efficiency, where here, steps may
#'   increase or decrease over time.  Each interval of time for which efficiecy
#'   needs to be estimated utilizes the earliest observed efficiency between
#'   efficiency trials.  Note that this method has not been fully programmed to
#'   be compatible with boostrapping in function \code{F.bootstrap.passage}.  This
#'   option should not be used until \code{F.bootstrap.passage} can handle its
#'   results.}
#'   
#'   \item{\code{method=3} : A "B-spline model."  This efficiency estimation 
#'   methodology chops up the date range into small temporal bits.  For each,
#'   possibly different degree polynomials are fit, with each individual
#'   polynomial fitting the data locally.  Each individual polynomial is then
#'   fit together into one final curve, with several conditions ensuring its
#'   smoothness. One condition includes matching the polynomial estimate at the
#'   end of one time point with the connecting starting time point of the next
#'   polynomial.  Others include ensuring that the first derivatives (local
#'   slopes) and second derivatives (local convexity) at these connecting time
#'   points are equal.  
#'   
#'   Estimation requires at least one trapping instance to occur within the 
#'   earliest and latest date with an efficiency trial.  All three can occur on 
#'   the same one day.  Generalized additive models, or GAMs, first fit the
#'   observed efficiency-trial data as
#'   \deqn{\frac{nCaught}{nReleased}}{nCaught / nReleased} outcome
#'   proportions against a null model with a binomial link.  This leads to a
#'   constant, or intercept-only model, where the intercept is the weighted 
#'   avearge of caught fish over all efficiency trials.
#'   
#'   Assuming at least ten efficiency trials, higher-order polynomial models are
#'   considered.  Akaike Information Criterion (AIC), rounded to four decimal
#'   places, determines winning models. Models with lower AICs trump all others,
#'   with the winning model having to be at least two AIC points less than its
#'   nearest competitor.  Function \code{bs} identifies the B-spline basis to
#'   use in higher-order models.  
#'   
#'   In generalized additive models, consideration of higher-order polynomials 
#'   often starts with cubic polynomials partitioned in a temporal piecewise
#'   fashion, chopped at the time values at which observed efficiencies were
#'   recorded. Currently, however, no polynomials of degree greater than three
#'   are considered in efficiency models;  thus, models could either be 
#'   intercept-only, or cubic.
#'   
#'   The \eqn{\beta}s from the final selected GAM are saved for use in
#'   bootstrapping via function \code{F.boostrap.passage}.  They are also 
#'   utilized to form predictions over the entire efficiency trial season, for
#'   each trap.  Model efficiency values are used for all days, i.e., in lieu of
#'   observed data.  All dates outside the efficiency trial season use an
#'   estimated mean spline calculated over all dates.}
#'   
#'   \item{\code{method=4} : An "Enhanced Efficiency model."  To be developed
#'   at a later date.}
#'   
#'   }
#'   
#' @seealso \code{F.get.releases}, \code{F.bootstrap.passage}
#' 
#' @author Trent McDonald (tmcdonald@west-inc.com)
#' 
#' @examples 
#' \dontrun{
#' #   ---- Fit an efficiency model for each unique trapPositionID 
#' #   ---- in data frame obs.eff.df.  
#' F.efficiency.model( obs.eff.df, plot=T, method=1, max.df.spline=4, plot.file=NA)
#' }
F.efficiency.model <- function( obs.eff.df, plot=T, method=1, max.df.spline=4, plot.file=NA ){

  # obs.eff.df <- eff
  # plot <- plot
  # method <- method
  # max.df.spline <- df.spline
  # plot.file <- plot.file

  ans <- NULL
  traps <- sort( unique(obs.eff.df$TrapPositionID))

  fits <- all.X <- all.ind.inside <- all.dts <- vector("list", length(traps))
  names(fits) <- traps
  names(all.X) <- traps
  names(all.dts) <- traps
  names(all.ind.inside) <- traps

  #   ---- Estimate a model for efficiency for each trap in obs.eff.df.
  for( trap in traps ){

    df <- obs.eff.df[ is.na(obs.eff.df$TrapPositionID) | (obs.eff.df$TrapPositionID == trap), ]
    ind <- !is.na(df$efficiency)
    obs <- df$efficiency[ ind ]
    
    #   ---- Take simple average of efficiency trials:  Seasonal-mean model.
    if( method == 1 ){
      
      #   ---- This is MOR, or mean of ratios.  
      obs.mean <- mean(obs)  
      cat(paste("MOR efficiency= ", obs.mean, "\n"))
        
      #   ---- This is ROM = Ratio of means, with bias correction.  
      obs.mean <- (sum(df$nCaught[ind])+1) / (sum(df$nReleased[ind])+1)   
      cat(paste("ROM efficiency= ", obs.mean, " (ROM will be used)\n"))
        
      #   ---- If want to use ROM for missing efficiencies only, uncomment the next line.  
      #df$efficiency[!ind] <- obs.mean
        
      #   ---- If, however, you want to use ROM for all days, missing or not, uncomment the next line.  
      df$efficiency <- obs.mean
        
      fits[[trap]] <- data.frame(nCaught=df$nCaught[ind], nReleased=df$nReleased[ind])
        
    #   ---- Use earliest observed efficiency between efficiency trials:  Constant model.  
    } else if( method == 2 ) {   
      
      #   ---- WARNING: method 2 is not fully programmed to be compatible with bootstrapping.  
      #   ---- Don't use unless you program the bootstrapping to accept it.
      fit <- approx( df$batchDate[ind], df$efficiency[ind], xout=df$batchDate, method="constant", rule=2 )
      df$efficiency <- fit$y
        
    #   ---- B-spline model.
    } else if( method == 3 ) {    

      #   ---- Fit glm model, increasing degress of freedom, until something goes wrong.  
      strt.dt <- min( df$batchDate[ind], na.rm=T )  # Earliest date with an efficiency trial
      end.dt  <- max( df$batchDate[ind], na.rm=T )  # Latest date with efficiency trial
      ind.inside <- (strt.dt <= df$batchDate) & (df$batchDate <= end.dt)
      inside.dates <- c(strt.dt, end.dt)

      tmp.df <- df[ind & ind.inside,]
      cat(paste("\n\n++++++Efficiency model fitting for trap:", trap, "\n"))
      print(tmp.df)
        
      #   ---- Check that there are adequate trials at this trap.  
      if( sum(ind & ind.inside) == 0  ){
        
        #   ---- No efficiency trials at this trap.
        cat( paste("NO EFFICIENCY TRIALS FOR TRAP", trap, "\n") )
        cat( paste("Catches at this trap will not be included in production estimates.\n"))
        fits[[trap]] <- NA
        all.X[[trap]] <- NA
        df$efficiency <- NA
      } else if( sum(ind & ind.inside) >= 1 ){

        #   ---- At least one efficiency trial "inside" for this trap.
        #   ---- Fit a null model.  
        fit <- glm( nCaught / nReleased ~ 1, family=binomial, data=tmp.df, weights=nReleased ) 
        fit.AIC <- AIC(fit)
    
        cat(paste("df= ", 1, ", conv= ", fit$converged, " bound= ", fit$boundary, " AIC= ", round(fit.AIC, 4), "\n"))
    
        #   ---- Consider higher polynomial models.  
        if( nrow(tmp.df) < 10 ){
          
          #   ---- Go with the mean model.  
          cat("Fewer than 10 trials found.  Mean efficiency model used\n")
        } else {

          cur.df <- 3
          repeat{
                 
            cur.bspl <- bs( df$batchDate[ind.inside], df=cur.df )
            tmp.bs <- cur.bspl[!is.na(df$efficiency[ind.inside]),]
        
            cur.fit <- glm( nCaught / nReleased ~ tmp.bs, family=binomial, data=tmp.df, weights=nReleased )   
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
        }
            
        cat("\nEfficiency model:\n")
        print(summary(fit, disp=sum(residuals(fit, type="pearson")^2)/fit$df.residual))
            
        #   ---- Save fit and season's dates for bootstrapping.
        fits[[trap]] <- fit     
        all.ind.inside[[trap]] <- inside.dates   
    
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
            
        #   ---- If you want to use observed efficiency on days when efficiency trials were turn, uncomment.
        #miss.eff.inside <- ind.inside & !ind  # missing efficiencies inside first and last trials, sized same as df
        #miss.eff <- miss.eff.inside[ind.inside]      # missing efficiencies inside first and last trials, sized same as pred
        #df$efficiency[miss.eff.inside] <- pred[miss.eff]
            
        #   ---- If, however, you want to use the modeled efficiency for all days, even when a trial was done, use these. 
        df$efficiency[ind.inside] <- pred

        #   ---- Use the mean of spline estimates for all dates outside efficiency trial season.  
        mean.p <- mean(pred, na.rm=T)
        df$efficiency[!ind.inside] <- mean.p
      }
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

  ans <- list(eff=ans, fits=fits, X=all.X, ind.inside=all.ind.inside, X.dates=all.dts)
  attr(ans, "out.fn.list") <- out.fn

  ans

}
