F.efficiency.model <- function( obs.eff.df, plot=T, method=1, max.df.spline=4, plot.file=NA ){
#
#   Compute and estimate efficiency for all traps and days that are missing in the input data set.
#   i.e., "impute" a value for efficiency when it is missing
#
#   input:
#   obs.eff.df = data frame with at least columns $batchDate, and $efficiency.
#       $efficiency is NA for all days that we need an estimate.  
#   method = scalar specifying type of extrapolation to do.  Method = 1 takes average of entire season. 
#       Method = 2 uses earliest observed efficiency between in intervals between efficiency trials (so called
#       constant model).  Method = 3 is a b-spline model with up to 'max.df.spline' degrees of freedom.
#   max.df.spling = maximum degrees of freedom for splines
#
#   Output:
#   A data frame with all observed $efficiency values, plus a the column $gam.estimated 
#   which is 1 = "Yes" for those that came from the model. 
#


#   It is possible to implement a GAM here using covariates like flow, time of day
#   check was done, etc. and a smooting component.  For now, I will only implement a
#   simple step function
#   obs.eff.df <- eff 
#   plot <- plot
#   method <- method
#   max.df.spline <- df.spline

ans <- NULL
traps <- sort( unique(obs.eff.df$trapPositionID))

#cat("%%%%%%%%%%%%%% in eff_model.r\n")
#print(traps)
#readline()

fits <- all.X <- all.ind.inside <- all.dts <- vector("list", length(traps))
names(fits) <- traps
names(all.X) <- traps
names(all.dts) <- traps
names(all.ind.inside) <- traps

for( trap in traps ){

    df <- obs.eff.df[ is.na(obs.eff.df$trapPositionID) | (obs.eff.df$trapPositionID == trap), ]

    
    ind <- !is.na(df$efficiency)
    obs <- df$efficiency[ ind ]
    
    if( method == 1 ){
        #   Season mean model
        obs.mean <- mean(obs)  # This is MOR = mean of ratios
        cat(paste("MOR efficiency= ", obs.mean, "\n"))
        
        obs.mean <- (sum(df$nCaught[ind])+1) / (sum(df$nReleased[ind])+1)   # this is ROM = Ratio of means, with bias correction
        cat(paste("ROM efficiency= ", obs.mean, " (ROM will be used)\n"))
        
        #   If want to use ROM for missing efficiencies only, uncomment the next line
        #df$efficiency[!ind] <- obs.mean
        
        #   If, however, you want to use ROM for all days, missing or not, uncomment the next line
        df$efficiency <- obs.mean
        
        fits[[trap]] <- data.frame(nCaught=df$nCaught[ind], nReleased=df$nReleased[ind])
    } else if( method == 2 ) {   
        #   WARNING: method 2 is not fully programmed to be compatible with bootstrapping.  Don't use unless you program the bootstrapping to accept it.
        fit <- approx( df$batchDate[ind], df$efficiency[ind], xout=df$batchDate, method="constant", rule=2 )
        df$efficiency <- fit$y
    } else if( method == 3 ) {    

        #   Fit glm model, increasing df, until something goes wrong
        strt.dt <- min( df$batchDate[ind], na.rm=T )  # Earliest date with an efficiency trial
        end.dt  <- max( df$batchDate[ind], na.rm=T )  # Latest date with efficiency trial
        ind.inside <- (strt.dt <= df$batchDate) & (df$batchDate <= end.dt)
        inside.dates <- c(strt.dt, end.dt)

        tmp.df <- df[ind & ind.inside,]
        cat(paste("\n\n++++++Efficiency model fitting for trap:", trap, "\n"))
        print(tmp.df)

        
        #   Check that there are adequate trials at this trap
        if( sum(ind & ind.inside) == 0  ){
            #   No efficiency trials at this trap
            cat( paste("NO EFFICIENCY TRIALS FOR TRAP", trap, "\n") )
            cat( paste("Catches at this trap will not be included in production estimates.\n"))
            fits[[trap]] <- NA
            all.X[[trap]] <- NA
            df$efficiency <- NA
        } else if( sum(ind & ind.inside) >= 1 ){

            fit <- glm( nCaught / nReleased ~ 1, family=binomial, data=tmp.df, weight=nReleased )   # null model
            fit.AIC <- AIC(fit)
    
            cat(paste("df= ", 1, ", conv= ", fit$converged, " bound= ", fit$boundary, " AIC= ", round(fit.AIC, 4), "\n"))
    
            if( nrow(tmp.df) < 10 ){
                #   Go with the mean model
                cat("Fewer than 10 trials found.  Mean efficiency model used\n")
            } else {
                require(splines)
        
                cur.df <- 3
                repeat{
                 
                    cur.bspl <- bs( df$batchDate[ind.inside], df=cur.df )
                    tmp.bs <- cur.bspl[!is.na(df$efficiency[ind.inside]),]
        
                    cur.fit <- glm( nCaught / nReleased ~ tmp.bs, family=binomial, data=tmp.df, weight=nReleased )   
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
            
            fits[[trap]] <- fit     # Save fit for bootstrapping
            all.ind.inside[[trap]] <- inside.dates   #    Save this season dates for bootstrap routine.  
    
            if( length(coef(fit)) <= 1 ){
                pred <- matrix( coef(fit), sum(ind.inside), 1 )
                X <- matrix( 1, sum(ind.inside), 1)
            } else {
                X <- cbind( 1, bspl )
                pred <- X %*% coef(fit)
            }
            
            all.X[[trap]] <- X   # Save X for bootstrapping
            all.dts[[trap]] <- df$batchDate[ind.inside]   #  Save dates we predict at for use in bootstrapping
            
            pred <- 1 / (1 + exp(-pred))   # pred is all efficiencies for dates between min and max of trials. 
            
            #   If you want to use observed efficiency on days when efficiency trials were turn, uncomment the following 
            #   three lines.  
            #miss.eff.inside <- ind.inside & !ind  # missing efficiencies inside first and last trials, sized same as df
            #miss.eff <- miss.eff.inside[ind.inside]      # missing efficiencies inside first and last trials, sized same as pred
            #df$efficiency[miss.eff.inside] <- pred[miss.eff]
            
            #   If, however, you want to use the modeled efficiency for all days, even when a trial was done, use these 
            #   lines.  
            df$efficiency[ind.inside] <- pred
    
            
            #print( c(sum(miss.eff.inside), sum(miss.eff)) )
            #print( c(length(miss.eff.inside), length(miss.eff)) )
            
            
            
            #   use the mean of spline estimate for all dates outside efficiency trial season
            mean.p <- mean(pred, na.rm=T)
            df$efficiency[!ind.inside] <- mean.p
        }
    }
    
 
    #   Uncomment the following line if using imputed value for all days.  Otherwise, comment it out, and imputed.eff will tell which are observed.
    #   With the following uncommented, you can find efficiency trials in grand.df with !is.na(grand.df$nReleased)
    ind <- rep(F, nrow(df))   
    
    df$imputed.eff <- factor( !ind, levels=c(T,F), labels=c("Yes", "No"))
    df$trapPositionID <- trap
    
    ans <- rbind(ans, df)
}

attr(ans,"subsites") <- attr(obs.eff.df, "subsites")
attr(ans,"site.name") <- attr(obs.eff.df, "site.name")

#   Make a plot if called for
if( !is.na(plot.file) ) {
    out.fn <- F.plot.eff.model( fit, ans, plot.file )
} else {
    out.fn <- NULL
}

ans <- list(eff=ans, fits=fits, X=all.X, ind.inside=all.ind.inside, X.dates=all.dts)
attr(ans, "out.fn.list") <- out.fn

ans

}
