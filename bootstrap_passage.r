F.bootstrap.passage <- function( grand.df, catch.fits, catch.Xmiss, catch.gapLens, catch.bDates.miss, eff.fits, eff.X, eff.ind.inside, sum.by, R, ci=T ){
#
#   Bootstrap or Monte Carlo simulate data sufficient to compute confidence intervals 
#   for passage. 
#
#   catch.fits = list of Poisson fitted objects used to imput missing catches.  One per trap.  i.e., list
#       has length equal to number of traps at this site. 
#   catch.Xmiss = X matrix for days where catch is missing.  This is constructed in F.catch.model, and passed into here.
#   catch.bDates.miss = batchDates for missing catches.  Needed because it is possible for 2 gaps to be on same batch date.
#   eff.fits = list of Binomial logistic regression fitted objects used to compute efficiency.  One per trap. 
#   R = number of Monte Carlo iterations to do.
#

library(mvtnorm)

conf <- 0.95   # this is the confidence level of the confidence intervals  

cat("---- entering bootstrap\n")



#   ---- Compute THE estimate.  F.summarize.passage first averages over traps then sums by sum.by
#        When this returns, n.orig is a data frame with columns
#           s.by, passage, date, pct.imputed.catch
n.orig <- F.summarize.passage( grand.df, sum.by )
cat("back from summarize\n")

#   ---- If confidence intervals are called for, do them, otherwise return.
n.len <- nrow(n.orig)
n.grand.df <- nrow(grand.df)
if( !ci ){
    na <- rep(NA, n.len)
    ans <- data.frame(l = na, u= na)
} else {


    n.traps <- length(catch.fits)
    #pass <- matrix(0, n.grand.df, R)

    bootbar <- winProgressBar( "Bootstrapping..." )
    barinc <- 1/(10*n.traps)

    
    #   These giant matrices will hold bootstrap iterations
    c.pred <- matrix( grand.df$catch, nrow=nrow(grand.df), ncol=R )
    e.pred <- matrix( grand.df$efficiency, nrow=nrow(grand.df), ncol=R )

    
    cat(paste("n.traps=", n.traps, "\n"))
    cat(paste("n.len=", n.len, "\n"))
        
    #   --- Main iteration loop (over traps)
    for(trap in 1:n.traps){
    
        trapID <- names(catch.fits)[trap]
        trap.ind <- grand.df$trapPositionID == trapID

        #cat("*=*=*=*=*=*=*=*=*\n")
        
        cat(paste("trap=", trapID, "\n" ))
        setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
    
        #   *=*=*=*=*=*=*=* Generate random realizations of CATCH
#        print(catch.fits) 
#        print(catch.Xmiss)
#        print(catch.gapLens)
        
        ind <- which(trapID == names(catch.fits))
        if( length(ind) > 0 ){
            c.fit <- catch.fits[[ind]]
            X <- catch.Xmiss[[ind]]
            gaps <- catch.gapLens[[ind]]
    
            setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
            
            
            bd.miss <- catch.bDates.miss[[trap]]
    
            if( !is.null(bd.miss)){
                #   We have some gaps
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
            
        #        print(bd.miss)
        #        print(names(catch.Xmiss))
        #        print(names(catch.gapLens))
        #        print(nrow(grand.df))
                
                # Variance matrix
                disp <- sum(residuals(c.fit, type="pearson")^2) / c.fit$df.residual
                sig <- disp * vcov( c.fit )   # vcov returns unscaled variance-covariance matrix.  scale by over dispersion.
        
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
            
        #        cat(paste("disp=", disp, "\n"))
        #        cat(paste("sig=", "\n"))
        #        print(sig)
            
            
                
                # Coefficients
                beta <- coef( c.fit )
                
        #        cat(paste("beta=", beta, "\n"))
        #        tmp <- chol.default( sigma, pivot=TRUE )
        #        print.default(tmp)
                      
                # Generate random coefficients       
                rbeta <- rmvnorm(n=R, mean=beta, sigma=sig, method="chol")  # R random realizations of the beta vector. rbeta is R X (n coef)
        
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
        
        #        cat(paste("rbeta=", "\n"))
        #        print(rbeta[1:20,])
        #        print(dim(rbeta))
        #        cat("hit return...")
        #        readline()
            
                
                # Predict catches using random coefficients
                # When computed, pred is a matrix where each column is a random realization of model predictions 
                # for missing catches. There are R columns (sets of predictions)
                
                pred <- X %*% t(rbeta) + log(gaps)
                pred <- exp(pred)    # pred is nrow(X) X R
        
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
        
                
                # But remember, it is possible for a gap to be small, say 3 hours, and the resulting gap be 
                # on the same batch date as another.  So, we need to sum over batch dates.  Do this for every column.
                pred <- apply( pred, 2, function(x,bd){tapply(x,bd,sum)}, bd=bd.miss )
                pred <- matrix( unlist(pred), nrow=length(unique(bd.miss)), ncol=R )        
        
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
        
                
                # Make catch matrix the correct size by including the observed counts
                ind.mat <- matrix( trap.ind & (grand.df$imputed.catch > 0), nrow=n.grand.df, ncol=R )
        
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
        
        #        print( dim(ind.mat) )
        #        print( dim(c.pred) )
        #        print( colSums( ind.mat ))
        #        cat("hit return...")
        #        readline()
                
                c.pred[ind.mat] <- pred   # replaces imputed values with other realizations contained in pred.  This is nrow(grand.df) X R, or (n.batch.days*n.traps) X R
            }
        }
        
        cat("...Catch BS complete")
        
        
        
        #   *=*=*=*=*=*=*=* Generate random realizations of EFFICIENCY

        ind <- which(trapID == names(eff.fits))
        if( length(ind) > 0 ){

            e.fit <- eff.fits[[ind]] 
            e.X <- eff.X[[ind]]
            e.ind <- eff.ind.inside[[ind]]
    
            if( !is.list(e.fit) | length(e.fit) == 0 ){
                #   No efficiency trials
                e.pred <- matrix( NA, nrow(c.pred), ncol(c.pred) )
            } else {
                        
                # Variance matrix
                if( e.fit$df.residual == 0 ){
                    #    Only one efficiency trial at this trap
                    disp <- 1
                } else {
                    disp <- sum(residuals(e.fit, type="pearson")^2) / e.fit$df.residual
                }
                
                sig <- disp * vcov( e.fit )   # vcov returns unscaled variance-covariance matrix.  scale by over dispersion.
        
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
                
                # Coefficients
                beta <- coef( e.fit )        
            
                # Generate random coefficients       
                rbeta <- rmvnorm(n=R, mean=beta, sigma=sig, method="chol")  # R random realizations of the beta vector
        
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
                
                # Predict efficiency using random coefficients
                # When computed, pred is a matrix where each column is a random realization of model predictions 
                # for missing catches. There are R columns (sets of predictions)
                pred <- (e.X %*% t(rbeta))
                pred <- 1 / (1 + exp(-pred))    # pred is nrow(e.X) X R
        
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
                
                # Use mean predicted efficiency for times outside first and last trials
                ind.mat <- matrix( trap.ind, nrow=n.grand.df, ncol=R )
                e.means <- matrix( colMeans( pred ), byrow=T, nrow=sum(trap.ind), ncol=R )
                e.pred[ind.mat] <- e.means    # Assign mean outside of eff.ind.inside[[trap]]
                
                
                ind.mat <- matrix( trap.ind & e.ind, nrow=n.grand.df, ncol=R )
                e.pred[ind.mat] <- pred   # e.pred is (n days*n traps) X R, or nrow(grand.df) X R, or (n.batch.days*n.traps) X R
                
                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
                
                
                #   *=*=*=*=*=*=*=* Store results from this trap.  We average, so just store sum.
                #pass <- apply( p.pred, 2, function(x, ind.by){ tapply( x, ind.by, FUN=sum, na.rm=T ) }, ind.by=sum.by )  # a list of length R, each element is vector of length unique(ind.by)
                #pass <- matrix( unlist(pass), nn, R )
                #pass <- pass + p.pred
                
                
            }
        }
        cat("...Efficiency BS complete\n")

    }


#    print(c.pred[,1:2])
#    print(e.pred[,1:2])
#    print(dim(c.pred))
#    print(dim(e.pred))
#    cat("hit return...")
#    readline()
    

    #   *=*=*=*=*=*=*=* Estimate passage
    #         c.pred and e.pred are the same size, so just divide
    c.pred <- c.pred / e.pred    # Re-use the c.pred matrix to save space

    
    #   ===== Now, average over traps
    #   At this point, c.pred is a (n.batch.day*n.trap) X R matrix, with each cell containing the passage estimated 
    #   at a particular trap at the site for a particular batch day for a particular iteration of the bootstrap. 
    #   Row dimension of list items corresponds to (batch days x trap), columns correspond to iterations.
    #   We now need to average the cells over the traps, and summarize by time.  Do this by calling F.summarize.passage on each column
    

    #   ===== Apply F.summarize to every column of pass
    f.sumize.pass <- function(p, s.by, bd){
        #   Internal function to summarize catch by s.by
        df <- data.frame( batchDate=bd, passage=p, imputed.catch=1 )  # we will not use inputed.catch.  Having F.summarize.passage average imputed catch wastes some time. 
        n <- F.summarize.passage( df, s.by )
        n$passage
    }
    pass <- apply( c.pred, 2, f.sumize.pass, s.by=sum.by, bd=grand.df$batchDate )
    pass <- matrix( unlist(pass), n.len, R )
    
    setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
    
    
    #   ===== An internal function to compute bias corrected bootstrap intervals
    f.bias.acc.ci <- function( x, alpha, x.orig ){
    #   Compute bias corrected bootstrap CI's
        p <- mean( x > x.orig, na.rm=TRUE)
        z.0 <- qnorm( 1 - p )
        z.alpha <- qnorm( 1 - (alpha/2)) 
        p.L <- pnorm( 2*z.0 - z.alpha )
        p.H <- pnorm( 2*z.0 + z.alpha )
        ci <- quantile( x[ !is.na(x) ], p=c(p.L, p.H) )
        ci
    }
    
    
    #   ===== Apply f.bias.bs.ci to every row of pass to get bootstrap intervals.
    ans <- apply( pass, 1, f.bias.acc.ci, alpha=(1-conf), x.orig=n.orig )
    ans <- as.data.frame(t(matrix( unlist(ans), 2, n.len )))
    
    close( bootbar )

}

#   ---- Append lower and upper end points and return
names(ans) <- paste0( c("lower.", "upper."), conf*100 )
ans <- data.frame( n.orig, ans, stringsAsFactors=F )


ans 

}
