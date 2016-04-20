#' @export F.bootstrap.passage
#' 
#' @title F.bootstrap.passage
#' 
#' @description
#' 
#'    Bootstrap or Monte Carlo simulate data sufficient to compute confidence intervals
#'    for passage.
#' 
#'    catch.fits = list of Poisson fitted objects used to imput missing catches.  One per trap.  i.e., list
#'        has length equal to number of traps at this site.
#'    catch.Xmiss = X matrix for days where catch is missing.  This is constructed in F.catch.model, and passed into here.
#'    catch.bDates.miss = batchDates for missing catches.  Needed because it is possible for 2 gaps to be on same batch date.
#'    eff.fits = list of Binomial logistic regression fitted objects used to compute efficiency.  One per trap.
#'    R = number of Monte Carlo iterations to do.
#' 
#'  grand.df <- grand.df
#'  catch.fits <- catch.and.fits$fits
#'  catch.Xmiss <- catch.and.fits$X.miss
#'  catch.gapLens <- catch.and.fits$gaps
#'  catch.bDates.miss <- catch.and.fits$bDates.miss
#'  eff.fits <- eff.and.fits$fits
#'  eff.X <- eff.and.fits$X
#'  eff.ind.inside <- eff.and.fits$ind.inside
#'  eff.X.dates <- eff.and.fits$X.dates
#'  sum.by <- summarize.by
#'  R <- 100
#'  ci=T <- ci
#' 
#' 
#' 
#' @param  grand.df <describe argument>
#' @param  catch.fits <describe argument>
#' @param  catch.Xmiss <describe argument>
#' @param  catch.gapLens <describe argument>
#' @param  catch.bDates.miss <describe argument>
#' @param  eff.fits <describe argument>
#' @param  eff.X <describe argument>
#' @param  eff.ind.inside <describe argument>
#' @param  eff.X.dates <describe argument>
#' @param  sum.by <describe argument>
#' @param  R <describe argument>
#' @param  ci=T  <describe argument>
#' 
#' @details <other comments found in file>
#'  get julian weeks and be done with it
#'    ---- Compute THE estimate.  F.summarize.passage first averages over traps then sums by sum.by
#'         When this returns, n.orig is a data frame with columns
#'            s.by, passage, date, pct.imputed.catch
#'  write.csv(grand.df,'C:/Users/jmitchell/Desktop/Incorrect.grand.df.csv')
#'  write.csv(n.orig,'C:/Users/jmitchell/Desktop/Incorrect.n.orig.csv')
#'    ---- If confidence intervals are called for, do them, otherwise return.
#' pass <- matrix(0, n.grand.df, R)
#'     bootbar <- winProgressBar( "Bootstrapping..." )
#'     barinc <- 1/(10*n.traps)
#'    These giant matrices will hold bootstrap iterations
#'    --- Main iteration loop (over traps)
#' cat("*=*=*=*=*=*=*=*=*\n")
#'         setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'    *=*=*=*=*=*=*=* Generate random realizations of CATCH
#'         print(catch.fits)
#'         print(catch.Xmiss)
#'         print(catch.gapLens)
#'  cat(paste("catch.fits=",names(catch.fits),"\n"))
#'             setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'    Debugging
#'  readline()
#'    We have some gaps
#'                 setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'                 print(names(catch.Xmiss))
#'                 print(names(catch.gapLens))
#'                 print(nrow(grand.df))
#'  Variance matrix
#'    We must cut down the over-dispersion parameter to avoid obviously
#'    a-typical residuals.  My approach is to toss the largest and smallest 5% of residuals
#'    then compute overdispersion
#' plot(predict(c.fit,type="response"), residuals(c.fit, type="pearson"))
#'    The following line includes all residuals in computation of overdispersion
#' disp <- sum(residuals(c.fit, type="pearson")^2) / c.fit$df.residual
#'                 setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'                 print(c(df.resid=c.fit$df.residual))
#'                 cat(paste("sig=", "\n"))
#'                 print(sig)
#'                 readline()
#'  Coefficients
#'  cat(paste("beta=", beta, "\n"))
#'                 tmp <- chol.default( sig, pivot=TRUE )
#'                 print.default(tmp)
#'    If there is only one coefficient, then number of non-zero catches is <10.
#'    It is possible for number of non-zero catches to be 0 (they never caught anything)
#'    In the latter case, coefficient is large negative and causes estimates to blow.
#'    Trap this situation.  The correct estimate in this case is 0.
#'  Generate random coefficients
#'                 setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'                 cat(paste("rbeta=", "\n"))
#'                 print(rbeta[1:20,])
#'                 print(dim(rbeta))
#'  
#'                 cat(paste("gaps=", "\n"))
#'                 print(gaps[1:20,])
#'                 print(dim(gaps))
#'  Predict catches using random coefficients
#'  When computed, pred is a matrix where each column is a random realization of model predictions
#'  for missing catches. There are R columns (sets of predictions)
#'                 print(sum(pred>1000000))
#'                 setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'  But remember, it is possible for a gap to be small, say 3 hours, and the resulting gap be
#'  on the same batch date as another.  So, we need to sum over batch dates.  Do this for every column.
#'                 setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'  Make catch matrix the correct size by including the observed counts
#'                 setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'         print( dim(ind.mat) )
#'         print( dim(c.pred) )
#'         print( colSums( ind.mat ))
#'         cat("hit return...")
#'         readline()
#'                 cat("in bootstrap.passage.r.  hit return...")
#'                 readline()
#'                  print(table(grand.df[grand.df$imputed.catch > 0,]$trapPositionID))
#'                  cat(paste0('ind.mat - ',dim(ind.mat)[1],'\n'))
#'                  cat(paste0('grand.df - ',dim(grand.df)[1],'\n'))
#'                  cat(paste0('trap.ind - ',length(trap.ind),'\n'))
#'                  cat(paste0('pred - ',dim(pred)[1],'\n'))
#'                  cat(paste0('c.pred - ',dim(c.pred)[1],'\n'))
#'         cat("...Catch BS complete")
#' pred <<- pred
#'    *=*=*=*=*=*=*=* Generate random realizations of EFFICIENCY
#'    No efficiency trials
#'  Variance matrix
#'     Only one efficiency trial at this trap
#' qrds <- quantile( resids, p=c(.025, .975))
#' toss.ind <- (resids < qrds[1]) | (qrds[2] < resids)
#'    For Binomial overdispersion, there are not very many efficiency trials.
#'    So, I do not want to toss the top 2.5% in magintude, like I did for the catches.
#'    However, we need to ensure that there are not some very very large residuals.
#'    Thus, any greater than a cut off will be eliminated.
#'                     print( sum(toss.ind) )
#'                     print( e.fit$df.residual - sum(toss.ind) )
#'                     readline()
#'                 print(c(dispersion=disp))
#'                 cat("in efficincy part of bootstrap_passage.r")
#'                 readline()
#'                 setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'  Coefficients
#'  Generate random coefficients
#' cat(paste(beta))
#'                 setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'  Predict efficiency using random coefficients
#'  When computed, pred is a matrix where each column is a random realization of model predictions
#'  for missing catches. There are R columns (sets of predictions)
#'  Use mean predicted efficiency for times outside first and last trials
#'    This is complicated, but we have to line up the catch dates with the efficiency dates.  Because length of seasons vary, this is necessary.
#' tmp.df.ce <<- df.ce
#' tmp.df.ec <<- df.ec
#'     print(c.pred[,1:2])
#'     print(e.pred[,1:2])
#'     print(dim(c.pred))
#'     print(dim(e.pred))
#'     cat("hit return...")
#'     readline()
#'    *=*=*=*=*=*=*=* Estimate passage
#'          c.pred and e.pred are the same size, so just divide
#'    Debugging
#'    ===== Now, average over traps
#'    At this point, c.pred is a (n.batch.day*n.trap) X R matrix, with each cell containing the passage estimated
#'    at a particular trap at the site for a particular batch day for a particular iteration of the bootstrap.
#'    Row dimension of list items corresponds to (batch days x trap), columns correspond to iterations.
#'    We now need to average the cells over the traps, and summarize by time.  Do this by calling F.summarize.passage on each column
#'    ===== Apply F.summarize to every column of pass
#'    Internal function to summarize catch by s.by
#'          df <- data.frame( batchDate=bd, passage=p, imputed.catch=imp.catch )  # we WILL use inputed.catch.
#'          rownames(df) <- NULL  # jason adds to make it look like the old df
#'     setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )
#'    ===== An internal function to compute bias corrected bootstrap intervals
#'    Compute bias corrected bootstrap CI's
#'    ===== Apply f.bias.bs.ci to every row of pass to get bootstrap intervals.
#'     close( bootbar )
#'    ---- Append lower and upper end points and return
#' cat("In bootstrap_passage.r.  HIt return...")
#' readline()
#'  # jason makes some bootstrap graphs.
#'  png(filename='C:/Users/jmitchell/Desktop/bs.png',res=200,width=40,height=48,units="in")
#'  par(mfrow=c(24,10))
#'  for(iter in 1:240){
#'    d <- ans[iter,]
#' 
#'    p <- mean( pass[iter,] > x.orig, na.rm=TRUE)
#' 
#'    # bg color
#'    rgb.palette <- colorRampPalette(c("red","pink","white"),space="rgb")
#'    h <- rev(rgb.palette(101))
#' 
#'    par( mar = c(2,1,2,1) )
#'    hist(pass[iter,],breaks=dim(pass)[2]/10,col="white",border="white",main="")  # breaks dependent on sample size
#'    par(new=TRUE)
#'    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=h[round(100*p,0) + 1])#round(100*d$pct.imputed.catch,0) + 1])
#'    par(new=TRUE)
#'    lines(x=rep(d$lower.95,20),y=seq(0,100,length.out=20),lwd=3,type="l",col="blue")
#'    lines(x=rep(d$upper.95,20),y=seq(0,100,length.out=20),lwd=3,type="l",col="blue")
#'    lines(x=rep(d$passage,20),y=seq(0,100,length.out=20),lwd=3,type="l",col="green")
#'    par(new=TRUE)
#'    hist(pass[iter,],breaks=dim(pass)[2]/10,col="white",main=paste0(d$s.by," - ",round(d$passage,0)," - (",round(d$lower.95,0),",",round(d$upper.95,0),") - ",round(d$pct.imputed.catch,2)," - p=",round(p,2)))  # breaks dependent on sample size
#' 
#'    #rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=h[round(d$pct.imputed.catch,0)*100 + 1])
#' 
#'  }
#'  dev.off()
#'  par(mfrow=c(1,1))
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' <insert examples>
#' 
F.bootstrap.passage <- function( grand.df, catch.fits, catch.Xmiss, catch.gapLens, catch.bDates.miss, eff.fits, eff.X, eff.ind.inside, eff.X.dates, sum.by, R, ci=T ){
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
#

library(mvtnorm)

conf <- 0.95   # this is the confidence level of the confidence intervals

#cat("---- entering bootstrap\n")

# get julian weeks and be done with it
if(sum.by == 'week'){
  db <- get( "db.file", env=.GlobalEnv )                                  #   Open ODBC channel
  ch <- odbcConnectAccess(db)
  the.dates <- sqlFetch( ch, "Dates" )                                    #   get the table that has the julian week labels.
  the.dates <<- subset(the.dates, as.Date(uniqueDate) >= min.date & as.Date(uniqueDate) <= max.date,c(uniqueDate,year,julianWeek,julianWeekLabel))
  close(ch)
}

#   ---- Compute THE estimate.  F.summarize.passage first averages over traps then sums by sum.by
#        When this returns, n.orig is a data frame with columns
#           s.by, passage, date, pct.imputed.catch
n.orig <- F.summarize.passage( grand.df, sum.by )
cat("back from summarize\n")

# write.csv(grand.df,'C:/Users/jmitchell/Desktop/Incorrect.grand.df.csv')
# write.csv(n.orig,'C:/Users/jmitchell/Desktop/Incorrect.n.orig.csv')

#   ---- If confidence intervals are called for, do them, otherwise return.
n.len <- nrow(n.orig)
n.grand.df <- nrow(grand.df)
if( !ci ){
    na <- rep(NA, n.len)
    ans <- data.frame(l = na, u= na)
} else {


    n.traps <- length(catch.fits)
    #pass <- matrix(0, n.grand.df, R)

#    bootbar <- winProgressBar( "Bootstrapping..." )
#    barinc <- 1/(10*n.traps)


    #   These giant matrices will hold bootstrap iterations
    c.pred <- matrix( grand.df$totalEstimatedCatch, nrow=nrow(grand.df), ncol=R )      # jason change to totalEstimatedCatch.
    e.pred <- matrix( grand.df$efficiency, nrow=nrow(grand.df), ncol=R )


    cat(paste("n.traps=", n.traps, "\n"))
    cat(paste("n.len=", n.len, "\n"))

    #   --- Main iteration loop (over traps)
    for(trap in 1:n.traps){

        trapID <- names(catch.fits)[trap]
        trap.ind <- grand.df$trapPositionID == trapID

        #cat("*=*=*=*=*=*=*=*=*\n")

        cat(paste("trap=", trapID, "\n" ))
#        setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )

        #   *=*=*=*=*=*=*=* Generate random realizations of CATCH
#        print(catch.fits)
#        print(catch.Xmiss)
#        print(catch.gapLens)
        
        # cat(paste("catch.fits=",names(catch.fits),"\n"))

        ind <- which(trapID == names(catch.fits))
        if( length(ind) > 0 ){
            c.fit <- catch.fits[[ind]]
            X <- catch.Xmiss[[ind]]
            gaps <- catch.gapLens[[ind]]

#            setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )

            bd.miss <- catch.bDates.miss[[trap]]


            #   Debugging
           print(bd.miss)
           cat("in bootstrap_passage.r (hit return)...")
           # readline()

            if( all(!is.na(bd.miss)) ){
                #   We have some gaps
#                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )

#                print(names(catch.Xmiss))
#                print(names(catch.gapLens))
#                print(nrow(grand.df))

                # Variance matrix
                #   We must cut down the over-dispersion parameter to avoid obviously
                #   a-typical residuals.  My approach is to toss the largest and smallest 5% of residuals
                #   then compute overdispersion

                resids <- residuals(c.fit, type="pearson")
                qrds <- quantile( resids, p=c(.2, .8))
                toss.ind <- (resids < qrds[1]) | (qrds[2] < resids)
                resids <- resids[!toss.ind]
                disp <- sum( resids*resids ) / (c.fit$df.residual - sum(toss.ind))
                if( disp < 1.0 ){
                    disp <- 1.0
                }

                #plot(predict(c.fit,type="response"), residuals(c.fit, type="pearson"))

                #   The following line includes all residuals in computation of overdispersion
                #disp <- sum(residuals(c.fit, type="pearson")^2) / c.fit$df.residual

                sig <- disp * vcov( c.fit )   # vcov returns unscaled variance-covariance matrix.  scale by over dispersion.

#                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )

                cat(paste("...Poisson over-dispersion in catch model for trap ", trapID, " = ", disp, "\n"))

#                print(c(df.resid=c.fit$df.residual))
#                cat(paste("sig=", "\n"))
#                print(sig)
               cat("in bootstrap_passage.r (hit return)...")
#                readline()



                # Coefficients
                beta <- coef( c.fit )

               # cat(paste("beta=", beta, "\n"))
#                tmp <- chol.default( sig, pivot=TRUE )
#                print.default(tmp)

                #   If there is only one coefficient, then number of non-zero catches is <10.
                #   It is possible for number of non-zero catches to be 0 (they never caught anything)
                #   In the latter case, coefficient is large negative and causes estimates to blow.
                #   Trap this situation.  The correct estimate in this case is 0.
                if( (length(beta) == 1) & (beta[1] < -2)){
                    rbeta <- matrix( -2, nrow=R, ncol=1 )
                } else {
                    # Generate random coefficients
                    rbeta <- rmvnorm(n=R, mean=beta, sigma=sig, method="chol")  # R random realizations of the beta vector. rbeta is R X (n coef)
                }

#                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )

#                cat(paste("rbeta=", "\n"))
#                print(rbeta[1:20,])
#                print(dim(rbeta))
# 
#                cat(paste("gaps=", "\n"))
#                print(gaps[1:20,])
#                print(dim(gaps))

                # Predict catches using random coefficients
                # When computed, pred is a matrix where each column is a random realization of model predictions
                # for missing catches. There are R columns (sets of predictions)

                pred <- X %*% t(rbeta) + log(gaps)
                pred <- exp(pred)    # pred is nrow(X) X R

#                print(sum(pred>1000000))

#                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )


                # But remember, it is possible for a gap to be small, say 3 hours, and the resulting gap be
                # on the same batch date as another.  So, we need to sum over batch dates.  Do this for every column.
                pred <- apply( pred, 2, function(x,bd){tapply(x,bd,sum)}, bd=bd.miss )
                pred <- matrix( unlist(pred), nrow=length(unique(bd.miss)), ncol=R )

#                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )


                # Make catch matrix the correct size by including the observed counts
                ind.mat <- matrix( trap.ind & (grand.df$imputed.catch > 0), nrow=n.grand.df, ncol=R )

#                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )

        #        print( dim(ind.mat) )
        #        print( dim(c.pred) )
        #        print( colSums( ind.mat ))
        #        cat("hit return...")
        #        readline()

#                cat("in bootstrap.passage.r.  hit return...")
#                readline()
#                 print(table(grand.df[grand.df$imputed.catch > 0,]$trapPositionID))
#                 cat(paste0('ind.mat - ',dim(ind.mat)[1],'\n'))
#                 cat(paste0('grand.df - ',dim(grand.df)[1],'\n'))
#                 cat(paste0('trap.ind - ',length(trap.ind),'\n'))
#                 cat(paste0('pred - ',dim(pred)[1],'\n'))
#                 cat(paste0('c.pred - ',dim(c.pred)[1],'\n'))

                c.pred[ind.mat] <- pred   # replaces imputed values with other realizations contained in pred.  This is nrow(grand.df) X R, or (n.batch.days*n.traps) X R
            }
        }

#        cat("...Catch BS complete")

        #pred <<- pred

        #   *=*=*=*=*=*=*=* Generate random realizations of EFFICIENCY

        ind <- which(trapID == names(eff.fits))
        if( length(ind) > 0 ){

            e.fit <- eff.fits[[ind]]
            e.X <- eff.X[[ind]]
            e.ind <- eff.ind.inside[[ind]]   # this is a 2 vector of first and last efficiency trials
            e.ind <- (e.ind[1] <= grand.df$batchDate) & (grand.df$batchDate <= e.ind[2]) & trap.ind   # this is an indicator for days inside the efficiency season
            e.dts <- eff.X.dates[[ind]] # Vector of dates inside efficiency season.  Used to line up with catches because catch seasons vary.

            if( !is.list(e.fit) | length(e.fit) == 0 ){
                #   No efficiency trials
                e.pred <- matrix( NA, nrow(c.pred), ncol(c.pred) )
            } else {

                # Variance matrix
                if( e.fit$df.residual == 0 ){
                    #    Only one efficiency trial at this trap
                    disp <- 1
                } else {
                    resids <- residuals(e.fit, type="pearson")

                    #qrds <- quantile( resids, p=c(.025, .975))
                    #toss.ind <- (resids < qrds[1]) | (qrds[2] < resids)

                    #   For Binomial overdispersion, there are not very many efficiency trials.
                    #   So, I do not want to toss the top 2.5% in magintude, like I did for the catches.
                    #   However, we need to ensure that there are not some very very large residuals.
                    #   Thus, any greater than a cut off will be eliminated.
                    toss.ind <- (abs(resids) > 8)

                    resids <- resids[!toss.ind]
                    disp <- sum( resids*resids ) / (e.fit$df.residual - sum(toss.ind))
                    if( disp < 1.0 ){
                        disp <- 1.0
                    }

#                    print( sum(toss.ind) )
#                    print( e.fit$df.residual - sum(toss.ind) )
#                    readline()
                }

                cat(paste("...Binomial over-dispersion in efficiency model for trap ", trapID, " = ", disp, "\n"))

#                print(c(dispersion=disp))
#                cat("in efficincy part of bootstrap_passage.r")
#                readline()


                sig <- disp * vcov( e.fit )   # vcov returns unscaled variance-covariance matrix.  scale by over dispersion.

#                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )

                # Coefficients
                beta <- coef( e.fit )

                # Generate random coefficients
                rbeta <- rmvnorm(n=R, mean=beta, sigma=sig, method="chol")  # R random realizations of the beta vector

                #cat(paste(beta))
#                setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )

                # Predict efficiency using random coefficients
                # When computed, pred is a matrix where each column is a random realization of model predictions
                # for missing catches. There are R columns (sets of predictions)
                pred <- (e.X %*% t(rbeta))
                pred <- 1 / (1 + exp(-pred))    # pred is nrow(e.X) X R


                # Use mean predicted efficiency for times outside first and last trials
                ind.mat <- matrix( trap.ind, nrow=n.grand.df, ncol=R )

                e.means <- matrix( colMeans( pred ), byrow=T, nrow=sum(trap.ind), ncol=R )


                #   This is complicated, but we have to line up the catch dates with the efficiency dates.  Because length of seasons vary, this is necessary.
                df.c <- data.frame(batchDate=format(grand.df$batchDate[trap.ind]), in.catch = TRUE, stringsAsFactors=FALSE )
                df.e <- data.frame(batchDate=format(e.dts), in.eff = TRUE, stringsAsFactors=FALSE )

                df.ce <- merge( df.c, df.e, all.x=TRUE )
                df.ec <- merge( df.e, df.c, all.x=TRUE )

                df.ec$in.catch[ is.na(df.ec$in.catch) ] <- FALSE
                df.ce$in.eff[ is.na(df.ce$in.eff) ] <- FALSE

                pred <- pred[ df.ec$in.catch, ]   # predictions that are in the catch data set

                #tmp.df.ce <<- df.ce
                #tmp.df.ec <<- df.ec

                e.means[ df.ce$in.eff ] <- pred  # predictions inside the season, on the right dates

                e.pred[ind.mat] <- e.means    # Assigns mean outside of eff.ind.inside[[trap]], and efficiency model inside season


            }
        }
        cat("...BS complete\n")

    }


#    print(c.pred[,1:2])
#    print(e.pred[,1:2])
#    print(dim(c.pred))
#    print(dim(e.pred))
#    cat("hit return...")
#    readline()

    assign("c.pred", c.pred, pos=.GlobalEnv)
    assign("e.pred", e.pred, pos=.GlobalEnv)



    #   *=*=*=*=*=*=*=* Estimate passage
    #         c.pred and e.pred are the same size, so just divide
    test <- ifelse(grand.df$imputed.catch > 0 & grand.df$imputed.catch < 1,grand.df$totalEstimatedCatch - grand.df$imputedCatch,0)
    c.pred <- apply(c.pred,2,function(x) x + test)
    c.pred <- c.pred / e.pred    # Re-use the c.pred matrix to save space






    #   Debugging
    assign("ce.pred", c.pred, pos=.GlobalEnv)

    #   ===== Now, average over traps
    #   At this point, c.pred is a (n.batch.day*n.trap) X R matrix, with each cell containing the passage estimated
    #   at a particular trap at the site for a particular batch day for a particular iteration of the bootstrap.
    #   Row dimension of list items corresponds to (batch days x trap), columns correspond to iterations.
    #   We now need to average the cells over the traps, and summarize by time.  Do this by calling F.summarize.passage on each column

    #   ===== Apply F.summarize to every column of pass
    f.sumize.pass <- function(p, s.by, bd){#}, imp.catch){
        #   Internal function to summarize catch by s.by
        df <- data.frame( batchDate=bd, passage=p, imputed.catch=1 )  # jason turns off 2/17/2016:  "we will not use inputed.catch.  Having F.summarize.passage average imputed catch wastes some time."
#         df <- data.frame( batchDate=bd, passage=p, imputed.catch=imp.catch )  # we WILL use inputed.catch.
#         rownames(df) <- NULL  # jason adds to make it look like the old df
        n <- F.summarize.passage( df, s.by )
        n$passage
    }
    pass <- apply( c.pred, 2, f.sumize.pass, s.by=sum.by, bd=grand.df$batchDate)#, imp.catch=grand.df$imputed.catch)
    pass <- matrix( unlist(pass), n.len, R )

#    setWinProgressBar( bootbar, getWinProgressBar(bootbar) + barinc )


    #   ===== An internal function to compute bias corrected bootstrap intervals
    f.bias.acc.ci <- function( x, alpha, x.orig ){
    #   Compute bias corrected bootstrap CI's
        p <- mean( x > x.orig, na.rm=TRUE)
        z.0 <- qnorm( 1 - p )
        z.alpha <- qnorm( 1 - (alpha/2))
        p.L <- pnorm( 2*z.0 - z.alpha )
        p.H <- pnorm( 2*z.0 + z.alpha )
        ci <- quantile( x[ !is.na(x) & (x < Inf) ], p=c(p.L, p.H) )
        ci
    }


    #   ===== Apply f.bias.bs.ci to every row of pass to get bootstrap intervals.
    ans <- apply( pass, 1, f.bias.acc.ci, alpha=(1-conf), x.orig=n.orig )
    ans <- as.data.frame(t(matrix( unlist(ans), 2, n.len )))

#    close( bootbar )

}


#   ---- Append lower and upper end points and return
names(ans) <- paste0( c("lower.", "upper."), conf*100 )
ans <- data.frame( n.orig, ans, stringsAsFactors=F )

#cat("In bootstrap_passage.r.  HIt return...")
#readline()

# # jason makes some bootstrap graphs.
# png(filename='C:/Users/jmitchell/Desktop/bs.png',res=200,width=40,height=48,units="in")
# par(mfrow=c(24,10))
# for(iter in 1:240){
#   d <- ans[iter,]
#
#   p <- mean( pass[iter,] > x.orig, na.rm=TRUE)
#
#   # bg color
#   rgb.palette <- colorRampPalette(c("red","pink","white"),space="rgb")
#   h <- rev(rgb.palette(101))
#
#   par( mar = c(2,1,2,1) )
#   hist(pass[iter,],breaks=dim(pass)[2]/10,col="white",border="white",main="")  # breaks dependent on sample size
#   par(new=TRUE)
#   rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=h[round(100*p,0) + 1])#round(100*d$pct.imputed.catch,0) + 1])
#   par(new=TRUE)
#   lines(x=rep(d$lower.95,20),y=seq(0,100,length.out=20),lwd=3,type="l",col="blue")
#   lines(x=rep(d$upper.95,20),y=seq(0,100,length.out=20),lwd=3,type="l",col="blue")
#   lines(x=rep(d$passage,20),y=seq(0,100,length.out=20),lwd=3,type="l",col="green")
#   par(new=TRUE)
#   hist(pass[iter,],breaks=dim(pass)[2]/10,col="white",main=paste0(d$s.by," - ",round(d$passage,0)," - (",round(d$lower.95,0),",",round(d$upper.95,0),") - ",round(d$pct.imputed.catch,2)," - p=",round(p,2)))  # breaks dependent on sample size
#
#   #rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=h[round(d$pct.imputed.catch,0)*100 + 1])
#
# }
# dev.off()
# par(mfrow=c(1,1))



ans

}
