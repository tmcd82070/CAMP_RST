F.catch.model <- function( catch.df ){
#
#   Compute and estimate of catch for all days that are missing in the input data set.
#   i.e., "imput" a value for catch when it is missing
#
#   input:
#   obs.catch.df = data frame from F.get.catch.data
#
#   Output:
#   a data frame with extra lines in it.  One extra line for each 24 hour period in the 
#   gaps that were bigger than max.ok.gap. If gap = 56 hours, there will be 3 extra lines. 
#   Two lines for the first 2 24-hr periods (48 hrs total) plus one line for the remaining 6-hr 
#   period.  StartTime and EndTime for each of the new lines are defined so that no gap appears.  Variable
#   'gamEstimated' is true for these new lines. Batch date is assigned based on EndTime, as usual. 
#   This applies for all days between start.day and end.day.

# catch.df <- df2

#   Sort the data frame properly, by trapPosition and date of visit
#   This puts the gaps in their correct locations 
catch.df <- catch.df[ order(catch.df$trapPositionID, catch.df$EndTime), ]


#   Compute the "night" variables.
time.zone <- get("time.zone", env=.GlobalEnv )
sunset  <- as.POSIXct( paste(format(catch.df$StartTime, "%Y-%m-%d"), "19:00:00"), format="%Y-%m-%d %H:%M:%S", tzone=time.zone )
sunrise <- as.POSIXct( paste(format(catch.df$EndTime, "%Y-%m-%d"), "07:00:00"), format="%Y-%m-%d %H:%M:%S", tzone=time.zone )

catch.df$night <- 0
catch.df$pct.night <- 0


ind <- (catch.df$StartTime <= sunset) & (sunrise <= catch.df$EndTime)
catch.df$night[ind] <- 1
catch.df$pct.night[ind] <- 1

ind <- (catch.df$StartTime <= sunset) & (sunrise > catch.df$EndTime)
catch.df$night[ind] <- 1
catch.df$pct.night[ind] <- (as.numeric(catch.df$EndTime[ind]) - as.numeric(sunset[ind])) / (as.numeric(sunrise[ind]) - as.numeric(sunset[ind]))

ind <- (catch.df$sampleStart > sunset) & (sunrise <= catch.df$EndTime)
catch.df$night[ind] <- 1
catch.df$pct.night[ind] <- (as.numeric(sunrise[ind]) - as.numeric(catch.df$StartTime[ind])) / (as.numeric(sunrise[ind]) - as.numeric(sunset[ind]))


#   Fit a rate model. 

library(splines)

#catch.df <- catch.df[ catch.df$TrapStatus == "Fishing", ]

catch.df$log.sampleLengthHrs <- log(as.numeric( catch.df$SampleMinutes/60 ))
p.night <- sum(catch.df$night) / nrow(catch.df)


#   Fit null model.  The gap catches are NA here, so they are dropped from the fit.  Later, they are replaced.
if( p.night < 0.9 ){
    fit <- glm( n.tot ~ offset(log.sampleLengthHrs)  + night, family=poisson, data=catch.df )
} else {
    fit <- glm( n.tot ~ offset(log.sampleLengthHrs) , family=poisson, data=catch.df )
}
fit.AIC <- AIC(fit)

#tmp.cc <<- catch.df   # print( tmp.cc[,c(1,3,4,5,6,7,10,18)] )

#   Fit glm model, increasing df, until the model goes bad
cat(paste("Number of non-zero catches : ", sum(!is.na(catch.df$n.tot) & (catch.df$n.tot > 0)), "\n"))
cat("Catch model fitting:\n")
cat(paste("df= ", 0, ", conv= ", fit$converged, " bound= ", fit$boundary, " AIC= ", round(fit.AIC, 4), "\n"))

if( sum(!is.na(catch.df$n.tot) & (catch.df$n.tot > 0)) > 10 ){
    cur.df <- 1  # this is df in smoother part, excluding intercept.  1=linear, 2=quadratic, 3=bspline w/ 1 internal knot, 4=bspline w/ 2 internal knots, etc.
    repeat{
     
        if( cur.df == 1 ){
            j <- as.numeric( format(catch.df$EndTime, "%j"))
            bs.sEnd <- matrix(j,ncol=1)
        } else if( cur.df == 2 ){
            j <- as.numeric( format(catch.df$EndTime, "%j"))
            bs.sEnd <- cbind(Lin=j, Quad=j*j)
        } else {
            bs.sEnd <- bs( catch.df$EndTime, df=cur.df )
        }
        
        if( p.night < 0.9 ){
            cur.fit <- glm( n.tot ~ offset(log.sampleLengthHrs) + bs.sEnd + night, family=poisson, data=catch.df )
        } else {
            cur.fit <- glm( n.tot ~ offset(log.sampleLengthHrs) + bs.sEnd, family=poisson, data=catch.df )
        }
        cur.AIC <- AIC( cur.fit )
    
        cat(paste("df= ", cur.df, ", conv= ", cur.fit$converged, " bound= ", cur.fit$boundary, " AIC= ", round(cur.AIC, 4), "\n"))
    
        
        if( !cur.fit$converged | cur.fit$boundary | cur.df > 15 | cur.AIC > (fit.AIC - 2) ){
            break
        } else {
            fit <- cur.fit
            fit.AIC <- cur.AIC
            bs.sampleEnd <- bs.sEnd
            cur.df <- cur.df + 1
        }
    
    }
}
#plot( catch.df$sampleEnd, catch.df$n.tot/as.numeric(catch.df$sampleLengthHrs), xlab="Date", ylab="Catch per hour" )
#pred <- predict( fit, newdata=tmp, type="response" )
#print(cbind(pred, catch.df$n.tot, catch.df$log.sampleLengthHrs, catch.df$sampleEnd, catch.df$night)[1:20,])
#lines(catch.df$sampleEnd, pred, col="red" )
#
print(summary(fit, disp=sum(residuals(fit, type="pearson")^2)/fit$df.residual))



#   Done fitting model

# ===============================================================================================================================

#   Loop over gaps one at a time, predicting catch for maximum of 24 hour periods.  for 56 hour gap, you will 
#   predict for 2 24-hr periods and 1 6-hr period.

#est.days <- seq(start.day, end.day, by=24*60*60 )
#est.days <- format( est.days, "%Y-%m-%d" )

night.in <- "night" %in% attr(fit$terms, "term.labels")

catch.df$gamEstimated <- FALSE 

sunset <- sunset[1]   # do something different here (pull in actual sunrise/sunset) if we have actual times.  Otherwise, its constant.
sunrise <- sunrise[1]

degree <- length(coef(fit)) - 1 - night.in  # number of columns in smoother part only.  Excludes intercept and potential night covars
if( degree <= 2 ){
    nots <- -1
    b.knots <- -1
} else {
    nots <- attr(bs.sampleEnd,"knots")
    b.knots <- attr(bs.sampleEnd, "Boundary.knots")
}

i <- 1
all.new.dat <- NULL
all.gaplens <- NULL
all.bdates <- NULL
jason.new <- NULL

#tmp.cc <<- catch.df
#cat("---------------- in catch_model.r\n")

repeat{

   
    if( i >= nrow(catch.df) ) break   # Don't do last line because no need to check.  After end, no more gaps by def'n.
    
    #print(catch.df[i,])
        
    if( catch.df$TrapStatus[i] == "Fishing" ){
        i <- i + 1
    } else if(catch.df$SampleMinutes[i] <= (60*max.ok.gap)){
        #   eliminate the small gaps.  Note, i will never be 1 if we are here.  But put the if in just in case.
        if( i > 1 ){
            catch.df$EndTime[i-1] <- catch.df$EndTime[i]
            catch.df$batchDate[i-1] <- catch.df$batchDate[i]
            catch.df <- catch.df[-i,]
        }
    } else {
    
        #   We have a missing value = a period of time >max.ok.gap with no estimate of fish 
        
        #   i.gapLens is length of all intervals that we want to predict for.  They all sum to total of gap.
        i.gapLens <- c(rep(24, floor(catch.df$SampleMinutes[i]/ (24*60))), (catch.df$SampleMinutes[i]/60) %% 24 )
        
        #   Make sure last interval is > max.ok.gap
        ng <- length(i.gapLens)
        if( i.gapLens[ng] <= max.ok.gap ){
            i.gapLens[ ng - 1 ] <- i.gapLens[ng-1] + i.gapLens[ng]  # add last small gap to previous interval
            i.gapLens <- i.gapLens[-ng]
        }
        ng <- length(i.gapLens)


        sEnd <- catch.df$StartTime[i] + cumsum(i.gapLens * 60*60)
        class(sEnd) <- class(catch.df$StartTime)
        attr(sEnd, "tzone") <- time.zone   

        sStart <- catch.df$StartTime[i] + cumsum( c(0,i.gapLens[-ng]) * 60*60 )
        class(sStart) <- class(sStart)
        attr(sStart, "tzone") <- time.zone  
        
#        print(cbind(1:nrow(catch.df),catch.df)[(i-2):(i+2),c(1,1+c(1,3,4,5,6,7,10,18))])
#        print( c(i=i, ng=ng))
#        print( cbind( i.gapLens, sEnd, sStart ))
#        print(c(degree=degree))
#        cat("in catch_model.r (hit return)...")
#        readline()
        
        #   The smoother
        if( degree <= 2 ){
            bs.sEnd <- -1
        } else {
            bs.sEnd <- bs( sEnd, knots=nots, Boundary.knots=b.knots )
            dimnames(bs.sEnd)[[2]] <- paste("bs.sampleEnd", dimnames(bs.sEnd)[[2]], sep="")
        }

        #   Night, if it is in the model
        if( night.in ){
            sNight <- 0
            sPct.night <- 0
    
            ind <- (sStart <= sunset) & (sunrise <= sEnd)
            sNight[ind] <- 1
            sPct.night[ind] <- 1
            
            ind <- (sStart <= sunset) & (sunrise > sEnd)
            sNight[ind] <- 1
            sPct.night[ind] <- (as.numeric(sEnd[ind]) - as.numeric(sunset[ind])) / (as.numeric(sunrise[ind]) - as.numeric(sunset[ind]))
            
            ind <- (sStart > sunset) & (sunrise <= sEnd)
            sNight[ind] <- 1
            sPct.night[ind] <- (as.numeric(sunrise[ind]) - as.numeric(sStart[ind])) / (as.numeric(sunrise[ind]) - as.numeric(sunset[ind]))
    
            if( degree == 0 ){
                #   Mean model
                new.dat <- cbind( Int=matrix( 1, length(sEnd), 1 ), night=sNight )
            } else if(degree == 1){
                #   Linear model
                j <- as.numeric( format(sEnd, "%j") )
                new.dat <- cbind( Int=rep(1, length(sEnd)), Lin=j, night=sNight )
            } else if(degree == 2){
                #   Quadratic model
                j <- as.numeric( format(sEnd, "%j") )
                new.dat <- cbind( Int=rep(1, length(sEnd)), Lin=j, Quad=j*j, night=sNight )            
            } else {
                new.dat <- cbind( 1, bs.sEnd, night=sNight )
            }

        } else {
            if( degree == 0 ){
                #   Mean model
                new.dat <- matrix( 1, length(sEnd), 1 )
            } else if(degree == 1){
                #   Linear model
                j <- as.numeric( format(sEnd, "%j") )
                new.dat <- cbind( rep(1, length(sEnd)), j )
            } else if(degree == 2){
                #   Quadratic model
                j <- as.numeric( format(sEnd, "%j") )
                new.dat <- cbind( rep(1, length(sEnd)), j, j*j )            
            } else {
                new.dat <- cbind( 1, bs.sEnd )
            }
        }

#        cat("---------------- in catch_model.r\n")
#        print(new.dat)
#        print(i.gapLens)
#        print(length(i.gapLens))
#        print(coef(fit))
#        print(c(degree=degree))
#        cat("in catch_model.r (hit return)...")
#        readline()        

        pred <- (new.dat %*% coef(fit)) + log(i.gapLens)
        pred <- exp(pred)
        
        #   Put things we need into a blank data frame suitable for inserting into catch.df
        new <- catch.df[1:ng,]   # initialize - do it this way to get classes and factor levels
        new$n.tot <- pred      # put imputed with plus counts.
        new$n.Unassd <- 0      # we've already accounted for these?
        new$n.Orig <- NA       # jason add 4/17/2015 ... we want imputed values to go into tot, and not mess with unassigned numbers
        new$SampleMinutes   <- i.gapLens * 60
        new$EndTime <- sEnd
        new$StartTime <- sStart
        new$gamEstimated <- TRUE
        new$siteID <- catch.df$siteID[i]
        new$mean.fl <- NA
        new$sd.fl <- NA
        new$trapPositionID <- catch.df$trapPositionID[i]
        new <- F.assign.batch.date( new )     
        
       print(new)                                             # jason turn on
       print(c(nrow.catch.df=nrow(catch.df), i=i, ng=ng))     # jason turn on

       # pull out the actual imputed numbers only for checking daily counts in baseTable.
       jason.new <- rbind(jason.new,new)                                      # 
        
        #   Insert new data frame into catch.df
        catch.df <- rbind( catch.df[1:(i-1),], new, catch.df[(i+1):nrow(catch.df),] )

#        print(c(nrow.catch.df=nrow(catch.df)))
     
        #   Save model matrix used to make predictions for use in bootstrapping
        all.new.dat <- rbind( all.new.dat, new.dat )
        all.gaplens <- c(all.gaplens, i.gapLens)
        all.bdates <- c(all.bdates, new$batchDate)  # we must save batch dates because it is possible for two gap estimates to fall on the same batch date
        
#        print(cbind(1:nrow(catch.df),catch.df)[(i-2):(i+ng+2),c(1,1+c(1,3,4,5,6,7,10,18))])
#        cat("in catch_model (bottom of loop): Hit return to continue...")        

        i <- i + ng + 1
        

#        print(c(new.i=i))
#        cat("in catch_model (bottom of loop): Hit return to continue...")
#        readline()
        
    }


            
}


#   If there are no gaps, all.new.dat, all.gaplens, and all.bdates are NULL. Turn these into NA so that 
#   storing them in a list in function est_catch works.  (assigning NULL to list item collapses the list)
if( is.null( all.new.dat )){
    all.new.dat <- NA
}
if( is.null( all.gaplens )){
    all.gaplens <- NA
}

if( is.null( all.bdates )){
    all.bdates <- NA
} else {
    class(all.bdates) <- class(sEnd)
    attr(all.bdates, "tzone") <- time.zone   
}

#print(all.bdates)
#readline()

jason.new <- jason.new[,c('batchDate','trapVisitID','trapPositionID','n.tot')]   # reduce scope to only those variables needed
list(df2=catch.df, fit=fit, X.for.missings=all.new.dat, gaps=all.gaplens, batchDate.for.missings=all.bdates, true.imp=jason.new)

}
