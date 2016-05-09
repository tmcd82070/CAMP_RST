#' @title Estimate a catch regression model.
#'
#' @param catch.df A data frame containing all requested trapping data for a
#'   particular trapPosition.  See 'Details'.
#' @return Function F.catch.model serves two main purposes.  The first utilizes
#'   cubic splines to estimate total catch per trapping position over the date
#'   range provided.  The second utilizes spline results to impute estimates for
#'   periods when the trap was not functioning.
#'
#' @section:  Cubic splines: Prior to model fitting, trapVisitIDs are identified
#'   as possible night-time trapping instances via examination of StartTimes and
#'   EndTimes along with calculated sunrises and sunsets.  As of the 1.5 release,
#'   all sunrises and sunsets are set to 7:00 AM and 7:00 PM local time.  Given
#'   daylight and trapping times, all trapVisitIDs with some portion of trapping
#'   taking place at night are identified, along with the overall proportion of a
#'   trapping instance that takes place at night.
#'
#'   Following computation of night-time variables, a null Poisson model of
#'   total catch is fit, via a long link, with an indicator variable for
#'   night-time trapping included if at least 90% of all the trapping instances
#'   trapped at night, as defined above;  otherwise, only an intercept term is
#'   included.  These models include log-transformed trap sampling time, in
#'   hours, as an offset.  All models fit via glm.
#'
#'   After the initial intercept-only model, increasingly complex models are
#'   fit.  More complex models require at least ten caught fish, over any
#'   combination of trapping instances.  Prior to a cubic spline, each of a
#'   polynomial linear, quadratic, and cubic is fit to the data.  (Note that a
#'   cubic polynomial model is a cubic spline with no internal knots.) Each
#'   incorporates a night variable if at least 90% of the trapping instances
#'   fished at night.  Models only consider the next complex model if two
#'   conditions are met.  First ,the difference in the Akaike Information
#'   Criterion (AIC), when comparing the current model to the previous model,
#'   must be greater than two, after rounding both models to four decimals.
#'   Second, the number of unique trapping instances, divided by 15, rounded
#'   down, must be greater than or equal to the model's degrees of freedom,
#'   excluding an intercept.  This means that for a linear model, at least 15
#'   data points are required.  Similar logic requires 30 for a quadratic.  The
#'   table below summarizes the relationship between the number of data points (or trapping instances) and maximal
#'   model possible.
#'
#'  \tabular{cc}{
#'    dof \tab model type                               \tab n trapping instances \tab model-form
#'    0   \tab intercept-only                           \tab 15
#'    1   \tab linear                                   \tab 30
#'    2   \tab quadratic                                \tab 45
#'    3   \tab cubic                                    \tab 60
#'    4   \tab cubic spline with one internal knot      \tab 75
#'    5   \tab cubis spline with two internal knots     \tab 90
#'    ... \tab ...                                      \tab ...
#'    k   \tab cubic spline with (k - 3) internal knots \tab 15*(k - 1)           \tab \beta_0 + \beta_1 + \beta_2
#'  }
#'
#'    When required, cubic splines utilize function bs to obtain an appropriate
#'    basis for use in model fitting. The model degrees of freedom determines
#'    the number of internal knots utilized, via parameter df.  Individual cubic splines include
#'    no intercept, although all cubic-spline Poisson catch models
#'    utilizes an overall intercept, so as to center models along the outcome axis.
#'
#' @section:  Imputation:
#'
#'
#' @seealso blah blah blah
#'
#' @aliases catch.model catchmodel catchModel
#'
#'
#' @examples
#' F.catch.model( catch.df ) # maybe later we'll make this happen.
#' @export
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

# catch.df <- df2      # from est_catch_trapN.r
# catch.df <- df3      # from est_catch.r

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


modelDF <- catch.df[,c('n.tot','log.sampleLengthHrs','night')]

#   Fit null model.  The gap catches are NA here, so they are dropped from the fit.  Later, they are replaced.
nightVec <- catch.df[!is.na(catch.df$n.tot),]$night    # jason add 3/16/2016 - doing gaps in fishing leads to small timeframes -- can have no variability in night var -- all 1!  deal with this possibility.
if( p.night < 0.9 & (length(nightVec) != sum(nightVec)) ){
  
    # 3/22/2016 -- turn off night for now, until after it can be fixed.
    #fit <- glm( n.tot ~ offset(log.sampleLengthHrs)  + night, family=poisson, data=catch.df )

    fit <- glm( n.tot ~ offset(log.sampleLengthHrs) , family=poisson, data=catch.df )
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
    nGoodData <- length(!is.na(catch.df$n.tot))    # get the number of good data points we have to work with, for this trapPosition

    repeat{

        valid <- (cur.df <= floor(nGoodData / knotMesh))

        # jason - 3/15/2016 -- we add a condition that checks to make sure at least 15 non-NA trapping EndTimes are present in the data.
        # now that we've dealt with gaps in fishing, we can have very short time periods for which we require a spline.  we need to
        # make sure that we don't inadvertently over-fit the data.  keep in mind this is per trapPositionID.
        if( cur.df == 1 & (valid == TRUE) ){
            j <- as.numeric( format(catch.df$EndTime, "%j"))
            bs.sEnd <- matrix(j,ncol=1)
        } else if( cur.df == 2 & (valid == TRUE) ){
            j <- as.numeric( format(catch.df$EndTime, "%j"))
            bs.sEnd <- cbind(Lin=j, Quad=j*j)
        } else if( cur.df > 2 & (valid == TRUE) ){
            bs.sEnd <- bs( catch.df$EndTime, df=cur.df )      # first time through, this is a cubic with no internal knots
        }

        if( p.night < 0.9 & (length(nightVec) != sum(nightVec)) ){
            # 3/22/2016 -- we get rid of night for now, until it can be fixed later.
            #cur.fit <- tryCatch(glm( n.tot ~ offset(log.sampleLengthHrs) + bs.sEnd + night, family=poisson, data=catch.df ),error=function(e) e ) 
            cur.fit <- tryCatch(glm( n.tot ~ offset(log.sampleLengthHrs) + bs.sEnd, family=poisson, data=catch.df ),error=function(e) e )
        } else {
            cur.fit <- tryCatch(glm( n.tot ~ offset(log.sampleLengthHrs) + bs.sEnd, family=poisson, data=catch.df ),error=function(e) e )
        }

        if(class(cur.fit)[1] == 'simpleError'){
          cur.AIC <- NA
          cat(paste0("Model fell apart;  tryCatch caught the output.  You may wish to investigate for trap ",catch.df$trapPositionID,".\n"))
        } else {
          cur.AIC <- AIC( cur.fit )
          cat(paste("df= ", cur.df, ", conv= ", cur.fit$converged, " bound= ", cur.fit$boundary, " AIC= ", round(cur.AIC, 4), "\n"))
        }

        if( is.na(cur.AIC) ){
          break
        } else if( !cur.fit$converged | cur.fit$boundary | cur.df > 15 | cur.AIC > (fit.AIC - 2) ){
          break
        } else {
            fit <- cur.fit
            fit.AIC <- cur.AIC
            bs.sampleEnd <- bs.sEnd
            cur.df <- cur.df + 1
        }

    }
}
# toPlot <- catch.df[,c('EndTime','n.tot','SampleMinutes','log.sampleLengthHrs')]
# toPlot$catchRate <- toPlot$n.tot/as.numeric(toPlot$SampleMinutes/60)
#
#
# plot( toPlot$EndTime, toPlot$n.tot/as.numeric(toPlot$SampleMinutes/60), xlab="Date", ylab="Catch per hour",main=paste0(trap," - ",run.name) )
# pred <- predict( fit, newdata=seq(min(catch.df$EndTime),max(catch.df$EndTime),length.out=30), type="response" )#
# print(cbind(pred, toPlot$n.tot, toPlot$log.sampleLengthHrs, toPlot$EndTime, catch.df$night)[1:20,])
# lines(toPlot$EndTime, pred, col="red" )
# #



























#' @section:  Imputation:  The trap-specific imputation procedure utilizes the final catch spline result obtained
#' via the process described above.  Specifically, it sweeps through all temporally sorted rows of the catch dataframe for the
#' trap of interest, replacing all instances of "Not fishing," which spline-estimated fish.  All estimates loop
#' over gaps one at a time, predicting catch for a maximum of up to 24 hours.  All "Not fishing" gaps estimate on
#' hours, in tandem with the temporal unit utilized in Poisson model offsets.  For example, for a 56-hour gap, predictions
#' occur for each 24-hour period, with catch estimated proportionally for any "leftover" preceding and antecedent times.  See examples.
#'
#' Catch is not estimated for individual "Not fishing" episodes of duration less than two hours.  In these cases, the sampleMinutes
#' associated with
#'
#'
#'








print(summary(fit, disp=sum(residuals(fit, type="pearson")^2)/fit$df.residual))



#   Done fitting model

# ===============================================================================================================================

#   Loop over gaps one at a time, predicting catch for maximum of 24 hour periods.  for 56 hour gap, you will
#   predict for 2 24-hr periods and 1 6-hr period.

#est.days <- seq(start.day, end.day, by=24*60*60 )
#est.days <- format( est.days, "%Y-%m-%d" )

# 3/22/2016 -- we turn off the night variable.  so, force night.in as zero.
night.in <- 0#"night" %in% attr(fit$terms, "term.labels")

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

    #if( i == 3) break    # debugging.

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
        i.gapLens <- c(rep(24, floor(catch.df$SampleMinutes[i]/ (24*60))), (catch.df$SampleMinutes[i]/60) %% 24 )   # length of SampleMinutes, in hours

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
<<<<<<< Updated upstream

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
        new <- catch.df[1:ng,]         # initialize - do it this way to get classes and factor levels -- note that if we add a lot of not fishing days, these become NA rows.
                                       # observe that we don't update TrapStatus for example, so only the vars we use later are made to be pretty.
        new$n.tot <- pred              # put imputed with plus counts.
        new$n.Unassd <- 0              # we've already accounted for these?
        new$n.Orig <- NA               # jason add 4/17/2015 ... we want imputed values to go into tot, and not mess with unassigned numbers
        new$n.Orig2 <- 0               # jason add 1/14/2016 ... as we rebuild our df, make sure previous values don't pollute this variable
        new$halfConeAssignedCatch <- 0     # jason add 1/14/2016 ... as we rebuild our df, make sure previous values don't pollute this variable
        new$halfConeUnassignedCatch <- 0   # jason add 1/14/2016 ... as we rebuild our df, make sure previous values don't pollute this variable
        new$assignedCatch <- 0             # jason add 1/14/2016 ... as we rebuild our df, make sure previous values don't pollute this variable
        new$unassignedCatch <- 0           # jason add 1/14/2016 ... as we rebuild our df, make sure previous values don't pollute this variable
        new$modAssignedCatch <- 0          # jason add 1/14/2016 ... as we rebuild our df, make sure previous values don't pollute this variable
        new$modUnassignedCatch <- 0        # jason add 1/14/2016 ... as we rebuild our df, make sure previous values don't pollute this variable
        new$SampleMinutes   <- i.gapLens * 60
=======
      

        #   ---- Get the catch estimate, based on the model 
        #   ---- parameters and length of time.
        pred <- (new.dat %*% coef(fit)) + log(i.gapLens)
        pred <- exp(pred)
  
        #   ---- Put things we need into a blank data frame 
        #   ---- suitable for inserting into catch.df.
        
        #   ---- Initialize - do it this way to get classes and factor levels.
        #   ---- See that if we add a lot of 'Not fishing' days, these become NA rows.
        #   ---- Also, e.g., we don't update TrapStatus, so only the vars we use later 
        #   ---- are made to be pretty.
        new <- catch.df[1:ng,]        
        
        #   ---- Put imputed with plus counts.
        new$n.tot <- pred              
        new$n.Unassd <- 0 
        
        #   ---- we want imputed values to go into tot, and not mess with unassigned.  
        new$n.Orig <- NA
        
        #   ---- As we rebuild, make sure previous values don't pollute these variables.
        new$n.Orig2 <- 0               
        new$halfConeAssignedCatch <- 0
        new$halfConeUnassignedCatch <- 0
        new$assignedCatch <- 0
        new$unassignedCatch <- 0
        new$modAssignedCatch <- 0
        new$modUnassignedCatch <- 0
        new$SampleMinutes <- i.gapLens * 60
>>>>>>> Stashed changes
        new$EndTime <- sEnd
        new$StartTime <- sStart
        new$gamEstimated <- TRUE
        new$siteID <- catch.df$siteID[i]
<<<<<<< Updated upstream
        #new$mean.fl <- NA
        #new$sd.fl <- NA
        new$trapPositionID <- catch.df$trapPositionID[i]
        new <- F.assign.batch.date( new )

       #print(new)                                             # jason turn on
       #print(c(nrow.catch.df=nrow(catch.df), i=i, ng=ng))     # jason turn on

       # pull out the actual imputed numbers only for checking daily counts in baseTable.
       jason.new <- rbind(jason.new,new)                                      #

       #   Insert new data frame into catch.df
       # degenerate case -- trap 42080, rbdd, 9/23/1998, fall, chucking zeros, results in 2 not fishing cases, and 2 fishing cases.
       # somehow, the code immediately above is throwing in an extra not-fishing.  maybe because the first line is a not
       # fishing?  not sure why.  this is the problem -- expand rbind here to allow for not fishing when i = 1.
#        if(i == 1){                                                    # we only have this true if first row of catch.df is Not fishing
#          catch.df <- rbind( new, catch.df[(i+1):nrow(catch.df),] )    # jason allows this condition if i = 1 is Not fishing, 1/22/2016.
#        } else {
        catch.df <- rbind( catch.df[1:(i-1),], new, catch.df[(i+1):nrow(catch.df),] )
       # }
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
=======
        new$trapPositionID <- catch.df$trapPositionID[i]
        new <- F.assign.batch.date( new )
  
        #   ---- Pull out imputed numbers for checking daily counts in baseTable.
        jason.new <- rbind(jason.new,new)
  
        #   ---- Insert new data frame into catch.df.
        catch.df <- rbind( catch.df[1:(i-1),], new, catch.df[(i+1):nrow(catch.df),] )
  
        #   ---- Save model matrix used to make predictions for use in bootstrapping. 
        all.new.dat <- rbind( all.new.dat, new.dat )
        all.gaplens <- c(all.gaplens, i.gapLens)
        
        #   ---- Save batch dates because it is possible for two 'Not fishing'
        #   ---- estimates to fall on the same batch date - must collapse later.
        all.bdates <- c(all.bdates, new$batchDate)  
  
        i <- i + ng + 1
      }
    #   ---- No brace here:  don't need it for the if break.  
  }
>>>>>>> Stashed changes



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

if( is.null( jason.new )){
  jason.new <- data.frame(batchDate=as.POSIXlt(character()),trapVisitID=character(),trapPositionID=character(),n.tot=integer(),stringsAsFactors=FALSE)
}

#print(all.bdates)
#readline()

jason.new <- jason.new[,c('batchDate','trapVisitID','trapPositionID','n.tot')]   # reduce scope to only those variables needed
list(df2=catch.df, fit=fit, X.for.missings=all.new.dat, gaps=all.gaplens, batchDate.for.missings=all.bdates, true.imp=jason.new)

}
