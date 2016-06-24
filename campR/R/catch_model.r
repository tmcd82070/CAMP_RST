#' @export
#' 
#' @title F.catch.model
#'   
#' @description Compute and estimate catch for all days missing in the input
#'   data set;  i.e., impute a value for catch when missing.
#'   
#' @param catch.df A data frame containing all trapping data for all  
#'   \code{trapPositionID}s of interest. 
#'   
#' @return A data frame with extra lines in it, when compared to input data 
#'   frame \code{catch.df}, with extra lines due to "Not fishing" periods.
#'   
#' @details Function \code{F.catch.model} serves two main purposes.  The first 
#'   utilizes cubic splines to fit Poisson generalized linear models to observed
#'   catch for each trapping position over the date range provided.  The second 
#'   utilizes spline results to impute estimates for periods whenever a trap was 
#'   not functioning.
#'   
#' @section  Cubic splines: First, given a trap, function \code{glm} fits a null
#'   Poisson model of total catch with a log link.  A null model is synonymous
#'   with an intercept-only model.  Log-transformed trap sampling time, in
#'   hours, serves as an offset.  Quality of fit is measured via use of Akaiake
#'   Information Criterion (AIC).
#'   
#'   After the initial intercept-only model, increasingly complex Poisson 
#'   log-link models are fit via \code{glm}. More complex models require at 
#'   least ten unique \code{trapVisitID} fishing instances to be considered. 
#'   Following the initial intercept-only model, linear, quadratic, and cubic
#'   polynomial models are sequentially fit to the data. Assuming the data
#'   support it (see below), cubic splines are fit following the rejection of a
#'   cubic polynomial model. Note that a cubic polynomial model is a cubic
#'   spline with no internal knots.
#'   
#'   Models only consider the next complex model if four conditions are met. 
#'   First, the difference in the Akaike Information Criterion (AIC), when 
#'   comparing the current model to the previous model, must be greater than 
#'   two, after rounding both models to four decimals. 
#'   
#'   Second, the number of unique trapping instances, divided by 15, rounded 
#'   down, must be greater than or equal to the model's degrees of freedom, 
#'   excluding an intercept. This means that for a linear model, which has one 
#'   corrected degree of freedom, requires at least 15 data points. Similar 
#'   logic requires 30 unique trapping instances for a quadratic, and so on. 
#'   Global variable \code{knotMesh} in function \code{GlobalVars} sets the
#'   number of unique trapping instances required for consideration of a more
#'   complex model.
#'   
#'   Third, resulting parameter estimates must not be on the boundary of the
#'   attainable values.  Due to the log-linked models utilized here, this means
#'   that parameter estimates must not be positive or negative infinite.  
#'   
#'   Finally, models can at most incorporate up to at most 16 degrees of freedom.  This
#'   means all cubic splines with 270 or more unique trapping instances can top
#'   off with at most 13 knots, and so 14 piecewise cubic polynomials. 
#'   
#'   The table below summarizes the relationship between the number of data
#'   points, i.e., unique trapping instances, and maximal model possible.
#'   Here, "DF" represents "Degrees of Freedom."  Note that all polynomial 
#'   pieces must be of the same degree.  Thus, give a particular catch time
#'   series, it is not possible to fit an Intercept-only model to the first
#'   half, say, and a Quadratic to the second.  Both pieces must either be 
#'   Intercept-only, Quadratic, or perhaps a different polynomial form.  
#'
#'  \tabular{ccc}{
#'    DF  \tab Maximal Model Type                       \tab \eqn{N} Trapping Instances \cr
#'    0   \tab Intercept-only                           \tab \eqn{1 \le N \le 14}       \cr
#'    1   \tab Linear                                   \tab \eqn{15 \le N \le 29}      \cr
#'    2   \tab Quadratic                                \tab \eqn{30 \le N \le 44}      \cr
#'    3   \tab Cubic                                    \tab \eqn{45 \le N \le 59}      \cr
#'    4   \tab Cubic Spline with One Internal Knot      \tab \eqn{60 \le N \le 74}      \cr
#'    5   \tab Cubic Spline with Two Internal Knots     \tab \eqn{75 \le N \le 89}      \cr
#'    ... \tab ...                                      \tab ... \cr
#'    k   \tab Cubic Spline with (k - 3) Internal Knots \tab \eqn{15*k \le N \le 15*(k + 1) - 1}    \cr
#'    ... \tab ...                                      \tab ... \cr
#'    16  \tab Cubic Spline with 13 Internal Knots      \tab \eqn{240 \le N }    \cr
#'  }
#'    
#' Models with at least 60 unique trapping instances incorporate the possibility
#' of a B-spline basis matrix via function \code{bs}. This means that piecewise
#' polynomials are utilized to fit observed trends, with one piece covering a
#' particular subset in the date range covered by trapping.  The points covered
#' by one polynomial piece correspond to quantiles in the temporal range.
#' 
#' Each piecewise polynomial is at most a cubic polynomial such that the end 
#' point of one piece connects with the start point of the next. Additionally, 
#' both first- and second-order derivatives are equal;  thus, resulting splines,
#' which may be composed of several individual polynomial pieces, appear smooth
#' over their entire sample range with respect to their local slope (first
#' derivative condition) and their local convexity (second derivative
#' condition).
#' 
#' Parameter \code{df}, or the model degrees of freedom in \code{bs}, determines
#' the number of internal knots utilized.  The value of \code{df} corresponds to
#' the values in the Table above.  Function \code{bs} makes no consideration of 
#' model intercept;  thus all \code{glm}-fit Poisson models utilize an 
#' additional overall intercept.  This serves to center models along the outcome
#' axis.
#' 
#' @section  Imputation:  
#' The trap-specific imputation procedure utilizes the final catch spline result
#' obtained via the process described above. Specifically, it sweeps through all
#' temporally sorted rows of the catch dataframe for the trap of interest,
#' replacing all instances of "Not fishing," with spline-estimated fish.  All
#' estimates loop over periods of "Not fishing" one at a time, predicting catch
#' for a maximum of up to 24 hours.  All 'Not fishing' periods estimate on
#' hours, in tandem with the temporal unit utilized in Poisson model offsets.
#' 
#' One extra line is inserted into \code{catch.df} for each unique 24-hour "Not 
#' fishing" period larger than global variable \code{max.ok.gap}. Currently, 
#' \code{max.ok.gap} is set at 2 hours in function \code{GlobalVars}.  Thus, 
#' catch is not estimated for individual "Not fishing" episodes of duration less
#' than two hours.  In these cases, the most immediately preceding valid fishing
#' period subsumes the \code{sampleMinutes} associated with these small time
#' frames.
#' 
#' For example, for a 56-hour period of "Not fishing," predictions occur for 
#' each unique 24-hour period, with catch estimated proportionally for any 
#' "leftover" preceding and antecedent times.  Assuming that a "Not fishing" 
#' period coincides with the start of a day, three resulting rows would be 
#' inserted into \code{catch.df} -- two for the first two 24-hour periods, and a
#' third for the leftover 8-hour period. The leftover 8-hour period would
#' necessarily impute one-third the number of fish specified by that trap's
#' catch model for that day.  This number would then be added to the observed
#' catch for that day, obtained over the remaining valid fishing of 16 hours. 
#' The sum of the imputed and observed catch comprises the total catch for that
#' day. 
#' 
#' The \code{StartTime} and \code{EndTime} variables for each of the new lines 
#' inserted into \code{catch.df} are defined so that no "Not fishing" periods 
#' remain.  For these lines, variable \code{gamEstimated} is set to \code{TRUE}.
#' Assignment of variable \code{batchDate} is based on \code{EndTime}, as usual.
#' This methodology applies for all days between the time period requested via 
#' \code{min.date} and \code{max.date} in associated passage functions, for each
#' unique trap \code{trapPositionID} in \code{catch.df}.
#'
#' If \code{catch.df} contains no periods of "Not fishing," no imputation is
#' required.
#' 
#' @seealso 
#' 
#' @author WEST Inc.
#'
#' @examples
#' \dontrun{
#' #   ---- Fit splines and impute for missing data for each unique
#' #   ---- trapPositionID in data frame catch.df.
#' fitCatch <- F.catch.model(catch.df)
#' }

F.catch.model <- function( catch.df ){
  
  # catch.df <- df2      # from est_catch_trapN.r
  # catch.df <- df3      # from est_catch.r
  
  #   ---- Get necessary variables from the global environment.
  knotMesh <- get("knotMesh",envir=.GlobalEnv)
  max.ok.gap <- get("max.ok.gap",envir=.GlobalEnv)
  time.zone <- get("time.zone",envir=.GlobalEnv)
  
  #   ---- Sort the data appropriately.  
  catch.df <- catch.df[ order(catch.df$trapPositionID, catch.df$EndTime), ]
  
  #   ---- Calculate an offset in terms of log hours.
  catch.df$log.sampleLengthHrs <- log(as.numeric( catch.df$SampleMinutes/60 ))
  
  #   ---- Fit null model.  The gap catches are NA here, so they
  #   ---- are dropped from the fit.  Later, they are replaced.
  fit <- glm( n.tot ~ offset(log.sampleLengthHrs) , family=poisson, data=catch.df )
  fit.AIC <- AIC(fit)
  
  #   ---- Fit glm model, increasing df, until the model goes bad.  
  cat(paste("Number of non-zero catches : ", sum(!is.na(catch.df$n.tot) & (catch.df$n.tot > 0)), "\n"))
  cat("Catch model fitting:\n")
  cat(paste("df= ", 0, ", conv= ", fit$converged, " bound= ", fit$boundary, " AIC= ", round(fit.AIC, 4), "\n"))
  
  #   ===============================================================================================================================
  #   ---- Estimate a spline of increasing complexity.  Start with 
  #   ---- linear, quadratic, cubic (spline with no internal knots),
  #   ---- cubic (spline with one internal knot), and so on.
  
  #   ---- Check to see if we have at least 10 good trapping visits.
  if( sum(!is.na(catch.df$n.tot) & (catch.df$n.tot > 0)) > 10 ){
    
    #   ---- This is the degrees of freedom in the smoother part, excluding intercept.
    cur.df <- 1   
    
    #   ---- Get the number of good data points we have to work with, for this trap.
    nGoodData <- length(!is.na(catch.df$n.tot))    
    
    #   ---- Build model complexity sequentially.  
    repeat{
      
      #   ---- Check if for N of data points to support complexity for this repeat.
      valid <- (cur.df <= floor(nGoodData / knotMesh))
      
      #   ---- Ensure at least 15 non-NA trapping EndTimes are present in the data.
      #   ---- Need this because of gaps in fishing - don't want to overfit the data.  
      #   ---- Thus, indicator valid must be TRUE.  
      if( cur.df == 1 & (valid == TRUE) ){
        j <- as.numeric( format(catch.df$EndTime, "%j"))  # Add linear spline term.   
        bs.sEnd <- matrix(j,ncol=1)
      } else if( cur.df == 2 & (valid == TRUE) ){
        j <- as.numeric( format(catch.df$EndTime, "%j"))  # Add quadratic spline term.  
        bs.sEnd <- cbind(Lin=j, Quad=j*j)
      } else if( cur.df > 2 & (valid == TRUE) ){
        bs.sEnd <- bs( catch.df$EndTime, df=cur.df )      # Add cubic spline term.  
      }
      
      #   ---- Hold in memory the current fit.  
      cur.fit <- tryCatch(glm( n.tot ~ offset(log.sampleLengthHrs) + bs.sEnd, family=poisson, data=catch.df ),error=function(e) e )
      
      #   ---- Ensure current fit is statistically interpretable.  
      if(class(cur.fit)[1] == 'simpleError'){
        cur.AIC <- NA
        cat(paste0("Model fell apart;  tryCatch caught the output.  You may wish to investigate for trap ",catch.df$trapPositionID,".\n"))
      } else {
        cur.AIC <- AIC( cur.fit )
        cat(paste("df= ", cur.df, ", conv= ", cur.fit$converged, " bound= ", cur.fit$boundary, " AIC= ", round(cur.AIC, 4), "\n"))
      }
      
      #   ---- If AIC is NA, we have a problem;  otherwise, compare the 
      #   ---- new fit with the previous fit.  Do we keep going or stop?
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
  
  #   ---- Done fitting model.
  print(summary(fit, disp=sum(residuals(fit, type="pearson")^2)/fit$df.residual))
  
  
  #   ===============================================================================================================================
  #   ---- Loop over gaps one at a time, predicting catch for a maximum of 24-hour periods.  
  
  catch.df$gamEstimated <- FALSE
  
  #   ---- Number of columns in smoother part only.  Excludes intercept.
  degree <- length(coef(fit)) - 1  
  if( degree <= 2 ){
    nots <- -1
    b.knots <- -1
  } else {
    nots <- attr(bs.sampleEnd,"knots")
    b.knots <- attr(bs.sampleEnd, "Boundary.knots")
  }
  
  #   ---- Setup for creation of goodies below.  
  i <- 1
  all.new.dat <- NULL
  all.gaplens <- NULL
  all.bdates <- NULL
  jason.new <- NULL
  
  #   ---- Sweet through catch.df, inserting imputation rows where needed;
  #   ---- i.e., wherever there is an instance of 'Not fishing'.
  repeat{
    
    #   ---- Don't do last line - no need to check.  At end, no more gaps by def'n.
    if( i >= nrow(catch.df) ) break  
    
    if( catch.df$TrapStatus[i] == "Fishing" ){
      i <- i + 1
    } else if(catch.df$SampleMinutes[i] <= (60*max.ok.gap)){
      
      #   ---- Eliminate the small gaps.  I will never be 1 if we are here.  
      #   ---- But put if just in case.
      if( i > 1 ){
        catch.df$EndTime[i-1] <- catch.df$EndTime[i]
        catch.df$batchDate[i-1] <- catch.df$batchDate[i]
        catch.df <- catch.df[-i,]
      }
    } else {
      
      #   ---- We have a missing value, i.e., a period of time > max.ok.gap 
      #   ---- with no estimate of fish.
      
      #   ---- i.gapLens is length of all intervals for which we want to
      #   ---- predict. They all sum to total of the 'Not fishing' period.
      #   ---- Note that we convert to hours, to match the offset.
      i.gapLens <- c(rep(24, floor(catch.df$SampleMinutes[i]/ (24*60))), (catch.df$SampleMinutes[i]/60) %% 24 ) 
      
      #   ---- Make sure last interval is > max.ok.gap.  
      ng <- length(i.gapLens)
      if( i.gapLens[ng] <= max.ok.gap ){
        
        # ---- Add last small gap to previous interval.  
        i.gapLens[ ng - 1 ] <- i.gapLens[ng-1] + i.gapLens[ng]  
        i.gapLens <- i.gapLens[-ng]
      }
      ng <- length(i.gapLens)
      
      #   ---- Calculate the length of time for which we need an estimate.
      sEnd <- catch.df$StartTime[i] + cumsum(i.gapLens * 60*60)
      class(sEnd) <- class(catch.df$StartTime)
      attr(sEnd, "tzone") <- time.zone
      
      sStart <- catch.df$StartTime[i] + cumsum( c(0,i.gapLens[-ng]) * 60*60 )
      class(sStart) <- class(sStart)
      attr(sStart, "tzone") <- time.zone
      
      #   ---- The smoother.
      if( degree <= 2 ){
        bs.sEnd <- -1
      } else {
        bs.sEnd <- bs( sEnd, knots=nots, Boundary.knots=b.knots )
        dimnames(bs.sEnd)[[2]] <- paste("bs.sampleEnd", dimnames(bs.sEnd)[[2]], sep="")
      }
      
      if( degree == 0 ){
        
        #   ---- Mean model.
        new.dat <- matrix( 1, length(sEnd), 1 )
      } else if(degree == 1){
        
        #   ---- Linear model.
        j <- as.numeric( format(sEnd, "%j") )
        new.dat <- cbind( rep(1, length(sEnd)), j )
      } else if(degree == 2){
        
        #   ---- Quadratic model.
        j <- as.numeric( format(sEnd, "%j") )
        new.dat <- cbind( rep(1, length(sEnd)), j, j*j )
      } else {
        
        #   ---- Cubic Spline model.
        new.dat <- cbind( 1, bs.sEnd )
      }
      
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
      new$EndTime <- sEnd
      new$StartTime <- sStart
      new$gamEstimated <- TRUE
      new$siteID <- catch.df$siteID[i]
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
    #   ---- No brace here for break if above.
  }
  
  #   ---- If there are no gaps, all.new.dat, all.gaplens, and all.bdates are NULL. 
  #   ---- Turn these into NA so that storing them in a list in function est_catch 
  #   ---- works.  (Note that assigning NULL to list item collapses the list.)
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
  
  #   ---- Reduce scope to only those variables needed.  
  jason.new <- jason.new[,c('batchDate','trapVisitID','trapPositionID','n.tot')]   
  list(df2=catch.df, fit=fit, X.for.missings=all.new.dat, gaps=all.gaplens, batchDate.for.missings=all.bdates, true.imp=jason.new)
}
