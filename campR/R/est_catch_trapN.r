#' @export F.est.catch.trapN
#' 
#' @title F.est.catch.trapN
#' 
#' @description
#' 
#'    Estimate capture for every day of the season.
#'    This assumes that only species and life stages we want are
#'    in catch.df.  I.e., that appropriate filtering has already been done.
#' 
#'    Input:
#'    catch.df = data frame resulting from call to F.get.catch.data.  Run season is
#'        an attribute of this data frame
#' 
#'    Output:
#'    A data frame containing $batchDay and $catch
#' 
#'   plot.file=file.root
#' 
#' 
#' @param  catch.df describe argument
#' @param  plot=TRUE describe argument
#' @param  plot.file="raw_catch.pdf"  describe argument
#' 
#' @details other comments found in file
#'         Times in run season must be same as time in batchDate each day. 
#'         batchDate must occur at same time every day.  Can be missing days, but always at same time when present.
#'    Impute a value for the gaps
#'    When df2 comes back from F.catch.model, it has extra lines in it.  One extra line for each 24 hour period in the 
#'    gaps that were bigger than max.ok.gap. If gap = 3 days, there will be 3 extra lines.
#'    sampleStart and sampleEnd for each of the new lines are defined so that no gap appears  now.  Variable
#'    'gamEstimated' is true for these periods. Batch date is assigned based on sampleEnd, as usual. 
#'    On return, there is a value or imputed value for each day from start of season to end of season.
#'    ---- Now that there are no gaps, sum within traps operating on a batch day, and all checks that occurred on a batch day.
#'    ---- Un-matrix the results and put into a data frame
#'    ---- Now remove times before and after a trap started and stopped.  The above tapply's using ind inserted NA for days when one trap was not fishing but others where. 
#'         There are no gaps between the start and stop of a trap in a season, but each trap could operated over a different part of the season.
#'         If you want to leave all the days in, comment the following code out.
#'    ---- Before we un-matrix-ized est.catch, we saved a copy so we could compute number of traps operating each day because it is easier.
#'         But, gaps are filled.  Take back out the gaps when counting.
#' 
#' @return describe return value
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{related routine}}, \code{\link{related routine}}
#' 
#' @examples
#' # insert examples
#' 
F.est.catch.trapN <- function( catch.df, plot=TRUE, plot.file="raw_catch.pdf" ){
#
#   Estimate capture for every day of the season.
#   This assumes that only species and life stages we want are
#   in catch.df.  I.e., that appropriate filtering has already been done.
#
#   Input:
#   catch.df = data frame resulting from call to F.get.catch.data.  Run season is
#       an attribute of this data frame
#
#   Output:
#   A data frame containing $batchDay and $catch
#
#  plot.file=file.root

  
time.zone <- get("time.zone", env=.GlobalEnv )

#   ---- Establish the days on which estimates need to be produced.
#        Times in run season must be same as time in batchDate each day. 
#        batchDate must occur at same time every day.  Can be missing days, but always at same time when present.
#start.season <- min(catch.df$batchDate)
#end.season   <- max(catch.df$batchDate)




#   ---- Fill in the gaps for individual traps
df <- NULL
true.imp <- NULL
u.traps <- unique( catch.df$trapPositionID )  
catch.fits <- X.miss <- Gaps <- bDates.miss <- vector("list", length(u.traps))   # lists to contain thing to save for bootstrapping 
names(catch.fits) <- u.traps
# catch.dff <<- catch.df   # save a copy for debugging


for( trap in u.traps ){
    cat(paste("==== Catch model for trapPositionID", trap, "========\n" ))

    df2 <- catch.df[catch.df$trapPositionID == trap,]

    #   Impute a value for the gaps
    #   When df2 comes back from F.catch.model, it has extra lines in it.  One extra line for each 24 hour period in the 
    #   gaps that were bigger than max.ok.gap. If gap = 3 days, there will be 3 extra lines.
    #   sampleStart and sampleEnd for each of the new lines are defined so that no gap appears  now.  Variable
    #   'gamEstimated' is true for these periods. Batch date is assigned based on sampleEnd, as usual. 
    #   On return, there is a value or imputed value for each day from start of season to end of season.
    
    df.and.fit <- suppressWarnings( F.catch.model( df2 ) )  # df.and.fit is list of, $df2 contains data frame, $fit contains model, etc

    df <- rbind( df, df.and.fit$df2)
    catch.fits[[which(trap==u.traps)]] <- df.and.fit$fit
    X.miss[[which(trap==u.traps)]] <- df.and.fit$X.for.missings
    Gaps[[which(trap==u.traps)]] <- df.and.fit$gaps
    bDates.miss[[which(trap==u.traps)]] <- df.and.fit$batchDate.for.missings

    true.imp <- rbind(true.imp,df.and.fit$true.imp)
}

cat("in est_catch.r  DF")
print( tapply(df$batchDate, df$trapPositionID, range) )
cat("-------\n")

#   ---- Now that there are no gaps, sum within traps operating on a batch day, and all checks that occurred on a batch day.
ind <- list( batchDate=format(df$batchDate, "%Y-%m-%d"), trapPositionID=df$trapPositionID  )
est.catch <- tapply( df$n.tot, ind, sum )
p.imputed <- tapply( df$gamEstimated, ind, mean )

tmp.est.catch <- est.catch   # save a copy of est.catch for counting traps later


#   ---- Un-matrix the results and put into a data frame
est.catch <- cbind( expand.grid( batchDate=dimnames(est.catch)[[1]], trapPositionID=dimnames(est.catch)[[2]]), 
                catch=c(est.catch), imputed.catch=c(p.imputed) )

est.catch$batchDate <- as.POSIXct( as.character(est.catch$batchDate), "%Y-%m-%d", tz=time.zone)                
est.catch <- est.catch[order(est.catch$trapPositionID,est.catch$batchDate),]

#   ---- Now remove times before and after a trap started and stopped.  The above tapply's using ind inserted NA for days when one trap was not fishing but others where. 
#        There are no gaps between the start and stop of a trap in a season, but each trap could operated over a different part of the season.
#        If you want to leave all the days in, comment the following code out.
trapSeasons <- tapply(est.catch$catch, est.catch$trapPositionID, function(x){ 
            seas <- which(!is.na(x))
            f <- min(seas)
            l <- max(seas)
            ans <- rep(FALSE,length(x))
            ans[f:l] <- TRUE
            ans
        }) 

trapSeasons <- unlist(trapSeasons)

est.catch <- est.catch[ trapSeasons, ] 

#   ---- Before we un-matrix-ized est.catch, we saved a copy so we could compute number of traps operating each day because it is easier.
#        But, gaps are filled.  Take back out the gaps when counting.
trapSeasons <- matrix( trapSeasons, nrow=nrow(tmp.est.catch), ncol=ncol(tmp.est.catch) )
for( i in 1:length(bDates.miss) ){
    tmp.est.catch[ trapSeasons[,i] & is.na(tmp.est.catch[,i]), i ] <- 0  # Add days when trap was operating but no trap visit.  I.e., trap operated for >24 hours without a check.  Must do this first, before next line.
    tmp.est.catch[dimnames(tmp.est.catch)[[1]] %in% format(bDates.miss[[i]]), i] <- NA   #  blank out the gaps.
}
trapsOperating <- apply(tmp.est.catch, 1, function(x){sum(!is.na(x))} )
trapsOperating <- data.frame( batchDate=names(trapsOperating), nTrapsOperating=trapsOperating, stringsAsFactors=F )
trapsOperating$batchDate <- as.POSIXct( as.character(trapsOperating$batchDate), "%Y-%m-%d", tz=time.zone)                

ans <- trapsOperating

ans

}
