#' @export
#' 
#' @title F.est.catch.trapN
#' 
#' @description Estimate catch for every day of the season, per trap.
#'   
#' @param catch.df A data frame, possibly restricted to one \code{lifeStage},
#'   resulting from a call to \code{F.get.catch.data}.  Run season is an
#'   attribute of this data frame.
#' @param plot A logical indicating if catch is to be plotted over time, per
#'   trap.
#' @param plot.file The name to which a graph of catch is to be output, if 
#'   \code{plot=TRUE}.
#'   
#' @return A list \code{ans} containing catch model information for each trap 
#'   listed in \code{catch.df}.  See Details.
#'   
#' @details Function \code{F.est.catch.trapN} performs exactly the same 
#'   functionality as function \code{F.est.catch}.  However,
#'   \code{F.est.catch.trapN} utilizes the full observed catch dataset in its
#'   implementation, i.e., including all preceding and antecedent strings of
#'   zeros, for each individual trap contained in the data.  Via the use of
#'   these modified data, accurate data regarding the number of deployed traps,
#'   per day, are obtained.
#'   
#' @seealso \code{F.est.catch}
#'  
#' @author WEST Inc.
#'   
#' @examples
#' \dontrun{
#' #   ---- Estimate catch for each unique trap in data 
#' #   ---- frame catch.df.  Also output a plot.  
#' F.est.catch.trapN(catch.df, plot=TRUE, plot.file="raw_catch.pdf")
#' }
F.est.catch.trapN <- function( catch.df, plot=TRUE, plot.file="raw_catch.pdf" ){
  
  # catch.df <- catch.df
  # plot <- TRUE
  # plot.file = "raw_catch.pdf"

  
  time.zone <- get("time.zone", envir=.GlobalEnv )

  #   ---- Fill in the gaps for individual traps
  df <- NULL
  true.imp <- NULL
  u.traps <- unique( catch.df$trapPositionID )  
  catch.fits <- X.miss <- Gaps <- bDates.miss <- vector("list", length(u.traps))   # lists to contain thing to save for bootstrapping 
  names(catch.fits) <- u.traps

  #   ---- Loop over each individual trap, getting statistics of interest.  
  for( trap in u.traps ){
      cat(paste("==== Catch model for trapPositionID", trap, "========\n" ))
  
      df2 <- catch.df[catch.df$trapPositionID == trap,]
  
      #   ---- Impute a value for the gaps.  See F.est.catch for details.  
      df.and.fit <- suppressWarnings( F.catch.model( df2 ) )
  
      #   --- Clean up output.  
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