#' @export
#' 
#' @title F.est.catch
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
#' @details Function \code{F.est.catch} assumes that only species and life 
#'   stages of interest are included in data frame \code{catch.df}.  In the case
#'   of estimating passage over distinct values of variable \code{lifeStage},
#'   estimation occurs per unique combinations of \code{lifeStage} and
#'   \code{FinalRun};  otherwise, estimation occurs per distinct value of
#'   variable \code{FinalRun}.
#'   
#'   Catch estimation occurs on a per-trap basis, where traps could be modified 
#'   \code{trapPositionID}s with a decimal appendage, due to gaps in fishing 
#'   identified via function \code{F.get.catch.data}.  See the "Fishing Gaps"
#'   query series in function \code{F.sqlFile}.  
#'   
#'   Daily catch sequences are examined for strings of preceding and antecedent 
#'   zeros (and \code{NA}s, resulting from periods not fished).  These zero 
#'   strings are not included as part of the estimation procedure performed by 
#'   function \code{F.catch.model}.  Only zeros and \code{NA}s before the first,
#'   and after the last, caught fish are excluded from modeling procedures.  All
#'   inclusive zeros, indicating a trapping instance during which no fish were 
#'   caught, and inclusive \code{NA}s, indicating a period of no fishing, are 
#'   retained. These zeros and \code{NA}s contribute to subsequent functions,
#'   where they contribute to plots, and estimates of hours fished.
#'   
#'   Function \code{F.est.catch} calls function \code{F.catch.model}, in which 
#'   catch sequences are fit and periods of "Not fishing" receive imputed 
#'   values. When the original trap-specific data frame returns from 
#'   \code{F.catch.model}, it has extra lines in it, with one extra line for 
#'   each 24-hour not-fishing period bigger than \code{max.ok.gap}, where 
#'   \code{max.gap.ok} is specified as 2 hours, and is set in the \code{GlobalVars}
#'   function.  For example, if a period of 
#'   "Not fishing" is 3 days, there are 3 extra records, where variables 
#'   \code{sampleStart} and \code{sampleEnd} for each of the new records are 
#'   redefined so that no "Not fishing" period remains.  For these imputed 
#'   periods, variable \code{gamEstimated} is \code{TRUE}. Variable 
#'   \code{sampleEnd} assigns the value of variable \code{batchDate}, as usual. 
#'   On return, there is either an observed catch value alone, an imputed catch
#'   value alone, or a combination of the two, for each day from the start of
#'   the season to its end.  Days with both observed and imputed catch values
#'   result from days containing both fishing and non-fishing periods.
#'   
#'   The total number of operating traps per day is obtained via internal 
#'   function \code{F.est.catch.trapN}, which is essentially a rehash of 
#'   \code{F.est.catch}.  Function \code{F.est.catch.trapN} fits all splines all
#'   over again while utilizing all zero records.  It is called for its side 
#'   effect of counting the total number of operating traps per day, which 
#'   includes the preceding and antecedent zero days deleted via function 
#'   \code{F.est.catch} in the course of processing, as described above.
#'   
#'   The output of this function comprises a list containing eight interior 
#'   objects, which may be a data frame, or an interior list containing a number
#'   of interior objects equal to the number of unique traps in 
#'   data.frame \code{catch.df}.  The eight internal objects include,
#'   
#'   \itemize{ 
#'   \item{\code{catch}}{ -- A data frame of estimated catch, including 
#'   imputed values, per day;} 
#'   \item{\code{fits}}{ -- A list of Poisson \code{glm}-fitted 
#'   objects, possibly with basis spline covariates, used to impute missing 
#'   catches, for each trap;} 
#'   \item{\code{X.miss}}{ -- A list containing a spline 
#'   basis matrix of imputed days for each trap;} 
#'   \item{\code{gaps}}{ -- A list 
#'   containing a numeric vector of hours of "Not fishing" for "Not fishing"
#'   days, necessarily with all entries less than 24, for each trap;}
#'   \item{\code{b.Dates.miss}}{ -- A list containing a POSIX vector of "Not
#'   fishing" days, for each trap;} 
#'   \item{\code{trapsOperating}}{ -- A data frame
#'   housing the number of traps operating per day;} 
#'   \item{\code{true.imp}}{ -- A
#'   data frame containing information of imputed values; and}
#'   \item{\code{allDates}}{ -- A data frame summarizing the days on which fishing
#'   begins and ends, taking into account preceding and antecedent zeros and
#'   \code{NA}s.} }
#'   
#'   Note that data frame \code{trapsOperating} originates via function 
#'   \code{F.est.catch.trapN}, as described above.
#'   
#' @seealso \code{F.get.catch.data}, \code{F.est.catch.trapN}
#'  
#' @author Trent McDonald (tmcdonald@west-inc.com)    
#'   
#' @examples
#' \dontrun{
#' #   ---- Estimate catch for each unique trap in data 
#' #   ---- frame catch.df.  Also output a plot.  
#' F.est.catch(catch.df, plot=TRUE, plot.file="raw_catch.pdf")
#' }
F.est.catch <- function( catch.df, plot=TRUE, plot.file="raw_catch.pdf" ){

  # catch.df <- catch.df
  # plot <- TRUE
  # plot.file <- file.root

  time.zone <- get("time.zone", env=.GlobalEnv )

  #   ---- Fill in the gaps for individual traps
  df <- NULL
  true.imp <- NULL
  allJBaseTable <- NULL

  #   ---- The restriction gets traps that have at least 1 caught fish.
  u.traps <- unique( catch.df$trapPositionID )
  catch.fits <- X.miss <- Gaps <- bDates.miss <- vector("list", length(u.traps))   # lists to contain thing to save for bootstrapping
  names(catch.fits) <- u.traps

  #   ---- House results of buffer analysis.
  the.zero.fits <- vector("list",length(unique(catch.df$trapPositionID)))
  allWinners <- NULL 
  catch.df$n.Orig2 <- catch.df$n.Orig   # keep a copy for accounting -- this becomes assignedCatch -- use for checking
  origBeg.date <- origEnd.date <- as.character("1990-01-01")   # need some fake dates.  these will be replaced.
  dateFramer <- data.frame(trapPositionID=u.traps,origBeg.date=rep(as.POSIXct( origBeg.date,"%Y-%m-%d",tz=time.zone)),origEnd.date=rep(as.POSIXct(origBeg.date,"%Y-%m-%d",tz=time.zone)))

  #   ---- Loop over each trap and estimate and impute.
  for( trap in u.traps ){
    cat(paste("==== Catch model for trapPositionID", trap, "========\n" ))

    df2 <- catch.df[catch.df$trapPositionID == trap,]
    df2$rownames <- rownames(df2)

    #   ---- Obtain the size of the buffer, in days.
    #   ---- First value of buffs is index of first non-zero / non-NA in the beginning.
    #   ---- Second value is index of last non-zero / non-NA in the end. 
    #   ---- Third value is length of n.tot vector.
    buffs <- max.buff.days(df2,trap)       

    #   ---- Preserve the original start and end dates.
    origBeg.date <- min(df2$batchDate)
    origEnd.date <- max(df2$batchDate)

    #   ---- Collect true (non-zero & non-NA) beginning and end dates.  Possibly redundant with data frame allWinners.
    dateFramer[dateFramer$trapPositionID == trap,2] <- as.POSIXct( as.character(origBeg.date), "%Y-%m-%d", tz=time.zone)
    dateFramer[dateFramer$trapPositionID == trap,3] <- as.POSIXct( as.character(origEnd.date), "%Y-%m-%d", tz=time.zone)

    #   ---- Assuming vector of goods isn't all zero, proceed.  
    if( !(buffs[1] == buffs[3] & buffs[2] == buffs[3]) ){ 
      
      thisTrap <- vector("list",length(u.traps))
      
      #   ---- Set buffer variables.  These are artifacts from varying these.
      bb <- 0   # number of zeros + NA to keep in the beginning 
      eb <- 0   # number of zeros + NA to keep at the end
      df3 <- chuck.zeros(buffs[1],buffs[2],bb,eb,df2)    
          
      #   ---- Get rid of first temporal records that are 'Not fishing.'
      df3 <- df3[ order(df3$trapPositionID, df3$EndTime), ]  
      m <- 1
      repeat{
        if(df3$TrapStatus[m] == 'Not fishing'){          
          df3 <- df3[-1,]
        } else {
          break
        }
      }
          
      #   ---- Get rid of last temporal records that are 'Not fishing.'
      df3 <- df3[order(df3$EndTime,decreasing=TRUE),]
      m <- 1
      repeat{
        if(df3$TrapStatus[m] == 'Not fishing'){
          df3 <- df3[-1,]
        } else {
          break
        }
      }
      df3 <- df3[order(df3$EndTime),]

      #   ---- Call the modeling function, and impute missing values. 
      thisTrap[[buffs[2]*(bb) + (eb + 1)]] <- suppressWarnings( F.catch.model( df3 ) )#,error=function(e) e )
      the.zero.fits[[match(trap,u.traps)]] <- thisTrap
      
      #   ---- Compile information regarding removal of zeros and NAs. 
      beg.date <- head(the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$df2,1)$batchDate
      end.date <- tail(the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$df2,1)$batchDate
      winner <- data.frame(bZerosRem=buffs[1] - bb - 1,eZerosRem=buffs[2] - eb - 1,bZerosKept=bb,eZerosKept=eb,nrow=nrow(df3),Nrow=nrow(df2[df2$trapPositionID == trap,]),beg.date=beg.date,end.date=end.date)
      winner$trap <- trap
      winner$origBeg.date <- origBeg.date
      winner$origEnd.date <- origEnd.date
      
      df2 <- df2[order(df2$batchDate),]
      #jBaseTable <- tryCatch(plot_spline(trap,df2,thisTrap[[1]],file="spline.pdf",df3), error=function(e) e)
      df.and.fit <- thisTrap[[1]]   
    } else {
      
      #   ---- All records are thrown out, due to zeros and NA.  
      df.and.fit <- suppressWarnings( F.catch.model( df2 ) )
      #jBaseTable <- tryCatch(plot_spline(trap,df2,df.and.fit,file="spline.pdf",df3), error=function(e) e)
    }

    df <- rbind( df, df.and.fit$df2)

    #   ---- Store interim results for each trap in different lists.  
    catch.fits[[which(trap==u.traps)]] <- df.and.fit$fit
    X.miss[[which(trap==u.traps)]] <- df.and.fit$X.for.missings
    Gaps[[which(trap==u.traps)]] <- df.and.fit$gaps
    bDates.miss[[which(trap==u.traps)]] <- df.and.fit$batchDate.for.missings
    true.imp <- rbind(true.imp,df.and.fit$true.imp)
    allWinners <- rbind(allWinners,winner)
  }

  cat("in est_catch.r  DF")
  print( tapply(df$batchDate, df$trapPositionID, range) )
  cat("-------\n")

  # ---- Output underlying splines plot data.
  #write.csv(allJBaseTable,paste0(output.file,"_",catch.df$FinalRun[1],"_",lsLabel,"_allJBaseTable.csv"))

  #   ---- Give something so the est_efficiency program knows those traps that end up with zero fish.
  #   ---- This may be redundant, since allWinners already has a trap variable. 
  allDates <- merge(data.frame(trap=u.traps),allWinners,by=c('trap'),all.x=TRUE)

  #   ---- Now that there are no gaps, sum within traps operating on a batch day, and all checks that occurred on a batch day.
  ind <- list( batchDate=format(df$batchDate, "%Y-%m-%d"), trapPositionID=df$trapPositionID  )
  est.catch <- tapply( df$n.tot, ind, sum )
  p.imputed <- tapply( df$gamEstimated, ind, mean )
  
  #   ---- Save a copy of est.catch for counting traps later.
  tmp.est.catch <- est.catch              
  
  #   ---- Summarize the count of imputed catch, per trap. 
  if(nrow(true.imp) > 0){
    est.catch1 <- data.frame(imputedCatch=tapply(true.imp$n.tot,list(true.imp$trapPositionID), sum))
    est.catch1$Traps <- rownames(est.catch1)
    rownames(est.catch1) <- NULL
  } else {  #   ---- No imputation required over all traps.  
    est.catch1 <- NULL
  }

  #   ---- Summarize the count of the other types of fish, per trap.  
  est.catch2 <- tapply( df$n.Orig, ind, sum )                    # we tally assigned catch
  est.catch2a<- tapply( df$n.Orig2, ind, sum )                   # we tally accounting assigned catch
  est.catch3 <- tapply( df$n.Unassd, ind, sum )                  # we tally unassigned catch
  est.catch4 <- tapply( df$halfConeAssignedCatch, ind, sum )     # we tally n.halfConeAdjAssd catch
  est.catch5 <- tapply( df$halfConeUnassignedCatch, ind, sum )   # we tally n.halfConeAdjUnassd catch
  est.catch6 <- tapply( df$assignedCatch, ind, sum )             # we tally n.fullConeAdjAssd catch
  est.catch7 <- tapply( df$unassignedCatch, ind, sum )           # we tally n.fullConeAdjUnassd catch
  est.catch8 <- tapply( df$modAssignedCatch, ind, sum )          # we tally n.adjUnassd catch
  est.catch9 <- tapply( df$modUnassignedCatch, ind, sum )        # we tally n.adjAssd catch

  #   ---- Un-matrix the results and put into data frames.
  est.catch  <- cbind( expand.grid( batchDate=dimnames(est.catch)[[1]], trapPositionID=dimnames(est.catch)[[2]]),
                       catch=c(est.catch), imputed.catch=c(p.imputed) )
  est.catch2 <- cbind( expand.grid( batchDate=dimnames(est.catch2)[[1]], trapPositionID=dimnames(est.catch2)[[2]]),
                       assdCatch=c(est.catch2), imputed.catch=c(p.imputed) )                # tally assigned catch
  est.catch2a<- cbind( expand.grid( batchDate=dimnames(est.catch2a)[[1]], trapPositionID=dimnames(est.catch2a)[[2]]),
                       assdCatchA=c(est.catch2a), imputed.catch=c(p.imputed) )              # tally assigned catch
  est.catch3 <- cbind( expand.grid( batchDate=dimnames(est.catch3)[[1]], trapPositionID=dimnames(est.catch3)[[2]]),
                       UnassdCatch=c(est.catch3), imputed.catch=c(p.imputed) )              # tally unassigned catch
  est.catch4 <- cbind( expand.grid( batchDate=dimnames(est.catch4)[[1]], trapPositionID=dimnames(est.catch4)[[2]]),
                       halfConeAssignedCatch=c(est.catch4), imputed.catch=c(p.imputed) )    # tally halfConeAssignedCatch
  est.catch5 <- cbind( expand.grid( batchDate=dimnames(est.catch5)[[1]], trapPositionID=dimnames(est.catch5)[[2]]),
                       halfConeUnassignedCatch=c(est.catch5), imputed.catch=c(p.imputed) )  # tally halfConeUnassignedCatch
  est.catch6 <- cbind( expand.grid( batchDate=dimnames(est.catch6)[[1]], trapPositionID=dimnames(est.catch6)[[2]]),
                       assignedCatch=c(est.catch6), imputed.catch=c(p.imputed) )            # tally assignedCatch
  est.catch7 <- cbind( expand.grid( batchDate=dimnames(est.catch7)[[1]], trapPositionID=dimnames(est.catch7)[[2]]),
                       unassignedCatch=c(est.catch7), imputed.catch=c(p.imputed) )          # tally unassignedCatch
  est.catch8 <- cbind( expand.grid( batchDate=dimnames(est.catch8)[[1]], trapPositionID=dimnames(est.catch8)[[2]]),
                       modAssignedCatch=c(est.catch8), imputed.catch=c(p.imputed) )          # tally modAssignedCatch
  est.catch9 <- cbind( expand.grid( batchDate=dimnames(est.catch9)[[1]], trapPositionID=dimnames(est.catch9)[[2]]),
                       modUnassignedCatch=c(est.catch9), imputed.catch=c(p.imputed) )        # tally modUnassignedCatch

  #   ---- Get both total catch and assigned catch and unassigned catch and halfConeAdj.
  #   ---- This is where we compile all the different types of catch into one data frame.  
  est.catch <- merge(est.catch,est.catch2 ,by=c('batchDate','trapPositionID','imputed.catch'))
  est.catch <- merge(est.catch,est.catch3 ,by=c('batchDate','trapPositionID','imputed.catch'))
  est.catch <- merge(est.catch,est.catch2a,by=c('batchDate','trapPositionID','imputed.catch'))
  est.catch <- merge(est.catch,est.catch4 ,by=c('batchDate','trapPositionID','imputed.catch'))
  est.catch <- merge(est.catch,est.catch5 ,by=c('batchDate','trapPositionID','imputed.catch'))
  est.catch <- merge(est.catch,est.catch6 ,by=c('batchDate','trapPositionID','imputed.catch'))
  est.catch <- merge(est.catch,est.catch7 ,by=c('batchDate','trapPositionID','imputed.catch'))
  est.catch <- merge(est.catch,est.catch8 ,by=c('batchDate','trapPositionID','imputed.catch'))
  est.catch <- merge(est.catch,est.catch9 ,by=c('batchDate','trapPositionID','imputed.catch'))
  est.catch$batchDate <- as.POSIXct( as.character(est.catch$batchDate), "%Y-%m-%d", tz=time.zone)
  est.catch <- est.catch[order(est.catch$trapPositionID,est.catch$batchDate),]

  #   ---- Fish accounting;  useful for checking.  
  # theSumsMiddle <- accounting(est.catch,"byTrap")
  # if(!is.null(est.catch1)){
  #   theSumsMiddle <<- merge(theSumsMiddle,est.catch1,by=c('Traps'),all.x=TRUE)
  # }

  #   ---- Get correct Ntraps per day, which need fishing days with zero catch. 
  #   ---- Note that essentially refits all of the traps, all over again.  Said 
  #   ---- another way, it does, in its entirety, what we used to do before we 
  #   ---- started deleting out preceding and antecedent zeros and NA.  
  trapsOperating <- F.est.catch.trapN(catch.df, plot=FALSE, plot.file="raw_catch.pdf" )

  # By design, we now have missing days in est.catch.  Fix those now.
  # Possibly, we are missing days due to deleted imputed periods.
  # Cf. Trap 42050, RBDD, May 2002.
  # Use collected data to rebuild the days we would have; i.e., reconstruct est.catch as it used to be, batchDate-wise.
  est.catch.fake <- NULL
  for(trap in dateFramer$trapPositionID){
    theDays <- dateFramer[dateFramer$trapPositionID == trap,]$origEnd.date - dateFramer[dateFramer$trapPositionID == trap,]$origBeg.date + 1
    
    #   ---- Get correct enddate.
    theDaysA <- seq(dateFramer[dateFramer$trapPositionID == trap,]$origBeg.date,dateFramer[dateFramer$trapPositionID == trap,]$origEnd.date,length.out=theDays)
    
    #   ---- Get Daylight Savings Time awkwardness correct.  Note the 22, so as to hit all days at least once.  
    theDaysB <- seq(dateFramer[dateFramer$trapPositionID == trap,]$origBeg.date,dateFramer[dateFramer$trapPositionID == trap,]$origEnd.date,by=60*60*22)          
    theDays <- unique(strptime(c(theDaysA,theDaysB),"%F",tz=time.zone))                                                                                           
    
    #   ---- Now, put together, and map to days, so as to get unique.
    lil.est.catch.fake <- data.frame(batchDate=theDays,trapPositionID=trap)
    est.catch.fake <- rbind(est.catch.fake,lil.est.catch.fake)
  }
  est.catch <- merge(est.catch.fake,est.catch,by=c('batchDate','trapPositionID'),all.x=TRUE)

  #   ---- If a trap runs without checks for 48 hours say, it runs over two batch dates.  When this happens,
  #   ---- the above statements result in an NA for catch on the day it skipped.  The real (non-imputed) catch
  #   ---- for these days is 0.  Replace these NAs with zeros.  (But, perhaps these lines should be tossed...)
  est.catch$assdCatch <- ifelse(is.na(est.catch$assdCatch),0,est.catch$assdCatch)
  est.catch$UnassdCatch <- ifelse(is.na(est.catch$UnassdCatch),0,est.catch$UnassdCatch)
  est.catch$assdCatchA <- ifelse(is.na(est.catch$assdCatchA),0,est.catch$assdCatchA)
  est.catch$halfConeAssignedCatch <- ifelse(is.na(est.catch$halfConeAssignedCatch),0,est.catch$halfConeAssignedCatch)
  est.catch$halfConeUnassignedCatch <- ifelse(is.na(est.catch$halfConeUnassignedCatch),0,est.catch$halfConeUnassignedCatch)
  est.catch$assignedCatch <- ifelse(is.na(est.catch$assignedCatch),0,est.catch$assignedCatch)
  est.catch$unassignedCatch <- ifelse(is.na(est.catch$unassignedCatch),0,est.catch$unassignedCatch)
  est.catch$modAssignedCatch <- ifelse(is.na(est.catch$modAssignedCatch),0,est.catch$modAssignedCatch)
  est.catch$modUnassignedCatch <- ifelse(is.na(est.catch$modUnassignedCatch),0,est.catch$modUnassignedCatch)
  est.catch$imputed.catch <- ifelse(is.na(est.catch$imputed.catch),0,est.catch$imputed.catch)
  est.catch$catch <- ifelse(is.na(est.catch$catch),0,est.catch$catch)

  #   ---- Assign attributes for plotting.
  u.ss.rows <- !duplicated(catch.df$trapPositionID)
  u.ss <- catch.df$trapPositionID[u.ss.rows]
  u.ss.name <- catch.df$TrapPosition[u.ss.rows]
  ord <- order( u.ss )
  u.ss <- u.ss[ ord ]
  u.ss.name <- u.ss.name[ ord ]

  #   ---- Merge in TrapPosition so that graphs show text labels instead of IDs.
  #   ---- Possibly redundant with data frame masterCatch below.  
  est.catch <- merge(est.catch,unique(catch.df[,c('trapPositionID','TrapPosition')]),by=c('trapPositionID'),all.x=TRUE)
  # attr(est.catch, "site.name") <- catch.df$siteName[1]
  # attr(est.catch, "subsites") <- data.frame(subSiteID = u.ss, subSiteName=u.ss.name)
  # attr(est.catch, "run.name") <- run.name
  # attr(est.catch, "life.stage" ) <- catch.df$lifeStage[1]
  # attr(est.catch, "species.name") <- "Chinook Salmon"

  #   ---- Collapse imputed data to day.  We could have two imputed records for one day, 
  #   ---- if more than one trap is non-functioning.
  if(nrow(true.imp) > 0){
    true.imp$shortdate <- strftime(true.imp$batchDate, format="%Y-%m-%d")
    true.imp.sum <- aggregate(true.imp$n.tot,by=list(true.imp$shortdate,true.imp$trapPositionID), FUN=sum)
    true.imp.sum$batchDate <- as.POSIXct(as.character(true.imp.sum$Group.1), "%Y-%m-%d", tz=time.zone)
    true.imp.sum$Group.1 <- NULL
    names(true.imp.sum)[names(true.imp.sum) == 'V1'] <- 'imputedCatch'     #'n.tot'
    names(true.imp.sum)[names(true.imp.sum) == 'Group.2'] <- 'trapPositionID'
  } else {  
    #   ---- Account for no imputed days.  Make a fake data frame so code continues.
    true.imp.sum <- data.frame(trapPositionID=u.traps,imputedCatch=rep(NA,length(u.traps)),batchDate=rep(est.catch$batchDate[1],length(u.traps)))
    true.imp <- true.imp.sum
    names(true.imp)[names(true.imp) == 'imputedCatch'] <- 'n.tot'  
  }

  #   ---- Clean up the resulting data frame, up to this point.  
  est.catch$assdCatch <- est.catch$UnassdCatch <- est.catch$assdCatchA <- NULL   # what are these ...?
  masterCatch <- merge(est.catch,true.imp.sum,by=c('trapPositionID','batchDate'),all.x=TRUE)
  masterCatch$imputedCatch <- ifelse(is.na(masterCatch$imputedCatch),0,masterCatch$imputedCatch)
  masterCatch$assignedCatch <- ifelse(is.na(masterCatch$assignedCatch),0,masterCatch$assignedCatch)
  masterCatch$totalEstimatedCatch <- masterCatch$imputedCatch + masterCatch$modAssignedCatch + masterCatch$modUnassignedCatch

  #   ---- Add attributes for use in plotting.  
  attr(masterCatch, "site.name") <- catch.df$siteName[1]
  attr(masterCatch, "subsites") <- data.frame(subSiteID = u.ss, subSiteName=u.ss.name)
  attr(masterCatch, "run.name") <- run.name#catch.df$FinalRun[1]
  attr(masterCatch, "life.stage" ) <- catch.df$lifeStage[1]
  attr(masterCatch, "species.name") <- "Chinook Salmon"

  #   ---- Output a plot of catch, with supersmoother approximation to the GAM spline.  
  if( !is.na(plot.file) ) {
    out.fn <- F.plot.catch.model( masterCatch, file=plot.file )
  } else {
    out.fn <- NULL
  }

  cat("Catch estimation complete...\n")

  #   ---- Put all the goodies into a list for further processing.  
  ans <- list(catch=est.catch, fits=catch.fits, X.miss=X.miss, gaps=Gaps, bDates.miss=bDates.miss, trapsOperating=trapsOperating, true.imp=true.imp, allDates=allDates)
  attr(ans, "out.fn.list") <- out.fn
  ans

}
