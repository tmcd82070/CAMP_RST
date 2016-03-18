F.est.catch <- function( catch.df, plot=TRUE, plot.file="raw_catch.pdf" ){
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
allJBaseTable <- NULL

# the restriction here is to get traps that have at least 1 caught fish.
u.traps <- unique( catch.df$trapPositionID )
#u.traps <- unique( catch.df[catch.df$n.tot > 0 & !is.na(catch.df$n.tot),]$trapPositionID )   # can't have this df restriction if we do a zero analysis
catch.fits <- X.miss <- Gaps <- bDates.miss <- vector("list", length(u.traps))   # lists to contain thing to save for bootstrapping
names(catch.fits) <- u.traps
# catch.dff <<- catch.df   # save a copy for debugging




# ----------- jason adds 12.04.2015 to house results of buffer analysis. ---------------------------

the.zero.fits <- vector("list",length(unique(catch.df$trapPositionID)))    # catch.df.ls?
allWinnerDates <- NULL   # jason adds

# ----------- jason adds 12.04.2015 to house results of buffer analysis. ---------------------------

catch.df$n.Orig2 <- catch.df$n.Orig   # we do stuff to n.Orig...keep a copy for accounting purposes -- this becomes assignedCatch -- use for checking
origBeg.date <- origEnd.date <- as.character("1990-01-01")   # need some fake dates.  these will be replaced.
dateFramer <- data.frame(trapPositionID=u.traps,origBeg.date=rep(as.POSIXct( origBeg.date, "%Y-%m-%d", tz=time.zone)),origEnd.date=rep(as.POSIXct( origBeg.date, "%Y-%m-%d", tz=time.zone)))

for( trap in u.traps ){
    cat(paste("==== Catch model for trapPositionID", trap, "========\n" ))

    df2 <- catch.df[catch.df$trapPositionID == trap,]
    df2$rownames <- rownames(df2)

    #   Impute a value for the gaps
    #   When df2 comes back from F.catch.model, it has extra lines in it.  One extra line for each 24 hour period in the
    #   gaps that were bigger than max.ok.gap. If gap = 3 days, there will be 3 extra lines.
    #   sampleStart and sampleEnd for each of the new lines are defined so that no gap appears  now.  Variable
    #   'gamEstimated' is true for these periods. Batch date is assigned based on sampleEnd, as usual.
    #   On return, there is a value or imputed value for each day from start of season to end of season.

    # --------------- jason generalizes to run catch models over all possible buffering zero iterations.  12.4.2015.-----------------------------------------------
    buffs <- max.buff.days(df2,trap)       # first value is zeros + NA in the beg, last is zeros + NA in the end. 3rd value is length of n.tot vector.

    origBeg.date <- min(df2$batchDate)
    origEnd.date <- max(df2$batchDate)

    # collect true beginning and end dates
    dateFramer[dateFramer$trapPositionID == trap,2] <- as.POSIXct( as.character(origBeg.date), "%Y-%m-%d", tz=time.zone)
    dateFramer[dateFramer$trapPositionID == trap,3] <- as.POSIXct( as.character(origEnd.date), "%Y-%m-%d", tz=time.zone)

    if( !(buffs[1] == buffs[3] & buffs[2] == buffs[3]) ){ #& (buffs[3] - buffs[1] - buffs[2] >=5) ){     # prob only need to check 1 of these conditions
      # 1st, assume both buffs are non-zero.
      iters <- buffs[1]*buffs[2]
      thisTrap <- vector("list",iters)
      allIter <- NULL
      bsSpine <- c('(Intercept)',paste0(rep('bs.sEnd',30),seq(1,30)))
      bsSpineDF <- data.frame(parms=bsSpine)
      for(bb in 0:0){#(buffs[1] - 1)){
        for(eb in 0:0){#(buffs[2] - 1)){
          df3 <- chuck.zeros(buffs[1],buffs[2],bb,eb,df2)     # bb and ef = number of zeros + NA to keep, before and after, respectively.
          df3 <- df3[ order(df3$trapPositionID, df3$EndTime), ]  # get rid of first temporal records that are Not fishing.

          m <- 1
          repeat{
            if(df3$TrapStatus[m] == 'Not fishing'){          # this repeat structure allows the possibility that more than one Not fishing record is up top.
              df3 <- df3[-1,]
            } else {
              break
            }
          }
          df3 <- df3[order(df3$EndTime,decreasing=TRUE),]
          m <- 1
          repeat{
            if(df3$TrapStatus[m] == 'Not fishing'){          # this repeat structure allows the possibility that more than one Not fishing record is down at the bottom.
              df3 <- df3[-1,]
            } else {
              break
            }
          }
          df3 <- df3[order(df3$EndTime),]

          thisTrap[[buffs[2]*(bb) + (eb + 1)]] <- suppressWarnings( F.catch.model( df3 ) )#,error=function(e) e )  # df.and.fit is list of, $df2 contains data frame, $fit contains model, etc
          the.zero.fits[[match(trap,u.traps)]] <- thisTrap

          betas <- data.frame(values=unlist(the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$fit$coefficients))
          betas$parms <- rownames(betas)
          betasT <- data.frame(t(merge(bsSpineDF,betas,by=c('parms'),all.x=TRUE)))
          betasT <- betasT[,c('X1','X2','X13','X24','X26','X27','X28','X29','X30','X31','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X14','X15','X16','X17','X18','X19','X20','X21','X22','X23','X25')]
          colnames(betasT) <- bsSpine
          betasT <- betasT[-1,]

          beg.date <- head(the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$df2,1)$batchDate
          end.date <- tail(the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$df2,1)$batchDate
          thisIter <- cbind(betasT,beg.date,end.date,bZerosRem=buffs[1] - bb - 1,eZerosRem=buffs[2] - eb - 1,bZerosKept=bb,eZerosKept=eb)
          thisIter$AIC <- the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$fit$aic
          thisIter$nrow <- nrow(df3)
          thisIter$Nrow <- nrow(df2[df2$trapPositionID == trap,])
          allIter <- rbind(allIter,thisIter)
        }
      }
      rownames(allIter) <- NULL
      #write.csv(allIter,paste0(output.file,' - ',run.name,' - ',trap,' - allIter.csv'))
      winner <- allIter[allIter$bZerosKept == 0 & allIter$eZerosKept == 0,]    # see what we have after throwing out the zeros.

      winnerDate <- winner[,c('bZerosRem','eZerosRem','bZerosKept','eZerosKept','nrow','Nrow','beg.date','end.date')]
      winnerDate$trap <- trap
      winnerDate$origBeg.date <- origBeg.date
      winnerDate$origEnd.date <- origEnd.date
      allWinnerDates <- rbind(allWinnerDates,winnerDate)

      df4 <- the.zero.fits[[match(trap,u.traps)]][[  buffs[1]*buffs[2] - winner$bZerosRem*buffs[2] - winner$eZerosRem  ]]$df2    # something happens to not fishing periods here?
      df2 <- df2[order(df2$batchDate),]
      df5 <- df2[ (1 + winner$bZerosRem) : (nrow(df2) - winner$eZerosRem),]     # this nrow(df4) != nrow(df5) in general.  df2 after fitting model gets screwy rows
      #df2 <- df5        # put new df on track with previous code expectations.  jason comments out 1/26.  i think we still want to report the zero catches.  these are data.


      #jBaseTable <- tryCatch(plot_spline(trap,df2,thisTrap[[1]],file="spline.pdf",df3), error=function(e) e)  # assumes zero-zero for now -- feed the spline f'n thisTrap[[1]] to get correct df2 (df3)
      df.and.fit <- thisTrap[[1]]   # assumes zero-zero
    } else {
      # what we used to do.  we end up here when all records are thrown out.  ... and maybe when we impute on all days with data?
      df.and.fit <- suppressWarnings( F.catch.model( df2 ) )
      #jBaseTable <- tryCatch(plot_spline(trap,df2,df.and.fit,file="spline.pdf",df3), error=function(e) e)
    }
    # ------------ end jason generalizing ---------------------------------------------------------------------------------------------------------------------------------------------------



#     # keep the original in this format, with this structure, so all its dependencies continue to work as intended.  jason 12.4.2015.
#     df.and.fit <- suppressWarnings( F.catch.model( df2 ) )  # df.and.fit is list of, $df2 contains data frame, $fit contains model, etc
#      #jason.df.and.fit <<- df.and.fit
#
#     jBaseTable <- plot_spline(trap,catch.df,df.and.fit,file="spline.pdf")
#     allJBaseTable <- rbind(allJBaseTable,jBaseTable)

#     if( class(jBaseTable)[1] != "simpleError" ){
#       allJBaseTable <- rbind(allJBaseTable,jBaseTable)
#     }
    df <- rbind( df, df.and.fit$df2)


    catch.fits[[which(trap==u.traps)]] <- df.and.fit$fit
    X.miss[[which(trap==u.traps)]] <- df.and.fit$X.for.missings
    Gaps[[which(trap==u.traps)]] <- df.and.fit$gaps
    bDates.miss[[which(trap==u.traps)]] <- df.and.fit$batchDate.for.missings

    true.imp <- rbind(true.imp,df.and.fit$true.imp)
#    print(df.and.fit)
#    cat("in est_catch (hit return):")
#    readline()
}
# true.imp <<- true.imp
cat("in est_catch.r  DF")
print( tapply(df$batchDate, df$trapPositionID, range) )
cat("-------\n")

# ---- output underlying data of splines plot ---
#write.csv(allJBaseTable,paste0(output.file,"_",catch.df$FinalRun[1],"_",lsLabel,"_allJBaseTable.csv"))
# ---- end output underlying data of splines plot ---





# ----- jason adds 12/11/2015 so we have something to tell the est_efficiency program those traps that end up with zero fish,
# ----- but may have release data

allDates <- merge(data.frame(trap=u.traps),allWinnerDates,by=c('trap'),all.x=TRUE)


# ----- end jason add. --------------------------





#   Should probably save df into the Access data base for storage and potential use by others.


#   ---- Now that there are no gaps, sum within traps operating on a batch day, and all checks that occurred on a batch day.
ind <- list( batchDate=format(df$batchDate, "%Y-%m-%d"), trapPositionID=df$trapPositionID  )
est.catch <- tapply( df$n.tot, ind, sum )
p.imputed <- tapply( df$gamEstimated, ind, mean )

if(nrow(true.imp) > 0){
  est.catch1 <- data.frame(imputedCatch=tapply(true.imp$n.tot, list(true.imp$trapPositionID), sum))  # we need to tally up the imputed catch
  est.catch1$Traps <- rownames(est.catch1)
  rownames(est.catch1) <- NULL
} else {  # no imputation required for all trap splines
  est.catch1 <- NULL
}

est.catch2 <- tapply( df$n.Orig, ind, sum )                    # we need to tally up the assigned catch
est.catch2a<- tapply( df$n.Orig2, ind, sum )                   # we need to tally up the accounting assigned catch
est.catch3 <- tapply( df$n.Unassd, ind, sum )                  # we need to tally up the unassigned catch
est.catch4 <- tapply( df$halfConeAssignedCatch, ind, sum )     # we need to tally up the n.halfConeAdjAssd catch
est.catch5 <- tapply( df$halfConeUnassignedCatch, ind, sum )   # we need to tally up the n.halfConeAdjUnassd catch
est.catch6 <- tapply( df$assignedCatch, ind, sum )             # we need to tally up the n.fullConeAdjAssd catch
est.catch7 <- tapply( df$unassignedCatch, ind, sum )           # we need to tally up the n.fullConeAdjUnassd catch
est.catch8 <- tapply( df$modAssignedCatch, ind, sum )          # we need to tally up the n.adjUnassd catch
est.catch9 <- tapply( df$modUnassignedCatch, ind, sum )        # we need to tally up the n.adjAssd catch

tmp.est.catch <- est.catch   # save a copy of est.catch for counting traps later


#   ---- Un-matrix the results and put into a data frame

est.catch <- cbind( expand.grid( batchDate=dimnames(est.catch)[[1]], trapPositionID=dimnames(est.catch)[[2]]),
                catch=c(est.catch), imputed.catch=c(p.imputed) )
est.catch2 <- cbind( expand.grid( batchDate=dimnames(est.catch2)[[1]], trapPositionID=dimnames(est.catch2)[[2]]),
                    assdCatch=c(est.catch2), imputed.catch=c(p.imputed) )    # jason add 4/17/2015.  need to tally up assigned catch
est.catch2a<- cbind( expand.grid( batchDate=dimnames(est.catch2a)[[1]], trapPositionID=dimnames(est.catch2a)[[2]]),
                     assdCatchA=c(est.catch2a), imputed.catch=c(p.imputed) )    # jason add 1/14/2016.  need to tally up assigned catch
est.catch3 <- cbind( expand.grid( batchDate=dimnames(est.catch3)[[1]], trapPositionID=dimnames(est.catch3)[[2]]),
                     UnassdCatch=c(est.catch3), imputed.catch=c(p.imputed) )    # jason add 4/17/2015.  need to tally up unassigned catch
est.catch4 <- cbind( expand.grid( batchDate=dimnames(est.catch4)[[1]], trapPositionID=dimnames(est.catch4)[[2]]),
                     halfConeAssignedCatch=c(est.catch4), imputed.catch=c(p.imputed) )    # jason add 1/14/2016.  need to tally up halfConeAssignedCatch
est.catch5 <- cbind( expand.grid( batchDate=dimnames(est.catch5)[[1]], trapPositionID=dimnames(est.catch5)[[2]]),
                     halfConeUnassignedCatch=c(est.catch5), imputed.catch=c(p.imputed) )    # jason add 1/14/2016.  need to tally up halfConeUnassignedCatch
est.catch6 <- cbind( expand.grid( batchDate=dimnames(est.catch6)[[1]], trapPositionID=dimnames(est.catch6)[[2]]),
                     assignedCatch=c(est.catch6), imputed.catch=c(p.imputed) )    # jason add 1/14/2016.  need to tally up assignedCatch
est.catch7 <- cbind( expand.grid( batchDate=dimnames(est.catch7)[[1]], trapPositionID=dimnames(est.catch7)[[2]]),
                     unassignedCatch=c(est.catch7), imputed.catch=c(p.imputed) )    # jason add 1/14/2016.  need to tally up unassignedCatch
est.catch8 <- cbind( expand.grid( batchDate=dimnames(est.catch8)[[1]], trapPositionID=dimnames(est.catch8)[[2]]),
                     modAssignedCatch=c(est.catch8), imputed.catch=c(p.imputed) )    # jason add 1/14/2016.  need to tally up modAssignedCatch
est.catch9 <- cbind( expand.grid( batchDate=dimnames(est.catch9)[[1]], trapPositionID=dimnames(est.catch9)[[2]]),
                     modUnassignedCatch=c(est.catch9), imputed.catch=c(p.imputed) )    # jason add 1/14/2016.  need to tally up modUnassignedCatch



# checking that the correct number of halfConeAdj makes it through from the beginning.   delete later (1/14/2016)
# findit <- merge(est.catch4,check,by=c('batchDate','trapPositionID'),all.x=TRUE)
# findit$halfConeAdj <- ifelse(is.na(findit$halfConeAdj),0,findit$halfConeAdj)
# findit$prehalfConeAdj <- ifelse(is.na(findit$prehalfConeAdj),0,findit$prehalfConeAdj)
# findit[findit$halfConeAdj != findit$prehalfConeAdj,]



# get both total catch and assigned catch and unassigned catch and halfConeAdj.
est.catch <- merge(est.catch,est.catch2 ,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 4/17/2015 - join assumes full join, i.e., dim(of both dfs) same
est.catch <- merge(est.catch,est.catch3 ,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 5/20/2015 - join assumes full join, i.e., dim(of both dfs) same

est.catch <- merge(est.catch,est.catch2a,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 1/11/2016 - join assumes full join, i.e., dim(of both dfs) same
est.catch <- merge(est.catch,est.catch4 ,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 1/14/2016 - join assumes full join, i.e., dim(of both dfs) same
est.catch <- merge(est.catch,est.catch5 ,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 1/14/2016 - join assumes full join, i.e., dim(of both dfs) same
est.catch <- merge(est.catch,est.catch6 ,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 1/14/2016 - join assumes full join, i.e., dim(of both dfs) same
est.catch <- merge(est.catch,est.catch7 ,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 1/14/2016 - join assumes full join, i.e., dim(of both dfs) same
est.catch <- merge(est.catch,est.catch8 ,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 1/14/2016 - join assumes full join, i.e., dim(of both dfs) same
est.catch <- merge(est.catch,est.catch9 ,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 1/14/2016 - join assumes full join, i.e., dim(of both dfs) same
est.catch$batchDate <- as.POSIXct( as.character(est.catch$batchDate), "%Y-%m-%d", tz=time.zone)
est.catch <- est.catch[order(est.catch$trapPositionID,est.catch$batchDate),]





# more fish accounting
theSumsMiddle <- accounting(est.catch,"byTrap")
if(!is.null(est.catch1)){
  theSumsMiddle <<- merge(theSumsMiddle,est.catch1,by=c('Traps'),all.x=TRUE)
}


# 1/28/2016 -- all of this moved to new function est_catch_trapN, so as to get correct Ntraps per day, which
# need fishing days with zero catch.
# # need to keep track which days went into the algorithm, for bootstrapping.
# est.catch$catchNA <- est.catch$catch
# est.catch$catch <- ifelse(is.na(est.catch$catch),0,est.catch$catch)
#
# trapSeasons <- tapply(est.catch$catch, est.catch$trapPositionID, function(x){
#               seas <- which(!is.na(x))
#               f <- min(seas)
#               l <- max(seas)
#               ans <- rep(FALSE,length(x))
#               ans[f:l] <- TRUE
#               ans
#           })
#
# trapSeasons <- unlist(trapSeasons)
#
# est.catch <- est.catch[ trapSeasons, ]
#
#
# #   ---- Before we un-matrix-ized est.catch, we saved a copy so we could compute number of traps operating each day because it is easier.
# #        But, gaps are filled.  Take back out the gaps when counting.
# trapSeasons <- matrix( trapSeasons, nrow=nrow(tmp.est.catch), ncol=ncol(tmp.est.catch) )
# for( i in 1:length(bDates.miss) ){   # !is.null(bDates.miss) <-- put back for now -- null happens when we have one catch day that req'd imputation (maybe if all catch days require imputation)
#     tmp.est.catch[ trapSeasons[,i] & is.na(tmp.est.catch[,i]), i ] <- 0  # Add days when trap was operating but no trap visit.  I.e., trap operated for >24 hours without a check.  Must do this first, before next line.
#     tmp.est.catch[dimnames(tmp.est.catch)[[1]] %in% format(bDates.miss[[i]]), i] <- NA   #  blank out the gaps.
# }
# trapsOperating <- apply(tmp.est.catch, 1, function(x){sum(!is.na(x))} )
# trapsOperating <- data.frame( batchDate=names(trapsOperating), nTrapsOperating=trapsOperating, stringsAsFactors=F )
# trapsOperating$batchDate <- as.POSIXct( as.character(trapsOperating$batchDate), "%Y-%m-%d", tz=time.zone)

trapsOperating <- F.est.catch.trapN(catch.df, plot=FALSE, plot.file="raw_catch.pdf" )



# we have some missing days in est.catch.  fix those now.
# so deleting zeros screwed things up.
# possibly missing days due to deleted imputed period at the end of a period, e.g., trap 42050 rbdd may 2002.
# use collected data to rebuild the days we would have, i.e., reconstruct est.catch as it used to be, batchDate-wise
# jason finagling around here...
est.catch.fake <- NULL
for(trap in dateFramer$trapPositionID){
  theDays <- dateFramer[dateFramer$trapPositionID == trap,]$origEnd.date - dateFramer[dateFramer$trapPositionID == trap,]$origBeg.date + 1
  theDaysA <- seq(dateFramer[dateFramer$trapPositionID == trap,]$origBeg.date,dateFramer[dateFramer$trapPositionID == trap,]$origEnd.date,length.out=theDays)   # gets enddate correct
  theDaysB <- seq(dateFramer[dateFramer$trapPositionID == trap,]$origBeg.date,dateFramer[dateFramer$trapPositionID == trap,]$origEnd.date,by=60*60*22)          # get dst awkwardness correct -- note the 22 -- hit all days at least once
  theDays <- unique(strptime(c(theDaysA,theDaysB),"%F",tz=time.zone))                                                                                           # put together, map to days, get unique
  lil.est.catch.fake <- data.frame(batchDate=theDays,trapPositionID=trap)
  est.catch.fake <- rbind(est.catch.fake,lil.est.catch.fake)
}
est.catch <- merge(est.catch.fake,est.catch,by=c('batchDate','trapPositionID'),all.x=TRUE)

# make these other variables zero...otherwise fish accounting fails later on.
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












#tmp.est.catch <<- tmp.est.catch
#tmp.trapsOperating <<- trapsOperating

#readline()

#   If a trap runs without checks for 48 hours say, it runs over two batch dates.  When this happens,
#   the above statements result in a NA for catch on the day it skipped.  The real (non-imputed) catch
#   for these days is 0.  Replace these NA's with zeros.  (But, perhaps these lines should be tossed...)
#ind <- is.na(est.catch$catch)
#est.catch$catch[ ind ] <- 0
#est.catch$imputed.catch[ ind ] <- 0


#   Assign attributes for plotting
u.ss.rows <- !duplicated(catch.df$trapPositionID)
u.ss <- catch.df$trapPositionID[u.ss.rows]
u.ss.name <- catch.df$TrapPosition[u.ss.rows]
ord <- order( u.ss )
u.ss <- u.ss[ ord ]
u.ss.name <- u.ss.name[ ord ]

#   Make a plot if called for (I don't use parameter 'plot', apparently.
# jason.est.catch <<- est.catch


# jason add:  merge in TrapPosition so that graphical output can show text labels instead of computer code numbers.
# est.catch <- merge(jason.est.catch,unique(catch.dff[,c('trapPositionID','TrapPosition')]),by=c('trapPositionID'),all.x=TRUE)  delete later
est.catch <- merge(est.catch,unique(catch.df[,c('trapPositionID','TrapPosition')]),by=c('trapPositionID'),all.x=TRUE)
attr(est.catch, "site.name") <- catch.df$siteName[1]
attr(est.catch, "subsites") <- data.frame(subSiteID = u.ss, subSiteName=u.ss.name)
attr(est.catch, "run.name") <- run.name#catch.df$FinalRun[1]
attr(est.catch, "life.stage" ) <- catch.df$lifeStage[1]
attr(est.catch, "species.name") <- "Chinook Salmon"

# jason adds 4/20/2015 to itemize out the different types of catches

# collapse imputed to day ... could be two per day if both traps non-functioning
if(nrow(true.imp) > 0){
  true.imp$shortdate <- strftime(true.imp$batchDate, format="%Y-%m-%d")
  true.imp.sum <- aggregate(true.imp$n.tot,by=list(true.imp$shortdate,true.imp$trapPositionID), FUN=sum)
  true.imp.sum$batchDate <- as.POSIXct( as.character(true.imp.sum$Group.1), "%Y-%m-%d", tz=time.zone)
  true.imp.sum$Group.1 <- NULL
  names(true.imp.sum)[names(true.imp.sum) == 'V1'] <- 'imputedCatch' #'n.tot'
  names(true.imp.sum)[names(true.imp.sum) == 'Group.2'] <- 'trapPositionID'
} else {  # no imputed days.  make a fake dataframe so code continues to run.
  true.imp.sum <- data.frame(trapPositionID=u.traps,imputedCatch=rep(NA,length(u.traps)),batchDate=rep(est.catch$batchDate[1],length(u.traps)))
  true.imp <- true.imp.sum
  names(true.imp)[names(true.imp) == 'imputedCatch'] <- 'n.tot'   # making a fake df for use in est.passage joins.
}

# new 1/15/2016
est.catch$assdCatch <- est.catch$UnassdCatch <- est.catch$assdCatchA <- NULL   # what are these ...?
masterCatch <- merge(est.catch,true.imp.sum,by=c('trapPositionID','batchDate'),all.x=TRUE)

# new way -- 1/15/2016
masterCatch$imputedCatch <- ifelse(is.na(masterCatch$imputedCatch),0,masterCatch$imputedCatch)
masterCatch$assignedCatch <- ifelse(is.na(masterCatch$assignedCatch),0,masterCatch$assignedCatch)
masterCatch$totalEstimatedCatch <- masterCatch$imputedCatch + masterCatch$modAssignedCatch + masterCatch$modUnassignedCatch

# ?? old 1/15/2016
# # collpase catch counts in catch.df over day, for both n.tot (totalCatch) and n.Orig (assigned)
# catch.df$shortdate  <- strftime(catch.df$batchDate, format="%Y-%m-%d")
# catch.df.reduced <- catch.df[catch.df$TrapStatus == 'Fishing',]
# catch.df.n.tot.sum <- aggregate(catch.df.reduced$n.tot,by=list(catch.df.reduced$shortdate,catch.df.reduced$trapPositionID), FUN=sum)
# catch.df.n.Orig.sum <- aggregate(catch.df.reduced$n.Orig,by=list(catch.df.reduced$shortdate,catch.df.reduced$trapPositionID), FUN=sum)
# colnames(catch.df.n.tot.sum) <- c('shortdate','trapPositionID','n.tot')
# colnames(catch.df.n.Orig.sum) <- c('shortdate','trapPositionID','n.Orig')
# catch.df.both.sum <- merge(catch.df.n.tot.sum,catch.df.n.Orig.sum,by=c('shortdate','trapPositionID'))
# catch.df.both.sum$batchDate <- as.POSIXct( as.character(catch.df.both.sum$shortdate), "%Y-%m-%d", tz=time.zone)
# catch.df.both.sum$shortdate <- NULL
#
#
# # full join here to bring in assd data data was observed but not randomly selected...i think
# masterCatch <- merge(preCatch,catch.df.both.sum,by=c('trapPositionID','batchDate'),all.x=TRUE,all.y=TRUE)
# #masterCatch$unassdCatch <- masterCatch$n.tot.y - masterCatch$n.Orig
#
# masterCatch <- masterCatch[, !(names(masterCatch) %in% c('assdCatch'))]    # we have NA for this var for batchDates with imputation -- its not good enough
#
# names(masterCatch)[names(masterCatch) == 'n.tot.x'] <- 'imputedCatch'
# names(masterCatch)[names(masterCatch) == 'catch'] <- 'totalCatch'   # this is trent's old catch col
# names(masterCatch)[names(masterCatch) == 'n.Orig'] <- 'assdCatch'
#
# masterCatch <- masterCatch[, !(names(masterCatch) %in% c('trapVisitID','n.tot.y'))]




#   If a trap runs without checks for 48 hours say, it runs over two batch dates.  When this happens,
#   the above statements result in a NA for catch on the day it skipped.  The real (non-imputed) catch
#   for these days is 0.  Replace these NA's with zeros.  (But, perhaps these lines should be tossed...)
#   jason -- 1/15/2016 -- i think i have everything i need up to this point...so toss.
#getMinDate1 <- masterCatch[!is.na(masterCatch$assdCatch),]
#getMinDate2 <- aggregate(getMinDate1[,names(getMinDate1) %in% c('trapPositionID','batchDate')],list(getMinDate1$trapPositionID),FUN=head, 1)
#getMinDate2$minDate <- as.POSIXct( as.character(getMinDate2$batchDate), "%Y-%m-%d", tz=time.zone)

#masterCatch <- merge(masterCatch,getMinDate2[,c('trapPositionID','minDate')],by='trapPositionID',all.x=TRUE)






# old?? -- 1/15/2016
# masterCatch$indOld <- !is.na(masterCatch$imputedCatch) | !is.na(masterCatch$assdCatch)   # 4/20/2015 reproduce output from before
# # masterCatch$assdCatch[ ind ] <- 0
# #masterCatch$imputed.catch[ ind ] <- 0
#
# # # adaptation of trent code from before, where he somehow had data only after the first catch day
# # ind1 <- (masterCatch$minDate <= masterCatch$batchDate) & is.na(masterCatch$assdCatch)   # 4/20/2015 reproduce output from before
# # masterCatch$assdCatch[ ind1 ] <- 0
# # ind2 <- (masterCatch$minDate <= masterCatch$batchDate) & is.na(masterCatch$imputed.catch)   # 4/20/2015 reproduce output from before
# # # masterCatch$imputed.catch[ ind2 ] <- 0
#
# # make sure that totalCatch includes unassdCatch
# masterCatch$totalCatch <- ifelse(masterCatch$UnassdCatch > 0 & is.na(masterCatch$totalCatch),masterCatch$UnassdCatch,masterCatch$totalCatch)
#
# # make sure that totalCatch includes imputedCatch -- it was this way before, but breaking things up broke everything
# masterCatch$totalCatch <- ifelse(masterCatch$imputedCatch > 0 & is.na(masterCatch$totalCatch),masterCatch$imputedCatch,masterCatch$totalCatch)
#
# # make sure that totalCatch includes assdCatch -- it was this way before, but breaking things up broke everything
# masterCatch$totalCatch <- ifelse(masterCatch$assdCatch >= 0 & is.na(masterCatch$totalCatch),masterCatch$assdCatch,masterCatch$totalCatch)

# make sure that imputed.catch is a 1 if no other source of fish for that day.
# masterCatch$imputed.catch <- ifelse(masterCatch$imputedCatch >0 & is.na(masterCatch$imputed.catch) & is.na(masterCatch$UnassdCatch) & is.na(masterCatch$assdCatch),1.0,masterCatch$imputed.catch)


attr(masterCatch, "site.name") <- catch.df$siteName[1]
attr(masterCatch, "subsites") <- data.frame(subSiteID = u.ss, subSiteName=u.ss.name)
attr(masterCatch, "run.name") <- run.name#catch.df$FinalRun[1]
attr(masterCatch, "life.stage" ) <- catch.df$lifeStage[1]
attr(masterCatch, "species.name") <- "Chinook Salmon"

if( !is.na(plot.file) ) {
    out.fn <- F.plot.catch.model( masterCatch, file=plot.file )    # change f'n input from est.catch to masterCatch
} else {
    out.fn <- NULL
}

cat("Catch estimation complete...\n")


ans <- list(catch=est.catch, fits=catch.fits, X.miss=X.miss, gaps=Gaps, bDates.miss=bDates.miss, trapsOperating=trapsOperating, true.imp=true.imp, allDates=allDates)

attr(ans, "out.fn.list") <- out.fn

ans

}
