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
u.traps <- unique( catch.df$trapPositionID )
catch.fits <- X.miss <- Gaps <- bDates.miss <- vector("list", length(u.traps))   # lists to contain thing to save for bootstrapping 
names(catch.fits) <- u.traps
# catch.dff <<- catch.df   # save a copy for debugging





# # ----------- jason adds 12.04.2015 to house results of buffer analysis. ---------------------------
# 
# the.zero.fits <- vector("list",length(unique(catch.df.ls$trapPositionID)))
# allWinnerDates <- NULL   # jason adds
# 
# # ----------- jason adds 12.04.2015 to house results of buffer analysis. ---------------------------



for( trap in u.traps ){
    cat(paste("==== Catch model for trapPositionID", trap, "========\n" ))

    df2 <- catch.df[catch.df$trapPositionID == trap,]

    #   Impute a value for the gaps
    #   When df2 comes back from F.catch.model, it has extra lines in it.  One extra line for each 24 hour period in the 
    #   gaps that were bigger than max.ok.gap. If gap = 3 days, there will be 3 extra lines.
    #   sampleStart and sampleEnd for each of the new lines are defined so that no gap appears  now.  Variable
    #   'gamEstimated' is true for these periods. Batch date is assigned based on sampleEnd, as usual. 
    #   On return, there is a value or imputed value for each day from start of season to end of season.
    
    
    
    
#     # --------------- jason generalizes to run catch models over all possible buffering zero iterations.  12.4.2015.-----------------------------------------------
#     buffs <- max.buff.days(df2)       # first value is zeros + NA in the beg, last is zeros + NA in the end. 3rd value is length of n.tot vector. 
#     
#     origBeg.date <- min(df2$batchDate)
#     origEnd.date <- max(df2$batchDate)
#     
#     if( !(buffs[1] == buffs[3] & buffs[2] == buffs[3]) ){     # prob only need to check 1 of these conditions
#       # 1st, assume both buffs are non-zero.
#       iters <- buffs[1]*buffs[2]
#       thisTrap <- vector("list",iters)
#       allIter <- NULL
#       bsSpine <- c('(Intercept)',paste0(rep('bs.sEnd',30),seq(1,30)))
#       bsSpineDF <- data.frame(parms=bsSpine)
#       for(bb in 0:0){#(buffs[1] - 1)){
#         for(eb in 0:0){#(buffs[2] - 1)){
#           df3 <- chuck.zeros(buffs[1],buffs[2],bb,eb,df2)     # bb and ef = number of zeros + NA to keep, before and after, respectively.  
#           thisTrap[[buffs[2]*(bb) + (eb + 1)]] <- suppressWarnings( F.catch.model( df3 ) )  # df.and.fit is list of, $df2 contains data frame, $fit contains model, etc
#           the.zero.fits[[match(trap,u.traps)]] <- thisTrap
#           
#           betas <- data.frame(values=unlist(the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$fit$coefficients))
#           betas$parms <- rownames(betas)
#           betasT <- data.frame(t(merge(bsSpineDF,betas,by=c('parms'),all.x=TRUE)))
#           betasT <- betasT[,c('X1','X2','X13','X24','X26','X27','X28','X29','X30','X31','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X14','X15','X16','X17','X18','X19','X20','X21','X22','X23','X25')]
#           colnames(betasT) <- bsSpine
#           betasT <- betasT[-1,]
#         
#           beg.date <- head(the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$df2,1)$batchDate
#           end.date <- tail(the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$df2,1)$batchDate
#           thisIter <- cbind(betasT,beg.date,end.date,bZerosRem=buffs[1] - bb - 1,eZerosRem=buffs[2] - eb - 1,bZerosKept=bb,eZerosKept=eb)
#           thisIter$AIC <- the.zero.fits[[match(trap,u.traps)]][[buffs[2]*(bb) + (eb + 1)]]$fit$aic
#           thisIter$nrow <- nrow(df3)
#           thisIter$Nrow <- nrow(df2[df2$trapPositionID == trap,])
#           allIter <- rbind(allIter,thisIter)
#         }
#       }
#       rownames(allIter) <- NULL
#       #write.csv(allIter,paste0(output.file,' - ',run.name,' - ',trap,' - allIter.csv'))
#       winner <- allIter[allIter$bZerosKept == 0 & allIter$eZerosKept == 0,]    # see what we have after throwing out the zeros.
#       
#       winnerDate <- winner[,c('bZerosRem','eZerosRem','bZerosKept','eZerosKept','nrow','Nrow','beg.date','end.date')]
#       winnerDate$trap <- trap
#       winnerDate$origBeg.date <- origBeg.date
#       winnerDate$origEnd.date <- origEnd.date
#       allWinnerDates <- rbind(allWinnerDates,winnerDate)
#       
#       df4 <- the.zero.fits[[match(trap,u.traps)]][[  buffs[1]*buffs[2] - winner$bZerosRem*buffs[2] - winner$eZerosRem  ]]$df2    # something happens to not fishing periods here?
#       df2 <- df2[order(df2$batchDate),]
#       df5 <- df2[ (1 + winner$bZerosRem) : (nrow(df2) - winner$eZerosRem),]     # this nrow(df4) != nrow(df5) in general.  df2 after fitting model gets screwy rows
#       df2 <- df5        # put new df on track with previous code expectations.
#     }
#     # ------------ end jason generalizing ---------------------------------------------------------------------------------------------------------------------------------------------------
    
    
  
    # keep the original in this format, with this structure, so all its dependencies continue to work as intended.  jason 12.4.2015.
    df.and.fit <- suppressWarnings( F.catch.model( df2 ) )  # df.and.fit is list of, $df2 contains data frame, $fit contains model, etc
     #jason.df.and.fit <<- df.and.fit
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




# # ----- jason adds 12/11/2015 so we have something to tell the est_efficiency program those traps that end up with zero fish,
# # ----- but may have release data
# 
# allDates <- merge(data.frame(trap=u.traps),allWinnerDates,by=c('trap'),all.x=TRUE)
# 
# 
# # ----- end jason add. --------------------------





#   Should probably save df into the Access data base for storage and potential use by others. 


#   ---- Now that there are no gaps, sum within traps operating on a batch day, and all checks that occurred on a batch day.
ind <- list( batchDate=format(df$batchDate, "%Y-%m-%d"), trapPositionID=df$trapPositionID  )
est.catch <- tapply( df$n.tot, ind, sum )
p.imputed <- tapply( df$gamEstimated, ind, mean )

est.catch2 <- tapply( df$n.Orig, ind, sum )       # we need to tally up the assigned catch
est.catch3 <- tapply( df$n.Unassd, ind, sum )     # we need to tally up the unassigned catch


tmp.est.catch <- est.catch   # save a copy of est.catch for counting traps later


#   ---- Un-matrix the results and put into a data frame

est.catch <- cbind( expand.grid( batchDate=dimnames(est.catch)[[1]], trapPositionID=dimnames(est.catch)[[2]]), 
                catch=c(est.catch), imputed.catch=c(p.imputed) )
est.catch2 <- cbind( expand.grid( batchDate=dimnames(est.catch2)[[1]], trapPositionID=dimnames(est.catch2)[[2]]), 
                    assdCatch=c(est.catch2), imputed.catch=c(p.imputed) )    # jason add 4/17/2015.  need to tally up assigned catch
est.catch3 <- cbind( expand.grid( batchDate=dimnames(est.catch3)[[1]], trapPositionID=dimnames(est.catch3)[[2]]), 
                     UnassdCatch=c(est.catch3), imputed.catch=c(p.imputed) )    # jason add 4/17/2015.  need to tally up unassigned catch

# get both total catch and assigned catch and unassigned catch.
est.catch <- merge(est.catch,est.catch2,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 4/17/2015 - join assumes full join, i.e., dim(of both dfs) same
est.catch <- merge(est.catch,est.catch3,by=c('batchDate','trapPositionID','imputed.catch'))     # jason add 5/20/2015 - join assumes full join, i.e., dim(of both dfs) same
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
true.imp$shortdate <- strftime(true.imp$batchDate, format="%Y-%m-%d")
true.imp.sum <- aggregate(true.imp$n.tot,by=list(true.imp$shortdate,true.imp$trapPositionID), FUN=sum)
true.imp.sum$batchDate <- as.POSIXct( as.character(true.imp.sum$Group.1), "%Y-%m-%d", tz=time.zone)  
true.imp.sum$Group.1 <- NULL
names(true.imp.sum)[names(true.imp.sum) == 'V1'] <- 'n.tot'  
names(true.imp.sum)[names(true.imp.sum) == 'Group.2'] <- 'trapPositionID'  


preCatch <- merge(est.catch,true.imp.sum,by=c('trapPositionID','batchDate'),all.x=TRUE)

# collpase catch counts in catch.df over day, for both n.tot (totalCatch) and n.Orig (assigned)
catch.df$shortdate  <- strftime(catch.df$batchDate, format="%Y-%m-%d")
catch.df.reduced <- catch.df[catch.df$TrapStatus == 'Fishing',]
catch.df.n.tot.sum <- aggregate(catch.df.reduced$n.tot,by=list(catch.df.reduced$shortdate,catch.df.reduced$trapPositionID), FUN=sum)
catch.df.n.Orig.sum <- aggregate(catch.df.reduced$n.Orig,by=list(catch.df.reduced$shortdate,catch.df.reduced$trapPositionID), FUN=sum)
colnames(catch.df.n.tot.sum) <- c('shortdate','trapPositionID','n.tot')
colnames(catch.df.n.Orig.sum) <- c('shortdate','trapPositionID','n.Orig')
catch.df.both.sum <- merge(catch.df.n.tot.sum,catch.df.n.Orig.sum,by=c('shortdate','trapPositionID'))
catch.df.both.sum$batchDate <- as.POSIXct( as.character(catch.df.both.sum$shortdate), "%Y-%m-%d", tz=time.zone)  
catch.df.both.sum$shortdate <- NULL


# full join here to bring in assd data data was observed but not randomly selected...i think
masterCatch <- merge(preCatch,catch.df.both.sum,by=c('trapPositionID','batchDate'),all.x=TRUE,all.y=TRUE)
#masterCatch$unassdCatch <- masterCatch$n.tot.y - masterCatch$n.Orig

masterCatch <- masterCatch[, !(names(masterCatch) %in% c('assdCatch'))]    # we have NA for this var for batchDates with imputation -- its not good enough

names(masterCatch)[names(masterCatch) == 'n.tot.x'] <- 'imputedCatch'   
names(masterCatch)[names(masterCatch) == 'catch'] <- 'totalCatch'   # this is trent's old catch col
names(masterCatch)[names(masterCatch) == 'n.Orig'] <- 'assdCatch'  

masterCatch <- masterCatch[, !(names(masterCatch) %in% c('trapVisitID','n.tot.y'))]

#   If a trap runs without checks for 48 hours say, it runs over two batch dates.  When this happens, 
#   the above statements result in a NA for catch on the day it skipped.  The real (non-imputed) catch 
#   for these days is 0.  Replace these NA's with zeros.  (But, perhaps these lines should be tossed...)

getMinDate1 <- masterCatch[!is.na(masterCatch$assdCatch),]
getMinDate2 <- aggregate(getMinDate1[,names(getMinDate1) %in% c('trapPositionID','batchDate')],list(getMinDate1$trapPositionID),FUN=head, 1)
getMinDate2$minDate <- as.POSIXct( as.character(getMinDate2$batchDate), "%Y-%m-%d", tz=time.zone)  

masterCatch <- merge(masterCatch,getMinDate2[,c('trapPositionID','minDate')],by='trapPositionID',all.x=TRUE)

masterCatch$indOld <- !is.na(masterCatch$imputedCatch) | !is.na(masterCatch$assdCatch)   # 4/20/2015 reproduce output from before
# masterCatch$assdCatch[ ind ] <- 0
#masterCatch$imputed.catch[ ind ] <- 0

# # adaptation of trent code from before, where he somehow had data only after the first catch day
# ind1 <- (masterCatch$minDate <= masterCatch$batchDate) & is.na(masterCatch$assdCatch)   # 4/20/2015 reproduce output from before
# masterCatch$assdCatch[ ind1 ] <- 0
# ind2 <- (masterCatch$minDate <= masterCatch$batchDate) & is.na(masterCatch$imputed.catch)   # 4/20/2015 reproduce output from before
# masterCatch$imputed.catch[ ind2 ] <- 0


# make sure that totalCatch includes unassdCatch
masterCatch$totalCatch <- ifelse(masterCatch$UnassdCatch > 0 & is.na(masterCatch$totalCatch),masterCatch$UnassdCatch,masterCatch$totalCatch)

# make sure that totalCatch includes imputedCatch -- it was this way before, but breaking things up broke everything
masterCatch$totalCatch <- ifelse(masterCatch$imputedCatch > 0 & is.na(masterCatch$totalCatch),masterCatch$imputedCatch,masterCatch$totalCatch)

# make sure that totalCatch includes assdCatch -- it was this way before, but breaking things up broke everything
masterCatch$totalCatch <- ifelse(masterCatch$assdCatch >= 0 & is.na(masterCatch$totalCatch),masterCatch$assdCatch,masterCatch$totalCatch)

# make sure that imputed.catch is a 1 if no other source of fish for that day.
masterCatch$imputed.catch <- ifelse(masterCatch$imputedCatch >0 & is.na(masterCatch$imputed.catch) & is.na(masterCatch$UnassdCatch) & is.na(masterCatch$assdCatch),1.0,masterCatch$imputed.catch)


attr(masterCatch, "site.name") <- catch.df$siteName[1]
attr(masterCatch, "subsites") <- data.frame(subSiteID = u.ss, subSiteName=u.ss.name)
attr(masterCatch, "run.name") <- run.name#catch.df$FinalRun[1]
attr(masterCatch, "life.stage" ) <- catch.df$lifeStage[1]
attr(masterCatch, "species.name") <- "Chinook Salmon"


#write.csv(masterCatch,'C:/Users/jmitchell/Desktop/masterCatch.csv')


if( !is.na(plot.file) ) {
    out.fn <- F.plot.catch.model( masterCatch, file=plot.file )    # change f'n input from est.catch to masterCatch
} else {
    out.fn <- NULL
}

cat("Catch estimation complete...\n")


ans <- list(catch=est.catch, fits=catch.fits, X.miss=X.miss, gaps=Gaps, bDates.miss=bDates.miss, trapsOperating=trapsOperating, true.imp=true.imp)#, allDates=allDates)

attr(ans, "out.fn.list") <- out.fn

ans

}
