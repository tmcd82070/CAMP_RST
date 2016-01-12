F.est.passage <- function( catch.df, release.df, summarize.by, file.root, ci ){
#
#   Compute passage estimates.
#
#   Input:
#   catch.df = data frame with one row per trapvisitID for a particular FinalRun and lifeStage
#       That is, catch.df has only a single run X lifestage combination
#   release.df = data frame resulting from call to F.get.release.data.  Contains info on efficiency.
#   summarize.by = string specifying how to sum passage estimates.  valid values
#       are "day", "week", "month", "year".
#   file.root = root of file name for graphics files
#  
  
# catch.df <- catch.df.ls
# release.df <- release.df
# summarize.by <- by
# file.root <- out.fn.root
# ci <- ci
#
#   Output,
#   A data frame containing date, passage estimate, and SE of passage estimate.
# 
  
catch.df.sites <- unique(catch.df[,c('trapPositionID','TrapPosition')])                     # jason add
colnames(catch.df.sites) <- c('subSiteID','subSiteName')                                    # jason add
#catch.df$n.Orig <- ifelse(is.na(catch.df$n.Orig) & catch.df$TrapStatus == 'Fishing',0,catch.df$n.Orig)   # jason add -- 4/15/2015 delete 5/20/2015.  do with other 0 overwrite in passage.R

time.zone <- get("time.zone", env=.GlobalEnv )

f.banner <- function( x ){

    cat("\n")
    cat(paste(rep("=",50), collapse="")); 
    cat(x); 
    cat(paste(rep("=",50), collapse="")); 
    cat("\n")
}

f.banner(" F.est.passage - START ")

#   This keeps track of the files produced
out.fn.list <- NULL

#   retrieve the progress bar
usepb <- exists( "progbar", where=.GlobalEnv )

# jason add: this data frame has the raw unmarked counts of catch.  note that rows with missing data for certain days, i.e., for which imputation
# occurs also appear as line items here.  so, to get catch, for different trapPositionID/subSiteID, summarise and add togeter (b/c some
# days have more than one record).  brings back more dates than ultimately wanted; let merge below (after grand.df) take care of which
# to keep.    
jason.catch2.df <- catch.df[,c('trapVisitID','batchDate','trapPositionID','n.Orig')]
jason.catch3.df <- data.frame(with(jason.catch2.df,tapply(n.Orig, list(batchDate,trapPositionID), sum, na.rm=T )))
jason.catch4.df <- na.omit(reshape(jason.catch3.df,idvar='batchDate',ids=row.names(jason.catch3.df),times=names(jason.catch3.df),timevar='trapPositionID',varying=list(names(jason.catch3.df)),direction='long'))
colnames(jason.catch4.df)[2] <- 'rawCatch'
jason.catch4.df$trapPositionID <- as.character(substr(jason.catch4.df$trapPositionID,2,nchar(jason.catch4.df$trapPositionID)))
jason.catch4.df$batchDate <- as.POSIXct(jason.catch4.df$batchDate,time.zone)

# jason 4/15/2015, do the same thing as above, but with n.tot.  sloppy to do this twice like this, but i know the above works.
jason.totCatch2.df <- catch.df[,c('trapVisitID','batchDate','trapPositionID','n.tot')]
jason.totCatch3.df <- data.frame(with(jason.totCatch2.df,tapply(n.tot, list(batchDate,trapPositionID), sum, na.rm=T )))
jason.totCatch4.df <- na.omit(reshape(jason.totCatch3.df,idvar='batchDate',ids=row.names(jason.totCatch3.df),times=names(jason.totCatch3.df),timevar='trapPositionID',varying=list(names(jason.totCatch3.df)),direction='long'))
colnames(jason.totCatch4.df)[2] <- 'inflatedCatch'
jason.totCatch4.df$trapPositionID <- as.character(substr(jason.totCatch4.df$trapPositionID,2,nchar(jason.totCatch4.df$trapPositionID)))
jason.totCatch4.df$batchDate <- as.POSIXct(jason.totCatch4.df$batchDate,time.zone)

#   ------------------------------------------------------------------
#   Estimate capture for every day of season.  Return value is
#   data frame with columns $batchDate and $catch.  
#   By default, this produces one graph in a pdf.  Turn this off with plot=F in call.
#       catch.and.fits has components $catch, $fits, $X.miss, $gaps, $bDates.miss, and $trapsOperating
catch.and.fits <- F.est.catch( catch.df, plot=TRUE, plot.file=file.root )
if(usepb){
    progbar <- get( "progbar", pos=.GlobalEnv )
    tmp <- getWinProgressBar(progbar)
    setWinProgressBar(progbar, (2*tmp + 1)/3 )
}
catch <- catch.and.fits$catch

  # the catch dataframe in this list has the imputed values already overwriting the original numbers
jason.catch.and.fits2.df <- catch.and.fits$true.imp
jason.catch.and.fits3.df <- data.frame(with(jason.catch.and.fits2.df,tapply(n.tot, list(batchDate,trapPositionID), sum, na.rm=T )))
jason.catch.and.fits4.df <- na.omit(reshape(jason.catch.and.fits3.df,idvar='batchDate',ids=row.names(jason.catch.and.fits3.df),times=names(jason.catch.and.fits3.df),timevar='trapPositionID',varying=list(names(jason.catch.and.fits3.df)),direction='long'))
colnames(jason.catch.and.fits4.df)[2] <- 'ImputedCatch'
jason.catch.and.fits4.df$trapPositionID <- as.character(substr(jason.catch.and.fits4.df$trapPositionID,2,nchar(jason.catch.and.fits4.df$trapPositionID)))
jason.catch.and.fits4.df$batchDate <- as.POSIXct(jason.catch.and.fits4.df$batchDate,time.zone)



out.fn.list <- c(out.fn.list, attr(catch.and.fits, "out.fn.list"))

#catch.fits <- catch.and.fits$fits  # fits are needed for variance computation
#print(catch[1:20,])

#   ------------------------------------------------------------------
#   Estimate trap efficiency for every batchDate of season.  Return value is
#   data frame with columns $batchDate and $eff.  
#   If plot=T, this produces a graph in a pdf.

# ------ old as of 12/11/2015 -----------------------------
f.banner(" Efficiency estimation ")
bd <- sort( unique(catch$batchDate) )
# --------------------------------------------------------


# # ----- jason adds 12/11/2015 so that release.df has info on adjusted beg and end fishing days, for each trap
# 
# allDates <- catch.and.fits$allDates
# release.df <- merge(release.df,allDates,by.x=c('trapPositionID'),by.y=c('trap'),all.x=TRUE)
# 
# f.banner("Efficiency estimation ")
# bd <- strptime(sort( seq(as.Date(min(na.omit(release.df$origBeg.date),unique(catch$batchDate))),as.Date(max(na.omit(release.df$origEnd.date),unique(catch$batchDate))),"days")),format="%F",tz=time.zone)
# # getting the bd to work here was tricky, due to time zones, etc.  
# # ----- jason ends this new section -----------------------------------------------------------------




eff.and.fits <- F.est.efficiency( release.df, bd, method=3, df=3, plot=TRUE, plot.file=file.root )
if(usepb){
    tmp <- getWinProgressBar(progbar)
    setWinProgressBar(progbar, (2*tmp + 1)/3 )
}
efficiency <- eff.and.fits$eff

out.fn.list <- c(out.fn.list, attr(eff.and.fits, "out.fn.list"))


if( all(is.na(efficiency[1,])) ){
    #   Something is wrong with efficiency data. Make an empty efficiency data frame
    efficiency <- data.frame( trapPositionID=catch$trapPositionID, batchDate=catch$batchDate, efficiency=rep(NA, nrow(catch)))
    warning("Zero efficiency")
}
    

#   could do this n <- data.base( catch, efficiency=efficiency$efficiency, gam.estimated.eff=efficiency$gam.estimated ) 
#   to produce a data frame of values that go into estimator, one line per batchDate

#   ------------------------------------------------------------------
#   Now, estimate passage
if( any(ind <- !is.na(efficiency$efficiency) & (efficiency$efficiency <= 0)) ){    # shouldn't happen that efficiency <= 0, but just in case.  This also gives us a way to exclude days (just set efficiency <= 0)
    efficiency$efficiency[ind] <- NA
}

#   First merge catch and efficiency data frames

catch$batchDay <- format(catch$batchDate, "%Y-%m-%d")
catch$trapPositionID <- as.character(catch$trapPositionID)
efficiency$batchDay <- format(efficiency$batchDate, "%Y-%m-%d")
efficiency$trapPositionID <- as.character(efficiency$trapPositionID)
efficiency <- efficiency[,names(efficiency) != "batchDate"]  # drop POSIX date from efficiency

cat("First 20 rows of CATCH...\n")
print(catch[1:20,])
cat("First 20 rows of EFFICIENCY...\n")
print(efficiency[1:20,])

##   Add a trapFunctioning column so can tell when traps start and stop.
#print(catch[1:10,])
#print(catch.and.fits$trapsOperating[1:10,])
#
#catch <- merge( catch, catch.and.fits$trapsOperating, by=c("trapPositionID","batchDate") )
#
#tmp.catch <<- catch

#   The Grand Merge.  Merge catch info with efficiency info.
grand.df <- merge( catch, efficiency, by=c("trapPositionID", "batchDay"), all=T)

#   For each trap, drop the dates that are outside it's start and stop date.  This 
#   The season for each trap is identified as non missing catch.  I.e., the grand merge puts
#   in every date because efficiency data frame has all dates. 
grand.df <- grand.df[!is.na(grand.df$catch), ]

grand.df.rawCatch <- merge(grand.df,jason.catch4.df,by=c('trapPositionID','batchDate'),all.x=TRUE)                                   # bring in raw catch (measured)
grand.df.rawCatch.Inflated <- merge(grand.df.rawCatch,jason.totCatch4.df,by=c('trapPositionID','batchDate'),all.x=TRUE)                 # bring in inflated catch (measured + plus counts)
grand.df.rawCatch.Imputed <- merge(grand.df.rawCatch.Inflated ,jason.catch.and.fits4.df,by=c('trapPositionID','batchDate'),all.x=TRUE)  # bring in imputed catch

grand.df <- grand.df.rawCatch.Imputed

# somewhere, there are comments that state that catches of NA mean zero.  so, replace NA in each of 
# rawCatch and ImputedCatch with zero. 
grand.df$assignedCatch <- ifelse(is.na(grand.df$rawCatch), 0, grand.df$rawCatch)
grand.df$inflatedCatch <- ifelse(is.na(grand.df$inflatedCatch), 0, grand.df$inflatedCatch)
grand.df$unassignedCatch <- ifelse(is.na(grand.df$UnassdCatch), 0, grand.df$UnassdCatch)
grand.df$imputedCatch <- ifelse(is.na(grand.df$ImputedCatch), 0, round(grand.df$ImputedCatch,1))
grand.df$totalCatch <- ifelse(is.na(grand.df$inflatedCatch + grand.df$imputedCatch), 0, round(grand.df$inflatedCatch + grand.df$imputedCatch,1))

grand.df$rawCatch <- grand.df$ImputedCatch <- grand.df$catch <- grand.df$UnassdCatch <- NULL       

# check and make sure that assignedCatch + unassignedCatch + imputedCatch = totalCatch
# check and make sure that assignedCatch + unassignedCatch = inflatedCatch
# check and make sure that inflatedCatch + imputedCatch = totalCatch
grand.df$sum1 <- grand.df$assignedCatch + grand.df$unassignedCatch + grand.df$imputedCatch
grand.df$sum2 <- grand.df$assignedCatch + grand.df$unassignedCatch
grand.df$sum3 <- grand.df$inflatedCatch + grand.df$imputedCatch
grand.df$check1 <- ifelse(grand.df$sum1 == grand.df$totalCatch,TRUE,FALSE)
grand.df$check2 <- ifelse(grand.df$sum2 == grand.df$inflatedCatch,TRUE,FALSE)
grand.df$check3 <- ifelse(grand.df$sum3 == grand.df$totalCatch,TRUE,FALSE)

grand.df <<- grand.df

if(sum(grand.df$check1 + grand.df$check2 + grand.df$check3) != nrow(grand.df)*3){
  stop('Issue with summation of assignedCatch, unassignedCatch, inflatedCatch, imputedCatch, and/or totalCatch.  Investigate est_passage.R, around line 176.')
} else {
  cat('No issue with summation of assignedCatch, unassignedCatch, inflatedCatch, imputedCatch, and/or totalCatch.  Continuing...\n')
}


#   The passage estimator
grand.df$passage <- rep(NA, nrow(grand.df))
grand.df$passage <- ifelse(!is.na(grand.df$efficiency),grand.df$totalCatch / grand.df$efficiency,0)
#grand.df$passage <- round(grand.df$passage,1)   # round final passage estimate here so different summaries sum to the same number.

# jason.catch <<- catch
# jason.efficiency <<- efficiency








 
 
 
# db <- get( "db.file", env=.GlobalEnv ) 
# ch <- odbcConnectAccess(db)
# 
# includecatchID <- sqlFetch(ch, "TempSamplingSummary")             # jason add to get variable includeCatchID
# 
# close(ch) 
# 
# #  jason add all this get includeCatchID:  Assign time zone (definitely does matter -- otherwise it goes to MST)
# time.zone <- get( "time.zone", env=.GlobalEnv )
# # includecatchID$StartTime <- includecatchID$timeSampleStarted 
# includecatchID$EndTime <- includecatchID$timeSampleEnded 
# includecatchID$ProjID <- includecatchID$projectDescriptionID
# includecatchID$timeSampleStarted <- includecatchID$timeSampleEnded <- includecatchID$projectDescriptionID <- includecatchID$trapVisitID <- includecatchID$sampleGearID <- NULL
# # attr(includecatchID$StartTime, "tzone") <- time.zone
# attr(includecatchID$EndTime, "tzone") <- time.zone
# includecatchID <- includecatchID[,c('trapPositionID','EndTime','includeCatchID')]
# includecatchID$batchDay <- as.character(as.Date(includecatchID$EndTime))
# includecatchID$EndTime <- NULL

# jason - 1/14/2015.  the inclusion of includeCatchID ends up creating extra rows of data, when there is a 1 and a 2 on the same day.
# need to collapse this down...or just get rid of it for the purposes of bootstrapping.
# includecatchID2 <- includecatchID
# includecatchID2$includeCatchID2 <- ifelse(is.na(includecatchID2$includeCatchID),0,includecatchID2$includeCatchID)
# ugh <- includecatchID[,c('trapPositionID','batchDay','includeCatchID')]
# ugh$text <- as.character(includecatchID$includeCatchID)
# heyyy <- reshape(ugh,timevar="text",idvar=c("trapPositionID","batchDay"),direction="wide")
# heyyy[is.na(heyyy)] <- ''
# heyyy$includeCatchID <- paste0(heyyy$includeCatchID.0,heyyy$includeCatchID.1,heyyy$includeCatchID.2)  # keep as char for now, so if it can happen, 01 and 10 and so on don't become 1 and 1
# heyyy[heyyy=='0'] <- "NA"                                                                             # if it can happen, want things like "NA1" or "1NA"
# heyyy <- heyyy[,!(names(heyyy) %in% c('includeCatchID.0','includeCatchID.1','includeCatchID.2'))]
# 
# grand.df <- merge(grand.df[!(duplicated(paste0(grand.df$batchDate,grand.df$trapPositionID))),!(names(grand.df) %in% 'includeCatchID')],heyyy,by=c('trapPositionID','batchDay'),all.x=T)
# 
# rm(ugh,heyyy)






#   Save grand.df to .GlobalEnv (for debuggin) and write it out to a csv file
# catch.df <<- catch
# grand.df <<- grand.df
cat("grand.df stored in .GlobalEnv\n")

if( !is.na(file.root) ){
    tmp.df <- grand.df[, !(names(grand.df) %in% c("nReleased", "nCaught", "batchDay")) ]  # do this so can change names (headers) in csv file, Drop 2 columns
    names(tmp.df)[ names(tmp.df) == "imputed.catch" ] <- "propImputedCatch"
    names(tmp.df)[ names(tmp.df) == "imputed.eff" ] <- "propImputedEff"
    tmp.df$propImputedEff <- as.numeric(tmp.df$propImputedEff)  # convert to numbers, 0 or 1
    #tmp.df$passage <- round(tmp.df$passage)  # Round off passage
    tmp.df$totalCatch <- round(tmp.df$totalCatch,1)
    tmp.df$efficiency <- round(tmp.df$efficiency, 4)  
    
    # Merge in subsiteNames
    # ssiteNames <- attr(catch, "subsites")    # jason turn off
    ssiteNames <- catch.df.sites               # jason turn on
    tmp.df <- merge( ssiteNames, tmp.df, by.x="subSiteID", by.y="trapPositionID", all.y=T )
    out.fn <- paste(file.root, "_baseTable.csv", sep="")
    tmp.df$TrapPosition <- tmp.df$TrapPositionID <- NULL

     #tmp.df$includeCatchID <- ifelse(is.na(tmp.df$includeCatchID),NA,ifelse(tmp.df$includeCatchID == 1,'Yes',ifelse(tmp.df$includeCatchID == 12,'Yes+No','No')))

    tmp.df <- tmp.df[c('subSiteID','subSiteName','batchDate','assignedCatch','unassignedCatch','imputedCatch','totalCatch','propImputedCatch','efficiency','propImputedEff','passage')]    # rearrange columns
    
    tmp.df <- tmp.df[order(tmp.df$subSiteID,tmp.df$batchDate),]   # need to sort now?  1/8/2016.
    
    write.table( tmp.df, file=out.fn, sep=",", row.names=FALSE, col.names=TRUE)
    out.fn.list <- c(out.fn.list, out.fn)
}

# ====== Passage estimates are done by day.  Compute variance and summarize ====================================================================================================
f.banner(paste(" Bootstrapping, if called for, and summarizing by", summarize.by))

#   Because the summarization (to weeks, years, etc.) needs to go on in the bootstrapping routine, 
#   it is easier to do it all there. 
#   Even if bootstraps are not called for, F.bootstrap averages over traps (if multiple present) and 
#   summarizes by 'summarize.by'.

#   Debugging (turn off bootstrapping)
#ci = F

n <- F.bootstrap.passage( grand.df, catch.and.fits$fits, catch.and.fits$X.miss, catch.and.fits$gaps,
                catch.and.fits$bDates.miss, eff.and.fits$fits, eff.and.fits$X, eff.and.fits$ind.inside, 
                eff.and.fits$X.dates, summarize.by, 100, ci )
                
              
if(usepb){
    tmp <- getWinProgressBar(progbar)
    setWinProgressBar(progbar, tmp + (1-tmp)*.9 )
}

#   Do I need the following?
#if( summarize.by == "day" ){
#    #   Add in some extra columns that don't apply otherwise.
#    n$catch <- c(tapply( grand.df$catch, index, sum, na.rm=T ))
#    n$pct.imputed.eff <- c(tapply(as.numeric(grand.df$imputed.eff), index, mean, na.rm=T ))
#    n$efficiency <- c(tapply(grand.df$efficiency, index, mean, na.rm=T ))
#}




#   ---- Summarize auxillary information about catch
#   ---- jason  - 4/15/2015.  note that these stats use vars of the form 'x.Orig'. use the other set, based on
#        .tot for metrics pertaining to the inflated catch.  

index.aux <- F.summarize.index( catch.df$batchDate, summarize.by )  

# jason: for some reason, for testi = 7, bootstrap passage brings back one year, but summarize index brings back
# another, when calculating per year.  this messes up the join below. force the two to be the 
# same in this one case.
if(summarize.by == 'year'){
  n[1,1] <- index.aux[[1]][1]
}

#   Mean Forklength
num <- catch.df$mean.fl.Orig * catch.df$n.Orig 
num <- tapply( num, index.aux, sum, na.rm=T )

#   SD of Forklength
num.sd <- (catch.df$sd.fl.Orig * catch.df$sd.fl.Orig) * (catch.df$n.Orig  - 1)    # this is sum of squares -- well, without the summing just yet
num.sd <- tapply( num.sd, index.aux, sum, na.rm=T )

#   n
den <- tapply( catch.df$n.Orig, index.aux, sum, na.rm=T)

#   Mean and SD computations
aux.fl <- ifelse( den > 0, num / den, NA )
aux.sd <- ifelse( den > 1, sqrt(num.sd / (den-1)), NA )



catch.df.reduced <- aggregate(catch.df,by=list(ID=catch.df$batchDate),head,1)  # 6/5/2015 - jason reduces df to select first of each and changes to batchdate...
catch.df.Fishing <- catch.df
catch.df.Fishing$SampleMinutes <- ifelse(catch.df.Fishing$TrapStatus == 'Not fishing',0,catch.df.Fishing$SampleMinutes)
catch.df.Fishing <- unique(catch.df.Fishing[,c('SampleMinutes','batchDate','trapPositionID')])
num <-  aggregate(catch.df.Fishing$SampleMinutes,by=list(ID=catch.df.Fishing$batchDate),sum)[,2]


#   Amount of time sampled
# if(summarize.by == "day"){
#   catch.df.reduced <- aggregate(catch.df,by=list(ID=catch.df$batchDate),head,1)  # 6/5/2015 - jason reduces df to select first of each and changes to batchdate...
#   catch.df.Fishing <- catch.df
#   catch.df.Fishing$SampleMinutes <- ifelse(catch.df.Fishing$TrapStatus == 'Not fishing',0,catch.df.Fishing$SampleMinutes)
#   catch.df.Fishing <- unique(catch.df.Fishing[,c('SampleMinutes','batchDate','trapPositionID')])
#   num <-  aggregate(catch.df.Fishing$SampleMinutes,by=list(ID=catch.df.Fishing$batchDate),sum)[,2]
# } else {
#   catch.df.reduced <- aggregate(catch.df,by=list(ID=catch.df$trapVisitID),head,1)  # 4/13/2015 - jason reduces df to select first of each 
#   num <- as.numeric( catch.df.reduced$SampleMinutes )                                   # 4/13/2015 - jason pulls from reduced df
# }

tzn <- get("time.zone", .GlobalEnv )                                                   # batchDate defaults to mountain time. fix that.
catch.df.reduced$batchDate <- as.POSIXct( strptime( format(catch.df.reduced$batchDate, "%Y-%m-%d"), "%Y-%m-%d", tz=tzn),tz=tzn)   # fix the time. 
   
index.aux <- F.summarize.index(catch.df.reduced$batchDate,summarize.by)               # 4/13/2015 - jason indexes in reduced df  
 


aux.hrs <- tapply( num, index.aux, sum, na.rm=T )/60                                  # this is hours actually sampled during the 'index' period

#den <- rep( 24, length(batchDate.filled) )
#den <- tapply( den, index.aux2, sum, na.rm=T )  # this is total hours in 'index' period
#
#   Note: I will leave the commented out code that computes amount of time in each index period.  The reason 
#   I commented it out is that 'den' may have more rows than num.  i.e., catch.df$batchDate may have fewer rows than batchDate.filled.
#   This makes 'den' difficult to merge back in to 'num', but it could be done.

aux<-data.frame( s.by=dimnames(aux.fl)[[1]], 
    nForkLenMM=c(den),
    meanForkLenMM=c(aux.fl), 
    sdForkLenMM=c(aux.sd),
    sampleLengthHrs=c(aux.hrs),
    stringsAsFactors=F, row.names=NULL )

#   ---- Merge 'n' and 'aux' information together    
n <- merge(n,aux, by="s.by", all.x=T)   

n$sampleLengthDays <- n$sampleLengthHrs / 24

tz.offset <- as.numeric(as.POSIXct(0, origin="1970-01-01", tz=time.zone))
n$date <- as.POSIXct( n$date-tz.offset, origin="1970-01-01", tz=time.zone )  # I think this only works west of GMT (North America).  East of GMT, it may be 12 hours off. UNTESTED east of GMT

#   Put the final data frame together
names(n)[names(n) == "s.by"] <- summarize.by

attr(n, "taxonID" ) <- attr(catch.df,"taxonID")
attr(n, "species.name") <- attr(catch.df, "species.name")
attr(n, "siteID" ) <- attr(catch.df,"siteID")
attr(n, "site.name") <- attr(catch.df, "site.name")
attr(n, "site.abbr") <- attr(catch.df, "site.abbr")  
attr(n, "runID") <- attr(catch.df, "runID")
attr(n, "run.name") <- attr(catch.df, "run.name")
attr(n, "year") <- attr(catch.df, "year")
attr(n, "run.season") <- attr(catch.df, "run.season")
attr(n, "summarized.by") <- summarize.by
attr(n, "out.fn.list") <- out.fn.list
attr(n, "trapsOperating") <- catch.and.fits$trapsOperating

f.banner(" F.est.passage - COMPLETE ")

n

}
