



reduceETrials <- function(df,possibleVars,bsplBegDt,bsplEndDt){
  
  # df <- obs.eff.df

  df <- df[ is.na(df$TrapPositionID) | (df$TrapPositionID == trap), ]
  ind <- !is.na(df$efficiency)
  
  initialVars <- c("(Intercept)",names(df)[!(names(df) %in% c("batchDate","nReleased","nCaught","efficiency","covar","batchDate2","fishDay"))])
  initialVarsNum <- c(2,as.integer(possibleVars %in% initialVars))
  
  # JASON PLAYS AROUND AND ENSURES A DATE REPEATS OVER TWO YEARS.
  #df[!is.na(df$efficiency),]$batchDate[15] <- df[!is.na(df$efficiency),]$batchDate[1]
  
  
  #   ---- Define these so we can model on (basically) fishing day of the season.  Do this with respect to 
  #   ---- the year 1960 paradigm defined above.  We really only care about the number of days since the
  #   ---- minimum start of fishing, over all years, so the year is immaterial.  Use 1960, or maybe 1959, 
  #   ---- so we always keep this fact in mind.  Not sure this will always work... Need to check.  
  sign <- as.numeric(strftime(df$batchDate,format="%j")) - as.numeric(strftime(bsplBegDt,format="%j"))
  df$fishDay <- ifelse(sign == 0,0,
                       ifelse(sign  < 0,sign + 365,as.numeric(strftime(df$batchDate,format="%j")))) 
  
  firstYear <- min(bsplBegDt$year + 1900)
  df$batchDate2 <- ifelse(sign >= 0,as.POSIXct(paste0(firstYear,"-",as.POSIXlt(df$batchDate)$mon + 1,"-",as.POSIXlt(df$batchDate)$mday),format="%Y-%m-%d",tz=time.zone),
                          ifelse(sign < 0,as.POSIXct(paste0(firstYear + 1,"-",as.POSIXlt(df$batchDate)$mon + 1,"-",as.POSIXlt(df$batchDate)$mday),format="%Y-%m-%d",tz=time.zone),
                                 NA))
  
  #   ---- If we have a batchDate on 2/29, we have a problem, because 1970 wasn't a leap year.  Note that I could have 
  #   ---- chosen 
  df$batchDate2 <- as.POSIXct(df$batchDate2,format="%Y-%m-%d",tz="America/Los_Angeles",origin="1970-01-01 00:00:00 UTC")
  
  #   ---- Find the "season", which is between first and last trials
  #   ---- We look for 'inside' with respect to our 1959-1960 mapped year of trials.  
  strt.dt <- bsplBegDt #min( df$batchDate[ind], na.rm=T )  # Earliest date with an efficiency trial
  end.dt  <- bsplEndDt #max( df$batchDate[ind], na.rm=T )  # Latest date with efficiency trial
  ind.inside <- (strt.dt <= df$batchDate2) & (df$batchDate2 <= end.dt)
  inside.dates <- c(strt.dt, end.dt)
  all.ind.inside[[trap]] <- inside.dates  # save season dates for bootstrapping
  
  #   ---- The fitting data frame
  tmp.df <- df[ind & ind.inside,]
  m.i <- sum(ind & ind.inside)
  
  return(list(df=df,tmp.df=tmp.df,m.i=m.i,initialVars=initialVars,initialVarsNum=initialVarsNum))
  
}