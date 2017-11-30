stepper <- function(df,varVec){
  
  # df <- obs.eff.df[!is.na(obs.eff.df$nReleased) & obs.eff.df$TrapPosition == 57001,c("batchDate","TrapPositionID","bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength")]
  # varVec <- c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength")
  
  traps <- unique(df$TrapPositionID)
  
  df.new <- NULL
  for(trap in traps){
    
    df.t <- df[df$TrapPositionID == trap,]
    
    #   ---- The earliest batchDate in df.t reflect the first e-trial.  This doesn't correspond with start of 
    #   ---- fishing.  This should use min.date, but that variable is not passed this deeply into 
    #   ---- passage estimation routines.  So, just add in a full year of batchDates back in time.  The 
    #   ---- merge of the dataframe resulting from this, along with the temporal spline batchDates (the real
    #   ---- ones that matter) will clean up the resulting mess.  
    earliestBatchDate <- min(df.t$batchDate) - 365*24*60*60
    fakeFirstRow <- data.frame(batchDate=earliestBatchDate,
                               TrapPositionID = trap,
                               bdMeanNightProp = df.t[1,]$bdMeanNightProp,
                               bdMeanMoonProp = df.t[1,]$bdMeanMoonProp,
                               bdMeanForkLength = df.t[1,]$bdMeanForkLength)
    
    df.t <- rbind(fakeFirstRow,df.t)
    
    df.t$batchDate  <- as.POSIXct(strptime(df.t$batchDate,format="%Y-%m-%d"),format="%Y-%m-%d",tz="UTC")
    df.t$batchDateNext <- as.POSIXct(strptime(df.t$batchDate[c(2:nrow(df.t),NA)],format="%Y-%m-%d"),format="%Y-%m-%d",tz="UTC")
    
    #   ---- Put in a value that is one month after the last batchDate for batchDateNext.
    df.t[nrow(df.t),]$batchDateNext <- df.t[nrow(df.t),]$batchDate + 30*24*60*60
    
    #   ---- Get rid of rows where batchDate = batchDateNext
    df.t <- df.t[df.t$batchDate != df.t$batchDateNext,]
    
    #   ---- Slide batchDateNext back by one day, so as to prevent duplication of batchDate in expansion. 
    df.t$batchDateNext <- df.t$batchDateNext - 24*60*60
    
    df.t.new <- NULL
    for(i in 1:nrow(df.t)){
      
      df.t.i <- data.frame(TrapPositionID=trap,
                           batchDate=seq(df.t[i,"batchDate"],df.t[i,"batchDateNext"],by="1 day"))
      
      for(j in 1:length(varVec)){
        df.t.i[,paste0(varVec[j],"Step")] <- ifelse(is.na(df.t[i,varVec[j]]) | is.nan(df.t[i,varVec[j]]),-99,df.t[i,varVec[j]])
      }
      
      df.t.new <- rbind(df.t.new,df.t.i)
    }
    
    #   ---- Grab a mean value to use in case any one of the metrics is -99.  Do this after the expansion 
    #   ---- so that we weight on the number of days.  Do this for each trap. 
    means <- rep(0,length(varVec))
    for(i in 1:length(varVec)){
      nums <- df.t.new[,paste0(varVec[i],"Step")]
      means[i] <- mean(nums[nums != -99])
      df.t.new[df.t.new[,paste0(varVec[i],"Step")] == -99,paste0(varVec[i],"Step")] <- means[i]
    }
    
    df.new <- rbind(df.new,df.t.new)
  }
  return(df.new)
}

# par(mfrow=c(5,1))
# for(i in 1:length(unique(df.new$TrapPositionID))){
#   trap <- unique(df.new$TrapPositionID)[i]
#   plot(df.new[df.new$TrapPositionID == trap,]$batchDate,df.new[df.new$TrapPositionID == trap,]$meanForkLengthStep)
# }
# par(mfrow=c(1,1))