getTheData <- function(openThese,theRiver,stem,before){

#   openThese <- openTheseB
#   theRiver <- theRiver
#   stem <- stemB
#   before <- TRUE

  bigDF <- NULL
  theCSVs <- vector("list",length(openThese))
  theDFs <- vector("list",length(openThese))
  for(i in 1:nrow(openThese)){
    if(openThese$type[i] == 'life'){
      theCSVs[[i]] <- read.csv(paste0(stem,'/',openThese$file[i]),skip=5)
      CBs <- NULL
      desc <- data.frame(t(strsplit(openThese$file[i],"_",fixed=TRUE)[[1]]),stringsAsFactors=FALSE)[,1:6]
      desc[1,5] <- substr(desc[1,5],1,10)
      names(desc) <- c('by','river','siteName','min.date','max.date','file')
      
      if( grepl("FL",openThese$file[i],fixed=TRUE) == TRUE ){
        desc$file <- "forklength"
      }
      
      J <- nrow(theCSVs[[i]])                              # could possibly not have all lifestages
      K <- (dim(theCSVs[[i]])[2] - 1) / 4                  # could possibly not have all runs
      for(j in 1:J){  # j is for LifeStages
        for(k in 1:K){  # k is for Runs
          lifeStage <- theCSVs[[i]]$LifeStage[j]
          run <- strsplit(colnames(theCSVs[[i]])[4*(k - 1) + 2],".",fixed=TRUE)[[1]][1]
          if(run == "Late"){
            run <- "Late fall"
          }
          time <- NA
          bEst <- theCSVs[[i]][j,4*(k - 1) + 3]
          bLCL <- ifelse(theCSVs[[i]][j,4*(k - 1) + 4] == 0,NA,theCSVs[[i]][j,4*(k - 1) + 4])
          bUCL <- ifelse(theCSVs[[i]][j,4*(k - 1) + 5] == 0,NA,theCSVs[[i]][j,4*(k - 1) + 5])
          bMag <- ifelse(is.na(bUCL),NA,log(bUCL,10) - log(bEst,10))
          bOOL <- ifelse(is.na(bMag),NA,ifelse(bMag > 2,1,ifelse(bMag < 0,2,0)))
          thisLine <- data.frame(run=run,lifeStage=lifeStage,time=time,bEst=bEst,bLCL=bLCL,bUCL=bUCL,bMag=bMag,bOOL=bOOL)
          thisLine <- cbind(desc,thisLine)
          CBs <- rbind(CBs,thisLine)
        }
      }
      theDFs[[i]] <- CBs
    } else if(openThese$type[i] == 'run'){
      theCSVs[[i]] <- read.csv(paste0(stem,'/',openThese$file[i]),skip=5)   
      CBs <- NULL
      desc <- data.frame(t(strsplit(openThese$file[i],"_",fixed=TRUE)[[1]]),stringsAsFactors=FALSE)[,1:6]
      names(desc) <- c('by','river','siteName','min.date','max.date','file')
      K <- (dim(theCSVs[[i]])[2] - 1) / 4                  # could possibly not have all runs
      for(k in 1:K){  # k is for Runs
        lifeStage <- theCSVs[[i]]$LifeStage[1]
        run <- strsplit(colnames(theCSVs[[i]])[4*(k - 1) + 2],".",fixed=TRUE)[[1]][1]
        if(run == "Late"){
          run <- "Late fall"
        }
        time <- NA
        bEst <- theCSVs[[i]][1,4*(k - 1) + 3]
        bLCL <- ifelse(theCSVs[[i]][1,4*(k - 1) + 4] == 0,NA,theCSVs[[i]][1,4*(k - 1) + 4])
        bUCL <- ifelse(theCSVs[[i]][1,4*(k - 1) + 5] == 0,NA,theCSVs[[i]][1,4*(k - 1) + 5])
        bMag <- ifelse(is.na(bUCL),NA,log(bUCL,10) - log(bEst,10))
        bOOL <- ifelse(is.na(bMag),NA,ifelse(bMag > 2,1,ifelse(bMag < 0,2,0)))
        thisLine <- data.frame(run=run,lifeStage=lifeStage,time=time,bEst=bEst,bLCL=bLCL,bUCL=bUCL,bMag=bMag,bOOL=bOOL)
        thisLine <- cbind(desc,thisLine)
        CBs <- rbind(CBs,thisLine)
      }
      theDFs[[i]] <- CBs
    } else if(openThese$type[i] == 'summary'){
      
      theCSVs[[i]] <- read.csv(paste0(stem,'/',openThese$file[i]),skip=8)    
      CBs <- NULL
      desc <- data.frame(t(strsplit(openThese$file[i],"_",fixed=TRUE)[[1]]),stringsAsFactors=FALSE)[,1:6]
      temp1 <- substr(desc[1,5],1,10)
      temp2 <- substr(desc[1,5],11,nchar(desc[1,5]))
      desc[1,5] <- temp1
      desc$Run <- temp2
      names(desc) <- c('by','river','siteName','min.date','max.date','file','run')
      
      if(substr(desc$run[1],1,2) == "FL"){
        temp <- desc[1,]$run
        desc[1,]$run <- strsplit(temp,"mm",fixed=TRUE)[[1]][2]
        lifeStage <- paste0(substr(strsplit(temp,"mm",fixed=TRUE)[[1]][1],3,nchar(strsplit(temp,"mm",fixed=TRUE)[[1]][1])),"mm")
        desc[1,]$file <- 'forklength'
      } else {
        lifeStage <- NA
      }
      
      time <- theCSVs[[i]][,1]
      bEst <- theCSVs[[i]]$passage
      bLCL <- theCSVs[[i]]$lower95pctCI
      bUCL <- theCSVs[[i]]$upper95pctCI
      bMag <- ifelse(is.na(bUCL),NA,log(bUCL,10) - log(bEst,10))
      bOOL <- ifelse(is.na(bMag),NA,ifelse(bMag > 2,1,ifelse(bMag < 0,2,0)))
      thisLine <- data.frame(lifeStage=lifeStage,time=time,bEst=bEst,bLCL=bLCL,bUCL=bUCL,bMag=bMag,bOOL=bOOL)
      thisLine <- cbind(desc,thisLine)  
      CBs <- rbind(CBs,thisLine)
      theDFs[[i]] <- CBs
    }
    bigDF <- rbind(bigDF,theDFs[[i]])
  }
  bigDF$sequence <- 'before'
  if(before == FALSE){
    names(bigDF)[names(bigDF) == 'bEst'] <- 'aEst'
    names(bigDF)[names(bigDF) == 'bLCL'] <- 'aLCL'
    names(bigDF)[names(bigDF) == 'bUCL'] <- 'aUCL'
    names(bigDF)[names(bigDF) == 'bMag'] <- 'aMag'
    names(bigDF)[names(bigDF) == 'bOOL'] <- 'aOOL'
    bigDF$sequence <- 'after'
  }
  bigDF
}