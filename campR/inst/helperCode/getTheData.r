getTheData <- function(openThese,stem){

#   openThese <- openTheseB
#   stem <- stem

  bigDF <- NULL
  theCSVs <- vector("list",length(openThese))
  theDesc <- vector("list",length(openThese))
  theDFs <- vector("list",length(openThese))
  for(i in 1:nrow(openThese)){
    if(openThese$type[i] == 'life'){
      theCSVs[[i]] <- read.csv(paste0(stem,'/',openThese$file[i]),skip=5)
      
      #   ---- Build up the desc data frame.  
      theDesc[[i]] <- read.csv(paste0(stem,'/',openThese$file[i]),nrows=4,header=FALSE,stringsAsFactors=FALSE)
      desc <- data.frame(t( theDesc[[i]]$V2))
      desc$min.date <- strsplit(as.character(droplevels(desc$X4))," to ",fixed=TRUE)[[1]][1]
      desc$max.date <- strsplit(as.character(droplevels(desc$X4))," to ",fixed=TRUE)[[1]][2]     
      names(desc)[names(desc) == "X1"] <- "siteName"
      names(desc)[names(desc) == "X2"] <- "site"
      desc$X3 <- desc$X4 <- NULL
      desc$by <- "All"
      desc$river <- "Later"
      desc$file <- openThese$type[i]
      desc <- desc[,c('by','river','siteName','min.date','max.date','file')]
    
      CBs <- NULL
  
      if( grepl("forkLength",openThese$file[i],fixed=TRUE) == TRUE ){
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
          thisLine <- data.frame(run=run,lifeStage=lifeStage,time=time,bEst=bEst,bLCL=bLCL,bUCL=bUCL)
          thisLine <- cbind(desc,thisLine)
          CBs <- rbind(CBs,thisLine)
        }
      }
      theDFs[[i]] <- CBs
    } else if(openThese$type[i] == 'run'){
      theCSVs[[i]] <- read.csv(paste0(stem,'/',openThese$file[i]),skip=5)   
      
      #   ---- Build up the desc data frame.  
      theDesc[[i]] <- read.csv(paste0(stem,'/',openThese$file[i]),nrows=4,header=FALSE,stringsAsFactors=FALSE)
      desc <- data.frame(t( theDesc[[i]]$V2))
      #lifestage <- as.character(droplevels(desc$X5))
      desc$min.date <- strsplit(as.character(droplevels(desc$X4))," to ",fixed=TRUE)[[1]][1]
      desc$max.date <- strsplit(as.character(droplevels(desc$X4))," to ",fixed=TRUE)[[1]][2]     
      names(desc)[names(desc) == "X1"] <- "siteName"
      names(desc)[names(desc) == "X2"] <- "site"
      desc$X3 <- desc$X4 <- NULL
      desc$river <- "Later"
      desc$file <- openThese$type[i]
      desc$by <- strsplit(openThese$file[i],"-",fixed=TRUE)[[1]][1]
      desc <- desc[,c('by','river','siteName','min.date','max.date','file')]
    
      CBs <- NULL

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
        thisLine <- data.frame(run=run,lifeStage=lifeStage,time=time,bEst=bEst,bLCL=bLCL,bUCL=bUCL)
        thisLine <- cbind(desc,thisLine)
        CBs <- rbind(CBs,thisLine)
      }
      theDFs[[i]] <- CBs
    } else if(openThese$type[i] == 'summary'){
      
      theCSVs[[i]] <- read.csv(paste0(stem,'/',openThese$file[i]),skip=8)   
      
      #   ---- Build up the desc data frame.  
      theDesc[[i]] <- read.csv(paste0(stem,'/',openThese$file[i]),nrows=7,header=FALSE,stringsAsFactors=FALSE)
      desc <- data.frame(t( theDesc[[i]]$V2))
      #lifestage <- as.character(droplevels(desc$X5))
      desc$min.date <- strsplit(as.character(droplevels(desc$X7))," to ",fixed=TRUE)[[1]][1]
      desc$max.date <- strsplit(as.character(droplevels(desc$X7))," to ",fixed=TRUE)[[1]][2]     
      names(desc)[names(desc) == "X1"] <- "siteName"
      names(desc)[names(desc) == "X2"] <- "site"
      names(desc)[names(desc) == "X4"] <- "run"
      names(desc)[names(desc) == "X5"] <- "lifeStage"
      names(desc)[names(desc) == "X6"] <- "by"
      desc$X3 <- NULL
      desc$river <- "Later"
      desc$file <- openThese$type[i]
      desc <- desc[,c('by','river','siteName','min.date','max.date','file','run','lifeStage')]
      
      CBs <- NULL
   
      if(substr(desc$lifeStage[1],1,2) == "FL"){
        desc[1,]$file <- 'forklength'
      } else {
        lifeStage <- NA
      }
      
      time <- theCSVs[[i]][,1]
      bEst <- theCSVs[[i]]$passage
      bLCL <- theCSVs[[i]]$lower95pctCI
      bUCL <- theCSVs[[i]]$upper95pctCI
      thisLine <- data.frame(time=time,bEst=bEst,bLCL=bLCL,bUCL=bUCL)
      thisLine <- cbind(desc,thisLine)  
      CBs <- rbind(CBs,thisLine)
      theDFs[[i]] <- CBs
    }
    bigDF <- rbind(bigDF,theDFs[[i]])
  }
  bigDF
}