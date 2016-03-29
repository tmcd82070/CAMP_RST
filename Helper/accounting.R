# calculate the sum of various fish groupings, in an effort to help check
# resulting calculations and statistics. 

# theDF <- est.catch
# slice <- "byRun"
# slice <- "byTrap"

accounting <- function(theDF,slice){
  

  if(slice == "byRun"){
    
    # reduce to df with data we care about.  makes replacing of NA with zero smarter
    theDF <- theDF[,c('FinalRun','trapPositionID','lifeStage','halfConeAssignedCatch','halfConeUnassignedCatch','assignedCatch','unassignedCatch','modAssignedCatch','modUnassignedCatch',"n.tot")]
    theDF[is.na(theDF)] <- 0
    
    theRuns <- unique(theDF$FinalRun)
    theTraps <- unique(theDF$trapPositionID)
    theStages <- unique(theDF$lifeStage)
    
    theGrid <- expand.grid(FinalRun=theRuns,Traps=theTraps,lifeStage=theStages)
    byString <- c('FinalRun','Traps','lifeStage')
    
    calcTheN <- function(variable){
      
      # variable <- "n.halfConeAdjAssd"
  
      theArray <- tapply( theDF[,variable], list(theDF$FinalRun,theDF$trapPositionID,theDF$lifeStage), FUN=sum)
      theStats <- cbind( expand.grid( FinalRun=dimnames(theArray)[[1]], Traps=dimnames(theArray)[[2]], lifeStage=dimnames(theArray)[[3]]), temp = c(theArray))
      names(theStats)[names(theStats) == 'temp'] <- variable
      cat(paste0("The summary table associated with counting variable ",variable," consists of ",nrow(theStats)," rows.\n"))
      theStats
    }
                 
    col1 <- calcTheN('halfConeAssignedCatch')
    col2 <- calcTheN('halfConeUnassignedCatch')
    col3 <- calcTheN('assignedCatch')
    col4 <- calcTheN('unassignedCatch')
    col5 <- calcTheN('modAssignedCatch')
    col6 <- calcTheN('modUnassignedCatch')
    col7 <- calcTheN("n.tot")
    nList <- list(col1,col2,col3,col4,col5,col6,col7)
    
  } else if(slice == "byTrap"){
    
    # reduce to df with data we care about.  makes replacing of NA with zero smarter
    theDF <- theDF[,c('trapPositionID','halfConeAssignedCatch','halfConeUnassignedCatch','assignedCatch','unassignedCatch','modAssignedCatch','modUnassignedCatch',"imputed.catch","catch","assdCatch","UnassdCatch","assdCatchA" )]
    theDF[is.na(theDF)] <- 0
    
    theTraps <- unique(theDF$trapPositionID)
    
    theGrid <- expand.grid(Traps=theTraps)
    byString <- c('Traps')
    
    calcTheN <- function(variable){
      
      # variable <- "assdCatch"
      
      theArray <- tapply( theDF[,variable], list(theDF$trapPositionID), FUN=sum)
      theStats <- cbind( expand.grid( Traps=dimnames(theArray)[[1]]), temp = c(theArray))
      names(theStats)[names(theStats) == 'temp'] <- variable
      cat(paste0("The summary table associated with counting variable ",variable," consists of ",nrow(theStats)," rows.\n"))
      theStats
    }
    
    col1 <- calcTheN('halfConeAssignedCatch')
    col2 <- calcTheN('halfConeUnassignedCatch')
    col3 <- calcTheN('assignedCatch')
    col4 <- calcTheN('unassignedCatch')
    col5 <- calcTheN('modAssignedCatch')
    col6 <- calcTheN('modUnassignedCatch')
    col7 <- calcTheN("imputed.catch")
    col8 <- calcTheN("catch")
    col9 <- calcTheN("assdCatch")
    col10 <- calcTheN("UnassdCatch")
    col11 <- calcTheN("assdCatchA")
    nList <- list(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10,col11)
    
  }
  
  # put it all together.
  for(m in 1:(length(nList) - 1)){
    if(m == 1){
      account <- merge(nList[[1]],nList[[2]],by=byString)
    } else {
      account <- merge(account,nList[[m + 1]],by=byString)
    }
  }
  
  account[is.na(account)] <- 0
  account
}