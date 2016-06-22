#' @export assignLifeStage
#' 
#' @title assignLifeStage
#' 
#' @description
#' 
#'  Jared Studyvin
#'  10 Feb 2016
#'  Assign life stage
#' 
#' 
#'  This function will be called within F.get.catch.data
#'  The purpose is to replace the life stage column with an updated assignment based on a clustering routine
#' 
#' 
#' 
#' 
#' @param DATA describe argument
#' @param groupN=NULL describe argument
#' @param USEWeight=NULL describe argument
#' 
#' @details other comments found in file
#'  create id column to keep track of expanding and collapsing the data
#'  get unique final runs
#'  noRun - grep('unassig',finalRun,ignore.case=TRUE)
#'  if(length(noRun)>0){
#'      finalRun - finalRun[-noRun]
#'  }
#'  save biologist life stage assignment
#'  for debugging
#' runDat - subset(DATA,FinalRun==sample(finalRun,1));with(runDat,unique(FinalRun))
#'  create list for saving mean vectors and variance covariance matrices from each mixture distribution
#' 
#'  save data before assignment
#'  save data after assignment
#' 
#' 
#' 
#' 
#' 
#' 
#'  keep only needed columns
#'  final run
#'  number of fish with a forklength
#'  if final run is unassigned OR if number of fish with fork length <100
#'  then make life stage unassigned and return
#'  number of fish with a weight
#'  number of fish with a forklength and weight
#'  won't use weight unless enough there is enough data
#'  subset data to be used for analysis
#'  use these rows for analysis
#'  use these columns for analysis
#'  columns to keep when collapsing data
#'  use these rows for analysis
#'  use these columns for analysis
#'  columns to keep when collapsing data
#'  need the cluster means to be at least this far apart in the forklength dimension otherwise the number of groups is reduced
#'  The reduction only happens if the number of groups is not specified by the user
#'  user overwrite choice of number of groups
#'  fit cluster with user specified number of groups
#'  fit 2 groups if means are close
#'  fit 1 group if means are close
#'  allow computer to choose
#'  while(!goodClust){
#'      clustTemp <- Mclust(data=expDat[inRow,covars],G=nGroup,mclust.options("emModelNames"))
#'      (meanFL <- clust[['parameters']]$mean['forkLength',])
#'      if(min(pairDiff(meanFL))<minMeanDiff){
#'          print(nGroup)
#'          if(nGroup == 1){
#'              goodClust <- TRUE
#'          }else{
#'              nGroup <- nGroup-1
#'          }
#'          ## if(nGroup==1){
#'          ##     runDat$lifeStage <- 'Unassigned'
#'          ##     runDat[with(runDat,!is.na(forkLength)),'lifeStage'] <- 'Medium'
#'          ##     return(runDat)
#'          ## }
#'      }else{
#'          goodClust <- TRUE
#'      }
#'  }
#' summary(clustTemp)
#' summary(clust)
#'  get group names based on number of groups
#'  array for var-cov matrices, last dim indicates groups
#'  array of mean vectors, last dim indicates groups
#'  save mixture distribution summary statistics
#'     mixDistSigmaList
#'     mixDistMUList
#' 
#'  for debugging
#'  save mu and Sigma
#'  print('save mu and sigma')
#'  print(saveName <- gsub(' ','',paste0(as.character(runDat[1,c('river','trap','year','FinalRun')]),collapse='')))
#'  parm <- list(mu,Sigma)
#'  save(parm,file=paste0(output.file,'parm',saveName,'.Rdata'))
#' 
#'  this these did not get an assignment
#' ddply(expDat,~group,summarize,FL=mean(forkLength))
#' row <- subset(expDat,id%in%c(1,2))
#'  this is the collapse data
#'  should have the same number of rows at runDat
#'  This is not needed any more, remove to not take up memory
#'  ddply(collapseDat,~group,summarize,FL=mean(forkLength,na.rm=TRUE))
#'  mu
#' with(runDat,sum(is.na(forkLength)))
#'  for debugging
#'  M <- mu
#'  S <- Sigma
#'  dat <- collapseDat
#'  w <- haveFLnoW
#'  varHave <- 'forkLength'
#'  rm(M,S,dat,w)
#' dat = whole data frame
#' w = logical vector of where assignment needs to be done
#'  M = mean matrix from Mclust
#'  S = variance covariance array from Mclust
#' print(X[,y])
#'  If weight was used some fish may only have a weight or forklength
#'  This if statement assigns lifestage to fish with either a weight or a forklength
#'  where there is a weight but no forklength
#'  where there is a forklength but no weight
#'  this matches up the group number to the group name
#' ddply(collapseDat[haveFLnoW,],~group,summarize,FL=mean(forkLength,na.rm=TRUE))
#' 
#' @return describe return value
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{related routine}}, \code{\link{related routine}}
#' 
#' @examples
#' \dontrun{
#' # insert examples
#' 
#' }
###############################################
## Jared Studyvin
## 10 Feb 2016
## Assign life stage
###############################################

## This function will be called within F.get.catch.data
## The purpose is to replace the life stage column with an updated assignment based on a clustering routine



assignLifeStage <- function(DATA,groupN=NULL,USEWeight=NULL,...){
  ## DATA = the catch data
  ## groupN = the number of life stage groups to fit, NULL allows the program to decide
  ## USEWeight = should weight be used in the mixture distribution, NULL allows the program to decide
  
  
#	JARED:  WHY IS THIS SAVE HERE?  IS IT NECESSARY?
	
  ## save data before assignment
  save(DATA,site,min.date,max.date,sample.size.forkLength,sample.size.forkLengthAndWeight,weight.prop.forkLength ,forkLength.mean.diff,output.file,file=paste0(output.file,'DATA.Rdata'))
  
  
  
  # Data <- DATA
  # groupN <- xxx
  # USEWeight <- NULL
  
  if(!is.null(groupN)){
    if(!(groupN%in%c(2,3))){
      stop('The number of groups must be either 2 or 3!')
    }
  }
  
  ## required packages
  #needPack <- c('Rcpp','plyr','mclust','car')
  #getPackages(needPack) # loads and installs if needed
  
  ## create id column to keep track of expanding and collapsing the data
  DATA$id <- 1:nrow(DATA)
  
  ## get unique final runs
  (finalRun <- with(DATA,as.character(unique(FinalRun))))
  
  ## noRun <- grep('unassig',finalRun,ignore.case=TRUE)
  ## if(length(noRun)>0){
  ##     finalRun <- finalRun[-noRun]
  ## }
  
  ## save biologist life stage assignment
  DATA$bioLS <- DATA$lifeStage
  DATA$lifeStage <- NULL
  
  
  ## for debugging
  ##runDat <- subset(DATA,FinalRun=='Fall');with(runDat,unique(FinalRun))
  

  
  ## create list for saving mean vectors and variance covariance matrices from each mixture distribution
  # JARED: CAN THESE <<- BE CHANGED TO <- ?  
  mixDistMUList <<- list()
  mixDistSigmaList <<- list()
  ####################################################################
  
  # JARED: SEEMS LIKE WE CAN ERASE THIS CALL TO MEMORYUSAGE()
  #memoryUsage()
  
  ## This function wraps the assign life stage into a try statement
  assignTry <- function(runDat,G=NULL,USEWeight=NULL){
    
    out <- tryCatch({
      
      assignLS(runDat=runDat,G=G,USEWeight=USEWeight)
      
    },
    error=function(cond){
      message('Assigning life stage produced an error.')
      message('Here is the original error message:')
      message(cond)
      
      cat('\n')
      runDat$days <- with(runDat,as.numeric(difftime(SampleDate,min(SampleDate),units='days')))
      fRun <- with(runDat,as.character(unique(FinalRun)))
      runDat$lifeStage <- 'Fail'
      ##runDat$lifeStage[is.na(runDat$forkLength)] <- 'Unassigned'
      mu <- NA
      Sigma <- NA
      ## save mixture distribution summary statistics
      mixDistMUList[[length(mixDistMUList)+1]] <<- mu
      mixDistSigmaList[[length(mixDistSigmaList)+1]] <<- Sigma
      
      names(mixDistMUList)[length(mixDistMUList)] <<- as.character(fRun)
      names(mixDistSigmaList)[length(mixDistSigmaList)] <<- as.character(fRun)
      cat(site,'\n')
      cat(min.date,'\n')
      cat(max.date,'\n')
      cat(fRun,'\n')
      cat(as.character(cond),'\n')
      assignCheck <- data.frame(site=site,minDate=min.date,maxDate=max.date,run=fRun,assignment=as.character(cond),stringsAsFactors=FALSE)
      
      write.csv(assignCheck,paste0(output.file,site,fRun,'AssignCheck.csv'),row.names=FALSE)
      
      
      cat('\n')
      cat('Life stage is being written as All, due to the error. \n')
      return(runDat)
    },
    finally={
      message('End tryCatch')
      
    }) #end tryCatch
    return(out)
  }
  
  assignNew <- ddply(DATA,~FinalRun,assignTry,G=groupN,USEWeight=USEWeight)
  
  
  
  
  ##assignNew <- ddply(DATA,~FinalRun,assignLS,G=groupN,USEWeight=USEWeight)
  
  
  DATA <- merge(assignNew[,c('id','lifeStage','days')],DATA)
  
  
  # JARED: SEEMS LIKE WE CAN ERASE THIS CALL TO MEMORYUSAGE()
  #memoryUsage()
  
  ## save data after assignment
  # JARED: WHY THIS SAVE?
  save(DATA,output.file,mixDistMUList,mixDistSigmaList,file=paste0(output.file,'newLS.Rdata'))
  
  
  assignLSCompare(DATA,...)
  
  # JARED: SEEMS LIKE WE CAN ERASE THIS CALL TO MEMORYUSAGE()
  #memoryUsage()
  
  return(DATA)
  
} # end assignLifeStage


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


# ## report the memory
# memoryUsage <- function(){
#   cat('The memory size is',memory.limit(NA),'\n')
#   cat('The max amount of memory obtained from the OS is', memory.size(TRUE),'\n')
#   cat('The current amount of memory in use is', memory.size(FALSE),'\n')
#   cat('\n')
#   cat('\n')
# }
