#' @export 
#'
#' @title assignLifeStage
#'
#' @description
#'
#' This function will be called within F.get.catch.data The purpose is to
#' replace the life stage column with an updated assignment based on a
#' clustering routine.
#' 
#' 
#' 
#' 
#' @param DATA A data frame of the catch data with the weight measurement, as
#'   returned from \code{\link{getCatchDataWeight}}.
#' @param groupN The number of life stage groups to be estimated, see details.
#' @param USEWeight Indicate whether weight should not (FALSE) be used in the
#'   analytical assignment or allow (NULL) weight to be used, see details.
#' @param output.file A text string indicating a prefix to append to all output.
#'   
#' @param ... Arguments passed to \code{\link{assignLSCompare}}.
#'   
#' @details The function expects the data frame DATA to have column names:
#' \code{lifeStage}, \code{SampleDate}, \code{FinalRun}, \code{forkLength}, \code{weight}, \code{Unmarked}. The
#' \code{lifeStage} column is overwritten with the new analytical life stage
#' assignment.
#' 
#' The life stage assignment is done by each unique value in the \code{FinalRun}
#' column. If the final run is unassigned, no estimation is done and the life
#' stage is set to Unassigned.
#' 
#' This function relies on several global variables as set by
#' \code{\link{GlobalVars}}: \code{sample.size.forkLength},
#' \code{sample.size.forkLengthAndWeight}, \code{weight.prop.forkLength},
#' \code{forkLength.mean.diff}, see \code{\link{GlobalVars}} for default values.
#' 
#' If the number of fish with a fork length value is less than
#' \code{sample.size.forkLength} the life stage is set to Unassigned, else the
#' life stage assignment will continue.
#' 
#' If \code{USEWeight=FALSE} the life stage is analytically assigned using fork
#' length and date only. If \code{USEWeight=NULL} the weight variable is also
#' used to assign life stage if the number of fish with a weight measurement
#' divided by the number of fish with a fork length measurement is greater than
#' \code{weight.prop.forkLength} AND the number of fish with a weight and fork
#' length measurements is greater than \code{sample.size.forkLengthAndWeight}.
#' It is not recommend for the user to specify \code{USEWeight=TRUE}, if done,
#' weight is only used if the number of fish with a weight measurement is
#' greater than \code{sample.size.forkLengthAndWeight} else weight is not used.
#' In all cases fork length and date are used in the life stage assignment and
#' based on user input and these conditions weight might also be used to assign
#' life stage.
#' 
#' \code{groupN} may take the values 2, 3, or NULL. If 2 or 3 the analytical
#' life stage is done with that number of groups. If 2, the group names are
#' Small and Large, if 3 the group names are Small, Medium, and Large. These
#' names are chosen to distinguish from the morphometric life stage names. If
#' \code{groupN=NULL} the analytical life stage begins with 3 groups. If the
#' minimum pairwise fork length mean difference is less than
#' \code{forkLength.mean.diff} the number of groups fit is reduced by one. In
#' this protocol the number of groups can be one, in which case the group name
#' is All.
#' 
#' The analytical assignment is done through a call to the
#' \code{\link[mclust]{Mclust}} function in the \code{mclust} package. The
#' \code{Mclust} function is fitting a mixture of multivariate normal
#' distributions. The number of distribution fit corresponds to the number of
#' life stage groups. The \code{Mclust} function returns mean vectors and
#' variance covariance matrices for each group and the group member ship for
#' each fish used in the mixture distribution estimation. The group names are
#' assigned based on the means of the fork length, smallest mean is label as
#' Small, etc.
#' 
#' In some cases not all of the fish are used in the mixture distribution
#' estimation. For example if weight is to be used, there could be some fish
#' without a weight measurement and complete observations are needed for the
#' \code{Mclust} function. For these fish a minimum Mahalanobis distance is used
#' for the life stage assignment. For each fish not used in the mixture
#' distribution estimation, but have at least a fork length or weight, the
#' Mahalanobis distance from the fish to each of the group means is calculated.
#' The minimum distance indicates which group that fish will be assigned too.
#' The concept is that all fish with a recorded fork length will have a group
#' assignment.
#' 
#' The analytical and morphometric life stage assignments are compared with a
#' call to \code{\link{assignLSCompare}}.
#' 
#' @return The data frame DATA is returned with the \code{lifeStage} column
#'   being updated by the analytical assignment.
#'   
#' @author Jared Studyvin, WEST Inc.
#'   
#' @seealso \code{\link{assignLSCompare}}, \code{\link{GlobalVars}}
#'   
#' @examples
#' \dontrun{
#' assignLifeStage(DATA,groupN,USEWeight)
#' 
#' }
###############################################
## Jared Studyvin
## 10 Feb 2016
## Assign life stage
###############################################

## This function will be called within F.get.catch.data
## The purpose is to replace the life stage column with an updated assignment based on a clustering routine



assignLifeStage <- function(DATA,groupN=1,USEWeight=NULL,output.file=output.file,...){
### DATA = the catch data
### groupN = the number of life stage groups to fit, NULL allows the program to decide
### USEWeight = should weight be used in the mixture distribution, NULL allows the program to decide



    ## This is the environment for the global variables
    .mycampREnv <- .GlobalEnv
    ## get the global variables
    site <- get('site',envir=.mycampREnv)
    min.date<- get('min.date',envir=.mycampREnv)
    max.date <- get('max.date',envir=.mycampREnv)
    sample.size.forkLength <- get('sample.size.forkLength',envir=.mycampREnv)
    sample.size.forkLengthAndWeight <- get('sample.size.forkLengthAndWeight',envir=.mycampREnv)
    weight.prop.forkLength <- get('site',envir=.mycampREnv)
    forkLength.mean.diff <- get('forkLength.mean.diff',envir=.mycampREnv)
    #output.dir <- get('output.dir',envir=.mycampREnv)


    ##	JARED:  WHY IS THIS SAVE HERE?  IS IT NECESSARY?
    ## This should be done until the next release
    ## save data before assignment
    #save(DATA,site,min.date,max.date,sample.size.forkLength,sample.size.forkLengthAndWeight,weight.prop.forkLength ,forkLength.mean.diff,output.dir,file=paste0(output.dir,'DATA.Rdata'))



  # DATA <- catchFishing
  # groupN <- nls
  # USEWeight <- NULL
  # output.file <- output.file
    
  #   ---- Jason: 10/25/2016.  The update to the Platform expects an nls=1 when the program is to decide 
  #   ---- the number of groups.  Jared originally coded this to NULL.  So, remap the "1" to NULL.
  if(groupN == 1){
    groupN <- NULL
  }

  cat(groupN)
    
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
  #  ---- jason chooses jared's updates from branch jaredDoc - 8-11-2016.
  # JARED: CAN THESE <<- BE CHANGED TO <- ?
  ##mixDistMUList <<- list()
  ##mixDistSigmaList <<- list()
####################################################################
####################################################################
####################################################################
    ## This is the work horse function that does the assignment for each run
    assignLS <- function(runDat,G=NULL,USEWeight=NULL){
	cat('\n')
	cat('\n')
	cat('\n')
	cat('<^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^>','\n')


        ## This is the environment for the global variables
        .mycampREnv <- .GlobalEnv
        ## get the global variables
        site <- get('site',envir=.mycampREnv)
        min.date<- get('min.date',envir=.mycampREnv)
        max.date <- get('max.date',envir=.mycampREnv)
        sample.size.forkLength <- get('sample.size.forkLength',envir=.mycampREnv)
        sample.size.forkLengthAndWeight <- get('sample.size.forkLengthAndWeight',envir=.mycampREnv)
        weight.prop.forkLength <- get('site',envir=.mycampREnv)
        forkLength.mean.diff <- get('forkLength.mean.diff',envir=.mycampREnv)



	## keep only needed columns
	runDat <- runDat[,c('id','SampleDate','FinalRun','forkLength','weight','Unmarked')]

	runDat$days <- with(runDat,as.numeric(difftime(SampleDate,min(SampleDate),units='days')))

	## final run
	(fRun <- with(runDat,unique(FinalRun)))

	cat('Assigning life stage for run:', as.character(fRun),'\n')
	cat('\n')

        ## this dataframe is for testing purposes
	assignCheck <- data.frame(site=site,minDate=min.date,maxDate=max.date,run=fRun,stringsAsFactors=FALSE)


	## number of fish with a forklength
	(nFL <- sum(runDat[with(runDat,!is.na(forkLength)),'Unmarked']))

	## if final run is unassigned OR if number of fish with fork length <100
	## then make life stage unassigned and return
	if(grepl('unassign',fRun,ignore.case=TRUE)| nFL<sample.size.forkLength){
            runDat$lifeStage <- 'Unassigned'

            assignCheck$assignment <- 'low sample size/unassigned run'
            #write.csv(assignCheck,paste0(output.file,site,fRun,'AssignCheck.csv'),row.names=FALSE)
            cat('\n')
            cat('Final run is either unassigned or there is not enough fish with a forklength. Life stage is being written as unassigned. \n')
            return(list(runDat=runDat,mu=NA,Sigma=NA,run=fRun))
	}



	## number of fish with a weight
	(nW <- sum(runDat[with(runDat,!is.na(weight)),'Unmarked']))
	## number of fish with a forklength and weight
	(nFLW <- sum(runDat[with(runDat,!is.na(forkLength)&!is.na(weight)),'Unmarked']))
	## won't use weight unless enough there is enough data
	useWeight <- FALSE
	if(nW/nFL > weight.prop.forkLength & nFLW > sample.size.forkLengthAndWeight){
            useWeight <- TRUE
	}

        ## allow some user control if weight is to be used
	if(!is.null(USEWeight)){
            useWeight <- USEWeight
	}


        if(useWeight & nW<=sample.size.forkLengthAndWeight){
            useWeight <- FALSE
            message('I know you requested to use weight in the life stage assignment but there is not adequate sample to do so.')
        }



	if(useWeight){
            cat('Weight will be used in the analysis. \n')
	}else{
            cat('Weight will NOT be used in the analysis. \n')
	}

        cat('Min Unmarked value:',with(runDat,min(Unmarked)),'\n')


        expandUnmarked <- function(dat,colKeep,colRep){
            ## replicate the rows of a dataframe based on one of the columns
            ## dat = data frame
            ## colKeep = string vector of the columns names of the data to be repeated
            ## colRep = single string of the frequency to repeat the data
            repeatRow <- function(data,col.Keep,col.Rep){
                row <- data[,col.Keep]
                rep <- data[,col.Rep]
                out <- do.call('rbind',replicate(rep,row,simplify=FALSE))
                return(out)
            }

            outData <- adply(dat,1,.fun=repeatRow,col.Keep=colKeep,col.Rep=colRep)
            outData[,colRep] <- NULL
            return(outData)
        } # end expandUnmarked function

	cat('Expanding data \n\n')
	expDat <- expandUnmarked(runDat,c('id','forkLength','weight','days'),'Unmarked')

### maybe remove runDat here

	## subset data to be used for analysis
	if(useWeight){
            ## use these rows for analysis
            inRow <- with(expDat,!is.na(forkLength)& !is.na(weight))
            ## use these columns for analysis
            covars <- c('forkLength','weight','days')
            ## columns to keep when collapsing data
            colExp <- c('id',covars,'group')
	}else{
            ## use these rows for analysis
            inRow <- with(expDat,!is.na(forkLength))
            ## use these columns for analysis
            covars <- c('forkLength','days')
            ## columns to keep when collapsing data
            colExp <- c('id',covars,'group')
	}

	cat('nrow(expDat):',nrow(expDat),'\n')
	cat('sum(inRow):',sum(inRow),'\n')


	pairDiff <- function(vec){
            if(length(vec)<2){
                return(vec)
            }
            comb <- combn(vec,2)
            d <- matrix(c(1,-1),nrow=1)
            return(as.vector(abs(d%*%comb)))
	}

	## need the cluster means to be at least this far apart in the forklength dimension otherwise the number of groups is reduced
	## The reduction only happens if the number of groups is not specified by the user
	minMeanDiff <- forkLength.mean.diff


	## user overwrite choice of number of groups
	if(!is.null(G)){
            nGroup <- G
            goodClust <- TRUE

            ## This is for testing purposes
            assignCheck$assignment <- paste('The set number of groups is',G)
            ##
	}else{
            nGroup <- 3 #start by fitting three groups
            goodClust <- FALSE
	}



	cat('Starting Mclust \n')



	cat('covars:',covars,'\n')
	cat('\n')
	cat('\n')
	cat('\n')

	## fit cluster with user specified number of groups
	if(goodClust){
            clust <- Mclust(data=expDat[inRow,covars],G=nGroup,mclust.options("emModelNames"))
	}else{ #else start with 3 groups
            clust <- Mclust(data=expDat[inRow,covars],G=nGroup,mclust.options("emModelNames"))
            (meanFL <- clust[['parameters']]$mean['forkLength',])

            ## fit 2 groups if means are close
            if(min(pairDiff(meanFL))<minMeanDiff){
                nGroup <- nGroup-1
                clust <- Mclust(data=expDat[inRow,covars],G=nGroup,mclust.options("emModelNames"))
                (meanFL <- clust[['parameters']]$mean['forkLength',])

                ## fit 1 group if means are close
                if(min(pairDiff(meanFL))<minMeanDiff){
                    nGroup <- nGroup-1
                    clust <- Mclust(data=expDat[inRow,covars],G=nGroup,mclust.options("emModelNames"))
                    (meanFL <- clust[['parameters']]$mean['forkLength',])
                } # end fit 1 groups

            } # end fit 2 groups


	}# end else (start with 3 groups)


	## allow computer to choose
	## while(!goodClust){
	##     clustTemp <- Mclust(data=expDat[inRow,covars],G=nGroup,mclust.options("emModelNames"))
	##     (meanFL <- clust[['parameters']]$mean['forkLength',])

	##     if(min(pairDiff(meanFL))<minMeanDiff){
	##         print(nGroup)
	##         if(nGroup == 1){
	##             goodClust <- TRUE
	##         }else{
	##             nGroup <- nGroup-1
	##         }
	##         ## if(nGroup==1){
	##         ##     runDat$lifeStage <- 'Unassigned'
	##         ##     runDat[with(runDat,!is.na(forkLength)),'lifeStage'] <- 'Medium'
	##         ##     return(runDat)
	##         ## }
	##     }else{
	##         goodClust <- TRUE
	##     }
	## }

	##summary(clustTemp)

	cat('Mclust is finished \n')
	##summary(clust)


	cat('Number of groups fit in the analysis:',nGroup,'\n')
	cat('\n')

	## This is for testing purposes
	if(is.null(assignCheck$assignment)){
            assignCheck$assignment <- paste('The final number of groups is',nGroup)
	}


        ########################
        ## This might not be needed when we get to the next release, but it is nice for testing.
	write.csv(assignCheck,paste0(output.file,site,fRun,'AssignCheck.csv'),row.names=FALSE)



	## get group names based on number of groups
	if(nGroup==1){
            groupName <- 'All'
	}else if(nGroup==2){
            groupName <- c('Small','Large')
	}else if(nGroup==3){
            groupName <- c('Small','Medium','Large')
	}else if(nGroup>3&nGroup%%1==0){
            groupName <- c('Small',paste0('Medium',1:(nGroup-2)),'Large')
	}else{
            stop(paste0('The number of groups to be fit is ',nGroup,'. This is a problem.'))
	}

	cat('The group labels: \n')
	cat(groupName,'\n')
	cat('\n')



	## array for var-cov matrices, last dim indicates groups
	(Sigma <- clust[['parameters']]$variance$sigma)
	## array of mean vectors, last dim indicates groups
	(mu <- clust[['parameters']]$mean)

	## give Sigma and mu names if null
	if(is.null(rownames(Sigma))|is.null(colnames(Sigma))){
            rownames(Sigma) <- colnames(Sigma) <- covars
	}

	if(is.null(rownames(mu))){
            rownames(mu) <- covars
	}




	##head(expDat)
	expDat[inRow,'group'] <- clust[['classification']]

	## this these did not get an assignment
	expDat[with(expDat,is.na(group)),'group'] <- -1


	##ddply(expDat,~group,summarize,FL=mean(forkLength))


	##row <- subset(expDat,id%in%c(1,2))
	collapseRow <- function(row){

            row[,'Unmarked'] <- nrow(row)
            if(nrow(row)==1){
                return(row)
            }
            if(with(row,length(unique(group))==1)){

                return(row[1,])
            }

            row[,'group'] <- -1
            return(row[1,])
	}

	cat('nrow(expDat):',nrow(expDat),'\n')
	cat('Before collapsing:\n')
	cat('nrow(runDat):',nrow(runDat),'\n')
	cat('with(runDat,sum(Unmarked)):',with(runDat,sum(Unmarked)),'\n')
	## this is the collapse data
	## should have the same number of rows at runDat
	runDat <- ddply(expDat,~id,collapseRow)
	cat('After collapsing:\n')
	cat('nrow(runDat):',nrow(runDat),'\n')
	cat('with(runDat,sum(Unmarked)):',with(runDat,sum(Unmarked)),'\n')


	## This is not needed any more, remove to not take up memory
	expDat <- NULL
	cat('Done with collapsing the data.\n')


	## ddply(collapseDat,~group,summarize,FL=mean(forkLength,na.rm=TRUE))
	## mu

	##with(runDat,sum(is.na(forkLength)))

	## for debugging
	## M <- mu
	## S <- Sigma
	## dat <- collapseDat
	## w <- haveFLnoW
	## varHave <- 'forkLength'
	## rm(M,S,dat,w)
	malDistAssign <- function(dat,w,M,S,varHave){
            ##dat = whole data frame
            ##w = logical vector of where assignment needs to be done
            ## M = mean matrix from Mclust
            ## S = variance covariance array from Mclust
            nG <- dim(M)[2]
            malMat <- matrix(NA,nrow=sum(w),ncol=nG)
            X <- t(as.matrix(dat[w,c(varHave,'days')]))
            for(i in 1:nG){
                m <- as.matrix(M[c(varHave,'days'),i])
                s <- as.matrix(S[c(varHave,'days'),c(varHave,'days'),i])
                malMat[,i] <- aaply(1:dim(X)[2],1,function(y,X,m,s){
                                        ##print(X[,y])
                                        sqrt(t(X[,y]-m)%*%solve(s)%*%(X[,y]-m))
                                    },X=X,m=m,s=s)
            }

            dat[w,'group'] <- aaply(malMat,1,which.min)
            ddply(dat[w,],~group,summarize,FL=mean(forkLength),sdFL=sd(forkLength))
            # ddply(dat[w,],~group,summarize,FL=mean(dat[w,]$forkLength),sdFL=sd(dat[w,]$forkLength))  # jason changes forkLength to 
                                                                                                     # dat[w,]$forkLength to make check() happy.
            return(dat)
	} # end malDistAssign



	## If weight was used some fish may only have a weight or forklength
	## This if statement assigns lifestage to fish with either a weight or a forklength


	if(useWeight){
            ## where there is a weight but no forklength
            haveWnoFL <- with(runDat,group==-1&is.na(forkLength)&!is.na(weight))

            if(sum(haveWnoFL)>0){
                runDat <- malDistAssign(dat=runDat,w=haveWnoFL,M=mu,S=Sigma,varHave='weight')
            }

            ## where there is a forklength but no weight
            haveFLnoW <- with(runDat,group==-1&is.na(weight)&!is.na(forkLength))

            if(sum(haveFLnoW)>0){
                runDat <- malDistAssign(dat=runDat,w=haveFLnoW,M=mu,S=Sigma,varHave='forkLength')
            }


	} #end if useWeight


	## this matches up the group number to the group name
	(labelToName <- data.frame(group=c(order(mu['forkLength',]),-1),lifeStage=c(groupName,'Unassigned')))

	##ddply(collapseDat[haveFLnoW,],~group,summarize,FL=mean(forkLength,na.rm=TRUE))


	runDat <- merge(runDat,labelToName,all.x=TRUE)
	runDat$lifeStage <- factor(runDat$lifeStage,levels=c(groupName,'Unassigned'),ordered=TRUE)
	cat('with(runDat,sum(is.na(forkLength))):',with(runDat,sum(is.na(forkLength))),'\n')

	print(head(runDat))
        out <- list(runDat=runDat,mu=mu,Sigma=Sigma,run=fRun)

	return(out)

    } # end assignLS






####################################################################



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
                            ##mixDistMUList[[length(mixDistMUList)+1]] <<- mu
                            ##mixDistSigmaList[[length(mixDistSigmaList)+1]] <<- Sigma

                            ##names(mixDistMUList)[length(mixDistMUList)] <<- as.character(fRun)
                            ##names(mixDistSigmaList)[length(mixDistSigmaList)] <<- as.character(fRun)
                            cat(site,'\n')
                            cat(min.date,'\n')
                            cat(max.date,'\n')
                            cat(fRun,'\n')
                            cat(as.character(cond),'\n')
                            assignCheck <- data.frame(site=site,minDate=min.date,maxDate=max.date,run=fRun,assignment=as.character(cond),stringsAsFactors=FALSE)

                            write.csv(assignCheck,paste0(output.file,site,fRun,'AssignCheck.csv'),row.names=FALSE)


                            cat('\n')
                            cat('Life stage is being written as Fail, due to the error. \n')
                            return(list(runDat=runDat,mu=NA,Sigma=NA,run=fRun))
                        },
                        finally={
                            message('End tryCatch')

                        }) #end tryCatch
        return(out)
    }

    assignList <- dlply(DATA,~FinalRun,assignTry,G=groupN,USEWeight=USEWeight)


    ## extract list information
    assignNew <- ldply(assignList,function(x){x[['runDat']]})
    muList <- llply(assignList,function(x){x$mu})
    sigmaList <- llply(assignList,function(x){x$Sigma})




    ## merge life stage assignment into original data
    DATA <- merge(assignNew[,c('id','lifeStage','days')],DATA)


    ## save data after assignment
    ## JARED: WHY THIS SAVE?
    ## This is for debugging and should be removed right before the next release
    #save(DATA,output.file,muList,sigmaList,file=paste0(output.file,'newLS.Rdata'))


    assignLSCompare(DATA,muLIST=muList,sigmaLIST=sigmaList,output.file=output.file,...)


    return(DATA)

} # end assignLifeStage
