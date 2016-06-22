#' @export assignLS
#' 
#' @title assignLS
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
#' @details Need some details

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
assignLS <- function(runDat,G=NULL,USEWeight=NULL){
	cat('\n')
	cat('\n')
	cat('\n')
	cat('<^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^>','\n')
	
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
		write.csv(assignCheck,paste0(output.file,site,fRun,'AssignCheck.csv'),row.names=FALSE)
		cat('\n')
		cat('Final run is either unassigned or there is not enough fish with a forklength. Life stage is being written as unassigned. \n')
		return(runDat)
	}
	
	
	
	## number of fish with a weight
	(nW <- sum(runDat[with(runDat,!is.na(weight)),'Unmarked']))
	## number of fish with a forklength and weight
	(nFLW <- sum(runDat[with(runDat,!is.na(forkLength)&!is.na(weight)),'Unmarked']))
	## won't use weight unless enough there is enough data
	useWeight <- FALSE
	if(nW/nFL > .5 & nFLW > sample.size.forkLengthAndWeight){
		useWeight <- TRUE
	}
	
	
	if(!is.null(USEWeight)){
		useWeight <- USEWeight
	}
	
	if(useWeight){
		cat('Weight will be used in the analysis. \n')
	}else{
		cat('Weight will NOT be used in the analysis. \n')
	}
	
	
	
	cat('Min Unmarked value:',with(runDat,min(Unmarked)),'\n')
	
	
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
	
	
	memoryUsage()
	
	gc()
	
	memoryUsage()
	
	
	cat('covars:',covars)
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
	write.csv(assignCheck,paste0(output.file,site,fRun,'AssignCheck.csv'),row.names=FALSE)
	
	
	## get group names based on number of groups
	if(nGroup==1){
		groupName <- 'All'
	}else if(nGroup==2){
		groupName <- c('Small','Large')
	}else if(nGroup==3){
		groupName <- c('Small','Medium','Large')
	}else if(nGroup>3){
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
	
	
	## save mixture distribution summary statistics
	mixDistMUList[[length(mixDistMUList)+1]] <<- mu
	mixDistSigmaList[[length(mixDistSigmaList)+1]] <<- Sigma
	
	names(mixDistMUList)[length(mixDistMUList)] <<- as.character(fRun)
	names(mixDistSigmaList)[length(mixDistSigmaList)] <<- as.character(fRun)
	
	##    mixDistSigmaList
	##    mixDistMUList
	
	###################################
	## for debugging
	## save mu and Sigma
	## print('save mu and sigma')
	## print(saveName <- gsub(' ','',paste0(as.character(runDat[1,c('river','trap','year','FinalRun')]),collapse='')))
	## parm <- list(mu,Sigma)
	## save(parm,file=paste0(output.file,'parm',saveName,'.Rdata'))
	###################################
	
	
	
	head(expDat)
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
	cat('After collapsing:')
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
	
	return(runDat)
	
} # end assignLS


