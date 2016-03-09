###################################################
## Jared Studyvin
## 8 March 2016
## output comparison table and figure between biologist life stage assignment and mixture distribution results
###################################################


assignLSCompare <- function(data,save=TRUE){


    getPackages('ellipse')


    ## confusion matrix
    cvTab <- with(data,table(bioLS,lifeStage,useNA='ifany'))
    cat('Confusion Matrix \n')
    print(cvTab)
    if(save){
        write.csv(cvTab,paste0(output.file,'ConfusionMatrix.csv'))
    }
    head(data)

    mixIndex <- data.frame(lifeStage=c('Small','Medium','Large'),col=c('red','green','blue'),stringsAsFactors=FALSE)
    bioIndex <- data.frame(bioLS=c('Fry','Parr','Smolt'),pch=1:3,stringsAsFactors=FALSE)
    plotIndex <- cbind(mixIndex,bioIndex)


    plotData <- merge(merge(data,mixIndex,all.x=TRUE),bioIndex,all.x=TRUE)

    nrow(plotData)
    nrow(data)
    head(plotData)

    addEllipse <- function(type='forkLength'){

        ## if type is not a variable do nothing
        if(!type%in%rownames(mixDistMUList[[1]])){
            return(NULL)
        }

        vars <- c('days',type)

        for(i in seq_along(mixDistMUList)){
            mu <- mixDistMUList[[i]]
            Sigma <- mixDistSigmaList[[i]]
            for(j in 1:ncol(mu)){
                points(ellipse(Sigma[vars,vars,j],centre=mu[vars,j]),type='l')

            } # end for j
        } # end for i
        return(NULL)
    } # end addEllipse function





    monthLabel <- data.frame(month.abb,first=c('01-01','02-01','03-01','04-01','05-01','06-01','07-01','08-01','09-01','10-01','11-01','12-01'),stringsAsFactors=FALSE)

    for(i in 1:nrow(monthLabel)){
        j <- 1;goodMonth <- FALSE

        while(!goodMonth){
            firstDay <- with(plotData,mean(days[format(SampleDate,'%m-%d')==paste0(formatC(i,width=2,flag=0),'-',formatC(j,width=2,flag=0))]))

            if(!is.na(firstDay)){
                monthLabel[i,'days'] <- firstDay-(j-1)
                goodMonth <- TRUE
            }else if(j>31){
                goodMonth <- TRUE
                monthLabel[i,'days'] <- NA
            }else{
                j <- j+1
            }

        } # end while
    } # end for i


    monthLabel

    leg <- "with(plotIndex,legend('topleft',legend=c(lifeStage,bioLS),col=c(col,rep('black',3)),pch=c(rep(20,3),pch),ncol=2,bg='white'))"

    cat('\n')
    cat('\n')
    cat('\n')
    cat('Saving comparison figure.\n')
    if(save){
        pdf(file=paste0(output.file,'plotLifeStageAssignComparison.pdf'),width=14)
    }else{
        windows(width=14)
    }
    par(mfrow=c(1,2))

    ## plot forklength and date
    with(plotData,plot(days,forkLength,ylab='Fork Length (mm)',xlab='Sample Date',col=col,pch=pch,xaxt='n'))
    addEllipse('forkLength')
    eval(parse(text=leg))
    with(monthLabel,axis(1,at=days,label=month.abb))


    if(nrow(subset(plotData,!is.na(weight)))>0){
        ## plot weight and date
        with(plotData,plot(days,weight,col=as.character(col),pch=pch,xlab='Sample Date',ylab='Weight (g)',xaxt='n'))
        addEllipse(type='weight')
        eval(parse(text=leg))
        with(monthLabel,axis(1,at=days,label=month.abb))
    }

    if(save){
        graphics.off()
    }

    return(NULL)
} # end function assignLSCompare
