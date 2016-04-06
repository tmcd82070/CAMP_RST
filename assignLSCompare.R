###################################################
## Jared Studyvin
## 8 March 2016
## output comparison table and figure between biologist life stage assignment and mixture distribution results
###################################################



assignLSCompare <- function(Data,SAVE=TRUE){

    ## get needed packages
    getPackages(c('plyr','ellipse','tidyr'))

    ## order the levels of the life stage
    (LS <- as.character(unique(Data[,'lifeStage'])))
    (lvl <- c(LS[grepl('small',LS,ignore.case=TRUE)],
             LS[grepl('med',LS,ignore.case=TRUE)],
             LS[grepl('Large',LS,ignore.case=TRUE)],
             LS[grepl('^all',LS,ignore.case=TRUE)],
             LS[grepl('^unass',LS,ignore.case=TRUE)]))
    Data[,'lifeStage'] <- factor(Data[,'lifeStage'],levels=lvl,ordered=TRUE)


    ## for debugging
    ##data <- subset(Data,FinalRun=='Fall')

    compare <- function(data,save){

        ## this run
        (fRun <- as.character(data[1,'FinalRun']))

        if(grepl('unass',fRun,ignore.case=TRUE)){
            return(NULL)
        }

        (LSlvl <- as.character(unique(data[,'lifeStage'])))

        if(sum(!grepl('unass',LSlvl,ignore.case=TRUE))==0){
            return(NULL)
        }



        cat('\n')
        cat('\n')
        cat('\n')
        cat('Generating comparison results for run =',fRun,'\n')


        ## confusion matrix
        compareDF <- ddply(data,~bioLS+lifeStage,summarize,fish=sum(Unmarked))
        cvTab <- spread(compareDF,key=lifeStage,value=fish,fill=0)

        cat('Confusion Matrix \n')
        print(cvTab)

        if(save){
            write.csv(cvTab,paste0(output.file,gsub(' ','',fRun),'ConfusionMatrix.csv'),row.names=FALSE)
        }
        ##head(data)

        ## mixture distribution life stage level colors
        mixIndex <- data.frame(lifeStage=c('Small','Medium','Large','All'),col=c('red','green','blue','orange'),stringsAsFactors=FALSE)

        ## biologist life stage symbols
        bioIndex <- data.frame(bioLS=c('Fry','Parr','Smolt'),pch=1:3,stringsAsFactors=FALSE)

        ## data to be plotted now
        data <- merge(merge(data,mixIndex,all.x=TRUE),bioIndex,all.x=TRUE)

        ##nrow(plotData)
        ##nrow(data)
        ##head(plotData)

        ## add ellipse to the figure
        addEllipse <- function(run){
            ## add ellipse to the figure

            vars <- c('days','forkLength')

            mu <<- mixDistMUList[[run]]
            Sigma <- mixDistSigmaList[[run]]
            for(j in 1:ncol(mu)){
                points(ellipse(Sigma[vars,vars,j],centre=mu[vars,j]),type='l')

            } # end for j

            return(NULL)

        } # end addEllipse function


        save.image(file="C:/Users/jmitchell/Desktop/FirstLineBigLooper.RData")


        monthLabel <- data.frame(month.abb,first=c('01-01','02-01','03-01','04-01','05-01','06-01','07-01','08-01','09-01','10-01','11-01','12-01'),stringsAsFactors=FALSE)

        for(i in 1:nrow(monthLabel)){
            j <- 1;goodMonth <- FALSE

            while(!goodMonth){
                firstDay <- with(data,mean(days[format(SampleDate,'%m-%d')==paste0(formatC(i,width=2,flag=0),'-',formatC(j,width=2,flag=0))]))

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



        cat('\n')
        cat('\n')
        cat('\n')
        cat('Saving comparison figure.\n')
        if(save){
            pdf(file=paste0(output.file,gsub(' ','',fRun),'plotLifeStageAssignComparison.pdf'),width=7)
        }else{
            windows(width=7)
        }
        ##par(mfrow=c(1,2))

        ## title for figure
        varUsed <- paste(rownames(mixDistMUList[[fRun]]),collapse=", ")
        plotMain <- paste0(fRun,'\nVariables used to assign lifestage: ',gsub('days','date',varUsed))
        ## plot forklength and date
        with(data,plot(days,forkLength,ylab='Fork Length (mm)',xlab='Sample Date',col=col,pch=pch,xaxt='n',main=plotMain))
        addEllipse(fRun)
        with(monthLabel,axis(1,at=days,label=month.abb))


        ##with(data,table(col,pch))


        ## legend info for mixture life stage
        legMix <- mixIndex[mixIndex$lifeStage%in%LSlvl[!grepl('^unass',LSlvl,ignore.case=TRUE)],]
        legMix$pch <- 20

        ## legend info for biologist life stage
        biolvl <- as.character(unique(data[,'bioLS']))
        havebiolvl <- c(biolvl[grepl('^fry',biolvl,ignore.case=TRUE)],
        biolvl[grepl('^parr',biolvl,ignore.case=TRUE)],
        biolvl[grepl('^smolt',biolvl,ignore.case=TRUE)])
        legBio <- bioIndex[bioIndex$bioLS%in%havebiolvl,]
        names(legBio) <- c('lifeStage','pch')
        legBio$col <- 'black'
        legBio <- legBio[,c('lifeStage','col','pch')]
        ##legMix
        ##legBio


        ## prepares the legend so the color and pch match up correctly
        d <- nrow(legMix) - nrow(legBio)
        if(d==0){

            legAll <- rbind(legMix,legBio)

        }else if(d>0){
            addRow <- as.data.frame(matrix(NA,nrow=d,ncol=ncol(legMix)))
            names(addRow) <- names(legMix)
            legAll <- rbind(legMix,legBio,addRow)
        }else if(d<0){
            addRow <- as.data.frame(matrix(NA,nrow=abs(d),ncol=ncol(legMix)))
            names(addRow) <- names(legMix)
            legAll <- rbind(legMix,addRow,legBio)
        }
        legAll


        with(legAll,legend('topleft',legend=lifeStage,col=col,pch=pch,ncol=2,bg='white'))


        if(save){
            graphics.off()
        }

        return(NULL)
    }# end compare function

    ## for debugging
    ##ddply(Data,~FinalRun,compare,save=FALSE)

    ddply(Data,~FinalRun,compare,save=SAVE)


    return(NULL)
} # end function assignLSCompare
