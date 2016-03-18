

# we want to compile all of the confidence intervals in a sample with an exploding right endpoint.

# # 1.  fill out this stuff.
# thePlatform <- 'CAMP_RST20160201'
# theRiver <- 'Sacramento River'
# #files <- list.files(paste0('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/',thePlatform,'/Outputs/',theRiver))


# get the before
# 2. fill out stemB
stemB <- '//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs/Sacramento River/After Knot Adj'
filesB <- list.files(stemB)
ls_passageB <- filesB[grep('lifestage_passage_table.csv',filesB)]
passageB <- filesB[grep('passage_table.csv',filesB)]

openTheseB <- unique(data.frame(file=c(ls_passageB,passageB),stringsAsFactors=FALSE))
for(i in 1:nrow(openTheseB)){
  if(substr(openTheseB$file[i],nchar(openTheseB$file[i]) - 26,nchar(openTheseB$file[i]) - 26 + 3) == 'life'){
    openTheseB$type[i] <- 'life'
  } else if(substr(openTheseB$file[i],nchar(openTheseB$file[i]) - 20,nchar(openTheseB$file[i]) - 20 + 2) == 'run'){
    openTheseB$type[i] <- 'run'
  } else {
    openTheseB$type[i] <- 'summary'
  }
}

bigDFB <- getTheData(openThese=openTheseB,thePlatform=thePlatform,theRiver=theRiver,stem=stemB,before=TRUE)

testByB <- unlist(strsplit(bigDFB$by,"--",fixed=TRUE))
bigDFB$testi <- testByB[c(TRUE,FALSE)]
bigDFB$by <- testByB[c(FALSE,TRUE)]
bigDFB$time <- ifelse(is.na(bigDFB$time),'--',bigDFB$time)
bigDFB$lifeStage <- as.character(droplevels(bigDFB$lifeStage))
bigDFB$lifeStage <- ifelse(is.na(bigDFB$lifeStage),'--',bigDFB$lifeStage)


# get the after
stemA <-  '//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs/Sacramento River/After Times 2 for Eff'
filesA <- list.files(stemA)
ls_passageA <- filesA[grep('lifestage_passage_table.csv',filesA)]
passageA <- filesA[grep('passage_table.csv',filesA)]

openTheseA <- unique(data.frame(file=c(ls_passageA,passageA),stringsAsFactors=FALSE))
for(i in 1:nrow(openTheseA)){
  if(substr(openTheseA$file[i],nchar(openTheseA$file[i]) - 26,nchar(openTheseA$file[i]) - 26 + 3) == 'life'){
    openTheseA$type[i] <- 'life'
  } else if(substr(openTheseA$file[i],nchar(openTheseA$file[i]) - 20,nchar(openTheseA$file[i]) - 20 + 2) == 'run'){
    openTheseA$type[i] <- 'run'
  } else {
    openTheseA$type[i] <- 'summary'
  }
}

bigDFA <- getTheData(openThese=openTheseA,thePlatform=thePlatform,theRiver=theRiver,stem=stemA,before=FALSE)

testByA <- unlist(strsplit(bigDFA$by,"--",fixed=TRUE))
bigDFA$testi <- testByA[c(TRUE,FALSE)]
bigDFA$by <- testByA[c(FALSE,TRUE)]
bigDFA$time <- ifelse(is.na(bigDFA$time),'--',bigDFA$time)
bigDFA$lifeStage <- as.character(droplevels(bigDFA$lifeStage))
bigDFA$lifeStage <- ifelse(is.na(bigDFA$lifeStage),'--',bigDFA$lifeStage)



bb <- function(x){
  table(x,exclude=NULL)
}

bb(bigDFB$by)
bb(bigDFB$river)
bb(bigDFB$siteName)
bb(bigDFB$min.date)
bb(bigDFB$max.date)
bb(bigDFB$file)
bb(bigDFB$run)
bb(bigDFB$lifeStage)
bb(bigDFB$time)

bb(bigDFA$by)
bb(bigDFA$river)
bb(bigDFA$siteName)
bb(bigDFA$min.date)
bb(bigDFA$max.date)
bb(bigDFA$file)
bb(bigDFA$run)
bb(bigDFA$lifeStage)
bb(bigDFA$time)

# double folder compilation
bigDF <- merge(bigDFB,bigDFA,by=c('by','river','siteName','min.date','max.date','file','run','lifeStage','time'),all.x=TRUE,all.y=TRUE)  # 'test.i' removed
bigDF <- bigDF[!is.na(bigDF$aEst) | !is.na(bigDF$bEst),]
bigDF <- bigDF[ ( (bigDF$aEst > 0 & !is.na(bigDF$aEst)) & (bigDF$bEst > 0 & !is.na(bigDF$bEst)) ) | ( is.na(bigDF$aEst) & (bigDF$bEst > 0 & !is.na(bigDF$bEst)) ) | ( (bigDF$aEst > 0 & !is.na(bigDF$aEst)) & is.na(bigDF$bEst) )  > 0,]
bigDF$passC <- bigDF$aEst/bigDF$bEst #round((bigDF$aEst - bigDF$bEst) / bigDF$bEst * 100,2)
bigDF$diffMag <- bigDF$bMag - bigDF$aMag
rownames(bigDF) <- NULL

write.csv(bigDF,'//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs/Sacramento River/bigTimes2DF.csv')





# single folder compilation
bigDF <- bigDFB
bigDF <- bigDF[!is.na(bigDF$bEst),]
bigDF <- bigDF[ bigDF$bEst > 0,]
rownames(bigDF) <- NULL

write.csv(bigDF,'//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs/Sacramento River/bigGapsDF.csv')



bigDF$bEst <- bigDF$bLCL <- bigDF$bUCL <- bigDF$bMag <- bigDF$bOOL <- bigDF$sequence.y <- bigDF$testi.y <- bigDF$passC <- bigDF$diffMag <- NULL
bigDF$sequence.x <- bigDF$testi.x <- NULL





# names(bigDF)[names(bigDF) == 'bEst'] <- 'aEst200'
# names(bigDF)[names(bigDF) == 'bLCL'] <- 'aLCL200'
# names(bigDF)[names(bigDF) == 'bUCL'] <- 'aUCL200'
# names(bigDF)[names(bigDF) == 'bMag'] <- 'aMag200'
# names(bigDF)[names(bigDF) == 'bOOL'] <- 'aOOL200'

names(bigDF)[names(bigDF) == 'aEst'] <- 'aEst5000'
names(bigDF)[names(bigDF) == 'aLCL'] <- 'aLCL5000'
names(bigDF)[names(bigDF) == 'aUCL'] <- 'aUCL5000'
names(bigDF)[names(bigDF) == 'aMag'] <- 'aMag5000'
names(bigDF)[names(bigDF) == 'aOOL'] <- 'aOOL5000'

bigDF0 <- read.csv('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs/Sacramento River/before/bigDF.csv')

names(bigDF0)[names(bigDF0) == 'bEst'] <- 'bEst100'
names(bigDF0)[names(bigDF0) == 'bLCL'] <- 'bLCL100'
names(bigDF0)[names(bigDF0) == 'bUCL'] <- 'bUCL100'
names(bigDF0)[names(bigDF0) == 'bMag'] <- 'bMag100'
names(bigDF0)[names(bigDF0) == 'bOOL'] <- 'bOOL100'

names(bigDF0)[names(bigDF0) == 'aEst'] <- 'aEst100'
names(bigDF0)[names(bigDF0) == 'aLCL'] <- 'aLCL100'
names(bigDF0)[names(bigDF0) == 'aUCL'] <- 'aUCL100'
names(bigDF0)[names(bigDF0) == 'aMag'] <- 'aMag100'
names(bigDF0)[names(bigDF0) == 'aOOL'] <- 'aOOL100'

bigDF2 <- bigDF # 5000

BigDF <- merge(bigDF0,bigDF,by=c('by','river','siteName','min.date','max.date','file','run','lifeStage','time'),all.x=TRUE,all.y=TRUE)

BigDF2 <- merge(BigDF,bigDF2,by=c('by','river','siteName','min.date','max.date','file','run','lifeStage','time'),all.x=TRUE,all.y=TRUE)

write.csv(BigDF2,'//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs/Sacramento River/before/bigDF5000.csv')
