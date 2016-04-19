source('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/R-Interface/Helper/getTheData.R')

getRiverPassage <- function(thePlatform,theRiver,stemB){

  # thePlatform <- 'CAMP_RST20160601-DougXXX-4.5'
  # theRiver <- 'Sacramento River'
  # stemB <- '//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/Outputs'
  
  stemB <- paste0(stemB,"/",theRiver)#,"/doug hold")
  
  filesB <- list.files(stemB)
  ls_passageB <- filesB[grep('lifestage_passage_table.csv',filesB)]
  #passageB <- filesB[grep('passage_table.csv',filesB)]
  
  openTheseB <- unique(data.frame(file=c(ls_passageB),stringsAsFactors=FALSE))
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
  bigDFB
}

sac <- getRiverPassage('CAMP_RST20160201','Sacramento River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
ame <- getRiverPassage('CAMP_RST20160201','American River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
fea <- getRiverPassage('CAMP_RST20160201','Feather River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
mok <- getRiverPassage('CAMP_RST20160201','Mokelumne River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
sta <- getRiverPassage('CAMP_RST20160201','Stanislaus River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
kni <- getRiverPassage('CAMP_RST20160201',"Knight's Landing",'//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')

all <- rbind(sac,ame,fea,mok,sta,kni)
nrow(all)
all <- all[!is.na(all$bEst),]
all <- all[all$bEst > 0,] 
rownames(all) <- NULL
nrow(all)
all$time <- ifelse(is.na(all$time),'--',all$time)
all$lifeStage <- as.character(droplevels(all$lifeStage))
all$lifeStage <- ifelse(is.na(all$lifeStage),'--',all$lifeStage)

all <- all[order(all$river,all$siteName,all$max.date,all$file,all$run,all$lifeStage,all$time),]
all$bMag <- all$bOOL <- all$sequence <- NULL

options(scipen=999)
all$bEst <- format(round(as.numeric(all$bEst), 0),nsmall=0,big.mark=",")#,scientific=FALSE) 
all$bLCL <- format(round(as.numeric(all$bLCL), 0),nsmall=0,big.mark=",")#,scientific=FALSE) 
all$bUCL <- format(round(as.numeric(all$bUCL), 0),nsmall=0,big.mark=",")

write.csv(all,"C:/Users/jmitchell/Desktop/allEstsCompare.csv")




sac <- getRiverPassage('CAMP_RST20160201','Sacramento River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
ame <- getRiverPassage('CAMP_RST20160201','American River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
fea <- getRiverPassage('CAMP_RST20160201','Feather River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
mok <- getRiverPassage('CAMP_RST20160201','Mokelumne River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
sta <- getRiverPassage('CAMP_RST20160201','Stanislaus River','//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')
kni <- getRiverPassage('CAMP_RST20160201',"Knight's Landing",'//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs')

allBefore <- rbind(sac,ame,fea,mok,sta,kni)
nrow(allBefore)
allBefore <- allBefore[!is.na(allBefore$bEst),]
allBefore <- allBefore[allBefore$bEst > 0,] 
rownames(allBefore) <- NULL
nrow(allBefore)
allBefore$time <- ifelse(is.na(allBefore$time),'--',allBefore$time)
allBefore$lifeStage <- as.character(droplevels(allBefore$lifeStage))
allBefore$lifeStage <- ifelse(is.na(allBefore$lifeStage),'--',allBefore$lifeStage)

compare <- merge(all,allBefore,by=c("by","river","siteName","min.date","max.date","file","run","lifeStage","time"),all.x=TRUE,all.y=TRUE)


write.csv(compare,"//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/R-Interface/allEstsCompare.csv")
