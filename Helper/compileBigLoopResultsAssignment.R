

getRiverPassage <- function(thePlatform,theRiver,stemB){

  #stemB <- NULL    # clear out

#   thePlatform <- 'CAMP_RST20160601-DougXXX-4.5'
#   theRiver <- 'Feather River'
#   stemB <- '//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/Outputs'
  
  source(paste0("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",thePlatform,"/R-Interface/Helper/getTheData.R"))
  
  stemB <- paste0(stemB,"/",theRiver)#,"/doug hold")
  
  filesB <- list.files(stemB)
  ls_passageB <- filesB[grep('lifestage_passage_table.csv',filesB)]
  times <- filesB[grep('JStuffTime.csv',filesB)]
  checks <- filesB[grep('AssignCheck.csv',filesB)]
  
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
  
  
  
  
  
  
  
  getTimes <- function(stemB,times){
    
    allTimes <- NULL
    for(i in 1:length(times)){
      oneTime <- read.csv(paste0(stemB,'/',times[i]))
      allTimes <- rbind(allTimes,oneTime)
    }
    allTimes$file <- ifelse(allTimes$file == 'J0','lifestage',as.character(droplevels(allTimes$file)))
    allTimes$X <- NULL
    allTimes
  }
  
  timesDFB <- getTimes(stemB,times)
  
  #http://stackoverflow.com/questions/15897236/whats-the-equivalent-to-excels-left-and-right-in-r
  left <- function (string,char){
    substr(string,1,char)
  }
  right <- function (string, char){
    substr(string,nchar(string)-(char-1),nchar(string))
  }
  
  getChecks <- function(stemB,checks){
    
    allChecks <- NULL
    for(i in 1:length(checks)){
      oneCheck <- read.csv(paste0(stemB,'/',checks[i]))
      oneCheck$testi <- strsplit(checks[i],"--")[[1]][1]
      oneCheck$file <- right(left(checks[i],gregexpr(pattern="J",checks[i])[[1]][1] + 1),2)
      
#       if( gregexpr(pattern="Fall",checks[i])[[1]][1] > -1 ){
#         oneCheck$run <- right(left(checks[i],gregexpr(pattern="Fall",checks[i])[[1]][1] + 3),4)
#       } else if ( gregexpr(pattern="Spring",checks[i])[[1]][1] > -1 ){
#         oneCheck$run <- right(left(checks[i],gregexpr(pattern="Spring",checks[i])[[1]][1] + 5),6)        
#       } else if ( gregexpr(pattern="Late fall",checks[i])[[1]][1] > -1 ){
#         oneCheck$run <- right(left(checks[i],gregexpr(pattern="Late fall",checks[i])[[1]][1] + 8),9)        
#       } else if ( gregexpr(pattern="Winter",checks[i])[[1]][1] > -1 ){
#         oneCheck$run <- right(left(checks[i],gregexpr(pattern="Winter",checks[i])[[1]][1] + 5),6)
#       }
      
      
      allChecks <- rbind(allChecks,oneCheck)
    }
    #theTesti <- unique(allChecks$testi)
    allChecks$site <- allChecks$minDate <- allChecks$maxDate <- NULL
    allChecks
  }
  
  checksDFB <- getChecks(stemB,checks)
  
  
  
  temp1 <- merge(checksDFB,timesDFB,by=c('testi','file'),all.x=TRUE,all.y=TRUE)
  temp2 <- merge(bigDFB,temp1,by=c('testi','file','run'),all.x=TRUE)
  
  #write.csv(ame,"C:/Users/jmitchell/Desktop/ame.csv")
  temp2
}

thePlatform <- 'CAMP_RST20160601-DougXXX-4.5'

sac <- getRiverPassage(thePlatform,'Sacramento River',paste0('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/',thePlatform,'/Outputs'))
ame <- getRiverPassage(thePlatform,'American River',paste0('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/',thePlatform,'/Outputs'))
fea <- getRiverPassage(thePlatform,'Feather River',paste0('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/',thePlatform,'/Outputs'))
mok <- getRiverPassage(thePlatform,'Mokelumne River',paste0('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/',thePlatform,'/Outputs'))
sta <- getRiverPassage(thePlatform,'Stanislaus River',paste0('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/',thePlatform,'/Outputs'))
kni <- getRiverPassage(thePlatform,"Knight's Landing",paste0('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/',thePlatform,'/Outputs'))

all <- rbind(sac,ame,fea,mok,sta),kni)
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

all$bEst <- ifelse(all$bEst > 1000000000,-99,all$bEst)
all$bLCL <- ifelse(all$bLCL > 1000000000,-99,all$bLCL)
all$bUCL <- ifelse(all$bUCL > 1000000000,-99,all$bUCL)

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
