getRiverPassage <- function(thePlatform,theRiver,stemB){

#   thePlatform <- 'CAMP_RST20160201'
#   theRiver <- 'Feather River'
#   stemB <- '//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs'
  
  stemB <- paste0(stemB,"/",theRiver)
  
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

write.csv(all,"//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/R-Interface/allEstimates.csv")
