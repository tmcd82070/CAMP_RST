

#   ---- Identify the master folder of results.  
masterFolder <- "//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20161015-campR1.0.1/Outputs"

#   ---- Identify the passage reports of results to include.  
passVec <- c("EstProdAllRunsLSReport","EstProdAllRunsReport","PassageEst_FL_Fall","AutoLS_2Group_YesWgt",
             "AutoLS_2Group_NoWgt","AutoLS_3Group_YesWgt","AutoLS_3Group_NoWgt","AutoLS_2or3_AutoWgt","AutoLS_2or3_NoWgt") 

#   ---- Identify the rivers to include in the summary.
riverVec <- c("Stanislaus River--Caswell State Park",
              "American River--American River at Watt Avenue",
              "Sacramento River--RBDD RST")



all <- NULL
l1 <- dir(masterFolder)[dir(masterFolder) != "Thumbs.db" & dir(masterFolder) %in% riverVec]

for(i in 1:length(l1)){
  
  l1Folder <- paste0(masterFolder,"/",l1[i])
  l2 <- dir(l1Folder)[dir(l1Folder) != "Thumbs.db"]
  
  for(j in 1:length(l2)){
    
    l2Folder <- paste0(l1Folder,"/",l2[j])
    l3 <- dir(l2Folder)[dir(l2Folder) != "Thumbs.db" & dir(l2Folder) %in% passVec]
    
    for(k in 1:length(l3)){
      
      l3Folder <- paste0(l2Folder,"/",l3[k])

      #   ---- Compile all passage results, customizing based on the type of report.
      ans <- getRiverPassage(l3Folder)
      all <- rbind(all,ans)
    }
  }
}

all <- all[!is.na(all$bEst),]
all <- all[all$bEst > 0,] 
rownames(all) <- NULL
nrow(all)
all$time <- ifelse(is.na(all$time),'--',all$time)
all$lifeStage <- as.character(droplevels(all$lifeStage))
all$lifeStage <- ifelse(is.na(all$lifeStage),'--',all$lifeStage)

all <- all[order(all$river,all$siteName,all$max.date,all$file,all$run,all$lifeStage,all$time),]
all$bMag <- all$bOOL <- all$sequence <- NULL

trouble <- all[all$bUCL > 100000000,]
good <- all[all$bUCL <= 100000000,]

options(scipen=999)
good$bEst <- format(round(as.numeric(good$bEst), 0),nsmall=0,big.mark=",") 
good$bLCL <- format(round(as.numeric(good$bLCL), 0),nsmall=0,big.mark=",") 
good$bUCL <- format(round(as.numeric(good$bUCL), 0),nsmall=0,big.mark=",")


print(trouble)

write.csv(good,"C:/Users/jmitchell/Desktop/allEstsCompareWRBDD.csv",row.names=FALSE)







source('//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/helperCode/getTheData.R')

getRiverPassage <- function(stem){

#   stem <- l3Folder
  
  #   ---- Identify the different type of passage runs.  
  files <- list.files(stem)
  
  if(length(files) > 0){
    ls_passageB <- files[grep('lifestage_passage_table.csv',files)]
    passageB <- files[grep('passage_table.csv',files)]         
  
    #   ---- Open up the files one-by-one, and suck out the passage results.  
    openTheseB <- unique(data.frame(file=c(ls_passageB,passageB),stringsAsFactors=FALSE))
    rownames(openTheseB) <- NULL
    for(l in 1:nrow(openTheseB)){
      if(substr(openTheseB$file[l],nchar(openTheseB$file[l]) - 26,nchar(openTheseB$file[l]) - 26 + 3) == 'life'){
        openTheseB$type[l] <- 'life'
      } else if(substr(openTheseB$file[l],nchar(openTheseB$file[l]) - 20,nchar(openTheseB$file[l]) - 20 + 2) == 'run'){
        openTheseB$type[l] <- 'run'
      } else {
        openTheseB$type[l] <- 'summary'
      }
    }
  
    bigDFB <- getTheData(openThese=openTheseB,stem=stem)  
    return(bigDFB)
  } 
}











