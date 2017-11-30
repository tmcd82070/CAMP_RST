


dir <- "L:/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Outputs/Holding"
files <- dir(dir,pattern=".RData")


allStreams <- NULL
for(i in 1:length(files)){
  load(paste0(dir,"/",files[i]))
  allStreams <- rbind(allStreams,varSummary)
}

if(is.factor(allStreams$subsiteID)){
  allStreams$subsiteID <- as.character(droplevels(allStreams$subsiteID))
}

#   ---- (Put betas in the campr package.)
betas <- allStreams[allStreams$Stage == "Final Model Betas",]
devtools::use_data(betas)




#   ---- (Now, somewhere in passage estimation, add the following...)

#   ---- See which subsiteIDs are in the data. 
trapsC <- unique(as.character(droplevels(obs.eff.df$TrapPositionID)))
trapsM <- trapsC[trapsC %in% betas$subsiteID]

#   ---- We need to check, for each trap in trapsM, that we have the data in EnvCovRaw?

betas <- allStreams[allStreams$subsiteID %in% c("57001","57002","57003","57004","57005") & allStreams$Stage == "Final Model Betas",]



covars <- names(betas)[!(names(betas) %in% c("subsiteID","threshold","available","Stage"))]

for(i in 1:length(covars)){
  if(betas[1,covars[i]] == 0){
    betas[,covars[i]] <- NULL
  }
}
betas <- apply(betas,2,function(x) x[x > 0])

#write.csv(allStreams,paste0(dir,"/","EnhancedBetas.csv"),row.names=FALSE)