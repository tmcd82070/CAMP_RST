



# 1.  Run enhanced efficiency models via reportRun "Q" in BigLooper2.0. 
# 2.  Collect .RData objects from wherever they are saved and placed in package campR folder inst/enhEffStats.
# 3.  Run this compiler code to make a new "betas" dataframe for use in package.
# 4.  Run covariateAnnualizer to get annual-based covariates.  
# 5.  Build package.  (Maybe run devtools::check too.)
# 6.  Install. 
# 7.  Move compiled campR from local WEST machine to campR "ThePlatform" R folder directory. 




dir <- "L:/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/enhEffStats"
files <- dir(dir,pattern=".RData")

varFiles <- files[grepl("var",files,fixed=FALSE)]

allStreams <- NULL
for(i in 1:length(varFiles)){
  load(paste0(dir,"/",varFiles[i]))
  
  #   ---- In the case of the RBDD, may not have turbidity.  
  #   ---- Add in zeros (not included in model) to make this go. 
  if(!("turbidity_ntu" %in% names(varSummary))){
    varSummary <- data.frame(varSummary[,c(1:12)],"turbidity_ntu"=rep(0,nrow(varSummary)),varSummary[,c(13:24)])
    names(varSummary)[names(varSummary) == "X.Intercept."] <- "(Intercept)"
  } 
  
  allStreams <- rbind(allStreams,varSummary)
}

if(is.factor(allStreams$subsiteID)){
  allStreams$subsiteID <- as.character(droplevels(allStreams$subsiteID))
}

#   ---- Make sure flow_cfs at least considered in all models.  
allStreams[allStreams$Stage == "Initial",]   # <---- Column flow_cfs should be all 1s (except 42xxx -- RBDD).

#   ---- (Put betas in the campr package.)
betas <- allStreams[allStreams$Stage == "Final Model Betas",]
devtools::use_data(pkg="L:/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR",betas,overwrite=TRUE)








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