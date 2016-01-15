#####################################
## Jared Studyvin
## 11 Jan 2016
## PSMFS 469-07.005 phase 4
## PM: Trent
## This is my test script for assigning the life stage
#####################################

rm(list=ls())

library(plyr)


codePath <- '~/GoogleDrive/project/PSMFS/fishLength/CAMP_RST/'

load(paste0(codePath,'catchData.Rdata'))



head(catchData)


catchData$whereWeight <- !is.na(catchData$weight)
nrow(catchData)
with(catchData,sum(Unmarked==0))
with(catchData,unique(Unmarked))
with(catchData,table(Unmarked,whereWeight,useNA='ifany'))
with(catchData,table(river,FinalRun,whereWeight,useNA='ifany'))


out <- ddply(catchData,~river+site,summarize,missingWeight=sum(is.na(weight)),haveWeight=sum(!is.na(weight)),n=length(weight),um=sum(Unmarked>1))
out

out$missingWeight+out$haveWeight
