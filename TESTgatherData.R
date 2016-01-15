#####################################
## Jared Studyvin
## 11 Jan 2016
## PSMFS 469-07.005 phase 4
## PM: Trent
## This is my script for getting all of the data into one place
#####################################

rm(list=ls())

library(plyr)

siteID <- list(mok=c('34000'),rbdd=c('42000'),american=c('57000'),battle=c('13000','15000'),feather=c('3000','52000','5000','4000'),stan=c('1000'))


codePath <- '~/GoogleDrive/project/PSMFS/fishLength/CAMP_RST/'
setwd(codePath)
## point somewhere / somehow to where the queries are

source( paste0(codePath,"/run_sqlFile.r" ))# a trent f'n from the platform
source( paste0(codePath,"/sql_error_check.r" ))# a trent f'n from the platform
source( paste0(codePath,"/build_Report_Criteria.r" ))# a trent f'n from the platform
source( paste0(codePath,"/getDataForLifeStageAssign.R" ))# a trent f'n from the platform



dirPath <- 'C:/Users/jstudyvin/GoogleDrive/project/PSMFS/fishLength/CAMP_RST20151014-DougEagleman/Data/TestingDBs/'

allPath <- list.dirs(dirPath,TRUE,FALSE)
allPath <- allPath[-grep('notforanalyses',allPath,ignore.case=TRUE)]



## dat=siteID
## path=allPath
## x=4
getAll <- function(x,dat,path){
thisPath <- path[grep(names(dat[x]),path,ignore.case=TRUE)]
print(thisPath)

print(dat[[x]])

catchData <- llply(dat[[x]],getDataForLifeStageAssign,taxon='161980',pathData=thisPath,min.date='1980-01-01',max.date=Sys.Date())

names(catchData) <- dat[[x]]

return(catchData)
}


system.time(
catch <- llply(seq_along(siteID),getAll,dat=siteID,path=allPath)
)
names(catch) <- names(siteID)


catch2 <- llply(catch,function(x){ldply(x,rbind,.id='site')})

catchData <- ldply(catch2,rbind,.id='river')

head(catchData)

save(catchData,file='catchData.Rdata')
