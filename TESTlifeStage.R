#####################################
## Jared Studyvin
## 11 Jan 2016
## PSMFS 469-07.005 phase 4
## PM: Trent
## Query the data base to get the data for life stage assignment
#####################################





taxon <- '161980'           # always chinook salmon
site <- '42000'             # this is the river -- this is in table 'site' in the mdbs.
min.date <- '1990-01-01'    # the pre queries need to restrict to a time frame --
max.date <- '2015-12-31'    #  some data goes back to the 90s


taxon='161980'
site='42000'
min.date='1990-01-01'
max.date='2015-12-31'


##pathData <- "//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20151130/Data/TestingDBs/CAMP_RBDD_29Jan2015"     # i point to an mdb


pathData <- 'C:/Users/jstudyvin/GoogleDrive/project/PSMFS/fishLength/CAMP_RST20151014-DougEagleman/Data/TestingDBs/CAMP_RBDD_29Jan2015/'


codePath <- '~/GoogleDrive/project/PSMFS/fishLength/CAMP_RST/'
setwd(codePath)
## point somewhere / somehow to where the queries are

source( paste0(codePath,"/run_sqlFile.r" ))# a trent f'n from the platform
source( paste0(codePath,"/sql_error_check.r" ))# a trent f'n from the platform
source( paste0(codePath,"/build_Report_Criteria.r" ))# a trent f'n from the platform
source( paste0(codePath,"/getDataForLifeStageAssign.R" ))# a trent f'n from the platform




