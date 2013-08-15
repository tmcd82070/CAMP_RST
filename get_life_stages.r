F.get.life.stages <- function( ch ){

    ch <- odbcConnectAccess(db.file)
    
    tmp <- paste("SELECT ",
        table.names["life.stages"], ".lifeStage, ",
        table.names["life.stages"], ".lifeStageID, ",
        table.names["life.stages"], ".lifeStageCAMPID, ",
        table.names["CAMP.life.stages"], ".lifeStageCAMP ",
        "FROM ",
        table.names["CAMP.life.stages"],
        " INNER JOIN ",
        table.names["life.stages"],
        " ON ", table.names["CAMP.life.stages"], ".lifeStageCAMPID = ", table.names["life.stages"], ".lifeStageCAMPID;", sep="")
    rst.life.stage <- sqlQuery(ch, tmp)
    F.sql.error.check(rst.life.stage)

    CAMP.life.stageIDs <- c(2,3,4,8,9,10,11,251)
    CAMP.life.stages <- NULL
    for( i in CAMP.life.stageIDs ){
        CAMP.life.stages <- c(CAMP.life.stages, as.character(rst.life.stage$lifeStageCAMP[ rst.life.stage$lifeStageCAMPID == i ][1]))
    }

    close(ch) 
    
    list(IDs=CAMP.life.stageIDs, stages=CAMP.life.stages, rst.ls=rst.life.stage)
}
