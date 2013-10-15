--  this is Connie's SQL from QC database for "ViewAllCatch" button, with some additions


SELECT
    TrapVisit.visitTime,
    SubSite.subSiteName AS subSite,
    Trapvisit.trapPositionID AS trapPositionID,
    TrapVisit.batchDate,
    DateValue(TrapVisit!visitTime) AS [Date],
    luTaxon.commonName AS commonName,
    CatchRaw.taxonID,
    CatchRaw.forkLength,
    luRun_1.run AS AtCapRun,
    luRun.run AS run,
    CatchRaw.finalRunID,
    CatchRaw.weight,
    luLifeStage.lifeStage,
    CatchRaw.lifeStageID,
    CatchRaw.fishOriginID,
    luFishOrigin.fishOrigin,
    [CatchRaw].[n]*[CatchRaw].[subsampleDenominator]/[CatchRaw].[subsampleNumerator] AS n,
    luNoYes.noYes AS mort,
    CatchRaw.mortID,
    TrapVisit.comments AS TrapVisitComment,
    luNoYes_1.noYes AS IncludeCatch,
    CatchRaw.comments AS CatchComment,
    luMarkType.markType,
    luBodyPart.bodyPart,
    luColor.color,
    CatchRaw.releaseID

FROM luFishOrigin
    INNER JOIN
        (
            (SubSite INNER JOIN
                (
                    (TempReportCriteria_TrapVisit INNER JOIN
                        TrapVisit ON (TempReportCriteria_TrapVisit.trapVisitID = TrapVisit.trapVisitID) AND (TempReportCriteria_TrapVisit.projectDescriptionID = 
                        TrapVisit.projectDescriptionID)
                    )
                    LEFT JOIN luNoYes AS luNoYes_1 ON TrapVisit.includeCatchID = luNoYes_1.noYesID
                )
                    ON SubSite.subSiteID = TrapVisit.trapPositionID)

                INNER JOIN (luTaxon RIGHT JOIN
            (luMarkType RIGHT JOIN
                (luColor RIGHT JOIN
                    (luBodyPart RIGHT JOIN
                        ((luNoYes RIGHT JOIN
                            (((luLifeStage RIGHT JOIN
                                CatchRaw ON
                                luLifeStage.lifeStageID = CatchRaw.lifeStageID)
                            LEFT JOIN luRun ON CatchRaw.finalRunID = luRun.runID)
                            LEFT JOIN luRun AS luRun_1
                            ON CatchRaw.atCaptureRunID = luRun_1.runID)
                        ON luNoYes.noYesID = CatchRaw.mortID)
                        LEFT JOIN MarkExisting ON
                        (CatchRaw.catchRawID = MarkExisting.catchRawID) AND
                            (CatchRaw.projectDescriptionID = MarkExisting.projectDescriptionID)) ON
                        luBodyPart.bodyPartID = MarkExisting.markPositionID) ON
                        luColor.colorID = MarkExisting.markColorID) ON
                        luMarkType.markTypeID = MarkExisting.markTypeID) ON
                        luTaxon.taxonID = CatchRaw.taxonID) ON
                        (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND
                            (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
        )
       ON luFishOrigin.fishOriginID = CatchRaw.fishOriginID

WHERE (((TrapVisit.visitTypeID) Like 2 Or
    (TrapVisit.visitTypeID) Like 3 Or
    (TrapVisit.visitTypeID) Like 4))
ORDER BY TrapVisit.visitTime, SubSite.subSiteName, luTaxon.commonName, CatchRaw.forkLength;

