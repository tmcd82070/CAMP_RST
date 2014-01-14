-- Final SQL for developing Chinook summary by trap...TempChinookSampling_i_final
-- TRENT changed all the chinook species codes to R.TAXON


-- qrySumChinookByTrap_a
DROP TABLE TempChinookSampling_a; 

SELECT TrapVisit.projectDescriptionID AS ProjID
    , TrapVisit.trapPositionID
    , IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes')) AS IncludeCatch
    , Site.siteName
    , Site.siteAbbreviation
    , SubSite.subSiteName AS TrapPosition
    , DateValue(TrapVisit!visitTime) AS [Date]
    , Sum(TempSamplingSummary.TotalSampleMinutes) AS SampleMinutes 
INTO TempChinookSampling_a
FROM 
    (Site RIGHT JOIN SubSite ON Site.siteID = SubSite.siteID) 
RIGHT JOIN 
    (
        (
            (TempReportCriteria_TrapVisit 
                INNER JOIN TrapVisit ON 
                    (TempReportCriteria_TrapVisit.trapVisitID = TrapVisit.trapVisitID) 
                    AND 
                    (TempReportCriteria_TrapVisit.projectDescriptionID = TrapVisit.projectDescriptionID)) 
                LEFT JOIN TempSamplingSummary ON 
                    (TrapVisit.trapVisitID = TempSamplingSummary.trapVisitID) 
                    AND 
                    (TrapVisit.projectDescriptionID = TempSamplingSummary.projectDescriptionID)) 
                LEFT JOIN luVisitType ON 
                    TrapVisit.visitTypeID = luVisitType.visitTypeID
    ) 
    ON SubSite.subSiteID = TrapVisit.trapPositionID
WHERE 
    (
        (
            (TrapVisit.visitTypeID)>1 
            And 
            (TrapVisit.visitTypeID)<5
        ) 
        AND ((TrapVisit.fishProcessedID)<>2)
    )
GROUP BY TrapVisit.projectDescriptionID
    , TrapVisit.trapPositionID
    , IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes'))
    , Site.siteName
    , Site.siteAbbreviation
    , SubSite.subSiteName
    , DateValue(TrapVisit!visitTime);

-- qrySumChinookByTrap_b
DROP TABLE TempChinookSampling_b;

SELECT TrapVisit.trapPositionID
    , DateValue(TrapVisit!visitTime) AS [Date]
    , IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes')) AS IncludeCatch
    , Sum(CatchRaw!n) AS AllChinook 
INTO TempChinookSampling_b
FROM 
    (
        TrapVisit INNER JOIN TempReportCriteria_TrapVisit ON 
            (TrapVisit.projectDescriptionID = TempReportCriteria_TrapVisit.projectDescriptionID) 
            AND 
            (TrapVisit.trapVisitID = TempReportCriteria_TrapVisit.trapVisitID)
    ) 
    LEFT JOIN CatchRaw ON 
        (TrapVisit.trapVisitID = CatchRaw.trapVisitID) 
        AND 
        (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE 
    (
        ((TrapVisit.fishProcessedID)<>2) 
        AND 
        ((CatchRaw.taxonID)='R.TAXON') 
        AND 
            (
                (TrapVisit.visitTypeID)>1 
                And 
                (TrapVisit.visitTypeID)<5
            )
    )
GROUP BY TrapVisit.trapPositionID, DateValue(TrapVisit!visitTime), IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes'));


-- qrySumChinookByTrap_c
DROP TABLE TempChinookSampling_c; 

SELECT TrapVisit.trapPositionID
    , DateValue(TrapVisit!visitTime) AS [Date]
    , IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes')) AS IncludeCatch
    , Sum(CatchRaw!n) AS EtestRecaps 
INTO TempChinookSampling_c
FROM 
    (
        TrapVisit 
        INNER JOIN TempReportCriteria_TrapVisit ON 
            (TrapVisit.trapVisitID = TempReportCriteria_TrapVisit.trapVisitID) 
            AND 
            (TrapVisit.projectDescriptionID = TempReportCriteria_TrapVisit.projectDescriptionID)
    ) 
    INNER JOIN 
        (
            Release RIGHT JOIN CatchRaw ON 
                (Release.releaseID = CatchRaw.releaseID) 
                AND 
                (Release.projectDescriptionID = CatchRaw.projectDescriptionID)
        ) ON 
            (TrapVisit.trapVisitID = CatchRaw.trapVisitID) 
            AND 
            (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE 
    (
        (
            (TrapVisit.fishProcessedID)<>2) 
            AND 
                (
                    (Release.releasePurposeID)=1) 
                    AND 
                    ((CatchRaw.taxonID)='R.TAXON') 
                    AND ((TrapVisit.visitTypeID)>1 And (TrapVisit.visitTypeID)<5)
    )
GROUP BY TrapVisit.trapPositionID
    , DateValue(TrapVisit!visitTime)
    , IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes'));

-- qrySumChinookByTrap_d1
DROP TABLE TempChinookSampling_d1; 

SELECT TempReportCriteria_TrapVisit.projectDescriptionID, 
    TempReportCriteria_TrapVisit.trapVisitID, 
    CatchRaw.catchRawID 
INTO TempChinookSampling_d1
FROM 
    (
        TrapVisit INNER JOIN TempReportCriteria_TrapVisit ON 
            (TrapVisit.trapVisitID = TempReportCriteria_TrapVisit.trapVisitID) 
            AND 
            (TrapVisit.projectDescriptionID = TempReportCriteria_TrapVisit.projectDescriptionID)
    ) INNER JOIN 
        (
            Release RIGHT JOIN 
                (
                    CatchRaw INNER JOIN MarkExisting ON 
                        (CatchRaw.catchRawID = MarkExisting.catchRawID) 
                        AND 
                        (CatchRaw.projectDescriptionID = MarkExisting.projectDescriptionID) 
                        AND 
                        (CatchRaw.projectDescriptionID = MarkExisting.projectDescriptionID)
                ) ON 
                (Release.releaseID = CatchRaw.releaseID) 
                AND 
                (Release.projectDescriptionID = CatchRaw.projectDescriptionID)
        ) ON 
        (TrapVisit.trapVisitID = CatchRaw.trapVisitID) 
        AND 
        (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE (
    (
        (CatchRaw.fishOriginID)=1) 
        AND 
            (
                (CatchRaw.n)>0) 
                AND 
                (
                    (CatchRaw.releaseID)=0 
                    Or 
                    (CatchRaw.releaseID)=255
                ) 
                AND 
                ((CatchRaw.taxonID)='R.TAXON') 
                AND 
                ((MarkExisting.markTypeID)=0) 
                AND 
                ((MarkExisting.markColorID)=252) 
                AND 
                ((MarkExisting.markPositionID)=252) 
                AND 
                ((TrapVisit.fishProcessedID)<>2) 
                AND 
                (
                    (TrapVisit.visitTypeID)>1 
                    And 
                    (TrapVisit.visitTypeID)<5)
                ) 
                OR 
                (
                    ((CatchRaw.n)>0) 
                    AND 
                    (
                        (CatchRaw.releaseID) Not Like 0 
                        And 
                        (CatchRaw.releaseID) Not Like 255
                    ) 
                    AND 
                        (
                            (CatchRaw.taxonID)='R.TAXON') 
                            AND 
                            ((TrapVisit.fishProcessedID)<>2) 
                            AND 
                            (
                                (TrapVisit.visitTypeID)>1 
                                And 
                                (TrapVisit.visitTypeID)<5
                            ) 
                            AND 
                            ((Release.releasePurposeID)=3)
                ) 
                OR 
                (
                    ((CatchRaw.n)>0) 
                    AND 
                    (
                        (CatchRaw.releaseID)=0 
                        Or 
                        (CatchRaw.releaseID)=255
                    ) 
                    AND 
                    ((CatchRaw.taxonID)='R.TAXON') 
                    AND 
                    ((MarkExisting.markTypeID)=2) 
                    AND 
                    ((MarkExisting.markColorID)=252) 
                    AND 
                    ((MarkExisting.markPositionID)=1) 
                    AND 
                    ((TrapVisit.fishProcessedID)<>2) 
                    AND 
                    (
                        (TrapVisit.visitTypeID)>1 
                        And 
                        (TrapVisit.visitTypeID)<5)
                    )
GROUP BY TempReportCriteria_TrapVisit.projectDescriptionID, TempReportCriteria_TrapVisit.trapVisitID, CatchRaw.catchRawID;


-- qrySumChinookByTrap_d2
DROP TABLE TempChinookSampling_d2; 

SELECT MarkExisting.catchRawID, CatchRaw.n, TrapVisit.trapPositionID, DateValue(TrapVisit!visitTime) AS [Date], IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes')) AS IncludeCatch INTO TempChinookSampling_d2
FROM (TrapVisit INNER JOIN TempReportCriteria_TrapVisit ON (TrapVisit.trapVisitID = TempReportCriteria_TrapVisit.trapVisitID) AND (TrapVisit.projectDescriptionID = TempReportCriteria_TrapVisit.projectDescriptionID)) INNER JOIN ((CatchRaw LEFT JOIN TempChinookSampling_d1 ON (CatchRaw.projectDescriptionID = TempChinookSampling_d1.projectDescriptionID) AND (CatchRaw.catchRawID = TempChinookSampling_d1.catchRawID)) LEFT JOIN MarkExisting ON (CatchRaw.catchRawID = MarkExisting.catchRawID) AND (CatchRaw.projectDescriptionID = MarkExisting.projectDescriptionID)) ON (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE (((TempChinookSampling_d1.catchRawID) Is Null) AND ((CatchRaw.releaseID)=255 Or (CatchRaw.releaseID)=0) AND ((CatchRaw.taxonID)='R.TAXON') AND ((CatchRaw.n)>0) AND ((MarkExisting.markTypeID) Not Like 0) AND ((TrapVisit.visitTypeID)>1 And (TrapVisit.visitTypeID)<5) AND ((TrapVisit.fishProcessedID)<>2))
GROUP BY MarkExisting.catchRawID, CatchRaw.n, TrapVisit.trapPositionID, DateValue(TrapVisit!visitTime), IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes'));

-- qrySumChinookByTrap_d3
DROP TABLE TempChinookSampling_d3; 

SELECT TempChinookSampling_d2.trapPositionID, TempChinookSampling_d2.Date, TempChinookSampling_d2.IncludeCatch, Sum(TempChinookSampling_d2!n) AS UnassignedMark INTO TempChinookSampling_d3
FROM TempChinookSampling_d2
GROUP BY TempChinookSampling_d2.trapPositionID, TempChinookSampling_d2.Date, TempChinookSampling_d2.IncludeCatch;


-- qrySumChinookByTrap_e
DROP TABLE TempChinookSampling_e; 

SELECT 
    TrapVisit.trapPositionID, 
    DateValue(TrapVisit!visitTime) AS [Date], 
    IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes')) AS IncludeCatch, 
    Sum(CatchRaw!n) AS Hatchery 
INTO TempChinookSampling_e
FROM TrapVisit 
    INNER JOIN 
    (
        CatchRaw 
        INNER JOIN 
        TempChinookSampling_d1 ON 
            (CatchRaw.catchRawID = TempChinookSampling_d1.catchRawID) 
            AND 
            (CatchRaw.projectDescriptionID = TempChinookSampling_d1.projectDescriptionID)
    ) ON 
        (TrapVisit.trapVisitID = CatchRaw.trapVisitID) 
        AND 
        (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
GROUP BY TrapVisit.trapPositionID, 
    DateValue(TrapVisit!visitTime), 
    IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes'))
ORDER BY DateValue(TrapVisit!visitTime);


-- qrySumChinookByTrap_f
DROP TABLE TempChinookSampling_f; 

SELECT TrapVisit.trapPositionID, DateValue(TrapVisit!visitTime) AS [Date], IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes')) AS IncludeCatch, Sum(CatchRaw!n) AS OtherRecaps INTO TempChinookSampling_f
FROM (TrapVisit INNER JOIN TempReportCriteria_TrapVisit ON (TrapVisit.projectDescriptionID = TempReportCriteria_TrapVisit.projectDescriptionID) AND (TrapVisit.trapVisitID = TempReportCriteria_TrapVisit.trapVisitID)) INNER JOIN (Release INNER JOIN CatchRaw ON (Release.releaseID = CatchRaw.releaseID) AND (Release.projectDescriptionID = CatchRaw.projectDescriptionID)) ON (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE (((CatchRaw.releaseID) Not Like 0 And (CatchRaw.releaseID) Not Like 255) AND ((Release.releasePurposeID) Like 2 Or (Release.releasePurposeID) Like 4 Or (Release.releasePurposeID) Like 250 Or (Release.releasePurposeID) Like 255) AND ((CatchRaw.taxonID)='R.TAXON') AND ((TrapVisit.visitTypeID)>1 And (TrapVisit.visitTypeID)<5) AND ((TrapVisit.fishProcessedID)<>2))
GROUP BY TrapVisit.trapPositionID, DateValue(TrapVisit!visitTime), IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes'));

-- qrySumChinookByTrap_g
DROP TABLE TempChinookSampling_g; 

SELECT TrapVisit.trapPositionID, DateValue(TrapVisit!visitTime) AS [Date], IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes')) AS IncludeCatch, Sum(CatchRaw!n) AS UnmarkedCHN INTO TempChinookSampling_g
FROM (TrapVisit INNER JOIN TempReportCriteria_TrapVisit ON (TrapVisit.trapVisitID = TempReportCriteria_TrapVisit.trapVisitID) AND (TrapVisit.projectDescriptionID = TempReportCriteria_TrapVisit.projectDescriptionID)) INNER JOIN (CatchRaw LEFT JOIN MarkExisting ON (CatchRaw.catchRawID = MarkExisting.catchRawID) AND (CatchRaw.projectDescriptionID = MarkExisting.projectDescriptionID)) ON (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE (((CatchRaw.fishOriginID)<>1) AND ((CatchRaw.releaseID) Like 0 Or (CatchRaw.releaseID) Like 255) AND ((CatchRaw.taxonID)='R.TAXON') AND ((TrapVisit.visitTypeID)>1 And (TrapVisit.visitTypeID)<5) AND ((TrapVisit.fishProcessedID)<>2) AND ((MarkExisting.markTypeID)=0) AND ((MarkExisting.markColorID)=252) AND ((MarkExisting.markPositionID)=252)) OR (((CatchRaw.fishOriginID) Is Null) AND ((CatchRaw.releaseID) Like 0 Or (CatchRaw.releaseID) Like 255) AND ((CatchRaw.taxonID)='R.TAXON') AND ((TrapVisit.visitTypeID)>1 And (TrapVisit.visitTypeID)<5) AND ((TrapVisit.fishProcessedID)<>2) AND ((MarkExisting.markTypeID)=0) AND ((MarkExisting.markColorID)=252) AND ((MarkExisting.markPositionID)=252))
GROUP BY TrapVisit.trapPositionID, DateValue(TrapVisit!visitTime), IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID=3,'No','Yes'));

-- qrySumChinookByTrap_h
DROP TABLE TempChinookSampling_h; 

SELECT TempChinookSampling_b.trapPositionID
    , TempChinookSampling_b.Date
    , TempChinookSampling_b.IncludeCatch
    , TempChinookSampling_b.AllChinook
    , TempChinookSampling_c.EtestRecaps
    , TempChinookSampling_f.OtherRecaps
    , TempChinookSampling_d3.UnassignedMark
    , TempChinookSampling_e.Hatchery
    , TempChinookSampling_g.UnmarkedCHN 
INTO TempChinookSampling_h
FROM 
    (
        (
            (
                (
                    TempChinookSampling_b LEFT JOIN TempChinookSampling_c 
                    ON (TempChinookSampling_b.trapPositionID = TempChinookSampling_c.trapPositionID) 
                    AND (TempChinookSampling_b.Date = TempChinookSampling_c.Date) 
                    AND (TempChinookSampling_b.IncludeCatch = TempChinookSampling_c.IncludeCatch)
                ) 
                LEFT JOIN TempChinookSampling_e ON 
                    (TempChinookSampling_b.trapPositionID = TempChinookSampling_e.trapPositionID) 
                    AND 
                    (TempChinookSampling_b.Date = TempChinookSampling_e.Date) 
                    AND 
                    (TempChinookSampling_b.IncludeCatch = TempChinookSampling_e.IncludeCatch)
            ) 
            LEFT JOIN TempChinookSampling_f ON 
                (TempChinookSampling_b.trapPositionID = TempChinookSampling_f.trapPositionID) 
                AND 
                (TempChinookSampling_b.Date = TempChinookSampling_f.Date) 
                AND 
                (TempChinookSampling_b.IncludeCatch = TempChinookSampling_f.IncludeCatch)
        ) 
        LEFT JOIN TempChinookSampling_g ON 
            (TempChinookSampling_b.trapPositionID = TempChinookSampling_g.trapPositionID) 
            AND 
            (TempChinookSampling_b.Date = TempChinookSampling_g.Date) 
            AND 
            (TempChinookSampling_b.IncludeCatch = TempChinookSampling_g.IncludeCatch)
    ) 
    LEFT JOIN TempChinookSampling_d3 ON 
        (TempChinookSampling_b.trapPositionID = TempChinookSampling_d3.trapPositionID) 
        AND 
        (TempChinookSampling_b.Date = TempChinookSampling_d3.Date) 
        AND 
        (TempChinookSampling_b.IncludeCatch = TempChinookSampling_d3.IncludeCatch)
GROUP BY TempChinookSampling_b.trapPositionID
    , TempChinookSampling_b.Date
    , TempChinookSampling_b.IncludeCatch
    , TempChinookSampling_b.AllChinook
    , TempChinookSampling_c.EtestRecaps
    , TempChinookSampling_f.OtherRecaps
    , TempChinookSampling_d3.UnassignedMark
    , TempChinookSampling_e.Hatchery
    , TempChinookSampling_g.UnmarkedCHN;

-- qrySumChinookByTrap_i_final
DROP TABLE TempChinookSampling_i_final; 

SELECT Dates.dayOfYear AS JulianDate
    , TempChinookSampling_a.Date
    , TempChinookSampling_a.siteName AS Site
    , TempChinookSampling_a.siteAbbreviation
    , TempChinookSampling_a.TrapPosition
    , TempChinookSampling_a.TrapPositionID
    , TempChinookSampling_a.IncludeCatch AS IncludeInAnalysis
    , Format(TempChinookSampling_a!SampleMinutes/60,'00') & ':' & Format(TempChinookSampling_a!SampleMinutes Mod 60,'00') AS HoursFishing
    , IIf(TempChinookSampling_h!AllChinook Is Null,0,TempChinookSampling_h!AllChinook) AS AllChinook
    , TempChinookSampling_h.EtestRecaps
    , TempChinookSampling_h.OtherRecaps
    , TempChinookSampling_h.UnassignedMark
    , TempChinookSampling_h.Hatchery
    , IIf(TempChinookSampling_h!UnmarkedCHN Is Null,0,TempChinookSampling_h!UnmarkedCHN) AS UnmarkedCHN 
INTO TempChinookSampling_i_final
FROM 
    (
        Dates RIGHT JOIN TempChinookSampling_a ON 
            Dates.uniqueDate = TempChinookSampling_a.Date
    ) 
    LEFT JOIN TempChinookSampling_h ON 
        (TempChinookSampling_a.trapPositionID = TempChinookSampling_h.trapPositionID) 
        AND 
        (TempChinookSampling_a.Date = TempChinookSampling_h.Date) 
        AND 
        (TempChinookSampling_a.IncludeCatch = TempChinookSampling_h.IncludeCatch)
ORDER BY TempChinookSampling_a.Date
    , TempChinookSampling_a.TrapPosition
    , TempChinookSampling_a.IncludeCatch;


-- qrySumChinookByTrap_j_fishTallyError
-- DROP TABLE TempChinookSampling_j_FishTallyError; 

-- SELECT TempChinookSampling_i_final.Date, TempChinookSampling_i_final.TrapPosition, TempChinookSampling_i_final.IncludeInAnalysis, TempChinookSampling_i_final.AllChinook, 'Not all Chinook are accounted for on this date.  Fish in various categories do not add up to the total number of Chinook in the AllChinook column. Please check the catch data for completeness and accuracy and report problems to CAMP program leads.' AS Error, nz(TempChinookSampling_i_final!EtestRecaps,0)+nz(TempChinookSampling_i_final!OtherRecaps,0)+nz(TempChinookSampling_i_final!UnassignedMark,0)+nz(TempChinookSampling_i_final!Hatchery,0)+nz(TempChinookSampling_i_final!UnmarkedCHN,0) AS ReportTotals INTO TempChinookSampling_j_FishTallyError
-- FROM TempChinookSampling_i_final
-- WHERE (((nz([TempChinookSampling_i_final]![EtestRecaps],0)+nz([TempChinookSampling_i_final]![OtherRecaps],0)+nz([TempChinookSampling_i_final]![UnassignedMark],0)+nz([TempChinookSampling_i_final]![Hatchery],0)+nz([TempChinookSampling_i_final]![UnmarkedCHN],0))<>[TempChinookSampling_i_final]![AllChinook]));


