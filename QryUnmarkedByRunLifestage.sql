-- Series to display target species by run
-- cshannon 10/21/2013
-- 
-- ***Requires QryBuildReportCriteria be developed first*** (to construct table TempReportCriteria_Trapvisit)
-- ***Requires QrySamplePeriod be developed first*** (to construct table tmpSamplingSummary)
-- ***Requires QryNotFishing be developed first*** (to constuct table TempNonSampling)
-- 
-- ***NOTE THAT UPON FINAL QUERY FOR TALLY ERRORS THERE was a difference of three 
-- fish between the ChinookSamplingSummary and the summarized results of this series 
-- when I ran all of the data for the feather River RST.  I can't find the reason and 
-- I'm not sure if it is a problem in the ChinookSamplingSummary or the SumUnmarkedByTrap sum. 
-- 
-- The series only pulls data for valid trap visits, where tempSamplingSummary includeCatchID is 1 ='yes' and visitTypeID is > 1 = Start trap & begin trapping.
-- 
-- * This is just the start, we haven't determined what format the output should be yet.
--
-- 1.  qrySumUnmarkedByTrap_Run_a uses tempSamplingSummary to limit records and to build table TempSumUnmarkedByTrap_Run_a

SELECT 
      TempSamplingSummary.projectDescriptionID AS ProjID
    , TempSamplingSummary.trapVisitID
    , DateValue(TempSamplingSummary!timeSampleEnded) AS SampleDate
    , TempSamplingSummary.timeSampleStarted AS StartTime
    , TempSamplingSummary.timeSampleEnded AS EndTime
    , TempSamplingSummary.TotalSampleMinutes AS SampleMinutes
    , 'Fishing' AS TrapStatus
    , SubSite.siteID
    , Site.siteName
    , TempSamplingSummary.trapPositionID
    , SubSite.subSiteName AS TrapPosition 
INTO TempSumUnmarkedByTrap_Run_a
FROM 
    Site RIGHT JOIN 
        (
            TempSamplingSummary LEFT JOIN 
                SubSite ON 
                TempSamplingSummary.trapPositionID = SubSite.subSiteID
        ) ON Site.siteID = SubSite.siteID
WHERE 
    (
        ((TempSamplingSummary.includeCatchID)=1) 
        AND 
        ((TempSamplingSummary.visitTypeID)>1)
    )
ORDER BY 
    TempSamplingSummary.timeSampleStarted
    , Site.siteName
    , SubSite.subSiteName;


-- 2.  qrySumUnmarkedByTrap_Run_b builds the catch table output and outputs to table TempSumUnmarkedByTrap_Run_b

SELECT TempSamplingSummary.projectDescriptionID
    , TempSamplingSummary.trapVisitID
    , Sum(CatchRaw!n) AS Unmarked
    , IIf(CatchRaw!finalRunID<6 And CatchRaw!finalRunID<>2,luRun!run,'Unassigned') AS FinalRun
    , IIf(CatchRaw!lifeStageID<10 Or CatchRaw!lifeStageID>15 And CatchRaw!lifeStageID<20,luLifeStageCAMP!lifeStageCAMP,IIf(CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'AdultSubAdult','Unassigned')) AS LifeStage
    , IIf([CatchRaw]![randomID]=2 Or [CatchRaw]![lifeStageID]=11 Or [CatchRaw]![lifeStageID]=15,'No','Yes') AS RandomSelection 
INTO TempSumUnmarkedByTrap_Run_b
FROM 
    (
        TempSamplingSummary INNER JOIN TrapVisit ON 
            (TempSamplingSummary.trapVisitID = TrapVisit.trapVisitID) 
            AND 
            (TempSamplingSummary.projectDescriptionID = TrapVisit.projectDescriptionID)
    ) 
    INNER JOIN 
        (
            (
                (
                    (
                        CatchRaw LEFT JOIN luRun ON CatchRaw.finalRunID = luRun.runID
                    ) 
                    LEFT JOIN luLifeStage ON CatchRaw.lifeStageID = luLifeStage.lifeStageID
                ) LEFT JOIN luLifeStageCAMP ON luLifeStage.lifeStageCAMPID = luLifeStageCAMP.lifeStageCAMPID
            ) LEFT JOIN MarkExisting ON (CatchRaw.catchRawID = MarkExisting.catchRawID) AND (CatchRaw.projectDescriptionID = MarkExisting.projectDescriptionID)
        ) ON (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE 
    (
        ((TempSamplingSummary.visitTypeID)>1) 
        AND 
        ((CatchRaw.fishOriginID)<>1 Or (CatchRaw.fishOriginID) Is Null) 
        AND 
        ((CatchRaw.n)>0) 
        AND 
        ((CatchRaw.releaseID) Like 0 Or (CatchRaw.releaseID) Like 255) 
        AND 
        ((CatchRaw.taxonID)='R.TAXON') 
        AND 
        ((MarkExisting.markTypeID)=0) 
        AND 
        ((MarkExisting.markColorID)=252) 
        AND 
        ((MarkExisting.markPositionID)=252)
    )
GROUP BY 
    TempSamplingSummary.projectDescriptionID
    , TempSamplingSummary.trapVisitID
    , IIf(CatchRaw!finalRunID<6 And CatchRaw!finalRunID<>2,luRun!run,'Unassigned')
    , IIf(CatchRaw!lifeStageID<10 Or CatchRaw!lifeStageID>15 And CatchRaw!lifeStageID<20,luLifeStageCAMP!lifeStageCAMP,IIf(CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'AdultSubAdult','Unassigned'))
    , IIf([CatchRaw]![randomID]=2 Or [CatchRaw]![lifeStageID]=11 Or [CatchRaw]![lifeStageID]=15,'No','Yes')
    , TempSamplingSummary.includeCatchID
HAVING 
    (((TempSamplingSummary.includeCatchID)=1));


-- 3.  qrySumUnmarkedByTrap_Run_c  Brings the two temp tables together adding zeros for visits with no unmarked catch and outputs to 	TempSumUnmarkedByTrap_Run_final

SELECT 
    TempSumUnmarkedByTrap_Run_a.ProjID
    , TempSumUnmarkedByTrap_Run_a.trapVisitID
    , TempSumUnmarkedByTrap_Run_a.SampleDate
    , TempSumUnmarkedByTrap_Run_a.StartTime
    , TempSumUnmarkedByTrap_Run_a.EndTime
    , TempSumUnmarkedByTrap_Run_a.SampleMinutes
    , TempSumUnmarkedByTrap_Run_a.TrapStatus
    , TempSumUnmarkedByTrap_Run_a.siteID
    , TempSumUnmarkedByTrap_Run_a.siteName
    , TempSumUnmarkedByTrap_Run_a.trapPositionID
    , TempSumUnmarkedByTrap_Run_a.TrapPosition
    , IIf(TempSumUnmarkedByTrap_Run_b!Unmarked Is Null,0,TempSumUnmarkedByTrap_Run_b!Unmarked) AS Unmarked
    , IIf(TempSumUnmarkedByTrap_Run_b!FinalRun Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!FinalRun) AS FinalRun
    , IIf(TempSumUnmarkedByTrap_Run_b!LifeStage Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!LifeStage) AS lifeStage
    , IIf([TempSumUnmarkedByTrap_Run_b]![RandomSelection] Is Null,'Yes',[TempSumUnmarkedByTrap_Run_b]![RandomSelection]) AS RandomSelection 
INTO TempSumUnmarkedByTrap_Run_Final
FROM 
    TempSumUnmarkedByTrap_Run_a LEFT JOIN TempSumUnmarkedByTrap_Run_b ON 
        (TempSumUnmarkedByTrap_Run_a.ProjID = TempSumUnmarkedByTrap_Run_b.projectDescriptionID) 
        AND 
        (TempSumUnmarkedByTrap_Run_a.trapVisitID = TempSumUnmarkedByTrap_Run_b.trapVisitID)
ORDER BY 
    TempSumUnmarkedByTrap_Run_a.StartTime
    , TempSumUnmarkedByTrap_Run_a.siteName
    , TempSumUnmarkedByTrap_Run_a.TrapPosition
    , IIf(TempSumUnmarkedByTrap_Run_b!FinalRun Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!FinalRun)
    , IIf(TempSumUnmarkedByTrap_Run_b!LifeStage Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!LifeStage)
    , IIf([TempSumUnmarkedByTrap_Run_b]![RandomSelection] Is Null,'Yes',[TempSumUnmarkedByTrap_Run_b]![RandomSelection]);


-- 4.  qrySumUnmarkedByTrap_Run_d Adds records for non sampling periods to the final table.  Trap status = not fishing

INSERT INTO TempSumUnmarkedByTrap_Run_Final 
    ( ProjID
      , StartTime
      , EndTime
      , SampleMinutes
      , TrapStatus
      , siteID
      , siteName
      , trapPositionID
      , TrapPosition )
SELECT 
    TempNonSamplingSummary.projectDescriptionID
    , TempNonSamplingSummary.timepreviousSampleEnd
    , TempNonSamplingSummary.timeSampleStarted
    , TempNonSamplingSummary.TotalNonSampleMinutes
    , 'Not fishing' AS TrapStatus
    , SubSite.siteID
    , Site.siteName
    , TempNonSamplingSummary.trapPositionID
    , SubSite.subSiteName
FROM 
    Site RIGHT JOIN 
        (
            TempNonSamplingSummary LEFT JOIN SubSite ON 
                TempNonSamplingSummary.trapPositionID = SubSite.subSiteID
        ) ON Site.siteID = SubSite.siteID
ORDER BY 
      TempNonSamplingSummary.timepreviousSampleEnd
    , SubSite.siteID
    , TempNonSamplingSummary.trapPositionID;


-- 5.  qrySumUnmarkedByTrap_Run_e_CheckForErrors
-- 	This one is a select query that can be used to check the above results against the unmarked fish column in TempChinookSampling_i_final.  
--  The series to develop the TempChinookSampling_i_final must be run first if this query is to be used.  
--  If you want me to make a table with it let me know.

SELECT 
    TempChinookSampling_i_final.Date
    , TempSumUnmarkedByTrap_Run_Final.trapPositionID
    , TempChinookSampling_i_final.TrapPosition
    , TempChinookSampling_i_final.UnmarkedCHN
    , TempChinookSampling_i_final.IncludeInAnalysis
    , Sum(TempSumUnmarkedByTrap_Run_Final.Unmarked) AS SumOfUnmarked
FROM TempChinookSampling_i_final 
    INNER JOIN TempSumUnmarkedByTrap_Run_Final ON 
        (TempChinookSampling_i_final.Date = TempSumUnmarkedByTrap_Run_Final.SampleDate) 
        AND 
        (TempChinookSampling_i_final.TrapPosition = TempSumUnmarkedByTrap_Run_Final.TrapPosition)
GROUP BY 
    TempChinookSampling_i_final.Date
    , TempSumUnmarkedByTrap_Run_Final.trapPositionID
    , TempChinookSampling_i_final.TrapPosition
    , TempChinookSampling_i_final.UnmarkedCHN
    , TempChinookSampling_i_final.IncludeInAnalysis
HAVING 
    (
        ((TempChinookSampling_i_final.IncludeInAnalysis)="yes") 
        AND 
        ((Sum(TempSumUnmarkedByTrap_Run_Final.Unmarked))<>[TempChinookSampling_i_final]![UnmarkedCHN])
    );

