-- Series to display target species data for life stage analysis.
-- 
-- cshannon 1/6/2016  
-- 
-- 
-- ''''Requires TempReportCriteria_TrapVisit be developed first***
-- ''''Requires TempSamplingSummary be developed first***
-- 
-- 
-- 
-- The series pulls catch data from valid trap visits (includeCatch = 'yes') and non valid (includeCatch = 'no') and visitTypeID is > 1 = Start trap & begin trapping.
-- 
-- 
-- *weight, and finalRunMethod added 1/5/2016 for Jared Studyvin for use in the development of life stage groups.
-- 
-- 
-- Note that the series is different from that used in production analysis.  This set was modified slightly for use in life stage development.  This code does uses two of the same output tables but the final table name was reset to TempSumUnmarkedByTrap_Run_Final2.
-- 
-- 
-- 
-- ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- 
-- 
-- 1.  qrySumUnmarkedByTrap_Run_za_lifeStage uses tempSamplingSummary to limit records and to build table TempSumUnmarkedByTrap_Run_a

DROP TABLE TempSumUnmarkedByTrap_Run_a;

SELECT TempSamplingSummary.projectDescriptionID AS ProjID, TempSamplingSummary.trapVisitID, DateValue(TempSamplingSummary!timeSampleEnded) AS SampleDate, TempSamplingSummary.timeSampleStarted AS StartTime, TempSamplingSummary.timeSampleEnded AS EndTime, TempSamplingSummary.TotalSampleMinutes AS SampleMinutes, 'Fishing' AS TrapStatus, SubSite.siteID, Site.siteName, TempSamplingSummary.trapPositionID, SubSite.subSiteName AS TrapPosition, TempSamplingSummary.sampleGearID, luSampleGear.sampleGear, TempSamplingSummary.halfConeID, luNoYes.noYes AS HalfCone INTO TempSumUnmarkedByTrap_Run_a
FROM Site RIGHT JOIN (((TempSamplingSummary LEFT JOIN SubSite ON TempSamplingSummary.trapPositionID = SubSite.subSiteID) LEFT JOIN luSampleGear ON TempSamplingSummary.sampleGearID = luSampleGear.sampleGearID) LEFT JOIN luNoYes ON TempSamplingSummary.halfConeID = luNoYes.noYesID) ON Site.siteID = SubSite.siteID
WHERE (((TempSamplingSummary.visitTypeID)>1))
ORDER BY TempSamplingSummary.timeSampleStarted, Site.siteName, SubSite.subSiteName;


-- 2.  qrySumUnmarkedByTrap_Run_zb_lifeStage builds the catch table output and outputs to table TempSumUnmarkedByTrap_Run_b
-- 	*Added FL, Added FinalRun method and Weight on 1/5/2016
DROP TABLE TempSumUnmarkedByTrap_Run_b;

SELECT TempSamplingSummary.projectDescriptionID, TempSamplingSummary.trapVisitID, Sum(CatchRaw!n) AS Unmarked, IIf(CatchRaw!finalRunID<6 And CatchRaw!finalRunID<>2,luRun!run,'Unassigned') AS FinalRun, luRunMethod.runMethod AS RunMethod, IIf(CatchRaw!lifeStageID<10 Or CatchRaw!lifeStageID>15 And CatchRaw!lifeStageID<20,luLifeStageCAMP!lifeStageCAMP,IIf(CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'AdultSubAdult','Unassigned')) AS LifeStage, CatchRaw.forkLength, CatchRaw.weight, IIf(CatchRaw!randomID=2 Or CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'No','Yes') AS RandomSelection INTO TempSumUnmarkedByTrap_Run_b
FROM (TempSamplingSummary INNER JOIN TrapVisit ON (TempSamplingSummary.trapVisitID = TrapVisit.trapVisitID) AND (TempSamplingSummary.projectDescriptionID = TrapVisit.projectDescriptionID)) INNER JOIN (luRunMethod RIGHT JOIN ((((CatchRaw LEFT JOIN luRun ON CatchRaw.finalRunID = luRun.runID) LEFT JOIN luLifeStage ON CatchRaw.lifeStageID = luLifeStage.lifeStageID) LEFT JOIN luLifeStageCAMP ON luLifeStage.lifeStageCAMPID = luLifeStageCAMP.lifeStageCAMPID) LEFT JOIN MarkExisting ON (CatchRaw.catchRawID = MarkExisting.catchRawID) AND (CatchRaw.projectDescriptionID = MarkExisting.projectDescriptionID)) ON luRunMethod.runMethodID = CatchRaw.finalRunMethodID) ON (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE (((TempSamplingSummary.visitTypeID)>1) AND ((CatchRaw.fishOriginID)<>1 Or (CatchRaw.fishOriginID) Is Null) AND ((CatchRaw.n)>0) AND ((CatchRaw.releaseID) Like 0 Or (CatchRaw.releaseID) Like 255) AND ((CatchRaw.taxonID)='R.TAXON') AND ((MarkExisting.markTypeID)=0) AND ((MarkExisting.markColorID)=252) AND ((MarkExisting.markPositionID)=252))
GROUP BY TempSamplingSummary.projectDescriptionID, TempSamplingSummary.trapVisitID, IIf(CatchRaw!finalRunID<6 And CatchRaw!finalRunID<>2,luRun!run,'Unassigned'), luRunMethod.runMethod, IIf(CatchRaw!lifeStageID<10 Or CatchRaw!lifeStageID>15 And CatchRaw!lifeStageID<20,luLifeStageCAMP!lifeStageCAMP,IIf(CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'AdultSubAdult','Unassigned')), CatchRaw.forkLength, CatchRaw.weight, IIf(CatchRaw!randomID=2 Or CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'No','Yes');



-- 3.  qrySumUnmarkedByTrap_Run_zc_lifeStage  Brings the two temp tables together adding zeros for visits with no unmarked catch and 		outputs to  TempSumUnmarkedByTrap_Run_final2 
DROP TABLE TempSumUnmarkedByTrap_Run_Final2;
	
SELECT TempSumUnmarkedByTrap_Run_a.ProjID, TempSumUnmarkedByTrap_Run_a.trapVisitID, TempSumUnmarkedByTrap_Run_a.SampleDate, TempSumUnmarkedByTrap_Run_a.StartTime, TempSumUnmarkedByTrap_Run_a.EndTime, TempSumUnmarkedByTrap_Run_a.SampleMinutes, TempSumUnmarkedByTrap_Run_a.TrapStatus, TempSumUnmarkedByTrap_Run_a.siteID, TempSumUnmarkedByTrap_Run_a.siteName, TempSumUnmarkedByTrap_Run_a.trapPositionID, TempSumUnmarkedByTrap_Run_a.TrapPosition, TempSumUnmarkedByTrap_Run_a.sampleGearID, TempSumUnmarkedByTrap_Run_a.sampleGear, TempSumUnmarkedByTrap_Run_a.halfConeID, TempSumUnmarkedByTrap_Run_a.HalfCone, IIf(TempSumUnmarkedByTrap_Run_b!Unmarked Is Null,0,TempSumUnmarkedByTrap_Run_b!Unmarked) AS Unmarked, IIf(TempSumUnmarkedByTrap_Run_b!FinalRun Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!FinalRun) AS FinalRun, TempSumUnmarkedByTrap_Run_b.RunMethod, IIf(TempSumUnmarkedByTrap_Run_b!LifeStage Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!LifeStage) AS lifeStage, TempSumUnmarkedByTrap_Run_b.forkLength, TempSumUnmarkedByTrap_Run_b.weight, IIf(TempSumUnmarkedByTrap_Run_b!RandomSelection Is Null,'Yes',TempSumUnmarkedByTrap_Run_b!RandomSelection) AS RandomSelection INTO TempSumUnmarkedByTrap_Run_Final2
FROM TempSumUnmarkedByTrap_Run_a LEFT JOIN TempSumUnmarkedByTrap_Run_b ON (TempSumUnmarkedByTrap_Run_a.ProjID=TempSumUnmarkedByTrap_Run_b.projectDescriptionID) AND (TempSumUnmarkedByTrap_Run_a.trapVisitID=TempSumUnmarkedByTrap_Run_b.trapVisitID)
WHERE (((IIf(TempSumUnmarkedByTrap_Run_b!LifeStage Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!LifeStage))<>'AdultSubAdult'))
ORDER BY TempSumUnmarkedByTrap_Run_a.StartTime, TempSumUnmarkedByTrap_Run_a.siteName, TempSumUnmarkedByTrap_Run_a.TrapPosition, IIf(TempSumUnmarkedByTrap_Run_b!FinalRun Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!FinalRun), IIf(TempSumUnmarkedByTrap_Run_b!LifeStage Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!LifeStage), IIf(TempSumUnmarkedByTrap_Run_b!RandomSelection Is Null,'Yes',TempSumUnmarkedByTrap_Run_b!RandomSelection);








