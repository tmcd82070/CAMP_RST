-- Series to display target species by run
-- cshannon 10/21/2013  Edits 11/5/2013 to include trap gear code and half cone fields.
-- edit 5/19/2014 to exclude Adult and grilse life stages.
--
-- ***Requires TempReportCriteria_TrapVisit be developed first***
-- ***Requires TempSamplingSummary be developed first***
--
-- ***NOTE THAT 
-- The series only pulls data for non-valid trap visits, where tempSamplingSummary includeCatchID is 2 ='no' and there are target fish in the catch.


-- 1.  qrySumUnmarkedByTrap_Run_X_a uses tempSamplingSummary to limit records to those excluded from analysis and builds temp table 		TempSumUnmarkedByTrap_Run_a.  The table TempSumUnmarkedByTrap_Run_a is recycled from earlier use in the series to build table 	TempSumUnmarkedByTrap_Run_Final used in production analyses.  The field TrapStatus identifies these records as 'Sample excluded' which can be used to color code FL analysis results if desired. 

-- Jason adds a DROP TABLE to get this to work.
DROP TABLE TempSumUnmarkedByTrap_Run_a;

SELECT TempSamplingSummary.projectDescriptionID AS ProjID, TempSamplingSummary.trapVisitID, DateValue(TempSamplingSummary!timeSampleEnded) AS SampleDate, TempSamplingSummary.timeSampleStarted AS StartTime, TempSamplingSummary.timeSampleEnded AS EndTime, TempSamplingSummary.TotalSampleMinutes AS SampleMinutes, 'Sample excluded' AS TrapStatus, SubSite.siteID, Site.siteName, TempSamplingSummary.trapPositionID, SubSite.subSiteName AS TrapPosition, TempSamplingSummary.sampleGearID, luSampleGear.sampleGear, TempSamplingSummary.halfConeID, luNoYes.noYes AS HalfCone INTO TempSumUnmarkedByTrap_Run_a
FROM Site RIGHT JOIN (((TempSamplingSummary LEFT JOIN SubSite ON TempSamplingSummary.trapPositionID = SubSite.subSiteID) LEFT JOIN luSampleGear ON TempSamplingSummary.sampleGearID = luSampleGear.sampleGearID) LEFT JOIN luNoYes ON TempSamplingSummary.halfConeID = luNoYes.noYesID) ON Site.siteID = SubSite.siteID
WHERE (((TempSamplingSummary.includeCatchID)=2))
ORDER BY TempSamplingSummary.timeSampleStarted, Site.siteName, SubSite.subSiteName;


-- 2.  qrySumUnmarkedByTrap_Run_X_b builds the catch table summary for excluded unmarked Chinook and outputs to table 		TempSumUnmarkedByTrap_Run_b.  The table TempSumUnmarkedByTrap_Run_a is recycled from earlier use in the series to build table 		TempSumUnmarkedByTrap_Run_Final used in production analyses.

-- Jason adds a DROP Table to get this to work.  
DROP TABLE TempSumUnmarkedByTrap_Run_b;
	
SELECT TempSamplingSummary.projectDescriptionID, TempSamplingSummary.trapVisitID, Sum(CatchRaw!n) AS Unmarked, IIf(CatchRaw!finalRunID<6 And CatchRaw!finalRunID<>2,luRun!run,'Unassigned') AS FinalRun, IIf(CatchRaw!lifeStageID<10 Or CatchRaw!lifeStageID>15 And CatchRaw!lifeStageID<20,luLifeStageCAMP!lifeStageCAMP,IIf(CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'AdultSubAdult','Unassigned')) AS LifeStage, CatchRaw.forkLength, IIf(CatchRaw!randomID=2 Or CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'No','Yes') AS RandomSelection INTO TempSumUnmarkedByTrap_Run_b
FROM (TempSamplingSummary INNER JOIN TrapVisit ON (TempSamplingSummary.projectDescriptionID = TrapVisit.projectDescriptionID) AND (TempSamplingSummary.trapVisitID = TrapVisit.trapVisitID)) INNER JOIN ((((CatchRaw LEFT JOIN luRun ON CatchRaw.finalRunID = luRun.runID) LEFT JOIN luLifeStage ON CatchRaw.lifeStageID = luLifeStage.lifeStageID) LEFT JOIN luLifeStageCAMP ON luLifeStage.lifeStageCAMPID = luLifeStageCAMP.lifeStageCAMPID) LEFT JOIN MarkExisting ON (CatchRaw.catchRawID = MarkExisting.catchRawID) AND (CatchRaw.projectDescriptionID = MarkExisting.projectDescriptionID)) ON (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE (((CatchRaw.fishOriginID)<>1 Or (CatchRaw.fishOriginID) Is Null) AND ((CatchRaw.n)>0) AND ((CatchRaw.releaseID) Like 0 Or (CatchRaw.releaseID) Like 255) AND ((CatchRaw.taxonID)='R.TAXON') AND ((MarkExisting.markTypeID)=0) AND ((MarkExisting.markColorID)=252) AND ((MarkExisting.markPositionID)=252))
GROUP BY TempSamplingSummary.projectDescriptionID, TempSamplingSummary.trapVisitID, IIf(CatchRaw!finalRunID<6 And CatchRaw!finalRunID<>2,luRun!run,'Unassigned'), IIf(CatchRaw!lifeStageID<10 Or CatchRaw!lifeStageID>15 And CatchRaw!lifeStageID<20,luLifeStageCAMP!lifeStageCAMP,IIf(CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'AdultSubAdult','Unassigned')), CatchRaw.forkLength, IIf(CatchRaw!randomID=2 Or CatchRaw!lifeStageID=11 Or CatchRaw!lifeStageID=15,'No','Yes'), TempSamplingSummary.includeCatchID
HAVING (((TempSamplingSummary.includeCatchID)=2));


-- 3.  qrySumUnmarkedByTrap_Run_X_c  Brings the two temp tables together and outputs to final table.  Only sampling periods with >0 Chinook are included.  The fields in the table are identical to those in table TempSumUnmarkedByTrap_Run_Final.  The table generated by the sql below, TempSumUnmarkedByTrap_Run_X_final, is a new table that will need to be added to each CAMP.mdb prior to running analysis.
	
-- Jason adds a DROP Table to get this to work.  
DROP TABLE TempSumUnmarkedByTrap_Run_X_Final;

SELECT TempSumUnmarkedByTrap_Run_a.ProjID, 
       TempSumUnmarkedByTrap_Run_a.trapVisitID, 
       TempSumUnmarkedByTrap_Run_a.SampleDate, 
       TempSumUnmarkedByTrap_Run_a.StartTime, 
       TempSumUnmarkedByTrap_Run_a.EndTime, 
       TempSumUnmarkedByTrap_Run_a.SampleMinutes, 
       TempSumUnmarkedByTrap_Run_a.TrapStatus, 
       TempSumUnmarkedByTrap_Run_a.siteID, 
       TempSumUnmarkedByTrap_Run_a.siteName, 
       TempSumUnmarkedByTrap_Run_a.trapPositionID, 
       TempSumUnmarkedByTrap_Run_a.TrapPosition, 
       TempSumUnmarkedByTrap_Run_a.sampleGearID, 
       TempSumUnmarkedByTrap_Run_a.sampleGear, 
       TempSumUnmarkedByTrap_Run_a.halfConeID, 
       TempSumUnmarkedByTrap_Run_a.HalfCone, 
       IIf(TempSumUnmarkedByTrap_Run_b!Unmarked Is Null,0,TempSumUnmarkedByTrap_Run_b!Unmarked) AS Unmarked, 
       IIf(TempSumUnmarkedByTrap_Run_b!FinalRun Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!FinalRun) AS FinalRun, 
       IIf(TempSumUnmarkedByTrap_Run_b!LifeStage Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!LifeStage) AS lifeStage, 
       TempSumUnmarkedByTrap_Run_b.forkLength, IIf(TempSumUnmarkedByTrap_Run_b!RandomSelection Is Null,'Yes',TempSumUnmarkedByTrap_Run_b!RandomSelection) AS RandomSelection 
INTO TempSumUnmarkedByTrap_Run_X_Final
FROM TempSumUnmarkedByTrap_Run_a LEFT JOIN TempSumUnmarkedByTrap_Run_b ON (TempSumUnmarkedByTrap_Run_a.trapVisitID = TempSumUnmarkedByTrap_Run_b.trapVisitID) AND (TempSumUnmarkedByTrap_Run_a.ProjID = TempSumUnmarkedByTrap_Run_b.projectDescriptionID)
WHERE (((IIf(TempSumUnmarkedByTrap_Run_b!LifeStage Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!LifeStage))<>'AdultSubAdult'))
ORDER BY TempSumUnmarkedByTrap_Run_a.StartTime, TempSumUnmarkedByTrap_Run_a.siteName, TempSumUnmarkedByTrap_Run_a.TrapPosition, IIf(TempSumUnmarkedByTrap_Run_b!FinalRun Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!FinalRun), IIf(TempSumUnmarkedByTrap_Run_b!LifeStage Is Null,'Unassigned',TempSumUnmarkedByTrap_Run_b!LifeStage), IIf(TempSumUnmarkedByTrap_Run_b!RandomSelection Is Null,'Yes',TempSumUnmarkedByTrap_Run_b!RandomSelection);



