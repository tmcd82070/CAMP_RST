-- Series to modify trap position when lengthy gaps occurr in the data, alias LongGapLoop
-- To be run only after a dcount finds lengthy gaps
-- To be used only for production analysis.  Series modifies key field trapPositionID in TempSumUnmarkedByTrap_Run_Final to accept decimals.  This means other sql that join will need to join on field OldtrapPositionID or alternative TempSumUnmarkedByTrap_Run_Final must be recreated without running the LongGapLoop series.
-- 
-- cshannon 3/3/2016
-- jason 3/8/2016 - changed the 7 days = 10080 minutes to R.FISHGAPMIN
-- 
-- Function:
-- This series is an addition to the original SQL for developing unmarked Chinook by run and life stage_19May2014.
-- 
-- Purpose:  
-- This series was needed to modify the TempSumUnmarkedByTrap_Run_final TrapPositionID in cases where a lenghty gap in fishing occurred, for example a trap was fished, then not fished for a lengthy period of 7 days, and then fished again. The sql finds these gaps and groups all data subsequent to the gap with an update to the trapPositionID.
-- 
-- '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- 
-- 'The longGapLoop series below should be preceeded by an if/else dcount of records meeting the gap criteria.  This will prevent the sql from being deployed when no long gaps are found.
-- 
-- 'My sql below uses 7 days, actually 10080 minutes since that is what is in TempSumUnmarkedByTrap_Run_final.
-- An example might be as follows
-- 
-- 	If DCount("SampleMinutes","TempSumUnmarkedByTrap_Run_Final","SampleMinutes > 10080")>0 then
-- 		msgbox "Long gaps were found in the data so the LongGapLoop series should be run. Paste it here."
-- 		
-- 	else
-- 		msgbox "No long gaps were found. Skip the LongGapLoop series."
-- 		
-- 	End if
-- 	
-- Some existing temp tables are used to develop the data need for the LongGapLoop, TempNonSamplingSummary
-- 
-- 1. qrySumUnmarkedByTrap_Run_e_LongGapLoop_01
-- 
-- '''Drop table TempNonSamplingSummary
-- Notes: The first query creates a new empty copy of tblNonTrapSampleTemplate_DoNotModify.  Code already in use in the tempNonSamplingSummary series can be used to build a table instead.

DROP TABLE TempNonSamplingSummary; 

-- SELECT tblNonTrapSampleTemplate_DoNotModify.* INTO TempNonSamplingSummary
-- FROM tblNonTrapSampleTemplate_DoNotModify;

CREATE TABLE TempNonSamplingSummary
    (
        OrderNonSample COUNTER
        , projectDescriptionID Number
        , trapPositionID Number
        , timeSampleStarted Date
        , timepreviousSampleEnd Date
    );	



-- 2. qrySumUnmarkedByTrap_Run_e_LongGapLoop_02
-- 
-- Notes: This query finds long gaps in sampling and adds them to the re-purposed table TempNonSammplingSummary.

INSERT INTO TempNonSamplingSummary ( projectDescriptionID, trapPositionID, timeSampleStarted )
SELECT TempSumUnmarkedByTrap_Run_Final.ProjID, Format(TempSumUnmarkedByTrap_Run_Final!trapPositionID,'fixed') AS trapPositionID, TempSumUnmarkedByTrap_Run_Final.EndTime
FROM TempSumUnmarkedByTrap_Run_Final
GROUP BY TempSumUnmarkedByTrap_Run_Final.ProjID, Format(TempSumUnmarkedByTrap_Run_Final!trapPositionID,'fixed'), TempSumUnmarkedByTrap_Run_Final.EndTime, TempSumUnmarkedByTrap_Run_Final.SampleMinutes, TempSumUnmarkedByTrap_Run_Final.TrapStatus
HAVING (((TempSumUnmarkedByTrap_Run_Final.SampleMinutes)>R.FISHGAPMIN) AND ((TempSumUnmarkedByTrap_Run_Final.TrapStatus)='Not fishing'));


-- 3. qrySumUnmarkedByTrap_Run_e_LongGapLoop_03
-- 
-- Notes: This query deletes any existing records in the NonTrapSample1 table.  The table will be re-purposed to contain the end date for trap visit date ranges that will be modified.

DELETE DISTINCTROW NonTrapSample1.*
FROM NonTrapSample1;


-- 4. qrySumUnmarkedByTrap_Run_e_LongGapLoop_04
-- 
-- Notes:  This query appends date, trapPositionID, and the auto number assigned in TempNonSammplingSummary summary OrderNonSample field to temp table NonTrapSample1 table.  The OrderNonSample variable is altered by subtractin -1.  Records that match on the OrderNonSample field and that match the TempNonSamplingSummary trapPositionID later become the end dates for date ranges needing to be modified.

INSERT INTO NonTrapSample1 ( OrderNonSample, trapPositionID, timePreviousSampleEnd )
SELECT DISTINCT TempNonSamplingSummary!OrderNonSample-1 AS OrderNonSample, TempNonSamplingSummary.trapPositionID, TempNonSamplingSummary.timeSampleStarted
FROM TempNonSamplingSummary
ORDER BY TempNonSamplingSummary!OrderNonSample-1;

-- SELECT
-- FROM TempNonSampling
-- 5. qrySumUnmarkedByTrap_Run_e_LongGapLoop_05
-- 
-- Notes:  See notes above only records that match on the OrderNonSample and trapPositionID field are used to update the TempNonSamplingSummary table.

UPDATE TempNonSamplingSummary INNER JOIN NonTrapSample1 ON (TempNonSamplingSummary.OrderNonSample = NonTrapSample1.OrderNonSample) AND (TempNonSamplingSummary.trapPositionID = NonTrapSample1.trapPositionID) SET TempNonSamplingSummary.timepreviousSampleEnd = NonTrapSample1!timePreviousSampleEnd;


-- 6. qrySumUnmarkedByTrap_Run_e_LongGapLoop_06
-- 
-- Notes:  This query is the same as qrySumUnmarkedByTrap_Run_e_LongGapLoop_03.  It deletes records in table NonTrapSample1.  The table is used again next.

DELETE DISTINCTROW NonTrapSample1.*
FROM NonTrapSample1;


-- 7. qrySumUnmarkedByTrap_Run_e_LongGapLoop_07
-- 
-- Notes:  This query finds the last day of sampling for each trap in TempSumUnmarkedByTrap_Run_Final and appends it to the NonTrapSample1 table.

INSERT INTO NonTrapSample1 ( trapPositionID, timePreviousSampleEnd )
SELECT TempSumUnmarkedByTrap_Run_Final.trapPositionID, Max(TempSumUnmarkedByTrap_Run_Final.EndTime) AS MaxOfEndTime
FROM TempSumUnmarkedByTrap_Run_Final
GROUP BY TempSumUnmarkedByTrap_Run_Final.trapPositionID;


-- 8. qrySumUnmarkedByTrap_Run_e_LongGapLoop_08
-- 
-- Notes:  Dates in table NonTrapSample1 are used to populate any missing dates in the timepreviousSampleEnd field table TempNonSamplingSummary.

UPDATE NonTrapSample1 INNER JOIN TempNonSamplingSummary ON NonTrapSample1.trapPositionID = TempNonSamplingSummary.trapPositionID SET TempNonSamplingSummary.timepreviousSampleEnd = NonTrapSample1!timePreviousSampleEnd
WHERE (((TempNonSamplingSummary.timepreviousSampleEnd) Is Null));


-- 9. qrySumUnmarkedByTrap_Run_e_LongGapLoop_09
-- 
-- ''Drop TempSumUnmarkedByTrap_Run_b
-- 
-- Notes:  This query re-builds table TempSumUnmarkedByTrap_Run_b so that the new trapPositionID can be created and retained.  The number field properties must be changed from long integer to double.  Query 09 only adds the records that need an update to the trapPositionID. 

DROP TABLE TempSumUnmarkedByTrap_Run_b; 

SELECT TempSumUnmarkedByTrap_Run_Final.ProjID, TempSumUnmarkedByTrap_Run_Final.trapVisitID, TempSumUnmarkedByTrap_Run_Final.SampleDate, TempSumUnmarkedByTrap_Run_Final.StartTime, TempSumUnmarkedByTrap_Run_Final.EndTime, TempSumUnmarkedByTrap_Run_Final.SampleMinutes, TempSumUnmarkedByTrap_Run_Final.TrapStatus, TempSumUnmarkedByTrap_Run_Final.siteID, TempSumUnmarkedByTrap_Run_Final.siteName, TempSumUnmarkedByTrap_Run_Final.trapPositionID AS oldtrapPositionID, TempSumUnmarkedByTrap_Run_Final.TrapPosition, TempSumUnmarkedByTrap_Run_Final.sampleGearID, TempSumUnmarkedByTrap_Run_Final.sampleGear, TempSumUnmarkedByTrap_Run_Final.halfConeID, TempSumUnmarkedByTrap_Run_Final.HalfCone, TempSumUnmarkedByTrap_Run_Final.Unmarked, TempSumUnmarkedByTrap_Run_Final.FinalRun, TempSumUnmarkedByTrap_Run_Final.lifeStage, TempSumUnmarkedByTrap_Run_Final.forkLength, TempSumUnmarkedByTrap_Run_Final.RandomSelection, (TempNonSamplingSummary!OrderNonSample/100)+TempNonSamplingSummary!trapPositionID AS trapPositionID INTO TempSumUnmarkedByTrap_Run_b
FROM TempSumUnmarkedByTrap_Run_Final LEFT JOIN TempNonSamplingSummary ON (TempSumUnmarkedByTrap_Run_Final.ProjID = TempNonSamplingSummary.projectDescriptionID) AND (TempSumUnmarkedByTrap_Run_Final.trapPositionID = TempNonSamplingSummary.trapPositionID)
WHERE (((TempSumUnmarkedByTrap_Run_Final.EndTime)>[TempNonSamplingSummary]![timeSampleStarted] And (TempSumUnmarkedByTrap_Run_Final.EndTime)<=[TempNonSamplingSummary]![timepreviousSampleEnd]))
ORDER BY TempSumUnmarkedByTrap_Run_Final.ProjID, TempSumUnmarkedByTrap_Run_Final.EndTime, TempSumUnmarkedByTrap_Run_Final.trapPositionID;


-- 10. qrySumUnmarkedByTrap_Run_e_LongGapLoop_10
-- 
-- Notes:  This query adds any records left to be appended from the TempSumUnmarkedByTrap_Run_Final table, those records that didn't need modifications to the trapPositionID.

INSERT INTO TempSumUnmarkedByTrap_Run_b ( ProjID, trapVisitID, SampleDate, StartTime, EndTime, SampleMinutes, TrapStatus, siteID, siteName, trapPositionID, TrapPosition, sampleGearID, sampleGear, halfConeID, HalfCone, Unmarked, FinalRun, lifeStage, forkLength, RandomSelection, oldtrapPositionID )
SELECT TempSumUnmarkedByTrap_Run_Final.ProjID, TempSumUnmarkedByTrap_Run_Final.trapVisitID, TempSumUnmarkedByTrap_Run_Final.SampleDate, TempSumUnmarkedByTrap_Run_Final.StartTime, TempSumUnmarkedByTrap_Run_Final.EndTime, TempSumUnmarkedByTrap_Run_Final.SampleMinutes, TempSumUnmarkedByTrap_Run_Final.TrapStatus, TempSumUnmarkedByTrap_Run_Final.siteID, TempSumUnmarkedByTrap_Run_Final.siteName, TempSumUnmarkedByTrap_Run_Final.trapPositionID, TempSumUnmarkedByTrap_Run_Final.TrapPosition, TempSumUnmarkedByTrap_Run_Final.sampleGearID, TempSumUnmarkedByTrap_Run_Final.sampleGear, TempSumUnmarkedByTrap_Run_Final.halfConeID, TempSumUnmarkedByTrap_Run_Final.HalfCone, TempSumUnmarkedByTrap_Run_Final.Unmarked, TempSumUnmarkedByTrap_Run_Final.FinalRun, TempSumUnmarkedByTrap_Run_Final.lifeStage, TempSumUnmarkedByTrap_Run_Final.forkLength, TempSumUnmarkedByTrap_Run_Final.RandomSelection, TempSumUnmarkedByTrap_Run_Final!trapPositionID AS oldtrapPositionID
FROM TempSumUnmarkedByTrap_Run_Final LEFT JOIN TempSumUnmarkedByTrap_Run_b ON (TempSumUnmarkedByTrap_Run_Final.StartTime = TempSumUnmarkedByTrap_Run_b.StartTime) AND (TempSumUnmarkedByTrap_Run_Final.trapPositionID = TempSumUnmarkedByTrap_Run_b.oldtrapPositionID) AND (TempSumUnmarkedByTrap_Run_Final.EndTime = TempSumUnmarkedByTrap_Run_b.EndTime) AND (TempSumUnmarkedByTrap_Run_Final.ProjID = TempSumUnmarkedByTrap_Run_b.ProjID)
WHERE (((TempSumUnmarkedByTrap_Run_b.trapPositionID) Is Null));


-- 11.  qrySumUnmarkedByTrap_Run_e_LongGapLoop_11
-- 
-- 'Drop table TempSumUnmarkedByTrap_Run_Final
-- Notes: This query uses data in the TempSumUnmarkedByTrap_Run_b table to re-create the table TempSumUnmarkedByTrap_Run_Final.

DROP TABLE TempSumUnmarkedByTrap_Run_Final; 

SELECT TempSumUnmarkedByTrap_Run_b.* INTO TempSumUnmarkedByTrap_Run_Final
FROM TempSumUnmarkedByTrap_Run_b;



