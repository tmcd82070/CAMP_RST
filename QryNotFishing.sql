-- This series develops a table that includes hours not fishing. 10/17/2013 CShannon
--
-- The table references the table tempSamplingSummary and is built similarly
--
-- The table identifies gaps in sampling down to the minute and includes the 
-- trapPossitionID and Date so that it can be used in conjuction with the tempSamplingSummary table.
--
-- **BuildReportCriteria must be run before this
-- **TempSamplingSummary must be developed first
--
-- 1.  qryNonSamplePeriod1_Prep
-- 	* As with the tempSamplingSummary table this first SQL builds a table with an autoID used to sort and match time ranges.

DROP TABLE TempNonSamplingSummary;

CREATE TABLE TempNonSamplingSummary
    (
        OrderNonSample COUNTER
        , orderStartSample Number
        , projectDescriptionID Number
        , trapVisitID Number
        , trapPositionID Number
        , timepreviousSampleEnd Date
        , timeSampleStarted Date
        , TotalNonSampleHours Text
        , TotalNonSampleMinutes Number
        , NonVisitDate Date
    );


-- 2.  qryNonSamplePeriod2_Append
-- 	This SQL filters the tempSamplingSummary records by including only those sampling records where the includeCatchID is 1 = 'yes' and the visitType is > 1 = 'Trap was set'. 
--  Records are appended to table TempNonSamplingSummary created in step 1 above.

INSERT INTO TempNonSamplingSummary 
    ( 
          orderStartSample
        , projectDescriptionID
        , trapVisitID
        , trapPositionID
        , timeSampleStarted
        , TotalNonSampleMinutes
        , NonVisitDate 
    )
SELECT 
      TempSamplingSummary.orderStartSample
    , TempSamplingSummary.projectDescriptionID
    , TempSamplingSummary.trapVisitID
    , TempSamplingSummary.trapPositionID
    , TempSamplingSummary.timeSampleStarted
    , 0 AS TotalNonSampleMinutes
    , DateValue([TempSamplingSummary]![timeSampleStarted]) AS NonVisitDate
FROM TempSamplingSummary
WHERE 
    (
        ((TempSamplingSummary.visitTypeID)>1) 
        AND 
        ((TempSamplingSummary.includeCatchID)=1)
    );


-- 3.   qryNonSamplePeriod3_Matchups
--      This SQL builds temp table NonTrapSample1 using the auto number field assigned to each sampling period in step 2. plus 1.

DROP TABLE NonTrapSample1;
-- IF OBJECT_ID('NonTrapSample1', 'U') IS NOT NULL DROP TABLE dbo.Scores;

SELECT 
      CDate(TempSamplingSummary!timeSampleEnded) AS timePreviousSampleEnd
    , CLng(TempNonSamplingSummary!OrderNonSample+1) AS OrderNonSample
    , TempSamplingSummary.trapPositionID 
INTO NonTrapSample1
FROM TempNonSamplingSummary 
    INNER JOIN TempSamplingSummary ON 
        (TempNonSamplingSummary.trapPositionID = TempSamplingSummary.trapPositionID) 
        AND 
        (TempNonSamplingSummary.orderStartSample = TempSamplingSummary.orderStartSample)
ORDER BY CLng(TempNonSamplingSummary!OrderNonSample+1);


-- 4.  qryNonSamplePeriod4_UpdatePrevSample
--      SQL matches records in temp table NonTrapSample1 to the main TempNonSamplingSummary table and updates field 'timepreviousSampleEnd' 
--      in the main TempNonSamplingSummary table.  Tables are joined on autoID field 'OrderNonSample' and trap position.

UPDATE 
    TempNonSamplingSummary 
    INNER JOIN NonTrapSample1 ON 
        (TempNonSamplingSummary.trapPositionID = NonTrapSample1.trapPositionID) 
        AND 
        (TempNonSamplingSummary.OrderNonSample = NonTrapSample1.OrderNonSample) 
SET TempNonSamplingSummary.timepreviousSampleEnd = CDate(NonTrapSample1!timePreviousSampleEnd);


-- 5.  qryNonSamplePeriod5_SumNonTrapHours
-- 	    This SQL calculates the difference between the previous sample end time and the start time of the next sample and updates 
--      two fields in the main TempNonSamplingSummary table, DateDifference is recorded as hours (text field) and/or minutes (numeric).

UPDATE 
    TempNonSamplingSummary 
SET 
      TempNonSamplingSummary.TotalNonSampleHours = DateDiff('n',TempNonSamplingSummary!timepreviousSampleEnd,TempNonSamplingSummary!timeSampleStarted)\'60' & Format(DateDiff('n',TempNonSamplingSummary!timepreviousSampleEnd,TempNonSamplingSummary!timeSampleStarted) Mod 60,'\:00')
    , TempNonSamplingSummary.TotalNonSampleMinutes = DateDiff('n',TempNonSamplingSummary!timepreviousSampleEnd,TempNonSamplingSummary!timeSampleStarted)
WHERE   (((TempNonSamplingSummary.timepreviousSampleEnd) Is Not Null) 
            AND 
        ((TempNonSamplingSummary.timeSampleStarted) Is Not Null));

-- 6.  qryNonSamplePeriod6_deleteNoGapFound
--      This is the final sql in the series.  It removes records where there was no gap in or where the gap in sampling was < 30 minutes.

DELETE DISTINCTROW TempNonSamplingSummary.*, TempNonSamplingSummary.TotalNonSampleMinutes
FROM TempNonSamplingSummary
WHERE (((TempNonSamplingSummary.TotalNonSampleMinutes)<30));
