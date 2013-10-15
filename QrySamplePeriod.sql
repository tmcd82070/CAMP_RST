-- Drop previous TempSamplingSummary table, if it exists
DROP TABLE TempSamplingSummary;

-- QrySamplePeriod1_prep
-- Create the Temp table
CREATE TABLE TempSamplingSummary
    (
          orderStartSample COUNTER
        , projectDescriptionID Number
        , sampleGearID Number
        , trapVisitID Number
        , trapPositionID Number
        , visitTypeID Number
        , trapFunctioningID Number
        , halfConeID Number
        , fishProcessedID Number
        , timeSampleStarted Date
        , timeSampleEnded Date
        , TotalSampleHours Text
        , TotalSampleMinutes Number
        , batchDate Date
        , includeCatchID Number
        , hasCatch Number
        , SumCHN Number
    );




-- QrySamplePeriod2_AppendFor
-- Populate the temp table
INSERT INTO TempSamplingSummary
    (
        projectDescriptionID
        , sampleGearID
        , halfConeID
        , trapVisitID
        , trapPositionID
        , visitTypeID
        , trapFunctioningID
        , fishProcessedID
        , timeSampleEnded
        , BatchDate
        , includeCatchID
        , hasCatch
    )
    SELECT
        TrapVisit.projectDescriptionID
        , TrapVisit.sampleGearID
        , TrapVisit.halfConeID
        , TrapVisit.trapVisitID
        , TrapVisit.trapPositionID
        , TrapVisit.visitTypeID
        , TrapVisit.trapFunctioningID
        , TrapVisit.fishProcessedID
        , CDate([TrapVisit]![visitTime]) AS timeSampleEnded
        , TrapVisit.batchDate
        , IIf([TrapVisit]![includeCatchID]=2,2,IIf([TrapVisit]![fishProcessedID] Like 3,2,1)) AS includeCatchID
        , 2 AS hasCatch
    FROM TrapVisit
        INNER JOIN TempReportCriteria_TrapVisit ON
            (
                (TrapVisit.trapVisitID = TempReportCriteria_TrapVisit.trapVisitID)
                AND (TrapVisit.projectDescriptionID = TempReportCriteria_TrapVisit.projectDescriptionID)
            )
    WHERE
        (
            ((TrapVisit.visitTypeID)<5) AND ((TrapVisit.fishProcessedID) Not Like 2)
        )
    ORDER BY TrapVisit.trapPositionID, CDate([TrapVisit]![visitTime]);


-- Drop the previous matchup table, if it exists. 
DROP TABLE TrapSample1;



-- QrySamplePeriod3_Matchups
-- Create matchup table by adding one to OrderSample

SELECT TempSamplingSummary.visitTypeID
    , TrapVisit.trapPositionID
    , IIf([TrapVisit]![visitTime2] Is Not Null,CDate([TrapVisit]![visitTime2]),CDate([TrapVisit]![visitTime])) AS visitTime2
    , CLng(TempSamplingSummary!OrderStartSample+1) AS OrderStartSample 
INTO TrapSample1
    FROM TempSamplingSummary INNER JOIN TrapVisit ON 
        (TempSamplingSummary.trapPositionID = TrapVisit.trapPositionID) 
        AND 
        (TempSamplingSummary.trapVisitID = TrapVisit.trapVisitID)
WHERE (((TempSamplingSummary.visitTypeID) Not Like 4))
ORDER BY CLng(TempSamplingSummary!OrderStartSample+1);




-- QrySamplePeriod4_UpdateStartSample
-- Update start sample
UPDATE
    TempSamplingSummary
INNER JOIN
    TrapSample1 ON
        (
            (TempSamplingSummary.orderStartSample = TrapSample1.OrderStartSample)
            AND
            (TempSamplingSummary.trapPositionID = TrapSample1.trapPositionID)
        )
SET
    TempSamplingSummary.timeSampleStarted = CDate(TrapSample1!visitTime2)
WHERE
    (
        ((TempSamplingSummary.visitTypeID) Not Like 1)
    );
    

    

--  QrySamplePeriod5_RemoveStartForTrapSet    
--  Sets the include catch to ‘no’ when the first record for each trap position is not a trap set record.
UPDATE 
    TempSamplingSummary 
SET 
    TempSamplingSummary.includeCatchID = 2
WHERE 
    (
        ((TempSamplingSummary.visitTypeID) Not Like 1) 
        AND 
        ((TempSamplingSummary.timeSampleStarted) Is Null)
    );    




-- QrySamplePeriod6_SumTrapHours
-- Compute time between starts and stops
UPDATE
    TempSamplingSummary
SET
    TempSamplingSummary.TotalSampleHours = DateDiff('n',TempSamplingSummary!timeSampleStarted,TempSamplingSummary!timeSampleEnded)\'60' & 
    Format(DateDiff('n',TempSamplingSummary!timeSampleStarted,TempSamplingSummary!timeSampleEnded) Mod 60,'\:00')
    , TempSamplingSummary.TotalSampleMinutes = DateDiff('n',TempSamplingSummary!timeSampleStarted,TempSamplingSummary!timeSampleEnded)
WHERE
    (
        ((TempSamplingSummary.timeSampleStarted) Is Not Null)
        AND
        ((TempSamplingSummary.timeSampleEnded) Is Not Null)
    );


-- QrySamplePeriod7_HasCatch
-- Flag checks that have some catch
UPDATE
    TempSamplingSummary
INNER JOIN
    CatchRaw ON
        (
            (TempSamplingSummary.projectDescriptionID=CatchRaw.projectDescriptionID)
            AND
            (TempSamplingSummary.trapVisitID=CatchRaw.trapVisitID)
        )
SET
    TempSamplingSummary.hasCatch = 1
WHERE
    (
        ((CatchRaw.n) Not Like 0)
    );



-- Drop the previous table containing fish sums, if it exists.
DROP TABLE tempSamplingSumCHN;


-- QrySamplePeriod8_SumChin
-- Sum catch of the intended species. This query needs taxon
SELECT
    TempSamplingSummary.projectDescriptionID
    , TempSamplingSummary.trapVisitID
    , CatchRaw.taxonID, Sum(CatchRaw.n) AS SumOfn 
INTO 
    tempSamplingSumCHN
FROM
    TempSamplingSummary
    INNER JOIN CatchRaw
        ON
            (
                (TempSamplingSummary.projectDescriptionID = CatchRaw.projectDescriptionID)
                AND
                (TempSamplingSummary.trapVisitID = CatchRaw.trapVisitID)
            )
GROUP BY
    TempSamplingSummary.projectDescriptionID
    , TempSamplingSummary.trapVisitID
    , CatchRaw.taxonID
HAVING
    (
        ((CatchRaw.taxonID)='R.TAXON')
    )
ORDER BY Sum(CatchRaw.n);


-- QrySamplePeriod9_UpSum8
-- Merge catches into table
UPDATE
    TempSamplingSummary
INNER JOIN
    tempSamplingSumCHN
    ON
        (
            (TempSamplingSummary.trapVisitID=tempSamplingSumCHN.trapVisitID)
            AND
            (TempSamplingSummary.projectDescriptionID=tempSamplingSumCHN.projectDescriptionID)
        )
SET
    TempSamplingSummary.SumCHN = tempSamplingSumCHN!SumOfn
WHERE
    (
        ((TempSamplingSummary.hasCatch)=Yes)
    );


-- Drop the previous final tempSampleSummary, if it exists.
DROP TABLE TempSamplingSummary_Final;



-- QrySamplePeriodReport
-- Run final sample periods report
SELECT
    TempSamplingSummary.trapVisitID AS TrapVisit
    , SubSite.subSiteName AS [Position]
    , luSampleGear.sampleGear AS Gear
    , IIf(TempSamplingSummary!halfConeID=1,'Half-cone',IIf(TempSamplingSummary!halfConeID=2,'Full-cone','No cone data')) AS [Half Cone]
    , luVisitType.visitType AS [Visit Type]
    , luTrapFunctioning.trapFunctioning AS [Trap Function]
    , luFishProcessed.fishProcessed AS [Fish Processed]
    , TempSamplingSummary.timeSampleStarted AS [Sample Start]
    , TempSamplingSummary.timeSampleEnded AS [Sample End]
    , TempSamplingSummary.TotalSampleHours AS Hours
    , TempSamplingSummary.TotalSampleMinutes AS Minutes
    , IIf(TempSamplingSummary!BatchDate Is Null
    ,Null
    ,DateValue(TempSamplingSummary!BatchDate)) AS [Batch Date]
    , luNoYes_2.noYes AS [Has Catch]
    , TempSamplingSummary.SumCHN
    , luNoYes_1.noYes AS [Include Catch]
    , TempSamplingSummary.orderStartSample
INTO
    TempSamplingSummary_Final
FROM
    (
        (
            (
                (
                    (
                        (
                            (TempSamplingSummary LEFT JOIN luTrapFunctioning ON TempSamplingSummary.trapFunctioningID = luTrapFunctioning.trapFunctioningID) 
                            LEFT JOIN luFishProcessed ON TempSamplingSummary.fishProcessedID = luFishProcessed.fishProcessedID
                        ) 
                        LEFT JOIN luVisitType ON TempSamplingSummary.visitTypeID = luVisitType.visitTypeID
                    ) 
                    LEFT JOIN SubSite ON TempSamplingSummary.trapPositionID = SubSite.subSiteID
                ) 
                LEFT JOIN luSampleGear ON TempSamplingSummary.sampleGearID = luSampleGear.sampleGearID
            ) 
            LEFT JOIN luNoYes ON TempSamplingSummary.halfConeID = luNoYes.noYesID
        ) 
        LEFT JOIN luNoYes AS luNoYes_1 ON TempSamplingSummary.includeCatchID = luNoYes_1.noYesID
    ) 
    LEFT JOIN luNoYes AS luNoYes_2 ON TempSamplingSummary.hasCatch = luNoYes_2.noYesID
ORDER BY 
    SubSite.subSiteName
    , TempSamplingSummary.timeSampleEnded
    , TempSamplingSummary.orderStartSample;
