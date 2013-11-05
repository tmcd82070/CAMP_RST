-- This SQL series compiles Efficiency test release and recapture data
-- 
-- 
-- Please note that there are programs using inclined plane and other traps that are not suited to this type of analsyis.
-- 
-- 1.  QryRelRecap_1_PullReleaseData_Selections
-- 	This sql pulls efficiency test data based on user set criteria, by target site (see BuildReportCriteria_Releases).
-- 	Only tests with an IncludeTestID <>2 are included
-- 	The release purpose must be 1 = Trap Capture efficiency test are included
-- 	release table taxonID must be 161980 = Chinook
-- 	The test days field is disregarded

DROP TABLE TempRelRecap_1;

SELECT TempReportCriteria_Release.projectDescriptionID
    , TempReportCriteria_Release.releaseID
    , 'Yes' AS IncludeTest
    , TempReportCriteria_Release.targetSiteID AS SiteID
    , Release!releaseTime AS ReleaseDate
    , Site.siteName AS SiteName
    , Release.nReleased
    , Release.includeTestID
    , Release.releasePurposeID
    , Release.markedTaxonID 
INTO TempRelRecap_1
FROM Site RIGHT JOIN 
    (
        (TempReportCriteria_Release INNER JOIN Release ON 
            (TempReportCriteria_Release.projectDescriptionID = Release.projectDescriptionID) 
            AND 
            (TempReportCriteria_Release.releaseID = Release.releaseID)
        ) INNER JOIN ReleaseXTargetSite ON 
            (Release.releaseID = ReleaseXTargetSite.releaseID) 
            AND 
            (Release.projectDescriptionID = ReleaseXTargetSite.projectDescriptionID)
    ) ON Site.siteID = ReleaseXTargetSite.targetSiteID
WHERE 
    (
        ((Release.includeTestID)<>2 
        Or 
        (Release.includeTestID) Is Null) 
        AND 
        ((Release.releasePurposeID)=1) 
        AND 
        ((Release.markedTaxonID)='R.TAXON')
    );

-- 2.  QryRelRecap_2_PullRecapData
-- 	This sql pulls together recaptures from the Catch table that match a ReleaseID included in the output table from above.
-- 	Include catch is included (IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID Like 3,'No','Yes'))
-- 	HalfCone is included (IIf(TrapVisit!halfConeID=1,'Yes','No')
-- 	SampleGear is included with no restrictions
-- 	taxonID must be 161980 = Chinook
-- 	visitType must be >1 And <5 (a sampling type visit)

DROP TABLE TempRelRecap_2;

SELECT TempRelRecap_1.projectDescriptionID
    , TempRelRecap_1.releaseID
    , IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID Like 3,'No','Yes')) AS IncludeCatch
    , TrapVisit.visitTime AS VisitTime
    , TrapVisit.trapPositionID
    , SubSite.subSiteName AS TrapPosition
    , Sum(CatchRaw.n) AS Recaps
    , luSampleGear.sampleGear AS RST
    , IIf(TrapVisit!halfConeID=1,'Yes','No') AS HalfCone 
INTO TempRelRecap_2
FROM 
    (
        SubSite INNER JOIN 
            (
                luSampleGear RIGHT JOIN TrapVisit ON 
                    luSampleGear.sampleGearID = TrapVisit.sampleGearID
            ) ON 
                SubSite.subSiteID = TrapVisit.trapPositionID
    ) INNER JOIN 
        (
            TempRelRecap_1 INNER JOIN CatchRaw ON 
                (TempRelRecap_1.projectDescriptionID = CatchRaw.projectDescriptionID) 
                AND 
                (TempRelRecap_1.releaseID = CatchRaw.releaseID)) ON 
                    (TrapVisit.trapVisitID = CatchRaw.trapVisitID) 
                    AND 
                    (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
WHERE 
    (
        ((SubSite.siteID)=[TempRelRecap_1]![SiteID]) 
        AND 
        ((CatchRaw.taxonID)='R.TAXON') 
        AND 
        ((TrapVisit.visitTypeID)>1 And (TrapVisit.visitTypeID)<5)
    )
GROUP BY TempRelRecap_1.projectDescriptionID
    , TempRelRecap_1.releaseID
    , IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID Like 3,'No','Yes'))
    , TrapVisit.visitTime
    , TrapVisit.trapPositionID
    , SubSite.subSiteName
    , luSampleGear.sampleGear
    , IIf(TrapVisit!halfConeID=1,'Yes','No')
ORDER BY TempRelRecap_1.projectDescriptionID
    , TempRelRecap_1.releaseID
    , IIf(TrapVisit!includeCatchID=2,'No',IIf(TrapVisit!fishProcessedID Like 3,'No','Yes'))
    , TrapVisit.visitTime
    , SubSite.subSiteName;
	
-- 3.  QryRelRecap_3_Final
-- 	This sql pulls the recaptures together with the releases.  
-- 	If no recaptures are found for a release the Recap field is set to zero.

DROP TABLE TempRelRecap_final;	

SELECT TempRelRecap_1.projectDescriptionID
    , TempRelRecap_1.releaseID
    , TempRelRecap_1.IncludeTest
    , TempRelRecap_2.IncludeCatch
    , TempRelRecap_1.ReleaseDate
    , TempRelRecap_2.VisitTime
    , TempRelRecap_1.SiteID
    , TempRelRecap_1.SiteName
    , TempRelRecap_2.trapPositionID
    , TempRelRecap_2.TrapPosition
    , TempRelRecap_1.nReleased
    , IIf(TempRelRecap_2!releaseID Is Null,0,TempRelRecap_2!Recaps) AS Recaps
    , TempRelRecap_2.RST
    , TempRelRecap_2.HalfCone 
INTO TempRelRecap_final
FROM TempRelRecap_1 LEFT JOIN TempRelRecap_2 ON 
    (TempRelRecap_1.releaseID = TempRelRecap_2.releaseID) 
    AND 
    (TempRelRecap_1.projectDescriptionID = TempRelRecap_2.projectDescriptionID)
ORDER BY TempRelRecap_1.ReleaseDate
    , TempRelRecap_2.VisitTime
    , TempRelRecap_1.SiteName
    , TempRelRecap_2.TrapPosition;
