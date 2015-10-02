-- This SQL series compiles Efficiency test release and recapture data
-- 
-- **BuildReportCriteria_Releases must be run before this (below is the 'else' portion of this code)
-- **Note QrySamplingPeriod series to develop tempSamplingSummary table must be run prior to the series below
-- **-Modified to add the Release table comment field 1/26/2015 _ cshannon
-- **-Modified to pre-set the IncludeCatch field to 'yes' for all recaptured fish 1/27/2015 _cshannon
-- 
-- ''''''''''''''''''''''''''''''''''''''''''''''''''''''
--     Else:
--         'Use criteria to select data for the report
--         DoCmd.SetWarnings False
--         strSQL2 = "SELECT Release.projectDescriptionID, Release.releaseID, Site.siteName, ReleaseXTargetSite.targetSiteID, Release.releaseTime, Release.testDays, DateAdd('d',[Release]![testDays],[Release]![releaseTime]) AS PlusTestDays INTO TempReportCriteria_Release FROM Site INNER JOIN (Release INNER JOIN ReleaseXTargetSite ON (Release.releaseID = ReleaseXTargetSite.releaseID) AND (Release.projectDescriptionID = ReleaseXTargetSite.projectDescriptionID)) ON Site.siteID = ReleaseXTargetSite.targetSiteID"
-- 
--         CriteriaString2 = " WHERE (((Site.siteName)=[Forms]![frmQCReports].[siteName])" & _
--                             " AND ((Release.releaseTime) >=[Forms]![frmQCReports]![FromDate] And (Release.releaseTime)<=[Forms]![frmQCReports]![ToDate]))"
--                 
--         OrderString2 = " ORDER BY Release.releaseTime;"
-- 
--         strSQL2 = strSQL2 & CriteriaString2 & OrderString2
-- 
--         DoCmd.RunSQL strSQL2
--         DoCmd.SetWarnings True
-- 
-- ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- 
-- Please note that there are programs using inclined plane and other traps that are not suited to this type of analsyis.
-- In cases where the releases overlap and a trapVisit may fall within the test period of more than one release, the queries add a row for each release.  So there may be more than one row in the final table for each TrapVisit, they are only unique when you add the ReleaseID.
-- 
-- The sql below does not add release records for when no recaps are made after the test days period is over.  To be included trapVisits must be made within the valid test period.  If a release has no recaptures than all visits with that ReleaseID will have a recap total of 0.
-- 
-- It is possible that a test might be approved and fish released when no traps were in the water, I don’t think those types of tests should be included even if the IncludeTestID is ‘yes’.  I have made no accommodation so that these would be included and so they are not.  If you disagree, I can add an append query to the end of this series.
-- 
-- 
-- 1.  QryRelRecap_1_PullReleaseData_Selections
-- 	This sql pulls efficiency test data based on user set criteria, by target site (see BuildReportCriteria_Releases).
-- 	Only tests with an IncludeTestID <>2 are included
-- 	The release purpose must be 1 = Trap Capture efficiency test are included
-- 	release table taxonID must be 161980 = Chinook
-- 	*ReleaseComments added 1/26/2015

DROP TABLE TempRelRecap_1;
	
SELECT TempReportCriteria_Release.projectDescriptionID, TempReportCriteria_Release.releaseID, 'Yes' AS IncludeTest, TempReportCriteria_Release.targetSiteID AS SiteID, Release!releaseTime AS ReleaseDate, TempReportCriteria_Release.PlusTestDays, Site.siteName AS SiteName, Release.nReleased, Release.includeTestID, Release.releasePurposeID, Release.markedTaxonID, Release.comments INTO TempRelRecap_1
FROM Site RIGHT JOIN ((TempReportCriteria_Release INNER JOIN Release ON (TempReportCriteria_Release.projectDescriptionID = Release.projectDescriptionID) AND (TempReportCriteria_Release.releaseID = Release.releaseID)) INNER JOIN ReleaseXTargetSite ON (Release.releaseID = ReleaseXTargetSite.releaseID) AND (Release.projectDescriptionID = ReleaseXTargetSite.projectDescriptionID)) ON Site.siteID = ReleaseXTargetSite.targetSiteID
WHERE (((Release.includeTestID)<>2 Or (Release.includeTestID) Is Null) AND ((Release.releasePurposeID)=1) AND ((Release.markedTaxonID)='R.TAXON'));



-- 2.  QryRelRecap_2_ValidTestDays
-- 	Creates the final table structure.
-- 	Adds a record for each release for each trapVisit within the days the test was valid.  
-- 	If test periods overlap it creates a record for each release.
-- 	The Recap field is added but set to zero initially
-- 	*ReleaseComments added 1/26/2015
-- 	*IncludeCatch pre-set to 'yes' for all recaptured fish 1/27/2015

DROP TABLE TempRelRecap_final;

SELECT TempSamplingSummary.trapVisitID, TempRelRecap_1.projectDescriptionID, TempRelRecap_1.releaseID, TempRelRecap_1.IncludeTest, 'Yes' AS IncludeCatch, TempRelRecap_1.ReleaseDate, TempRelRecap_1.PlusTestDays, TempSamplingSummary.timeSampleEnded AS VisitTime, TempSamplingSummary.TotalSampleMinutes AS SampleMinutes, SubSite.siteID, Site.siteName, TempSamplingSummary.trapPositionID, SubSite.subSiteName AS TrapPosition, luSampleGear.sampleGear, IIf(TempSamplingSummary!halfConeID=1,'Yes','no') AS HalfCone, TempRelRecap_1.nReleased, 0 AS Recaps, TempRelRecap_1.comments AS ReleaseComments INTO TempRelRecap_final
FROM (Site INNER JOIN TempRelRecap_1 ON Site.siteName = TempRelRecap_1.SiteName) INNER JOIN ((TempSamplingSummary INNER JOIN SubSite ON TempSamplingSummary.trapPositionID = SubSite.subSiteID) LEFT JOIN luSampleGear ON TempSamplingSummary.sampleGearID = luSampleGear.sampleGearID) ON Site.siteID = SubSite.siteID
WHERE (((TempSamplingSummary.timeSampleEnded)>=[TempRelRecap_1]![ReleaseDate] And (TempSamplingSummary.timeSampleEnded)<=[TempRelRecap_1]![PlusTestDays]) AND ((TempSamplingSummary.visitTypeID)>1 And (TempSamplingSummary.visitTypeID)<5))
ORDER BY Site.siteName, SubSite.subSiteName;



-- 3.  QryRelRecap_3_PullRecapData
-- 	This sql pulls together and summarizes by trap visit, recaptures from the Catch table that match a ReleaseID included in the 		final table created above and outputs the results to table tempRelRecap_3
-- 	taxonID must be 161980 = Chinook

DROP TABLE TempRelRecap_3;

SELECT TempRelRecap_final.trapVisitID, TempRelRecap_final.projectDescriptionID, TempRelRecap_final.releaseID, Sum(CatchRaw.n) AS SumOfn INTO TempRelRecap_3
FROM CatchRaw INNER JOIN TempRelRecap_final ON (CatchRaw.releaseID = TempRelRecap_final.releaseID) AND (CatchRaw.trapVisitID = TempRelRecap_final.trapVisitID) AND (CatchRaw.projectDescriptionID = TempRelRecap_final.projectDescriptionID)
GROUP BY TempRelRecap_final.trapVisitID, TempRelRecap_final.projectDescriptionID, TempRelRecap_final.releaseID, CatchRaw.taxonID
HAVING (((CatchRaw.taxonID)='R.TAXON'));


-- 4.  QryRelRecap_4_recapUpdatesFinished
-- 	This sql updates the final table with recaptures summed in the table above, tempRelRecap_3
	
UPDATE TempRelRecap_final INNER JOIN TempRelRecap_3 ON (TempRelRecap_final.releaseID = TempRelRecap_3.releaseID) AND (TempRelRecap_final.projectDescriptionID = TempRelRecap_3.projectDescriptionID) AND (TempRelRecap_final.trapVisitID = TempRelRecap_3.trapVisitID) SET TempRelRecap_final.Recaps = TempRelRecap_3!SumOfn;

