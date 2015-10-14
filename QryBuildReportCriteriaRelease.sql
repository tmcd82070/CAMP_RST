-- This is the BuildReportCriteria_Releases query.  This is different from the
-- BuildReportCrietria for regular catches.

DROP TABLE TempReportCriteria_Release;


SELECT Release.projectDescriptionID,
                Release.releaseID,
                Site.siteName,
                Site.siteID,
                ReleaseXTargetSite.targetSiteID,
                Release.releaseTime,
                Release.testDays,
                DateAdd('d',[Release]![testDays],[Release]![releaseTime]) AS PlusTestDays
        INTO TempReportCriteria_Release
        FROM Site
            INNER JOIN
                (
                    Release INNER JOIN ReleaseXTargetSite ON
                        (Release.releaseID = ReleaseXTargetSite.releaseID)
                        AND
                        (Release.projectDescriptionID = ReleaseXTargetSite.projectDescriptionID)
                ) ON
                    Site.siteID = ReleaseXTargetSite.targetSiteID
        WHERE (
                (
                    (Site.siteID)=SITE
                )
                AND
                (
                    (Release.releaseTime) >= #STRT.DT#
                    And
                    (Release.releaseTime)<= #END.DT#
                )
              )
        ORDER BY Release.releaseTime;
