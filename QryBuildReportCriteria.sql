--
--  Query to build the report criteria
--  I.e., this finds all the TrapVisitID's to a site between specified dates.
--


DROP TABLE TempReportCriteria_Trapvisit;


SELECT TrapVisit.projectDescriptionID
        , TrapVisit.trapVisitID
        , Subsite.siteID
        , Site.siteName
    INTO TempReportCriteria_Trapvisit
    FROM 
        (
            Site INNER JOIN SubSite ON 
                Site.siteID = SubSite.siteID
        )
        INNER JOIN TrapVisit ON 
            SubSite.subSiteID = TrapVisit.trapPositionID
    WHERE 
        (
            (
                (Subsite.siteID)= SITE 
            )
            AND 
                (
                    (TrapVisit.visitTime) >= #STRT.DT#
                    AND 
                    (TrapVisit.visitTime) <= #END.DT#
                )
        );
