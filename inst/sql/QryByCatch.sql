-- this is Connie's SQL for bycatch, sent via email 19 Sep 2013


SELECT Year([TrapVisit]![visitTime]) AS [Year]
    , Month([TrapVisit]![visitTime]) AS [Month]
    , Site.siteName
    , DateValue(TrapVisit!visitTime) AS [Date]
    , SubSite.subSiteName AS Subsite
    , luTaxon.commonName AS Taxon
    , Sum(CatchRaw.n) AS n

FROM
    (
        (Site RIGHT JOIN SubSite ON Site.siteID = SubSite.siteID)
        INNER JOIN
            (
                (
                    TempReportCriteria_TrapVisit INNER JOIN TrapVisit ON
                    (TempReportCriteria_TrapVisit.trapVisitID = TrapVisit.trapVisitID) AND
                    (TempReportCriteria_TrapVisit.projectDescriptionID = TrapVisit.projectDescriptionID)
                )
                LEFT JOIN luNoYes AS luNoYes_1 ON TrapVisit.includeCatchID = luNoYes_1.noYesID
            ) ON SubSite.subSiteID = TrapVisit.trapPositionID
    ) INNER JOIN
        (luTaxon RIGHT JOIN CatchRaw ON luTaxon.taxonID = CatchRaw.taxonID) ON
        (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND
        (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)

GROUP BY Year([TrapVisit]![visitTime]), Month([TrapVisit]![visitTime]), Site.siteName, DateValue(TrapVisit!visitTime), SubSite.subSiteName, luTaxon.commonName

HAVING
(
    (
        (luTaxon.commonName) Not Like 'Chinook salmon' And (luTaxon.commonName) Not Like 'Not yet assigned' And (luTaxon.commonName) Not Like 'other' And 
        (luTaxon.commonName) Not Like 'Not applicable (n/a)'
    )
    AND
    (
        (Sum(CatchRaw.n)) Not Like 0
    )
)

ORDER BY DateValue(TrapVisit!visitTime), SubSite.subSiteName;
