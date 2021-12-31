--SQL documentation for series to standardize units of measure in table EnvDataRaw
--C. Shannon Nov 15, 2017
--Notes: 
--?	This series was developed to standardize the environmental data units of measure found in the EnvDataRaw table.  The sql below results in the development of a new table EnvDataRaw_Standardized.  
--?	The series is dependent upon output tables from other SQL series used to develop production estimates.  Specifically the tables TempReportCriteria_Trapvisit.  Table TempReportCriteria_Trapvisit should be rebuild prior to each analysis run.
--?	Following is a list of fields modified to contain standard units of measure.
--o	discharge (CFS)
--o	waterDepth (CM)
--o	waterVel (ft/sec)
--o	airTemp (?F)
--o	waterTemp (C)
--o	lightPenetration (CM)
--o	turbidity (NTU)
--o	dissolvedOxygen (Not used, not calculated)
--o	conductivity (Not used, not calculated)
--o	barometer (Not used, not calculated)
--o	cloudCover (Text field left as-is)
--o	weather (Text field left as-is)
--?	SQL
--
--
--1.	QEnv_Standardize01_Select:  this sql uses the TempReportCriteria_Trapvisit table to build a dataset table named EnvDataRaw_StandardSelect.  Table builds in a 10 day buffer on either side of the user selected date range when those data are available.
--Notes:  Must have access to the TempReportCriteria_Trapvisit table else, run sql for that table first.

DROP TABLE EnvDataRaw_StandardSelect;

SELECT TrapVisit.projectDescriptionID, SubSite.siteID, Min(DateAdd('d',-10,(Format(TrapVisit!visitTime,'short date')))) AS MinDate, Max(DateAdd('d',+10,(Format(TrapVisit!visitTime,'short date')))) AS MaxDate INTO EnvDataRaw_StandardSelect
FROM SubSite INNER JOIN (TempReportCriteria_Trapvisit INNER JOIN TrapVisit ON (TempReportCriteria_Trapvisit.projectDescriptionID = TrapVisit.projectDescriptionID) AND (TempReportCriteria_Trapvisit.trapVisitID = TrapVisit.trapVisitID)) ON SubSite.subSiteID = TrapVisit.trapPositionID
GROUP BY TrapVisit.projectDescriptionID, SubSite.siteID;

--2.	QEnv_Standardize02_Build:  This query converts data when necessary to comply with standard units detailed at top of this page.  Creates an output table named EnvDataRaw_Standardized.
--Notes:  The table created with the fist sql above is added to the query to filter data for a specific trap and date range.

DROP TABLE EnvDataRaw_Standardized;

SELECT TrapVisit.projectDescriptionID, TrapVisit.trapVisitID, SubSite.siteID, SubSite.subSiteID, CDate(Format(TrapVisit!visitTime,'short date')) AS measureDate, IIf(EnvDataRaw!discharge Is Null,Null,IIf(EnvDataRaw!dischargeUnitID+12,EnvDataRaw!discharge,EnvDataRaw!discharge*35.314666212661)) AS discharge, IIf(EnvDataRaw!discharge Is Null,Null,12) AS dischargeUnitID, IIf(EnvDataRaw!waterDepth Is Null,Null,IIf(EnvDataRaw!waterDepthUnitID=3,EnvDataRaw!waterDepth,IIf(EnvDataRaw!waterDepthUnitID=6,EnvDataRaw!waterDepth*30.48,IIf(EnvDataRaw!waterDepthUnitID=7,EnvDataRaw!waterDepth*2.54,IIf(EnvDataRaw!waterDepthUnitID=4,EnvDataRaw!waterDepth/10,IIf(EnvDataRaw!waterDepthUnitID=2,EnvDataRaw!waterDepth*100)))))) AS waterDepth, IIf(EnvDataRaw!waterDepth Is Null,Null,3) AS waterDepthUnitID, IIf(EnvDataRaw!waterVelUnitID Is Null,Null,IIf(EnvDataRaw!waterVelUnitID=8,EnvDataRaw!waterVel,IIf(EnvDataRaw!waterVelUnitID=9,EnvDataRaw!waterVel/0.3048,Null))) AS waterVel, IIf(EnvDataRaw!waterVel Is Not Null,8,Null) AS waterVelUnitID, IIf(EnvDataRaw!airTempUnitID Is Null,Null,IIf(EnvDataRaw!airTempUnitID=19,EnvDataRaw!airTemp,IIf(EnvDataRaw!airTempUnitID=18,EnvDataRaw!airTemp*1.8+32))) AS airTemp, IIf(EnvDataRaw!airTemp Is Null,Null,19) AS airTempUnitID, IIf(EnvDataRaw!waterTemp Is Null,Null,IIf(EnvDataRaw!waterTempUnitID=18,EnvDataRaw!waterTemp,((EnvDataRaw!waterTemp-32)*5)/9)) AS waterTemp, IIf(EnvDataRaw!waterTemp Is Null,Null,18) AS waterTempUnitID, IIf(EnvDataRaw!lightPenetration Is Null,Null,IIf(EnvDataRaw!lightPenetrationUnitID=3,EnvDataRaw!lightPenetration,IIf(EnvDataRaw!lightPenetrationUnitID=6,EnvDataRaw!lightPenetration*0.3048,IIf(EnvDataRaw!lightPenetrationUnitID=7,EnvDataRaw!lightPenetration*2.54,IIf(EnvDataRaw!lightPenetrationUnitID=2,EnvDataRaw!lightPenetration*100))))) AS lightPenetration, IIf(EnvDataRaw!lightPenetration Is Null,Null,2) AS lightPenetrationUnitID, EnvDataRaw.turbidity, IIf(EnvDataRaw!turbidity Is Null,Null,EnvDataRaw!turbidityUnitID) AS turbidityUnitID, EnvDataRaw.dissolvedOxygen, EnvDataRaw.dissolvedOxygenUnitID, EnvDataRaw.conductivity, EnvDataRaw.conductivityUnitID, EnvDataRaw.barometer, EnvDataRaw.barometerUnitID, EnvDataRaw.cloudCover, EnvDataRaw.weather INTO EnvDataRaw_Standardized
FROM ((EnvDataRaw_StandardSelect RIGHT JOIN SubSite ON EnvDataRaw_StandardSelect.siteID = SubSite.siteID) RIGHT JOIN TrapVisit ON SubSite.subSiteID = TrapVisit.trapPositionID) INNER JOIN EnvDataRaw ON (TrapVisit.trapVisitID = EnvDataRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = EnvDataRaw.projectDescriptionID)
WHERE (((TrapVisit.visitTime) Between [EnvDataRaw_StandardSelect]![MinDate] And [EnvDataRaw_StandardSelect]![MaxDate]))
ORDER BY CDate(Format(TrapVisit!visitTime,'short date'));
