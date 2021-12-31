--SQL documentation for series to build trapping effort dataset.
--C. Shannon May 22, 2015
--	Modified again May 28, 2015 (Tips section added)
--	Modified again May 29, 2015 (Date changed to EffortDate start with step 11 or sql y05 )
--	And yet again in the pm of May 29, 2015 (I broke the last query in the series when I changed the field 	name from date to effortDate.)
--Notes: 
--•	This series was developed to prepare trapping effort data from the CAMP.mdb to be displayed in a graph.  The graph is to display effort by Julian week including times effort that resulted in a valid sample, effort for invalid samples not used in analyses, and times when no trapping effort was extended.
--•	The series is dependent upon output tables from other SQL series used to develop table TempSumUnmarkedByTrap_Run_Final.  Specifically the tables bulleted below.  Therefore BuildReportCriteria and TempSamplingSummary series must be run prior to using the below SQL.
--o	TempSamplingSummary
--o	TempSamplingSummary_Final
--•	This series includes 17 Queries.  Most were necessary to capture times not fishing.  There are an additional 5 queries Included in the mdb I’ll be sending.  These extra queries can be used to check the output.  
--•	Tips:
--For the QrySamplePeriod.sql you should be fine so long as you use Trent’s version.  His version is already building the first table and is already dropping temp tables before it builds them with my sql.
--
--For the new series SQLGraphTrappingEffortDocumentation_22May2015 step 1.
--Build a new table instead of using the template tblNonTrapSampleTemplate_DoNotModify
--Trent already created the code for this in step one of the series for the non-fishing summary.
--	
--There are several used in this new series that will need to be dropped:
--	TempNonSamplingSummary
--	NonTrapSample1
--	TempEffortSummary_a
--	TempEffortSummary_b
--	TempEffortSummary_a1
--	
--	Table TempEffortSummary_b currently contains the final output table.  
--SQL:
--1.	QSamplingEffortByDay_n1
--	Build a table with an autoID field called TempNonSamplingSummary instead of using the template table  	that Connie uses.
--This make table query builds a new empty table TempNonSamplingSummary with a fresh AutoId field.  The same table was used to prep hours not fishing for table TempSumUnmarkedByTrap_Run_Final.  The queries must be re-run with slightly different criteria to include excluded sampling periods.

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

--2.	QSamplingEffortByDay_n2
--Takes data from table TempSampling Summary and reformats it.

INSERT INTO TempNonSamplingSummary ( orderStartSample, projectDescriptionID, trapVisitID, trapPositionID, timeSampleStarted, TotalNonSampleMinutes, NonVisitDate )
SELECT TempSamplingSummary.orderStartSample, TempSamplingSummary.projectDescriptionID, TempSamplingSummary.trapVisitID, TempSamplingSummary.trapPositionID, TempSamplingSummary.timeSampleStarted, 0 AS TotalNonSampleMinutes, DateValue(TempSamplingSummary!timeSampleStarted) AS NonVisitDate
FROM TempSamplingSummary
WHERE (((TempSamplingSummary.visitTypeID)>1) AND ((TempSamplingSummary.TotalSampleMinutes)>0));

--3.	QSamplingEffortByDay_n3
--Re-joins data from TempSamplingSummary from new table TempNonSamplingSummary adding one to each record’s autoID, OrderNonSample.  

	DROP TABLE NonTrapSample1;
	
--	CREATE TABLE NonTrapSample1
--	    (
--	        timePreviousSampleEnd Date
--	        , OrderNonSample Number
--	        , trapPositionID Number
--	    );

SELECT CDate(TempSamplingSummary!timeSampleEnded) AS timePreviousSampleEnd, CLng(TempNonSamplingSummary!OrderNonSample+1) AS OrderNonSample, TempSamplingSummary.trapPositionID INTO NonTrapSample1
FROM TempNonSamplingSummary INNER JOIN TempSamplingSummary ON (TempNonSamplingSummary.trapPositionID = TempSamplingSummary.trapPositionID) AND (TempNonSamplingSummary.orderStartSample = TempSamplingSummary.orderStartSample)
ORDER BY CLng(TempNonSamplingSummary!OrderNonSample+1);

--4.	QSamplingEffortByDay_n4

--	Uses the two tables created above to join the start of the sampling period to the end.
	UPDATE TempNonSamplingSummary INNER JOIN NonTrapSample1 ON 	(TempNonSamplingSummary.OrderNonSample = NonTrapSample1.OrderNonSample) AND 	(TempNonSamplingSummary.trapPositionID = NonTrapSample1.trapPositionID) SET 	TempNonSamplingSummary.timepreviousSampleEnd = NonTrapSample1!timePreviousSampleEnd;

--5.	QSamplingEffortByDay_n5
--	Calculates minutes not fishing

	UPDATE TempNonSamplingSummary SET TempNonSamplingSummary.TotalNonSampleMinutes = 	DateDiff('n',TempNonSamplingSummary!timepreviousSampleEnd,TempNonSamplingSummary!timeSam	pleStarted)
	WHERE (((TempNonSamplingSummary.timepreviousSampleEnd) Is Not Null) AND 	((TempNonSamplingSummary.timeSampleStarted) Is Not Null));

--6.	QSamplingEffortByDay_n6
--	Pulls data where non fishing minutes >0 and adds them to a new workup table called 	TempEffortSummary_a.

	DROP TABLE TempEffortSummary_a;
	
--	CREATE TABLE TempEffortSummary_a
--	    (
--	        TrapVisit Number
--	        , Position Text
--	        , StartTime Date
--	        , EndTime Date
--	        , TotalMinutes Number
--	        , StartDate Date
--	        , EndDate Date
--	        , MinutesStart Number
--	        , MinutesEnd Number
--	        , EffortID Number
--		, FishingEffort Text
--	    );

SELECT 0 AS TrapVisit, SubSite.subSiteName AS [Position], TempNonSamplingSummary.timepreviousSampleEnd AS StartTime, TempNonSamplingSummary.timeSampleStarted AS EndTime, TempNonSamplingSummary.TotalNonSampleMinutes AS TotalMinutes, DateValue(TempNonSamplingSummary!timepreviousSampleEnd) AS StartDate, DateValue(TempNonSamplingSummary!timeSampleStarted) AS EndDate, DateDiff('n',TempNonSamplingSummary!timepreviousSampleEnd,DateAdd('d',1,DateValue(TempNonSamplingSummary!timepreviousSampleEnd))) AS MinutesStart, IIf(DateValue(TempNonSamplingSummary!timepreviousSampleEnd)=DateValue(TempNonSamplingSummary!timeSampleStarted),TempNonSamplingSummary!TotalNonSampleMinutes,DateDiff('n',DateValue(TempNonSamplingSummary!timeSampleStarted),TempNonSamplingSummary!timeSampleStarted)) AS MinutesEnd, 0 AS EffortID, 'Not fishing' AS FishingEffort INTO TempEffortSummary_a
FROM TempNonSamplingSummary LEFT JOIN SubSite ON TempNonSamplingSummary.trapPositionID = SubSite.subSiteID
WHERE (((TempNonSamplingSummary.TotalNonSampleMinutes)>0));


--7.	QSamplingEffortByDay_y01
--	Appends sampling data found in table TempSamplingSummary_Final with sampling minutes >0 to table 	TempEffortSummary_a.

	INSERT INTO TempEffortSummary_a ( TrapVisit, [Position], StartTime, EndTime, TotalMinutes, StartDate, 	EndDate, MinutesStart, MinutesEnd, EffortID, FishingEffort )
	SELECT TempSamplingSummary_Final.TrapVisit, TempSamplingSummary_Final.Position, 	TempSamplingSummary_Final.[Sample Start] AS StartTime, TempSamplingSummary_Final.[Sample End] 	AS EndTime, TempSamplingSummary_Final.Minutes AS TotalMinutes, 	DateValue(TempSamplingSummary_Final![Sample Start]) AS StartDate, 	DateValue(TempSamplingSummary_Final![Sample End]) AS EndDate, 	DateDiff('n',TempSamplingSummary_Final![Sample 	Start],DateAdd('d',1,DateValue(TempSamplingSummary_Final![Sample Start]))) AS EffortStart, 	IIf(DateValue(TempSamplingSummary_Final![Sample Start])=DateValue(TempSamplingSummary_Final![Sample End]),TempSamplingSummary_Final!Minutes,DateDiff('n',DateValue(TempSamplingSummary_Final![Sample End]),TempSamplingSummary_Final![Sample End])) AS EffortEnd, 0 AS EffortID, 	IIf(TempSamplingSummary_Final![Include Catch]='yes','Included','Excluded') AS FishingEffort
	FROM TempSamplingSummary_Final
	WHERE (((TempSamplingSummary_Final.Minutes) Is Not Null))
	ORDER BY TempSamplingSummary_Final.Position, TempSamplingSummary_Final.[Sample Start];
	
--8.	QSamplingEffortByDay_y02
--	Appends a record for the last start time by trap to table TempEffortSummary_a.  This is to capture 	non fishing for these partial days.

	INSERT INTO TempEffortSummary_a ( TrapVisit, [Position], StartTime, EndTime, TotalMinutes, StartDate, 	EndDate, MinutesEnd, FishingEffort )
	SELECT 0 AS TrapVisit, TempSamplingSummary_Final.Position, 	Last(TempSamplingSummary_Final![Sample End]) AS StartTime, 	Last(DateAdd('d',1,DateValue(TempSamplingSummary_Final![Sample End]))) AS EndTime, 	Last(DateDiff('n',TempSamplingSummary_Final![Sample 	End],DateAdd('d',1,DateValue(TempSamplingSummary_Final![Sample End])))) AS TotalMinutes, 	Last(DateValue(TempSamplingSummary_Final![Sample End])) AS StartDate, 	Last(DateValue(TempSamplingSummary_Final![Sample End])) AS EndDate, 	Last(DateDiff('n',TempSamplingSummary_Final![Sample 	End],DateAdd('d',1,DateValue(TempSamplingSummary_Final![Sample End])))) AS MinutesEnd, 'Not 	fishing' AS FishingEffort
	FROM TempSamplingSummary_Final
	WHERE (((TempSamplingSummary_Final.Minutes)>0))
GROUP BY 0, TempSamplingSummary_Final.Position;

--9.	QSamplingEffortByDay_y03
--Similar to above appends a record for the last end time by trap table TempEffortSummary_a.  This is to capture non fishing for these partial days.

INSERT INTO TempEffortSummary_a ( TrapVisit, [Position], StartTime, EndTime, TotalMinutes, StartDate, EndDate, MinutesEnd, FishingEffort )
SELECT 0 AS TrapVisit, TempSamplingSummary_Final.Position, First(DateValue(TempSamplingSummary_Final![Sample Start])) AS StartTime, First(TempSamplingSummary_Final.[Sample Start]) AS EndTime, First(DateDiff('n',DateValue(TempSamplingSummary_Final![Sample Start]),TempSamplingSummary_Final![Sample Start])) AS TotalMinutes, First(DateValue(TempSamplingSummary_Final![Sample Start])) AS StartDate, First(DateValue(TempSamplingSummary_Final![Sample Start])) AS EndDate, First(DateDiff('n',DateValue(TempSamplingSummary_Final![Sample Start]),TempSamplingSummary_Final![Sample Start])) AS MinutesEnd, 'Not fishing' AS FishingEffort
FROM TempSamplingSummary_Final
WHERE (((TempSamplingSummary_Final.Minutes)>0))
GROUP BY 0, TempSamplingSummary_Final.Position;

--10.	QSamplingEffortByDay_y04
--	Updates the field EffortID with 1, 2, or 3. Depending upon the type of time range found in the StartTime 	and EndTime fields.

	UPDATE TempEffortSummary_a SET TempEffortSummary_a.EffortID = 	IIf(TempEffortSummary_a!MinutesEnd=TempEffortSummary_a!TotalMinutes,1,IIf(TempEffortSummary	_a!MinutesStart+TempEffortSummary_a!MinutesEnd=TempEffortSummary_a!TotalMinutes,2,3));

--11.	QSamplingEffortByDay_y05
--	Builds the output table TempEffortSummary_b and begins adding records parsed midnight to midnight.
--	Adds records for Effort type 1 = Constrained to a single day.

	DROP TABLE TempEffortSummary_b;
	
--	CREATE TABLE TempEffortSummary_b
--	    (
--	        TrapVisit COUNTER
--	        , Position Text
--	        , StartTime Date
--	        , EndTime Date
--	        , TotalMinutes Number
--	        , EffortID Number
--	        , EffortType Text
--	        , EffortDate Date
--	        , Minutes Number
--	        , FishingEffort Text
--		, JWeek Number
--		, Year Number
--	    );

	SELECT TempEffortSummary_a.TrapVisit, TempEffortSummary_a.Position, 	TempEffortSummary_a.StartTime, TempEffortSummary_a.EndTime, 	TempEffortSummary_a.TotalMinutes, TempEffortSummary_a.EffortID, 'Constrained to a single day.' AS 	EffortType, TempEffortSummary_a!EndDate AS EffortDate, TempEffortSummary_a!MinutesEnd AS 	Minutes, TempEffortSummary_a.FishingEffort, 0 AS JWeek, 0 AS [Year] INTO TempEffortSummary_b
	FROM TempEffortSummary_a
	WHERE (((TempEffortSummary_a.EffortID)=1))
	ORDER BY TempEffortSummary_a.Position, TempEffortSummary_a.EndTime;

--12.	QSamplingEffortByDay_y06
--	Appends records for end date and minutes for effort types 1 and 2.  Time extended over night = 1 and 	Extended over multiple days = 2.

	INSERT INTO TempEffortSummary_b ( TrapVisit, [Position], StartTime, EndTime, TotalMinutes, EffortID, 	EffortType, EffortDate, Minutes, FishingEffort )
	SELECT TempEffortSummary_a.TrapVisit, TempEffortSummary_a.Position, 	TempEffortSummary_a.StartTime, TempEffortSummary_a.EndTime, 	TempEffortSummary_a.TotalMinutes, TempEffortSummary_a.EffortID, 	IIf(TempEffortSummary_a!EffortID=2,'Extended over night.','Extended over multiple days.') AS 	EffortType, TempEffortSummary_a!EndDate AS EffortDate, TempEffortSummary_a!MinutesEnd AS 	Minutes, TempEffortSummary_a.FishingEffort
	FROM TempEffortSummary_a
	WHERE (((TempEffortSummary_a.EffortID)=2 Or (TempEffortSummary_a.EffortID)=3));

--13.	QSamplingEffortByDay_y07
--	Appends records for start date and minutes for effort types 1 and 2.  Time extended over night = 1 and 	Extended over multiple days = 2.

	INSERT INTO TempEffortSummary_b ( TrapVisit, [Position], StartTime, EndTime, TotalMinutes, EffortID, 	EffortType, EffortDate, Minutes, FishingEffort )
	SELECT TempEffortSummary_a.TrapVisit, TempEffortSummary_a.Position, 	TempEffortSummary_a.StartTime, TempEffortSummary_a.EndTime, 	TempEffortSummary_a.TotalMinutes, TempEffortSummary_a.EffortID, 	IIf(TempEffortSummary_a!EffortID=2,'Extended over night.','Extended over multiple days.') AS 	EffortType, TempEffortSummary_a!StartDate AS EffortDate, TempEffortSummary_a!MinutesStart AS 	Minutes, TempEffortSummary_a.FishingEffort
	FROM TempEffortSummary_a
	WHERE (((TempEffortSummary_a.EffortID)=2 Or (TempEffortSummary_a.EffortID)=3));
	
--14.	QSamplingEffortByDay_y08
--	Joins the Date table to populate the TempEffortSummary_b table with Julian week and year.

	UPDATE TempEffortSummary_b INNER JOIN Dates ON TempEffortSummary_b.EffortDate = 	Dates.uniqueDate SET TempEffortSummary_b.JWeek = Dates!julianWeek, TempEffortSummary_b.[Year] 	= Dates!year;

--15.	QSamplingEffortByDay_y09
--	Appends 1440 minute records for effort type 3 = extended over multiple days.  Joins on the date table 	and finds any date between the start and end of the times.  Adds one record for each day found.
--	*This is an oddly built query but seems to work.  If things break down this is a good spot to check.

	INSERT INTO TempEffortSummary_b ( TrapVisit, [Position], StartTime, EndTime, TotalMinutes, EffortID, 	EffortType, EffortDate, Minutes, FishingEffort, JWeek, [Year] )
	SELECT TempEffortSummary_a.TrapVisit, TempEffortSummary_a.Position, 	TempEffortSummary_a.StartTime, TempEffortSummary_a.EndTime, 	TempEffortSummary_a.TotalMinutes, TempEffortSummary_a.EffortID, 'Extended over multiple days.' AS 	EffortType, Dates.uniqueDate AS EffortDate, 1440 AS Minutes, TempEffortSummary_a.FishingEffort, 	Dates!julianWeek AS JWeek, Dates.year
	FROM (Dates INNER JOIN TempEffortSummary_b ON Dates.year = TempEffortSummary_b.Year) INNER 	JOIN TempEffortSummary_a ON TempEffortSummary_b.TrapVisit = TempEffortSummary_a.TrapVisit
	GROUP BY TempEffortSummary_a.TrapVisit, TempEffortSummary_a.Position, 	TempEffortSummary_a.StartTime, TempEffortSummary_a.EndTime, 	TempEffortSummary_a.TotalMinutes, TempEffortSummary_a.EffortID, 'Extended over multiple days.', 	Dates.uniqueDate, 1440, TempEffortSummary_a.FishingEffort, Dates!julianWeek, Dates.year, 	TempEffortSummary_a.StartDate, TempEffortSummary_a.EndDate
	HAVING (((Dates.uniqueDate)>DateValue([TempEffortSummary_a]![StartDate]) And 	(Dates.uniqueDate)<DateValue([TempEffortSummary_a]![EndDate])));
	
--16.	QSamplingEffortByDay_y10
--	Creates a temp table populated with each date in each of the Julian weeks by trap.

--	CREATE TABLE TempEffortSummary_a1
--	    (
--	        Position Text
--	        , Year Number
--	        , JWeek Number
--	        , uniqueDate Date
--	    );

	SELECT TempEffortSummary_b.Position, TempEffortSummary_b.Year, TempEffortSummary_b.JWeek, 	Dates.uniqueDate INTO TempEffortSummary_a1
	FROM TempEffortSummary_b INNER JOIN Dates ON (TempEffortSummary_b.Year = Dates.year) AND 	(TempEffortSummary_b.JWeek = Dates.julianWeek)
	GROUP BY TempEffortSummary_b.Position, TempEffortSummary_b.Year, 	TempEffortSummary_b.JWeek, Dates.uniqueDate;
	
--17.	QSamplingEffortByDay_y11
--	Uses the temp table created above to add missing dates to flush out the Julian weeks

INSERT INTO TempEffortSummary_b ( TrapVisit, [Position], StartTime, EndTime, EffortID, TotalMinutes, EffortDate, Minutes, EffortType, FishingEffort, JWeek, [Year] )
SELECT 0 AS TrapVisit, TempEffortSummary_a1.Position, TempEffortSummary_a1.uniqueDate AS StartTime, TempEffortSummary_a1.uniqueDate AS EndTime, 4 AS EffortID, 1440 AS TotalMinutes, TempEffortSummary_a1.uniqueDate AS EffortDate, 1440 AS Minutes, 'Added to fill JWeeks' AS EffortType, 'Not fishing' AS FishingEffort, TempEffortSummary_a1.JWeek, TempEffortSummary_a1.Year
FROM TempEffortSummary_a1 LEFT JOIN TempEffortSummary_b ON (TempEffortSummary_a1.Position = TempEffortSummary_b.Position) AND (TempEffortSummary_a1.Year = TempEffortSummary_b.Year) AND (TempEffortSummary_a1.JWeek = TempEffortSummary_b.JWeek) AND (TempEffortSummary_a1.uniqueDate = TempEffortSummary_b.EffortDate)
GROUP BY 0, TempEffortSummary_a1.Position, TempEffortSummary_a1.uniqueDate, TempEffortSummary_a1.uniqueDate, 4, 1440, TempEffortSummary_a1.uniqueDate, 1440, 'Added to fill JWeeks', 'Not fishing', TempEffortSummary_a1.JWeek, TempEffortSummary_a1.Year, TempEffortSummary_b.EffortDate
HAVING (((TempEffortSummary_b.EffortDate) Is Null));

--18.  get rid of this table so it can be created anew the next time. 
DROP TABLE TempEffortSummary_a1;