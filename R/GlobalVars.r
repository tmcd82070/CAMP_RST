#' @export GlobalVars
#'   
#' @title CAMP RST Global Variables
#'   
#' @description Function to run at the beginning to define global variables.
#'   
#' @param db.file Character string giving the full path and name of the CAMP 
#'   Access data base file. Path relative to \code{getwd()} can be used.
#'   
#' @param sql.code.dir Directory containing all the SQL code required by 
#'   \code{campR}. Defaults to the \code{sql} directory of the \emph{installed} 
#'   \code{campR} package. By default, all SQL code should be placed in the 
#'   \code{inst/sql} directory in the \code{campR} code repository.
#'   
#' @param output.dir Directory for all output.
#'   
#' @param Yes.code Integer defining the code for \code{"Yes"}.  Usually
#'   \code{2}. This code is given in an Access look-up table named
#'   \code{table.names["yes.no.codes"]}. Unless it has changed, this table was
#'   named "\code{luNoYes}" in Access. To read this code from Access, an ODBC
#'   connection is established and disconnected during execution of this
#'   routine.
#'   
#' @param No.code Integer defining the code for \code{"No"}.  Usually \code{1}.
#'   This code is given in an Access look-up table named
#'   \code{table.names["yes.no.codes"]}. Unless it has changed, this table was
#'   named "\code{luNoYes}" in Access. To read this code from Access, an ODBC
#'   connection is established and disconnected during execution of this
#'   routine.
#'   
#' @param samplePeriodCutTime String giving the sample period cut off for batch 
#'   date assignment, in military time. Default value is \code{"04:00:00"}. This
#'   is used to assign batch dates that are missing. If a sample period ends
#'   before this time on a particular day, the batch date assigned is the
#'   previous day. If a sample period ends after this time, batch date assigned
#'   is the day that the sampling period ends (i.e., current day). For example,
#'   assuming \code{samplePeriodCutTime="04:00:00"}, a sample period ending at
#'   01:00:00 (1 am) would assign catch to the previous day.  A sample period
#'   ending at 07:00:00 (7 am) would assign catch to the current day.  See
#'   routine \code{F.assign.batch.date}.
#'   
#' @param max.ok.gap Maximum gap, in hours, that is "okay." Default value is 
#'   \code{2}. Temporal gaps in trapping smaller than this are subsumed by the 
#'   most immediate preceding valid trapping instance. Gaps in trapping bigger 
#'   than this get assigned an imputed catch from the GAM model.  Because the 
#'   GAM model for imputation predicts an hourly rate, this max gap cannot be < 
#'   1 hour.
#'   
#' @param fishingGapMinutes Maximum gap, in minutes, that is NOT okay.  Default 
#'   value is 10,080 minutes or 7 days. Gaps in fishing greater than this 
#'   constitute a "big" gap, and no imputation is performed.  The GAM models are
#'   unstable for "big" gaps. In these cases, the season is broken into two 
#'   periods (before and after the gap) and treated as two separate traps.
#'   
#' @param knotMesh The number of data points required per smoothing spline knot.
#'   Default value is 15. For example, \code{knotMesh=15} means each additional 
#'   knot in a smoothing spline requires an additional 15 data points.  A linear
#'   fit requires at least 15 data points, a quadratic requires at least 30 data
#'   points, etc. This restriction helps assure smoothing splines have adequate
#'   support and are stable.
#'   
#' @param halfConeMulti The multiplication factor to use for expanding fish 
#'   caught during half-cone operations. Default value is \code{2}. For example,
#'   if 15 fish were caught during half cone operation, the analysis considers
#'   this \code{halfConeMulti*15} fish.
#'   
#' @param sample.size.forkLength Number of fish with measured fork length 
#'   required to assign life stage. Default value is \code{100}.
#'   
#' @param sample.size.forkLengthAndWeight Number of fish with measured fork
#'   length \emph{and} weight required to use weight in the assignment of life
#'   stage.  Default value is \code{100}.
#'   
#' @param weight.prop.forkLength The minimum proportion of fish with weight to 
#'   fish with fork length in order to use weight in assignment of life stage. 
#'   Default value is \code{0.5}.
#'   
#' @param forkLength.mean.diff When the number of life stage groups (\code{2} or
#'   \code{3}) is not specified, the clustering algorithm starts with \code{3}
#'   groups and reduces groups if the mean difference in forklength between
#'   groups is less than this number. Units are same as fork lengths (mm).
#'   Default value is \code{10} mm.
#'   
#' @param time.zone Time zone to assign to all times.  Default value is 
#'   \code{"America/Los_Angeles"}, or Pacific time. It would likely never
#'   happen, but these routines cannot correctly account for times in two
#'   different zones. For example, one trap in one time zone, and another trap
#'   in a second zone. This might only happen if a future analysis combined
#'   sites over large areas (e.g., states). In the current routines, all times
#'   are assigned (or forced) to be in this time zone.
#'   
#' @param seed An integer specifying the seed to use in all functions utilizing 
#'   a random draw.  Currently set equal to \code{884}.
#'   
#' @param forkLengthCutPoints A dataframe specifying the groups into which fish 
#'   are to be partitioned via forklength.  The first column containing text 
#'   strings, which must be named \code{lifeStage}, contains new group names, 
#'   while the second column containing integers, which must be named 
#'   \code{cutPoints}, specifies the right end point of the interval of the 
#'   forklength range corresponding to the group name represented by that row in
#'   dataframe \code{forkLengthCutPoints}.  Intervals are assumed closed on the 
#'   right. The number of rows in \code{forkLengthCutPoints} should equal the 
#'   number of desired groups. Finally, the last row should have a right-end 
#'   point equal or in excess to the maximum possible forklength, e.g., 999. See
#'   Details.
#'   
#' @param passageRounder An integer specifying the place to which final passage 
#'   estimates and confidence intervals should be rounded.  For example, a value
#'   of \code{4} rounds results to the nearest 1,000.
#'   
#' @param eff.min.spline.samp.size An integer specifying the number of
#'   efficiency trials required to fit a spline efficiency model.  If fewer than
#'   this number of efficiency trials target a particular subsite, the Platform
#'   estimates constant efficiency for that subsite using the ROM method (see
#'   \code{F.efficiency.model}). If this number or more are available, 
#'   the Platform fits increasingly complex spline-based logistic regressions 
#'   and uses the one with lowest AIC.
#'   
#' @param unassd.sig.digit A positive integer specifiying the number of 
#'   significant digits to which unassigned fish should be estimated during the 
#'   plus-count algorithm.  Values other than zero allow for fractional
#'   representation of less common fish types (e.g., Winter or Smolt) during
#'   passage estimation.  For example, when
#'   \code{unassd.sig.digit=1}, plus-count fish are rounded to the nearest tenth. 
#'   Setting \code{unassd.sig.digit=0} rounds to the nearest whole fish.
#'   
#' @param bootstrap.CI.fx The function, entered as a character string, to use
#'   for identifying \eqn{(1 - \alpha)\%} bootstrapped confidence bounds for
#'   passage.  Current options include \code{"f.bias.acc.ci"} or \code{"f.ci"}.
#'   Default is \code{"f.ci"}.
#'
#' @param R The number of replicates to use for the bootstrap.  Default is
#'   \code{100}.
#'   
#' @details One additional global variable is defined. \code{table.names} is a 
#'   list containing the mapping of table names in Access to table names in R. 
#'   This was set up to facilitate painless table name changes in Access.  This 
#'   should not change unless tables or table names in Access change.  Because 
#'   these rarely change, we purposefully left this variable out of the 
#'   arguments to \code{GlobalVars}. To change table names, edit the code of
#'   \code{GlobalVars} and recompile the package.
#'   
#'   In dataframe \code{forkLengthCutPoints}, cut point intervals are closed on
#'   the right.  For example, if the second column of \code{forkLengthCutPoints}
#'   contains the integers 37, 59, and 105, then it is assumed that three
#'   forklength intervals are desired.  The first will cover (0,37], the second
#'   (37,59], and the third (59,105]. The code assumes maximum forklength to be 
#'   included is the maximum number given (i.e., 105 in the example). Cut points
#'   must be set so that the last (and greatest) number is larger than the
#'   maximum forklength to include.  It is acceptable to set the last cut point
#'   to a large number, e.g., Inf or 999, to ensure all forklengths are included
#'   in the new grouping scheme.
#'   
#'   The Access data base name in use is written to the R log file.
#'   
#'   Global variable \code{unassd.sig.digit} allows for more robust estimation of
#'   fish captured with lesser frequencies, e.g., endangered fish for which
#'   estimation of passage is deemed useful.  Often, underlying
#'   non-randomly-sampled fish are caught in insufficient quantities to allow
#'   for the estimation of at least one of these special fish types, when
#'   rounded to the nearest fish following the application of underlying
#'   frequency distributions resulting from randomly sampled fish.  Manipulation
#'   of the decimal place following the allocation of these unassigned fish
#'   leads to non-zero estimates for rare categories represented in the random
#'   sample.
#'   
#' @return No return value.
#'   
#' @author WEST Inc.
#'   
#'   
#' @examples 
#' \dontrun{
#' #   ---- Change data base file.
#' GlobalVars(db.file="../../Platform/data/StanislawCAMP.mdb" ) 
#' 
#' #   ---- Change where output goes.
#' GlobalVars(ouput.dir="~/Camp_output" ) 
#' 
#' }
GlobalVars <- function(
	db.file="..\\Data\\CAMP.mdb",
	output.dir="..\\outputs",
	sql.code.dir=file.path(find.package("campR"),"sql"),
	samplePeriodCutTime = "04:00:00", 
	max.ok.gap = 2,
	fishingGapMinutes = 10080,
	knotMesh = 15,
	halfConeMulti = 2,
	sample.size.forkLength = 100,
	sample.size.forkLengthAndWeight = 100,
	weight.prop.forkLength = 0.5,
	forkLength.mean.diff = 10,
	time.zone = "America/Los_Angeles", 
	Yes.code = 1,
	No.code = 2,
	seed = 884,
	forkLengthCutPoints = data.frame(lifeStage=c("FL1 leq 41mm","FL2 42-72mm","FL3 73-110mm","FL4 geq 111mm"),cutPoints=c(41,72,110,9999)),
	passageRounder = 4,
	eff.min.spline.samp.size = 10,
	unassd.sig.digit = 1,
	bootstrap.CI.fx = "f.ci",
	R = 100
	){

    # Utilize this construction to avoid NOTEs about assigning variables to the 
    # .GlobalEnv when running devtools::check().  
    pos <- 1
    envir <- as.environment(pos)
  
	  # DB file -------
		assign("db.file", db.file, pos=envir)
	  cat(paste("DB file:", db.file ,"\n"))

	  # Output dir ------	  
	  assign("output.dir", output.dir, pos=envir)
	  
		# SQL code dir ------	  
	  assign("sql.code.dir", sql.code.dir, pos=envir)
	  
	  # Sample Period Cut time ------
    assign("samplePeriodCutTime", samplePeriodCutTime, pos=envir)

	  # Max ok gap ------
    assign("max.ok.gap", max.ok.gap, pos=envir)

	  # Fishing Gap Minutes ------
    assign("fishingGapMinutes", fishingGapMinutes, pos=envir)

	  # Knot mesh -----
    assign("knotMesh", knotMesh, pos=envir)

	  # half cone multiplier -------
    assign("halfConeMulti", halfConeMulti, pos=envir)

	  # sample size to use fork length --------
    assign("sample.size.forkLength", sample.size.forkLength, pos=envir)

	  # sample size to use fork length and weight -------
    assign("sample.size.forkLengthAndWeight", sample.size.forkLengthAndWeight, pos=envir)

	  # weight proportion forklength ------
    assign("weight.prop.forkLength", weight.prop.forkLength, pos=envir)

	  # Mean fork length difference for clusters ------
    assign("forkLength.mean.diff", forkLength.mean.diff, pos=envir)

	  # time zone ------
    assign("time.zone", time.zone, pos=envir)

	  # Yes code (must match data lu.YesNo table in data base) ------
    assign("Yes.code", Yes.code, pos=envir)

	  # No code (must match data lu.YesNo table in data base) ------
	  assign("No.code", No.code, pos=envir)
	  
	  # Set the seed ------
	  set.seed(884)
	  assign("seed", seed, pos=envir)
	  
	  # Set dataframe forkLengthCutPoints
	  assign("forkLengthCutPoints",forkLengthCutPoints,pos=envir)
	  
	  # Set the number of significant digits by which we want to round final passage. 
	  assign("passageRounder", passageRounder, pos=envir)
	  
	  #	Set number of efficiency trials for spline model
	  assign("eff.min.spline.samp.size", 	eff.min.spline.samp.size, pos=envir)
	  
	  # Set number of significant digits for unassigned fish during plus counting.
	  assign("unassd.sig.digit", unassd.sig.digit, pos=envir)
	  
    # Table names is a special global variable that we intentionally make harder ----
    # to change.  I.e., must change code here and re-compile. 
    table.names <- c(trap.visit="TrapVisit",
    								 sites = "Site",
    								 project = "ProjectDescription",
    								 subsites = "SubSite",
    								 catch = "CatchRaw",
    								 release="Release",
    								 mark.applied="MarkApplied",
    								 catch = "CatchRaw",
    								 mark.found="MarkExisting",
    								 trap.visit="TrapVisit",
    								 species.codes="luTaxon",
    								 run.codes ="luRun",
    								 rel.x.target="ReleaseXTargetSite",
    								 LAD = "LengthAtDate",
    								 yes.no.codes="luNoYes",
    								 CAMP.life.stages="luLifeStageCAMP",
    								 life.stages="luLifeStage",
    								 fish.origin="luFishOrigin" )
    assign("table.names", table.names, pos=envir)
    
    # Define the boostrap function.  
    assign("bootstrap.CI.fx", bootstrap.CI.fx, pos=envir)
    
    # Define the number of bootstrap replicates.  
    assign("R", R, pos=envir)
    
    
    #  *************** NOTE: To do - read the data base and figure out which water shed is being analyzed.  Then,
    #  *************** Set the efficiency model to use.
    #
    #   Can specify the capture efficiency model here.

}

