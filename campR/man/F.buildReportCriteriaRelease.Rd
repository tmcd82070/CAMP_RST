% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_Report_Criteria_Release.r
\name{F.buildReportCriteriaRelease}
\alias{F.buildReportCriteriaRelease}
\title{F.buildReportCriteriaRelease}
\usage{
F.buildReportCriteriaRelease(site, min.date, max.date)
}
\arguments{
\item{site}{The identification number of the site for which estimates are 
required.}

\item{min.date}{The start date for data to include. This is a text string in 
the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.}

\item{max.date}{The end date for data to include.  Same format as 
\code{min.date}.}
}
\value{
Within R, the total integer count of efficiency trials.  Within 
  Access, table \code{TempReportCriteria_Release} is created, itemizing the
  releases.
}
\description{
Update Access database table \code{TempReport_Criteria_Release} 
  to contain release \code{trapVisitID}s for the site and inclusive calendar 
  dates specified by the user.
}
\details{
Unique efficiency trials are identified by unique \code{releaseIDs} 
  falling within the specified \code{min.date} and \code{max.date}.  See
  section Structured Query Language (SQL) Queries in function
  \code{F.run.sqlFile} for more details on query series.
  
  Note that the difference between this and the \code{buildReportCriteria} 
  query series is this one's focus on releases and subsequent recaptures
  related to efficiency trials.
}
\examples{
\dontrun{
#   ---- American River at Watt Avenue, 2013 Season
site <- 57000                 
min.date <- "2013-01-16"
max.date <- "2013-06-08" 

#   ---- Obtain inclusive releases.    
nreleases <- F.buildReportCriteriaRelease( site, min.date, max.date )
}

}
