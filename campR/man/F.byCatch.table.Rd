% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by_catch_table.r
\name{F.byCatch.table}
\alias{F.byCatch.table}
\title{F.byCatch.table}
\usage{
F.byCatch.table(site, min.date, max.date, output.file)
}
\arguments{
\item{site}{The identification number of the site for which estimates are 
required.}

\item{min.date}{The start date for data to include. This is a text string in 
the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.}

\item{max.date}{The end date for data to include.  Same format as 
\code{min.date}.}

\item{output.file}{The name of the file prefix under which output is to be 
saved.  Set to NA to plot to the Plot window.}
}
\value{
A \code{csv} file containing all non-Chinook catch, along with a
  \code{png} pie chart detailing underlying counts and proportions of caught
  species.
}
\description{
List all non-Chinook catch.
}
\details{
Query sequence Build Report Criteria builds the appropriate time
  frame of interest while query series By Catch intemize the non-Chinook
  catch.  See section Structured Query Language (SQL) Queries in
  \code{F.run.sqlFile} for details.
  
  Function \code{F.byCatch.table} calls helper function \code{F.getByCatch} 
  to obtain the non-Chinook catch.
}
\examples{
\dontrun{
#   ---- Find all non-Chinook catch resulting from fishing the American
#   ---- River, from Jan. 16, 2013 through June 8, 2103.  
F.byCatch.table(57000,"2013-01-16","2013-06-08",output.file="American River")
}
}
\seealso{
\code{F.run.sqlFile}, \code{F.getByCatch}
}
\author{
WEST Inc.
}
