% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_passage.r
\name{F.run.passage}
\alias{F.run.passage}
\title{F.run.passage - Estimate passage of all fish.}
\usage{
F.run.passage(
  site,
  taxon,
  min.date,
  max.date,
  by,
  output.file,
  ci = TRUE,
  enhmodel = TRUE
)
}
\arguments{
\item{site}{The identification number of the site for which estimates are 
required.}

\item{taxon}{The species identifier indicating the type of fish of interest. 
This is always \code{161980}; i.e., Chinook Salmon.}

\item{min.date}{The start date for data to include. This is a text string in 
the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.}

\item{max.date}{The end date for data to include.  Same format as 
\code{min.date}.}

\item{by}{A text string indicating the temporal unit over which daily 
estimated catch is to be summarized.  Can be one of \code{day}, 
\code{week}, \code{month}, \code{year}.}

\item{output.file}{A text string indicating a prefix to append to all output.}

\item{ci}{A logical indicating if 95\% bootstrapped confidence intervals 
should be estimated along with passage estimates..  Default is \code{TRUE}.}

\item{enhmodel}{A logical indicating if enhanced efficiency models should 
be used to estimate trap efficiencies.  Default is \code{TRUE}.}

\item{run}{The text seasonal identifier.  This is a one of \code{"Spring"},
\code{"Fall"}, \code{"Late Fall"}, or \code{"Winter"}.}
}
\value{
A data frame containing the final passage estimates.
}
\description{
Estimate passage by run over all life stages within a
  date range.
}
\details{
Function \code{F.run.passage} is the main workhorse function for 
  estimating passage with respect to each of run and life stage.  As such, it
  calls other functions, some of which query raw data from the Access database.
  
  Generally, queries against a database have two parts.  The first
  involves a query for efficiency trial data, generally called "release" 
  data.  These queries are performed by function \code{F.get.release.data}. 
  The second part involves queries of raw catch, which are 
  performed by function \code{F.get.catch.data}.
  
  Once catch data are obtained, fish are partitioned as to whether or not 
  they were assigned and caught during a half-cone operation.  Function 
  \code{F.est.passage} wraps the functions that conduct the actual passage 
  estimation, which involves statistical fits of each of catch and efficiency
  over time.
  
  All calls to function \code{F.run.passage} result in daily passage 
  estimates (in the "baseTable.csv" file).  Courser temporal estimates can 
  be obtained by specifying the value of \code{by}.  
  Regardless of \code{by}, estimates are summarized by year.  
  Function runs with \code{by} specified 
  as \code{year} output only one set of annual estimates.
  
  The difference between the specified \code{max.date} and \code{min.date}
  must be less than or equal to 366 days, as calculated via function
  \code{difftime}.
  
  Selection of \code{week} for input variable \code{by} results in weeks 
  displayed as customized Julian weeks, where weeks number 1-53.  The 
  specific mapping of days to weeks can be found within the "\code{Dates}" 
  table of any associated Access database.
}
\section{Files Created}{

This routine produces the following files in the 'outputs' directory of the 
RST package: 
  \itemize{
    \item File \bold{<id>_<site>_<date>_<run>_passage_table.csv}: A \code{csv} 
    table of passage estimates over the specified date range. This file contains
    auxiliary information on mean fork length, sd of fork length, fishing length, 
    the efficiency model used, and number of traps operating. If called for, 
    this file contains daily confidence intervals surrounding estimated passage.
      
    \item File \bold{<id>_<site>_<date>_<run>_masterCatch.csv}: A \code{csv} 
    of daily assigned catch, unassigned catch, imputed catch, and total 
    catch.  All catch numbers in this file are un-inflated by efficiency. 
    
    \item File \bold{<id>_<site>_<date>_<run>_baseTable.csv}: A \code{csv} 
    containing raw catch, imputed catch, total estimated catch, estimated 
    efficiency and passage summarized  by the temporal unit specified via 
    \code{by}.  Usually, this file contains one row per batch day.
    
    \item File \bold{<id>_<site>_<date>_<run>_catch.png}: A \code{png} of catch by time, for all traps. This graph shows raw
    (un-inflated) catch, which days have some imputed catch, and the smoothed
    catch model used for imputation.
    \item File \bold{<id>_<site>_<date>_<run>_eff.png}: A \code{png} of daily efficiency estimates.  
    \item File \bold{<id>_<site>_<date>_<run>_passage.png}: A \code{png} containing a bar chart of passage estimates 
    over the time period for time units specified in \code{by}. This graph 
    contains inflated passage estimates (usually by day), the proportion 
    of each day's estimate that has been imputed, and total passage (sum 
    of all bar heights).
  }
}

\examples{
 
\dontrun{
#   ---- Estimate passage on the American for the Fall run.
site <- 6000
taxon <- 161980
min.date <- "2010-12-07"
max.date <- "2011-06-02"
by <- "day"
output.file <- "Feather"
ci <- TRUE
}
}
\seealso{
\code{F.get.release.data}, \code{F.get.catch.data}
}
