% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_passage.r
\name{F.plot.passage}
\alias{F.plot.passage}
\title{F.plot.passage}
\usage{
F.plot.passage(df, out.file = "passage.png")
}
\arguments{
\item{df}{A data frame containing passage estimates calculated via function 
\code{F.est.passage} per one of day, week, month, or year.}

\item{out.file}{The name of the file prefix under which output is to be
saved.  Set to \code{NA} to plot to the Plot window.}
}
\value{
Output includes a \code{png} graphical display of total estimated 
  passage by specified temporal unit.  Temporal units, or bars in the graph, 
  display both the number of observed fish and imputed fish.  The total 
  number of estimated fish over the entire temporal range is also reported.
}
\description{
Plot a bar graph of the passage estimates in the temporal units
  specified by the user in the original passage function call.
}
\details{
It is assumed that data frame \code{df} contains at the least 
  variables \code{<temporal time frame>}, \code{passage}, \code{date}, and 
  \code{pct.imputed.catch}. Variable \code{<temporal time frame>} is either 
  one of "\code{year}," "\code{month}," "\code{week}," or "\code{day}." Weeks
  are reported in a modified Julian fashion as recorded in the "\code{Dates}"
  table in any associated CAMP Access database.  Passage estimates for each
  reported time period are rounded to the nearest whole fish.  Variable 
  \code{date} is a POSIX date, with data formatted as \code{\%Y-\%m-\%d}, 
  i.e., the ISO 8601 date format, with a 4-digit year.  Variable
  \code{pct.imputed.catch} contains the estimated imputed proportion, and so
  takes on values between zero and one, inclusive.
  
  Other variables included as part of dataframe \code{df} are not utilized 
  \emph{per se} in the function, but do pass through.
}
\examples{
\dontrun{
#   ---- Plot passage estimates per temporal time unit to
#   ---- the plot window.
F.plot.passage( df, out.file=NA )
}
}
\seealso{
\code{F.est.passage}
}
\author{
WEST Inc.
}
