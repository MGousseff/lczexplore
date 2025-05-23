% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matConfLCZ.R
\name{matConfLCZ}
\alias{matConfLCZ}
\title{Produces a confusion matrix between two sets of lcz on the same area
(geoms do not need to be the same)}
\usage{
matConfLCZ(
  sf1,
  column1,
  sf2,
  column2,
  typeLevels = .lczenv$typeLevelsDefault,
  plotNow = FALSE,
  wf1 = "Reference",
  wf2 = "Alternative",
  sfInt = NULL,
  drop = FALSE,
  ...
)
}
\arguments{
\item{sf1}{is the sf dataset containing the first lcz classification}

\item{column1}{is the column of the first data set containing the lcz to be compared}

\item{sf2}{is the sf dataset containing the second lcz classification}

\item{column2}{is the column of the second data set containing the lcz to be compared}

\item{typeLevels}{by default the levels of lcz incoded from 1 to 10 and 101 to 107.}

\item{plotNow}{shows the graphical representation of the confusion matrix}

\item{wf1}{the name of the workflow used to produce data from sf1}

\item{wf2}{the name of the workflow used to produce data from sf2}

\item{sfInt}{allows to produce the confusion matrix directly on the intersected sf object if it was produced before}

\item{drop}{if TRUE the empty LCZ types are dropped
When comparing othe variable, like grouped LCZ, the grouped levels have to be specified.}

\item{...}{a set of unspecified arguments, for instance when the produceAnalysis function calls other functions}
}
\value{
returns an object called matConfOut which contains
matConfLong, a confusion matrix in a longer form, which can be written in a file by the compareLCZ function
and is used by the geom_tile function of the ggplot2 package.
matConfPlot is a ggplot2 object showing the confusion matrix. If plot=TRUE, it is also directly plotted
marginAreas contains the sums of each LCZ area
percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
}
\description{
Produces a confusion matrix between two sets of lcz on the same area
(geoms do not need to be the same)
}
\details{
Most of the time this function will not be called directly
by the user but by the compareLCZ function
}
\examples{
matConfRedonBDTOSM<-matConfLCZ(sf1=redonBDT,column1='LCZ_PRIMARY',
sf2=redonOSM,column2='LCZ_PRIMARY',plot=TRUE)
}
