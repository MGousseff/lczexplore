% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barplotLCZaLocation.R
\name{barplotLCZaLocation}
\alias{barplotLCZaLocation}
\title{in a given directory, if several LCZ files are present, plots the repartition
of LCZ regarding of their source (workflow)
NOTE: to represent the map of LCZ for a given fil, use \code{showLCZ} function instead}
\usage{
barplotLCZaLocation(
  dirPath,
  location,
  workflowNames = c("osm", "bdt", "iau", "wudapt"),
  refWf = NULL,
  refLCZ = NA,
  residualLCZvalue = NA,
  plotNow = FALSE,
  plotSave = TRUE
)
}
\arguments{
\item{dirPath}{is the path where the datasets are stored}

\item{location}{is the name of the locations for the plot is produced}

\item{workflowNames}{is a vector of prefixes. The LCZ files must be named workflow_rsu.fgb
where workflow is on of the values in workflowNames vector}

\item{refWf}{is a reference workflow name, passed to the function addMissingRSUs when needed}

\item{refLCZ}{is a reference LCZtype, passed to the function addMissingRSUs when needed}

\item{residualLCZvalue}{a LCZ default type, passed to the function addMissingRSUs when needed}

\item{plotNow}{If TRUE, the boxplot of the repartition will be printed}

\item{plotSave}{If TRUE, the plot will be saved in the directory pointed by dirPath}
}
\value{
Cramer's V between pairs of levels, in a matrix (cramerMatrix) or long form (cramerLong),
and a dataframe with the nbOutAssociation most significant association
}
\description{
in a given directory, if several LCZ files are present, plots the repartition
of LCZ regarding of their source (workflow)
NOTE: to represent the map of LCZ for a given fil, use \code{showLCZ} function instead
}
\examples{
barplotLCZaLocation(
dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Goussainville"),
refWf = NULL, refLCZ = NA, residualLCZvalue = "Unclassified",
location = "Goussainville", plotNow = TRUE)
}
