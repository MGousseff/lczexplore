% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intersectAlocation.R
\name{intersectAlocation}
\alias{intersectAlocation}
\title{Intersects several sf files for a given location identified by a given directory path and name}
\usage{
intersectAlocation(
  dirPath,
  workflowNames = c("osm", "bdt", "iau", "wudapt"),
  location,
  addMissingRSUs = TRUE,
  missingGeomsWf = "iau",
  refWf = NULL,
  refLCZ = "Unclassified",
  residualLCZvalue = "Unclassified",
  column = "lcz_primary"
)
}
\arguments{
\item{dirPath}{is the directory where the original data are}

\item{workflowNames}{are the names of the workflows : they are used to identify the files}

\item{location}{is the name of the location (string)}

\item{addMissingRSUs}{if TRUE calls the addMissingRSUs function to avoid areas with no geometries}

\item{missingGeomsWf}{is passed to addMissingRSU function}

\item{refWf}{is passed to addMissingRSU function}

\item{refLCZ}{is passed to addMissingRSU function}

\item{residualLCZvalue}{is passed to addMissingRSU function}

\item{column}{the column name of the LCZ classification}
}
\value{
an sf file with values of LCZ from all the input
are assigned to geometries resulting from intersection of all input geometries
}
\description{
Intersects several sf files for a given location identified by a given directory path and name
}
\details{
This function is not generic, it expects the data files to be named wf_rsu_lcz; wf varying among
the values of workflownames, and the LCZ columns are expected to be lcz_primary (but lower and upper cases are accepted)
}
\examples{
# deprecated : use concatIntersectedLocations with the proper arguments
}
