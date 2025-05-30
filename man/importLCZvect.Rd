% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importLCZvect.R
\name{importLCZvect}
\alias{importLCZvect}
\title{Imports Local Climate Zone classifications from a standard geographical file (tested : geojson, shp, more to come)}
\usage{
importLCZvect(
  dirPath,
  file = "rsu_lcz.geojson",
  output = "sfFile",
  column = "LCZ_PRIMARY",
  geomID = "",
  confid = "",
  typeLevels = .lczenv$typeLevelsDefault,
  drop = T,
  verbose = FALSE,
  sfIn = NULL,
  naAsUnclassified = TRUE
)
}
\arguments{
\item{dirPath}{is the path of the directory of the file}

\item{file}{is the name of the file from which the LCZ are imported}

\item{output}{: if sfFile, the function returns an sfFile with the specified columns,
if bBox, returns a bounding box one can use to crop a raster file or to intersect another sf file}

\item{column}{indicates the name of the column containing LCZ values.
LCZ values are expected to be of a standard LCZ format (1 to 17, or 1 to 10 and 101 to 107 or 1 to G),
else, use the importQualVar function}

\item{geomID}{is the name of the column containing the ID of each geom to load.
If an empty string, no column is loaded.}

\item{confid}{is the name of the column containing a confidence indicator to filter geoms,
for instance the uniqueness of the LCZ level of each geom}

\item{typeLevels}{the levels of the imported LCZ classification}

\item{drop}{: the default is TRUE, which means all the column are
dropped excepted those specified in previous parameters}

\item{verbose}{if TRUE show the discrepancies between specified levels of LCZ and
levels actually present in column}

\item{sfIn}{allows you to import from an existing object of your session, not from an external file}

\item{naAsUnclassified}{if TRUE allows NAs in the input file and assigns them the value "Unclassified"}
}
\value{
returns an sf object containing at least the geoms and the LCZ values,
and if specified, columns for the IDs of the geoms and the confidence value of the LCZ levels.
}
\description{
Imports Local Climate Zone classifications from a standard geographical file (tested : geojson, shp, more to come)
}
\examples{
redonBDTex<-importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"),
"/bdtopo_2_2/Redon"), file="rsu_lcz.geojson", column="LCZ_PRIMARY",
geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE")
showLCZ(redonBDTex)
}
