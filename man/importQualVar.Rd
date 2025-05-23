% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importQualVar.R
\name{importQualVar}
\alias{importQualVar}
\title{Imports a qualitative variable associated with  polygons
from a geographical dataset (tested : geojson, shp, more to come)}
\usage{
importQualVar(
  dirPath,
  file = "rsu_utrf_area.geojson",
  output = "sfFile",
  column = "TYPO_MAJ",
  geomID = "ID_RSU",
  confid = "UNIQUENESS_VALUE",
  typeLevels = "",
  drop = T,
  verbose = TRUE
)
}
\arguments{
\item{dirPath}{is the path of the directory of the file}

\item{file}{is the name of the file from which the variable is imported}

\item{output}{: if sfFile, the function returns an sfFile with the qualitative variable,
if bBox, returns a bounding box one can use to crop a raster file or to intersect another sf file}

\item{column}{indicates the name of the column containing the qualitative variable is imported}

\item{geomID}{is the name of the column containing the ID of each geom to load.
If an empty string, no column is loaded.}

\item{confid}{is the name of the column containing a confidence indicator,
for instance the uniqueness of the LCZ level of each geom, in order to filter geoms,}

\item{typeLevels}{the levels of the imported qualitative variable}

\item{drop}{: the default is TRUE, which means all the column are dropped excepted those specified in previous parameters}

\item{verbose}{allows to hide some messages when set to false.}
}
\value{
returns an sf object containing at least the geoms and the qualitative variable,
and if specified, columns for the IDs of the geoms and the confidence value of
thevalues of the variable.
}
\description{
Imports a qualitative variable associated with  polygons
from a geographical dataset (tested : geojson, shp, more to come)
}
\examples{
utrfRedonBDT<-importQualVar(dirPath=paste0(
system.file("extdata", package = "lczexplore"), "/bdtopo_2_2/Redon"),
file="rsu_utrf_area.geojson", column="TYPO_MAJ")
showLCZ(sf=utrfRedonBDT, column="TYPO_MAJ",repr="alter")
utrfRedonOSM<-importQualVar(dirPath=
paste0(system.file("extdata", package = "lczexplore"),"/osm/2022/Redon"),
file="rsu_utrf_area.geojson", column="TYPO_MAJ",geomID="ID_RSU",confid="UNIQUENESS_VALUE")
# One can now compare these 2 classifications and store the result in an object...
utrfComparison<-compareLCZ(
 sf1=utrfRedonBDT, column1="TYPO_MAJ", wf1=" UTRF BDT",
 sf2=utrfRedonOSM, column2="TYPO_MAJ", wf2 = " UTRF OSM",
location = " Redon",exwrite=FALSE,repr="alter")
 # ... then plot the confusion matrix of these two classifications  
print(utrfComparison$matConfPlot)


}
