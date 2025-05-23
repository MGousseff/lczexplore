% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflowAgreeAreas.R
\name{workflowAgreeAreas}
\alias{workflowAgreeAreas}
\title{From the output of compare multiple, cmputes which workflows agree the most regarding the area of agreement}
\usage{
workflowAgreeAreas(sfMultiCompLong)
}
\arguments{
\item{sfMultiCompLong}{the sfLong output of compareMultipleLCZ function}
}
\value{
the pairwise agreement between workflows, sorted by decreasing agreeing areas
}
\description{
From the output of compare multiple, cmputes which workflows agree the most regarding the area of agreement
}
\examples{
sfList<-loadMultipleSfs(dirPath = paste0(
system.file("extdata", package = "lczexplore"),
"/multipleWfs/Goussainville"),
workflowNames = c("osm","bdt","iau","wudapt"), location = "Goussainville")
GoussainvilleIntersect <- createIntersect(
 sfList = sfList, columns = rep("lcz_primary", 4),  
 sfWf = c("osm","bdt","iau","wudapt"))
GoussainvilleMultipleComparison<-compareMultipleLCZ(
 sfInt = GoussainvilleIntersect,
 LCZcolumns = c("osm","bdt","iau","wudapt"),
 trimPerc = 0.5)
GoussainvilleWorkflowAgreement<-workflowAgreeAreas(GoussainvilleMultipleComparison$sfIntLong)
}
