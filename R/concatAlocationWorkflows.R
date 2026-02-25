#' Take sf files with an lcz_primary column, and concatenates them in a single sf object, 
#' adding a column for location and workflow names 
#' @param sfList the list of LCZ sf objects
#' @param location the name of the location at which all LCZ are created
#' @param refCrs a number telling which sf of the sfList will be the reference in termes of Coordinate Reference System
#' @importFrom sf st_transform st_crs st_drop_geometry
#' @return returns graphics of comparison and an object called matConfOut which contains :
#' matConfLong, a confusion matrix in a longer form, 
#' matConfPlot is a ggplot2 object showing the confusion matrix.
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' pseudoK is a heuristic estimate of a Cohen's kappa coefficient of agreement between classifications
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' @export
#' @examples
#' sfList<-loadMultipleSfs(dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs/Goussainville"),
#' workflowNames = c("osm","bdt","iau","wudapt"), inLocation = "Goussainville"  )
#' zoneSf <- sf::read_sf(
#' paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Goussainville/zone.fgb")
#' )
#' GoussainvilleAllWfs <-  concatAlocationWorkflows(
#' sfList = sfList, location = "Goussainville")
concatAlocationWorkflows<-function(sfList, location=NA, refCrs = 1){

  if (is.na(location) | is.null(location)){
    location<- st_drop_geometry(sfList[[1]]["location"][1])
  }

  refCrs<-st_crs(sfList[[refCrs]]$geometry)
  sfList<-lapply(sfList, st_transform, crs = refCrs)
concatSf<-do.call(rbind, sfList)
# return(concatSf)
}