#' In a given directory (or a list of directories) the function looks for LCZ datafiles,
#' intersects them and return a dataset with intersected geometries and LCZ values for each workflow
#' @param dirList the list of directories for which the different LCZ files will be intersected
#' @param workflowNames sets the names of workflows and define the name of the files which will be loaded and intersected
#' @param locations : for each diretory from dirList, a location name must be fed to the function
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf
#' @return returns an sf object with all intersections
#' @export
#' @examples
#' dirList<-list.dirs(paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs"), recursive = FALSE)
#' allLocIntersected<-concatIntersectedLocations(
#' dirList = dirList, locations = c("Redon", "Arville"), columns = "lcz_primary")
concatIntersectedLocations<-function(dirList, locations, workflowNames = c("osm","bdt","wudapt"),
                                     columns = "lcz_primary"){
  sfList<-list()
  if (length(columns == 1)){columns <- rep( columns, length(workflowNames))}
  for (i in seq_along(dirList)){
    sfList[[i]]<-loadMultipleSfs(
      dirPath = dirList[i],
      workflowNames = c("osm","bdt","wudapt"),
      inLocation = locations[i], column = columns[i] )
  }
  print(names(sfList[[1]][[1]]))
  concatIntersectedSf<-createIntersect(
  sfList = sfList, columns = columns, refCrs=NULL, workflowNames=workflowNames,
  minZeroArea=0.0001
  )

  # concatIntersectedSf$location<-factor(
  #   concatIntersectedSf$location, levels = .lczenv$typeLevelsDefault)
  return(concatIntersectedSf)
}