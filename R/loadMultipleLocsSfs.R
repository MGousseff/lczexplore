#' In a given directory the function looks different locations as subdirectories and LCZ datafiles
#' in each subdirectory, and load them in a list
#' In each directory, files must have names built as follow :
#' <wf>_lcz.<fileExtension>, where wf are the values specified in workflowNames parameter and
#' fileExtension is a fil extension known by sf drivers, like fgb, geojson...
#' @param dirPath is the place where the files are
#' @param workflowNames sets the names of workflows
#' @param inLocation is the name of the location at which all LCZ are created
#' @param fileExtension is the extensions of the files to load (.fgb is the recommended format)
#' @param column is the name of the column containing the LCZ types, must be the same in all files.
#' @importFrom forcats fct_recode
#' @importFrom dplyr mutate
#' @import sf units RColorBrewer utils grDevices
#' @return returns graphics of comparison and an object called matConfOut which contains :
#' matConfLong, a confusion matrix in a longer form,
#' matConfPlot is a ggplot2 object showing the confusion matrix.
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' pseudoK is a heuristic estimate of a Cohen's kappa coefficient of agreement between classifications
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' @export
#' @examples
#' sfList<-loadMultipleSfs(dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
#' workflowNames = c("osm","bdt","iau","wudapt"), inLocation = c("Arville", "Blaru"))
loadMultipleLocsSfs<-function(
  dirPath = paste0(
    system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
  workflowNames = c("osm","bdt","iau","wudapt"), inLocations = c("Arville", "Blaru")
){
  dirList<-list.dirs(dirPath, recursive = FALSE)
  print(dirList)
  print(inLocations)
  if (length(inLocations)<length(dirList) | prod(!is.na(inLocations))==FALSE){
    inLocations <- gsub(pattern = "(.*)(/)(.+)", replacement ="\\3", x = dirList)
    message(
      paste0("Some location names are missing, the following directory names will replace location names: "
        , paste0(inLocations, collapse = ", ")))

  }

  allLocAllWfs<-vector("list", length = length(inLocations))
  names(allLocAllWfs)<-inLocations
  for(loc_i in seq_along(dirList)){
  allLocAllWfs[[inLocations[loc_i]]]<-
  # sfTemp<-
    loadMultipleSfs(
    dirPath = dirList[loc_i],
    inLocation = inLocations[loc_i],
    workflowNames = workflowNames
  )

  }
  return(allLocAllWfs)
}

# sfListAll<-loadMultipleLocsSfs(dirPath = paste0(
# system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
# workflowNames = c("osm","bdt","iau","wudapt"), inLocations = c(NA, "Blaru"))