#' In a given directory (or a list of directories) the function looks for LCZ datafiles
#' and load them in a list. In each directory, files must have names built as follow :
#' <wf>_lcz.<fileExtension>, where wf are the values specified in workflowNames parameter and
#' fileExtension is a fil extension known by sf drivers, like fgb, geojson...
#' @param dirPath is the place where the files are
#' @param workflowNames sets the names of workflows
#' @param inLocation is the name of the location at which all LCZ are created
#' @param fileExtension is the extensions of the files to load (.fgb is the recommended format)
#' @param columns the name (string) of the column containing LCZ types.
#' If the different workflows do no use the same column names, a vector of names is passed
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
#' system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
#' workflowNames = c("osm","bdt","wudapt"), inLocation = "Arville", columns = "lcz_primary")
loadMultipleSfs <- function(
  dirPath, workflowNames = c("osm", "bdt", "wudapt"),
  inLocation = NA,
  fileExtension = ".fgb",
  columns = "lcz_primary") {
  typeLevels <- c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8",
                  "9" = "9", "10" = "10",
                  "101" = "101", "102" = "102", "103" = "103", "104" = "104", "105" = "105", "106" = "106", "107" = "107",
                  "101" = "11", "102" = "12", "103" = "13", "104" = "14", "105" = "15", "106" = "16", "107" = "17",
                  "101" = "A", "102" = "B", "103" = "C", "104" = "D", "105" = "E", "106" = "F", "107" = "G")
  if (is.null(inLocation) | prod(!is.na(inLocation)) == 0) {
    print("location")
    print(inLocation)
    inLocation <- gsub(pattern = "(.*)(/)(.+)(/$)", replacement = "\\3", x = dirPath)
    print(inLocation)
  }
  if (length(columns) == 1) { columns <- rep(columns, length(workflowNames)) }
  dirPath <- checkDirSlash(dirPath)
  print(dirPath)
  sfList <- list()
  for (i in seq_along(workflowNames)) {
    inName <- paste0(dirPath, workflowNames[i], "_lcz", fileExtension)
    inSf <- read_sf(inName)
    inSf$lcz_primary <- inSf[[columns[i]]]
    names(inSf) <- tolower(names(inSf))

    inSf <- select(inSf, lcz_primary) %>% mutate(
      lcz_primary = factor(lcz_primary, levels = typeLevels))
    inSf <- dplyr::mutate(inSf, wf = workflowNames[i], location = inLocation, .before = geometry)
    inSf[[columns[i]]] <- forcats::fct_recode(inSf[[columns[i]]], !!!typeLevels)
    sfList[[workflowNames[i]]] <- inSf
  }
  return(sfList)
}