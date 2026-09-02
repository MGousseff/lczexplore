#' Takes an sf object with several workflow values,
#' and creates all pairwise confusion matrices, then conatenates them in long form,
#' usable to plot chord diagrams
#'
#' @param intersectSfWide is an sf object, containing the different lcz classification on
#' already intersected geometries in wide format
#' with a column wf for the workflow names, a column lcz_primary for the LCZ types.
#' @param columns a vector containing the names of the LCZ type columns
#' @param wfNamesIn the name of the input compared workflows. They will be the output columns for the LCZ types
#' @param typeLevelsDefaultIn is a named vector of strings containing the LCZ levels. By default inherited from lczexplore
#' @importFrom dplyr mutate select filter
#' @return a dataframe with columns orig, dest and weightedFlux, weighted flux the percentage of area from a given
#' LCZ type of a given workflow (orig) to another LCZ type of another workflow (dest).
#' @examples
#' twoLocsDir<-paste0(
#'  system.file("extdata", package = "lczexplore"),"/multipleWfs")
#' twoLocsSfList<-loadMultipleLocsSfs(dirPath = twoLocsDir, workflowNames = c("osm","bdt","wudapt"),
#'                                   inLocation = c("Arville", "Redon"))
#'
#' twoLocsSfIntersected <- createIntersect(sfList = twoLocsSfList, columns = rep("lcz_primary", 4),
#'                                        refCrs=NULL, workflowNames=c("osm", "bdt", "wudapt"), minZeroArea=0.001)
#'
#' twoLocsWeightedFlux<-createWeightedFlux(twoLocsSfIntersected, wfNamesIn = c("osm","bdt","wudapt"))
#'
#' @export
createWeightedFlux <- function(intersectSfWide, columns = NULL, wfNamesIn = NULL,
                               typeLevelsDefaultIn = .lczenv$typeLevelsDefault) {
  if (nrow(intersectSfWide) > 100) { message("This function computes how any LCZ type from any workflow
  breaks into the LCZ types of all other workflows: it can take a while") }
print(columns)
  print(wfNamesIn)
      if (
        (is.null(wfNamesIn) | prod(!is.na(wfNamesIn))) &
          (!is.null(columns) & prod(!is.na(columns)))
      ) {wfNamesIn<-columns}

      if (
        (is.null(columns) | prod(!is.na(columns))) &
          (!is.null(wfNamesIn) & !prod(is.na(wfNamesIn)))
      ) {columns <- wfNamesIn}

  print(columns)

  for (i in 1:(length(columns) - 1)) {
    for (j in (i + 1):length(columns)) {
      sf1 <- intersectSfWide[,i]
      sf2 <- intersectSfWide[,j]
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      assign(compareName,
             matConfLCZ(
               sf1 = sf1, column1 = columns[i],
               sf2 = sf2, column2 = columns[j],
               typeLevels = unique(names(typeLevelsDefaultIn)),
               plotNow = FALSE, wf1 = wfNamesIn[i], wf2 = wfNamesIn[j])
      )
    }
  }


  for (i in 1:(length(wfNamesIn) - 1)) {
    for (j in (i + 1):length(wfNamesIn)) {
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      compareNameToBind <- paste0(wfNamesIn[i], "_", wfNamesIn[j], "_to_bind")
      assign(compareNameToBind,
             get(compareName)$matConf %>%
               dplyr::mutate(wf_pair = paste0(
                 wfNamesIn[i], "_", .data[[columns[i]]], "_",
                 wfNamesIn[j], "_", .data[[columns[j]]])) %>%
               dplyr::select(.data$wf_pair, .data$agreePercArea) %>%
               mutate(percArea = rep(
                 get(compareName)$areas$percArea1,
                 each = length(get(compareName)$areas$percArea2)))
      )
      print(get(compareNameToBind)$wf_pair)
    }
  }

  ############################################
  ##
  ## symetrical
  ##
  ############################################

  for (i in (length(columns):2)) {
    for (j in 1:(i - 1)) {
      sf1 <- intersectSfWide[,i]
      sf2 <- intersectSfWide[,j]
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      assign(compareName,
             matConfLCZ(
               sf1 = sf1, column1 = columns[i],
               sf2 = sf2, column2 = columns[j],
               typeLevels = unique(names(typeLevelsDefaultIn)),
               plotNow = FALSE, wf1 = wfNamesIn[i], wf2 = wfNamesIn[j]))
    }
  }

  for (i in (length(wfNamesIn):2)) {
    for (j in 1:(i - 1)) {
      compareNameToBind <- paste0(wfNamesIn[i], "_", wfNamesIn[j], "_to_bind")
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      assign(compareNameToBind,
             get(compareName)$matConf %>%
               dplyr::mutate(wf_pair = paste0(
                 wfNamesIn[i], "_", .data[[columns[i]]], "_", wfNamesIn[j], "_", .data[[columns[j]]])) %>%
               dplyr::select(.data$wf_pair, .data$agreePercArea) %>%
               mutate(percArea = rep(get(compareName)$areas$percArea1, each = length(get(compareName)$areas$percArea2)))
      )
    }
  }

  matConfNames <- grep("_to_bind", ls(), value = TRUE)

  if (exists("allMAtConfLong")) { rm(allMatConfLong) }

  allMatConfLong <- do.call(rbind, mget(matConfNames))

  allMatConfLong$orig <- gsub(
    x = allMatConfLong$wf_pair,
    pattern = "(.*)(_)(.*)(_)(.*)(_)(.*)",
    replacement = "\\1\\2\\3"
  )

  allMatConfLong$orig <- gsub(
    x = allMatConfLong$orig,
    pattern = "(wudapt)(_)(.*)",
    replacement = "wud\\2\\3"
  )


  allMatConfLong$dest <- gsub(
    x = allMatConfLong$wf_pair,
    pattern = "(.*)(_)(.*)(_)(.*)(_)(.*)",
    replacement = "\\5\\6\\7"
  )

  allMatConfLong$dest <- gsub(
    x = allMatConfLong$dest,
    pattern = "(wudapt)(_)(.*)",
    replacement = "wud\\2\\3"
  )


  allMatConfLong$weightedFlux <- allMatConfLong$agreePercArea * allMatConfLong$percArea / 10000


  # gsub(x = (allMatConfLong$wf_pair %>% unique),
  #      pattern = "(.*)(_)(.*)(_)(.*)(_)(.*)",
  #      replacement = "\\1_\\5") %>% unique

  allMatConfLong$dest <- LCZlevelToOrderedString(allMatConfLong$dest)
  allMatConfLong$orig <- LCZlevelToOrderedString(allMatConfLong$orig)

  allMatConfLong <- allMatConfLong[, c("orig", "dest", "weightedFlux")]

  return(allMatConfLong)
}