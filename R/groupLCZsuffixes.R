#' Regroups levels of origin and destination of a multiple confusion matrix in long form.
#'
#' @param multiMatConfLongIn is typically the output of the function createMultipleMatConf,
#' and is expected to contain the following columns orig, dest and weightedFlux,
#' whose names are quite self explanatory : orig is the origin LCZ type, dest is the destination LCZ type
#' and weightedFlux is the percentage of area transfered from orig to dest
#' @param ... a set of argument passed as groupName = groupValues
#' where groupName is the name of a resulting group and groupValues a vector of the initial values
#' it will regroup.
#' @return a list containing vectors and groups for a chord diagram
#' @export
groupLCZsuffix <- function(multiMatConfLongIn, ...) {
  #require(forcats)
  origPref <- sub("(.*)(_)(.*)", "\\1\\2", multiMatConfLongIn$orig)
  destPref <- sub("(.*)(_)(.*)", "\\1\\2", multiMatConfLongIn$dest)
  origSuff <- gsub("(.*)(_)(.*)", "\\3", multiMatConfLongIn$orig)
  destSuff <- gsub("(.*)(_)(.*)", "\\3", multiMatConfLongIn$dest)

  # ensure all the LCZ levels are present in the imported column
  uniqueSuff <- unique(c(origSuff, destSuff)) %>% as.character # Attention unique outputs a list of length 1

  # get the grouping levels as passed by ..., but without keeping arguments about colours
  args <- list(...)[names(list(...)) != "groupColors"]
  indSep <- names(args)
  indCol <- grep(x = indSep, pattern = "groupColors")
  print(names(args))

  args <- append(list(origSuff), args)
  # temp<-do.call(fct_collapse,args)
  origSuffOut <-
    tryCatch(expr = do.call(fct_collapse, args),
             warning = function(w) {
               message("One of the specified levels to group doesn't exist in the data, if it is a mispelled level of the data,
             this level will be kept as ungrouped", w)
               return(
                 do.call(fct_collapse, args)
               )
             })

  multiMatConfLongIn$orig <- paste0(origPref, origSuffOut)

  args <- append(list(destSuff), args)
  # temp<-do.call(fct_collapse,args)
  destSuffOut <-
    tryCatch(expr = do.call(fct_collapse, args),
             warning = function(w) {
               message("One of the specified levels to group doesn't exist in the data, if it is a mispelled level of the data,
             this level will be kept as ungrouped", w)
               return(
                 do.call(fct_collapse, args)
               )
             })


  multiMatConfLongIn$dest <- paste0(destPref, destSuffOut)

  return(multiMatConfLongIn)
}