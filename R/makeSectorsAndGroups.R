#' Creates sectors and groups for the drawChordDiagram function
#'
#' @param multMatConfLongIn is typically the output of the function createMultipleMatConf,
#' and is expected to contain the following columns orig, dest and weightedFlux,
#' whose names are quite self explanatory : orig is the origin LCZ type, dest is the destination LCZ type
#' and weightedFlux is the percentage of area transfered from orig to dest
#' @return a list containing vectors and groups for a chord diagram
#' @importFrom dplyr case_when
#' @export
makeSectorsAndGroups<-function(multMatConfLongIn) {

  sectors <- sort(unique(c(multMatConfLongIn$orig, multMatConfLongIn$dest)))

  sect_lev <- gsub(
    x = sectors,
    pattern = "(.*)(_)(.*)",
    replacement = "\\3")

  df_groups_val <- case_when(
    nchar(sect_lev) == 1 ~ paste0("00", sect_lev),
    nchar(sect_lev) == 2 ~ paste0("0", sect_lev),
    .default = sect_lev
  )

  df.groups <- structure(
    df_groups_val,
    names = sectors) 
  output<-list(sectors = sectors, df.groups = df.groups)
  return(output)
}