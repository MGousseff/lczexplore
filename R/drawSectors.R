#' Draws sectors and groups inside the drawChordDiagram function
#'
#' @param sector_id ar the ideitnfiers of sectors as returned by the makeSectorsAndGroups function
#' @param sectorsIn are the values of a sectors as returned by the makeSectorsAndGroups function
#' @param colorMapIn is a named vector whose names must contain the values of sectorsIn and
#' whose values are the desired colors
#' @param facing defines the way labels of sectors are plotted, the default is "clockwise",
#' see drawChorDiagram function for details
#' @param textMatch is
#' @return a list containing vectors and groups for a chord diagram
#' @importFrom shades complement
#' @importFrom circlize highlight.sector
#'
#' @export
drawSectors <- function(sector_id, sectorsIn = sectors,
                        colorMapIn = colorMap, textMatch,
                        facing = "clockwise") {
  print(paste0("textMatch ", textMatch))
  print("sectorsIn") ;   print(sectorsIn)
  print("sector_id")  ; print(sector_id)
  sectorsToHighlight <- grep(x = sectorsIn, pattern = sector_id, value = T) %>% unique
  print(paste0("sectorsToHighlight :", sectorsToHighlight))

  textOut <- textMatch[sector_id]
  print(textOut)

  textColor <- shades::complement(colorMapIn[sector_id])
  print(textColor)
  names(textColor) <- names(colorMapIn[sector_id])
  if (length(sectorsToHighlight)>0){
  highlight.sector(sectorsToHighlight,
                   track.index = 1, col = colorMapIn[sector_id],
                   text = textOut, text.col = shades::complement(colorMapIn[sector_id]),
                   cex = 0.9, niceFacing = TRUE, facing = facing)
  }
}