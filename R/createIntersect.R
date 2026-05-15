#' Intersect multiple sf files in which Local Climate Zones are set for polygon geometries 
#' @param sfList a list which contains the classifications to compare, as sf objects
#' @param columns a vector which contains, for each sf of sfList,
#' the name of the columns of the classification to compare
#' @param refCrs a number which indicates which sf object from sfList will provide
#' the CRS in which all the sf objects will be projected before comparison
#' By defautl it is set to an empty string and no ID is loaded.
#' @param workflowNames a vector of strings which contains the names of the workflows used to produce the sf objects
#' @param minZeroArea all geometries smaller than this value are discarded (avoids numeric precision problems)
#' @details The input SfList can contain two levels, the first level being the locations,
#' the second being the workflow. In this case, the output will still be a single sf object,
#' concatenating the workflows and locations as columns
#' @importFrom dplyr mutate
#' @import sf utils
#' @importFrom magrittr "%>%"
#' @return a single sf file with values of LCZ from all the input
#' are assigned to geometries resulting from intersection of all input geometries
#' @export
#' @examples
#' sfList<-loadMultipleSfs(
#' dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),
#' "/multipleWfs/Arville"),
#' workflowNames = c("osm","bdt","iau","wudapt"), inLocation = "Arville")
#' ArvilleIntersect <- createIntersect(
#'  sfList = sfList, columns = rep("lcz_primary", 4),  
#'  workflowNames = c("osm","bdt","iau","wudapt"))
createIntersect<-function(sfList, columns, refCrs=NULL, workflowNames=NULL, minZeroArea=0.0001){

  if (collapse::ldepth(sfList)>1){
    intersectedList<-lapply(sfList, createIntersect,
                            columns = columns, refCrs = refCrs, workflowNames = workflowNames, minZeroArea = minZeroArea )

    for (i in seq_along(intersectedList)) {
      intersectedList[[i]]$location <- names(sfList)[i]
          }
    sfInt<-do.call(rbind, intersectedList)
    return(sfInt)
  }

  # sfInt<-sfList[[1]] %>% select(columns[1])

  if (is.null(refCrs)){refCrs<-st_crs(sfList[[1]])}

  sfIntCRSed <- lapply(seq_along(sfList), function(i) {
    sf_obj <- sfList[[i]][, columns[i], drop = FALSE]
    if (st_crs(sf_obj) != refCrs) st_transform(sf_obj, crs = refCrs) else sf_obj
  })

  sfInt <- Reduce(st_intersection, sfIntCRSed)
  if (!is.null(workflowNames) & length(workflowNames) == length(sfList)){
    names(sfInt)[1:(ncol(sfInt)-1)]<-workflowNames
  } else { names(sfInt)[1:(ncol(sfInt)-1)]<-paste0("LCZ", seq_along(sfList)) }

  sfInt<-dplyr::mutate(sfInt, area = units::drop_units(st_area(sfInt$geometry)),
                                         .before=geometry)
  sfInt<-sfInt[sfInt$area>minZeroArea,]
  return(sfInt) 
}

# sfBDT_11_78030<-importLCZvect(dirPath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/BDT/2011/bdtopo_2_78030",
#                    file="rsu_lcz.fgb", column="LCZ_PRIMARY")
# class(sfBDT_11_78030)
# sfBDT_22_78030<-importLCZvect(dirPath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/BDT/2022/bdtopo_3_78030",
#                               file="rsu_lcz.fgb", column="LCZ_PRIMARY")
# sf_OSM_11_Auffargis<-importLCZvect(dirPath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/OSM/2011/osm_Auffargis/",
#                                    file="rsu_lcz.fgb", column="LCZ_PRIMARY")
# sf_OSM_22_Auffargis<-importLCZvect(dirPath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/OSM/2022/osm_Auffargis/",
#                                    file="rsu_lcz.fgb", column="LCZ_PRIMARY")
# sf_WUDAPT_78030<-importLCZvect("/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/WUDAPT", 
#                                file ="wudapt_78030.geojson", column="lcz_primary")
# 
# sfList<-list(BDT11 = sfBDT_11_78030, BDT22 = sfBDT_22_78030, OSM11= sf_OSM_11_Auffargis, OSM22 = sf_OSM_22_Auffargis, 
#              WUDAPT = sf_WUDAPT_78030)
# showLCZ(sfList[[1]])
# 
# 
# 
# intersected<-createIntersec(sfList = sfList, columns = c(rep("LCZ_PRIMARY",4),"lcz_primary"), 
#                             workflowNames = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"))
# 
# 
# test_list<-list(a=c(1,2),b="top",c=TRUE)
# length(test_list)
# for (i in test_list[2:3]) print(str(i))