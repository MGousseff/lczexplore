# This tests the function createIntersect
# library(tinytest)
#
# library(sf)

# intersected<-createIntersect(sfList = sfList, columns = c(rep("LCZ_PRIMARY",4),"lcz_primary"),
#                             sfWf = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"))
 sfList<-loadMultipleSfs(
 dirPath = paste0(
 system.file("extdata", package = "lczexplore"),
 "/multipleWfs/Arville"),
 workflowNames = c("osm","bdt","iau","wudapt"), inLocation = "Arville")

collapse::ldepth(sfList)

ArvilleIntersect <- createIntersect(
  sfList = sfList, columns = rep("lcz_primary", 4),
  sfWf = c("osm","bdt","iau","wudapt"))

sfListTwoLocs<-loadMultipleLocsSfs(dirPath = paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
                            workflowNames = c("osm","bdt","iau","wudapt"), inLocation = c("Arville", "Blaru"))

collapse::ldepth(sfListTwoLocs)
twoLocsIntersec <- createIntersect(sfList = sfListTwoLocs, columns = rep("lcz_primary", 4),
                                   sfWf = c("osm","bdt","iau","wudapt"))

sfListLocationned<-lapply(
  seq_along(sfList),
  function(i){
    lapply(sfList[[i]], function(x){x$location<-names(sfList)[i] ; return(x)})
  })
names(sfListLocationned) <- names(sfList)


d
