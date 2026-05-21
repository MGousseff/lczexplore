# This tests the function createIntersect
# library(tinytest)
#
# library(sf)

# intersected<-createIntersect(sfList = sfList, columns = c(rep("LCZ_PRIMARY",4),"lcz_primary"),
#                             workflowNames = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"))
 sfList<-loadMultipleSfs(
   dirPath = paste0(
   system.file("extdata", package = "lczexplore"),
   "/multipleWfs/Arville"),
 workflowNames = c("osm","bdt","wudapt"), inLocation = "Arville")

collapse::ldepth(sfList)

ArvilleIntersect <- createIntersect(
  sfList = sfList, columns = rep("lcz_primary", 4),
  workflowNames = c("osm", "bdt", "wudapt"))

sfListTwoLocs<-loadMultipleLocsSfs(dirPath = paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
                            workflowNames = c("osm","bdt","wudapt"), inLocation = c("Arville", "Redon"))

collapse::ldepth(sfListTwoLocs)
twoLocsIntersec <- createIntersect(sfList = sfListTwoLocs, columns = rep("lcz_primary", 4),
                                   workflowNames = c("osm", "bdt", "wudapt"))


