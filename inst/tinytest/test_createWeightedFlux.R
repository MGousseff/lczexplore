require(lczexplore)
twoLocsDir<-paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs")
twoLocsSfList<-loadMultipleLocsSfs(dirPath = twoLocsDir, workflowNames = c("osm","bdt","wudapt"),
                                   inLocation = c("Arville", "Redon"))

twoLocsSfIntersected <- createIntersect(sfList = twoLocsSfList, columns = rep("lcz_primary", 4),
                                        refCrs=NULL, workflowNames=c("osm", "bdt", "wudapt"), minZeroArea=0.001)

twoLocsMatConfLong<-createWeightedFlux(twoLocsSfIntersected, wfNamesIn = c("osm","bdt","wudapt"))