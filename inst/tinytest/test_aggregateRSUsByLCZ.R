 dirPath<-paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs")
 allLocAllWfs<-loadConcatAllLocsAllWfs(
  dirPath = dirPath, locations = c("Blaru", "Arville"),
 workflowNames = c("osm","bdt","iau","wudapt"),
  missingGeomsWf = "iau",
  refWf = NULL,
  refLCZ = "Unclassified",
  residualLCZvalue = "Unclassified",
  column = "lcz_primary"
)
 ASUallLocAllWfs <- aggregateRSUsByLCZ(
 allLocAllWfs,
 LCZcolumn = "lcz_primary", wfColumn = "wf", locationColumn = "location", aggregateBufferSize = 0.5)