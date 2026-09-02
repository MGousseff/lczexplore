require(lczexplore)
twoLocsDir<-paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs")
twoLocsSfList<-loadMultipleLocsSfs(dirPath = twoLocsDir, workflowNames = c("osm","bdt","wudapt"),
                                   inLocation = c("Arville", "Redon"))

twoLocsSfIntersected <- createIntersect(sfList = twoLocsSfList, columns = rep("lcz_primary", 4),
                                        refCrs=NULL, workflowNames=c("osm", "bdt", "wudapt"), minZeroArea=0.001)

twoLocsWeightedFlux<-createWeightedFlux(twoLocsSfIntersected, wfNamesIn = c("osm","bdt","wudapt"))

test <- makeSectorsAndGroups(twoLocsWeightedFlux)

drawChordDiagram(twoLocsWeightedFlux, colorMapIn = NULL, labelMatch = NULL, inFacing = "clockwise")

twoLocsWeightedFluxNo104no101<-subset(twoLocsWeightedFlux,
                             !grepl("101", twoLocsWeightedFlux$orig) &
                               !grepl("104", twoLocsWeightedFlux$orig) &
                               !grepl("101", twoLocsWeightedFlux$dest) &
                               !grepl("104", twoLocsWeightedFlux$dest))
aggregMatch<-c("acompact"="Compact", "blessCompact" = "Less Compact", "cfewToNoBuild" = "Few to No Buildings",
               "dunclass" = "Unclassified")
drawChordDiagram(twoLocsWeightedFluxNo104no101, labelMatch = aggregMatch, inFacing = "bending",
                 acompact = c("001", "002", "003"),
                 blessCompact = c("004", "005", "006", "007", "008", "010"),
                 cfewToNoBuild = c("101", "102", "103", "104", "105", "106", "107", "009"),
                 dunclass = "Unclassified",
                 groupColors = c(
                   "acompact" = "#8b0101",
                   "blessCompact" = "#ff9856",
                   "cfewToNoBuild" = "#bbdb7a",
                   "dunclass" = "grey"
                 )
)

drawChordDiagram(twoLocsWeightedFluxNo104no101, inFacing = "bending",
                 compact = c("001", "002", "003"),
                 lessCompact = c("004", "005", "006", "007", "008", "010"),
                 fewToNoBuild = c("101", "102", "103", "104", "105", "106", "107", "009"),
                 unclass = "Unclassified",
                 groupColors = c(
                   "compact" = "#8b0101",
                   "lessCompact" = "#ff9856",
                   "fewToNoBuild" = "#bbdb7a",
                   "unclass" = "grey"
                 )
)

