#This tests the function importQualVar
library(tinytest)

library(sf)
library(dplyr)
library(lczexplore)

expect_silent(
  utrfRedonBDT<-
    importQualVar(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/utrfFiles"),
                 file="bdt_utrf_area.fgb", column="TYPO_MAJ", geomID="ID_RSU", confid="UNIQUENESS_VALUE")
)

# map2<-showLCZ(utrfRedonBDT,column = "TYPO_MAJ",repr="alter")

expect_silent(
  utrfRedonOSM<-
    importQualVar(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/utrfFiles"),
                  file="osm_utrf_area.fgb", column="TYPO_MAJ",geomID="ID_RSU",confid="UNIQUENESS_VALUE")
)

#summary(st_geometry_type(utrfRedonOSM))
utrfComparison<-
  compareLCZ(sf1=utrfRedonBDT, column1="TYPO_MAJ", wf1=" UTRF BDT",
             sf2=utrfRedonOSM, column2="TYPO_MAJ", wf2 = " UTRF OSM", 
           location = " Redon",exwrite=FALSE,repr="alter", saveG="", plotNow = FALSE)


# utrfComparison$matConfPlot %>% print
# utrfComparison$data
# utrfComparison$matConf
# utrfComparison$matConfLarge

# library(tidyr)
# pivot_wider(utrfComparison$matConf, names_from = TYPO_MAJ.1, values_from = agreePercArea)
expect_equal(round(as.numeric(utrfComparison$matConf[1,3]), 2), 62.96)
# 
