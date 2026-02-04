library(dplyr)
library(sf)
library(ggplot2)

expect_silent(
  sfList<-loadMultipleSfs(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Goussainville"),
                          workflowNames = c("osm","bdt","iau","wudapt"),
                          inLocation = "Goussainville", column = "lcz_primary")
)
