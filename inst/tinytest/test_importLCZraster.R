#This tests teh function importLCZvect
# library(tinytest)
#
# library(sf)

# Test functionnal import

redonBbox<-importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/lczfiles/Redon"),file="osm_lcz.fgb",column="LCZ_PRIMARY",
  , output="bBox")

expect_warning(redonWudapt<-importLCZraster(

  system.file("extdata", package = "lczexplore"),
  fileName="redonWudapt.tif", bBox=redonBbox,
  LCZband=1, LCZcolumn="LCZ_PRIMARY"),
              'attribute variables are assumed to be spatially constant throughout all geometries' )

# library(terra)
# test<-rast(paste0(system.file("extdata", package = "lczexplore"),"/redonWudapt.tif"))

expect_silent(showLCZ(redonWudapt, column = "LCZ_PRIMARY", repr = "standard"))

# Test out of Europe Bbox (supposed to fail)
# library(sf)
#bBoxCoord<-c(-117.312698,32.805168,-117.227554,32.864593)
lowCorner<-sf::st_point(c(-117.312698,32.805168))
upCorner<-sf::st_point(c(-117.227554,32.864593))
outBbox<-sf::st_sfc(lowCorner,upCorner,crs=4326)
#importLCZraster("/home/gousseff/Documents/2_CodesSources/Wudapt/WudaptEurope/",bBox=outBbox)

# test bounding box not intersecting with rastet
# expect_error(current = test<-importLCZraster(dirPath = system.file("extdata", package = "lczexplore"),
#                                              fileName="redonWudapt.tif",bBox=outBbox),
#              "The bounding box doesn\'t intersect ")


# test chosing wich band of the raster is imported
# redonWudapt2<-importLCZraster(  
#   system.file("extdata", package = "lczexplore"),
#   fileName="redonWudapt.tif",bBox=redonBbox,
# LCZband=1, LCZcolumn="LCZ")
#  showLCZ(redonWudapt2, column = "LCZ")

# sidneyOSM<-importLCZvect(
<<<<<<< HEAD
<<<<<<< HEAD
#   dirPath = system.file("extdata/osm/2022/Sidney", package = "lczexplore"), file="sidney_rsu_lcz.fgb",
=======
#   dirPath = system.file("extdata/mczfiles/Sidney", package = "lczexplore"), file="sidney_rsu_lcz.fgb",
>>>>>>> 747f811 (File path correction in test files)
=======
#   dirPath = system.file("extdata/osm/2022/Sidney", package = "lczexplore"), file="sidney_rsu_lcz.fgb",
>>>>>>> MGousseff/lczexplore/afterMultipleComparisons
#   confid="LCZ_UNIQUENESS_VALUE",
#   geomID="ID_RSU")

sidneyBbox<-importLCZvect(
  system.file("extdata/lczfiles/Sidney", package = "lczexplore"), file="sidney_rsu_lcz.fgb",
  ,output = "bBox")

# test default import

expect_warning(sidneyWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
                              fileName="rasterMD.tif",bBox=sidneyBbox),
               "attribute variables are assumed to be spatially constant throughout all geometries")

# test default specifying band number

expect_warning(sidneyWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
                             fileName="redonWudapt.tif",bBox=redonBbox, LCZband=1, LCZcolumn='EU_LCZ_map'),
               "attribute variables are assumed to be spatially constant throughout all geometries")


expect_error(sidneyWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
                              fileName="redonWudapt.tif",bBox=redonBbox, LCZband=4, LCZcolumn='EU_LCZ_map'),
             "invalid name")

# test importing a band that doesn't exist (wrong name)
expect_error(sidneyWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
                                           fileName="redonWudapt.tif",bBox=redonBbox, LCZband="Ski shoe", LCZcolumn='EU_LCZ_map'),
             "invalid name")

# test importing several bands of the raster is imported, one being the confidence, one being numeric and the other the name of a band

expect_warning(sidneyWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
                fileName="rasterMD.tif", LCZband=1,  LCZcolumn = "LCZraster",
                confidenceBand = 3, confidenceColumn = "confidence",
                bBox = sidneyBbox ),
               "attribute variables are assumed to be spatially constant")

expect_warning(sidneyWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
                                             fileName="rasterMD.tif", LCZband="lcz",  LCZcolumn = "LCZraster",
                                             confidenceBand = 3, confidenceColumn = "confidence",
                                             bBox = sidneyBbox ),
               "attribute variables are assumed to be spatially constant")

# test importing several bands, one being wrong

expect_error(sidneyWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
                                           fileName="rasterMD.tif",bBox=sidneyBbox, LCZband=1, LCZcolumn='EU_LCZ_map',
                                           confidenceBand = "skishoe", confidenceColumn = "confidence"),
             "invalid name")
