# This tests the function createIntersect
# library(tinytest)
# library(dplyr)
# library(tidyr)
# library(sf)
# library(ggplot2)
# library(forcats)

sfList<-loadMultipleSfs(dirPath = paste0(system.file("extdata/multipleWfs/Arville", package = "lczexplore")),
                        workflowNames = c("osm","bdt","wudapt"), inLocation = "Arville"  )

intersected<-createIntersect(sfList = sfList, columns = rep("lcz_primary", 3),
                             workflowNames = c("osm", "bdt", "wudapt"))

expect_silent(multicompare_test<-compareMultipleLCZ(intersected,
                                      LCZcolumns = c("osm","bdt","wudapt"),
                                      trimPerc = 0.5))

expect_silent(testAreas<-workflowAgreeAreas(multicompare_test$sfIntLong))

expect_equal(testAreas$areaAgree[1], 7733495)
expect_equal(round(testAreas$areaDisagree[1], 2), 80362.35)

osm<-importLCZvect(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
                          file = "osm_lcz.fgb")
bdt<-importLCZvect(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
                   file = "bdt_lcz.fgb")
wudapt<-importLCZvect(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
                   file = "wudapt_lcz.fgb", column = "lcz_primary")

sfList2<-list(osm = osm, bdt = bdt, wudapt = wudapt)

test3<- loadmultipleSfsFromFromSession(sfList = sfList2,
                                            workflowNames = c("osm", "bdt", "wudapt"),
                                            location = "Arville",
                                            columns = c("LCZ_PRIMARY", "LCZ_PRIMARY", "lcz_primary" ))

intersected<-createIntersect(sfList = test3, columns = rep("lcz_primary", 3),
                             workflowNames = c("osm", "bdt", "wudapt"))

expect_silent(multicompare_test<-compareMultipleLCZ(intersected,
                                                    LCZcolumns = c("osm","bdt","wudapt"),
                                                    trimPerc = 0.5))

expect_silent(testAreas<-workflowAgreeAreas(multicompare_test$sfIntLong))


# multicompare_test

test<-multicompare_test$sfIntLong
test2<-test %>% subset(agree==TRUE) %>% group_by(LCZvalue) %>% summarize(agreementArea=sum(area)) %>%
  mutate(percAgreementArea=agreementArea/sum(agreementArea))

testWfAgree<-test %>% subset(agree==TRUE) %>% group_by(whichWfs) %>% summarize(agreementArea=sum(area))

test<-multicompare_test$sfInt[,1:5] %>% st_drop_geometry()
prov1<-apply(X = test, MARGIN = 1, table )
prov2<-apply(X = test, MARGIN = 1, function(x) max(table(x)) )

head(prov1)
head(prov2)

plot1<-showLCZ(sf = multicompare_test$sfInt, column="bdt", wf="bdt")
plot2<-showLCZ(sf = multicompare_test$sfInt, column="osm", wf="osm")
plot4<-showLCZ(sf = multicompare_test$sfInt, column="wudapt", wf="wud")
plot5<-ggplot(data=multicompare_test$sfInt) +
  geom_sf(aes(fill=nbAgree, color=after_scale(fill)))+
  scale_fill_gradient(low = "red" , high = "green", na.value = NA)
cowplot::plot_grid(plot1, plot2, plot4, plot5)
